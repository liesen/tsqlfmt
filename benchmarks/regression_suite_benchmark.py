#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import statistics
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path


@dataclass
class FileSample:
    path: str
    seconds: float
    bytes: int

    @property
    def milliseconds(self) -> float:
        return self.seconds * 1000.0

    def to_dict(self) -> dict[str, object]:
        return {
            "path": self.path,
            "seconds": self.seconds,
            "milliseconds": self.milliseconds,
            "bytes": self.bytes,
        }


@dataclass
class BenchmarkResult:
    name: str
    display_command: list[str]
    iterations: int
    warmups: int
    total_seconds: list[float]
    files_per_run: int
    bytes_per_run: int
    per_file_mean_seconds: list[FileSample]

    @property
    def mean_seconds(self) -> float:
        return statistics.fmean(self.total_seconds)

    @property
    def median_seconds(self) -> float:
        return statistics.median(self.total_seconds)

    @property
    def min_seconds(self) -> float:
        return min(self.total_seconds)

    @property
    def max_seconds(self) -> float:
        return max(self.total_seconds)

    @property
    def stdev_seconds(self) -> float:
        return statistics.stdev(self.total_seconds) if len(self.total_seconds) > 1 else 0.0

    @property
    def files_per_second(self) -> float:
        return self.files_per_run / self.mean_seconds

    @property
    def milliseconds_per_file(self) -> float:
        return (self.mean_seconds * 1000.0) / self.files_per_run

    def to_dict(self) -> dict[str, object]:
        return {
            "name": self.name,
            "display_command": self.display_command,
            "iterations": self.iterations,
            "warmups": self.warmups,
            "total_seconds": self.total_seconds,
            "files_per_run": self.files_per_run,
            "bytes_per_run": self.bytes_per_run,
            "mean_seconds": self.mean_seconds,
            "median_seconds": self.median_seconds,
            "min_seconds": self.min_seconds,
            "max_seconds": self.max_seconds,
            "stdev_seconds": self.stdev_seconds,
            "files_per_second": self.files_per_second,
            "milliseconds_per_file": self.milliseconds_per_file,
            "per_file_mean_seconds": [sample.to_dict() for sample in self.per_file_mean_seconds],
        }


def parse_args() -> argparse.Namespace:
    repo_root = Path(__file__).resolve().parents[1]
    default_suite = repo_root / "tests" / "tsqlfmt.Tests" / "TestData"
    default_style = repo_root / "default-style.json"
    default_tsqlfmt_dll = repo_root / "src" / "tsqlfmt" / "bin" / "Release" / "net10.0" / "tsqlfmt.dll"

    parser = argparse.ArgumentParser(
        description="Benchmark tsqlfmt and sqlprompt over the regression fixture corpus."
    )
    parser.add_argument("--suite-dir", type=Path, default=default_suite)
    parser.add_argument("--style", type=Path, default=default_style)
    parser.add_argument("--tsqlfmt-dll", type=Path, default=default_tsqlfmt_dll)
    parser.add_argument("--iterations", type=int, default=15)
    parser.add_argument("--warmups", type=int, default=3)
    parser.add_argument("--json-out", type=Path)
    parser.add_argument(
        "--pattern",
        default="*.actual.sql",
        help="Glob pattern for benchmark inputs inside --suite-dir.",
    )
    parser.add_argument(
        "--top-files",
        type=int,
        default=10,
        help="How many slowest files to print per formatter.",
    )
    return parser.parse_args()


def require_file(path: Path, label: str) -> None:
    if not path.is_file():
        raise SystemExit(f"Missing {label}: {path}")


def require_inputs(paths: list[Path]) -> None:
    if not paths:
        raise SystemExit("No input files matched the selected suite directory and pattern.")


def require_env_var(name: str) -> str:
    value = os.environ.get(name)

    if value is None or not value.strip():
        raise SystemExit(f"Set {name} to run the regression benchmark.")

    return value


def redact_command(command: list[str], sqlprompt_exe: str) -> list[str]:
    redacted: list[str] = []
    redact_next = False

    for index, arg in enumerate(command):
        if index == 0 and arg == sqlprompt_exe:
            redacted.append("$SQLPROMPT_EXE")
            continue

        if redact_next:
            redacted.append("<redacted>")
            redact_next = False
            continue

        redacted.append(arg)

        if arg == "--authToken":
            redact_next = True

    return redacted


def run_command(name: str, command: list[str], path: Path, sql: bytes) -> tuple[float, bytes]:
    started = time.perf_counter()
    proc = subprocess.run(command, input=sql, capture_output=True, check=False)
    elapsed = time.perf_counter() - started

    if proc.returncode != 0:
        stderr = proc.stderr.decode("utf-8", errors="replace").strip()
        raise RuntimeError(f"{name} failed for {path}: {stderr}")

    return elapsed, len(sql)


def run_suite(name: str, command: list[str], inputs: list[tuple[Path, bytes]]) -> tuple[float, list[tuple[Path, float, int]]]:
    suite_started = time.perf_counter()
    file_samples: list[tuple[Path, float, int]] = []

    for path, sql in inputs:
        elapsed, size = run_command(name, command, path, sql)
        file_samples.append((path, elapsed, size))

    return time.perf_counter() - suite_started, file_samples


def benchmark(
    name: str,
    command: list[str],
    display_command: list[str],
    inputs: list[tuple[Path, bytes]],
    warmups: int,
    iterations: int,
) -> BenchmarkResult:
    for _ in range(warmups):
        run_suite(name, command, inputs)

    total_seconds: list[float] = []
    per_file_samples: dict[Path, list[float]] = {path: [] for path, _ in inputs}
    file_sizes = {path: len(sql) for path, sql in inputs}

    for _ in range(iterations):
        suite_elapsed, file_samples = run_suite(name, command, inputs)
        total_seconds.append(suite_elapsed)

        for path, elapsed, _ in file_samples:
            per_file_samples[path].append(elapsed)

    per_file_mean_seconds = [
        FileSample(
            path=str(path),
            seconds=statistics.fmean(samples),
            bytes=file_sizes[path],
        )
        for path, samples in per_file_samples.items()
    ]
    per_file_mean_seconds.sort(key=lambda sample: sample.seconds, reverse=True)

    return BenchmarkResult(
        name=name,
        display_command=display_command,
        iterations=iterations,
        warmups=warmups,
        total_seconds=total_seconds,
        files_per_run=len(inputs),
        bytes_per_run=sum(file_sizes.values()),
        per_file_mean_seconds=per_file_mean_seconds,
    )


def print_result(result: BenchmarkResult, top_files: int) -> None:
    print(result.name)
    print(f"  mean:   {result.mean_seconds * 1000.0:8.2f} ms per suite")
    print(f"  median: {result.median_seconds * 1000.0:8.2f} ms per suite")
    print(f"  min:    {result.min_seconds * 1000.0:8.2f} ms per suite")
    print(f"  max:    {result.max_seconds * 1000.0:8.2f} ms per suite")
    print(f"  stdev:  {result.stdev_seconds * 1000.0:8.2f} ms")
    print(f"  rate:   {result.files_per_second:8.2f} files/s")
    print(f"  file:   {result.milliseconds_per_file:8.2f} ms/file")
    print(f"  cmd:    {' '.join(result.display_command)}")

    if top_files > 0:
        print("  slowest files:")

        for sample in result.per_file_mean_seconds[:top_files]:
            print(f"    {sample.milliseconds:8.2f} ms  {sample.path}")


def main() -> int:
    args = parse_args()

    require_file(args.style, "style file")
    require_file(args.tsqlfmt_dll, "tsqlfmt release DLL")

    sqlprompt_exe = require_env_var("SQLPROMPT_EXE")
    auth_token = require_env_var("SQLPROMPT_AUTH_TOKEN")

    files = sorted(args.suite_dir.glob(args.pattern))
    require_inputs(files)
    inputs = [(path, path.read_bytes()) for path in files]

    with tempfile.TemporaryDirectory(prefix="tsqlfmt-bench-style-") as style_dir_str:
        style_dir = Path(style_dir_str)
        benchmark_style = style_dir / "Default.formattingstyle"
        benchmark_style.write_text(args.style.read_text(encoding="utf-8"), encoding="utf-8")

        common_args = [
            "formatSql",
            "--authToken",
            auth_token,
            "--adsStylesPath",
            str(style_dir),
            "--styleName",
            "Default",
            "--applyCasing",
        ]

        tsqlfmt_command = ["dotnet", str(args.tsqlfmt_dll), *common_args]
        sqlprompt_command = [sqlprompt_exe, *common_args]
        tsqlfmt_display_command = redact_command(tsqlfmt_command, sqlprompt_exe)
        sqlprompt_display_command = redact_command(sqlprompt_command, sqlprompt_exe)

        tsqlfmt_result = benchmark(
            "tsqlfmt",
            tsqlfmt_command,
            tsqlfmt_display_command,
            inputs,
            args.warmups,
            args.iterations,
        )
        sqlprompt_result = benchmark(
            "sqlprompt",
            sqlprompt_command,
            sqlprompt_display_command,
            inputs,
            args.warmups,
            args.iterations,
        )

    print(f"Corpus: {len(inputs)} files, {sum(len(sql) for _, sql in inputs)} bytes")
    print(f"Warmups: {args.warmups}, iterations: {args.iterations}")
    print()
    print_result(tsqlfmt_result, args.top_files)
    print()
    print_result(sqlprompt_result, args.top_files)
    print()

    speedup = sqlprompt_result.mean_seconds / tsqlfmt_result.mean_seconds
    print(f"tsqlfmt speedup vs sqlprompt: {speedup:.2f}x")

    if args.json_out is not None:
        payload = {
            "suite_dir": str(args.suite_dir),
            "pattern": args.pattern,
            "style": str(args.style),
            "results": [tsqlfmt_result.to_dict(), sqlprompt_result.to_dict()],
            "speedup_tsqlfmt_vs_sqlprompt": speedup,
        }
        args.json_out.write_text(json.dumps(payload, indent=2), encoding="utf-8")

    return 0


if __name__ == "__main__":
    sys.exit(main())
