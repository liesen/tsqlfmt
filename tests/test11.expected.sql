MERGE INTO dbo.product_inventory AS target
USING (
    SELECT product_id,
        warehouse_id,
        quantity,
        last_updated
    FROM dbo.staging_inventory
    WHERE batch_id = 1001
) source
ON target.product_id = source.product_id
        AND target.warehouse_id = source.warehouse_id
WHEN MATCHED AND source.quantity <> target.quantity THEN
    UPDATE SET target.quantity = source.quantity,
        target.last_updated = source.last_updated
WHEN NOT MATCHED BY TARGET THEN
    INSERT (product_id, warehouse_id, quantity, last_updated)
    VALUES (source.product_id, source.warehouse_id, source.quantity, source.last_updated)
WHEN NOT MATCHED BY SOURCE THEN
    DELETE
