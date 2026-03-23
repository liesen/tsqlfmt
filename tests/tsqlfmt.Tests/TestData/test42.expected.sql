CREATE TABLE dbo.test42 (
    tenant_id INT NOT NULL,
    customer_id INT NOT NULL,
    order_id INT NOT NULL,
    created_at DATETIME2 NOT NULL,
    CONSTRAINT PK_test42 PRIMARY KEY CLUSTERED (
        tenant_id,
        customer_id,
        order_id
    )
)
ON [PRIMARY];
