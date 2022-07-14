USE master;
GO
CREATE DATABASE BobsShoes;
GO


SELECT * FROM sys.databases WHERE name = 'BobsShoes';


EXEC sp_helpfile;
GO


CREATE SCHEMA Orders
    AUTHORIZATION dbo;
GO



ALTER DATABASE BobsShoes
	ADD FILEGROUP BobsData;
ALTER DATABASE BobsShoes
	ADD FILE (
    	NAME = BobsData,
        FILENAME = '/Users/hukar/Documents/mssql/BobsShoes/BobsData.mdf'
    )
    TO FILEGROUP BobsData;
    
ALTER DATABASE BobsShoes
	ADD LOG FILE (
        NAME = BobsLogs,
        FILENAME = '/Users/hukar/Documents/mssql/BobsShoes/BobsLogs.ldf'
    );
GO


USE BobsShoes;
GO

CREATE TABLE Orders.OrderTracking (
	OrderId int IDENTITY (1,1) NOT NULL,
    OrderDate datetime2(0) NOT NULL,
    RequestedDate datetime2(0) NOT NULL,
    DeliveryDate datetime2(0) NULL,
    CustName nvarchar(200) NOT NULL,
    CustAddress nvarchar(200) NOT NULL,
    ShoeStyle varchar(200) NOT NULL,
    ShoeSize varchar(10) NOT NULL,
    SKU char(8) NOT NULL,
    UnitPrice  numeric(7,2) NOT NULL,
    Quantity smallint NOT NULL,
    Discount numeric(4,2) NOT NULL,
    IsExpedited bit NOT NULL,
    TotalPrice AS (Quantity * UnitPrice * (1.0 - Discount)),
)
ON BobsData
WITH (DATA_COMPRESSION = PAGE);
GO


USE BobsShoes;
GO

ALTER TABLE Orders.OrderTracking
ADD CONSTRAINT PK_OrderTRacking_OrderId
	PRIMARY KEY (OrderId)
		ON [BobsData];
GO