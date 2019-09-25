use AdventureWorks2014
select * from [HumanResources].[EmployeePayHistory] as ALIAS
--[HumanResources].[JobCandidate]

--cuando usas tablas tmporales se borran al cerrar sesions, eliminar tableas.
--con linked servers puedes invocar otros servidores del FROM

/* production.product */  -- con ALT + f1 obtienes detalles de la dolumna selecciona

select P.Name,P.ProductNumber, ListPrice AS PRICE
from [Production].[Product] P
--order by Price desc
--order by ListPrice desc
order by 3 desc


select name,SafetyStockLevel, ReorderPoint , ListPrice-StandardCost as ingreso, SafetyStockLevel-ReorderPoint as RESTA
from Production.Product P 
where P.listprice > 0
order by ingreso desc

--creacion de rangos (case) + group by
select count(*) as 'numero de productos',
case 
	when ListPrice-StandardCost <100 then '1 Menor a 100' 
	when ListPrice-StandardCost between 100 and 500 then '2 de 101 a 500' 
	when ListPrice-StandardCost between 501 and 1000 then '3 de 501 a 1000' 
	when ListPrice-StandardCost between 1001 and 1500 then '4 de 1001 a 1500' 
	END RANGO
from Production.Product P 
where P.listprice > 0
group by 
	case 
		when ListPrice-StandardCost <100 then '1 Menor a 100' 
		when ListPrice-StandardCost between 100 and 500 then '2 de 101 a 500' 
		when ListPrice-StandardCost between 501 and 1000 then '3 de 501 a 1000' 
		when ListPrice-StandardCost between 1001 and 1500 then '4 de 1001 a 1500' 
	END




--having
select count(*) as 'numero de productos',
case 
	when ListPrice-StandardCost <100 then '1 Menor a 100' 
	when ListPrice-StandardCost between 100 and 500 then '2 de 101 a 500' 
	when ListPrice-StandardCost between 501 and 1000 then '3 de 501 a 1000' 
	when ListPrice-StandardCost between 1001 and 1500 then '4 de 1001 a 1500' 
	END RANGO,
	sum(ListPrice-StandardCost) as 'INGRESOS ACUMULADOS'
from Production.Product P 
where P.listprice > 0
group by 
	case 
		when ListPrice-StandardCost <100 then '1 Menor a 100' 
		when ListPrice-StandardCost between 100 and 500 then '2 de 101 a 500' 
		when ListPrice-StandardCost between 501 and 1000 then '3 de 501 a 1000' 
		when ListPrice-StandardCost between 1001 and 1500 then '4 de 1001 a 1500' 
	END
--having sum(ListPrice-StandardCost) > 10000 -- having filtra los datos acumulados obtenidos en el Group by, mientras que los where filtran filas
having count(*) > 100 


------

use BD_FINANCIERA

select count(*) from [dbo].[BD_CLIENTE] 
select top 10 * from [dbo].[BD_CLIENTE] 
