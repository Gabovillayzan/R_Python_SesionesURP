use BD_FINANCIERA

--1. cuantos registros
select count(*) from [dbo].[BD_CLIENTE] 

--select top 20 * from [dbo].[BD_CLIENTE] 
--2. cuantos periodos
select distinct MES_PROCESO from [dbo].[BD_CLIENTE] 

--3. periodo con mas registros
select MES_PROCESO, COUNT(*)
from [dbo].[BD_CLIENTE] 
group by MES_PROCESO
order by COUNT(*) -- fila 14

--4. periodos tienen más de 140mil registros
select MES_PROCESO, COUNT(*)
from [dbo].[BD_CLIENTE] 
group by MES_PROCESO
having COUNT(*)>140000

--5. clientes repetidos
select distinct codigo as 'Codigo de cliente', COUNT(*) as 'Numero de veces que se repite'
from [dbo].[BD_CLIENTE] 
where MES_PROCESO=201401
group by CODIGO

select distinct codigo as 'Codigo de cliente REPETIDO', COUNT(*) as 'Numero de veces que se repite'
from [dbo].[BD_CLIENTE] where MES_PROCESO=201401
group by CODIGO
having COUNT(*)>1

--6. Cuantos clientes hombres solteros o mujeres casadas hay?
select CODIGO, SEXO, ESTADO_CIVIL --, COUNT(*)
from [dbo].[BD_CLIENTE]
where ((SEXO like '%M%' and ESTADO_CIVIL like '%S%') or (SEXO like '%F%' and ESTADO_CIVIL like '%C%')) AND MES_PROCESO=201401
order by SEXO, ESTADO_CIVIL

--7. Cuantos clientes tiene la edad mas poblada?
select EDAD, COUNT(*)
from [dbo].[BD_CLIENTE] 
where MES_PROCESO=201401
group by EDAD order by COUNT(*) desc

--8. Cuantos clientes hombres tiene el estado civil más poblado?
select ESTADO_CIVIL, SEXO, COUNT(*)
from [dbo].[BD_CLIENTE] 
where MES_PROCESO=201401 and SEXO like '%M%'
group by ESTADO_CIVIL,SEXO order by COUNT(*) desc

--9. tabla temporal con los clientes con ingreso_score entre mil y 5 mil. 15,359 registros
select * INTO #CLIENTES_TEMP
from [dbo].[BD_CLIENTE] 
where MES_PROCESO=201401 and INGRESO_SCORE between 1000 and 5000
order by EDAD   -- select * from #CLIENTES_TEMP

--10. De la tabla temporal, cuantos clientes hay en los ubigeos que empiezan con 1501?
select * from #CLIENTES_TEMP
where UBIGEO like '1501%'

