use CA_Database_201812

go
drop table _ul_nullcheck

go

select a.polno
	, a.[plan]
	, a.[partner]
	, a.[branchcode]
	, a.ppstat
	, b3.[SPCODE]
	, b1.*
into _ul_nullcheck
from struc_UL2 a
left join [ul_dist_model] b2 on a.[partner] = b2.[PARTNER]
left join [SPCODE] b3 on b2.[DIST] = b3.[DIST]
left join [ul_model] b1 on b2.[DIST] = b1.DIST
	and a.packcode = b1.PACKCODE
	and a.[plan] = b1.PRODUCT
where a.ppstat = 'if'

DECLARE @tb nvarchar(512) = N'dbo.[_ul_nullcheck]';

DECLARE @sql nvarchar(max) = N'SELECT * FROM ' + @tb
    + ' WHERE 1 = 0';

SELECT @sql += N' OR ' + QUOTENAME(name) + ' IS NULL'
    FROM sys.columns 
    WHERE [object_id] = OBJECT_ID(@tb);

EXEC sys.sp_executesql @sql;