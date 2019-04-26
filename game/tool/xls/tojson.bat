set ExcelDir=/
set DIR=%~dp0

for %%a in (%DIR%%ExcelDir%*)  do (
	python %DIR%tojson.py %DIR%%ExcelDir%%%~na%%~xa %%~na
)

pause