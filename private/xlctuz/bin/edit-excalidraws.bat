for /f %%a in ('echo "%1" ^| sed "s/.svg//"') do (
set to_edit=%%a
)

echo %to_edit%

start "" %to_edit%
