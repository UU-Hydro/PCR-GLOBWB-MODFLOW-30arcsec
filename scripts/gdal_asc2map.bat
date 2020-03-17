set gdal=c:\OSGeo4W64\bin\gdal_translate.exe
setlocal EnableDelayedExpansion
set np=128

%gdal% -of PCRaster rcb_126-%np%.asc rcb_126-%np%.map
pause

for /l %%a in (1,1,9) do (
  %gdal% -of PCRaster rcb_00%%a-%np%.asc rcb_00%%a-%np%.map
)
for /l %%a in (10,1,99) do (
  %gdal% -of PCRaster rcb_0%%a-%np%.asc rcb_0%%a-%np%.map
)
for /l %%a in (100,1,%np%) do (
  %gdal% -of PCRaster rcb_%%a-%np%.asc rcb_%%a-%np%.map
)

pause

