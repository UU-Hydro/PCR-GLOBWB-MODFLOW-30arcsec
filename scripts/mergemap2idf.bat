set exe=d:\codes\git\PCR-GLOBWB-MODFLOW-30arcsec\fortran\mergemap2idf\vs\x64\Release\mergemap2idf.exe

set top=g:\models\pcr-globwb-30arcsec\hydrosheds\make_clonemap\128\128.txt

set var=abstraction_lowermost_layer
::%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=abstraction_uppermost_layer
::%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map

set var=bed_conductance_used
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=bottom_lowermost_layer
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=bottom_uppermost_layer
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=drain_conductance
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=drain_elevation_lowermost_layer
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=drain_elevation_uppermost_layer
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=horizontal_conductivity_lowermost_layer
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=horizontal_conductivity_uppermost_layer
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=initial_head_lowermost_layer
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=initial_head_uppermost_layer
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=net_RCH
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map

set var=primary_storage_coefficient_lowermost_layer
::%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=primary_storage_coefficient_uppermost_layer
::%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=secondary_storage_coefficient_lowermost_layer
::%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=secondary_storage_coefficient_uppermost_layer
::%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map

set var=surface_water_bed_elevation_used
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=surface_water_elevation
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=top_uppermost_layer
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=vertical_conductivity_lowermost_layer
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map
set var=vertical_conductivity_uppermost_layer
%exe% %var%.idf %top% -180 -90 .\rcb_{1}-{2}_2\steady-state_only\maps\%var%.map

pause

