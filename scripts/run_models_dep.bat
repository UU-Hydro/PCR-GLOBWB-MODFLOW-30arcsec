set mf6=d:\codes\git\modflow6-parallel-fork-refactor_checkin\modflow6-parallel\bin\mf6.exe
python run_models.py -mf6 %mf6% -mod 48 -np 8 -pre
python run_models.py -mf6 %mf6% -mod 47 -np 7 -pre
python run_models.py -mf6 %mf6% -mod 46 -np 5 -pre
python run_models.py -mf6 %mf6% -mod 45 -np 4 -pre
python run_models.py -mf6 %mf6% -mod 44 -np 4 -pre
python run_models.py -mf6 %mf6% -mod 43 -np 4 -pre
python run_models.py -mf6 %mf6% -mod 42 -np 2 -pre
python run_models.py -mf6 %mf6% -mod 41 -np 2 -pre
python run_models.py -mf6 %mf6% -mod 40 -np 2 -pre
python run_models.py -mf6 %mf6% -mod 39 -np 2 -pre

pause
