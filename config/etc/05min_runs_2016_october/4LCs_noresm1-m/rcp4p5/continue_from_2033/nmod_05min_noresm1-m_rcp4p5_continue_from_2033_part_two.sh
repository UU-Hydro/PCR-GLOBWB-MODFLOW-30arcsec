#!/bin/bash
#SBATCH -N 1
#SBATCH -t 79:59:00 
#SBATCH -p normal

# load a special version of pcraster
. /home/edwin/bin-special/pcraster-4.1.0-beta-20151027_x86-64_gcc-4/bashrc_special_pcraster_modflow

cd /home/edwinsut/github/edwinkost/PCR-GLOBWB/model
python parallel_pcrglobwb_runner.py ../config/05min_runs_2016_october/4LCs_noresm1-m/rcp4p5/continue_from_2033/setup_05min_noresm1-m_rcp4p5_continue_from_2033_part_two.ini
