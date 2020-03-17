#!/usr/bin/env python
__author__ = "Jarno Verkaik, Deltares"
__version__ = "0.0.1"
__email__ = "jarno.verkaik@deltares.nl"
__status__ = "Development for 1km PCR-GLOWB MODFLOW 6"

import os
import argparse
from glob import glob

clp = argparse.ArgumentParser()

clp.add_argument('-pre', '--pre', action='store_true', default=False,
                help='Flag for pre-processing.')
clp.add_argument('-run_ser', '--run_ser', action='store_true', default=False,
                help='Flag for running in serial mode.')
clp.add_argument('-run_par', '--run_par', action='store_true', default=False,
                help='Flag for running in parallel mode.')
#
clp.add_argument('-mod', '--mod', type=int, default=None,
                help='Model name.')
#
clp.add_argument('-mf6ggm', '--mf6ggm', type=str,
                default=r'mf6ggm.exe',
                help='MODFLOW 6 pre=processing executable.')
clp.add_argument('-mf6ggminp', '--mf6ggminp', type=str,
                default=r'mf6ggm.inp',
                help='MODFLOW 6 pre=processing input file.')

clp.add_argument('-mf6', '--mf6', type=str,
                default=r'mf6.exe',
                help='MODFLOW 6 executable.')
clp.add_argument('-mpi', '--mpi', type=str,
                default=r'c:\Program Files\Microsoft MPI\Bin\mpiexec.exe',
                help='Message Passing Interface program.')
clp.add_argument('-np', '--np', type=int, default=None,
                help='Number of processes.')
cla = clp.parse_args().__dict__

def get_key(key):
    if key not in cla:
        raise Exception('Key %s not found' % key)
    return cla[key]

if __name__ == "__main__":

    mod = 's%.2i'%get_key('mod')

    if get_key('pre'):
        s = '"%s" %i .\\s%.2i\\ %s'%(get_key('mf6ggm'), get_key('mod'), get_key('mod'), get_key('mf6ggminp'))
        os.system(s)

    if (get_key('run_ser') or get_key('run_par')):
        os.chdir(mod)

    if get_key('run_ser'):
        mfsim_list = glob('%s.m*.mfsim.nam'%mod)
        for mfsim in mfsim_list:
            s = '%s -s %s'%(get_key('mf6'), mfsim)
            os.system(s)

    if get_key('run_par'):
        np  = cla['np']
        if (np is not None):
            mfsim = '%s.mfsim.nam'%mod
            s = '"%s" -np %i %s -s %s'%(get_key('mpi'), np, get_key('mf6'), mfsim)
            print(s)
            os.system(s)
