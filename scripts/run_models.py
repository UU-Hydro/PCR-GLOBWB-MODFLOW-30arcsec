#!/usr/bin/env python
__author__ = "Jarno Verkaik, Deltares"
__version__ = "0.0.1"
__email__ = "jarno.verkaik@deltares.nl"
__status__ = "Development for 1km PCR-GLOWB MODFLOW 6"

import os
import argparse
from glob import glob

clp = argparse.ArgumentParser()
clp.add_argument('-exe', '--exe', type=str, default=False,
                help='MODFLOW 6 executable.')
clp.add_argument('-mod', '--mod', type=str, default=False,
                help='Model name.')
cla = clp.parse_args().__dict__

if __name__ == "__main__":

    exe = cla['exe']
    mod = cla['mod']
    os.chdir(mod)
    mfsim_list = glob('%s.m*.mfsim.nam'%mod)

    for mfsim in mfsim_list:
        s = '%s -s %s'%(exe, mfsim)
        os.system(s)

