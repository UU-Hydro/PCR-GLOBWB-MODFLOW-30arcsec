#!/usr/bin/env python
__author__ = "Jarno Verkaik, Deltares"
__version__ = "0.0.1"
__email__ = "jarno.verkaik@deltares.nl"
__status__ = "Development for 1km PCR-GLOWB MODFLOW 6"
#
import os
import sys
import argparse

clp = argparse.ArgumentParser()
clp.add_argument('-s', '--s', nargs='+', type=str, default=False,
                help='Script name.')
clp.add_argument('-n', '--n', nargs='+', type=int, default=False,
                help='Subdomain numbers.')
clp.add_argument('-np', '--np', type=int, default=False,
                help='Number of RCB partitions.')
clp.add_argument('-th', '--th', type=int, default=4,
                help='Node running hours.')
clp.add_argument('-tm', '--tm', type=int, default=0,
                help='Node running minutes.')
clp.add_argument('-ts', '--ts', type=int, default=0,
                help='Node running seconds.')
clp.add_argument('-q', '--q', type=str, default='normal',
                help='Queue.')
clp.add_argument('-r', '--r', action='store_true', default=False,
                help='Run the batch script.')

cla = clp.parse_args().__dict__

def getn(x, n):
    return ('%.' + str(n) + 'i')%x

if __name__ == "__main__":

    np = cla['np']
    nd = len(str(np))
    ns = cla['n']

    py  = 'deterministic_runner_for_offline_monthly_modflow.py'
    ini = '../config/rcb/template.ini'
    opt = 'debug steady-state-only'

    if cla['s']:
        fn = cla['s']
    else:
        fn = 'rcb_'+ getn(np, nd) + '_'
        lst = []
        for i in ns:
            lst.append(getn(i, nd))
        fn = fn + '-'.join(lst)

    print('Writing %s...'%fn)

    f = open(fn, 'w')
    f.write('#!/bin/bash\n')
    f.write('#SBATCH -t %.2i:%.2i:%.2i\n'%(cla['th'],cla['tm'],cla['ts']))
    f.write('#SBATCH --output=%s.out\n'%os.path.splitext(fn)[0].strip())
    f.write('#SBATCH --error=%s.err\n'%os.path.splitext(fn)[0].strip())
    f.write('#SBATCH -p %s\n'%cla['q'])
    f.write('#SBATCH -N 1\n')
    f.write('\n')
    f.write('. /home/hydrowld/opt/pcr43dev/load_pcraster_2019-01-18.sh\n')
    f.write('module load python')
    f.write('\n')
    f.write('cd /home/jarno/global-30/PCR-GLOBWB-MODFLOW-30arcsec/model\n')
    f.write('\n')
    for i in cla['n']:
        clone = 'rcb_' + getn(i, nd) + '-' + getn(np, nd)
        f.write('python %s %s %s %s\n'%(py, ini, opt, clone))
    f.close()

    if cla['r']:
        print('Starting job %s...'%fn)
        os.system('sbatch %s'%fn)








