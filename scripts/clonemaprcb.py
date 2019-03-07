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
clp.add_argument('-a', '--a', action='store_true', default=False,
                help='Do for all RCB partitions.')
clp.add_argument('-s', '--s', nargs='+', type=str, default=False,
                help='Script name.')
clp.add_argument('-n', '--n', nargs='+', type=int, default=False,
                help='Subdomain numbers.')
clp.add_argument('-np', '--np', type=int, default=False,
                help='Number of RCB partitions.')
#
clp.add_argument('-q', '--q', type=str, default='normal',
                help='Queue.')
clp.add_argument('-th', '--th', type=int, default=4,
                help='Node running hours.')
clp.add_argument('-tm', '--tm', type=int, default=0,
                help='Node running minutes.')
clp.add_argument('-ts', '--ts', type=int, default=0,
                help='Node running seconds.')
#
clp.add_argument('-fs', '--fs', nargs='+', type=int, default=False,
                help='File size intervals.')
clp.add_argument('-q_a', '--q_a', nargs='+', type=str, default=None,
                help='Queue.')
clp.add_argument('-th_a', '--th_a', nargs='+', type=int, default=None,
                help='Node running hours.')
clp.add_argument('-tm_a', '--tm_a', nargs='+', type=int, default=None,
                help='Node running minutes.')
clp.add_argument('-ts_a', '--ts_a', nargs='+', type=int, default=None,
                help='Node running seconds.')
#
clp.add_argument('-r', '--r', action='store_true', default=False,
                help='Run the batch script.')
clp.add_argument('-mapdir', '--mapdir', type=str, default=None,
                help='Mapfile directory.')
cla = clp.parse_args().__dict__

def getn(x, n):
    return ('%.' + str(n) + 'i')%x

def writejob(fn, s, th, tm, ts, q, n, np, nd):
    py  = 'deterministic_runner_for_offline_monthly_modflow.py'
    ini = '../config/rcb/template.ini'
    opt = 'debug steady-state-only'

    print('Writing %s%s...'%(fn,s))

    f = open(fn, 'w')
    f.write('#!/bin/bash\n')
    f.write('#SBATCH -t %.2i:%.2i:%.2i\n'%(th,tm,ts))
    f.write('#SBATCH --output=%s.out\n'%os.path.splitext(fn)[0].strip())
    f.write('#SBATCH --error=%s.err\n'%os.path.splitext(fn)[0].strip())
    f.write('#SBATCH -p %s\n'%q)
    f.write('#SBATCH -N 1\n')
    f.write('\n')
    f.write('. /home/hydrowld/opt/pcr43dev/load_pcraster_2019-01-18.sh\n')
    f.write('module load python')
    f.write('\n')
    f.write('cd /home/jarno/global-30/PCR-GLOBWB-MODFLOW-30arcsec/model\n')
    f.write('\n')
    for i in n:
        clone = 'rcb_' + getn(i, nd) + '-' + getn(np, nd)
        f.write('python %s %s %s %s\n'%(py, ini, opt, clone))
    f.close()


if __name__ == "__main__":

    np = cla['np']
    nd = len(str(np))
    ns = cla['n']

    if cla['a']:
         for i in range(1, np+1):
            fn = 'rcb_'+ getn(np, nd) + '_' + getn(i, nd)
            mf = os.path.join(cla['mapdir'], 'rcb_' + getn(i, nd) + '-' + getn(np, nd) + '.map')
            if not os.path.isfile(mf):
                raise Exception('Error, %s not found!'%mf)

            fs = os.path.getsize(mf); fs = int(fs)/(1024*1024)

            s = ''
            th = cla['th']; tm = cla['tm']; ts = cla['ts']; q = cla['q']
            if cla['fs'] is not None:
                lst = cla['fs']
                itgt = -1
                fs_min = 0
                for i in range(len(lst)):
                    fs_max = lst[i]
                    if ((fs_min <= fs) and (fs <= fs_max)):
                        itgt = i
                        break
                if (itgt > -1):
                    s = ''
                    found = False
                    lst = cla['th_a']
                    if lst is not None:
                        found = True
                        th = lst[itgt]
                        s = s.strip() + ' ' + '-th %i'%th
                    lst = cla['tm_a']
                    if lst is not None:
                        found = True
                        tm = lst[itgt]
                        s = s.strip() +' ' + '-tm %i'%tm
                    lst = cla['ts_a']
                    if lst is not None:
                        found = True
                        ts = lst[itgt]
                        s = s.strip() +' ' + '-ts %i'%ts
                    lst = cla['q_a']
                    if lst is not None:
                        found = True
                        q = lst[itgt]
                        s = s.strip() +' ' + '-q %s'%q
                    if found:
                        s = ' (fs %iMB: %s)'%(fs,s.strip())

            writejob(fn, s, th, tm, ts, q, [i], np, nd)
            #print '%s --> %i'%(mf,fs)

            if cla['r']:
                print('Starting job %s...'%fn)
                os.system('sbatch %s'%fn)

    else:
        if cla['s']:
            fn = cla['s']
        else:
            fn = 'rcb_'+ getn(np, nd) + '_'
            lst = []
            for i in ns:
                lst.append(getn(i, nd))
            fn = fn + '-'.join(lst)
        th = cla['th']; tm = cla['tm']; ts = cla['ts']; q = cla['q']; n = cla['n']
        writejob(fn, '', th, tm, ts, q, n, np, nd)
        if cla['r']:
            print('Starting job %s...'%fn)
            os.system('sbatch %s'%fn)








