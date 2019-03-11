import os
from shutil import copy2
from glob import glob

def getn(x, n):
    return ('%.' + str(n) + 'i')%x

if __name__ == "__main__":

    np = 128
    nd = len(str(np))

    for i in range(1,np+1):
        p = 'rcb_' + getn(i, nd) + '-' + getn(np, nd)
        f = '%s.zip'%p
        md = '%s/steady-state_only/maps'%p
        s = 'zip %s %s/*.map'%(f, md)
        os.system(s)

