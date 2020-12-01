import os
import glob
from pyproj import Proj, transform
from datetime import datetime as dt
from dateutil.relativedelta import relativedelta as rdel

if __name__ == "__main__":
    statDir = 'f:/basis_data/dinoloket/proc/grondwaterstanden_Put/'
    outCsv = 'dino.csv'
    minYear = 5

    #
    fullStatList = glob.glob(statDir+'*_1.csv')
    statList = [os.path.splitext(os.path.basename(x))[0][:-5] for x in fullStatList]
    unStatList = list(set(statList)); unStatList.sort()

    ns = len(unStatList)
    print('Number of stations found:   %i'%ns)

    #
    statDict = {}
    i = 0
    inProj = Proj(init='EPSG:28992'); outProj = Proj(init='epsg:4326')

    for stat in unStatList:
        statDict[stat] = {}
        i += 1
        pp = 100.*i/ns
        if (pp % 10. == 0):
             print('Processing stations %.2f done...'%pp)

        ind = [ind for ind, val in enumerate(statList) if val == stat]
        filtList = fullStatList[ind[0]:ind[-1]+1]; filtList.sort()
        statDict[stat]['n_filt'] = len(filtList)
        statDict[stat]['filt_dat'] = {}
        statDict[stat]['csv'] = filtList

        #
        j = -1
        for csv in filtList:
            j += 1
            statDict[stat]['filt_dat'][j] = {}
            fd = statDict[stat]['filt_dat'][j]
            fd['skip'] = False
            fd['csv'] = csv

            # read for the first filter
            f = open(csv, 'r')
            for k in range(3):
                s = f.readline()
            s = f.readline(); l = s.split(',')
            fd['sdat'] = l[1]
            fd['edat'] = l[3]

            # check time-series length
            ts = dt.strptime(fd['sdat'], '%d-%m-%Y')
            te = dt.strptime(fd['edat'], '%d-%m-%Y')
            ny = rdel(te, ts).years
            if (ny < minYear):
                print('Only %s years of data found for station %s'%(str(ny),stat))
                fd['skip'] = True; continue

            for k in range(8):
                s = f.readline()
            s = f.readline(); l = s.split(',')

            # coordinates
            try:
                rd_x = float(l[3]); rd_y = float(l[4])
            except:
                try:
                    print('Could not parse coordinates for station %s: "%s" "%s"'%(stat,l[3].strip(),l[4].strip()))
                except:
                    print('Could not parse coordinates for station %s'%stat)
                fd['skip'] = True; continue
            #
            lon_x, lat_y = transform(inProj, outProj, rd_x, rd_y)
            fd['rd_x']  = rd_x;  fd['rd_y']  = rd_x
            fd['lon_x'] = lon_x; fd['lat_y'] = lat_y

            # surface level
            try:
                sl_nap = float(l[5])/100.
            except:
                try:
                    print('Could not parse surface level for station %s: "%s"'%(stat,l[5].strip()))
                except:
                    print('Could not parse surface level for station %s'%stat)
                fd['skip'] = True; continue
            fd['sl_nap'] = sl_nap

            # filter top
            try:
                tf_nap = float(l[11])/100.
            except:
                try:
                    print('Could not parse filter top for station %s: "%s"'%(stat,l[11].strip()))
                except:
                    print('Could not parse filter top for station %s'%stat)
                fd['skip'] = True; continue
            fd['tf_nap'] = tf_nap

            # bottom top
            try:
                bf_nap = float(l[12])/100.
            except:
                try:
                    print('Could not parse filter bottom for station %s: "%s"'%(stat,l[12].strip()))
                except:
                    print('Could not parse filter bottom for station %s'%stat)
                fd['skip'] = True; continue
            fd['bf_nap'] = bf_nap

            # mid filter
            fd['mf_nap'] = (tf_nap - bf_nap)/2.

            f.close()

        # debug:
        #if (i == 1000): break

    # remove from dictionary
    remStatList = []
    for stat in statDict:
        n_filt = statDict[stat]['n_filt']
        n = 0
        for i in range(n_filt):
            fd = statDict[stat]['filt_dat'][i]
            if (fd['skip']):
                n += 1
        if (n == n_filt):
            remStatList.append(stat)

    print('Number of stations removed: %i'%(len(remStatList)))
    for stat in remStatList:
        statDict.pop(stat)
    print('Number of stations written: %i'%(len(statDict)))

    n_filt_max = 0
    for stat in statDict:
        n_filt_max = max(n_filt_max, statDict[stat]['n_filt'])
    print('Maximum number of filters:  %i'%(n_filt_max))

    f = open(outCsv, 'w')
    hdr = ['stat_code','location','longitude','latitude','n_filt']
    for i in range(1,n_filt_max+1):
        hdr.extend(['f%i_sdat'%i, 'f%i_edat'%i, 'f%i_sl_nap'%i, 'f%i_tf_nap'%i, 'f%i_bf_nap'%i, 'f%i_mf_nap'%i])
    hdr = ['"{0}"'.format(x) for x in hdr] # convert with quotes
    f.write('\t'.join(hdr)+'\n')
    i = 0
    for stat in statDict:
        i += 1
        n_filt = statDict[stat]['n_filt']
        fd = statDict[stat]['filt_dat'][0]
        #
        first = True
        for j in range(1, n_filt_max+1):
            if (j <= n_filt):
                fd = statDict[stat]['filt_dat'][j-1]
                if (not fd['skip']):
                    if first:
                        l = ['%.8i'%i, stat, str(fd['lon_x']), str(fd['lat_y']), str(n_filt)]
                    else:
                        first = False
                    l.extend([fd['sdat'], fd['edat'], \
                        str(fd['sl_nap']), str(fd['tf_nap']), str(fd['bf_nap']), str(fd['mf_nap'])])
                else:
                    l.extend(6*['NA'])
            else:
                l.extend(6*['NA'])
        l = ['"{0}"'.format(x) for x in l]
        f.write('\t'.join(l)+'\n')
    f.close()






