import urllib.request
import os
from time import time as timer
from multiprocessing.pool import ThreadPool
from lat_lon_parser import parse # https://pypi.org/project/lat-lon-parser/
from bs4 import BeautifulSoup
from collections import OrderedDict as od
from datetime import datetime as dt
from progressbar import ProgressBar as pb
import glob

def select_sites(f_in, f_out, f_deb, t_beg, t_end, min_year, min_obs, list_inge):

    t_b = dt.strptime(t_beg, '%Y-%m-%d')
    t_e = dt.strptime(t_end, '%Y-%m-%d')

    year_sec = 365.25 * 24 * 60 * 60

    fi = open(f_in, 'r'); fo = open(f_out, 'w'); fd = open(f_deb, 'w')
    s = fi.readline()
    fo.write(s); fd.write(s)

    m = 0; n = 0
    while 1:
        s = fi.readline()
        if not s: break
        lst = s.split('\t')
        if (len(lst) == 1): continue
        m += 1

        site_no = lst[1]
        ts1 = lst[7]; ts2 = lst[8]

        if (('-' not in ts1) or ('-' not in ts2)): continue

        try:
            t1 = dt.strptime(ts1, '%Y-%m-%d')
        except:
            try:
                t1 = dt.strptime(ts1, '%Y-%m')
            except:
                continue
        try:
            t2 = dt.strptime(ts2, '%Y-%m-%d')
        except:
            try:
                t2 = dt.strptime(ts2, '%Y-%m')
            except:
                continue

        t1 = max(t_b, t1); t2 = min(t_e, t2)
        delt = t2 - t1
        ny1 = divmod(delt.total_seconds(), year_sec)[0]
        ny2 = delt.total_seconds()/year_sec
        nobs = int(lst[9])

        if ((ny1 >= min_year) and (nobs >= min_obs)):
            fo.write(s)
            n += 1
            if (site_no not in list_inge):
                fd.write(s)


        #print('%s %s, Delta: %f %f'%(ts1,ts2,ny1,ny2))

    print('Using %i out of %i stations...'%(n,m))
    fi.close(); fo.close(); fd.close()

def fetch_timeseries(site_tup):

    lat = None; lon = None

    agency = site_tup[0]; site_no = site_tup[1];

    #print('Downloading for USGS site number %s...'%site_no)

    fn = "%s_tmp.txt"%site_no

    url = 'https://nwis.waterdata.usgs.gov/nwis/gwlevels?site_no=%s&agency_cd=%s&format=rdb'%(site_no,agency)
    #print('url :%s'%url)

    try:
        urllib.request.urlretrieve(url, fn)
    except Exception as e:
        return site_no, lat, lon, e

    f = open(fn,'r'); lst = f.read().split('\n'); f.close(); os.remove(fn)

    if 'No sites' in lst[0]:
        print('Site %s not found!'%site_no)
        return '-', lat, lon, None

    # Remove the comments
    n_comm = 0
    while 1:
        s = lst[n_comm].strip()
        if (s[0:1] == '#'):
            n_comm += 1
        else:
            break
    lst = lst[n_comm:]

    # remove the format specification
    del lst[1]

    # insert quotes
    i = 0
    for i in range(len(lst)):
        s = lst[i].rstrip('\n')
        lst2 = s.split('\t')
        s = '\t'.join('"{0}"'.format(w) for w in lst2)
        lst[i] = s

    f = open('./%s/%s.txt'%(out_dir,site_no),'w'); f.write('\n'.join(lst)); f.close()

    if False:
        fn = "%s_tmp.html"%site_no
        url = 'https://nwis.waterdata.usgs.gov/nwis/gwlevels?site_no=%s'%site_no
        try:
            urllib.request.urlretrieve(url, fn)
        except Exception as e:
            return site_no, e
        f = open(fn,'r'); html = f.read(); f.close()

        #soup = BeautifulSoup(html, 'html.parser')
        #print(soup.title)
        #print(soup.prettify())

        f = open(fn,'r'); lst = f.read().split('\n'); f.close()#; os.remove(fn)
        for line in lst:
            if (('Latitude' in line) and ('Longitude' in line)):
                 d = float(line.split('Latitude&nbsp;')[1].split('&#176;')[0])
                 m = float(line.split('Latitude&nbsp;')[1].split('&#176;')[1].split('\'')[0])
                 s = float(line.split('Latitude&nbsp;')[1].split('&#176;')[1].split('\'')[1].split('"')[0])
                 lat = d + m/float(60) + s/float(3600)
                 #
                 d = float(line.split('Longitude&nbsp;')[1].split('&#176;')[0])
                 m = float(line.split('Longitude&nbsp;')[1].split('&#176;')[1].split('\'')[0])
                 s = float(line.split('Longitude&nbsp;')[1].split('&#176;')[1].split('\'')[1].split('"')[0])
                 lon = d + m/float(60) + s/float(3600)

    return site_no, lat, lon, None

if __name__ == "__main__":

    out_dir = 'USGS'
    try:
        os.makedirs(out_dir)
    except:
        pass

    site_no_f = 'site_no.txt' #inge
    f = open(site_no_f, 'r')
    lst_inge = []
    while 1:
        s = f.readline()
        if not s: break
        lst = s.split()
        lst_inge.append(lst[0])

    site_no_f = 'site_select.txt'
    select_sites('./download/all.txt', site_no_f, 'debug.txt', '1958-01-01', '2015-12-31', 5, 60, lst_inge)

    site_list = []
    with open(site_no_f,'r') as f:
        for line in f:
            site_list.append((line.split()[0],line.split()[1]))

    #site_list = [('USGS','335911117030801')]
    site_list = []

    print('Downloading time-series for %i USGS site(s)...'%len(site_list))
    t0 = timer()

    if False:
        i_site = 0; n_sites = len(site_list)
        for tup in site_no_list:
            i_site += 1
            fetch_timeseries(tup)
            #if (i_site == 2): break
        print('Done in %.2f seconds...'%(timer() - t0))
    else:
        bar = pb(maxval=len(site_list)).start()
        t0 = timer()
        site_no_dict = od()
        results = ThreadPool(15).imap_unordered(fetch_timeseries, site_list)
        n = 0
        for site_no, lat, lon, error in results:
            if error is None:
                if (site_no != '-'):
                    site_no_dict[site_no] = od()
                    site_no_dict[site_no]['lat'] = lat
                    site_no_dict[site_no]['lon'] = lon
                #print("Site %r fetched in %.2f seconds" % (site_no, timer() - t0))
                n += 1
                #print(n)
                bar.update(n)
            else:
                print("Error downloading %r: %s" % (site_no, error))
        print('Done in %.2f seconds...'%(timer() - t0))

        #print("Total of site found: %i"%len(site_no_dict))
        #f = open('site_no_final.txt','w'); f.write('\n'.join(site_no_dict.keys())); f.close()

    site_dict = od()
    f = open(site_no_f, 'r'); hdr = f.readline()
    while 1:
        s = f.readline()
        if not s: break
        site_dict[s.split()[1]] = s
    f.close()
    site_list = [os.path.splitext(os.path.basename(x))[0] for x in glob.glob('./%s/*.txt'%out_dir)]

    fn = 'site_no_final.txt'
    f = open(fn,'w'); f.write(hdr); print('Writing %s...'%fn)
    for site in site_list:
        f.write(site_dict[site])
    f.close()








