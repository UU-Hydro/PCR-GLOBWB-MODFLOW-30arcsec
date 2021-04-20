region = 'usgs'

if region == 'usgs':
    fn_sm    = './USGS/summary_sm.txt'
    fn_sel_i = '../download_nwis/site_no_final_2.txt'; istat = 1
    fn_sel_o = '../download_nwis/site_no_final_2_sm.txt'
elif region == 'dinof':
    fn_sm    = './DINOF/summary_sm.txt'
    fn_sel_i = '../download_dino/dino_sel.csv'; istat = 0
    fn_sel_o = '../download_dino/dino_sel_sm.csv'
elif region == 'ades':
    fn_sm    = './ADES/summary_sm.txt'
    fn_sel_i = './ADES/station_list_ades_sel.csv'; istat = 0
    fn_sel_o = './ADES/station_list_ades_sel_sm.csv'
else:
    pass

sm_lst = []
f = open(fn_sm,'r'); s = f.readline()
while 1:
    s = f.readline()
    if not s:
        break
    if (len(s) == 0):
        break
    sm_lst.append(s.split('\t')[0])
f.close()

fi = open(fn_sel_i,'r'); fo = open(fn_sel_o,'w')

l = fi.readline().split(); l.append('sm_corr')
fo.write(" ".join(l)+'\n')
n = 0; nc = 0
while 1:
    s = fi.readline()
    if not s:
        break
    if (len(s) == 0):
        break
    stat = s.split('\t')[istat].strip('"')
    n += 1
    if stat in sm_lst:
        nc +=1
        fo.write('%s\t1\n'%s.strip())
    else:
        fo.write('%s\t0\n'%s.strip())
fi.close(); fo.close()
print('# Soil moisture correlated: %i out of %i (%.2f %%)'%(nc,n,100.*nc/n))











