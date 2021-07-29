from collections import OrderedDict as od

#fn = './USGS/summary_260421.txt'
fn = './USGS/summary_260421_bot_2.txt'

f = open(fn,'r'); s = f.readline()
hdr = s.strip().split('\t')
i_rho = hdr.index('RHO_p_month')
i_qre = hdr.index('QRE7525_evalua')
i_perf = hdr.index('performance')
i_oslope = hdr.index('ms_slope')
i_mslope = hdr.index('mo_slope')

d = od()
d['total'] = 0
d['total C1'] = 0
d['total C2'] = 0
d['total C3'] = 0
d['total C4'] = 0
d['total obs_iq7525 > mod_iq7225'] = 0
d['total obs_iq7525 <= mod_iq7225'] = 0
d['C4 improve candidate'] = 0
d['C4 near II'] = 0
d['C4 near III'] = 0
d['C4 obs_iq7525 > mod_iq7225'] = 0
d['C4 obs_iq7525 <= mod_iq7225'] = 0
d['C4 obs and mod flat'] = 0
d['C4 mod flat, obs inc.'] = 0
d['C4 mod flat, obs dec.'] = 0
d['C4 mod inc., obs flat'] = 0
d['C4 mod dec., obs flat'] = 0
d['C4 mod dec., obs dec.'] = 0
d['C4 mod dec., obs inc.'] = 0
d['C4 mod inc., obs dec.'] = 0
d['C4 mod inc., obs inc.'] = 0

delta_rho  = 0.1
delta_aqre = 0.1

min_slope = 0.05

while 1:
    s = f.readline()
    if not s: break
    if (len(s) == 0): break
    d['total'] = d['total'] + 1
    l = s.strip().split('\t')

    rho  = float(l[i_rho])
    qre  = float(l[i_qre])
    aqre = abs(qre)
    perf = int(l[i_perf])
    oslope = float(l[i_oslope])
    mslope = float(l[i_mslope])

    if (perf == 1): d['total C1'] += 1
    if (perf == 2): d['total C2'] += 1
    if (perf == 3): d['total C3'] += 1
    if (perf == 4):
        d['total C4'] += 1
        if (rho >= (0.5-delta_rho)):
            d['C4 improve candidate'] += 1
            d['C4 near II'] += 1
        elif(aqre <= (0.5+delta_aqre)):
            d['C4 improve candidate'] += 1
            d['C4 near III'] += 1
        if (qre <= 0):
            d['C4 obs_iq7525 > mod_iq7225'] += 1
        else:
            d['C4 obs_iq7525 <= mod_iq7225'] += 1
        # both model and observation have no trend
        if ((abs(oslope) <= min_slope) and (abs(mslope) <= min_slope)):
            d['C4 obs and mod flat'] += 1
        else:
            # model has no trend, observation has trend
            if ((abs(mslope) <= min_slope) and (abs(oslope) > min_slope)):
                 if (oslope > 0.):
                     d['C4 mod flat, obs inc.'] += 1
                 else:
                     d['C4 mod flat, obs dec.'] += 1


            # observation has no trend, model has no trend
            if ((abs(oslope) <= min_slope) and (abs(mslope) > min_slope)):
                 if (mslope > 0.):
                     d['C4 mod inc., obs flat'] += 1
                 else:
                     d['C4 mod dec., obs flat'] += 1

            # both model as observation have a trend
            if ((abs(mslope) > min_slope) and (abs(oslope) > min_slope)):
                 if (mslope > 0.):
                     if (oslope > 0.):
                         d['C4 mod inc., obs inc.'] += 1
                     else:
                         d['C4 mod inc., obs dec.'] += 1
                 else:
                     if (oslope > 0.):
                         d['C4 mod dec., obs inc.'] += 1
                     else:
                         d['C4 mod dec., obs dec.'] += 1

    if (qre <= 0):
        d['total obs_iq7525 > mod_iq7225'] += 1
    else:
        d['total obs_iq7525 <= mod_iq7225'] += 1


for k in d.keys():
    if k is 'total':
        print('%s: %i'%(k,d[k]))
    else:
        v = 100.*float(d[k])/float(d['total'])
        print('%s: %.2f %%'%(k,v))





