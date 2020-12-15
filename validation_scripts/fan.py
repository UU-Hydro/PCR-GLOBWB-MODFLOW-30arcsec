import numpy as np
from matplotlib import pyplot as plt
import os.path
from scipy.stats import skew as sp_skew

def plot_hist(plt, isp, cont, fp, binList, bins, rgbList):
    print('Plotting for %s...'%cont)

    dat = []
    for bin in binList:
        f = '%s.%s.bin'%(fp%cont.lower(),bin)
        if (not os.path.isfile(f)):
            raise Exception('File %s not found.'%f)
        x = np.fromfile(f, dtype=np.float64)
        dat.append(x)

    datf = np.concatenate(dat, axis=0 )
    n = len(datf); mean = np.mean(datf); median = np.median(datf); stdev= np.std(datf); skew = sp_skew(datf)
    s = '\n'.join(['n = %i'%n, 'mean = %.2f'%mean, 'median = %.2f'%median, 'stdev = %.2f'%stdev, 'skew = %.2f'%skew])

    colList = []
    for rgb in rgbList:
        colList.append(tuple([x/255. for x in rgb] + [1.0]))

    #Stack the data
    ax = plt.subplot(2,3,isp)
    ax.set_title(cont)

    #n, bins, patches = plt.hist(dat, bins, stacked=True, color=colList, density=False, rwidth=0.9)
    n, bins, patches = plt.hist(dat, bins, stacked=True, color=colList, density=False)

    plt.text(0.05,0.8,s,horizontalalignment='left', verticalalignment='center', transform = ax.transAxes)

    return plt

if __name__ == "__main__":

    fp = r'f:\models\pcr-globwb-30arcsec\model_new\results_cartesius\steady_state\post\%s.wtd.stat.ss.19580101'
    ffig = 'ss-wtd-res'

    contList = ['Europe', 'Asia', 'Africa', 'North-America', 'South-America', 'Australia-Oceania']

    binList = ['le-0.25',     '0.25-0.50',   '0.50-2.50',   '2.50-5.00',    '5.00-10.00', \
               '10.00-20.00', '20.00-40.00', '40.00-80.00', '80.00-160.00', 'gt-160.00']
    # from red to blue
    rgbList = [[165,  1, 38], [215, 48, 39], [245,109, 67], [251,175, 95], [255,223,144], \
               [224,243,249], [168,218,231], [113,173,210], [ 68,117,182], [ 49, 54, 148]]
    rgbList = rgbList[::-1]

    bins = np.linspace(-1000.,100,110,endpoint=False)

    isp = 0
    for cont in contList:
        isp += 1
        plt = plot_hist(plt, isp, cont, fp, binList, bins, rgbList)
    plt.gcf().suptitle('Water-depth residuals $d_{obs} - d_{mod}$ in meters',fontsize=20)

    manager = plt.get_current_fig_manager()
    manager.window.showMaximized()
    plt.savefig(ffig+'.png'); plt.savefig(ffig+'.pdf')
    plt.show()


