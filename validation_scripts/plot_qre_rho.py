import numpy as np
import matplotlib.pyplot as plt

def plot_qre_r(provList, res):
    i = 0
    for prov in provList:
        i += 1
        #f = r'f:\models\pcr-globwb-30arcsec\model_new\validation\statistics\sensitivity_analysys_080621\2_ggm5m_awr17_mean.txt'
        f = './%s/mean_summary_10km_awr2017_bot_5m.txt'%(prov); print('Reading %s...'%f)
        #f = './%s/mean_summary_%s.txt'%(prov,res); print('Reading %s...'%f)
        #f = './%s/summary_%s_inge.txt'%(prov,res); print('Reading %s...'%f)
        x1 = np.genfromtxt(f, delimiter='\t', skip_header=1)
        print('--> %i stations'%np.shape(x1)[0])
        if (i == 1):
            x = x1
        else:
            x = np.concatenate((x, x1))
    nstat = np.shape(x)[0]
    print('Total of %i stations'%nstat)

    qre = 100.*abs(x[:,4]); rho = x[:,3];
    #qre = 100.*abs(x[:,16]); rho = x[:,5]

    # inge:
    qre = abs(x[:,4]); rho = x[:,3];


    xmin = 0; xmax = 150; ymin = -1; ymax = 1.5
    #fpdf = 'ggm_tr_qre_rho_%s_%s_inge.pdf'%("_".join(provList),res)
    #fpng = 'ggm_tr_qre_rho_%s_%s_inge.png'%("_".join(provList),res)
    fpdf = 'ggm_tr_qre_rho_%s_%s.pdf'%("_".join(provList),res)
    fpng = 'ggm_tr_qre_rho_%s_%s.png'%("_".join(provList),res)
    plt.plot([50, 50], [ymin, ymax], color='blue', linestyle='dashed', linewidth=2)
    plt.plot([xmin, xmax], [0.5, 0.5], color='blue', linestyle='dashed', linewidth=2)
    plt.scatter(qre, rho, color='grey', alpha=1.0, marker='x', linewidth=1);
    plt.xlabel('absolute amplitude error [%]'); plt.ylabel('correlation [-]')
    plt.xlim([xmin, xmax]); plt.ylim([ymin, ymax])
    ax = plt.gca(); ax.set_xticks([0, 50, 100, 150]); ax.set_yticks([-1.0, -0.5, 0.0, 0.5, 1.0, 1.5])
    ax.set_title('%s; %i stations; best for %s'%(','.join(provList),nstat,res))
    #plt.savefig(fpdf);
    plt.savefig(fpng); # plt.show()
    plt.clf()

    return


if __name__ == "__main__":
    for res in ['30s']:
        #plot_qre_r(['ADES'],res)
        #plot_qre_r(['DINOF'],res)
        plot_qre_r(['USGS'],res)
        #plot_qre_r(['ADES','DINO','USGS'],res)


