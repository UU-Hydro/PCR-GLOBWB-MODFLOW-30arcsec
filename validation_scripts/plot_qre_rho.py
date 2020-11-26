import numpy as np
import matplotlib.pyplot as plt

if __name__ == "__main__":
    provList = ['ADES','DINO','USGS']

    i = 0
    for prov in provList:
        i += 1
        f = './%s/summary.txt'%prov; print('Reading %s...'%f)
        x1 = np.genfromtxt(f, delimiter='\t', skip_header=1)
        print('--> %i stations'%np.shape(x1)[0])
        if (i == 1):
            x = x1
        else:
            x = np.concatenate((x, x1))
    print('Total of %i stations'%np.shape(x)[0])

    qre = 100.*abs(x[:,4]); rho = x[:,3];
    #qre = 100.*abs(x[:,16]); rho = x[:,5];
    xmin = 0; xmax = 150; ymin = -1; ymax = 1.5
    fpdf = 'ggm_tr_qre_rho_%s.pdf'%"_".join(provList)
    plt.plot([50, 50], [ymin, ymax], color='blue', linestyle='dashed', linewidth=2)
    plt.plot([xmin, xmax], [0.5, 0.5], color='blue', linestyle='dashed', linewidth=2)
    plt.scatter(qre, rho, color='grey', alpha=1.0, marker='x', linewidth=1);
    plt.xlabel('absolute amplitude error [%]'); plt.ylabel('correlation [-]')
    plt.xlim([xmin, xmax]); plt.ylim([ymin, ymax])
    ax = plt.gca(); ax.set_xticks([0, 50, 100, 150]); ax.set_yticks([-1.0, -0.5, 0.0, 0.5, 1.0, 1.5])
    plt.savefig(fpdf); # plt.show()
