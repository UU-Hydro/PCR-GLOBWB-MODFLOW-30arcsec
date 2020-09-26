set -x

# prepare and go to the output directory
mkdir /projects/0/dfguu/user/edwin/data/groundwater_recession_30sec/version_2019_05_03_quick_and_dirty
cd /projects/0/dfguu/user/edwin/data/groundwater_recession_30sec/version_2019_05_03_quick_and_dirty

# obtain the initial values from Cuthbert et al., 2019
cp /projects/0/dfguu/users/edwin/data/cuthbert_et_al_2019/7393304/gtr_30arcsec_in_day.map .

# identify the extent of permeability values that will be replaced
# - all unconsolidated sediments (i.e. coarse, fine and mixed) will be assigned log permeability -10.9
