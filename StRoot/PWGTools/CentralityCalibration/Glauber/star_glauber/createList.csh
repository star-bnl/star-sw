#!/bin/csh -f

#set energy = "11"
set energy = "200"
#set energy = "27"

#set types =("default")
#set types = ( "default" "small" "large" "smallXsec" "largeXsec" "gray" "gauss" )
set types = ( "default" "small" "large" "smallXsec" "largeXsec" "gauss" "smallNpp" "largeNpp" )

foreach type ($types)
  set treelist = "./LIST/tree.$type.list"
  echo "Make $treelist..."
   ls -1 ./output/fastglaubermc_RuRu_Case1_${energy}*GeV_${type}_deformed_*.root > $treelist
end

# Make link "smallNpp" "largeNpp" "smallTotal" "largeTotal" to "default"
cd LIST
#ln -vs tree.default.list tree.smallNpp.list
#ln -vs tree.default.list tree.largeNpp.list
ln -vsf tree.default.list tree.smallTotal.list
ln -vsf tree.default.list tree.largeTotal.list
ln -vsf tree.default.list tree.lowrw.list
ln -vsf tree.default.list tree.highrw.list
ls -lrta
cd -

