foreach f (`ls -1d */*/*.picoDst.root`)
 echo $f
 set d = `dirname ${f}` ; 
 set b = `basename ${f} .picoDst.root` ; 
 set s = `echo ${b} | sed -e 's/st_//'`
# echo "find /star/subsys/tpc/fisyak/reco/2014/50M/SL15StiCAKFV/108/15108011 -name *15108011_raw_1500018*.root -anewer 108/15108011/st_15108011_raw_1500018.picoDst.root"
 find /star/subsys/tpc/fisyak/reco/2014/50M/SL15StiCAKFV/${d} -name '"*${s}*.root"' -anewer ${f} -exec ls -l {} \;
# if ($?) mv ${f} ${f}.HOLD.`date +%m%d%y`
end
