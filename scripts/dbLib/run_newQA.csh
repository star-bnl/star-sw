#!/bin/csh
source /star/u/starreco/.cshrc
source /afs/rhic.bnl.gov/rhstar/group/.stardev
unsetenv NODEBUG
set today = `date +%Y-%m-%d`

set newlib = SL15g

starver SL15g
echo $STAR

 echo "  ######################## Start QA for test"

set outdir = /star/rcf/test/new/daq_sl302.ittf/year_2015/production_pAu200_2015

echo $outdir
cd ${outdir}

if ( -e st_physics_16148020_raw_4000024.hist.root) then

set histsize = `stat -c %s st_physics_16148020_raw_4000024.hist.root`
set histdate = `date +%Y-%m-%d -r st_physics_16148020_raw_4000024.hist.root`

echo " Size and date of file st_physics_16148020_raw_4000024.hist.root:  $histsize,  $histdate "

 if(  $histsize > 100000 ) then

 if ( -e "QA_hist_pAu200.2015.pdf") then

 set qadate = `date +%Y-%m-%d -r QA_hist_pAu200.2015.pdf`

 if ( $qadate != $today ) then
 /bin/rm -f QA_hist_pAu200.2015.pdf

 root4star -b -q bfcread_hist_to_ps.C\(\"st_physics_16148020_raw_4000024.hist.root\",\"EventQA\",\"bfcTree\",\"QA_hist_pAu200.2015.pdf\",\"QA\ plots\ for\ pAu\ 200GeV\ run\ 2015\ for\ ${newlib}\ library\",\"/star/u/starreco/newtest/newQAhists.lis\",2,3\)

 else
 echo "QA for pAu 200GeV was done today "
 endif

 else
 root4star -b -q bfcread_hist_to_ps.C\(\"st_physics_16148020_raw_4000024.hist.root\",\"EventQA\",\"bfcTree\",\"QA_hist_pAu200.2015.pdf\",\"QA\ plots\ for\ pAu\ 200GeV\ run\ 2015\ for\ ${newlib}\ library\",\"/star/u/starreco/newtest/newQAhists.lis\",2,3\)
 endif
 else
 echo "Size of hist file less then 100000 "
 endif
 else
 echo "No file st_physics_16148020_raw_4000024.hist.root "
 endif

 echo "  ######################## Next test"

set outdir = /star/rcf/test/new/daq_sl302.ittf/year_2015/production_pp200long_2015

echo $outdir
cd ${outdir}

if ( -e st_physics_16060046_raw_5000015.hist.root) then

set histsize = `stat -c %s st_physics_16060046_raw_5000015.hist.root`
set histdate = `date +%Y-%m-%d -r st_physics_16060046_raw_5000015.hist.root`

echo "Size and date of file st_physics_16060046_raw_5000015.hist.root :  $histsize, $histdate "

 if(  $histsize > 100000 ) then

 if ( -e "QA_hist_pp200.2015.pdf") then

set qadate = `date +%Y-%m-%d -r QA_hist_pp200.2015.pdf`

 if ( $qadate != $today ) then
 /bin/rm -f QA_hist_pp200.2015.pdf

root4star -b -q bfcread_hist_to_ps.C\(\"st_physics_16060046_raw_5000015.hist.root\",\"EventQA\",\"bfcTree\",\"QA_hist_pp200.2015.pdf\",\"QA\ plots\ for\ pp\ 200GeV\ run\ 2015\ for\ ${newlib}\ library\",\"/star/u/starreco/newtest/newQAhists.lis\",2,3\)

 else
 echo "QA for pp 200GeV was done today "
 endif

 else
 root4star -b -q bfcread_hist_to_ps.C\(\"st_physics_16060046_raw_5000015.hist.root\",\"EventQA\",\"bfcTree\",\"QA_hist_pp200.2015.pdf\",\"QA\ plots\ for\ pp\ 200GeV\ run\ 2015\ for\ ${newlib}\ library\",\"/star/u/starreco/newtest/newQAhists.lis\",2,3\)
 endif

 else
 echo "Size of hist file less then 100000  "
 endif
 else
 echo "No file st_physics_16060046_raw_5000015.hist.root "
 endif
 
 echo "  ######################## Next test"

set outdir = /star/rcf/test/new/daq_sl302.ittf/year_2014/AuAu200_production_low_2014

echo $outdir
cd ${outdir}

if ( -e st_physics_15164004_raw_2000022.hist.root) then

set histsize = `stat -c %s st_physics_15164004_raw_2000022.hist.root`
set histdate = `date +%Y-%m-%d -r st_physics_15164004_raw_2000022.hist.root`

echo "Size and date of file st_physics_15164004_raw_2000022.hist.root : $histsize, $histdate"

 if(  $histsize > 100000 ) then

if ( -e "QA_hist_AuAu200.2014.pdf") then

set qadate = `date +%Y-%m-%d -r QA_hist_AuAu200.2014.pdf`

if ( $qadate != $today ) then

/bin/rm -f QA_hist_AuAu200.2014.pdf

root4star -b -q bfcread_hist_to_ps.C\(\"st_physics_15164004_raw_2000022.hist.root\",\"EventQA\",\"bfcTree\",\"QA_hist_AuAu200.2014.pdf\",\"QA\ plots\ for\ AuAu\ 200GeV\ run\ 2014\ for\ ${newlib}\ library\",\"/star/u/starreco/newtest/newQAhists.lis\",2,3\)

 else
 echo "QA for AuAu 200GeV was done today "
 endif

 else
root4star -b -q bfcread_hist_to_ps.C\(\"st_physics_15164004_raw_2000022.hist.root\",\"EventQA\",\"bfcTree\",\"QA_hist_AuAu200.2014.pdf\",\"QA\ plots\ for\ AuAu\ 200GeV\ run\ 2014\ for\ ${newlib}\ library\",\"/star/u/starreco/newtest/newQAhists.lis\",2,3\)
 endif 
 else
 echo "Size of hist file less then 100000 "
 endif
 else
 echo "No file st_physics_15164004_raw_2000022.hist.root "
 endif

 echo "  ######################## End of scan"

exit 0
