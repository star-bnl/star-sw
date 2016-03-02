#!/bin/csh

set thisday = `date +%a`
set today = `date +%Y-%m-%d`

set outdir = /star/rcf/test/dev/daq_sl302.ittf/${thisday}/year_2015/production_pAu200_2015
set webdir = /afs/rhic.bnl.gov/star/doc/www/comp/devQA/${thisday}

echo " --------------------------- Start copy"

echo $outdir
echo $webdir

cd ${outdir}

set qafile = QA_hist_pAu200.2015.pdf

 if ( -e "QA_hist_pAu200.2015.pdf") then

 set qadate = `date +%Y-%m-%d -r QA_hist_pAu200.2015.pdf`

 if ( $qadate == $today ) then
 
  /bin/cp -p $outdir/$qafile $webdir

  echo "$qafile is copied to $webdir "
 
 else
 echo "File QA_hist_pAu200.2015.pdf is old "
 endif
 else
 echo "File QA_hist_pAu200.2015.pdf doesn't exist "
 endif

 echo " --------------------------- Next copy"

set outdir = /star/rcf/test/dev/daq_sl302.ittf/${thisday}/year_2015/production_pp200long_2015

echo $outdir
cd ${outdir}

set qafile = QA_hist_pp200.2015.pdf

 if ( -e "QA_hist_pp200.2015.pdf") then

set qadate = `date +%Y-%m-%d -r QA_hist_pp200.2015.pdf`

 if ( $qadate == $today ) then

 /bin/cp -p $outdir/$qafile $webdir
 
 echo "$qafile is copied to $webdir "
 else
 echo "File QA_hist_pp200.2015.pdf is old "
 endif
 else
 echo "File QA_hist_pp200.2015.pdf doesn't exist "
 endif
 
 echo " -------------------------- Next copy"

set outdir = /star/rcf/test/dev/daq_sl302.ittf/${thisday}/year_2014/AuAu200_production_low_2014

echo $outdir
cd ${outdir}

set qafile = QA_hist_AuAu200.2014.pdf

if ( -e "QA_hist_AuAu200.2014.pdf") then

set qadate = `date +%Y-%m-%d -r QA_hist_AuAu200.2014.pdf`

if ( $qadate == $today ) then

 /bin/cp -p $outdir/$qafile $webdir
 
 echo "$qafile is copied to $webdir "
 else
 echo "File QA_hist_AuAu200.2014.pdf is old "
 endif
 else
 echo "File QA_hist_AuAu200.2014.pdf doesn't exist "
 endif

 echo "  ######################## End of copy"

exit 0
