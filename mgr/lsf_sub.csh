#! /usr/local/bin/tcsh -f
# submit lsf job with tfs_lsf
if ($#argv < 1) then
  echo "required at least 1 argument = input file name (on HPSS)"
  exit 1
endif
set HOST = ""
if ( $#argv == 2) then
  set HOST = $2
endif
set PROCESS = tfs_dst
set TOPHPSS_SINK =  /home/starsink/raw/auau200/hijing135/default
set TOPHPSS_RECO =  /home/starreco/reco/auau200/hijing135/default
set TOP_TEST     =  /star/scr2f/starreco/MDC1/tests/auau200/hijing135/default
set REQUEST      =  /star/u2e/starreco/MDC1/requests
set JOB_SUMMARY  =  /star/u2e/starreco/MDC1/summary
set JOB_LOG      =  /star/scr2h/starreco/MDC1/auau200/hijing135/default
set DIR          = `dirname $1`;
set SET          = `echo $DIR | sed -e 's/\/home//g' -e 's/\/starsink//g' -e 's/\/raw//g' -e 's/\/auau200//g' -e 's/\/hijing135//g' -e 's/\/default//g' -e 's/\/gstardata//g'`
echo "set = "$SET
set batch = `basename $1 .fzd`
set input_file   = `echo ${TOPHPSS_SINK}${SET}/gstardata/${batch}.fzd | sed -e 's/\/\//\//g'`
echo "input_file =" $input_file
set log_dir    = ${JOB_LOG}${SET}/${PROCESS}
if (! -d ${log_dir}) mkdir -p ${log_dir}
set log_file   = ${log_dir}/${batch}
cd ${REQUEST}/${PROCESS}
if (! -d ${PROCESS}.lsf) mkdir ${PROCESS}.lsf
cd ${PROCESS}.lsf
if (! -d ${log_dir}) mkdir -p ${log_dir}
set log_file   = ${log_dir}/${batch}
if (-f ${batch}) rm -f ${batch}
cat > ${batch} <<EOD
csh $STAR/mgr/tfs_lsf.csh ${input_file}
EOD
set jobname = `echo $batch | cut -c1-9`
if (-f ${log_file}) mv ${log_file} ${log_file}.old
echo bsub -o ${log_file} -e ${log_file}  -J ${jobname} ${batch}
chmod +x ${batch}
if ("$HOST" == "") then
bsub -o ${log_file} -e ${log_file} -J ${jobname} -R crs -u fisyak@bnl.gov -N ${batch}
else
bsub -o ${log_file} -e ${log_file} -J ${jobname} -N -m ${HOST} -u fisyak@bnl.gov ${batch}
endif
