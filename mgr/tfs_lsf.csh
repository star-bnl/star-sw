#! /usr/local/bin/tcsh -f
#  $Id: tfs_lsf.csh,v 1.1 1998/10/09 00:37:03 fisyak Exp $
#  $Log: tfs_lsf.csh,v $
#  Revision 1.1  1998/10/09 00:37:03  fisyak
#  Production scripts
#
#  Revision 1.1.2.1  1998/09/16 22:50:52  fisyak
#  BFC with tfs
#
#setenv NODEBUG yes
#source /afs/rhic/rhstar/group/.starnew
#set script      = $0
set script      = tfs_dst
set script_name = `basename ${script}`; echo ${script_name}
set size        = 80
if ($#argv != 1) then
  echo "required 1 argument = input file name (on HPSS)"
  exit 1
endif
set HPSS_INPUT = $1;
set set         = `basename ${HPSS_INPUT} .fzd`
set input_file  = `basename ${HPSS_INPUT}`
#
echo "HPSS_INPUT = "${HPSS_INPUT} "set = "${set} "input_file = " ${input_file}
set HPSS_OUTPUT = `echo $HPSS_INPUT | sed -e 's/\/home\/starsink\/raw\//\/home\/starreco\/reco\//g' -e 's/gstardata/tfs\_dst/g'  -e 's/\.fzd/\_h\_dst\.xdf/g'`
set OUTPUT_FILE    = `echo $HPSS_OUTPUT | sed -e 's/\/home\/starreco\/reco\//\/disk1\/star\//g'`
set output_file = `basename ${OUTPUT_FILE} _dst.xdf`
echo "HPSS_OUTPUT = "${HPSS_OUTPUT} "OUTPUT_FILE = "${OUTPUT_FILE}
set log_file    = ${set}.`hostname`
set no_events   = `echo ${input_file} | awk -F_ '{print $3}' | awk -Fe '{print $1}'`
if ($no_events <1 ) set no_events = 1000;
set long_log    = /star/u2e/starreco/MDC1/summary/${script_name}/${log_file}
cd /home/starreco/mdc1/home
if ( -d ${set} )  rm  -r ${set}
mkdir ${set}
# get input file
set HOSTNAME = `hostname | cut -c1-4`
if ("$HOSTNAME" == "rcrs") then
  $STAR/mgr/get_raw_data.csh -i ${HPSS_INPUT} -o ./${input_file}
  if ($status != 0) exit 1
  set input_file = ../${input_file}
  set INPUT_FILE = ${input_file}
else
  set INPUT_FILE = `echo ${HPSS_INPUT} | sed -e 's/\/home\/starsink\/raw\//\/disk1\/star\//g'`
  if ("$HOSTNAME" == "rcf.") set INPUT_FILE = /net/rmds03${INPUT_FILE}
  csh -x $STAR/mgr/get_raw_data.csh -i ${HPSS_INPUT} -o ${INPUT_FILE}
  if ($status != 0) exit 1
  ln -s ${INPUT_FILE} ${input_file}
endif
cd ${set}
mkdir core
touch ${long_log}
printenv  >>  ${long_log}
echo Starting job execution at `date` >>  ${long_log}
STAR_LEVELS  >>  ${long_log}
pwd          >>  ${long_log}
echo `hostname`; ls /opt/star/lib;
echo `hostname` >> ${long_log};  ls /opt/star/lib >> ${long_log};
while (! -d /opt/star/lib )
  set HOSTNAME = `hostname`
  set DATE = `date`
#                                        mms@bnl.gov  
  Mail -s "GENERAL: /opt/star/lib is not accessable from $HOSTNAME" fisyak@bnl.gov << EOF
  /opt/star/lib is not accessable from $HOSTNAME at $DATE
  Please check afs connection to this host ($HOSTNAME)
  Set job with ${input_file} sleep for 10 minutes
EOF
sleep 600
end
cd /home/starreco/mdc1/home/${set}
ln -s ${OUTPUT_FILE} ${output_file}_dst.xdf; 
ls -alF  ../ ./  ; ls -alF  ../ ./  >>  ${long_log};
if (-f batch.kumac) rm batch.kumac
cat > batch.kumac <<EOF
trace off full
debug on 
   exec $STAR/kumacs/chain/bfc _ 
   TOP=$STAR _
   tpc_sector_first=1 _
   tpc_sector_last=24 _
   gstar_settings=' field_only' _
   domain=' geometry g2t tpc ctf svt global' _
   chain=' fzin rg2t tpg svg tfs srs tpt tid tte_e sgr drhd dehd svm egr evr ev0 ddedx dpnt desum dmsft drsum dout ' _
   skip=0 _
   no_events=${no_events} _ 
   input_data_file=${input_file} _ 
   output_file=${output_file} _ 
   log=$log_file 
exit 
quit
EOF
more batch.kumac  >>  ${long_log}
time  staf -w 0 -g 20 < batch.kumac
# Clean files/links not needed anymore...
csh -x $STAR/mgr/put_dst_data.csh -i ${output_file} -o ${HPSS_OUTPUT}
rm ${INPUT_FILE}
cd ..
rm -rf ${set}
echo "++++++++++++++++++++++++++++  SUMMARY +++++++++++++++++++++++"
ls -alF ../ ./ >>  ${long_log}
more $log_file >>  ${long_log}
rm   $log_file
echo
echo   Job finished at `date` >>  ${long_log}
mv ${long_log} ${long_log}.done
exit 0
#END









