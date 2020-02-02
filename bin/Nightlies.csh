grep 'Processing bfc.C' /star/rcf/test/dev/*/*/*/*/*.log | tee bfc.log
#sed -e 's/daq_sl302/RC/' -e 's/simu/MC/' -e 's/trs_sl302/MC/' -e 's/_opt//' -e 's/ittf/Sti/' -e 's/stica/StiCA/' -e 's/stihr/StiHR/' bfc.log | awk -F\/ '{print $6}' | sort -u
#sed -e 's/daq_sl302/RC/' -e 's/simu/MC/' -e 's/trs_sl302/MC/' -e 's/_opt//' -e 's/ittf/Sti/' -e 's/stica/StiCA/' -e 's/stihr/StiHR/' bfc.log | awk -F\/ '{print $6"/"$8"/"$9" "$10}' | awk '{print $3":"$1}' | sort  -u 
sed -e 's/daq_sl302/RC/' -e 's/simu/MC\.Sti/' -e 's/trs_sl302/MC/' -e 's/_opt//' -e 's/ittf/Sti/' -e 's/stica/StiCA/' -e 's/stihr/StiHR/' \
-e 's/bfc\.C(//' -e 's/","/:/' -e 's/")\.\.\.//' bfc.log | awk -F\/ '{print $8"/"$6"/"$9" "$10}' | awk '{print $1":"$3}' | sort -u | tee Nightlies.listRaf
makeNightliesList.pl bfc.log
