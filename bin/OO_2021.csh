#set p = TpcSecRowB;
#set ext = root;
#
#foreach p (TpcSecRowB TpcAccumulatedQ TpcZCorrectionC TpcPadCorrectionMDF TpcLengthCorrectionMDN)
# set ext = C
# if (${p} == "TpcSecRowB") set ext = root;
#echo "ln -s ${p}.3p85GeV_fixedTarget_2019.${ext}                ${p}.20190607.174703.${ext}" 
#end

foreach p (TpcPadCorrectionMDF) # (TpcZCorrectionC) #TpcLengthCorrectionMDN) # TpcSecRowB) # TpcAccumulatedQ TpcZCorrectionC TpcPadCorrectionMDF TpcLengthCorrectionMDN)
 set ext = C
 if (${p} == "TpcSecRowB") set ext = root;
ln -s ${p}.OO_200GeV_2021.${ext}                          ${p}.20210510.134728.${ext} # 20210510.134726  ln -s TpcSecRowB.OO_200GeV_2021.C                          TpcSecRowB.20210510.134727.C # 20210510.134726        
ln -s ${p}.ps_OO_200GeV_2021.${ext}                       ${p}.20210513.083122.${ext} # 20210513.083120  ln -s TpcSecRowB.ps_OO_200GeV_2021.C                       TpcSecRowB.20210513.083121.C # 20210513.083120     
ln -s ${p}.OO_200GeV_2021.${ext}                          ${p}.20210513.084424.${ext} # 20210513.084422  ln -s TpcSecRowB.OO_200GeV_2021.C                          TpcSecRowB.20210513.084423.C # 20210513.084422     
ln -s ${p}.ps_OO_200GeV_2021.${ext}                       ${p}.20210513.175230.${ext} # 20210513.175228  ln -s TpcSecRowB.ps_OO_200GeV_2021.C                       TpcSecRowB.20210513.175229.C # 20210513.175228     
ln -s ${p}.OO_200GeV_2021.${ext}                          ${p}.20210513.182207.${ext} # 20210513.182205  ln -s TpcSecRowB.OO_200GeV_2021.C                          TpcSecRowB.20210513.182206.C # 20210513.182205     
#ln -s ${p}.OO_fcsTiming_2021.${ext}                       ${p}.20210514.144730.${ext} # 20210514.144728  ln -s TpcSecRowB.OO_fcsTiming.C                            TpcSecRowB.20210514.144729.C # 20210514.144728     
ln -s ${p}.OO_200GeV_2021.${ext}                          ${p}.20210514.150221.${ext} # 20210514.150240  ln -s TpcSecRowB.OO_200GeV_2021.C                          TpcSecRowB.20210514.150241.C # 20210514.150240     
ln -s ${p}.ps_OO_200GeV_2021.${ext}                       ${p}.20210516.112017.${ext} # 20210516.112015  ln -s TpcSecRowB.ps_OO_200GeV_2021.C                       TpcSecRowB.20210516.112016.C # 20210516.112015     
ln -s ${p}.FF_OO_200GeV_2021.${ext}                       ${p}.20210522.023644.${ext} # 20210522.023642  ln -s TpcSecRowB.FF_OO_200GeV_2021.C                       TpcSecRowB.20210522.023643.C # 20210522.023642     
end
