#set p = TpcSecRowB;
#set ext = root;
#
#foreach p (TpcSecRowB TpcAccumulatedQ TpcZCorrectionC TpcPadCorrectionMDF TpcLengthCorrectionMDN)
# set ext = C
# if (${p} == "TpcSecRowB") set ext = root;
#echo "ln -s ${p}.3p85GeV_fixedTarget_2019.${ext}                ${p}.20190607.174703.${ext}" 
#end

foreach p (TpcSecRowB TpcAccumulatedQ TpcZCorrectionC TpcPadCorrectionMDF TpcLengthCorrectionMDN)
 set ext = C
 if (${p} == "TpcSecRowB") set ext = root;
ln -sf ${p}.19GeV_2019.${ext}                              ${p}.20190225.202321.${ext} # 20190225.202320  ln -sf ${p}.${ext}  ${p}.19GeV_2019.${ext}                       
ln -sf ${p}.14p5GeV_2019.${ext}                            ${p}.20190404.094406.${ext} # 20190404.094405  ln -sf ${p}.${ext}  ${p}.14p5GeV_2019.${ext}                     
ln -sf ${p}.7.3GeV_fixedTarget_2019.${ext}                 ${p}.20190417.133402.${ext} # 20190417.133401  ln -sf ${p}.${ext}  ${p}.7.3GeV_fixedTarget_2019.${ext}          
ln -sf ${p}.14p5GeV_2019.${ext}                            ${p}.20190417.141247.${ext} # 20190417.141246  ln -sf ${p}.${ext}  ${p}.14p5GeV_2019.${ext}                     
ln -sf ${p}.7.3GeV_fixedTarget_2019.${ext}                 ${p}.20190423.155419.${ext} # 20190423.155418  ln -sf ${p}.${ext}  ${p}.7.3GeV_fixedTarget_2019.${ext}          
ln -sf ${p}.14p5GeV_2019.${ext}                            ${p}.20190423.181159.${ext} # 20190423.181158  ln -sf ${p}.${ext}  ${p}.14p5GeV_2019.${ext}                     
ln -sf ${p}.7p7GeV_2019.${ext}                             ${p}.20190603.223656.${ext} # 20190603.223655  ln -sf ${p}.${ext}  ${p}.7p7GeV_2019.${ext}                      
ln -sf ${p}.3p85GeV_fixedTarget_2019.${ext}                ${p}.20190607.174703.${ext} # 20190607.174702  ln -sf ${p}.${ext}  ${p}.3p85GeV_fixedTarget_2019.${ext}         
ln -sf ${p}.7p7GeV_2019.${ext}                             ${p}.20190607.202245.${ext} # 20190607.202244  ln -sf ${p}.${ext}  ${p}.7p7GeV_2019.${ext}                      
ln -sf ${p}.3p85GeV_fixedTarget_2019.${ext}                ${p}.20190609.124529.${ext} # 20190609.124528  ln -sf ${p}.${ext}  ${p}.3p85GeV_fixedTarget_2019.${ext}         
ln -sf ${p}.7p7GeV_2019.${ext}                             ${p}.20190610.024110.${ext} # 20190610.024109  ln -sf ${p}.${ext}  ${p}.7p7GeV_2019.${ext}                      
ln -sf ${p}.7.3GeV_fixedTarget_2019.${ext}                 ${p}.20190618.143659.${ext} # 20190618.143658  ln -sf ${p}.${ext}  ${p}.7.3GeV_fixedTarget_2019.${ext}          
ln -sf ${p}.7p7GeV_2019.${ext}                             ${p}.20190619.025127.${ext} # 20190619.025126  ln -sf ${p}.${ext}  ${p}.7p7GeV_2019.${ext}                      
ln -sf ${p}.9p2GeV_2019.${ext}                             ${p}.20190628.081932.${ext} # 20190628.081931  ln -sf ${p}.${ext}  ${p}.9p2GeV_2019.${ext}                      
ln -sf ${p}.4p59GeV_fixedTarget_2019.${ext}                ${p}.20190629.032835.${ext} # 20190629.032834  ln -sf ${p}.${ext}  ${p}.4p59GeV_fixedTarget_2019.${ext}         
ln -sf ${p}.9p2GeV_2019.${ext}                             ${p}.20190702.144152.${ext} # 20190702.144151  ln -sf ${p}.${ext}  ${p}.9p2GeV_2019.${ext}                      
ln -sf ${p}.31GeV_fixedTarget_2019.${ext}                  ${p}.20190709.032313.${ext} # 20190709.032312  ln -sf ${p}.${ext}  ${p}.31GeV_fixedTarget_2019.${ext}           
ln -sf ${p}.AuAu200GeV_2019.${ext}                            ${p}.20190710.110158.${ext} # 20190710.110157  ln -sf ${p}.${ext}  ${p}.AuAu200GeV_2019.${ext}                     
ln -sf ${p}.9p2GeV_2019.${ext}                             ${p}.20190715.085142.${ext} # 20190715.085141  ln -sf ${p}.${ext}  ${p}.9p2GeV_2019.${ext}                      

#         Checked
ln -sf ${p}.11p5GeV_2020.${ext}                            ${p}.20191208.091309.${ext} # 20191208.091308  ln -sf ${p}.${ext}  ${p}.11p5GeV_2020.${ext}  ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191208.091309.C # 20191208.091308
ln -sf ${p}.11p5GeV_2020.${ext}                            ${p}.20191221.190033.${ext} # 20191221.190032  ln -sf ${p}.${ext}  ${p}.11p5GeV_2020.${ext}  ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191221.190033.C # 20191221.190032             
ln -sf ${p}.11p5GeV_2020.${ext}                            ${p}.20200210.220429.${ext} # 20200210.220428  ln -sf ${p}.${ext}  ${p}.11p5GeV_2020.${ext}	ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20200210.220429.C # 20200210.220428
ln -sf ${p}.11p5GeV_2020.${ext}                            ${p}.20200214.143743.${ext} # 20200214.143742  ln -sf ${p}.${ext}  ${p}.11p5GeV_2020.${ext}  ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20200214.143743.C # 20200214.143742                 

ln -sf ${p}.7p7GeV_2020.${ext}                             ${p}.20200902.105441.${ext} # 20200902.105440  ln -sf ${p}.${ext}  ${p}.7p7GeV_2020.${ext}   ln -sf TpcSecRowB.7p7GeV_2020.C                             TpcSecRowB.20200902.105441.C # 20200902.105440
ln -sf ${p}.7p7GeV_2020.${ext}                             ${p}.20200910.041640.${ext} # 20200910.041639  ln -sf ${p}.${ext}  ${p}.7p7GeV_2020.${ext}   ln -sf TpcSecRowB.7p7GeV_2020.C                             TpcSecRowB.20200910.041640.C # 20200910.041639

ln -sf ${p}.9p2GeVc_2020.${ext}                            ${p}.20200617.233209.${ext} # 20200617.233208  ln -sf ${p}.${ext}  ${p}.9p2GeVc_2020.${ext}  ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200617.233209.C # 20200617.233208                  
ln -sf ${p}.9p2GeVc_2020.${ext}                            ${p}.20200730.042837.${ext} # 20200730.042836  ln -sf ${p}.${ext}  ${p}.9p2GeVc_2020.${ext}  ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200730.042837.C # 20200730.042836                  
ln -sf ${p}.9p2GeVc_2020.${ext}                            ${p}.20200814.054358.${ext} # 20200814.054357  ln -sf ${p}.${ext}  ${p}.9p2GeVc_2020.${ext}  ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200814.054358.C # 20200814.054357                  
ln -sf ${p}.9p2GeVc_2020.${ext}                            ${p}.20200820.055403.${ext} # 20200820.055402  ln -sf ${p}.${ext}  ${p}.9p2GeVc_2020.${ext}  ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200820.055403.C # 20200820.055402                  
ln -sf ${p}.9p2GeVc_2020.${ext}                            ${p}.20200828.064251.${ext} # 20200828.064250  ln -sf ${p}.${ext}  ${p}.9p2GeVc_2020.${ext}  ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200828.064251.C # 20200828.064250                              

ln -sf ${p}.5p75GeV_fixedTarget_2020.${ext}                ${p}.20191221.154022.${ext} # 20191221.154021  ln -sf ${p}.${ext}  ${p}.5p75GeV_fixedTarget_2020.${ext}                      
ln -sf ${p}.31p2GeV_fixedTarget_2020.${ext}                ${p}.20200128.182913.${ext} # 20200128.182912  ln -sf ${p}.${ext}  ${p}.31p2GeV_fixedTarget_2020.${ext}                      
ln -sf ${p}.9p8GeV_fixedTarget_2020.${ext}                 ${p}.20200130.005841.${ext} # 20200130.005840  ln -sf ${p}.${ext}  ${p}.9p8GeV_fixedTarget_2020.${ext}                       
ln -sf ${p}.9p2GeV_2020.${ext}                             ${p}.20200131.012113.${ext} # 20200131.012112  ln -sf ${p}.${ext}  ${p}.9p2GeV_2020.${ext}                                   
ln -sf ${p}.9p8GeV_fixedTarget_2020.${ext}                 ${p}.20200131.050329.${ext} # 20200131.050328  ln -sf ${p}.${ext}  ${p}.9p8GeV_fixedTarget_2020.${ext}                       
ln -sf ${p}.19p5GeV_fixedTarget_2020.${ext}                ${p}.20200201.191905.${ext} # 20200201.191904  ln -sf ${p}.${ext}  ${p}.19p5GeV_fixedTarget_2020.${ext}                      
ln -sf ${p}.13p5GeV_fixedTarget_2020.${ext}                ${p}.20200202.160410.${ext} # 20200202.160409  ln -sf ${p}.${ext}  ${p}.13p5GeV_fixedTarget_2020.${ext}                      
ln -sf ${p}.9p2GeV_2020.${ext}                             ${p}.20200203.202535.${ext} # 20200203.202534  ln -sf ${p}.${ext}  ${p}.9p2GeV_2020.${ext}                                   
ln -sf ${p}.7p3GeV_fixedTarget_2020.${ext}                 ${p}.20200204.053519.${ext} # 20200204.053518  ln -sf ${p}.${ext}  ${p}.7p3GeV_fixedTarget_2020.${ext}                       
ln -sf ${p}.9p2GeV_2020.${ext}                             ${p}.20200205.144627.${ext} # 20200205.144626  ln -sf ${p}.${ext}  ${p}.9p2GeV_2020.${ext}                                   
ln -sf ${p}.5p75GeV_fixedTarget_2020.${ext}                ${p}.20200213.152340.${ext} # 20200213.152339  ln -sf ${p}.${ext}  ${p}.5p75GeV_fixedTarget_2020.${ext}       
ln -sf ${p}.9p2GeVb_2020.${ext}                            ${p}.20200224.230741.${ext} # 20200224.230740  ln -sf ${p}.${ext}  ${p}.9p2GeVb_2020.${ext}                   
ln -sf ${p}.26p5GeV_fixedTarget_2020.${ext}                ${p}.20200730.015355.${ext} # 20200730.015354  ln -sf ${p}.${ext}  ${p}.26p5GeV_fixedTarget_2020.${ext}       
ln -sf ${p}.26p5GeV_fixedTarget_2020.${ext}                ${p}.20200813.213252.${ext} # 20200813.213251  ln -sf ${p}.${ext}  ${p}.26p5GeV_fixedTarget_2020.${ext}       
ln -sf ${p}.26p5GeV_fixedTarget_2020.${ext}                ${p}.20200819.192254.${ext} # 20200819.192253  ln -sf ${p}.${ext}  ${p}.26p5GeV_fixedTarget_2020.${ext}       
ln -sf ${p}.26p5GeV_fixedTarget_2020.${ext}                ${p}.20200827.143508.${ext} # 20200827.143507  ln -sf ${p}.${ext}  ${p}.26p5GeV_fixedTarget_2020.${ext}                   
ln -sf ${p}.26p5GeV_fixedTarget_2020.${ext}                ${p}.20200907.221142.${ext} # 20200907.221141  ln -sf ${p}.${ext}  ${p}.26p5GeV_fixedTarget_2020.${ext}							         
ln -sf ${p}.26p5GeV_fixedTarget_2020.${ext}                ${p}.20200912.102034.${ext} # 20200912.102033  ln -sf ${p}.${ext}  ${p}.26p5GeV_fixedTarget_2020.${ext}
													  							         
ln -sf ${p}.7p7GeV_2021.${ext}                             ${p}.20210131.193347.${ext} # 20210131.193346  ln -sf TpcSecRowB.7p7GeV_2021.C                             TpcSecRowB.20210131.193347.C # 20210131.193346 
ln -sf ${p}.3p85GeV_fixedTarget_2021.${ext}                ${p}.20210501.165438.${ext} # 20210501.165437  ln -sf TpcSecRowB.3p85GeV_fixedTarget_2021.C                TpcSecRowB.20210501.165438.C # 20210501.165437 
ln -sf ${p}.44p5GeV_fixedTarget_2021.${ext}                ${p}.20210506.070147.${ext} # 20210506.070146  ln -sf TpcSecRowB.44p5GeV_fixedTarget_2021.C                TpcSecRowB.20210506.070147.C # 20210506.070146        
ln -sf ${p}.70GeV_fixedTarget_2021.${ext}                  ${p}.20210507.031640.${ext} # 20210507.031639  ln -sf TpcSecRowB.70GeV_fixedTarget_2021.C                  TpcSecRowB.20210507.031640.C # 20210507.031639        
ln -sf ${p}.100GeV_fixedTarget_2021.${ext}                 ${p}.20210508.041527.${ext} # 20210508.041526  ln -sf TpcSecRowB.100GeV_fixedTarget_2021.C                 TpcSecRowB.20210508.041527.C # 20210508.041526 
ln -sf ${p}.OO_200GeV_2021.${ext}                          ${p}.20210510.134727.${ext} # 20210510.134726  ln -sf TpcSecRowB.OO_200GeV_2021.C                          TpcSecRowB.20210510.134727.C # 20210510.134726        
ln -sf ${p}.ps_OO_200GeV_2021.${ext}                       ${p}.20210513.083121.${ext} # 20210513.083120  ln -sf TpcSecRowB.ps_OO_200GeV_2021.C                       TpcSecRowB.20210513.083121.C # 20210513.083120     
ln -sf ${p}.OO_200GeV_2021.${ext}                          ${p}.20210513.084423.${ext} # 20210513.084422  ln -sf TpcSecRowB.OO_200GeV_2021.C                          TpcSecRowB.20210513.084423.C # 20210513.084422     
ln -sf ${p}.ps_OO_200GeV_2021.${ext}                       ${p}.20210513.175229.${ext} # 20210513.175228  ln -sf TpcSecRowB.ps_OO_200GeV_2021.C                       TpcSecRowB.20210513.175229.C # 20210513.175228     
ln -sf ${p}.OO_200GeV_2021.${ext}                          ${p}.20210513.182206.${ext} # 20210513.182205  ln -sf TpcSecRowB.OO_200GeV_2021.C                          TpcSecRowB.20210513.182206.C # 20210513.182205     
ln -sf ${p}.OO_fcsTiming_2021.${ext}                       ${p}.20210514.144729.${ext} # 20210514.144728  ln -sf TpcSecRowB.OO_fcsTiming.C                            TpcSecRowB.20210514.144729.C # 20210514.144728     
ln -sf ${p}.OO_200GeV_2021.${ext}                          ${p}.20210514.150241.${ext} # 20210514.150240  ln -sf TpcSecRowB.OO_200GeV_2021.C                          TpcSecRowB.20210514.150241.C # 20210514.150240     
ln -sf ${p}.ps_OO_200GeV_2021.${ext}                       ${p}.20210516.112016.${ext} # 20210516.112015  ln -sf TpcSecRowB.ps_OO_200GeV_2021.C                       TpcSecRowB.20210516.112016.C # 20210516.112015     
ln -sf ${p}.FF_OO_200GeV_2021.${ext}                       ${p}.20210522.023643.${ext} # 20210522.023642  ln -sf TpcSecRowB.FF_OO_200GeV_2021.C                       TpcSecRowB.20210522.023643.C # 20210522.023642     
ln -sf ${p}.17p3GeV_2021.${ext}                            ${p}.20210525.113237.${ext} # 20210525.113236  ln -sf TpcSecRowB.17p3GeV_2021.C                            TpcSecRowB.20210525.113237.C # 20210525.113236     
ln -sf ${p}.26p5GeV_fixedTarget_2021.${ext}                ${p}.20210604.023046.${ext} # 20210604.023045  ln -sf TpcSecRowB.26p5GeV_fixedTarget_2021.C                TpcSecRowB.20210604.023046.C # 20210604.023045     
ln -sf ${p}.17p3GeV_2021.${ext}                            ${p}.20210604.081456.${ext} # 20210604.081455  ln -sf TpcSecRowB.17p3GeV_2021.C                            TpcSecRowB.20210604.081456.C # 20210604.081455     
ln -sf ${p}.3p85GeV_fixedTarget_2021.${ext}                ${p}.20210608.004952.${ext} # 20210608.004951  ln -sf TpcSecRowB.3p85GeV_fixedTarget_2021.C                TpcSecRowB.20210608.004952.C # 20210608.004951 
ln -sf ${p}.26p5GeV_fixedTarget_2021.${ext}                ${p}.20210617.222551.${ext} # 20210617.222550  ln -sf TpcSecRowB.26p5GeV_fixedTarget_2021.C                TpcSecRowB.20210617.222551.C # 20210617.222550 
ln -sf ${p}.3p85GeV_fixedTarget_2021.${ext}                ${p}.20210618.040936.${ext} # 20210618.040935  ln -sf TpcSecRowB.3p85GeV_fixedTarget_2021.C                TpcSecRowB.20210618.040936.C # 20210618.040935 
ln -sf ${p}.26p5GeV_fixedTarget_2021.${ext}                ${p}.20210627.152748.${ext} # 20210627.152747  ln -sf TpcSecRowB.26p5GeV_fixedTarget_2021.C                TpcSecRowB.20210627.152748.C # 20210627.152747 
ln -sf ${p}.3p85GeV_fixedTarget_2021.${ext}                ${p}.20210628.115429.${ext} # 20210628.115428  ln -sf TpcSecRowB.3p85GeV_fixedTarget_2021.C                TpcSecRowB.20210628.115429.C # 20210628.115428 
ln -sf ${p}.dAu200GeV_2021.${ext}                             ${p}.20210630.022850.${ext} # 20210630.022849  ln -sf TpcSecRowB.dAu200GeV_2021.C                             TpcSecRowB.20210630.022850.C # 20210630.022849 
ln -sf ${p}.26p5GeV_fixedTarget_2021.${ext}                ${p}.20210703.010651.${ext} # 20210703.010650  ln -sf TpcSecRowB.26p5GeV_fixedTarget_2021.C                TpcSecRowB.20210703.010651.C # 20210703.010650 
ln -sf ${p}.dAu200GeV_2021.${ext}                             ${p}.20210703.053034.${ext} # 20210703.053033  ln -sf TpcSecRowB.dAu200GeV_2021.C                             TpcSecRowB.20210703.053034.C # 20210703.053033 
ln -sf ${p}.26p5GeV_fixedTarget_2021.${ext}                ${p}.20210703.230658.${ext} # 20210703.230657  ln -sf TpcSecRowB.26p5GeV_fixedTarget_2021.C                TpcSecRowB.20210703.230658.C # 20210703.230657 
ln -sf ${p}.dAu200GeV_2021.${ext}                             ${p}.20210704.034047.${ext} # 20210704.034046  ln -sf TpcSecRowB.dAu200GeV_2021.C                             TpcSecRowB.20210704.034047.C # 20210704.034046 

ln -sf ${p}.pp500GeV_2022.${ext}                              ${p}.20220205.165806.${ext} # 20220205.165805  ln -sf TpcSecRowB.pp500GeV_2022.C                              TpcSecRowB.20211215.081703.C # 20211215.081702 
#ln -sf ${p}.zeroFieldAligment_2022.${ext}                  ${p}.20220207.204124.${ext} # 20220207.204123  ln -sf TpcSecRowB.zeroFieldAligment_2022.C                  TpcSecRowB.20220207.204124.C # 20220207.204123 
#ln -sf ${p}.pp500GeV_2022.${ext}                              ${p}.20220208.033748.${ext} # 20220208.033747  ln -sf TpcSecRowB.pp500GeV_2022.C                              TpcSecRowB.20220208.033748.C # 20220208.033747 
#ln -sf ${p}.zeroFieldAligment_2022.${ext}                  ${p}.20220209.225512.${ext} # 20220209.225511  ln -sf TpcSecRowB.zeroFieldAligment_2022.C                  TpcSecRowB.20220209.225512.C # 20220209.225511 
#ln -sf ${p}.pp500GeV_2022.${ext}                              ${p}.20220210.112620.${ext} # 20220210.112619  ln -sf TpcSecRowB.pp500GeV_2022.C                              TpcSecRowB.20220210.112620.C # 20220210.112619 
#ln -sf ${p}.zeroFieldAligment_2022.${ext}                  ${p}.20220304.185314.${ext} # 20220304.185313  ln -sf TpcSecRowB.zeroFieldAligment_2022.C                  TpcSecRowB.20220304.185314.C # 20220304.185313 
#ln -sf ${p}.pp500GeV_2022.${ext}                              ${p}.20220305.042127.${ext} # 20220305.042126  ln -sf TpcSecRowB.pp500GeV_2022.C                              TpcSecRowB.20220305.042127.C # 20220305.042126 
#ln -sf ${p}.zeroFieldAligment_2022.${ext}                  ${p}.20220313.051900.${ext} # 20220313.051859  ln -sf TpcSecRowB.zeroFieldAligment_2022.C                  TpcSecRowB.20220313.051900.C # 20220313.051859 
#ln -sf ${p}.pp500GeV_2022.${ext}                              ${p}.20220313.141509.${ext} # 20220313.141508  ln -sf TpcSecRowB.pp500GeV_2022.C                              TpcSecRowB.20220313.141509.C # 20220313.141508 


ln -sf ${p}.${ext}  ${p}.COLGeV_2019.${ext}
ln -sf ${p}.COLGeV_2019.${ext}  ${p}.7p7GeV_2019.${ext}                             
ln -sf ${p}.COLGeV_2019.${ext}  ${p}.9p2GeV_2019.${ext}                             
ln -sf ${p}.COLGeV_2019.${ext}  ${p}.14p5GeV_2019.${ext}                            
ln -sf ${p}.COLGeV_2019.${ext}  ${p}.19GeV_2019.${ext}                              
ln -sf ${p}.COLGeV_2019.${ext}  ${p}.AuAu200GeV_2019.${ext}                            

ln -sf ${p}.${ext}  ${p}.fixedTarget_2019.${ext}
ln -sf ${p}.fixedTarget_2019.${ext}  ${p}.3p85GeV_fixedTarget_2019.${ext}                
ln -sf ${p}.fixedTarget_2019.${ext}  ${p}.4p59GeV_fixedTarget_2019.${ext} 
ln -sf ${p}.fixedTarget_2019.${ext}  ${p}.7.3GeV_fixedTarget_2019.${ext}                 
ln -sf ${p}.fixedTarget_2019.${ext}  ${p}.31GeV_fixedTarget_2019.${ext}                  
							    
ln -sf ${p}.${ext}  ${p}.COLGeV_2020.${ext}
ln -sf ${p}.COLGeV_2020.${ext}  ${p}.7p7GeV_2020.${ext}                             
ln -sf ${p}.COLGeV_2020.${ext}  ${p}.9p2GeV_2020.${ext}                             
ln -sf ${p}.COLGeV_2020.${ext}  ${p}.9p2GeVb_2020.${ext}                            
ln -sf ${p}.COLGeV_2020.${ext}  ${p}.9p2GeVc_2020.${ext}                            
ln -sf ${p}.COLGeV_2020.${ext}  ${p}.11p5GeV_2020.${ext}                            

ln -sf ${p}.${ext}  ${p}.fixedTarget_2020.${ext}
ln -sf ${p}.fixedTarget_2020.${ext}  ${p}.5p75GeV_fixedTarget_2020.${ext}                
ln -sf ${p}.fixedTarget_2020.${ext}  ${p}.7p3GeV_fixedTarget_2020.${ext}                 
ln -sf ${p}.fixedTarget_2020.${ext}  ${p}.9p8GeV_fixedTarget_2020.${ext}                 
ln -sf ${p}.fixedTarget_2020.${ext}  ${p}.13p5GeV_fixedTarget_2020.${ext}                
ln -sf ${p}.fixedTarget_2020.${ext}  ${p}.19p5GeV_fixedTarget_2020.${ext}                
ln -sf ${p}.fixedTarget_2020.${ext}  ${p}.26p5GeV_fixedTarget_2020.${ext}                
ln -sf ${p}.fixedTarget_2020.${ext}  ${p}.31p2GeV_fixedTarget_2020.${ext}                
							    
ln -sf ${p}.${ext}  ${p}.COLGeV_2021.${ext}
ln -sf ${p}.COLGeV_2021.${ext}  ${p}.7p7GeV_2021.${ext}                             
ln -sf ${p}.COLGeV_2021.${ext}  ${p}.17p3GeV_2021.${ext}                            
ln -sf ${p}.COLGeV_2021.${ext}  ${p}.OO_200GeV_2021.${ext}                          
ln -sf ${p}.COLGeV_2021.${ext}  ${p}.ps_OO_200GeV_2021.${ext}                       
ln -sf ${p}.COLGeV_2021.${ext}  ${p}.OO_fcsTiming_2021.${ext}                       
ln -sf ${p}.COLGeV_2021.${ext}  ${p}.FF_OO_200GeV_2021.${ext}                       
ln -sf ${p}.COLGeV_2021.${ext}  ${p}.dAu200GeV_2021.${ext}                             

ln -sf ${p}.${ext}  ${p}.fixedTarget_2021.${ext}
ln -sf ${p}.fixedTarget_2021.${ext}  ${p}.3p85GeV_fixedTarget_2021.${ext}                
ln -sf ${p}.fixedTarget_2021.${ext}  ${p}.26p5GeV_fixedTarget_2021.${ext}                
ln -sf ${p}.fixedTarget_2021.${ext}  ${p}.44p5GeV_fixedTarget_2021.${ext}                
ln -sf ${p}.fixedTarget_2021.${ext}  ${p}.70GeV_fixedTarget_2021.${ext}                  
ln -sf ${p}.fixedTarget_2021.${ext}  ${p}.100GeV_fixedTarget_2021.${ext}                 


							    
ln -sf ${p}.pp500GeV_2022.${ext}                              ${p}.20211215.081703.${ext} # 20211215.081702 

ln -sf ${p}.${ext}  ${p}.pp500GeV_2022.${ext}                              
end
#================================================================================
# ln -sf TpcSecRowB.19GeV_2019.C                              TpcSecRowB.20190225.202321.C # 20190225.202320 
# ln -sf TpcSecRowB.14p5GeV_2019.C                            TpcSecRowB.20190404.094406.C # 20190404.094405 
# ln -sf TpcSecRowB.7.3GeV_fixedTarget_2019.C                 TpcSecRowB.20190417.133402.C # 20190417.133401 
# ln -sf TpcSecRowB.14p5GeV_2019.C                            TpcSecRowB.20190417.141247.C # 20190417.141246 
# ln -sf TpcSecRowB.7.3GeV_fixedTarget_2019.C                 TpcSecRowB.20190423.155419.C # 20190423.155418 
# ln -sf TpcSecRowB.14p5GeV_2019.C                            TpcSecRowB.20190423.181159.C # 20190423.181158 
# ln -sf TpcSecRowB.7p7GeV_2019.C                             TpcSecRowB.20190603.223656.C # 20190603.223655 
# ln -sf TpcSecRowB.3p85GeV_fixedTarget_2019.C                TpcSecRowB.20190607.174703.C # 20190607.174702 
# ln -sf TpcSecRowB.7p7GeV_2019.C                             TpcSecRowB.20190607.202245.C # 20190607.202244 
# ln -sf TpcSecRowB.3p85GeV_fixedTarget_2019.C                TpcSecRowB.20190609.124529.C # 20190609.124528 
# ln -sf TpcSecRowB.7p7GeV_2019.C                             TpcSecRowB.20190610.024110.C # 20190610.024109 
# ln -sf TpcSecRowB.7.3GeV_fixedTarget_2019.C                 TpcSecRowB.20190618.143659.C # 20190618.143658 
# ln -sf TpcSecRowB.7p7GeV_2019.C                             TpcSecRowB.20190619.025127.C # 20190619.025126 
# ln -sf TpcSecRowB.9p2GeV_2019.C                             TpcSecRowB.20190628.081932.C # 20190628.081931 
# ln -sf TpcSecRowB.4p59GeV_fixedTarget_2019.C                TpcSecRowB.20190629.032835.C # 20190629.032834 
# ln -sf TpcSecRowB.9p2GeV_2019.C                             TpcSecRowB.20190702.144152.C # 20190702.144151 
# ln -sf TpcSecRowB.31GeV_fixedTarget_2019.C                  TpcSecRowB.20190709.032313.C # 20190709.032312 
# ln -sf TpcSecRowB.AuAu200GeV_2019.C                            TpcSecRowB.20190710.110158.C # 20190710.110157 
# ln -sf TpcSecRowB.9p2GeV_2019.C                             TpcSecRowB.20190715.085142.C # 20190715.085141 
# 
# ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191208.091309.C # 20191208.091308 
# ln -sf TpcSecRowB.5p75GeV_fixedTarget_2020.C                TpcSecRowB.20191221.154022.C # 20191221.154021 
# ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191221.190033.C # 20191221.190032 
# ln -sf TpcSecRowB.31p2GeV_fixedTarget_2020.C                TpcSecRowB.20200128.182913.C # 20200128.182912 
# ln -sf TpcSecRowB.9p8GeV_fixedTarget_2020.C                 TpcSecRowB.20200130.005841.C # 20200130.005840 
# ln -sf TpcSecRowB.9p2GeV_2020.C                             TpcSecRowB.20200131.012113.C # 20200131.012112 
# ln -sf TpcSecRowB.9p8GeV_fixedTarget_2020.C                 TpcSecRowB.20200131.050329.C # 20200131.050328 
# ln -sf TpcSecRowB.19p5GeV_fixedTarget_2020.C                TpcSecRowB.20200201.191905.C # 20200201.191904 
# ln -sf TpcSecRowB.13p5GeV_fixedTarget_2020.C                TpcSecRowB.20200202.160410.C # 20200202.160409 
# ln -sf TpcSecRowB.9p2GeV_2020.C                             TpcSecRowB.20200203.202535.C # 20200203.202534 
# ln -sf TpcSecRowB.7p3GeV_fixedTarget_2020.C                 TpcSecRowB.20200204.053519.C # 20200204.053518 
# ln -sf TpcSecRowB.9p2GeV_2020.C                             TpcSecRowB.20200205.144627.C # 20200205.144626 
# ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20200210.220429.C # 20200210.220428 
# ln -sf TpcSecRowB.5p75GeV_fixedTarget_2020.C                TpcSecRowB.20200213.152340.C # 20200213.152339 
# ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20200214.143743.C # 20200214.143742 
# ln -sf TpcSecRowB.9p2GeVb_2020.C                            TpcSecRowB.20200224.230741.C # 20200224.230740 
# ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200617.233209.C # 20200617.233208 
# ln -sf TpcSecRowB.26p5GeV_fixedTarget_2020.C                TpcSecRowB.20200730.015355.C # 20200730.015354 
# ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200730.042837.C # 20200730.042836 
# ln -sf TpcSecRowB.26p5GeV_fixedTarget_2020.C                TpcSecRowB.20200813.213252.C # 20200813.213251 
# ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200814.054358.C # 20200814.054357 
# ln -sf TpcSecRowB.26p5GeV_fixedTarget_2020.C                TpcSecRowB.20200819.192254.C # 20200819.192253 
# ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200820.055403.C # 20200820.055402 
# ln -sf TpcSecRowB.26p5GeV_fixedTarget_2020.C                TpcSecRowB.20200827.143508.C # 20200827.143507 
# ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200828.064251.C # 20200828.064250 
# ln -sf TpcSecRowB.7p7GeV_2020.C                             TpcSecRowB.20200902.105441.C # 20200902.105440 
# ln -sf TpcSecRowB.26p5GeV_fixedTarget_2020.C                TpcSecRowB.20200907.221142.C # 20200907.221141 
# ln -sf TpcSecRowB.7p7GeV_2020.C                             TpcSecRowB.20200910.041640.C # 20200910.041639 
# ln -sf TpcSecRowB.26p5GeV_fixedTarget_2020.C                TpcSecRowB.20200912.102034.C # 20200912.102033 
# 
# ln -sf TpcSecRowB.7p7GeV_2021.C                             TpcSecRowB.20210131.193347.C # 20210131.193346 
# ln -sf TpcSecRowB.3p85GeV_fixedTarget_2021.C                TpcSecRowB.20210501.165438.C # 20210501.165437 
# ln -sf TpcSecRowB.44p5GeV_fixedTarget_2021.C                TpcSecRowB.20210506.070147.C # 20210506.070146 
# ln -sf TpcSecRowB.70GeV_fixedTarget_2021.C                  TpcSecRowB.20210507.031640.C # 20210507.031639 
# ln -sf TpcSecRowB.100GeV_fixedTarget_2021.C                 TpcSecRowB.20210508.041527.C # 20210508.041526 
# ln -sf TpcSecRowB.OO_200GeV_2021.C                          TpcSecRowB.20210510.134727.C # 20210510.134726 
# ln -sf TpcSecRowB.ps_OO_200GeV_2021.C                       TpcSecRowB.20210513.083121.C # 20210513.083120 
# ln -sf TpcSecRowB.OO_200GeV_2021.C                          TpcSecRowB.20210513.084423.C # 20210513.084422 
# ln -sf TpcSecRowB.ps_OO_200GeV_2021.C                       TpcSecRowB.20210513.175229.C # 20210513.175228 
# ln -sf TpcSecRowB.OO_200GeV_2021.C                          TpcSecRowB.20210513.182206.C # 20210513.182205 
# ln -sf TpcSecRowB.OO_fcsTiming.C                            TpcSecRowB.20210514.144729.C # 20210514.144728 
# ln -sf TpcSecRowB.OO_200GeV_2021.C                          TpcSecRowB.20210514.150241.C # 20210514.150240 
# ln -sf TpcSecRowB.ps_OO_200GeV_2021.C                       TpcSecRowB.20210516.112016.C # 20210516.112015 
# ln -sf TpcSecRowB.FF_OO_200GeV_2021.C                       TpcSecRowB.20210522.023643.C # 20210522.023642 
# ln -sf TpcSecRowB.17p3GeV_2021.C                            TpcSecRowB.20210525.113237.C # 20210525.113236 
# ln -sf TpcSecRowB.26p5GeV_fixedTarget_2021.C                TpcSecRowB.20210604.023046.C # 20210604.023045 
# ln -sf TpcSecRowB.17p3GeV_2021.C                            TpcSecRowB.20210604.081456.C # 20210604.081455 
# ln -sf TpcSecRowB.3p85GeV_fixedTarget_2021.C                TpcSecRowB.20210608.004952.C # 20210608.004951 
# ln -sf TpcSecRowB.26p5GeV_fixedTarget_2021.C                TpcSecRowB.20210617.222551.C # 20210617.222550 
# ln -sf TpcSecRowB.3p85GeV_fixedTarget_2021.C                TpcSecRowB.20210618.040936.C # 20210618.040935 
# ln -sf TpcSecRowB.26p5GeV_fixedTarget_2021.C                TpcSecRowB.20210627.152748.C # 20210627.152747 
# ln -sf TpcSecRowB.3p85GeV_fixedTarget_2021.C                TpcSecRowB.20210628.115429.C # 20210628.115428 
# ln -sf TpcSecRowB.dAu200GeV_2021.C                             TpcSecRowB.20210630.022850.C # 20210630.022849 
# ln -sf TpcSecRowB.26p5GeV_fixedTarget_2021.C                TpcSecRowB.20210703.010651.C # 20210703.010650 
# ln -sf TpcSecRowB.dAu200GeV_2021.C                             TpcSecRowB.20210703.053034.C # 20210703.053033 
# ln -sf TpcSecRowB.26p5GeV_fixedTarget_2021.C                TpcSecRowB.20210703.230658.C # 20210703.230657 
# ln -sf TpcSecRowB.dAu200GeV_2021.C                             TpcSecRowB.20210704.034047.C # 20210704.034046 
# 
# ln -sf TpcSecRowB.pp500GeV_2022.C                              TpcSecRowB.20211215.081703.C # 20211215.081702 
# ln -sf TpcSecRowB.zeroFieldAligment_2022.C                  TpcSecRowB.20220207.204124.C # 20220207.204123 
# ln -sf TpcSecRowB.pp500GeV_2022.C                              TpcSecRowB.20220208.033748.C # 20220208.033747 
# ln -sf TpcSecRowB.zeroFieldAligment_2022.C                  TpcSecRowB.20220209.225512.C # 20220209.225511 
# ln -sf TpcSecRowB.pp500GeV_2022.C                              TpcSecRowB.20220210.112620.C # 20220210.112619 
# ln -sf TpcSecRowB.zeroFieldAligment_2022.C                  TpcSecRowB.20220304.185314.C # 20220304.185313 
# ln -sf TpcSecRowB.pp500GeV_2022.C                              TpcSecRowB.20220305.042127.C # 20220305.042126 
# ln -sf TpcSecRowB.zeroFieldAligment_2022.C                  TpcSecRowB.20220313.051900.C # 20220313.051859 
# ln -sf TpcSecRowB.pp500GeV_2022.C                              TpcSecRowB.20220313.141509.C # 20220313.141508 
