* $Id: phys_constants.inc,v 1.4 1999/04/13 21:08:36 sakrejda Exp $
* $Log: phys_constants.inc,v $
* Revision 1.4  1999/04/13 21:08:36  sakrejda
* #define replaced by the PARAMETER statements
*
* Revision 1.3  1998/05/25 23:54:06  fisyak
* replace +/- to PLUS/MINUS
*
* Revision 1.2  1998/03/05 00:16:08  fisyak
* Missing C_D_CURVATURE
*
* Revision 1.1  1998/02/19 15:56:22  fisyak
* Math. Phys. constants from H.Ward
*
      DOUBLE PRECISION  C_C_LIGHT,  C_ELECT_CHARGE_MAG, C_AMU, C_ALPHA,
     >  C_AVOGAD, C_PLABRC, C_RCLSEL, C_DGEV, C_ELCCGS, C_K_BOLTZMAN,
     >  C_G_GRAV, C_MOLAR_VOL, C_D_CURVATURE, M_GAMMA,        
     >  L_GAMMA, M_POSITRON, L_POSITRON, M_ELECTRON, L_ELECTRON ,M_NEUTRINO,
     >  L_NEUTRINO   ,M_MUON_PLUS     ,L_MUON_PLUS  ,M_MUON_MINUS   
     >  ,L_MUON_MINUS ,M_PION_0        ,L_PION_0      ,M_PION_PLUS     
     >  ,L_PION_PLUS   ,M_PION_MINUS   ,L_PION_MINUS  ,M_KAON_0_LONG   
     >  ,L_KAON_0_LONG ,M_KAON_PLUS      ,L_KAON_PLUS   ,M_KAON_MINUS    
     >  ,L_KAON_MINUS  ,M_NEUTRON        ,L_NEUTRON     ,M_PROTON        
     >  ,L_PROTON       ,M_ANTIPROTON    ,L_ANTIPROTON   ,M_KAON_0_SHORT  
     >  ,L_KAON_0_SHORT ,M_ETA           ,L_ETA          ,M_LAMBDA        
     >  ,L_LAMBDA       ,M_SIGMA_PLUS    ,L_SIGMA_PLUS   ,M_SIGMA_0       
     >  ,L_SIGMA_0      ,M_SIGMA_MINUS   ,L_SIGMA_MINUS  ,M_XI_0
 
      DOUBLE PRECISION L_XI_0 ,M_XI_MINUS   ,L_XI_MINUS     ,M_OMEGA_MINUS   
     >  ,L_OMEGA_MINUS  ,M_ANTINEUTRON      ,L_ANTINEUTRON   ,M_ANTILAMBDA     
     >  ,L_ANTILAMBDA   ,M_ANTISIGMA_MINUS  ,L_ANTISIGMA_MINUS  ,M_ANTISIGMA_0
     >  ,L_ANTISIGMA_0  ,M_ANTISIGMA_PLUS   ,L_ANTISIGMA_PLUS  ,M_ANTIXI_0
     >  ,L_ANTIXI_0     ,M_ANTIXI_PLUS      ,L_ANTIXI_PLUS  ,M_ANTIOMEGA_PLUS  
     >  ,L_ANTIOMEGA_PLUS  ,M_TAU_PLUS      ,L_TAU_PLUS     ,M_TAU_MINUS       
     >  ,L_TAU_MINUS    ,M_D_PLUS           ,L_D_PLUS       ,M_D_MINUS         
     >  ,L_D_MINUS      ,M_D_0              ,L_D_0          ,M_ANTI_D_0        
     >  ,L_ANTI_D_0     ,M_DSPLUS           ,L_DSPLUS       ,M_DSMINUS         
     >  ,L_DSMINUS      ,M_LAMBDA_C_PLUS    ,L_LAMBDA_C_PLUS,M_W_PLUS          
     >  ,L_W_PLUS       ,M_W_MINUS          ,L_W_MINUS      ,M_Z_0             
     >  ,L_Z_0          ,M_DEUTERON         ,L_DEUTERON     ,M_TRITON          
     >  ,L_TRITON       ,M_ALPHA            ,L_ALPHA        ,M_GEANTINO
        
      DOUBLE PRECISION L_GEANTINO,M_HE3  ,L_HE3   ,M_CERENKOV        
     >  ,L_CERENKOV ,M_LI6             ,L_LI6     ,M_LI7             
     >  ,L_LI7      ,M_BE7             ,L_BE7     ,M_BE9             
     >  ,L_BE9     ,M_B10              ,L_B10     ,M_B11             
     >  ,L_B11     ,M_C12              ,L_C12     ,M_N14             
     >  ,L_N14     ,M_O16              ,L_O16     ,M_F19             
     >  ,L_F19     ,M_NE20             ,L_NE20    ,M_NA23            
     >  ,L_NA23    ,M_MG24             ,L_MG24    ,M_AL27            
     >  ,L_AL27    ,M_SI28             ,L_SI28    ,M_P31             
     >  ,L_P31     ,M_S32              ,L_S32     ,M_CL35            
     >  ,L_CL35    ,M_AR36             ,L_AR36    ,M_K39             
     >  ,L_K39     ,M_CA40             ,L_CA40    ,M_SC45            
     >  ,L_SC45    ,M_TI48             ,L_TI48    ,M_V51
             
      DOUBLE PRECISION L_V51 ,M_CR52   ,L_CR52    ,M_MN55            
     >  ,L_MN55    ,M_FE56             ,L_FE56    ,M_CO59            
     >  ,L_CO59    ,M_NI58             ,L_NI58    ,M_CU63            
     >  ,L_CU63    ,M_ZN64             ,L_ZN64    ,M_GE74            
     >  ,L_GE74    ,M_SE80             ,L_SE80    ,M_KR84            
     >  ,L_KR84    ,M_SR88             ,L_SR88    ,M_ZR90            
     >  ,L_ZR90    ,M_MO98             ,L_MO98    ,M_PD106
     >  ,L_PD106   ,M_CD114            ,L_CD114  ,M_SN120           
     >  ,L_SN120   ,M_XE132           ,L_XE132   ,M_BA138           
     >  ,L_BA138   ,M_CE140           ,L_CE140   ,M_SM152           
     >  ,L_SM152   ,M_DY164           ,L_DY164   ,M_YB174           
     >  ,L_YB174   ,M_W184            ,L_W184    ,M_PT194
     >  ,L_PT194   ,M_AU197 
     >  ,L_AU197   ,M_HG202           ,L_HG202   ,M_PB208
     >  ,L_PB208   ,M_U238            ,L_U238            


      PARAMETER( C_C_LIGHT =          2.99792458D10    ) ! speed light cm/s 
      PARAMETER( C_ELECT_CHARGE_MAG = 1.60217733D-19   ) ! electron charge C 
      PARAMETER( C_AMU  =             0.93149432       ) ! unified amu (u) GeV/c^2 
      PARAMETER( C_ALPHA =            1.0/137.035989561  ) ! Fine structure const 
      PARAMETER( C_AVOGAD =           6.0221367D23     ) ! Avogadro's number 
      PARAMETER( C_PLABRC =           1.97327053D-14   ) ! hbar-c (GeV-cm) 
      PARAMETER( C_RCLSEL =           2.8179409183694872D-13  ) ! e- radius (cm) 
      PARAMETER( C_DGEV =             1.53538D-4       ) ! E-loss const (GeV/(g/cm^2)) 
      PARAMETER( C_ELCCGS =           4.8032068D-10    ) ! Electron charge (CGS) 
      PARAMETER( C_K_BOLTZMAN =       8.617385D-14     ) ! Boltzman k (GeV/deg. K) 
      PARAMETER( C_G_GRAV =           9.80665D2        ) ! Grav., sea level (cm/s^2) 
      PARAMETER( C_MOLAR_VOL =        2.241410D4       ) ! STP Molar volume (cm^3/mol) 
      PARAMETER( C_D_CURVATURE =      0.000299792458   ) ! (GeV/c)(1/kG)(1/cm) 
      PARAMETER(  M_GAMMA =           0.                ) ! mass     in GeV 
      PARAMETER(  L_GAMMA =           1.D+15            ) ! lifetime in sec 
      PARAMETER(  M_POSITRON =        0.0005109990615   ) ! mass     in GeV 
      PARAMETER(  L_POSITRON =        1.D+15            ) ! lifetime in sec 
      PARAMETER(  M_ELECTRON =        0.0005109990615   ) ! mass     in GeV 
      PARAMETER(  L_ELECTRON =        1.D+15            ) ! lifetime in sec 
      PARAMETER(  M_NEUTRINO =        0.                ) ! mass     in GeV 
      PARAMETER(  L_NEUTRINO =        1.D+15            ) ! lifetime in sec 
      PARAMETER(  M_MUON_PLUS =       0.105658387       ) ! mass     in GeV 
      PARAMETER(  L_MUON_PLUS =       2.19703D-06       ) ! lifetime in sec 
      PARAMETER(  M_MUON_MINUS =      0.105658387       ) ! mass     in GeV 
      PARAMETER(  L_MUON_MINUS =      2.19703D-06       ) ! lifetime in sec 
      PARAMETER(  M_PION_0 =          0.1349764         ) ! mass     in GeV 
      PARAMETER(  L_PION_0 =          0.84D-16          ) ! lifetime in sec 
      PARAMETER(  M_PION_PLUS =       0.1395700         ) ! mass     in GeV 
      PARAMETER(  L_PION_PLUS =       2.603D-08         ) ! lifetime in sec 
      PARAMETER(  M_PION_MINUS =      0.1395700         ) ! mass     in GeV 
      PARAMETER(  L_PION_MINUS =      2.603D-08         ) ! lifetime in sec 
      PARAMETER(  M_KAON_0_LONG =     0.497672          ) ! mass     in GeV 
      PARAMETER(  L_KAON_0_LONG =     5.17D-08          ) ! lifetime in sec 
      PARAMETER(  M_KAON_PLUS =       0.493677          ) ! mass     in GeV 
      PARAMETER(  L_KAON_PLUS =       1.237D-08         ) ! lifetime in sec 
      PARAMETER(  M_KAON_MINUS=       0.493677          ) ! mass     in GeV 
      PARAMETER(  L_KAON_MINUS =      1.237D-08         ) ! lifetime in sec 
      PARAMETER(  M_NEUTRON =         0.93956563        ) ! mass     in GeV 
      PARAMETER(  L_NEUTRON =         8.870D+02         ) ! lifetime in sec 
      PARAMETER(  M_PROTON =          0.9382723128      ) ! mass     in GeV 
      PARAMETER(  L_PROTON =          1.000000D+15      ) ! lifetime in sec 
      PARAMETER(  M_ANTIPROTON =      0.9382723128      ) ! mass     in GeV 
      PARAMETER(  L_ANTIPROTON =      1.000000D+15      ) ! lifetime in sec 
      PARAMETER(  M_KAON_0_SHORT =    0.497672          ) ! mass     in GeV 
      PARAMETER(  L_KAON_0_SHORT =    8.926D-11         ) ! lifetime in sec 
      PARAMETER(  M_ETA =             0.54745           ) ! mass     in GeV 
      PARAMETER(  L_ETA =             5.485D-19         ) ! lifetime in sec 
      PARAMETER(  M_LAMBDA =          1.115684          ) ! mass     in GeV 
      PARAMETER(  L_LAMBDA =          2.632D-10         ) ! lifetime in sec 
      PARAMETER(  M_SIGMA_PLUS =      1.18937           ) ! mass     in GeV 
      PARAMETER(  L_SIGMA_PLUS =      0.799D-10         ) ! lifetime in sec 
      PARAMETER(  M_SIGMA_0 =         1.19255           ) ! mass     in GeV 
      PARAMETER(  L_SIGMA_0 =         7.4D-20           ) ! lifetime in sec 
      PARAMETER(  M_SIGMA_MINUS =     1.197436          ) ! mass     in GeV 
      PARAMETER(  L_SIGMA_MINUS =     1.479D-10         ) ! lifetime in sec 
      PARAMETER(  M_XI_0 =            1.3149            ) ! mass     in GeV 
      PARAMETER(  L_XI_0 =            2.9D-10           ) ! lifetime in sec 
      PARAMETER(  M_XI_MINUS =        1.32132           ) ! mass     in GeV 
      PARAMETER(  L_XI_MINUS =        1.639D-10         ) ! lifetime in sec 
      PARAMETER(  M_OMEGA_MINUS =     1.67245           ) ! mass     in GeV 
      PARAMETER(  L_OMEGA_MINUS=      0.822D-10         ) ! lifetime in sec 
      PARAMETER(  M_ANTINEUTRON =     0.93956563        ) ! mass     in GeV 
      PARAMETER(  L_ANTINEUTRON =     8.870D+02         ) ! lifetime in sec 
      PARAMETER(  M_ANTILAMBDA =      1.115684          ) ! mass     in GeV 
      PARAMETER(  L_ANTILAMBDA =      2.632D-10         ) ! lifetime in sec 
      PARAMETER(  M_ANTISIGMA_MINUS=  1.18937           ) ! mass     in GeV 
      PARAMETER(  L_ANTISIGMA_MINUS=  0.799D-10         ) ! lifetime in sec 
      PARAMETER(  M_ANTISIGMA_0 =     1.19255           ) ! mass     in GeV 
      PARAMETER(  L_ANTISIGMA_0 =     7.4D-20           ) ! lifetime in sec 
      PARAMETER(  M_ANTISIGMA_PLUS =  1.197436          ) ! mass     in GeV 
      PARAMETER(  L_ANTISIGMA_PLUS =  1.479D-10         ) ! lifetime in sec 
      PARAMETER(  M_ANTIXI_0 =        1.3149            ) ! mass     in GeV 
      PARAMETER(  L_ANTIXI_0 =        2.9D-10           ) ! lifetime in sec 
      PARAMETER(  M_ANTIXI_PLUS =     1.32132           ) ! mass     in GeV 
      PARAMETER(  L_ANTIXI_PLUS =     1.639D-10         ) ! lifetime in sec 
      PARAMETER(  M_ANTIOMEGA_PLUS =  1.67245           ) ! mass     in GeV 
      PARAMETER(  L_ANTIOMEGA_PLUS =  0.822D-10         ) ! lifetime in sec 
      PARAMETER(  M_TAU_PLUS =        1.7771            ) ! mass     in GeV 
      PARAMETER(  L_TAU_PLUS =        3.040000D-13      ) ! lifetime in sec 
      PARAMETER(  M_TAU_MINUS =       1.7771            ) ! mass     in GeV 
      PARAMETER(  L_TAU_MINUS  =      3.040000D-13      ) ! lifetime in sec 
      PARAMETER(  M_D_PLUS  =         1.8694            ) ! mass     in GeV 
      PARAMETER(  L_D_PLUS =          1.062000D-12      ) ! lifetime in sec 
      PARAMETER(  M_D_MINUS =         1.8694            ) ! mass     in GeV 
      PARAMETER(  L_D_MINUS =         1.062000D-12      ) ! lifetime in sec 
      PARAMETER(  M_D_0 =             1.8646            ) ! mass     in GeV 
      PARAMETER(  L_D_0 =             4.280000D-13      ) ! lifetime in sec 
      PARAMETER(  M_ANTI_D_0 =        1.8646            ) ! mass     in GeV 
      PARAMETER(  L_ANTI_D_0 =        4.280000D-13      ) ! lifetime in sec 
      PARAMETER(  M_DSPLUS   =        1.9685            ) ! mass     in GeV 
      PARAMETER(  L_DSPLUS   =        4.360000D-13      ) ! lifetime in sec 
      PARAMETER(  M_DSMINUS  =        1.9685            ) ! mass     in GeV 
      PARAMETER(  L_DSMINUS  =        4.360000D-13      ) ! lifetime in sec 
      PARAMETER(  M_LAMBDA_C_PLUS =   2.2851            ) ! mass     in GeV 
      PARAMETER(  L_LAMBDA_C_PLUS =   1.790000D-13      ) ! lifetime in sec 
      PARAMETER(  M_W_PLUS  =         80.220            ) ! mass     in GeV 
      PARAMETER(  L_W_PLUS  =         3.160000D-25      ) ! lifetime in sec 
      PARAMETER(  M_W_MINUS =         80.220            ) ! mass     in GeV 
      PARAMETER(  L_W_MINUS =         2.640000D-25      ) ! lifetime in sec 
      PARAMETER(  M_Z_0 =             91.187            ) ! mass     in GeV 
      PARAMETER(  L_Z_0 =             7.740000D-26      ) ! lifetime in sec 
      PARAMETER(  M_DEUTERON =        1.875613          ) ! mass     in GeV 
      PARAMETER(  L_DEUTERON =        1.000000D+15      ) ! lifetime in sec 
      PARAMETER(  M_TRITON =          2.80925           ) ! mass     in GeV 
      PARAMETER(  L_TRITON =          1.000000D+15      ) ! lifetime in sec 
      PARAMETER(  M_ALPHA =           3.727417          ) ! mass     in GeV 
      PARAMETER(  L_ALPHA =           1.000000D+15      ) ! lifetime in sec 
      PARAMETER(  M_GEANTINO =        0.                ) ! mass     in GeV 
      PARAMETER(  L_GEANTINO =        1.000000D+15      ) ! lifetime in sec 
      PARAMETER(  M_HE3 =             2.80923           ) ! mass     in GeV 
      PARAMETER(  L_HE3 =             1.000000D+15      ) ! lifetime in sec 
      PARAMETER(  M_CERENKOV =        0.                ) ! mass     in GeV 
      PARAMETER(  L_CERENKOV =        1.000000D+15      ) ! lifetime in sec 
      PARAMETER(  M_LI6 =             5.60305           ) ! mass     in GeV 
      PARAMETER(  L_LI6 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_LI7 =             6.53536           ) ! mass     in GeV 
      PARAMETER(  L_LI7 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_BE7 =             6.53622           ) ! mass     in GeV 
      PARAMETER(  L_BE7 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_BE9 =             8.39479           ) ! mass     in GeV 
      PARAMETER(  L_BE9 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_B10 =             9.32699           ) ! mass     in GeV 
      PARAMETER(  L_B10 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_B11 =             10.25510          ) ! mass     in GeV 
      PARAMETER(  L_B11 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_C12 =             11.17793          ) ! mass     in GeV 
      PARAMETER(  L_C12 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_N14 =             13.0437           ) ! mass     in GeV 
      PARAMETER(  L_N14 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_O16 =             14.89917          ) ! mass     in GeV 
      PARAMETER(  L_O16 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_F19 =             17.69690          ) ! mass     in GeV 
      PARAMETER(  L_F19 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_NE20 =            18.62284          ) ! mass     in GeV 
      PARAMETER(  L_NE20 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_NA23 =            21.41483          ) ! mass     in GeV 
      PARAMETER(  L_NA23 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_MG24 =            22.34193          ) ! mass     in GeV 
      PARAMETER(  L_MG24 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_AL27 =            25.13314          ) ! mass     in GeV 
      PARAMETER(  L_AL27 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_SI28 =            26.06034          ) ! mass     in GeV 
      PARAMETER(  L_SI28 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_P31 =             28.85188          ) ! mass     in GeV 
      PARAMETER(  L_P31 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_S32 =             29.78180          ) ! mass     in GeV 
      PARAMETER(  L_S32 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_CL35 =            32.57328          ) ! mass     in GeV 
      PARAMETER(  L_CL35 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_AR36 =            33.50356          ) ! mass     in GeV 
      PARAMETER(  L_AR36 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_K39 =             36.29447          ) ! mass     in GeV 
      PARAMETER(  L_K39 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_CA40 =            37.22492          ) ! mass     in GeV 
      PARAMETER(  L_CA40 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_SC45 =            41.87617          ) ! mass     in GeV 
      PARAMETER(  L_SC45 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_TI48 =            44.66324          ) ! mass     in GeV 
      PARAMETER(  L_TI48 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_V51 =             47.45401          ) ! mass     in GeV 
      PARAMETER(  L_V51 =             1000.             ) ! lifetime in sec 
      PARAMETER(  M_CR52 =            48.38228          ) ! mass     in GeV 
      PARAMETER(  L_CR52 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_MN55 =            51.17447          ) ! mass     in GeV 
      PARAMETER(  L_MN55 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_FE56 =            52.10307          ) ! mass     in GeV 
      PARAMETER(  L_FE56 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_CO59 =            54.89593          ) ! mass     in GeV 
      PARAMETER(  L_CO59 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_NI58 =            53.96644          ) ! mass     in GeV 
      PARAMETER(  L_NI58 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_CU63 =            58.61856          ) ! mass     in GeV 
      PARAMETER(  L_CU63 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_ZN64 =            59.54963          ) ! mass     in GeV 
      PARAMETER(  L_ZN64 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_GE74 =            68.85715          ) ! mass     in GeV 
      PARAMETER(  L_GE74 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_SE80 =            74.44178          ) ! mass     in GeV 
      PARAMETER(  L_SE80 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_KR84 =            78.16309          ) ! mass     in GeV 
      PARAMETER(  L_KR84 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_SR88 =            81.88358          ) ! mass     in GeV 
      PARAMETER(  L_SR88 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_ZR90 =            83.74571          ) ! mass     in GeV 
      PARAMETER(  L_ZR90 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_MO98 =            91.19832          ) ! mass     in GeV 
      PARAMETER(  L_MO98 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_PD106 =           98.64997          ) ! mass     in GeV 
      PARAMETER(  L_PD106 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_CD114 =           106.10997         ) ! mass     in GeV 
      PARAMETER(  L_CD114 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_SN120 =           111.68821         ) ! mass     in GeV 
      PARAMETER(  L_SN120 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_XE132 =           122.86796         ) ! mass     in GeV 
      PARAMETER(  L_XE132 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_BA138 =           128.45793         ) ! mass     in GeV 
      PARAMETER(  L_BA138 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_CE140 =           130.32111         ) ! mass     in GeV 
      PARAMETER(  L_CE140 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_SM152 =           141.51236         ) ! mass     in GeV 
      PARAMETER(  L_SM152 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_DY164 =           152.69909         ) ! mass     in GeV 
      PARAMETER(  L_DY164 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_YB174 =           162.02245         ) ! mass     in GeV 
      PARAMETER(  L_YB174 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_W184 =            171.34924         ) ! mass     in GeV 
      PARAMETER(  L_W184 =            1000.             ) ! lifetime in sec 
      PARAMETER(  M_PT194 =           180.67513         ) ! mass     in GeV 
      PARAMETER(  L_PT194 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_AU197 =           183.47324         ) ! mass     in GeV 
      PARAMETER(  L_AU197 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_HG202 =           188.13451         ) ! mass     in GeV 
      PARAMETER(  L_HG202 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_PB208 =           193.72907         ) ! mass     in GeV 
      PARAMETER(  L_PB208 =           1000.             ) ! lifetime in sec 
      PARAMETER(  M_U238 =            221.74295         ) ! mass     in GeV 
      PARAMETER(  L_U238 =            1000.             ) ! lifetime in sec 















