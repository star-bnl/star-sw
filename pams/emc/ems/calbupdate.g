      module    calbupdate is a system
      author    OGAWA, Akio
      created   1998.Sep.21
      integer   iprin,istat/0/

      Structure CALG { version,  Rmin,     Etacut,   CrackWd,
                       FrontThk, CompThk,  AirThk,   BackThk,  SpaceThk,
                       ScintThk, AbsorThk, AbPapThk, Sntchthk, g10SbThk,
                       SmAlfWdh, SmAlfThk, SmGasThk, SmGasWdh, SmGasRad,
                       SmAffWdh, SmAfbWdh, SmetaWdh, Seta1Wdh, Netfirst,
                       Seta2Wdh, Netsecon, Set12Wdh, SphiWdh,  SphidWdh,
                       NPhistr,  NSmdAlw,  Nsuper  , Nsmd,     NsubLay(2),
                       Nmodule(2), Shift }
      begin
      calg_nmodule(1) =60
      calg_nmodule(2) =60
      calg_shift =75
      use  CALBGEO/CALG  
      print *,'Old version was ',calg_version 
      flag_calg = 0
      fill CALG(1)  		! more system data
      version = 2.5             !  bank version
      print *,'Update CALB/CALG to version 2.5'
      print *,'version, Nmodule, Shift ', 
     +  calg_version, calg_Nmodule(1),calg_Nmodule(2), calg_Shift
      end
