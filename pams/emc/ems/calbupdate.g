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
                       Nmodule(2), Shift(2) }
      begin
 
      use  CALBGEO/CALG 
      print *,'Update CALB/CALG to version 2.7'
      print *,'Old version(in GBANK) was ',calg_version 
*   
      flag_calg = 0

      if (calg_version=2.7) then   ! only g2t table was old verion
	fill CALG(1)            ! a better calg bank
          version  = 2.71       !  bank version
        endfill
      elseif (calg_shift(1)>0) then		
        calg_shift(2) = calg_shift(1)
 	fill CALG(1)            ! a better calg bank
          version  = 2.72       !  bank version
        endfill	
      else
        fill CALG(1)            ! a better calg bank
          version  = 2.73       !  bank version
          nmodule  = {60,60}    !  number of modules
          shift    = {75,75}    !  offset of the first sector
        endfill
      endif
*
      print *,'New version(in GBANK) is ',calg_version
      print *,'Nmodule(1,2), Shift(1,2) ', 
      calg_Nmodule(1),calg_Nmodule(2),calg_Shift(1),calg_Shift(2)

      end




