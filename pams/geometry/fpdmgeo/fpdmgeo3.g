******************************************************************************
* $Id: fpdmgeo3.g,v 1.1 2006/11/28 00:02:42 potekhin Exp $
* $Name:  $
* $Log: fpdmgeo3.g,v $
* Revision 1.1  2006/11/28 00:02:42  potekhin
* First version of the FMS (FPD) that still needs work.
*
*
*
******************************************************************************
Module FPDMGEO3 is the Forward Pion Detector Modules GEOmetry
Created   27 Nov 2006
Author    Akio Ogawa
+CDE,AGECOM,GCONST,GCUNIT.
*
      Content    FBOX,FLGT,FLXF,FWAL,FLGR,FPRB,FPCT,FUMT,PBPT,FSHM,FHMS,FXGT
*     
      Structure FMCG {Version,ChkvSim,PbPlate}
      Structure FPOS {iMod,iType,X,Y,Z,AY,AZ}
      Structure FBXD {Type,Height,Depth,NX,NY,NXL,NYL,XOffset,ZOffset,PSOffset,SmdOff}
      Structure FLGG {Type,Width,Depth,DGap,AlThick,PhCathDz,PhCathR,MuMetDz,MuMetR}
      Structure FLGM {Type,Density,RadLen,PbCont,CritEne,MoliereR}
      Structure PBPD {Z,Width,Height,Thick}
      Structure FMXG {Version,Sapex,Sbase,Sgap,NStrip,G10Width,G10hgt,G10Thick}

      integer    ChkvSim,iMod,iType,Type,PbPlate
      Integer    i,j,m,serN

      Real       xx,yy,zz,x1,y1,z1,ztot,rtot,wid,wid2,bwid,x0
      Real       ztotsmd,wtotsmd,xxx,yyy,zzz,wsmd

      Integer    N
      Parameter (N=12)

* --- Enegry bins
      real E(N)  /1.776e-9, 1.912e-9,  2.072e-9, 2.26e-9, 2.486e-9, 2.702e-9,
                2.825e-9, 2.96e-9, 3.108e-9,  3.271e-9,  3.551e-9, 3.767e-9/

* --- Refraction indexes
      real rindex_PbG(N)    /N*1.67/
      real rindex_SiRub(N)  /N*1.67/
      real rindex_PhCath(N) /N*1.67/
      real rindex_Alm(N)    /N*1.0/
      real rindex_MuMet(N)  /N*1.0/

* ---- Absorbtion lenghts (in cm)
      real absco_PbG(N)     /N*38.0/
      real absco_SiRub(N)   /N*38.0/ 
      real absco_PhCath(N)  /N*0.0001/
      real absco_Alm(N)     /N*0.0001/
      real absco_MuMet(N)   /N*0.0001/

* ---- Detection efficiencies (quantum efficiency for Photo Cathode)
*     real effic_PhCath(N)  / N*1.0/
      real effic_PhCath(N)  / 0.06,  0.08,   0.105,  0.14,   0.155,  0.17,
                              0.18,  0.2,    0.19,   0.175,  0.135,  0.09/
      real effic_all(N)    /N*0.0/

      EXTERNAL  FFPDSTEP,FPCTSTEP
* ----------------------------------------------------------------------------
Fill  FMCG                          ! Fpd Calorimeter basic data 
      Version  = 8.0                ! Geometry version 
      ChkvSim  = 0                  ! = 0 dE, = 1 Cherenkov simulation for PbG
* NO CHERENKOV BY DEFAULT
      PbPlate  = 0                  ! =0 no plate, =1 with plate
EndFill
* ----------------------------------------------------------------------------
* Near of FY03 is x = 20.40-2.54 = 17.86 
* Far of  FY03 is x = 30.65-2.54 = 28.1 ===> 30.70-2.54=28.16
* Far of  FY05 is x = 50.70-2.54 = 48.19
* z position for N/S = 798 - 19 = 779
Fill  FPOS                          ! Fpd EN positioning
      iMod=1                        ! Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)
      iType=1                       ! Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6, 4=17*34+14*28)
      X=-48.19                      ! X distance from beam to edge of detector
      Y=0.0                         ! Y distance from beam to edge of detector
      Z=-779.0                      ! Z distance from IP to surface of detector
      AY=180                        ! Angle aroound Y (0 for west, 180 for east)
      AZ=0                          ! Angle around Z
     
EndFill

Fill  FPOS                          ! Fpd ES positioning
      iMod=2                        ! Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)
      iType=1                       ! Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6, 4=17*34+14*28)
      X=48.19                       ! X distance from beam to edge of detector
      Y=0.0                         ! Y distance from beam to edge of detector
      Z=-779.0                      ! Z distance from IP to surface of detector
      AY=180                        ! Angle aroound Y (0 for west, 180 for east)
      AZ=0                          ! Angle around Z
EndFill

Fill  FPOS                          ! Fpd ET positioning
      iMod=3                        ! Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)
      iType=2                       ! Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6, 4=17*34+14*28)
      X=0.0                         ! X distance from beam to edge of detector
      Y=30.8                        ! Y distance from beam to edge of detector
      Z=-590.2                      ! Z distance from IP to surface of detector
      AY=180                        ! Angle aroound Y (0 for west, 180 for east)
      AZ=0                          ! Angle around Z
EndFill

Fill  FPOS                          ! Fpd EB positioning
      iMod=4                        ! Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)
      iType=2                       ! Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6, 4=17*34+14*28)
      X=0.0                         ! X distance from beam to edge of detector
      Y=-30.2                       ! Y distance from beam to edge of detector
      Z=-590.2                      ! Z distance from IP to surface of detector
      AY=180                        ! Angle aroound Y (0 for west, 180 for east)
      AZ=0                          ! Angle around Z
EndFill

Fill  FPOS                          ! FMS WN positioning
      iMod=5                        ! Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)
      iType=4                       ! Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6, 4=17*34+14*28)
      X=-0.5                        ! X distance from beam to edge of detector
      Y=0.0                         ! Y distance from beam to edge of detector
      Z=720.0                       ! Z distance from IP to surface of detector
      AY=0                          ! Angle aroound Y (0 for west, 180 for east)
EndFill

Fill  FPOS                          ! FMS WS positioning
      iMod=6                        ! Module# (EN=1, ES=2, ET=3, EB=4, WN=5...)
      iType=4                       ! Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6, 4=17*34+14*28)
      X=0.5                         ! X distance from beam to edge of detector
      Y=0.0                         ! Y distance from beam to edge of detector
      Z=720.0                       ! Z distance from IP to surface of detector
      AY=0                          ! Angle aroound Y (0 for west, 180 for east)
      AZ=0                          ! Angle around Z
EndFill
* ----------------------------------------------------------------------------
Fill FBXD                           ! FPD Box Geometry
      Type=1                        ! Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6, 4=17*34+14*28)
      Height=100                    ! Box height
      Depth=96                      ! Box Depth
      NX=7                          ! Number of pbg in x
      NY=7                          ! number of pbg in y
      NXL=0                         ! Number of large pbg in x
      NYL=0                         ! number of large pbg in y
      XOffset=2.54                  ! tower x offset from box edge to PbG edge
      ZOffset=19                    ! tower z offset from box edge to PbG edge      
      PSOffset=2.0                  ! PreShower z offset from box edge to PbG edge
      SmdOff=8.0                    ! SMD V-plane z offset from box edge
EndFill
Fill FBXD                           ! FPD Box Geometry
      Type=2                        ! Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6, 4=17*34+14*28)
      Height=20                     ! Box height
      Depth=65                      ! Box Depth
      NX=5                          ! Number of pbg in x
      NY=5                          ! number of pbg in y
      NXL=0                         ! Number of large pbg in x
      NYL=0                         ! number of large pbg in y
      XOffset=0.0                   ! tower x offset from box edge to PbG edge
      ZOffset=1                     ! tower z offset from box edge to PbG edge
      PSOffset=0                    ! PreShower z offset from box edge to PbG edge
      SmdOff=0.0                    ! SMD z offset from box edge
EndFill
Fill FBXD                           ! FPD Box Geometry
      Type=3                        ! Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6, 4=17*34+14*28)
      Height=82                     ! Box height
      Depth=82                      ! Box Depth
      NX=6                          ! Number of pbg in x
      NY=6                          ! number of pbg in y
      NXL=14                        ! Number of large pbg in x
      NYL=14                        ! number of large pbg in y
      XOffset=26.0                  ! tower x offset from box edge to PbG edge
      ZOffset=1                     ! tower z offset from box edge to PbG edge
      PSOffset=0                    ! PreShower z offset from box edge to PbG edge
      SmdOff=0.0                    ! SMD z offset from box edge
EndFill
Fill FBXD                           ! FPD Box Geometry
      Type=4                        ! Type (1=7*7+SMD+PreShower, 2=5*5, 3=14*14+6*6, 4=17*34+14*28)
      Height=200                    ! Box height
      Depth=82                      ! Box Depth
      NX=12                         ! Number of pbg in x
      NY=24                         ! number of pbg in y
      NXL=17                        ! Number of large pbg in x
      NYL=34                        ! number of large pbg in y
      XOffset=-26.44                ! tower x offset from box edge to PbG edge
      ZOffset=0.0                   ! tower z offset from box edge to PbG edge
      PSOffset=0                    ! PreShower z offset from box edge to PbG edge
      SmdOff=0.0                    ! SMD z offset from box edge
EndFill


*----------------------------------------------------------------------------
Fill FLGG                           ! PbG detector geometry
      Type     = 1                  ! Type (1=Protovino Cell, 2=FLab Cell)
      Width    = 3.81		    ! PbG width	
      Depth    = 45.0		    ! PbG depth
      DGap     = 0.01		    ! Gap between PbG
      AlThick  = 0.001		    ! almunim wrap thickness (real)
      PhCathDz = 2.0 		    ! Photo Cathode thickness
      PhCathR  = 1.8 		    ! Photo Cathode radius  (real)
      MuMetDz  = 11.0 		    ! Mu Metal Length
      MuMetR   = 1.9 		    ! Mu metal outer Radius  (real)
Fill FLGG                           ! PbG detector geometry
      Type     = 2                  ! Type (1=Protovino Cell, 2=FLab Cell)
      Width    = 5.8		    ! PbG width	
      Depth    = 60.2		    ! PbG depth
      DGap     = 0.01		    ! Gap between PbG
      AlThick  = 0.001		    ! almunim wrap thickness (real)
      PhCathDz = 2.0 		    ! Photo Cathode thickness
      PhCathR  = 1.8 		    ! Photo Cathode radius  (real)
      MuMetDz  = 11.0 		    ! Mu Metal Length
      MuMetR   = 1.9 		    ! Mu metal outer Radius  (real)
EndFill
*----------------------------------------------------------------------------
Fill FLGM			    ! PbG detector materials
      Type     = 1                  ! Type (1=Protovino Cell, 2=FLab Cell)
      Density  = 3.86		    ! gdensity [/cm^3]
      RadLen   = 2.5		    ! radiation length [cm]
      PbCont   = 65.4		    ! PbO content [%]
      CritEne  = 0.0158   	    ! critical energy [GeV]
      MoliereR = 3.32		    ! Moliere radius [cm]
EndFill
Fill FLGM			    ! PbG detector materials
      Type     = 2                  ! Type (1=Protovino Cell, 2=FLab Cell)
      Density  = 3.61		    ! gdensity [/cm^3]
      RadLen   = 2.5		    ! radiation length [cm]
      PbCont   = 65.4		    ! PbO content [%]
      CritEne  = 0.0158   	    ! critical energy [GeV]
      MoliereR = 3.32		    ! Moliere radius [cm]
EndFill
*-----------------------------------------------------------------------------
Fill PBPD                           ! Pb Plate dimensions
      Width=33.02                   ! Width
      Height=33.02                  ! Height
      Thick=1.27                    ! Thickness
EndFill
*----------------------------------------------------------------------------
Fill FMXG                           ! SMD geometry
      Version=2                     ! Geometry version
      Sapex=0.7                     ! Scintillator strip apex
      Sbase=1.0                     ! Scintillator strip base
      Sgap=0.0064                   ! Gap between strips
      NStrip=50                     ! # of strips
      G10Width=27.0                 ! G10 plate width
      G10hgt=27.0                   ! G10 plate height
      G10Thick=0.15                 ! G10 plate thickness
EndFill
*----------------------------------------------------------------------------

      USE FMCG

      prin1 fmcg_version; ('****************** FPDMGEO version ', F4.2)

      do m=1,6  ! modules

        USE FPOS iMod=m
        USE FBXD Type=FPOS_iType
*        print *,FPOS_iType

        if(FBXD_Type.lt.3) then
           USE FLGG Type=1
           wid  =  FLGG_Width + FLGG_DGap + FLGG_AlThick*2
           ztot = (FLGG_Depth + FLGG_AlThick + FLGG_MuMetDz)/2.0
           rtot = FBXD_NX*wid/2.0        
           bwid = rtot+FBXD_XOffset
        else
           USE FLGG Type=2
           wid  =  FLGG_Width + FLGG_DGap + FLGG_AlThick*2
           ztot = (FLGG_Depth + FLGG_AlThick + FLGG_MuMetDz)/2.0
           rtot = FBXD_NXL*wid/2.0        
           bwid = rtot
        endif

        if(FPOS_X.gt.0.0) then
           xx=FPOS_X+bwid
        else if(FPOS_X.eq.0.0) then
           xx=0.0
        else 
           xx=FPOS_X-bwid
        endif

        if(FPOS_Y.gt.0.0) then
           yy=FPOS_Y+FBXD_Height/2.0
        else if(FPOS_Y.eq.0.0) then
           yy=0.0
        else 
           yy=FPOS_Y-FBXD_Height/2.0
        endif

        if(FPOS_Z.gt.0.0) then
           zz=FPOS_Z+FBXD_Depth/2.0
        else
           zz=FPOS_Z-FBXD_Depth/2.0
        endif

        serN=m
c        if(m.eq.6)  then
c          serN=1
c        endif
        Create and Position FBOX in CAVE x=xx y=yy z=zz AlphaY=FPOS_AY   
*        print *,m,xx,yy,zz,FPOS_AY

      enddo

* ----------------------------------------------------------------------------------
Block FBOX is one Pb-Glass fpd detector
      Material  Air
      Medium    standard
      Attribute FBOX seen=1 colo=2
* serial=serN
      shape box dx=bwid dy=FBXD_Height/2 dz=FBXD_Depth/2
      
*Towers
      USE FLGG Type=1
      USE FLGM Type=1
      wid  =  FLGG_Width + FLGG_DGap + FLGG_AlThick*2
      ztot = (FLGG_Depth + FLGG_AlThick + FLGG_MuMetDz)/2.0
      rtot = FBXD_NX*wid/2.0
      bwid = rtot+FBXD_XOffset

      x0 =  rtot - FBXD_XOffset - wid/2
      wid2 = -wid
      if(FBXD_Type.eq.4 .and. FPOS_iMod.eq.6) then
        x0 = - rtot + FBXD_XOffset + wid/2        
        wid2=wid
      endif
      if(FBXD_Type.eq.4) then
        y1 =  FBXD_NY*wid/2.0 - wid/2 + 0.68    
      else
        y1 =  FBXD_NY*wid/2.0 - wid/2
      endif 
      z1 = -FBXD_Depth/2 + FBXD_ZOffset + ztot
      do i=1, FBXD_NY
        x1=x0
        do j=1, FBXD_NX
           if(FBXD_Type.eq.4 .and. j.lt.6 .and.
     +        i.gt.7 .and. i.lt.18) then
             x1=x1+wid2
           else
             Create and Position FLGT x=x1 y=y1 z=z1        
c          write(*,'(2I3,3F12.6)') i,j,x1,y1,z1
               x1=x1+wid2
           endif
   	    enddo
        if(FBXD_Type.eq.4) then
          y1 =  y1-wid-0.0591304
        else
          y1 =  y1-wid
        endif                  
      enddo
      
      if(FBXD_Type.eq.1) then
*PreShowers
         x1=x0
         y1= -rtot + ztot
         z1=-FBXD_Depth/2  + FBXD_PSOffset + wid/2
         do j=1,FBXD_NX
           Create and Position FLGT x=x1 y=y1 z=z1 alphaX=90
           x1=x1-wid
         enddo
*Pb Plate
         if(fmcg_PbPlate==1) then
           Create and Position PBPT x=0 y=0 z=PBPD_Thick/2.0-FBXD_Depth/2
         endif
*SMD
         ztotsmd=FMXG_G10Thick+FMXG_Sapex
         Create and Position FSHM x=0 y=0 z=FBXD_SmdOff+ztotsmd-FBXD_Depth/2
         
      endif     

*Large Cell
      if(FBXD_Type.ge.3) then
         USE FLGG Type=2
         USE FLGM Type=2
         wid  = FLGG_Width + FLGG_DGap 
         ztot = FLGG_Depth/2.0
         rtot = FBXD_NXL*wid/2.0
         bwid = rtot

         x0 =  bwid - wid/2
         wid2=-wid
         if(FBXD_Type.eq.4 .and. FPOS_X.gt.0.0) then           
           x0 =  -bwid + wid/2
           wid2 = wid
         endif
         y1 =  FBXD_NYL*wid/2.0 - wid/2
         z1 = -FBXD_Depth/2 + FBXD_ZOffset + ztot
         do i=1,FBXD_NYL
            x1=x0            
            do j=1,FBXD_NXL
               if(FBXD_Type.eq.3 .and. i.gt.5 .and. i.lt.10 .and.
     +            j.gt.5 .and. j.lt.10) then
                  x1=x1+wid2
               elseif(FBXD_Type.eq.4 .and. j.lt.9 .and.
     +            i.gt.9 .and. i.lt.26) then
                  x1=x1+wid2 
               elseif((FBXD_Type.eq.4 .and. (i+j).ge.45) .or.
     +            (FBXD_Type.eq.4 .and. (j-i).ge.10)) then
                  x1=x1+wid2                  
               else              
                  Create and Position FLXF x=x1 y=y1 z=z1             
                  write(*,'(2I3,3F12.6)') i,j,x1,y1,z1
                  x1=x1+wid2
               endif
            enddo
            y1=y1-wid           
         enddo
      endif

EndBlock
* ----------------------------------------------------------------------------
Block FLGT is one PbG Tower
      material Air
      Attribute FLGT seen=1 colo=2
      Shape box	dx=0 dy=0 dz=0
*       Shape box	dx=wid/2 dy=wid/2 dz=ztot

      Create and Position FWAL z=-ztot+(FLGG_AlThick+FLGG_depth)/2.0
      Create and Position FUMT z=-ztot+FLGG_AlThick+FLGG_depth+FLGG_MuMetDz/2.0
      Create and Position FPCT z=-ztot+FLGG_AlThick+FLGG_depth+FLGG_PhCathDz/2.0
Endblock
* ----------------------------------------------------------------------------
Block FWAL is almunum wrapper
      material Aluminium
      Attribute FWAL seen=1 colo=3
      Shape box	dz=FLGG_Depth/2.0+FLGG_AlThick/2.0,
		dx=FLGG_Width/2.0+FLGG_AlThick,
		dy=FLGG_Width/2.0+FLGG_AlThick 
      if(fmcg_ChkvSim==1) CALL GSCKOV(%Imed,N,E,ABSCO_Alm,EFFIC_all,RINDEX_Alm)
      Create and Position FLGR z=+FLGG_AlThick/2.0
Endblock
* ----------------------------------------------------------------------------
Block FLGR is Lead Glass detector
*     PbG is about 65% Pb 
      Component Pb    A=207.19 Z=82   W=.60712
      Component K     A=39.102 Z=19   W=.02324
      Component Si    A=28.088 Z=14   W=.14771
      Component O     A=15.999 Z=8    W=.22041
      Component As    A=74.922 Z=33   W=.00152
      Mixture   PbG   Dens=FLGM_Density Radl=FLGM_RadLen
      Medium leadglass ISVOL=1
      Attribute FLGR  seen=1    colo=4    ! red
      Shape box dz=FLGG_depth/2 dx=FLGG_Width/2 dy=FLGG_Width/2

*      Call GSTPAR (ag_imed,'CUTELE', flgm_CritEne)
      HITS FLGR ELOS:0:(0,50)
      if(FMCG_ChkvSim==1) then
         CALL GSCKOV(%Imed,N,E,ABSCO_PbG,EFFIC_All,RINDEX_PbG)
      endif
Endblock
* ----------------------------------------------------------------------------
Block FLXF is Lead Glass detector
*     PbG is about 65% Pb 
      Component Pb    A=207.19 Z=82   W=.60712
      Component K     A=39.102 Z=19   W=.02324
      Component Si    A=28.088 Z=14   W=.14771
      Component O     A=15.999 Z=8    W=.22041
      Component As    A=74.922 Z=33   W=.00152
      Mixture   PbG   Dens=FLGM_Density Radl=FLGM_RadLen
      Medium leadglass ISVOL=1
      Attribute FLXF  seen=1    colo=4    ! red
      Shape     box dz=FLGG_depth/2 dx=FLGG_Width/2 dy=FLGG_Width/2
      Call GSTPAR (ag_imed,'CUTELE', flgm_CritEne)
      HITS FLXF ELOS:0:(0,50)
Endblock
* ----------------------------------------------------------------------------
Block FPCT is Photo Cathode
      material Air
      Medium PhotCath ISVOL=1
      Attribute LPCT seen=1 colo=6
      Shape tube dz=FLGG_PhCathDz/2.0 rmin=0 rmax=FLGG_PhCathR
      if(FMCG_ChkvSim==1) then
	CALL GSCKOV(%Imed,N,E,ABSCO_PhCath,EFFIC_PhCath,RINDEX_PhCath)
	HITS FPCT USER:0:(0,100000)
      endif
Endblock
* ----------------------------------------------------------------------------
Block FUMT is mu metal
      material iron
      Attribute LUMT  seen=1    colo=5
      Shape tube dz=FLGG_MuMetDz/2.0 rmin=FLGG_PhCathR rmax=FLGG_MuMetR
      if(fmcg_ChkvSim==1) CALL GSCKOV(%Imed,N,E,ABSCO_MuMet,EFFIC_All,RINDEX_MuMet)
Endblock
* ----------------------------------------------------------------------------
Block PBPT is pb plate
      material Lead
      Attribute PBPT seen=1 colo=7
      Shape box dx=PBPD_Width/2.0 dy=PBPD_Height/2.0 dz=PBPD_Thick/2.0
Endblock
* ----------------------------------------------------------------------------
Block FSHM  is the SHower Max  section
      Material  Air 
      Attribute FSHM  seen=1   colo=4                 
      Shape     box dx=FMXG_G10Width/2 dy=FMXG_G10hgt/2 dz=ztotsmd
      
      wsmd=FMXG_Sbase/2+FMXG_Sgap
      wtotsmd=(FMXG_Nstrip+1)*wsmd
*G10 
      zzz=-ztotsmd+FMXG_G10Thick/2
      Create and Position FXGT x=0 y=0 z=zzz

*SMD V-Plane
      xxx=-wtotsmd/2-FMXG_Sgap/2+wsmd
      yyy=0.0
      zzz=zzz+FMXG_G10Thick/2+FMXG_Sapex/2
      do i=1,FMXG_Nstrip
         if(mod(i,2)!= 0) then 
           Create and Position FHMS x=xxx y=yyy z=zzz
         else
           Create and Position FHMS x=xxx y=yyy z=zzz AlphaX=180
         endif
         xxx=xxx+wsmd
      enddo

*G10 
      zzz=zzz+FMXG_G10Thick/2++FMXG_Sapex/2
      Create and Position FXGT x=0 y=0 z=zzz
      
*SMD H-Plane
      xxx=0.0
      yyy=-wtotsmd/2-FMXG_Sgap/2+wsmd
      zzz=zzz+FMXG_G10Thick/2+FMXG_Sapex/2
      do i=1,FMXG_Nstrip
         if(mod(i,2)!= 0) then
           Create and Position FHMS x=xxx y=yyy z=zzz AlphaZ=90
         else
           Create and Position FHMS x=xxx y=yyy z=zzz AlphaZ=90 AlphaX=180
         endif
         yyy=yyy+wsmd
      enddo

EndBlock
* ----------------------------------------------------------------------------
Block FXGT  is the G10 layer in the SMax
*     G10 is about 60% SiO2 and 40% epoxy
      Component Si    A=28.08  Z=14   W=0.6*1*28./60.
      Component O     A=16     Z=8    W=0.6*2*16./60.
      Component C     A=12     Z=6    W=0.4*8*12./174.
      Component H     A=1      Z=1    W=0.4*14*1./174.
      Component O     A=16     Z=8    W=0.4*4*16./174.
      Mixture   g10   Dens=1.7
      Attribute FXGT   seen=1   colo=7
      Shape     BOX dx=FMXG_G10Width/2 dy=FMXG_G10hgt/2 dz=FMXG_G10Thick/2
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
EndBlock
* ----------------------------------------------------------------------------                    
Block FHMS is sHower Max Strip
      Material  POLYSTYREN
      Material  Cpolystyren Isvol=1
      Attribute FHMS seen=1    colo=2     ! red
      Shape     TRD1 dx1=0 dx2=fmxg_Sbase/2 dy=FMXG_G10hgt/2,
                dz=fmxg_Sapex/2
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE', 0.0001)
      Call GSTPAR (ag_imed,'BIRK1',1.)
      Call GSTPAR (ag_imed,'BIRK2',0.0130)
      Call GSTPAR (ag_imed,'BIRK3',9.6E-3)
      HITS FHMS    Birk:0:(0,10)
Endblock
* ----------------------------------------------------------------------------

end
