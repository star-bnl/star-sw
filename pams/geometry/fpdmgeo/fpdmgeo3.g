******************************************************************************
* $Id: fpdmgeo3.g,v 1.9 2011/02/28 16:20:02 jwebb Exp $
* $Name:  $
* $Log: fpdmgeo3.g,v $
* Revision 1.9  2011/02/28 16:20:02  jwebb
* Switched from fortran-style to mortran-style continuation lines.
*
* Revision 1.8  2009/08/14 22:40:27  perev
* Fix BIRK3 constant
*
* Revision 1.7  2008/11/19 04:08:23  perev
*  updates to the corrected(vp) starsim
*
* Revision 1.6  2007/09/21 23:00:08  perev
* corrects E.Braidot@phys.uu.nl
*
* Revision 1.4  2007/02/07 23:38:37  potekhin
* Further evolution of the code, with many improvements
* by Ermes Braidot
*
* Revision 1.1  2006/11/28 00:02:42  potekhin
* First version of the FMS (FPD) that still needs work.
******************************************************************************
Module FPDMGEO3 is the Forward Pion Detector Modules GEOmetry
     Created   27 Nov 2006
     Author    Akio Ogawa
+CDE,AGECOM,GCONST,GCUNIT.
*
      Content    FBOX,FTOW,FLXF,FWAL,FLGR,FPRB,FPCT,FUMT,PBPT,FSHM,FHMS,FXGT,
                 FALU,FBAS,FENC,FEAC,FEBC,FECC,FEDC,FETC,FERC,FESC,FEEC
*     
      Structure FMCG {Version,ChkvSim,PbPlate}
      Structure FPOS {iMod,iType,X,Y,Z,AY,AZ}
      Structure FBXD {Type,Height,Depth,Width,NX,NY,NXL,NYL,XOffset,ZOffset,PSOffset,SmdOff}
      Structure FLGG {Type,Width,Depth,DGap,AlThick,PhCathDz,PhCathR,MuMetDz,MuMetR}
      Structure FLGM {Type,Density,RadLen,PbCont,CritEne,MoliereR}
      Structure PBPD {Z,Width,Height,Thick}
      Structure FMXG {Version,Sapex,Sbase,Sgap,NStrip,G10Width,G10hgt,G10Thick}
      Structure INSE {Width,Depth,Height,SheetDpt,HoleGap,HoleDepth,HoleHeight,GapDepth,
                      GapHeight,GateDepth,Ra,Rb,Diam,RMax,GateGap}

      integer    ChkvSim,iMod,iType,Type,PbPlate
      Integer    i,j,m,serN

      Real       xx,yy,zz,x1,y1,z1,ztot,rtot,wid,widx,widy,bwid,x0,widL
      Real       ztotsmd,wtotsmd,zsmd,zsmd2,wsmd
      Real       xsmdh,ysmdh,zsmdh,xsmdv,ysmdv,zsmdv
      Real       xlcOffSet, BZOffSet
      Real       basewidth,distancer,xoffFECC,xoffFEDC,xshift
      Real       xoffFENC,yoffFENC,zoffFENC,zoffFECC

      REAL       tmp(7)  ! temporary test variable

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
      iMod=1                        ! Module# (EN=1, ES=2, WN=3, WS=4, ...)
      iType=1                       ! Type (1=7*7+SMD+PreShower, 2=17*34+14*28)
      X=-48.19                      ! X distance from beam to edge of detector
      Y=0.0                         ! Y distance from beam to edge of detector
      Z=-779.0                      ! Z distance from IP to surface of detector
      AY=180                        ! Angle aroound Y (0 for west, 180 for east)
      AZ=0                          ! Angle around Z
     
EndFill

Fill  FPOS                          ! Fpd ES positioning
      iMod=2                        ! Module# (EN=1, ES=2, WS=3, WS=4, ...)
      iType=1                       ! Type (1=7*7+SMD+PreShower, 2=17*34+14*28)
      X=48.19                       ! X distance from beam to edge of detector
      Y=0.0                         ! Y distance from beam to edge of detector
      Z=-779.0                      ! Z distance from IP to surface of detector
      AY=180                        ! Angle aroound Y (0 for west, 180 for east)
      AZ=0                          ! Angle around Z
EndFill

Fill  FPOS                          ! FMS WN positioning
      iMod=3                        ! Module# (EN=1, ES=2, WN=3, WS=4, ...)
      iType=2                       ! Type (1=7*7+SMD+PreShower, 2=17*34+14*28)
      X=-0.3                        ! X distance from beam to edge of detector
      Y=0.0                         ! Y distance from beam to edge of detector
      Z=706.3                       ! Z distance from IP to surface of detector
      AY=0                          ! Angle aroound Y (0 for west, 180 for east)
EndFill

Fill  FPOS                          ! FMS WS positioning
      iMod=4                        ! Module# (EN=1, ES=2, WN=3, WS=4, ...)
      iType=2                       ! Type (1=7*7+SMD+PreShower, 2=17*34+14*28)
      X=0.3                         ! X distance from beam to edge of detector
      Y=0.0                         ! Y distance from beam to edge of detector
      Z=706.3                       ! Z distance from IP to surface of detector
      AY=0                          ! Angle aroound Y (0 for west, 180 for east)
      AZ=0                          ! Angle around Z
EndFill
* ----------------------------------------------------------------------------
Fill FBXD                           ! FPD Box Geometry
      Type=1                        ! Type (1=7*7+SMD+PreShower, 2=17*34+14*28)
      Height=100                    ! Box height
      Depth=96                      ! Box Depth
      Width=0.0                     ! Box Width (only for FMS)
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
      Type=2                        ! Type (1=7*7+SMD+PreShower, 2=17*34+14*28)
      Height=210                    ! Box height
      Depth=98.425                  ! Box Depth
      Width=127.0                   ! Box Width (only for FMS)
      NX=12                         ! Number of pbg in x
      NY=24                         ! number of pbg in y
      NXL=17                        ! Number of large pbg in x
      NYL=34                        ! number of large pbg in y
      XOffset=(6*3.822+0.5*5.812)+(127.0-17*5.812)/2.0     ! tower x offset from box edge to PbG edge
      ZOffset=10.4                  ! tower z offset from box edge to PbG edge
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
      RadLen   = 3.21		    ! radiation length [cm]
      PbCont   = 45.0		    ! PbO content [%]
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
Fill INSE                           ! INSERT geometry
      Width=19.30908                ! Width of the insert (x)
      Depth=98.425                  ! Depth of the insert (z)
      Height=38.608                 ! Height of the insert (y)  
      SheetDpt=1.27                 ! Depth of the steel parts (x,y)
      HoleGap=5.08                  ! Distance between edge of INSERT and square hole
      HoleDepth=25.4                ! Depth of the square hole (z)
      HoleHeight=30.48              ! Height of the square hole (y)
      GapDepth=19.685               ! Depth of the iron distancer (z)
      GapHeight=7.874               ! Height of the iron distancer (y)
      GateDepth=1.905               ! Depth of one of the three Gates (z)
      GateGap=12.7                  ! Distance between the back edge of the box and last gate
      Ra=13.97                      ! Radius of the inner circle of tubes
      Rb=20.6375                    ! Radius of the outer circle of tubes 
      Diam=6.0325                   ! Diameter of the tubes
      RMax=10.16                    ! Radius of the inner tube for beampipe
EndFill
*----------------------------------------------------------------------------

      USE FMCG

      prin1 fmcg_version; ('****************** FPDMGEO version ', F4.2)

      do m=1,4  ! box modules for FMS

        USE FPOS iMod=m
        USE FBXD Type=FPOS_iType
*        print *,FPOS_iType

        if(FBXD_Type.eq.1) then
           USE FLGG Type=1
           wid  =  FLGG_Width + FLGG_DGap + FLGG_AlThick*2.0
           ztot = (FLGG_Depth + FLGG_AlThick + FLGG_MuMetDz)/2.0
           rtot = FBXD_NX*wid/2.0        
           bwid = rtot+FBXD_XOffset
        else
           USE FLGG Type=2
           wid  =  FLGG_Width + FLGG_DGap + FLGG_AlThick*2.0
           ztot = (FLGG_Depth + FLGG_AlThick + FLGG_MuMetDz)/2.0
           rtot = FBXD_NXL*wid/2.0        
           bwid = rtot
        endif

        if(m.ge.3) then
           bwid=FBXD_Width/2.0 
           if(FPOS_X.gt.0.0) then
             xx=FPOS_X+bwid 
            elseif(FPOS_X.eq.0.0) then
             xx=0.0
            else 
             xx=FPOS_X-bwid
           endif 
        else 
           if(FPOS_X.gt.0.0) then
             xx=FPOS_X+bwid
           elseif(FPOS_X.eq.0.0) then
             xx=0.0
           else 
             xx=FPOS_X-bwid
           endif
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

        serN=0

        if(m.eq.4)  then
          serN=1
        endif
        if(m.ne.7) then    ! just a check for the box number
        Create and Position FBOX in CAVE x=xx y=yy z=zz AlphaY=FPOS_AY  kOnly='MANY'
*        print *,m,xx,yy,zz,FPOS_AY
        endif
      enddo

* ----------------------------------------------------------------------------------
Block FBOX is one Pb-Glass fpd detector
      Material  Air
      Medium    standard
      Attribute FBOX seen=1 colo=2 serial=serN
      if(FBXD_Type.eq.2) then
       shape box dx=FBXD_Width/2.0 dy=FBXD_Height/2.0 dz=FBXD_Depth/2.0
      else
       shape box dx=bwid dy=FBXD_Height/2.0 dz=FBXD_Depth/2.0
      endif

*Towers
      USE FLGG Type=2
      widL = FLGG_Width + FLGG_DGap + FLGG_AlThick*2.0              !large cell width
      USE FLGG Type=1
      USE FLGM Type=1
      wid  =  FLGG_Width + FLGG_DGap + FLGG_AlThick*2.0             !small cell width
      ztot = (FLGG_Depth + FLGG_AlThick + FLGG_MuMetDz)/2.0       
      rtot = FBXD_NX*wid/2.0
      bwid = rtot-FBXD_XOffset

      Create FTOW
      Create PBPT
      Create FSHM

      if(FBXD_Type.eq.2 .and. FPOS_iMod.eq.4) then                 
        x0 = - rtot - FBXD_XOffset + wid/2.0                        ! x0 start from north (near beam) for 
        widx = wid                                                ! WS (FMS-south) module
      else 
        x0 =  rtot + FBXD_XOffset - wid/2.0                         ! x0 start from south for north/top/bottom module
      widx = -wid                                               ! since fpd is symmetric, true for ES module too
      endif
      if(FBXD_Type.eq.2) then                                     !this is to set the vertical gap between small cells
        y1 =  FBXD_NY*wid/2.0 - wid/2.0 + (16*widL-FBXD_NY*wid)/2.0   !in order to correct the differences between  
        widy = wid+(16.0*widL-FBXD_NY*wid)/23.0                       !3 small cells and 2 large ones (see again in 17 lines)  
      else                                                        
        y1 =  FBXD_NY*wid/2.0 - wid/2.0                          
        widy = wid
      endif 
      z1 = -FBXD_Depth/2.0 + FBXD_ZOffset + ztot
      do i=1, FBXD_NY
        x1=x0
        do j=1, FBXD_NX
           if(FBXD_Type.eq.2 .and. j.lt.6 .and. _         
              i.gt.7 .and. i.lt.18) then                    !hole in the small cell block
             x1=x1+widx
           else
             Create and Position FTOW x=x1 y=y1 z=z1        !positioning the small cell block  
*   !!          write(*,'(2I3,3F12.6)') i,j,x1,y1,z1
               x1=x1+widx
           endif
   	    enddo
        y1 =  y1-widy
      enddo
      
      if(FBXD_Type.eq.1) then
*PreShowers
         x1=x0
         y1= -rtot + ztot
         z1=-FBXD_Depth/2.0  + FBXD_PSOffset + wid/2.0
         do j=1,FBXD_NX
           Create and Position FTOW x=x1 y=y1 z=z1 alphaX=90
           x1=x1-wid
         enddo
*Pb Plate
         if(fmcg_PbPlate==1) then
           Create and Position PBPT x=0 y=0 z=PBPD_Thick/2.0-FBXD_Depth/2.0
         endif
*SMD
         ztotsmd=FMXG_G10Thick+FMXG_Sapex
         Create and Position FSHM x=0 y=0 z=FBXD_SmdOff+ztotsmd-FBXD_Depth/2.0
         
      endif     

*Large Cell
      if(FBXD_Type.ge.2) then
         USE FLGG Type=2
         USE FLGM Type=2
         wid  = FLGG_Width + FLGG_DGap +  FLGG_AlThick*2.0
         ztot = FLGG_Depth/2.0
         rtot = FBXD_NXL*wid/2.0
         bwid = rtot
         xlcOffSet = (FBXD_Width-FBXD_NXL*wid)/2.0          ! Large Cells OffSet in X
 
         if(FPOS_iMod.eq.4) then                            ! defining x0 for North and South
           x0 =  -bwid + wid/2.0 - xlcOffSet
           widx = wid
         elseif(FPOS_iMod.eq.3) then           
           x0 =  +bwid - wid/2.0 + xlcOffSet 
            widx = -wid
         endif
         y1 =  FBXD_NYL*wid/2.0 - wid/2.0
         z1 = -FBXD_Depth/2.0 + FBXD_ZOffset + ztot
         do i=1,FBXD_NYL
            x1=x0            
            do j=1,FBXD_NXL
               if(j.lt.9 .and. _
                  i.gt.9 .and. i.lt.26) then                   !hole in the large cells
                  x1=x1+widx 
               elseif((i+j).ge.45) then                        !bottom corners fill with plain lead cell
                  Create and Position FALU x=x1 y=y1 z=z1
                  x1=x1+widx     
               elseif((j-i).ge.10) then                        !hole in the top corners
                  x1=x1+widx            
               else              
                  Create and Position FLXF x=x1 y=y1 z=z1      !positioning cells       
*c!!                  write(*,'(2I3,3F12.6)') i,j,x1,y1,z1
                  x1=x1+widx
               endif
            enddo
            y1=y1-wid           
         enddo
       
      endif

* Steel Base Plate Elevator
        if(FBXD_Type.eq.2) then
         USE FLGG Type=2
         USE FLGM Type=2
         BZOffSet=0.0
         wid  = FLGG_Width + FLGG_DGap 
         Create and Position FBAS x=FPOS_X  y=-(FBXD_NXL*wid+basewidth/2.0) z=-FBXD_Depth/2.0 +BZOffSet+INSE_Depth/2.0
        endif

* Steel Insert   
        Create FENC
        Create FEAC
        Create FECC
        Create FEDC
        Create FEEC
        distancer=INSE_GapHeight-(INSE_Height-2.0*INSE_SheetDpt)
        xoffFECC=(INSE_SheetDpt-FBXD_Width)/2.0
        zoffFECC=-FBXD_Depth/2.0+BZOffSet+INSE_Depth-INSE_GateGap
        xoffFEDC=INSE_Width - INSE_SheetDpt
        xoffFENC=(INSE_Width-FBXD_Width)/2.0
        yoffFENC=(INSE_Height-INSE_SheetDpt)/2.0
        zoffFENC=-FBXD_Depth/2.0 + BZOffSet + INSE_Depth/2.0
        xshift=8*5.812-12*3.822+5*3.822-INSE_Width    !this is to move insert to small cell edge
        if(FBXD_Type.eq.2) then
        if(FPOS_iMod.eq.4) then    

          Position FENC x=xoffFENC+xshift,
                        y=-yoffFENC,
                        z=zoffFENC
          Position FENC x=xoffFENC+xshift,
                        y=yoffFENC,
                        z=zoffFENC 
          Position FEAC x=INSE_Width+xshift-(FBXD_Width+INSE_SheetDpt)/2.0,
                        y=0,
                        z=-FBXD_Depth/2.0+BZOffSet+INSE_Depth/2.0
          Position FECC x=xoffFECC+xshift,
                        y=distancer/2.0, 
                        z=zoffFECC-INSE_GateDepth-INSE_GapDepth/2.0  
          Position FECC x=xoffFECC+xshift,
                        y=distancer/2.0,
                        z=zoffFECC-2.0*INSE_GateDepth-3.0*INSE_GapDepth/2.0  
          Position FECC x=xoffFECC+xshift,
                        y=-distancer/2.0, 
                        z=zoffFECC-INSE_GateDepth-INSE_GapDepth/2.0  
          Position FECC x=xoffFECC+xshift,
                        y=-distancer/2.0, 
                        z=zoffFECC-2.0*INSE_GateDepth-3.0*INSE_GapDepth/2.0  
          Position FEDC x=xshift+(xoffFEDC-FBXD_Width)/2.0,
                        y=0 z=zoffFECC-INSE_GateDepth/2.0 
          Position FEDC x=xshift+(xoffFEDC-FBXD_Width)/2.0,
                        y=0 z=zoffFECC-INSE_GateDepth-INSE_GapDepth-INSE_GateDepth/2.0  
          Position FEDC x=xshift+(xoffFEDC-FBXD_Width)/2.0,
                        y=0 z=zoffFECC-2.0*(INSE_GateDepth+INSE_GapDepth)-INSE_GateDepth/2.0  

      
         elseif(FPOS_iMod.eq.3) then           

          Position FENC x=-xoffFENC-xshift,   
                        y=-yoffFENC z=zoffFENC
          Position FENC x=-xoffFENC-xshift,      
                        y=yoffFENC z=zoffFENC     
          Position FEAC x=-xshift-INSE_Width+(FBXD_Width+INSE_SheetDpt)/2.0,   
                        y=0 z=-FBXD_Depth/2.0+BZOffSet+INSE_Depth/2.0  
          Position FECC x=-xoffFECC-xshift,     
                        y=distancer/2.0, 
                        z=zoffFECC-INSE_GateDepth-INSE_GapDepth/2.0  
          Position FECC x=-xoffFECC-xshift,     
                        y=distancer/2.0,
                        z=zoffFECC-2.0*INSE_GateDepth-3.0*INSE_GapDepth/2.0  
          Position FECC x=-xoffFECC-xshift,     
                        y=-distancer/2.0, 
                        z=zoffFECC-INSE_GateDepth-INSE_GapDepth/2.0  
          Position FECC x=-xoffFECC-xshift,    
                        y=-distancer/2.0, 
                        z=zoffFECC-2.0*INSE_GateDepth-3.0*INSE_GapDepth/2.0  
          Position FEEC x=-xshift-(xoffFEDC-FBXD_Width)/2.0, 
                        y=0 z=zoffFECC-INSE_GateDepth/2.0  
          Position FEEC x=-xshift-(xoffFEDC-FBXD_Width)/2.0, 
                        y=0 z=zoffFECC-INSE_GateDepth-INSE_GapDepth-INSE_GateDepth/2.0  
          Position FEEC x=-xshift-(xoffFEDC-FBXD_Width)/2.0, 
                        y=0 z=zoffFECC-2.0*(INSE_GateDepth+INSE_GapDepth)-INSE_GateDepth/2.0 
         
        endif
        endif
EndBlock
* ----------------------------------------------------------------------------
Block FTOW is one PbG Tower
      material Air
      Attribute FTOW seen=1 colo=2
      Shape box	dx=wid/2.0 dy=wid/2.0 dz=ztot
*      Shape box	dx=0 dy=0 dz=0

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
      Shape box dz=FLGG_depth/2.0 dx=FLGG_Width/2.0 dy=FLGG_Width/2.0

*      Call GSTPAR (ag_imed,'CUTELE', flgm_CritEne)
      HITS FLGR ELOS:0:(0,50)
      if(FMCG_ChkvSim==1) then
         CALL GSCKOV(%Imed,N,E,ABSCO_PbG,EFFIC_All,RINDEX_PbG)
      endif
Endblock
* ----------------------------------------------------------------------------
Block FLXF is Lead Glass detector
*     F2 is about 45% Pb 
      Component Pb    A=207.19 Z=82   W=.41774
      Component K     A=39.102 Z=19   W=.04151
      Component Si    A=28.088 Z=14   W=.21047
      Component O     A=15.999 Z=8    W=.29330
      Component Na    A=22.990 Z=11   W=.03710
      Mixture   F2   Dens=FLGM_Density Radl=FLGM_RadLen
      Medium leadglass ISVOL=1
      Attribute FLXF  seen=1    colo=4    ! red
      Shape     box dz=FLGG_depth/2.0 dx=FLGG_Width/2.0 dy=FLGG_Width/2.0
*      Call GSTPAR (ag_imed,'CUTELE', flgm_CritEne)
      HITS FLXF ELOS:0:(0,50)
Endblock
* ----------------------------------------------------------------------------
Block FALU is Aluminium Base Cell
      material Aluminium
      Attribute FALU  seen=1    colo=1    ! black
      Shape     box dz=FLGG_depth/2.0 dx=FLGG_Width/2.0 dy=FLGG_Width/2.0
    
Endblock
* ----------------------------------------------------------------------------
Block FBAS is Steel Base Plate
      material Iron 
      Attribute FBAS  seen=1    colo=1   
      Shape     box dz=INSE_Depth/2.0 dx=FBXD_Width/2.0 dy=basewidth/2.0
    
Endblock
* ----------------------------------------------------------------------------
Block FENC is Steel Enclosure 
      material Iron 
      Attribute FENC  seen=1    colo=5   
      Shape     box dz=INSE_Depth/2.0 dx=INSE_Width/2.0 dy=INSE_SheetDpt/2.0
Endblock

Block FEAC is Steel Enclosure 
      material Iron 
      Attribute FEAC  seen=1    colo=5    
      Shape     box dz=INSE_Depth/2.0 dx=INSE_SheetDpt/2.0 dy=(INSE_Height - 2.0*INSE_SheetDpt)/2.0
        Create and Position FEBC x=0.0 y=0 z=-INSE_Depth/2.0+INSE_HoleDepth/2.0 + INSE_HoleGap

Endblock

Block FEBC is Air square hole 
      material Air 
      Attribute FEBC  seen=1    colo=5     
      Shape     box dz=INSE_HoleDepth/2.0 dx=INSE_SheetDpt/2.0 dy=INSE_HoleHeight/2.0
Endblock

Block FECC is Steel distancer
      material Iron
      Attribute FECC  seen=1    colo=5     
      Shape     box dz=INSE_GapDepth/2.0 dx=INSE_SheetDpt/2.0 dy=INSE_GapHeight/2.0
Endblock

Block FEDC is Steel Enclosure part on south 
      material Iron
      Attribute FEDC  seen=1    colo=5             
      Shape     box dz=INSE_GateDepth/2.0 dx=(xoffFEDC)/2.0 dy=(INSE_Height-2.0*INSE_SheetDpt)/2.0
      Create and Position FERC x=-(xoffFEDC)/2.0  y=0 z=0.0
      Create FESC
      Position FESC x=-(xoffFEDC)/2.0+INSE_Ra*cos(PI*5.0/12.0), 
                    y=INSE_Ra*sin(PI*5.0/12.0)  z=0.0
      Position FESC x=-(xoffFEDC)/2.0+INSE_Ra*cos(PI/4.0), 
                    y=INSE_Ra*sin(PI/4.0)  z=0.0
      Position FESC x=-(xoffFEDC)/2.0+INSE_Ra*cos(PI/12.0), 
                    y=INSE_Ra*sin(PI/12.0)  z=0.0
      Position FESC x=-(xoffFEDC)/2.0+INSE_Ra*cos(PI/12.0) ,
                    y=-INSE_Ra*sin(PI/12.0)  z=0.0
      Position FESC x=-(xoffFEDC)/2.0+INSE_Ra*cos(PI/4.0) ,
                    y=-INSE_Ra*sin(PI/4.0)  z=0.0
      Position FESC x=-(xoffFEDC)/2.0+INSE_Ra*cos(PI*5.0/12.0),
                    y=-INSE_Ra*sin(PI*5.0/12.0)  z=0.0
      Position FESC x=-(xoffFEDC)/2.0+INSE_Rb*cos(PI/4.0),
                    y=INSE_Rb*sin(PI/4.0)  z=0.0
      Position FESC x=-(xoffFEDC)/2.0+INSE_Rb*cos(PI/4.0) ,
                    y=-INSE_Rb*sin(PI/4.0) z=0.0      
Endblock

Block FEEC is Steel Enclosure part on north 
      material Iron
      Attribute FEEC  seen=1    colo=5     
      Shape     box dz=INSE_GateDepth/2.0 dx=(xoffFEDC)/2.0 dy=(INSE_Height - 2.0*INSE_SheetDpt)/2.0
      Create and Position FETC x=(xoffFEDC)/2.0  y=0 z=0.0
      Create FESC        
      Position FESC x=(xoffFEDC)/2.0-INSE_Ra*cos(PI*5.0/12.0)  ,
                    y=INSE_Ra*sin(PI*5.0/12.0)  z=0.0
      Position FESC x=(xoffFEDC)/2.0-INSE_Ra*cos(PI/4.0) ,
                    y=INSE_Ra*sin(PI/4.0)  z=0.0
      Position FESC x=(xoffFEDC)/2.0-INSE_Ra*cos(PI/12.0) , 
                    y=INSE_Ra*sin(PI/12.0)  z=0.0
      Position FESC x=(xoffFEDC)/2.0-INSE_Ra*cos(PI/12.0) ,
                    y=-INSE_Ra*sin(PI/12.0)  z=0.0
      Position FESC x=(xoffFEDC)/2.0-INSE_Ra*cos(PI/4.0)  ,
                    y=-INSE_Ra*sin(PI/4.0)  z=0.0
      Position FESC x=(xoffFEDC)/2.0-INSE_Ra*cos(PI*5.0/12.0), 
                    y=-INSE_Ra*sin(PI*5.0/12.0)  z=0.0
      Position FESC x=(xoffFEDC)/2.0-INSE_Rb*cos(PI/4.0) ,
                    y=INSE_Rb*sin(PI/4.0) z=0.0
      Position FESC x=(xoffFEDC)/2.0-INSE_Rb*cos(PI/4.0),
                    y=-INSE_Rb*sin(PI/4.0) z=0.0
Endblock

Block FETC is Air Enclosure part 
      material Air
      Attribute FETC  seen=1    colo=6     
      Shape     tubs rmin=0.0 rmax=INSE_RMax dz=INSE_GateDepth/2.0 phi1=90 phi2=-90
Endblock
Block FERC is Air Enclosure part 
      material Air
      Attribute FERC  seen=1    colo=6     
      Shape     tubs rmin=0.0 rmax=INSE_RMax dz=INSE_GateDepth/2.0 phi1=-90 phi2=90
Endblock

Block FESC is Air Enclosure part 
      material Air
      Attribute FESC  seen=1    colo=6     
      Shape     tube rmin=0.0 rmax=INSE_Diam/2.0 dz=INSE_GateDepth/2.0
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
      Shape     box dx=FMXG_G10Width/2.0 dy=FMXG_G10hgt/2.0 dz=ztotsmd
      
      wsmd=FMXG_Sbase/2.0+FMXG_Sgap
      wtotsmd=(FMXG_Nstrip+1)*wsmd
*G10 
      zsmd=-ztotsmd+FMXG_G10Thick/2.0
      Create and Position FXGT x=0 y=0 z=zsmd

*SMD V-Plane
      xsmdv=-wtotsmd/2.0-FMXG_Sgap/2.0+wsmd
      ysmdv=0.0
      zsmdv=zsmd+FMXG_G10Thick/2.0+FMXG_Sapex/2.0
      do i=1,FMXG_Nstrip
         if(mod(i,2)!= 0) then 
           Create and Position FHMS x=xsmdv y=ysmdv z=zsmdv
         else
           Create and Position FHMS x=xsmdv y=ysmdv z=zsmdv AlphaX=180
         endif
         xsmdv=xsmdv+wsmd
      enddo

*G10 
      zsmd2=zsmdv+FMXG_G10Thick/2.0++FMXG_Sapex/2.0
      Create and Position FXGT x=0 y=0 z=zsmd2
      
*SMD H-Plane
      xsmdh=0.0
      ysmdh=-wtotsmd/2.0-FMXG_Sgap/2.0+wsmd
      zsmdh=zsmd2+FMXG_G10Thick/2.0+FMXG_Sapex/2.0
      Create FHMS
      do i=1,FMXG_Nstrip
         if(mod(i,2)!= 0) then
           Position FHMS x=xsmdh y=ysmdh z=zsmdh ORT=Y-XZ	   
         else
           Position FHMS x=xsmdh y=ysmdh z=zsmdh ORT=YX-Z
         endif
         ysmdh=ysmdh+wsmd
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
      Shape     TRD1 dx1=0 dx2=fmxg_Sbase/2.0 dy=FMXG_G10hgt/2.0,
                dz=fmxg_Sapex/2.0
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE', 0.0001)
      Call GSTPAR (ag_imed,'BIRK1',1.)
      Call GSTPAR (ag_imed,'BIRK2',0.0130)
      Call GSTPAR (ag_imed,'BIRK3',9.6E-6)
      HITS FHMS    Birk:0:(0,10)
Endblock
* ----------------------------------------------------------------------------

end
