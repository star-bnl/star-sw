Module FHCMGEO is the Forward Hadron Detector Modules GEOmetry
Created  17 Apr 2009 
Author   QINGHUA XU...  
+CDE,AGECOM,GCUNIT

      Content    FHCM,FHCC,FHCS

      Structure  FHCG {Version,ZOffset,XOffset,YOffset,NXcell,NYCell,NZsub,XWID,YWID,ZWID}

      Integer    i,j,k,sn
      Real       xx,yy,zz

Fill  FHCG                          ! Forward Hadronic Calorimeter basic data 
      Version  = 1.0                ! Geometry version 
      ZOffset  = 900                ! offsets in z from IP to center of FHC
      XOffset  = 100                ! offsets in x from beamline to center of FHC
      YOffset  = 0                  ! offsets in y from beamline to center of FHC
      NXCell   = 9                  ! Number of cells in X
      NYCell   = 12                 ! Number of cells in Y
      NZsub    = 5                  ! Number of sub-cells in Z
      XWID     = 10                 ! cell x width
      YWID     = 10                 ! cell y width
      ZWID     = 117                ! cell z length
endfill

      prin1 FHCG_Version; (' FHCMGEO Version ', F4.2)

* FHC Module
      Create FHCM
      sn=1
      Position FHCM in CAVE z=FHCG_ZOffset x=FHCG_XOffset  y=FHCG_YOffset Konly='MANY'
      sn=2
      Position FHCM in CAVE z=FHCG_ZOffset x=-FHCG_XOffset y=FHCG_YOffset Konly='MANY'
      prin1; ('FHCMGEO done')

* ----------------------------------------------------------------------------
Block FHCM is one FHC module
      Material  Air
      Medium    standard
      Attribute FHCM   seen=1 colo=2 
      shape     BOX    dx=FHCG_XWID*FHCG_NXCell/2.0 dy=FHCG_YWID*FHCG_NYCell/2.0 dz=FHCG_ZWID/2.0 
      Create FHCC
      do i=1,FHCG_NXCell
        do j=1,FHCG_NYCell  
           xx= (i-0.5)*FHCG_XWID - FHCG_NXCell*FHCG_XWID/2.0
	   if (sn.eq.2) xx = -xx
           yy= -(j-0.5)*FHCG_YWID + FHCG_NYCell*FHCG_YWID/2.0
           write(*,*) i,j,xx,yy
           Position FHCC x=xx y=yy
        enddo
      enddo
EndBlock

Block FHCC is one FHC cell
      Material  Air
      Medium    standard
      Attribute FHCC   seen=1 colo=3
      shape     BOX    dx=FHCG_XWID/2.0 dy=FHCG_YWID/2.0 dz=FHCG_ZWID/2.0
      Create FHCS
      do k=1,FHCG_NZsub
        zz= (k - 0.5)*FHCG_ZWID/FHCG_NZsub - FHCG_ZWID/2.0
*        zz= (k - FHCG_NZsub/2.0-0.5)*FHCG_ZWID/FHCG_NZsub
        write(*,*) zz
        Position FHCS z=zz
      enddo
EndBlock


Block FHCS is one longitudinal sub-dividsion of FHC cell
      Material  Lead
      Medium    standard
      Attribute FHCS   seen=1 colo=4
      shape     BOX    dx=FHCG_XWID/2.0 dy=FHCG_YWID/2.0 dz=FHCG_ZWID/FHCG_NZsub/2.0

      HITS FHCS ELoss:0:(0,250)
EndBlock

end
      
 
