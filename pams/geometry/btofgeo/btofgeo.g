*******************************************************************************
Module  BTOFGEO is the Geometry of Barrel Trigger / Time Of Flight system 
  Author     Harlan Howe, Pablo Yepes
  Created    23 March 1996
* Original   Version of Harlan Howe, Pablo Yepes modified by Pavel Nevski
* modified   19 Sept 1996, Geary Eppley
*******************************************************************************
+CDE,AGECOM,GCUNIT,GCONST.
*
*   List of GEANT volumes produce here:
      Content   BTOF,BTOH,BSEC,BSET,BTRA,BUND,BTPF,BBTF,BASE,BARM,BANG,BWAT,
                BCOV,BMTR,BMTC,BMTM,BMTD,BCSA,BCSB,BCCV,BCPM,BCSK,BSEL,BCEL
*
*   Data Base interface staff:
      Structure BTOG { Version, Rmin, Rmax, dz , ctbtrue}
*
      Structure TRAY { Height, Width, Length, WallThk, SupFullH, SupFullW,
                       SupBaseT, SupBaseW, SupArmT, CoolOutR, CoolInnR }
*
      Structure CTBB { Slab1Len, Slab2Len, Slab1x,  Slab2x, SlabThck, SlabWid,  
                       ConvLen,  ConvWidM, ConvThck, 
                       PmtLen,   PmtMaxR,  PmtMinR, 
                       BaseLen,  BaseMaxR, BaseMinR, 
                       ElecThck, ElecWid,  ElecLen, 
                       ElecX,    ElecZ   }
*
      Structure TOFF { Slab1Len, Slab2Len, Slab3Len, Slab4Len, Slab5Len,
                       Slab6Len, Slab7Len, Slab8Len, Slab9Len, Slab0Len,
                       Slab1x,   Slab2x, 
                       Slab1z,   Slab2z,   Slab3z,   Slab4z,   Slab5z,
                       Slab6z,   Slab7z,   Slab8z,   Slab9z,   Slab10z,
                       SlabThck, SlabWid,  
                       ConvLen,  ConvWidM, ConvThck, 
                       PmtLen,   PmtMaxR,  PmtMinR, 
                       BaseLen,  BaseMaxR, BaseMinR,
                       ElecThck, ElecWid,  ElecLen, 
                       ElecX,    ElecZ   }

*
*   Normal Fortran variables to be used in this module:
      real      support_arm_width,  support_arm_Xpos,  support_arm_Ypos,
                support_aile_width, support_aile_Ypos, 
                xpos, ypos, zpos
*
* -------------------------------------------------------------------------
* 
      Fill BTOG ! Barrel Trigger, CTB/TOF Basic dimensions 
         version   = 1          ! geometry version
         Rmin      = 207.75     ! minimum CTB/TOF system radius
         Rmax      = 219.5      ! maximum CTB/TOF system radius
         dz        = 246.0      ! CTB/TOF tube half length
         ctbtrue   = 0          ! set to "0" to generate TOF geometry, "1" for CTB 


      Fill TRAY ! general tray stats        
         Height    =  9.40      ! tray height
         Width     = 21.59      ! full tray width
         Length    = 241.62     ! full tray lehgth
         WallThk   =  0.13      ! tray wall thickness
         SupFullH  =  2.08      ! support height (radial)
         SupFullW  =  15.24     ! support full width with arms
         SupBaseW  =  9.22      ! support base width
         SupBaseT  =  0.37      ! support base thickness, 0.32+0.05## 
         SupArmT   =  0.64      ! support arm  thickness
         CoolOutR  =  0.80      ! Cooling channel outer radius
         CoolInnR  =  0.48      ! Cooling channel inner radius
*
*  ## +0.05 to compensate for the fact that the TPC radius is 207.80 as built.
*
      Fill CTBB ! barrel trigger stats
         Slab1Len  = 112.5      ! first slab (B) length
         Slab2Len  = 130.0      ! second slab (A)length 
         Slab1x    = 1.38       ! first slab (B) x position
         Slab2x    =-1.53       ! second slab (A) x position
         SlabThck  = 1.0        ! scintillator slab thicknesses
         SlabWid   = 21.0       ! scintillator slab width
         ConvLen   = 8.5        ! optical converter length
         ConvWidM  = 4.0        ! optical convertor min width
         ConvThck  = 0.92       ! optical convertor thickness
         PmtLen    = 5.0        ! PMT length
         PmtMaxR   = 2.0        ! PMT max radius
         PmtMinR   = 1.84       ! PMT min radius
         BaseLen   = 13.0       ! Base length
         BaseMaxR  = 2.4        ! Base max radius
         baseMinR  = 2.27       ! Base min radius
         ElecThck  = 0.25       ! readout electronics thickness
         ElecWid   = 15.0       ! readout electronics width
         ElecLen   = 16.0       ! readout electronics length
         ElecX     = 4.0        ! readout electronics x position
         ElecZ     = -110.0     ! readout electronics z position
*
      Fill TOFF ! time of flight stats
         Slab1Len  = 21.55      ! first slab length
         Slab2Len  = 21.49      ! second slab length
         Slab3Len  = 22.23      ! nth slab length
         Slab4Len  = 22.47      ! nth slab length
         Slab5Len  = 23.67      ! nth slab length
         Slab6Len  = 24.36      ! nth slab length
         Slab7Len  = 26.07      ! nth slab length
         Slab8Len  = 27.48      ! nth slab length
         Slab9Len  = 29.51      ! nth slab length
         Slab0Len  = 22.33      ! 10th slab length
         Slab1x    = 2.13       ! first slab x position
         Slab2x    =-1.28       ! second slab x position
         Slab1z    = 109.905    ! first slab z position
         Slab2z    =  88.675    ! second slab z position
         Slab3z    =  66.025    ! nth slab z position
         Slab4z    =  44.555    ! nth slab z position
         Slab5z    =  19.875    ! nth slab z position
         Slab6z    =  -2.62     ! nth slab z position
         Slab7z    = -30.325    ! nth slab z position
         Slab8z    = -54.89     ! nth slab z position
         Slab9z    = -86.585    ! nth slab z position
         Slab10z   =-109.515    ! nth slab z position
         SlabThck  = 1.5        ! scintillator slab thicknesses
         SlabWid   = 4.15       ! scintillator slab width
         ConvLen   = 0.5        ! optical converter length
         ConvWidM  = 3.15       ! optical convertor min width
         ConvThck  = 1.5        ! optical convertor thickness
         PmtLen    = 4.0        ! PMT length
         PmtMaxR   = 1.59       ! PMT max radius
         PmtMinR   = 1.43       ! PMT min radius
         BaseLen   = 16.5       ! Base length
         BaseMaxR  = 2.1        ! Base max radius
         baseMinR  = 1.97       ! Base min radius
         ElecThck  = 0.25       ! readout electronics thickness
         ElecWid   = 15.0       ! readout electronics width
         ElecLen   = 16.0       ! readout electronics length
         ElecX     = 4.0        ! readout electronics x position
         ElecZ     = -110.0     ! readout electronics z position
*
      EndFill
*
      USE   BTOG   Version=1
      USE   TRAY
      USE   CTBB
      USE   TOFF
*
      Create and Position BTOF in Cave
*
*******************************************************************************
*
Block BTOF is the whole CTB / TOF system envelope 
      Attribute BTOF      seen=0  colo=1
      Material  Air
      Medium    Standard
      Shape     Tube      rmin=btog_Rmin  Rmax=btog_Rmax  dz=btog_dz
*
      Create and Position BTOH  z=+btog_dz/2    alphay=180
*
*     Rotate the +z half of the tube so that the phi divisions will run clockwise
*     for each half of the detector when viewed from outside. The direction of 
*     increasing z is then toward eta=0 in each half.
*
                 Position BTOH  z=-btog_dz/2
EndBlock
*------------------------------------------------------------------------------
Block BTOH is a half of trigger system (west-east)
      Attribute BTOH      seen=0  colo=1
      Shape     Tube      dz=btog_dz/2
      if  (btog_ctbtrue.eq.1) then
        Create   BSEC   " dont need to positition it, because this is division "
      else 
        Create   BSET   " dont need to positition it, because this is division "
      endif
EndBlock
*
*  BSEC and BSET will both be needed to model a system that contains TOF and CTB. In 
* this event the two copies of BTOH will require individual names.
*
*------------------------------------------------------------------------------
Block BSEC is a sector of CTB Trigger Barrel Scintillators
      Attribute BSEC      seen=0   colo=3
      Shape     division  Iaxis=2  Ndiv=60  C0=105
      Create and Position BTRA     X=btog_Rmin+tray_SupFullH/2+tray_height/2
EndBlock
*------------------------------------------------------------------------------
Block BSET is a sector of TOF Scintillators
      Attribute BSET      seen=0   colo=3
      Shape     division  Iaxis=2  Ndiv=60  C0=105
      Create and Position BTRA     X=btog_Rmin+tray_SupFullH/2+tray_height/2
EndBlock
*------------------------------------------------------------------------------
*
Block BTRA is one full tray plus supporting structure
      Attribute BTRA      seen=0   colo=4
      Shape     BOX       dx=tray_SupFullH/2+tray_height/2,
                          dy=tray_Width/2
      Create and Position BMTR     X=tray_SupFullH/2,
                                   z=(btog_dz-tray_length)/2
*                                  ! trays abut at z=0
      Create and Position BUND     X=-tray_height/2
EndBlock
*
********************************************************************************
*
Block BMTR  is a Main TRay covering box
      Attribute BMTR      seen=1   colo=4
      Material   Aluminium
      Shape      BOX      DX=tray_height/2,
                          dz=tray_length/2  
      create and Position BMTC
EndBlock
*
Block BMTC  is  the Main TRay Cavity filled with MANY details
*
      Material   Air
      Shape      BOX      dx=tray_height/2-tray_WallThk,  
                          dy=tray_Width/2-tray_WallThk,
                          dz=tray_Length/2-tray_WallThk
*
      if  (btog_ctbtrue.eq.1) then
*
*                           inner slab + readout
*
      zpos  =  (tray_length-ctbb_Slab1Len)/2-tray_WallThk
*
      Create and Position BCSA                 dx=ctbb_SlabThck/2,
                                               dy=ctbb_SlabWid/2,
                                               dz=ctbb_Slab1Len/2,
                                               X=ctbb_Slab1x,  
                                               Z=zpos
      zpos = zpos - (ctbb_Slab1Len + ctbb_ConvLen)/2
      Create and Position BCCV  X=ctbb_Slab1x  Z=zpos,
                     Dx1=ctbb_SlabThck/2  Dx2=ctbb_SlabThck/2,
                     Dy1=ctbb_ConvWidM/2  Dy2=ctbb_SlabWid/2  dz=ctbb_ConvLen/2
      zpos = zpos - (ctbb_ConvLen + ctbb_PmtLen)/2
      Create and Position BCPM  X=ctbb_Slab1x  Z=zpos,
                     Rmin=ctbb_PmtMinR  Rmax=ctbb_PmtMaxR  Dz=ctbb_PmtLen/2
      zpos = zpos - (ctbb_PmtLen + ctbb_BaseLen)/2
      Create and Position BCSK  X=ctbb_Slab1x  Z=zpos,
                     Rmin=ctbb_BaseMinR  Rmax=ctbb_BaseMaxR   Dz=ctbb_BaseLen/2
      Create and Position BSEL X=ctbb_Slab1x  Z=zpos dx=ctbb_ElecThck/2, 
                               dy=ctbb_BaseMinR-0.1  dz=ctbb_BaseLen/2
*
*                           Outer slab + readout
*
      zpos  =  (tray_length-ctbb_Slab2Len)/2-tray_WallThk
*
      Create and Position BCSA                 dx=ctbb_SlabThck/2,
                                               dy=ctbb_SlabWid/2,
                                               dz=ctbb_Slab2Len/2,
                                               X=ctbb_Slab2x,  
                                               Z=-zpos
      zpos = zpos - (ctbb_Slab2len + ctbb_ConvLen)/2
                 Position BCCV  X=ctbb_Slab2x  Z=-zpos  alphax=180,
                     Dx1=ctbb_SlabThck/2  Dx2=ctbb_SlabThck/2,
                     Dy1=ctbb_ConvWidM/2  Dy2=ctbb_SlabWid/2  dz=ctbb_ConvLen/2
      zpos = zpos - (ctbb_ConvLen + ctbb_PmtLen)/2
                 Position BCPM  X=ctbb_Slab2x  Z=-zpos,
                     Rmin=ctbb_PmtMinR  Rmax=ctbb_PmtMaxR  Dz=ctbb_PmtLen/2
      zpos = zpos - (ctbb_PmtLen + ctbb_BaseLen)/2
                 Position BCSK  X=ctbb_Slab2x  Z=-zpos,
                     Rmin=ctbb_BaseMinR  Rmax=ctbb_BaseMaxR   Dz=ctbb_BaseLen/2
                 Position BSEL  X=ctbb_Slab2x  Z=-zpos dx=ctbb_ElecThck/2,  
                                dy=ctbb_BaseMinR-0.1  dz=ctbb_BaseLen/2
*
      Create and Position BCEL  X=ctbb_ElecX, Z=ctbb_ElecZ, 
                     dx=ctbb_ElecThck/2  dy=ctbb_ElecWid/2  dz=ctbb_ElecLen/2
*
      else 
*
*
*       There isn't any room for BCEL in BMTC currently. The TOF electronics is not 
*       designed yet. Maybe the electronics can be included in each pmt base.
*
*        Create and Position BCEL  X=toff_ElecX,     Z=toff_ElecZ,
*                     dx=toff_ElecThck/2  dy=toff_ElecWid/2  dz=toff_ElecLen/2
*                 Position BCEL  X=toff_ElecX-0.5, Z=toff_ElecZ,
*                     dx=toff_ElecThck/2  dy=toff_ElecWid/2  dz=toff_ElecLen/2
*                 Position BCEL  X=toff_ElecX-1.0, Z=toff_ElecZ, 
*                     dx=toff_ElecThck/2  dy=toff_ElecWid/2  dz=toff_ElecLen/2
*                 Position BCEL  X=toff_ElecX-1.5, Z=toff_ElecZ, 
*                     dx=toff_ElecThck/2  dy=toff_ElecWid/2  dz=toff_ElecLen/2
*                 Position BCEL  X=toff_ElecX-2.0, Z=toff_ElecZ, 
*                     dx=toff_ElecThck/2  dy=toff_ElecWid/2  dz=toff_ElecLen/2
        Create and Position  BMTM   
      endif
EndBlock
*
*------------------------------------------------------------------------------
Block BMTM  is  the Main Tray cavity divisions Mother volume
*           BMTM has the identical size as BMTC and exists to allow the division 
*           of BMTC into 5 strips in phi while BMTC is also containing other stuff. 
* 
      Attribute BMTM      seen=0   colo=5
      Material   Air
      Shape      BOX      dx=tray_height/2-tray_WallThk,  
                          dy=tray_Width/2-tray_WallThk,
                          dz=tray_Length/2-tray_WallThk
        Create   BMTD  " dont need to positition it, because this is division "
EndBlock
*
*------------------------------------------------------------------------------

Block BMTD is a phi column of TOF Scintillators
      Attribute BMTD      seen=0   colo=5
      Shape     division  Iaxis=2  Ndiv=5  
*
*                           slab 1 + readout
*
      zpos  =  toff_Slab1z
*
      Create and Position BCSB                 dx=toff_SlabThck/2,
                                               dy=toff_SlabWid/2,
                                               dz=toff_Slab1Len/2,
                                               X=toff_Slab1x,  
                                               Z=zpos
      zpos = zpos - (toff_Slab1Len + toff_ConvLen)/2
      Create and Position BCCV  X=toff_Slab1x  Z=zpos,
                     Dx1=toff_SlabThck/2  Dx2=toff_SlabThck/2,
                     Dy1=toff_ConvWidM/2  Dy2=toff_SlabWid/2  dz=toff_ConvLen/2
      zpos = zpos - (toff_ConvLen + toff_PmtLen)/2
      Create and Position BCPM  X=toff_Slab1x  Z=zpos,
                     Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR  Dz=toff_PmtLen/2
      zpos = zpos - (toff_PmtLen + toff_BaseLen)/2
      Create and Position BCSK  X=toff_Slab1x  Z=zpos,
                     Rmin=toff_BaseMinR  Rmax=toff_BaseMaxR   Dz=toff_BaseLen/2
      Create and Position BSEL  X=toff_Slab1x  Z=zpos dx=toff_ElecThck/2,
                                dy=toff_BaseMinR-0.1  dz=toff_BaseLen/2
*
*                           slab 2 + readout
*
      zpos  =  toff_Slab2z
*
      Create and Position BCSB                 dx=toff_SlabThck/2,
                                               dy=toff_SlabWid/2,
                                               dz=toff_Slab2Len/2,
                                               X=toff_Slab2x,  
                                               Z=zpos
      zpos = zpos + (toff_Slab2len + toff_ConvLen)/2
                 Position BCCV  X=toff_Slab2x  Z=zpos  alphax=180,
                     Dx1=toff_SlabThck/2  Dx2=toff_SlabThck/2,
                     Dy1=toff_ConvWidM/2  Dy2=toff_SlabWid/2  dz=toff_ConvLen/2
      zpos = zpos + (toff_ConvLen + toff_PmtLen)/2
                 Position BCPM  X=toff_Slab2x  Z=zpos,
                     Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR  Dz=toff_PmtLen/2
      zpos = zpos + (toff_PmtLen + toff_BaseLen)/2
                 Position BCSK  X=toff_Slab2x  Z=zpos,
                     Rmin=toff_BaseMinR  Rmax=toff_BaseMaxR   Dz=toff_BaseLen/2
                 Position BSEL  X=toff_Slab2x  Z=zpos dx=toff_ElecThck/2,
                                dy=toff_BaseMinR-0.1  dz=toff_BaseLen/2
*
*                           slab 3 + readout
*
      zpos  =  toff_Slab3z
*
      Create and Position BCSB                 dx=toff_SlabThck/2,
                                               dy=toff_SlabWid/2,
                                               dz=toff_Slab3Len/2,
                                               X=toff_Slab1x,  
                                               Z=zpos
      zpos = zpos - (toff_Slab3Len + toff_ConvLen)/2
                 Position BCCV  X=toff_Slab1x  Z=zpos,
                     Dx1=toff_SlabThck/2  Dx2=toff_SlabThck/2,
                     Dy1=toff_ConvWidM/2  Dy2=toff_SlabWid/2  dz=toff_ConvLen/2
      zpos = zpos - (toff_ConvLen + toff_PmtLen)/2
                 Position BCPM  X=toff_Slab1x  Z=zpos,
                     Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR  Dz=toff_PmtLen/2
      zpos = zpos - (toff_PmtLen + toff_BaseLen)/2
                 Position BCSK  X=toff_Slab1x  Z=zpos,
                     Rmin=toff_BaseMinR  Rmax=toff_BaseMaxR   Dz=toff_BaseLen/2
                 Position BSEL  X=toff_Slab1x  Z=zpos dx=toff_ElecThck/2,
                                dy=toff_BaseMinR-0.1  dz=toff_BaseLen/2
*
*                           slab 4 + readout
*
      zpos  =  toff_Slab4z
*
      Create and Position BCSB                 dx=toff_SlabThck/2,
                                               dy=toff_SlabWid/2,
                                               dz=toff_Slab4Len/2,
                                               X=toff_Slab2x,  
                                               Z=zpos
      zpos = zpos + (toff_Slab4len + toff_ConvLen)/2
                 Position BCCV  X=toff_Slab2x  Z=zpos  alphax=180,
                     Dx1=toff_SlabThck/2  Dx2=toff_SlabThck/2,
                     Dy1=toff_ConvWidM/2  Dy2=toff_SlabWid/2  dz=toff_ConvLen/2
      zpos = zpos + (toff_ConvLen + toff_PmtLen)/2
                 Position BCPM  X=toff_Slab2x  Z=zpos,
                     Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR  Dz=toff_PmtLen/2
      zpos = zpos + (toff_PmtLen + toff_BaseLen)/2
                 Position BCSK  X=toff_Slab2x  Z=zpos,
                     Rmin=toff_BaseMinR  Rmax=toff_BaseMaxR   Dz=toff_BaseLen/2
                 Position BSEL  X=toff_Slab2x  Z=zpos dx=toff_ElecThck/2,  
                                dy=toff_BaseMinR-0.1  dz=toff_BaseLen/2
*
*                           slab 5 + readout
*
      zpos  =  toff_Slab5z
*
      Create and Position BCSB                 dx=toff_SlabThck/2,
                                               dy=toff_SlabWid/2,
                                               dz=toff_Slab5Len/2,
                                               X=toff_Slab1x,  
                                               Z=zpos
      zpos = zpos - (toff_Slab5Len + toff_ConvLen)/2
                 Position BCCV  X=toff_Slab1x  Z=zpos,
                     Dx1=toff_SlabThck/2  Dx2=toff_SlabThck/2,
                     Dy1=toff_ConvWidM/2  Dy2=toff_SlabWid/2  dz=toff_ConvLen/2
      zpos = zpos - (toff_ConvLen + toff_PmtLen)/2
                 Position BCPM  X=toff_Slab1x  Z=zpos,
                     Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR  Dz=toff_PmtLen/2
      zpos = zpos - (toff_PmtLen + toff_BaseLen)/2
                 Position BCSK  X=toff_Slab1x  Z=zpos,
                     Rmin=toff_BaseMinR  Rmax=toff_BaseMaxR   Dz=toff_BaseLen/2
                 Position BSEL  X=toff_Slab1x  Z=zpos dx=toff_ElecThck/2,  
                                dy=toff_BaseMinR-0.1  dz=toff_BaseLen/2
*
*                           slab 6 + readout
*
      zpos  =  toff_Slab6z
*
      Create and Position BCSB                 dx=toff_SlabThck/2,
                                               dy=toff_SlabWid/2,
                                               dz=toff_Slab6Len/2,
                                               X=toff_Slab2x,  
                                               Z=zpos
      zpos = zpos + (toff_Slab6len + toff_ConvLen)/2
                 Position BCCV  X=toff_Slab2x  Z=zpos  alphax=180,
                     Dx1=toff_SlabThck/2  Dx2=toff_SlabThck/2,
                     Dy1=toff_ConvWidM/2  Dy2=toff_SlabWid/2  dz=toff_ConvLen/2
      zpos = zpos + (toff_ConvLen + toff_PmtLen)/2
                 Position BCPM  X=toff_Slab2x  Z=zpos,
                     Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR  Dz=toff_PmtLen/2
      zpos = zpos + (toff_PmtLen + toff_BaseLen)/2
                 Position BCSK  X=toff_Slab2x  Z=zpos,
                     Rmin=toff_BaseMinR  Rmax=toff_BaseMaxR   Dz=toff_BaseLen/2
                 Position BSEL  X=toff_Slab2x  Z=zpos dx=toff_ElecThck/2,  
                                dy=toff_BaseMinR-0.1  dz=toff_BaseLen/2
*
*                           slab 7 + readout
*
      zpos  =  toff_Slab7z
*
      Create and Position BCSB                 dx=toff_SlabThck/2,
                                               dy=toff_SlabWid/2,
                                               dz=toff_Slab7Len/2,
                                               X=toff_Slab1x,  
                                               Z=zpos
      zpos = zpos - (toff_Slab7Len + toff_ConvLen)/2
                 Position BCCV  X=toff_Slab1x  Z=zpos,
                     Dx1=toff_SlabThck/2  Dx2=toff_SlabThck/2,
                     Dy1=toff_ConvWidM/2  Dy2=toff_SlabWid/2  dz=toff_ConvLen/2
      zpos = zpos - (toff_ConvLen + toff_PmtLen)/2
                 Position BCPM  X=toff_Slab1x  Z=zpos,
                     Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR  Dz=toff_PmtLen/2
      zpos = zpos - (toff_PmtLen + toff_BaseLen)/2
                 Position BCSK  X=toff_Slab1x  Z=zpos,
                     Rmin=toff_BaseMinR  Rmax=toff_BaseMaxR   Dz=toff_BaseLen/2
                 Position BSEL  X=toff_Slab1x  Z=zpos dx=toff_ElecThck/2,  
                                dy=toff_BaseMinR-0.1  dz=toff_BaseLen/2
*
*                           slab 8 + readout
*
      zpos  =  toff_Slab8z
*
      Create and Position BCSB                 dx=toff_SlabThck/2,
                                               dy=toff_SlabWid/2,
                                               dz=toff_Slab8Len/2,
                                               X=toff_Slab2x,  
                                               Z=zpos
      zpos = zpos + (toff_Slab8len + toff_ConvLen)/2
                 Position BCCV  X=toff_Slab2x  Z=zpos  alphax=180,
                     Dx1=toff_SlabThck/2  Dx2=toff_SlabThck/2,
                     Dy1=toff_ConvWidM/2  Dy2=toff_SlabWid/2  dz=toff_ConvLen/2
      zpos = zpos + (toff_ConvLen + toff_PmtLen)/2
                 Position BCPM  X=toff_Slab2x  Z=zpos,
                     Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR  Dz=toff_PmtLen/2
      zpos = zpos + (toff_PmtLen + toff_BaseLen)/2
                 Position BCSK  X=toff_Slab2x  Z=zpos,
                     Rmin=toff_BaseMinR  Rmax=toff_BaseMaxR   Dz=toff_BaseLen/2
                 Position BSEL  X=toff_Slab2x  Z=zpos dx=toff_ElecThck/2,  
                                dy=toff_BaseMinR-0.1  dz=toff_BaseLen/2
*
*                           slab 9 + readout
*
      zpos  =  toff_Slab9z
*
      Create and Position BCSB                 dx=toff_SlabThck/2,
                                               dy=toff_SlabWid/2,
                                               dz=toff_Slab9Len/2,
                                               X=toff_Slab1x,  
                                               Z=zpos
      zpos = zpos - (toff_Slab9Len + toff_ConvLen)/2
                 Position BCCV  X=toff_Slab1x  Z=zpos,
                     Dx1=toff_SlabThck/2  Dx2=toff_SlabThck/2,
                     Dy1=toff_ConvWidM/2  Dy2=toff_SlabWid/2  dz=toff_ConvLen/2
      zpos = zpos - (toff_ConvLen + toff_PmtLen)/2
                 Position BCPM  X=toff_Slab1x  Z=zpos,
                     Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR  Dz=toff_PmtLen/2
      zpos = zpos - (toff_PmtLen + toff_BaseLen)/2
                 Position BCSK  X=toff_Slab1x  Z=zpos,
                     Rmin=toff_BaseMinR  Rmax=toff_BaseMaxR   Dz=toff_BaseLen/2
                 Position BSEL  X=toff_Slab1x  Z=zpos dx=toff_ElecThck/2,  
                                dy=toff_BaseMinR-0.1  dz=toff_BaseLen/2
*
*                           slab 10 + readout
*
      zpos  =  toff_Slab10z
*
      Create and Position BCSB                 dx=toff_SlabThck/2,
                                               dy=toff_SlabWid/2,
                                               dz=toff_Slab0Len/2,
                                               X=toff_Slab2x,  
                                               Z=zpos
      zpos = zpos + (toff_Slab0len + toff_ConvLen)/2
                 Position BCCV  X=toff_Slab2x  Z=zpos  alphax=180,
                     Dx1=toff_SlabThck/2  Dx2=toff_SlabThck/2,
                     Dy1=toff_ConvWidM/2  Dy2=toff_SlabWid/2  dz=toff_ConvLen/2
      zpos = zpos + (toff_ConvLen + toff_PmtLen)/2
                 Position BCPM  X=toff_Slab2x  Z=zpos,
                     Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR  Dz=toff_PmtLen/2
      zpos = zpos + (toff_PmtLen + toff_BaseLen)/2
                 Position BCSK  X=toff_Slab2x  Z=zpos,
                     Rmin=toff_BaseMinR  Rmax=toff_BaseMaxR   Dz=toff_BaseLen/2
                 Position BSEL  X=toff_Slab2x  Z=zpos dx=toff_ElecThck/2,  
                                dy=toff_BaseMinR-0.1  dz=toff_BaseLen/2
*
*
EndBlock
*
*------------------------------------------------------------------------------
Block BCSA  is  the active trigger scintillator SLAB for ctb 
      Attribute BCSA      seen=1   colo=3
      Material polystyren
      Medium   sensitive    IsVol=1
      Shape   BOX     dx=0 dy=0 dz=0

*   hit options: H - put in GEANT hit field (instead of PseudoVolumes)
*                S - Single step
*
      HITS    BCSA   XX:16:HS(-250,250)   YY:16:(-250,250)    ZZ:16:(-250,250),
                     px:16:(-100,100)     py:16:(-100,100)    pz:16:(-100,100),
                     Sleng:16:(0,1.e4)    ToF:16:(0,1.e-6)    Step:16:(0,100),
                     ShtN:16:             Eloss:32:(0,1) 
EndBlock
*------------------------------------------------------------------------------
Block BCSB  is  the active trigger scintillator SLAB for tof
      Attribute BCSA      seen=1   colo=3
      Material polystyren
      Medium   sensitive    IsVol=1
      Shape   BOX     dx=0 dy=0 dz=0

*   hit options: H - put in GEANT hit field (instead of PseudoVolumes)
*                S - Single step
*
      HITS    BCSB   XX:16:HS(-250,250)   YY:16:(-250,250)    ZZ:16:(-250,250),
                     px:16:(-100,100)     py:16:(-100,100)    pz:16:(-100,100),
                     Sleng:16:(0,1.e4)    ToF:16:(0,1.e-6)    Step:16:(0,100),
                     ShtN:16:             Eloss:32:(0,1) 
EndBlock
*------------------------------------------------------------------------------

Block BCCV  is  a  Ctb optical ConVerter
      Attribute BCCV      seen=1   colo=6
      Material polystyren 
      Shape   TRD2   Dx1=0   Dx2=0   Dy1=0   Dy2=0  dz=0
EndBlock
* 
Block BCPM  is  a  Ctb PhotoMultiplier
      Attribute BCPM      seen=1   colo=6
      Material polystyren 
      Shape   TUBE   Rmin=0  Rmax=0  Dz=0
EndBlock
*
Block BCSK  is  a  PhotoMultiplier socket
      Attribute BCSK      seen=1   colo=4
      Material Aluminium 
      Shape   TUBE   Rmin=0  Rmax=0   Dz=0
EndBlock

Block BSEL  is  a  Ctb PM electronics
      Attribute BSEL      seen=1   colo=1
      Material silicon
      Shape   BOX    dx=0  dy=0  dz=0
EndBlock

Block BCEL  is  a  Ctb PM electronics
      Attribute BCEL      seen=1   colo=1
      Material silicon
      Shape   BOX    dx=0  dy=0  dz=0
EndBlock
*
*******************************************************************************
*
Block BUND   is  Undercarriage support tray 
*
      Attribute BUND      seen=0   colo=4
      SHAPE  BOX   dx=tray_SupFullH/2  dy=tray_Width/2  

*   normally material should be declared INSIDE each of the following volumes
*   but here we apparently have a rather crazy case of extreme details 
*
      Material   Aluminium
                                  xpos = (tray_SupFullH - 0.7)/2
                                  ypos = (tray_SupFullW + 2.54)/2 + 0.06
                                  zpos = (btog_dz-tray_length)/2
      Create and Position  BTPF   X = xpos  Y = -ypos Z = zpos
                 Position  BTPF   X = xpos  Y = +ypos Z = zpos
                                  xpos = (tray_SupFullH - 0.9)/2 - 0.7
                                  ypos = ypos - (3.18 - 2.54)/2
      Create and Position  BBTF   X = xpos  Y = -ypos Z = zpos
                 Position  BBTF   X = xpos  Y = +ypos Z = zpos
*
      support_aile_width = ( tray_SupFullH-tray_SupArmT )/tan(60*DegRad) 
      support_arm_width  = ( tray_SupFullW-tray_SupBaseW)/2-support_aile_width
      support_aile_Ypos  = ( tray_SupBaseW+Support_Aile_width)/2
      support_arm_Xpos   = ( tray_SupFullH-tray_SupArmT )/2
      support_arm_Ypos   = ( tray_SupFullW-Support_Arm_width)/2

      Material   Aluminium
      Create and Position  BASE   X=(-tray_SupFullH+tray_SupBaseT)/2
      Create and Position  BARM   X=support_arm_Xpos     Y=-support_arm_Ypos
                 Position  BARM   X=support_arm_Xpos     Y=+support_arm_Ypos
      Create and Position  BANG   Y=-support_aile_Ypos
                 Position  BANG   Y=+support_aile_Ypos   thetaX=270
      Create and Position  BCOV   X=-tray_SupFullH/2+tray_CoolOutR+0.05
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block BTPF  is  a ToP of Foot structure    ( Material  Aluminium )
      Attribute BTPF      seen=1   colo=4
      Shape     BOX    dx=0.35  dy=1.27  dz = tray_length/2 
EndBlock

Block BBTF  is  a Bottom of foot structure ( Material  Aluminium )
      Attribute BBTF      seen=1   colo=4
      Shape     BOX    Dx=0.45  Dy=1.59  dz = tray_length/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block BARM  is  a TPC cooling structure arm             ( Material  Aluminium )
      Attribute BARM      seen=1   colo=4
      Shape     BOX    Dx=tray_SupArmT/2   DY=support_arm_width/2
EndBlock

Block BANG  is  an angled part of TPC cooling structure ( Aile )
      Attribute BANG      seen=1   colo=4
      Shape     PARA   dx=tray_SupArmT/2   Dy=support_aile_width/2,
                       Alph=-60   thet=0   phi=0
EndBlock

Block BASE  is  a bottom of TPC coolant structure       
      Attribute BASE      seen=1   colo=4
      Shape     BOX    Dx=tray_SupBaseT/2  Dy=tray_SupBaseW/2
EndBlock

Block BCOV  is  a whole TPC cooling channel             ( Material  Aluminium )
      Attribute BCOV      seen=1   colo=4
      Shape     TUBE   Rmin=0   Rmax=tray_CoolOutR
      Create and Position BWAT 
EndBlock

Block BWAT  is  TPC cooling water
      Attribute BWAT      seen=1   colo=4
      Component H2     A=1   Z=1   W=2
      Component O      A=16  Z=8   W=1
      Mixture   Water  Dens=1.0
      Shape     TUBE   Rmax=tray_CoolInnR
EndBlock
* ----------------------------------------------------------------------------
   end

