*******************************************************************************
*
*******************************************************************************
Module  BTOFGEO is the Geometry of Barrel Trigger / Time Of Flight system 
  Author     Harlan Howe, Pablo Yepes
  Created    23 March 1996
* Original   Version of Harlan Howe, Pablo Yepes modified by Pavel Nevski
* modified   19 Sept 1996, Geary Eppley
* modified   17 Mar  1997, GE - new aluminum feet for the tray, new tray height
* modified   22 Oct  1997, GE - position scintillator as built
* modified   20 Apr  1998, GE - add btog.choice: 1 CTB, 2 TOF, 3 25% TOF
*******************************************************************************
+CDE,AGECOM,GCUNIT,GCONST.
*
*   List of GEANT volumes produce here:
      Content   BTOF,BTOH,BSEC,BSET,BTRA,BTRB,BUND,BTFT,BASE,BARM,BANG,
                BWAT,BCOV,BMTR,BTTR,BMTC,BTTC,BMTM,BMTD,BCSA,BCSB,BCCV,BCPM,
                BCSK,BSEL,BCEL
*
*   Data Base interface staff:
      Structure BTOG { Version, Rmin, Rmax, dz, choice }
*
      Structure TRAY { Height, Width, Length, WallThk, SupFullH, SupFullW,
                       SupLen,
                       SupBaseT, SupBaseW, SupArmT, CoolOutR, CoolInnR,
                       StripT  , FootInse, FootThk, Foot1Len, Foot2Thk,
                       Foot3Len}
*
      Structure CTBB { Slab1Len, Slab2Len, Slab1x,  Slab2x, SlabThck, SlabWid,  
                       ConvLen,  ConvWidM, ConvThck, 
                       PmtLen,   PmtMaxR,  PmtMinR, 
                       BaseLen,  BaseMaxR, BaseMinR, 
                       ElecThck, ElecWid,  ElecLen, 
                       ElecX,    ElecZ,    Wrap,    Shim   }
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
      integer   i, nrval
*
* -------------------------------------------------------------------------
* 
      Fill BTOG ! Barrel Trigger, CTB/TOF Basic dimensions 
         version   = 1         ! geometry version
         Rmin      = 207.80    ! minimum CTB/TOF system radius (as built)
         Rmax      = 219.5     ! maximum CTB/TOF system radius
         dz        = 246.0     ! CTB/TOF tube half length
         choice    = 1         ! 1=CTB, 2=TOF, 3=25% TOF


      Fill TRAY ! general tray stats        
         Height    =  8.70      ! tray height
         Width     = 21.59      ! full tray width
         Length    = 241.62     ! full tray length
         WallThk   =  0.13      ! tray wall thickness
         SupFullH  =  2.03      ! support height (radial)
         SupFullW  =  15.24     ! support full width with arms
         SupLen    = 215.9      ! support length
         SupBaseW  =  9.22      ! support base width
         SupBaseT  =  0.32      ! support base thickness  
         SupArmT   =  0.64      ! support arm  thickness
         CoolOutR  =  0.80      ! Cooling channel outer radius
         CoolInnR  =  0.48      ! Cooling channel inner radius
         StripT    =  0.08      ! Thickness of polyethylene strip on bottom
         FootInse  =  1.06      ! foot inset from tray edge
         FootThk   =  0.23      ! thickness of foot material
         Foot1len  =  1.68      ! length (in section) of first part of foot
         Foot2Thk  =  1.16      ! thickness of second foot section
         Foot3Len  =  2.16      ! length of third part of foot


      Fill CTBB ! barrel trigger stats
         Slab1Len  = 112.5      ! first slab (B) length
         Slab2Len  = 130.0      ! second slab (A)length 
         Slab1x    = 5.84       ! first slab (B) x position
         Slab2x    = 2.67       ! second slab (A) x position
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
         Wrap      = 0.13       ! thickness of Tyvek + black plastic
         Shim      = 0.26       ! thickness of shim to position slat 2
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
*     Rotate the +z half of the tube so that the phi divisions will run 
*     clockwise
*     for each half of the detector when viewed from outside. The direction of 
*     increasing z is then toward eta=0 in each half.
*
                 Position BTOH  z=-btog_dz/2
EndBlock
*------------------------------------------------------------------------------
Block BTOH is a half of trigger system (west-east)
      Attribute BTOH      seen=0  colo=1
      Shape     Tube      dz=btog_dz/2
      if (btog_choice.eq.3) then
        Create BSEC
        do  i=0,44
          Position BSEC  alphaz = real(138+6*i), 
                                  NCOPY=6+i   
        enddo
        Create BSET
        do  i=0,14
          nrval = 51+i
          if (nrval.gt.60) nrval = nrval - 60 
          Position BSET  alphaz = real(48+6*i), 
                                  NCOPY=nrval
        enddo
      else if (btog_choice.eq.2) then
        Create BSET
        do  i=0,59
          Position BSET  alphaz = real(108+6*i),
                                  NCOPY=1+i
        enddo
      else
        Create BSEC
        do  i=0,59
          Position BSEC  alphaz = real(108+6*i),
                                  NCOPY=1+i
        enddo
      endif
EndBlock
*
*  BSEC and BSET will both be needed to model a system that contains TOF and 
*  CTB. In the event that z symmetry is broken, the two copies of BTOH will 
*  require individual names.
*
*------------------------------------------------------------------------------
Block BSEC is a sector of CTB Trigger Barrel Scintillators
      Attribute BSEC      seen=0   colo=1
      Shape     Tubs  phi1 = -3.0 phi2 = 3.0
      Create and Position BTRA   X = _ 
                          btog_Rmin+(tray_SupFullH+tray_height+tray_StripT)/2
EndBlock
*------------------------------------------------------------------------------
Block BSET is a sector of TOF Scintillators
      Attribute BSET      seen=0   colo=1
      Shape     Tubs  phi1 = -3.0 phi2 = 3.0
      Create and Position BTRB   X = _
                          btog_Rmin+(tray_SupFullH+tray_height+tray_StripT)/2
EndBlock
*------------------------------------------------------------------------------
*
Block BTRA is one full tray plus supporting structure for CTB
      Attribute BTRA      seen=0   colo=1
      Shape     BOX       dx=(tray_SupFullH+tray_height+tray_StripT)/2,
                          dy=tray_Width/2
      Create and Position BMTR     X=(tray_SupFullH+tray_StripT)/2,
                                   z=(btog_dz-tray_length)/2
*                                  ! trays abut at z=0
      Create and Position BUND     X=-(tray_height+tray_StripT)/2,
                                   z=(btog_dz-tray_SupLen)/2
EndBlock
*
*------------------------------------------------------------------------------
*
Block BTRB is one full tray plus supporting structure for TOF
      Attribute BTRB      seen=0   colo=1
      Shape     BOX       dx=(tray_SupFullH+tray_height+tray_StripT)/2,
                          dy=tray_Width/2
      Create and Position BTTR     X=(tray_SupFullH+tray_StripT)/2,
                                   z=(btog_dz-tray_length)/2
*                                  ! trays abut at z=0
      Create and Position BUND     X=-(tray_height+tray_StripT)/2,
                                   z=(btog_dz-tray_SupLen)/2
EndBlock
*
*-------------------------------------------------------------------------------
*
Block BMTR  is a Main TRay covering box for CTB
      Attribute BMTR      seen=1   colo=2
      Material   Aluminium
      Shape      BOX      DX=tray_height/2,
                          dz=tray_length/2  
      create and Position BMTC
EndBlock
*
*-------------------------------------------------------------------------------
*
Block BTTR  is a Main TRay covering box for TOF
      Attribute BTTR      seen=1   colo=2
      Material   Aluminium
      Shape      BOX      DX=tray_height/2,
                          dz=tray_length/2  
      create and Position BTTC
EndBlock
*
*-------------------------------------------------------------------------------
*
Block BMTC  is  the Main Tray Cavity filled with MANY details for CTB
*
      Attribute  BMTC     seen=1   colo=2
      Material   Air
      Shape      BOX      dx=tray_height/2-tray_WallThk,  
                          dy=tray_Width/2-tray_WallThk,
                          dz=tray_Length/2-tray_WallThk
*
*                           inner slab + readout
*
      zpos  =  (tray_length-ctbb_Slab1Len)/2-tray_WallThk-ctbb_Wrap
      xpos  =  -tray_Height/2+ctbb_Slab1x
      Create and Position BCSA           dx=ctbb_SlabThck/2,
                                         dy=ctbb_SlabWid/2,
                                         dz=ctbb_Slab1Len/2,
                                         X=xpos,
                                         Z=zpos
      zpos = zpos - (ctbb_Slab1Len + ctbb_ConvLen)/2
      Create and Position BCCV  X=xpos  Z=zpos,
                     Dx1=ctbb_SlabThck/2  Dx2=ctbb_SlabThck/2,
                     Dy1=ctbb_ConvWidM/2  Dy2=ctbb_SlabWid/2  dz=ctbb_ConvLen/2
      zpos = zpos - (ctbb_ConvLen + ctbb_PmtLen)/2
      Create and Position BCPM  X=xpos  Z=zpos,
                     Rmin=ctbb_PmtMinR  Rmax=ctbb_PmtMaxR  Dz=ctbb_PmtLen/2
      zpos = zpos - (ctbb_PmtLen + ctbb_BaseLen)/2
      Create and Position BCSK  X=xpos  Z=zpos,
                     Rmin=ctbb_BaseMinR  Rmax=ctbb_BaseMaxR   Dz=ctbb_BaseLen/2
      Create and Position BSEL  X=xpos  Z=zpos dx=ctbb_ElecThck/2, 
                               dy=ctbb_BaseMinR-0.1  dz=ctbb_BaseLen/2
*
*                           Outer slab + readout
*
      zpos  =  (tray_length-ctbb_Slab2Len)/2-tray_WallThk-ctbb_Wrap-ctbb_Shim
      xpos  =  -tray_Height/2+ctbb_Slab2x
      Create and Position BCSA          dx=ctbb_SlabThck/2,
                                        dy=ctbb_SlabWid/2,
                                        dz=ctbb_Slab2Len/2,
                                        X=xpos,
                                        Z=-zpos
      zpos = zpos - (ctbb_Slab2len + ctbb_ConvLen)/2
                 Position BCCV  X=xpos  Z=-zpos  alphax=180,
                     Dx1=ctbb_SlabThck/2  Dx2=ctbb_SlabThck/2,
                     Dy1=ctbb_ConvWidM/2  Dy2=ctbb_SlabWid/2  dz=ctbb_ConvLen/2
      zpos = zpos - (ctbb_ConvLen + ctbb_PmtLen)/2
                 Position BCPM  X=xpos  Z=-zpos,
                     Rmin=ctbb_PmtMinR  Rmax=ctbb_PmtMaxR  Dz=ctbb_PmtLen/2
      zpos = zpos - (ctbb_PmtLen + ctbb_BaseLen)/2
                 Position BCSK  X=xpos  Z=-zpos,
                     Rmin=ctbb_BaseMinR  Rmax=ctbb_BaseMaxR   Dz=ctbb_BaseLen/2
                 Position BSEL  X=xpos  Z=-zpos dx=ctbb_ElecThck/2,  
                                dy=ctbb_BaseMinR-0.1  dz=ctbb_BaseLen/2
*
*     currently not on board
*      Create and Position BCEL  X=ctbb_ElecX, Z=ctbb_ElecZ, 
*                     dx=ctbb_ElecThck/2  dy=ctbb_ElecWid/2  dz=ctbb_ElecLen/2
*
EndBlock
*
*-------------------------------------------------------------------------------
*
Block BTTC  is  the Main Tray Cavity filled with MANY details for TOF
*
      Attribute  BTTC     seen=1  colo=2
      Material   Air
      Shape      BOX      dx=tray_height/2-tray_WallThk,  
                          dy=tray_Width/2-tray_WallThk,
                          dz=tray_Length/2-tray_WallThk
*
*
*     There isn't any room for BCEL in BMTC currently. The TOF electronics is 
*     not 
*     designed yet. Maybe the electronics can be included in each pmt base.
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
*
EndBlock
*
*------------------------------------------------------------------------------
Block BMTM  is  the Main Tray cavity divisions Mother volume for TOF
*           BMTM has the identical size as BTTC and exists to allow the division 
*           of BTTC into 5 strips in phi while BTTC is also containing other 
*           stuff. 
* 
      Attribute BMTM      seen=0   colo=1
      Material   Air
      Shape      BOX      dx=tray_height/2-tray_WallThk,  
                          dy=tray_Width/2-tray_WallThk,
                          dz=tray_Length/2-tray_WallThk
        Create   BMTD  " dont need to positition it, because this is division "
EndBlock
*
*------------------------------------------------------------------------------

Block BMTD is a phi column of TOF Scintillators
      Attribute BMTD      seen=0   colo=1
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
*
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
*
*------------------------------------------------------------------------------
*
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
*
*------------------------------------------------------------------------------
*
Block BCCV  is  a  Ctb optical ConVerter
      Attribute BCCV      seen=1   colo=3
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
      Attribute BCSK      seen=1   colo=6
      Material Aluminium 
      Shape   TUBE   Rmin=0  Rmax=0   Dz=0
EndBlock
*
Block BSEL  is  a  Ctb PM electronics
      Attribute BSEL      seen=1   colo=6
      Material silicon
      Shape   BOX    dx=0  dy=0  dz=0
EndBlock
*
*Block BCEL  is  a  Ctb PM electronics
*      Attribute BCEL      seen=1   colo=6
*      Material silicon
*      Shape   BOX    dx=0  dy=0  dz=0
*EndBlock
*
*******************************************************************************
*
Block BUND   is  Undercarriage support tray 
*
      Attribute BUND      seen=0   colo=1
      SHAPE  BOX   dx=tray_SupFullH/2  dy=tray_Width/2  dz=tray_SupLen/2

*   normally material should be declared INSIDE each of the following volumes
*   but here we apparently have a rather crazy case of extreme details 
*
      Material   Aluminium
                 xpos = (tray_SupFullH - tray_FootThk)/2
                 ypos = (tray_width - tray_Foot1Len)/2 - tray_FootInse 
      Create and Position  BTFT   X = xpos,
                 Y = -ypos dx = tray_FootThk/2 dy = tray_Foot1Len/2
                 Position  BTFT   X = xpos,
                 Y = +ypos dx = tray_FootThk/2 dy = tray_Foot1Len/2
                 xpos = (tray_SupFullH - tray_Foot2Thk)/2
                 ypos = ypos - (tray_Foot1Len + tray_FootThk)/2 
                 Position  BTFT   X = xpos,
                 Y = -ypos dx = tray_Foot2Thk/2 dy = tray_FootThk/2
                 Position  BTFT   X = xpos,
                 Y = +ypos dx = tray_Foot2Thk/2 dy = tray_FootThk/2
                 xpos = (tray_SupFullH + tray_FootThk)/2 - tray_Foot2Thk 
                 ypos = ypos - (tray_FootThk + tray_Foot3Len)/2
                 Position  BTFT   X = xpos,
                 Y = -ypos dx = tray_FootThk/2 dy = tray_Foot3Len/2
                 Position  BTFT   X = xpos,
                 Y = +ypos dx = tray_FootThk/2 dy = tray_Foot3Len/2
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
      Create and Position  BCOV   X=-tray_SupFullH/2+tray_CoolOutR
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block BTFT  is the Foot structure    ( Material  Aluminium )
      Attribute BTFT      seen=1   colo=2
      Shape     BOX    dx = 0.0  dy = 0.0  dz = 0.0 
EndBlock

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block BARM  is  a TPC cooling structure arm             ( Material  Aluminium )
      Attribute BARM      seen=1   colo=2
      Shape     BOX    Dx=tray_SupArmT/2   DY=support_arm_width/2
EndBlock

Block BANG  is  an angled part of TPC cooling structure ( Aile )
      Attribute BANG      seen=1   colo=2
      Shape     PARA   dx=tray_SupArmT/2   Dy=support_aile_width/2,
                       Alph=-60   thet=0   phi=0
EndBlock

Block BASE  is  a bottom of TPC coolant structure       
      Attribute BASE      seen=1   colo=2
      Shape     BOX    Dx=tray_SupBaseT/2  Dy=tray_SupBaseW/2
EndBlock

Block BCOV  is  a whole TPC cooling channel             ( Material  Aluminium )
      Attribute BCOV      seen=1   colo=2
      Shape     TUBE   Rmin=tray_CoolInnR   Rmax=tray_CoolOutR
      Create and Position BWAT 
EndBlock

Block BWAT  is  TPC cooling water
      Attribute BWAT      seen=1   colo=2
      Component H2     A=1   Z=1   W=2
      Component O      A=16  Z=8   W=1
      Mixture   Water  Dens=1.0
      Shape     TUBE   Rmin=0  Rmax=tray_CoolInnR
EndBlock
* ----------------------------------------------------------------------------
   end

