*******************************************************************************
*
*******************************************************************************
Module  BTOFGEO is the Geometry of Barrel Trigger / Time Of Flight system 
  Author     W.J. Llope, Geary Eppley, Harlan Howe, Pablo Yepes
  Created    23 March 1996
*---Version 1------------------------------------------------------------------
* Original   Version of Harlan Howe, Pablo Yepes modified by Pavel Nevski
* modified   19 Sept 1996, Geary Eppley
* modified   17 Mar  1997, GE - new aluminum feet for the tray, new tray height
* modified   22 Oct  1997, GE - position scintillator as built
* modified   20 Apr  1998, GE - add btog.choice: 1 CTB, 2 TOF, 3 25% TOF
*---Version 2------------------------------------------------------------------
* modified   02 Dec  1998, WJL- changed tray height to 8.89 (was 8.70)
*                          WJL- changed CTB base length to 4.0 (was 13.0)
*                          WJL- changed CTB base outer/inner R values
*                          WJL- cleaned up CTBB structure
*                          WJL- TOFp geometry:
*                          WJL-    new hierarchy w/ BASS = Slat Assembly
*                          WJL-    tray (BTTC) filled with LastAFoam, not air
*                          WJL-    electronics boards now G10, not silicon
*                          WJL-    added BFEE (Disc/CW Control Boards)
*                          WJL-    uncommented/changed BCEL (Boards in CW Base)
*                          WJL-    added interior cooling rails and pipes
* modified   09 Dec  1998, GE - add btog.choice: 4: 1 TOF at 5 o'clock west
*******************************************************************************
+CDE,AGECOM,GCUNIT,GCONST.
*
*   List of GEANT volumes produce here:
      Content   BTOF,BTOH,BSEC,BTRA,BUND,BTFT,BASE,BARM,BANG,
                BWAT,BCOV,BXTR,BMTC,BTTC,BMTM,BMTD,BASS,BXSA,BCSB,BCCV,
                BCPM,BCSK,BTSK,BZEL,BCEL,BFEE,BCOO,BRAI,BPIP
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
      Structure CTBB { Slab1Len, Slab2Len, Slab1x,  
                       Slab2x,   SlabThck, SlabWid,  
                       ConvLen,  ConvWidM, ConvThck, 
                       PmtLen,   PmtMaxR,  PmtMinR, 
                       BaseLen,  BaseMaxR, BaseMinR, 
                       ElecThck, Wrap,     Shim   }
*
      Structure TOFF { Slat1Len, Slat1z,   SlatDz,   
                       SlatThck, SlatWid,  SlatAng,
                       PmtLen,   PmtMaxR,  PmtMinR, 
                       BaseLen,  BaseMaxR, BaseMinR,
                       ElecX,    Elec1z,   ElecDz,
                       ElecThck, ElecWid,  ElecLen,
                       RailThck, RailWid,
                       CoolInnR, CoolOutR } 
*
      real      support_arm_width,  support_arm_Xpos,  support_arm_Ypos,
                support_aile_width, support_aile_Ypos, 
                xpos, ypos, zpos, totlen, zpbass, zpfee
      integer   i, is, choice, tof
*
* -------------------------------------------------------------------------
*
      Fill BTOG ! Barrel Trigger, CTB/TOF Basic dimensions 
         Version   = 2         ! geometry version
         Rmin      = 207.80    ! minimum CTB/TOF system radius (as built)
         Rmax      = 219.5     ! maximum CTB/TOF system radius
         dz        = 246.0     ! CTB/TOF tube half length
         choice    = 4         ! 1=CTB, 2=TOF, 3=1 tray TOF+rest CTB
*
      Fill TRAY ! general tray stats        
         Height    =  8.89      ! tray height
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
*
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
         BaseLen   = 4.0        ! Base length
         BaseMaxR  = 2.13       ! Base max radius
         baseMinR  = 1.0        ! Base min radius
         ElecThck  = 0.25       ! readout electronics thickness
         Wrap      = 0.13       ! thickness of Tyvek + black plastic
         Shim      = 0.26       ! thickness of shim to position slat 2
*
      Fill TOFF ! time of flight stats
         Slat1Len  = 22.0        ! slat length
         Slat1z    = 101.5       ! slat 1 Z position
         SlatDz    = 24.         ! slat Z separation (Same as toff_ElecDz)
         SlatThck  = 2.0         ! scintillator slab thicknesses
         SlatWid   = 4.0         ! scintillator slab width
         SlatAng   = 8.          ! slat assy. angle w.r.t. tray
         PmtLen    = 5.0         ! PMT length
         PmtMaxR   = 1.9         ! PMT max radius
         PmtMinR   = 1.8         ! PMT min radius
         BaseLen   = 8.0         ! Base length
         BaseMaxR  = 2.1         ! Base max radius
         baseMinR  = 1.97        ! Base min radius  
         ElecX     = 4.19        ! FEE Board x position
         Elec1z    = 104.0       ! FEE Board 1 z position
         ElecDz    = toff_SlatDz ! FEE Board Dz (Same as toff_SlatDz)
         ElecThck  = 0.17        ! FEE Board thickness (67 mils)
         ElecWid   = 21.0        ! FEE Board width
         ElecLen   = 16.0        ! FEE Board length
         RailThck  = 0.2         ! Cooling loop rail thickness
         RailWid   = 1.0         ! Cooling loop rail width
         CoolOutR  = 0.32        ! Cooling loop pipe outer radius
         CoolInnR  = 0.28        ! Cooling loop pipe inner radius
      EndFill
*
      USE   BTOG
      USE   TRAY
      USE   CTBB
      USE   TOFF
*
      Create and Position BTOF in Cave
*
*******************************************************************************
*     Rotate the +z half of the tube so that the phi divisions run clockwise
*     for each half of the detector when viewed from outside. The direction of 
*     increasing z is then toward eta=0 in each half.
*
Block BTOF is the whole CTF system envelope 
      Attribute BTOF      seen=0  colo=1
      Material  Air
      Medium    Standard
      Shape     Tube      rmin=btog_Rmin  Rmax=btog_Rmax  dz=btog_dz
      choice = btog_choice
      Create and Position BTOH  z=+btog_dz/2    alphay=180
      choice = 1
      Create and Position BTOH  z=-btog_dz/2
EndBlock
*
*------------------------------------------------------------------------------
*  BSEC only is needed to model a system that contains both TOF and CTB
*  In the event that z symmetry is broken, two copies of BTOH/BSEC
*  with DIFFERENT names are automatically created by AGI 
*
Block BTOH is a half of trigger system (west-east)
      Attribute BTOH      seen=0  colo=1  serial=choice
      Shape     Tube      dz=btog_dz/2

      do is=1,60
         tof=0
         if (choice==3 & 51<=is&is<=65)    tof=1
         if (choice==4 &     is==23   )    tof=1
         Create and Position BSEC  alphaz = 102+6*is
      enddo
EndBlock
*
*------------------------------------------------------------------------------
Block BSEC is a sector of CTB/TOF Trigger Barrel Scintillators
      Attribute BSEC      seen=0   colo=1  serial=tof  
      Shape     Tubs      phi1 = -3.0 phi2 = 3.0
      Create and Position BTRA   X = _ 
                          btog_Rmin+(tray_SupFullH+tray_height+tray_StripT)/2
EndBlock
*
*------------------------------------------------------------------------------
*
Block BTRA is one full tray plus supporting structure for CTB/TOF
*     remember that volume attributes are inherited, no need to redefine serial
      Attribute BTRA      seen=0   colo=2
      Shape     BOX       dx=(tray_SupFullH+tray_height+tray_StripT)/2,
                          dy=tray_Width/2
      Create and Position BXTR     X=(tray_SupFullH+tray_StripT)/2,
                                   z=(btog_dz-tray_length)/2
      Create and Position BUND     X=-(tray_height+tray_StripT)/2,
                                   z=(btog_dz-tray_SupLen)/2
EndBlock
*
*------------------------------------------------------------------------------
*
Block BXTR  is a Main TRay covering box for CTB
      Attribute BXTR      seen=1   colo=2
      Material   Aluminium
      Shape      BOX      DX=tray_height/2,
                          dz=tray_length/2  
      if (tof==1) then
         Create and Position BTTC
      else
         Create and Position BMTC
      endif
EndBlock
*
*------------------------------------------------------------------------------
*
Block BMTC  is  the Main Tray Cavity filled with MANY details for CTB
      Attribute  BMTC     seen=1   colo=5
      Material   Air
      Shape      BOX      dx=tray_height/2-tray_WallThk,  
                          dy=tray_Width/2-tray_WallThk,
                          dz=tray_Length/2-tray_WallThk
*
*---- inner slab + readout
      zpos  =  (tray_length-ctbb_Slab1Len)/2-tray_WallThk-ctbb_Wrap
      xpos  =  -tray_Height/2+ctbb_Slab1x
      Create and Position BXSA           dx=ctbb_SlabThck/2,
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
      Create and Position BZEL  X=xpos  Z=zpos dx=ctbb_ElecThck/2, 
                               dy=ctbb_BaseMinR-0.1  dz=ctbb_BaseLen/2
*
*---- outer slab + readout
      zpos  =  (tray_length-ctbb_Slab2Len)/2-tray_WallThk-ctbb_Wrap-ctbb_Shim
      xpos  =  -tray_Height/2+ctbb_Slab2x
      Create and Position BXSA          dx=ctbb_SlabThck/2,
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
                 Position BZEL  X=xpos  Z=-zpos dx=ctbb_ElecThck/2,  
                                dy=ctbb_BaseMinR-0.1  dz=ctbb_BaseLen/2
EndBlock
*
*-------------------------------------------------------------------------------
Block BTTC  is  the Main Tray Cavity filled with MANY details for TOF
      Attribute  BTTC      seen=1  colo=5
      Component  C         A=12 Z=6 W=1
      Component  H2        A=1  Z=1 W=2
      Mixture    LastAFoam Dens=0.048
      Shape      BOX       dx=tray_height/2-tray_WallThk,  
                           dy=tray_Width/2-tray_WallThk,
                           dz=tray_Length/2-tray_WallThk
      Create    BFEE       dx=toff_ElecThck/2, 
                           dy=toff_ElecWid/2, 
                           dz=toff_ElecLen/2
      zpfee     = toff_Elec1z
      Do i      = 1,9
       Position BFEE X=toff_ElecX Z=zpfee
       zpfee    = zpfee - toff_ElecDz
      Enddo
      Create and Position  BCOO X=0 Y=0 dx=0 dy=0 dz=0
      Create and Position  BMTM   
EndBlock
*
*------------------------------------------------------------------------------
*           BMTM has the identical size as BTTC and exists to allow the division 
*           of BTTC into 5 strips in phi while BTTC is also containing other 
*           stuff. 
*
Block BMTM  is  the Main Tray cavity divisions Mother volume for TOF
      Attribute BMTM      seen=0   colo=1
      Material  Air
      Shape     BOX      dx=tray_height/2-tray_WallThk,  
                         dy=tray_Width/2-tray_WallThk,
                         dz=tray_Length/2-tray_WallThk
      Create    BMTD  " dont need to positition it, because this is division "
EndBlock
*
*------------------------------------------------------------------------------
Block BMTD is a phi column of TOF Scintillators
      Attribute BMTD      seen=1   colo=1
      Shape     division  Iaxis=2  Ndiv=5  
      Create    BASS
      zpbass    = toff_Slat1z
      Do i      = 1,9
       Position BASS X=-0.8 Z=zpbass alphay=toff_SlatAng konly='MANY'
       zpbass   = zpbass - toff_SlatDz
      Enddo
EndBlock
*
*------------------------------------------------------------------------------
Block BASS is a single TOF Slat Assembly (slat+PMT+base)
      Attribute BASS seen=1 colo=6
      totlen = toff_Slat1Len+toff_PmtLen+toff_BaseLen
      Shape BOX dx=toff_PmtMaxR, 
                dy=(tray_Width/2-tray_WallThk)/5.,
                dz=totlen/2.
      zpos = -(totlen-toff_Slat1Len)/2
      Create and Position BCSB  dx=toff_SlatThck/2 dy=toff_SlatWid/2,
                                dz=toff_Slat1Len/2 X=0 Z=zpos
      zpos = zpos + (toff_Slat1Len+toff_PmtLen)/2
      Create and Position BCPM  X=0                Z=zpos,
                                Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR,  
                                Dz=toff_PmtLen/2
      zpos = zpos + (toff_PmtLen + toff_BaseLen)/2
      Create and Position BTSK  X=0                Z=zpos,
                                Rmin=toff_PmtMinR  Rmax=toff_PmtMaxR,  
                                Dz=toff_BaseLen/2
      Create BCEL               Rmin=0             Rmax=toff_PmtMinR,
                                Dz=toff_ElecThck/2
      zpos = zpos + 0.4 - toff_BaseLen/2
      Do i = 1,6
       Position BCEL  Z=zpos + i
      Enddo
EndBlock
*
*------------------------------------------------------------------------------
Block BXSA  is  the active trigger scintillator SLAB for ctb 
      Attribute BXSA      seen=1   colo=3
      Material polystyren
      Medium   sensitive    IsVol=1
      Shape   BOX     dx=0 dy=0 dz=0
*
*   hit options: H - put in GEANT hit field (instead of PseudoVolumes)
*                S - Single step
*
      HITS    BXSA   XX:16:HS(-250,250)   YY:16:(-250,250)    ZZ:16:(-250,250),
                     px:16:(-100,100)     py:16:(-100,100)    pz:16:(-100,100),
                     Sleng:16:(0,1.e4)    ToF:16:(0,1.e-6)    Step:16:(0,100),
                     ShtN:16:             Eloss:32:(0,1) 
EndBlock
*
*------------------------------------------------------------------------------
Block BCSB  is  the active trigger scintillator SLAB for tof
      Attribute BCSB      seen=1   colo=4
      Material polystyren
      Medium   sensitive    IsVol=1
      Shape   BOX     dx=0 dy=0 dz=0
*
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
Block BCCV  is  a  Ctb optical ConVerter
      Attribute BCCV      seen=1   colo=3
      Material polystyren 
      Shape   TRD2   Dx1=0   Dx2=0   Dy1=0   Dy2=0  dz=0
EndBlock
* 
Block BCPM  is a PhotoMultiplier Tube (same for CTB and TOF)
      Attribute BCPM      seen=1   colo=1
      Material polystyren 
      Shape   TUBE   Rmin=0  Rmax=0  Dz=0
EndBlock
*
Block BCSK  is a CTB Linear Base tube
      Attribute BCSK      seen=1   colo=2
      Material  polystyren 
      Shape   TUBE   Rmin=0  Rmax=0   Dz=0
EndBlock
*
Block BTSK  is the outer shell of a TOF CW Base 
      Attribute BTSK      seen=1   colo=2
      Material  Aluminium
      Shape   TUBE   Rmin=0  Rmax=0   Dz=0
EndBlock
*
Block BZEL  is a Ctb PM electronics
      Attribute BZEL      seen=1   colo=6
      Material silicon
      Shape   BOX    dx=0  dy=0  dz=0
EndBlock
*
Block BCEL is a G10 board in the CW Base for TOF
      Attribute BCEL seen=1   colo=3
      Component Si   A=28.08  Z=14   W=0.6*1*28./60.
      Component O    A=16     Z=8    W=0.6*2*16./60.
      Component C    A=12     Z=6    W=0.4*8*12./174.
      Component H    A=1      Z=1    W=0.4*14*1./174.
      Component O    A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10  Dens=1.7
      Shape     TUBE Rmin=0  Rmax=0   Dz=0
EndBlock
*
Block BFEE is a G10 discriminator/CW control board for TOF
      Attribute BFEE seen=1   colo=3
      Component Si   A=28.08  Z=14   W=0.6*1*28./60.
      Component O    A=16     Z=8    W=0.6*2*16./60.
      Component C    A=12     Z=6    W=0.4*8*12./174.
      Component H    A=1      Z=1    W=0.4*14*1./174.
      Component O    A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10  Dens=1.7
      Shape     BOX  dx=0  dy=0  dz=0
EndBlock
*
Block BCOO  are the cooling rails/loops
      Attribute BCOO  seen=1  colo=2
      Shape     BOX   dx=0 dy=0 dz=0
      Create    BRAI  dx=toff_RailThck/2,
                      dy=toff_RailWid/2,
                      dz=tray_Length/2-tray_WallThk
      Position  BRAI  X=toff_ElecX-toff_RailThck,
                      Y= (tray_width/2-toff_RailWid/2-tray_WallThk),
                      konly='MANY'
      Position  BRAI  X=toff_ElecX-toff_RailWid/2-toff_RailThck/2,
                      Y= (tray_width/2-toff_RailThck/2-tray_WallThk),
                      alphaz=90,
                      konly='MANY'
      Position  BRAI  X=toff_ElecX-toff_RailThck,
                      Y=-(tray_width/2-toff_RailWid/2-tray_WallThk),
                      konly='MANY'
      Position  BRAI  X=toff_ElecX-toff_RailWid/2-toff_RailThck/2,
                      Y=-(tray_width/2-toff_RailThck/2-tray_WallThk),
                      alphaz=90,
                      konly='MANY'
      Create    BPIP  Rmin=toff_CoolInnR Rmax=toff_CoolOutR,
                      dz=tray_Length/2-tray_WallThk
      Position  BPIP  X=toff_ElecX-3.*toff_RailThck/2.-toff_CoolOutR,
             Y= (tray_width/2-toff_RailThck-tray_WallThk-toff_CoolOutR),
             konly='MANY'
      Position  BPIP  X=toff_ElecX-3.*toff_RailThck/2.-toff_CoolOutR,
             Y=-(tray_width/2-toff_RailThck-tray_WallThk-toff_CoolOutR),
             konly='MANY'
EndBlock
*
Block BRAI  is the Rail for the cooling loop
      Attribute BRAI   seen=1  colo=1
      Material  Aluminium
      Shape     BOX    dx=0.0  dy=0.0  dz=0.0 
EndBlock
*
Block BPIP  is the Pipe for the cooling loop
      Attribute BPIP seen=1  colo=1
      Material  Aluminium
      Shape     TUBE Rmin=0  Rmax=0  Dz=0
EndBlock
*
*******************************************************************************
Block BUND   is  Undercarriage support tray - same both for CTB and TOF 
      Attribute BUND      seen=0   colo=1   serial=0
      Shape  BOX dx=tray_SupFullH/2  dy=tray_Width/2  dz=tray_SupLen/2
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
*
Block BTFT  is the Foot structure    ( Material  Aluminium )
      Attribute BTFT      seen=1   colo=2
      Shape     BOX    dx = 0.0  dy = 0.0  dz = 0.0 
EndBlock
*
Block BARM  is  a TPC cooling structure arm             ( Material  Aluminium )
      Attribute BARM      seen=1   colo=2
      Shape     BOX    Dx=tray_SupArmT/2   DY=support_arm_width/2
EndBlock
*
Block BANG  is  an angled part of TPC cooling structure ( Aile )
      Attribute BANG      seen=1   colo=2
      Shape     PARA   dx=tray_SupArmT/2   Dy=support_aile_width/2,
                       Alph=-60   thet=0   phi=0
EndBlock
*
Block BASE  is  a bottom of TPC coolant structure       
      Attribute BASE      seen=1   colo=2
      Shape     BOX    Dx=tray_SupBaseT/2  Dy=tray_SupBaseW/2
EndBlock
*
Block BCOV  is  a whole TPC cooling channel
      Attribute BCOV      seen=1   colo=2
      Shape     TUBE   Rmin=tray_CoolInnR   Rmax=tray_CoolOutR
      Create and Position BWAT 
EndBlock
*
Block BWAT  is  TPC cooling water
      Attribute BWAT      seen=1   colo=2
      Component H2     A=1   Z=1   W=2
      Component O      A=16  Z=8   W=1
      Mixture   Water  Dens=1.0
      Shape     TUBE   Rmin=0  Rmax=tray_CoolInnR
EndBlock
*
* ----------------------------------------------------------------------------
   end
