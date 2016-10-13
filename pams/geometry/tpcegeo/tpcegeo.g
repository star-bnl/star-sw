****************************************************************************
* $Id: tpcegeo.g,v 1.31 2011/02/28 16:47:18 jwebb Exp $
*
* $Log: tpcegeo.g,v $
* Revision 1.31  2011/02/28 16:47:18  jwebb
* Added "EndFILL" statements where required.
*
* Revision 1.30  2009/11/10 02:14:31  perev
* Where GSTPAR, set local material avoid bug in gphysi
*
* Revision 1.29  2005/07/14 22:06:43  potekhin
* An informative diagnostics about the TPC gas density added,
* prompted by a recent debugging session with Yuri
*
* Revision 1.28  2003/11/13 01:37:46  potekhin
* Added four missing lines of code
* for the gas density correction,
* a comment from Yuri and a diagnostic
* print.
*
* Revision 1.27  2003/11/13 00:56:40  potekhin
* Added the Log CVS tag
*
*
module   TPCEGEO  is the TPC system in GSTAR
   Author   Pavel Nevski (MEPhI) 
   Created  March 16, 1996
*  Original Version - Peter Jacobs (LBL), March 16, 1995.                   *
*  dimensions checked with the latest design with D.Fritz and H.Weiman      *
*  modification history                                                     *
*  PN  04/04/96: MWC sensetive volumes added into TPC sectors               *
*  WJL 04/15/97: changed no of pads in rows 10, 13, and 38                  *
*  WJL 05/07/97: changed divisioning of TMSE to work with new /sim/mws      * 
*  PN  05/29/97: interpolation inside padrow corrected                      *
*                Laser hits generated in a special subroutine TPCELASER     *
*  PN  06/28/97: gas in pseudo-padrows and MWC volumes corrected            *
*  PN  08/15/97: CSADDR,vect_inp/out not used anymore                       *
*  PN  08/21/97: anything denser than TPC gas absorbs laserino              *
*  PN  10/28/98: two more pseudo-padrows in the outer sector for Iwona      *
*  PN  11/10/99: full gap between gating grid and the padplane is sensitive *
*  PN  03/10/01: TMSE hit x-size determined automatically with MWCread>=2   *
*  PN  06/04/02: TPSM membrane material changed to mylar, copper was a bug  *
*****************************************************************************
+cde,AGECOM,GCUNIT,GCONST.     " - standart geant commons "
Content   TPCE,TOFC,TOFS,TOST,TOKA,TONX,TOAD,TOHA,TPGV,TPSS,
          TIFC,TIAL,TIKA,TINX,TPCW,TWSS,TWGI,TPCM,TPEA,
          TESS,TSEC,TMWC,TMEA,TMSE,TIAG,TOAE,TPAD,TPAI,TPAO      
*
Structure TPCG { version,rmin,rmax,length,WheelIR,WheelOR,WheelTHK,
                 SenGasOR,tpeaTHK,MembTHK,tiadDR,tinxDR,tikaDR,tialDR, 
                 tocsDR,tokaDR,tonxDR,toadDR,toigDR,toalDR,tohaDR,MWCread,
                 gasCorr }
*
Structure TECW { sec,GapRad,GapHeit,GapWidI,GapWidO,inwidth,ouwidth,
                 height,ppdepth,asdepth,ggdepth,MWCdepth,boundary,
                 Rcenter,MWCinn,MWCout,MWChei,MwcCent,MwcNwir,n,nex,
                 z(8),dz(8),xex(5),zex(5),dxex(5),dzex(5) } 
*
Structure TPRS { sec,Nrow,pitch,width,super,Rpads(40),Npads(40) }
*
Real      tocsIR,tocsOR,tokaIR,tokaOR,tonxIR,tonxOR,toadIR,toadOR,toigIR,toigOR,
          toalIR,toalOR,tohaIR,tohaOR,tofsIR,tofsOR,tofcIR,tofcOR,
          tiadIR,tiadOR,tinxIR,tinxOR,tikaIR,tikaOR,tialIR,tialOR,
          tifcIR,tifcOR,tpgvIR,tpgvLeng,tofcLeng,
          tpcwz,tpgvz,tpeaZ,dx_dz,dxb,del,a,b

Real      T, P, PATM, RHO, density

Integer   i_row,i_sec,i
External  TPADSTEP,TPAISTEP,TPAOSTEP,TPCELASER
*******************************************************************************
*
   Fill  TPCG             !  TPC basic dimensions
      version    = 2        ! current version
      rmin       = 46.107   ! TPC inner radius 
      rmax       = 207.750  ! TPC outer radius
      length     = 459.37   ! TPC full length
      WheelIR    = 49.60    ! support wheel inner radius
      WheelOR    = 206.75   ! support wheel outer radius
      WheelTHK   = 11.43    ! support wheel length
      SenGasOR   = 200      ! TPC sensitive gas outer radius
      tpeaTHK    = 9.7      ! endcap MWC + sector thickness
      MembTHK    = .00762   ! Central membrane thickness
      tocsDR     = .013     ! outer copper thickness
      tokaDR     = .015     ! outer kapton thickness
      tonxDR     = 1.00     ! outer nomex thickness
      toadDR     = 0.02     ! outer adhesive thickness
      toigDR     = 5.70     ! outer isolating gas thickness
      toalDR     = 0.40     ! outer aluminum thickness
      tohaDR     = 0.60     ! outer HoneyUcomb Al thickness  
      tiadDR     = 0.080    ! inner adhesive layer thickness
      tinxDR     = 1.270    ! inner nomex structure thickness
      tikaDR     = 0.015    ! inner Kapton layer thickness
      tialDR     = 0.004    ! inner aluminum layer thickness
      MWCread    = 2        ! MWC readout flag
      gasCorr    = 1        ! gas density correction version
   EndFILL
*
   Fill TPRS              ! sector of padrows
      sec    = 1            ! sector number: 1 for inner, 2 for outer
      nRow   = 13           ! number of padrows in the sector
      pitch  = 0.335        ! tpc padrow pitch width
      width  = 1.15         ! tpc padrow thickness
      super  = 3            ! number of padraws in a superpadrow
      Npads  = { 88, 96, 104, 112, 118, 126, 134, 142, 150, 
                158, 166, 174, 182 }        ! number of pads in row
      Rpads  = {60.0, 64.8, 69.6, 74.4, 79.2, 84.0, 88.8, 93.6, 98.8, 
               104.0,109.2,114.4,119.6 }    ! tpc padrow radii
   EndFILL
*
   Fill TPRS              ! sector of padrows
      sec    = 2            ! sector number: 1 for inner, 2 for outer
      nRow   = 32           ! number of padrows in outer sector
      pitch  = 0.67         ! outer tpc padrow pitch width
      width  = 1.95         ! outer tpc padrow thickness
      super  = 1            ! number of padrows in a superpadrow
      Npads  = { 98, 100, 102, 104, 106, 106, 108, 110, 112,
                112, 114, 116, 118, 120, 122, 122, 124, 126, 
                128, 128, 130, 132, 134, 136, 138, 138, 140, 
                142, 144, 144, 144, 144 } ! number of pads in row
      Rpads  = {127.195, 129.195, 131.195, 133.195, 135.195, 
                137.195, 139.195, 141.195, 143.195, 145.195, 
                147.195, 149.195, 151.195, 153.195, 155.195, 
                157.195, 159.195, 161.195, 163.195, 165.195, 
                167.195, 169.195, 171.195, 173.195, 175.195, 
                177.195, 179.195, 181.195, 183.195, 185.195, 
                187.195, 189.195 }        ! tpc padrow radii
   EndFILL
*
   Fill TECW              ! EC trapezoid and support Wheel
      sec      = 1          ! sector number: 1 for inner, 2 for outer
      GapWidI  = 2.* 10.91  ! air in support wheel - inner width
      GapWidO  = 2.* 27.56  ! air in support wheel - outer width
      GapHeit  = 62.15      ! air in support wheel - height (dr)
      GapRad   = 87.6       ! air in support wheel - center radius
      inwidth  = 2.* 13.75  ! sector width at inner radius 
      ouwidth  = 2.* 32.38  ! sector width at outer radius
      height   = 69.52      ! sector radial height
      ppdepth  = 1.35+0.32  ! padplane thickness (both Al and PCB)
      asdepth  = 6.99       ! depth of openings in aluminum structure
      ggdepth  = 1.0        ! MWC gap from gated grid to pad plane
      MWCdepth = 1.0        ! sensitive MWC gas gap full thickness
      boundary = 3.74       ! al frame - boundary width
      Rcenter  = 86.669     ! sector center radius (set by precision holes)
      MwcInn   = 2*12.804   ! MWC sensitive region inner size
      MwcOut   = 2*29.953   ! MWC sensitive region outer size
      MwcHei   = 64         ! MWC sensitive region height (radial)
      MwcCent  = -1.669     ! sensitive region center position
      MwcNwir  = 160        ! number of MWC sensitive wires
      n        = 5          ! number of air gaps in Al
      nex      = 0          ! number of extra aluminum support pieces
      z    = { 6.67, 18.22, 34.22, 50.22, 62.65 }  ! positions of air gaps
      dz   = { 6.13, 15.05, 15.05, 15.05,  7.09 }  ! size of air gaps
      xex  = { 0, 0, 0, 0, 0 }  ! x positions of extra aluminum
      zex  = { 0, 0, 0, 0, 0 }  ! z positions of extra aluminum
      dxex = { 0, 0, 0, 0, 0 }  ! x-thickness of extra aluminum
      dzex = { 0, 0, 0, 0, 0 }  ! z-thickness of extra aluminum
   EndFILL
*
   Fill TECW              ! endcap trapezoid and support Wheel
      sec      = 2          ! sector number: 1 for inner, 2 for outer
      GapWidI  = 2.* 28.92  ! air in support wheel - inner width
      GapWidO  = 2.* 46.74  ! air in support wheel - outer width
      GapHeit  = 66.47      ! air in support wheel - height (dr)
      GapRad   = 157.34     ! air in support wheel - radius
      inwidth  = 64.92      ! sector width at inner radius 
      ouwidth  = 103.25     ! sector width at outer radius 
      height   = 71.51      ! sector radial height
      ppdepth  = 0.95+0.32  ! padplane thickness (both Al and PCB)
      asdepth  = 6.99       ! depth of openings in aluminum structure
      ggdepth  = 1.4        ! MWC gap from gated grid to pad plane
      MWCdepth = 1.4        ! sensitive MWC gas gap full thickness
      boundary = 3.74       ! al frame - boundary width
      Rcenter  = 157.488    ! sector center radius (set by precision holes)
      MwcInn   = 2*32.202   ! MWC sensitive region inner size
      MwcOut   = 2*49.351   ! MWC sensitive region outer size
      MwcHei   = 64         ! MWC sensitive region height (radial)
      MwcCent  = -0.093     ! sensitive region center position
      MwcNwir  = 160        ! number of MWC sensitive wires
      n        = 8          ! number of air gaps in Al
      nex      = 5          ! number of extra aluminum support pieces
      z    = { 8.216, 16.217, 24.218, 32.219, 
              40.220, 48.221, 56.222, 64.143    }     ! opening positions
      dz   = { 7.05,   7.05,   7.05,   7.05,
               7.05,   7.05,   7.05,   6.89     }     ! opening size 
      xex  = { 0., 0., 0., -21.907, 21.907      }     ! x positions
      zex  = { 6.69, 32.22, 40.22, 56.22, 56.22 }     ! z positions
      dxex = { 2.54, 0.476, 0.476, 3.175, 3.175 }     ! x-thickness
      dzex = { 4.00, 7.05,  7.05,  7.05,  7.05  }     ! z-thickness
   EndFILL
*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
      Use  TPCG  
*
*  TPC basic dimensions are the full system size, the gas volume length
*  and inner radius are derived from them and from material thicknesses.
*  The outer gas radius is also an input ( as this is used for winding ),
*  but the remaining clearance is checked to be positive (PN, 16 Mar 96).
*
      tofcLENG = tpcg_Length-2*tpcg_WheelTHK              ! gas plus endcaps
      tpgvLeng = (tofcLeng-tpcg_MembTHK-2*tpcg_TpeaThk)/2 ! active gas
*
* layer names mnemonic:
*-----------------------------------------------------------------------
* letter 1-6 :   t  :     i/o     :  cs/ka/nx/al/fc/fs  :     IR/OR    :
*-----------------------------------------------------------------------
*   meaning  :  TPC : inner/outer :    materials as     :  inner/outer :
*            :      :     cage    : Copper shild,Kapton :    radius    :
*            :      :             : Nomex,Aluminum  OR  :    of the    :
*            :      :             : field cage and      :    layer     :
*            :      :             : field cage support  :              :
*-----------------------------------------------------------------------
*
*** calculate radii of outer finest structures
      tocsIR = tpcg_SenGasOR;   tocsOR = tocsIR + tpcg_tocsDR
      tokaIR = tocsOR;          tokaOR = tokaIR + tpcg_tokaDR
      tonxIR = tokaOR;          tonxOR = tonxIR + tpcg_tonxDR
      toadIR = tonxOR;          toadOR = toadIR + tpcg_toadDR
      toigIR = toadOR;          toigOR = toigIR + tpcg_toigDR
      toalIR = toigOR;          toalOR = toalIR + tpcg_toalDR
      tohaIR = toalOR;          tohaOR = tohaIR + tpcg_tohaDR
*
*** calculate radii of inner finest structures
      tifcIR = tpcg_Rmin      ! inner field cage inner radius
      tiadIR = tifcIR;          tiadOR = tiadIR + tpcg_tiadDR
      tinxIR = tiadOR;          tinxOR = tinxIR + tpcg_tinxDR
      tikaIR = tinxOR;          tikaOR = tikaIR + tpcg_tikaDR
      tialIR = tikaOR;          tialOR = tialIR + tpcg_tialDR      
*
***  derive radii of larger structures
      tofcIR = tocsIR;          tofcOR = tohaOR
      tofsIR = tocsIR;          tofsOR = toadOR
      tifcIR = tiadIR;          tifcOR = tialOR
      tpgvIR = tifcOR         ! TPC gas inner radius
*
***  make primitive control and basic printout
      del = tpcg_Rmax-tofcOR 
      prin1  tofcOR,del
      (' TPCEgeo: maximum  TPC  Radius is ',F10.4,'  clearance is ',F8.4)
      if (del<0) Print *,' *** TPCEgeo ERROR : outer clearance negative ',del
      prin1 tpgvIR;   (' TPCEgeo: senset. gas inner radius',F10.4)
      prin1 tpgvLeng; (' TPCEgeo: sensitive gas length is ',F10.4)
*
*
      CALL AGSSTEP(TPCELASER)
      create and position TPCE in CAVE 
*
*------------------------------------------------------------------------------
*
block TPCE is the TPC envelope
*
      material  Air
      Medium    Standard
      Attribute TPCE  seen=0 colo=2
      shape     TUBE  rmin=tpcg_rmin  rmax=tpcg_rmax  dz=tpcg_length/2
*                                                                1
      tpgvz  = (tpcg_MembTHK + tpgvLeng)/2               "   gas volume   " 
                                          
      Create and position  TPGV z=+tpgvz,
                                thetax=90 thetay=90 thetaz=0,
                                phix = 75 phiy =-15 phiz=0
                 position  TPGV z=-tpgvz,
                                thetax=90 thetay=90 thetaz=180,
                                phix =105 phiy =195 phiz=0
*                                                                2
      Create and position  TIFC                          "   inner cage   "
      Create and position  TOFC                          "   outer cage   "
      Create and position  TPCM                          "    membrane    "
*
*                                                                3
      tpeaZ  = (tofcLeng - tpcg_TpeaThk)/2               "   TPC endcaps  "
      create and position  TPEA  z=+tpeaZ,
                                thetax=90 thetay=90 thetaz=0,
                                phix = 75 phiy =-15 phiz=0
                 position  TPEA  z=-tpeaZ,
                                thetax=90 thetay=90 thetaz=180,
                                phix =105 phiy =195 phiz=0
*                                                                4
      tpcwz  = (tpcg_LENGth-tpcg_WheelTHK)/2             " support wheels " 
      Create and position  TPCW  z=+tpcwz,
                                thetax=90 thetay=90 thetaz=0,
                                phix = 75 phiy =-15 phiz=0
                 position  TPCW  z=-tpcwz,
                                thetax=90 thetay=90 thetaz=180,
                                phix =105 phiy =195 phiz=0
*
endblock
*
******************************************************************************
*                       section one - sensitive gas                          *
******************************************************************************
*
Block  TPGV is the Gas Volume placed in TPC
*
*     TPC default gas P10: Ar/methane 9:1 by weight
*     Component Ar    A=40  Z=18 W=.9
*     Component C     A=12  Z=6  W=.1*12./16.
*     Component H     A=1   Z=1  W=.1* 4./16.
*     TPC default gas P10: Ar/methane 9:1 by volume
      Component Ar    A=40  Z=18 W=9
      Component C     A=12  Z=6  W=1
      Component H     A=1   Z=1  W=4

      if(TPCG_gasCorr==1) then
         density=0.9*0.001782+0.1*0.000667
         write(*,*) 'Older (buggy) TPC gas density = ',density
      else
* Comment by Y.Fisyak:
* The STAR TPC has been operating with P10 (90% Ar, 10% methane by volume)
* at ~2 mbar above the ambient atmospheric pressure.
* Temperature is regulated close to 75F, both for the hall [supposedly regulated at (75 +/-5)F]
* dAu run (2003) average
         T    = 298.06 "Kelvin degree"
         P    = 1016.59 "mbar"
         PATM = 0.001*P*750.062/760. "mbar => torr => atm"
         rho  =(0.9*0.001782+0.1*0.000717) " g/cm**3 "
* at STP (Standard Temperature and Pressure, 0°C and one atmosphere pressure).

         density=rho*(273.15/T)*PATM
         write(*,*) 'Corrected TPC GAS DENSITY = ',density
      endif

      Mixture   p10   Dens=density   " g/cm**3 "
      SHAPE     TUBE  rmin=tpgvIR  rmax=tpcg_SenGasOR  dz=tpgvLeng/2
      Create    TPSS
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block  TPSS is a division of gas volume corresponding to a supersectors 
      attribute TPSS  seen=0
      shape Division  NDIV=12  IAXIS=2
*
      do i_sec=1,2
         Use    TPRS  sec=i_sec

*        position within supersector (this assumes rectangular padrows)
         do i_row = 1,nint(tprs_nRow)
            If (nint(tprs_super)==3 | i_row==1) then
               Create and Position TPAD  x=tprs_Rpads(i_row)-tprs_width,
                          dx=tprs_width/2 dy=tprs_npads(i_row)*tprs_pitch/2
            endif
            If (nint(tprs_super)>0)  then
               create and position TPAD  x=tprs_Rpads(i_row),
                          dx=tprs_width/2 dy=tprs_npads(i_row)*tprs_pitch/2
            endif
            If (nint(tprs_super)==3 | i_row==nint(tprs_nRow))  then
               Create and Position TPAD  x=tprs_Rpads(i_row)+tprs_width,
                          dx=tprs_width/2 dy=tprs_npads(i_row)*tprs_pitch/2
            endif
         enddo
      enddo
Endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
block TPAD is a real padrow with dimensions defined at positioning time
* We want to use PAI model in padrows to simulate energy loss more precisely.
* (see GEANT PHYS334, Allisson and Cobb, Ann.Rev.Nucl.Part.Sci.30(1980),253).
* (    best formalism in Grishin,Ermilova,Kotelnikov, NIM A307(1991),273).
* To switch it ON only in this volume we introduce P10 as a different MATERIAL,
* since ISTRA flag is kept in medium, but all tables are stored in the material
* According to AGI rules for this we need A parameter. Lets use ISVOL=1 (!)
* (- this eliminates a need for a separate medium definition ?)
*
      attribute TPAD seen=0 colo=2
      material p10
      material sensitive_gas  ISVOL=1  stemax=2.5*tprs_width
      SHAPE    BOX   dx=0   dy=0   dz=0    
*     Call     GSTPAR(ag_imed,'STRA',1.)
*
*     The following is the corrected hits definition: 25-dec-98 (PN)
      HITS    TPAD   Z:.0005:S  Y:.0005:  X:.0005:   cx:10: cy:10: cz:10:,
                     LPtot:18:(-3,2)      Sleng:.1:(0,800),
                     ToF:16:(0,1.e-6)     LGAM:16:(-4,6),    
                     Step:11:(0,10)       USER:21:(-.01,.01) 

endblock
*
********************************************************************************
*        section two:   TPC inner and outer envelopes, membrane                *
* Groups of thin layers are positioned sequentially one inside another keeping *
* the outer radius constant. This is more efficient for GEANT and they can be  *
* easily replaced by single layer with a combined mixture (if needed later)    *
********************************************************************************
*
block TIFC  defines the Inner Field Cage placed in TPC 
*** Contents of Inner Field Cage Structure TIFC: Adhesive,Nomex,Kapton,Aluminum
*   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      Material  Mylar " should be close to Adhesive "
      Attribute TIFC   seen=1  colo=4
      SHAPE     TUBE   rmin=tiadIR   rmax=tifcOR   dz=tofcLeng/2
      Create and position TINX
endblock
*
Block TINX is the inner nomex structure
*
*     nomex - PN  8/3/96 following I.Sacreda note
      component C      A=12  Z=6  W=5
      component H      A=1   Z=1  W=8
      component O      A=16  Z=8  W=2
      mixture   Nomex  Dens=0.048
      Attribute TINX   seen=1  colo=5
      Shape     TUBE   rmin=tinxIR 
      Create and position TIKA
endblock
*
Block TIKA is the kapton film of the inner field cage
      Material  Mylar
      Attribute TIKA   seen=1  colo=6
      SHAPE     TUBE   rmin=tikaIR 
      Create and position TIAL
endblock
*
Block TIAL is the inner Aluminum cylinder
      material  Aluminium
      Attribute TIAL   seen=1  colo=7
      SHAPE     TUBE   rmin=tialIR 
endblock
*
*******************************************************************************
*
block TOFC  defines outer field cage - fill it with insulating gas already
      material  Nitrogen_gas
      Attribute TOFC   seen=1 colo=3
      SHAPE     TUBE   rmin=tofcIR  rmax=tofcOR  dz=tofcLeng/2
      create and position TOFS 
      create and position TOST
endblock
*------------------------------------------------------------------------------
*
*** Contents of Outer Field Cage Structure TOFS:  Copper/Kapton/Nomex/Adhesive
*   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block TOFS  is the Outer Field Cage structure 
* Actually only Copper strips will remain as material,
* the rest will be taken out by other layers. 
* This allows to avoid thin cylinder and to reduce number of volums
      material  COPPER
      Attribute TOFS      seen=1  colo=4
      SHAPE     TUBE      rmax=tofsOR  
*
      create and position TOKA
endblock
*
block TOKA  is  KAPTON layer
      material  mylar     " should be close to kapton "
      Attribute TOKA      seen=1  colo=3
      SHAPE     TUBE      rmin=tokaIR  
      create and position TONX
endblock
* 
block TONX  is  Nomex support
      material  Nomex     
      Attribute TONX      seen=1  colo=2
      SHAPE     TUBE      rmin=tonxIR  
      create and position TOAD
endblock
* 
block TOAD  is  Adhesive layer
      material  mylar     " should be close ? "
      Attribute TOAD      seen=1  colo=7
      SHAPE     TUBE      rmin=toadIR  
endblock
*
*------------------------------------------------------------------------------
*** Contents of Outer Field Cage Support TOST:  Aluminum/Honecomb
*   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block TOST is the Outer Field Cage Support
      material  Aluminium
      Attribute TOST      seen=1 colo=2
      SHAPE     TUBE      rmin=toalIR
      create and position TOHA
endblock
* 
Block TOHA  is  Honeycomb/Adhesive mixture
      Component Al        A=27  Z=13   W=0.0105
      Component N         A=14  Z=7    W=0.7395
      Component Adhesive  A=9   Z=4.5  W=0.2500
      mixture   Al_honeycomb    dens=0.282
      Attribute TOHA      seen=1 colo=7
      SHAPE     TUBE      rmin=tohaIR
endblock
*------------------------------------------------------------------------------

Block TPCM is the Central Membrane placed in TPC
      material  Mylar 
      Attribute TPCM  seen=1    Colo=4
*     Medium    dense_membrane  SteMax=1  " just to show that we define it "
      SHAPE     TUBE  rmin=tpgvIR  rmax=tpcg_SenGasOR  dz=tpcg_MembTHK/2
endblock
*
******************************************************************************
*                           section three - endcaps                          *
******************************************************************************
Block TPEA is one endcap placed in TPC 
      Material  p10
      attribute TPEA  seen=0
      SHAPE     TUBE  rmin=tpgvIR  rmax=tpcg_SenGasOR  dz=tpcg_TpeaThk/2
      create    TESS
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
block TESS is a division of endcap volume corresponding to one supersector
      attribute TESS  seen=1
      shape Division  NDIV=12  IAXIS=2

      do i_sec=1,2
         Use  TECW  sec=i_sec
*PN:     sectors are positioned by precision holes (drawing 24A7266)
         create and position TSEC                     X=tecw_Rcenter,
                Z=(tpcg_TpeaThk-tecw_AsDepth-tecw_PpDepth-tecw_GgDepth)/2,
                Ort=YZX
      enddo      

endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block TSEC is a supersector containing Al sector, PCB and MWC volume
*
      material  Aluminium
      SHAPE     TRD1 dx1=tecw_inwidth/2         dx2=tecw_ouwidth/2,
                     dy=(tecw_PpDepth+tecw_AsDepth+tecw_GgDepth)/2,
                     dz=tecw_Height/2

*    First, produce the MWC as a whole, all internal structure put inside
      create and position TMWC  y=-(tecw_PpDepth+tecw_AsDepth)/2
*
*    PCB is not created separetely, so it is now included in Al sector volume,
*    it can be created in this block to replace an additional 0.32 layer of Al 
*    like:  create and position TPCB  y=(tecw_GgDepth+tecw_PcbDepth)/2
*
      create TIAG                                       " air opennings "

*    trapezoids defining gaps have same angles as sector boundary
      dx_dz = (tecw_ouwidth - tecw_inwidth)/(2*tecw_Height )

*    boundary width is given parallel to sector edge, get it parallel to x-axis
      dxb = tecw_boundary / sqrt( 1. + dx_dz**2 )

      do i = 1,nint(tecw_n)
*        opening in pad back plane are at ouside and they contain extra pieces:
         position TIAG  Konly='MANY',  
                  z=-tecw_Height/2+tecw_z(i)  y=+tecw_ppdepth/2+tecw_GgDepth/2,
                  dx1=tecw_inwidth/2-dxb+(tecw_z(i)-tecw_dz(i)/2)*dx_dz, 
                  dx2=tecw_inwidth/2-dxb+(tecw_z(i)+tecw_dz(i)/2)*dx_dz, 
                  dy=tecw_asdepth/2,
                  dz=tecw_dz(i)/2
      enddo
*
      Create TOAE                                  " extra aluminum pieces "
      Do i = 1,nint(tecw_nex)
         Position TOAE x=tecw_Xex(i)      y=+tecw_ppdepth/2+tecw_GgDepth/2,
                       z=tecw_Zex(i)-tecw_Height/2,
                       dx=tecw_DXex(i)/2  dy=tecw_asdepth/2  dz=tecw_DZex(i)/2
      enddo
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block TMWC  is a wire chamber volume with both gated and sensitive volumes
* Wire grids are not created now. If needed, they should be included as thin
* planes at the leftmost position of TMWC and TMSE
      material  p10
      attribute TMWC seen=1   colo=3
      SHAPE     TRD1 dx1=tecw_inwidth/2       dx2=tecw_ouwidth/2,
                     dy=tecw_GgDepth/2,       dz=tecw_Height/2
      Check  tpcg_MWCread>0
      create and position TMEA  y=+(tecw_GgDepth-tecw_MWCdepth)/2,
                                z=tecw_MwcCent

endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block TMEA  is a double sensitive layer around gating grid
      material p10
      material sensitive_gas  ISVOL=1  stemax=5
      attribute TMEA seen=1   colo=4
      SHAPE     TRD1 dx1=tecw_MWCinn/2        dx2=tecw_MWCout/2,
                     dy=tecw_MWCdepth/2       dz=tecw_MWChei/2
      Create    TMSE
*     Call      GSTPAR(ag_imed,'STRA',1.)
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block TMSE  is a single sensitive volume

      SHAPE   division  Iaxis=3 Ndiv=tecw_MwcNwir
      if (TPCG_MWCread==1) then
*     until 10-03-01 AGI was not seting proper dimensions here, force to max:
      HITS    TMSE   Z:.01:S  Y:.01:   X:.01:(-tecw_MWCout/2,tecw_MWCout/2),
                     cx:10:   cy:10:   cz:10:,
                     Sleng:0.1:(0,800) Step:.01:,  
                     ToF:16:(0,1.e-6)  Ptot:16:(0,100), 
                     LGAM:16:(-4,6)    Elos:16:(0,0.01) 
      else
      HITS    TMSE   Z:.01:S  Y:.01:   X:.01:,
                     cx:10:   cy:10:   cz:10:,
                     Sleng:0.1:(0,800) Step:.01:,  
                     ToF:16:(0,1.e-6)  Ptot:16:(0,100), 
                     LGAM:16:(-4,6)    Elos:16:(0,0.01) 
      endif
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block TIAG is an air gap in inner sector aluminum structure
*     in reality they are filled with electronics PCB (PN!)
      material  Air
      SHAPE     TRD1  dx1=0  dx2=0  dy=0  dz=0
endblock
*
Block TOAE is extra aluminum supports in the air opennings
      material  Aluminium
      SHAPE     BOX   dx=0   dy=0   dz=0
endblock
*
*******************************************************************************
*                   section four - support wheels
*******************************************************************************
*
Block TPCW in the TPC supporting endcap Wheel
      material  Aluminium
      attribute TPEA seen=1
      shape     TUBE Rmin=tpcg_WheelIR  Rmax=tpcg_WheelOR  dz=tpcg_WheelTHK/2
      create    TWSS
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block TWSS is a division of wheel corresponding to supersectors 
      attribute TWSS   seen=0
      SHAPE division   NDIV=12   IAXIS=2
      do i_sec=1,2
         USE  TECW  sec=i_sec
         create and position TWGI   x=tecw_GapRad   ORT=YZX
      enddo
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
block TWGI is the Inner air gap in Wheel
      Material  Air
      attribute TWGI   seen=1 colo=7
      SHAPE     TRD1   dx1=tecw_GapWidI/2   dx2=tecw_GapWidO/2,
                       dy=tpcg_WheelTHK/2   dz=tecw_GapHeit/2
endblock
*
end


*******************************************************************************
                Subroutine   TPCELASER
*                                                                             *
* Description: generate correct energy deposition produced by the laser beam  *
*   Regarding the beam intensity and ionization, A. Lebedev wrote in his      *
*   note that the ionization density decreases along the beam according to    *
*   I(l) = I(0)*exp(-l/300.),  l in cm I(0) = 100e-/cm., dE of 1 e- is 26 eV  *
*   PN  21.08.97: anything denser than TPC gas absorbs laserino               *
*   PN  30.03.99: introducing dE dependence on beam intensity (Q measured)    *
*       Normalisation is done on a measured Q peak value for cosmic muons     *
*       Although the beam intensity is sqrt of Q, the ionization is I**2.     *
*******************************************************************************
      implicit none
+CDE,GCMATE,GCTMED,GCKINE,GCTRAK,AGCSTEP.
      character*8  cpart,laserino/'LASERINO'/
      equivalence (cpart,NaPart)
      Real Average
      REAL ANORM/4.2/       " MUON <Q> IN 87 MEASURMENTS "
      REAL PNORM/100./      " Npairs/cm for relativistic muons " 
      Integer Npair,ier
*                                    this happens only with primary laserino   
      Check cpart==laserino
      if (Dens>0.005) Istop=1  
      Check Isvol > 0 & Istak==0
*                                    number of produced pairs follows Poisson
      Average = Pnorm*aStep * GETOT/Anorm * exp(-Sleng/300.)
      Call POISSN(Average,Npair,ier)
      AdEstep  = 26.e-9*Npair
*
      end


*******************************************************************************
                Subroutine   T P A D S T E P (j,Hit)
* Description:  Step routine for TPCs, called at the end of the HITS operator *
*    A correction for the track curvature is introduced here. It assumes that
*    hits are (z,y,x)- packed and negative Z are in left-handed coordinates.
*     
*******************************************************************************
*
+CDE,Typing,GCBANK,GCVOLU,GCKINE,GCTRAK,AgCSTEP,GCFLAG.
*/AGCSTEP/: r*7 vect0,vloc0,vloc,xloc, r: Astep,Adestep
*
      Integer   J,MyPad,JMyPad,Ishape,JMyPar,i_flag,Jcenter
      Real      center(3)/0,0,0/,Field(3)/0,0,0/,Hit,Dr,Dt,Vect_middle(6),
                xi,yi,xo,yo,dx,dy,dphi,dtr,Vr,Pt,SIGN,smax/6.0/
      data      i_flag/0/,Jcenter/-1/,Vect_middle/6*0./
      Logical   Valid_hit,First/.true./
      Character Cname*4
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                Entry     TPAISTEP (J,Hit)
                Entry     TPAOSTEP (J,Hit)
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      If (First) then
          First = .false.
          Call GUFLD (center,field)
          if (IDEBUG>1) print *,' In TPADSTEP field=',field
      endif
* 
* Extract parameters of the current padraw from GEANT
*
*    This we need to get the actual padraw width from GEANT:
    JMYPAR  = LQ(JGPAR-NLEVEL)         ! pointer to the volume parameters
    dr      = Q(JMYPAR+1)              ! Radial half-size
    dt      = Q(JMYPAR+2)              ! transverse half-size
*
    If (Idebug>0) then
*    this is for control purpouse only - not really needed:
      MyPad   = LVOLUM(NLEVEL)           ! Padrow Volume Pointer (Number)
      JMYPad  = LQ(JVOLUM-MYPad)         ! pointer to the volume bank
      Ishape  = Q(JMyPad+2)              ! GEANT shape code of the PadRaw
      If (Ishape != 1) then
        Call UHTOC(IQ(JMyPad-4),4,CNAME,4) ! should  be TPAD,TPAI or TPAO
        print *,' Awful error in TPADSTEP, valid hit may not be valid !'
        print *,' volume ',cname,' at level ',NLEVEL,' shape=',Ishape,' dr=',dr
        print *,' we are in point ',vect0
      endif 
    endif
*
*    A hit is called Valid when trajectory cross a padrow (almost) completely:
      Valid_hit = abs(vloc0(1)-vloc(1)) .gt. 2*dr-0.1
*
      xi = vloc0(1);      yi = vloc0(2)
      xo = vloc(1);       yo = vloc(2)

      Pt = vloc0(7) * sqrt(vloc0(4)**2+vloc0(5)**2)
      Vr = 1.e9;  if (Pt>0) Vr = 0.0003*Field(3)/Pt

*     more precisely 2*asin(S/2R), but we use a limited linear approximation:
      dphi = min (aStep*Vr,1.0)
      dtr  = dphi/8*charge*SIGN(1.,VECT(3))

*     correction to   HITS TPAD  Z  Y  X  cx cy cz 
*     again precise coefficient is S/R*(1-cos(phi/2), take linear term only:
      dy = dtr*(xi-xo);  hits(2) = min(max(-dt,hits(2)+dy),dt)
      dx = dtr*(yo-yi);  hits(3) = min(max(-dr,hits(3)+dx),dr)

      hit = AdEStep
      end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
* Suppose we have a piece of trajectory from i to o (in xy plane only):
*       _
*      |\       o
*         n   ( / \
*            ( /    \
*            (/       \
*            i---------c
*
* - where c in center of the helix, ('s represent the helix arc. 
* To find its middle point (in direction of n) we introduce vectors:
*         i={xi,yi}
*         o={xo,yo)
*         s={xo-xi,yo-yi}
*         n={yo-yi,xi-xo}/L
* where
*         L=dist(i-o)=|S|
* Then
*         R=p/0.0003B    - R[cm], B[KGs], p[GeV/c]
*         phi=L/R        - linear approximation should be good enouph, 
*                          geant will keep this value < 20 degrees)
*         d=R*(1-cos phi)= ~R*phi^2/8  - displacement 
*         from to middle of the straight line to the middle of the arc
*             
*         m={(xi+xo)/2,(yi+yo)/2} + L/8R*{yo-yi,xi-xo}
*         ============================================
*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
