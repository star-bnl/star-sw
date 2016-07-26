
****************************************************************************
* $Id: tpcegeo2.g,v 1.2 2009/02/22 21:39:29 perev Exp $
* $Log: tpcegeo2.g,v $
* Revision 1.2  2009/02/22 21:39:29  perev
* max radius added TIKA,TIAL
*
* Revision 1.1  2005/12/08 00:40:18  potekhin
* The origianal Underwoof version contained a syntax error in the array
* initialization part. This has been corrected to the best of my intuition, as
* I haven't heard from D.Underwood. As of this writing, it's not in any
* geometry tag
*
* Revision 1.3  2005/07/14 22:06:53  potekhin
* An informative diagnostics about the TPC gas density added,
* prompted by a recent debugging session with Yuri
*
* Revision 1.2  2005/03/08 01:03:31  potekhin
* Added a CVS log tag, reformatted some data structures, and removed
* a few duplicate entries which were defined in the TPCEGEO source, which
* is still compiled and loaded witht he lib.
*
*
*
*
module   TPCEGEO2  is the updated TPC
   Author   David Underwood corrected by  Maxim Potekhin
   Created  Dec 7, 2005
*  Found incosistencies in the hx array initialization
*  Original Version - Peter Jacobs (LBL), March 16, 1995.                   *
*  Please see the file tpcegeo.g for the original comments for the previous *
*  version of the code, which has since been enriched with new content      *
*                                                                           *
*  DGU,HMS  15/02/05:  Aded FEE Cards, Holes for FEE cards, Water Pipes for *
*               FEE CArds, Water Manifolds, Cutouts and Blocks in Wheels,   *
*               and  RDO boards in addtiional +- 30 cm length of tpcegeo    *
*****************************************************************************
+cde,AGECOM,GCUNIT,GCONST.     " - standard geant commons "
Content   TPCE,TOFC,TOFS,TOST,TOKA,TONX,TOAD,TOHA,TPGV,TPSS,
          TIFC,TIAL,TIKA,TINX,TPCW,TWSS,TWGI,TPCM,TPEA,
          TESS,TSEC,TMWC,TMEA,TMSE,TIAG,TOAE,TPAD,TPAI,TPAO,
          THOL,THRA,THLA,TALC,TAEC,TCEX,TCRX,
          TWGC,TWGB,TPIP,TMAN,TRDV,TRDS,TRDC
*
Structure TPCG { version,rmin,rmax,length,WheelIR,WheelOR,WheelTHK,
                 SenGasOR,tpeaTHK,MembTHK,tiadDR,tinxDR,tikaDR,tialDR, 
                 tocsDR,tokaDR,tonxDR,toadDR,toigDR,toalDR,tohaDR,MWCread,
                 gasCorr }
*
Structure TECW { sec,GapRad,GapHeit,GapWidI,GapWidO,GapShft,inwidth,ouwidth,
                 height,ppdepth,asdepth,ggdepth,MWCdepth,boundary,
                 Rcenter,MWCinn,MWCout,MWChei,MwcCent,MwcNwir,n,nex,
                 z(8),dz(8),xex(5),zex(5),dxex(5),dzex(5),nhplane(8),
                 cardw,cardth,coolw,coolth,cardoff,cooloff,
                 slotw,slotrad, pipethk,pipeht, manithk,maniwid,
                 tan15, clearance,
                 Whlipthk,Whlipwid,Whblklen,Whblkpos,Whblkin} 

Structure TROV { sec,nhp(16),hx(9,16),offCrdIo} 
Structure TPRS { sec,Nrow,pitch,width,super,Rpads(40),Npads(40) }
Structure TRDO { RdoVthk,Rdothk,Rdolen,NRdobrd,Rdoht(9) }

Real      tocsIR,tocsOR,tokaIR,tokaOR,tonxIR,tonxOR,toadIR,toadOR,toigIR,toigOR,
          toalIR,toalOR,tohaIR,tohaOR,tofsIR,tofsOR,tofcIR,tofcOR,
          tiadIR,tiadOR,tinxIR,tinxOR,tikaIR,tikaOR,tialIR,tialOR,
          tifcIR,tifcOR,tpgvIR,tpgvLeng,tofcLeng,
          tpcwz,tpgvz,tpeaZ,tprdoz,dx_dz,dxb,del,a,b,
          radoff,zvalue
*
Real      pipeoff
Real      ztemp,xtemp,hpla,xbottom
Real      mntcylid,mntcylod,mntcylht,xlip
Real      T, P, PATM, RHO, density

Integer   i_row,i_sec,i,jj,kk, i_nhp, j_nho
External  TPADSTEP,TPAISTEP,TPAOSTEP,TPCELASER
*******************************************************************************
*

   Fill  TPCG             !  TPC basic dimensions
      version    = 2        ! current version
      rmin       = 46.107   ! TPC inner radius 
      rmax       = 207.750  ! TPC outer radius
      length     = 519.37   ! TPC full length
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
  EndFill
*
   Fill TRDO              ! volume for tpc readout boards
      RdoVthk   =30.      ! length of RDO vloume
      Rdothk    =.25      ! thickness of rdo card
      Rdolen    =27       ! card length along beam direction
      NRdobrd   = 9       ! number of RDO boards
      Rdoht = {60.0, 74.0, 84.0, 101.0,106.0,
               126.0,146.0,166.0,186.0} ! radial pos of rdo boards
  EndFill
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
  EndFill
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
   EndFill
*
   Fill TECW              ! EC trapezoid and support Wheel
      sec      = 1          ! sector number: 1 for inner, 2 for outer
      GapWidI  = 2.* 10.91  ! air in support wheel - inner width
      GapWidO  = 2.* 27.56  ! air in support wheel - outer width
      GapHeit  = 62.15      ! air in support wheel - height (dr)
      GapRad   = 87.0       ! air in support wheel - center radius
      GapShft  = 1.4        ! offset air gap to make lip
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
      nhplane = {1, 4, 4, 4, 2}  ! number of PC planes per air gap
      Whlipthk = 1.0               ! wheel bolt lip along beam
      Whlipwid = 2.8               ! wheel lip in phi
      Whblklen = 5.5               ! blocks on wheel radial size
      Whblkpos = 18.0              ! bolt blocks at +- 18 in whl
      Whblkin  = 0                 ! no block bottom inner wheel
      cardw = 7.33                 ! PC card width and thickness 
      cardth = 0.19                ! PC card width and thickness 
      coolw = 5.7                  ! cooling AL width and thickness
      coolth = 0.33                ! cooling AL width and thickness
      cardoff = 1.77               ! card and cooling offsets from
      cooloff = 1.50               ! card and cooling offsets from
      slotw = 4.70                 ! slot width and radius
      slotrad = 0.95               ! slot width and radius
      pipethk = 1.26               ! water pipe along beam dir
      pipeht = 1.9                 ! water pipe radial
      manithk = 4.5                ! water manifold along beam
      maniwid = 2.2                ! water manifold in phi
      tan15 = .26795               ! tangent of 15 deg
      clearance = 1.0              ! clearance for RDO boardsin phi
   EndFill
*
   Fill TECW              ! endcap trapezoid and support Wheel
      sec      = 2          ! sector number: 1 for inner, 2 for outer
      GapWidI  = 2.* 28.92  ! air in support wheel - inner width
      GapWidO  = 2.* 46.74  ! air in support wheel - outer width
      GapHeit  = 65.0       ! air in support wheel - height (dr)
      GapRad   = 158.0     ! air in support wheel - radius
      GapShft  = 0.0        ! offset of air gap lip
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
      nhplane = {2, 2, 2, 2, 2, 2, 2, 2}  ! number of PC planes per air gap
      Whlipthk = 1.0               ! wheel bolt lip along beam
      Whlipwid = 2.8               ! wheel lip in phi
      Whblklen = 5.5               ! blocks on wheel radial size
      Whblkpos = 18.0              ! bolt blocks at +- 18 in whl
      Whblkin  = 1                 ! is block bottom outer wheel
      cardw = 7.33                 ! PC card width and thickness 
      cardth = 0.19                ! PC card width and thickness 
      coolw = 5.7                  ! cooling AL width and thickness
      coolth = 0.33                ! cooling AL width and thickness
      cardoff = 1.77               ! card and cooling offsets from
      cooloff = 1.50               ! card and cooling offsets from
      slotw = 4.70                 ! slot width and radius
      slotrad = 0.95               ! slot width and radius
      pipethk = 1.26               ! water pipe along beam dir
      pipeht = 1.9                 ! water pipe radial
      manithk = 4.5                ! water manifold along beam
      maniwid = 2.2                ! water manifold in phi
      tan15 = .26795               ! tangent of 15 deg
      clearance = 1.0              ! clearance for RDO boardsin phi
    EndFill

*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
**
      Fill TROV   !  cards in row 1
        sec=1  !  inner
        nhp ={0,2,  2,3,3,3, 3,4,4,4, 4,5,2,5, 5,6} !no of holes/plane

        hx ={0.0,   0.0,    0.0,   0.0;    0.0,   0.0, 0.0, 0.0, 0.0,
            -5.17,  5.17,   0.0,   0.0,    0.0,   0.0, 0.0, 0.0, 0.0;
            -6.21,  6.21,   0.0,   0.0,    0.0,   0.0, 0.0, 0.0, 0.0;
            -7.79,  0.0,    7.79,  0.0,    0.0,   0.0, 0.0, 0.0, 0.0;
            -8.34,  0.0,    8.34,  0.0,    0.0,   0.0, 0.0, 0.0, 0.0;
            -9.38,  0.0,    9.38,  0.0,    0.0,   0.0, 0.0, 0.0, 0.0;
            -10.50, 0.0,   10.50,  0.0,    0.0,   0.0, 0.0, 0.0, 0.0;
            -11.97,-4.05,   4.05, 11.97,   0.0,   0.0, 0.0, 0.0, 0.0;
            -12.70,-4.32,   4.32, 12.70,   0.0,   0.0, 0.0, 0.0, 0.0;
            -13.70,-4.63,   4.63, 13.70,   0.0,   0.0, 0.0, 0.0, 0.0;
            -14.67,-4.98,   4.98, 14.67,   0.0,   0.0, 0.0, 0.0, 0.0;
            -15.86,-7.91,   0.0,   7.91,  15.86,  0.0, 0.0, 0.0, 0.0;
            -16.95,16.95,   0.0,   0.0,    0.0,   0.0, 0.0, 0.0, 0.0;
            -18.37,-8.99,   0.0,   8.99,  18.37,  0.0, 0.0, 0.0, 0.0;
            -19.11,-9.76,   0.0,   9.76,  19.11,  0.0, 0.0, 0.0, 0.0;
            -20.15,-12.08, -4.05,  4.05,  12.08, 20.15,0.0, 0.0, 0.0} ! x of holes
*

         offCrdIo = -0.9    ! cards, first row inner sector
*  input PC card locations and information
    EndFill
*
      Fill TROV   ! holes and cards in row 2
        sec = 2  !  outer
        nhp={ 6,7, 7,7, 7,7, 8,8,  8,8, 8,9, 9,9, 9,9}  ! number of holes in plane

        hx ={-23.70, -15.77, -7.72, 7.72, 15.77, 23.70, 0.0,   0.0,     0.0;
             -24.15, -16.25, -8.10, 0.00,  8.10, 16.25, 24.15, 0.0,     0.0;
             -25.48, -17.47 ,-8.71, 0.00,  8.71, 17.47, 25.48, 0.0,     0.0;
             -25.48, -17.47, -8.71, 0.00,  8.71, 17.47, 25.48, 0.0,     0.0;
             -28.19, -19.43,-10.81, 0.00 ,10.81, 19.43, 28.19, 0.0,     0.0;
             -28.19, -19.43,-10.81, 0.00, 10.81, 19.43, 28.19, 0.0,     0.0;
             -30.24, -21.51,-13.54,-5.37,  5.37, 13.54, 21.51, 30.24,   0.0;
             -30.24, -21.51,-13.54,-5.37,  5.37, 13.54, 21.51, 30.24,   0.0;
             -32.23, -24.17,-16.09,-5.42,  5.42, 16.09, 24.17, 32.23,   0.0;
             -32.23, -24.17,-16.09,-5.42,  5.42, 16.09, 24.17, 32.23,   0.0;
             -34.30, -25.86,-16.14,-8.10,  8.10, 16.14, 25.86, 34.30,   0.0;
             -34.30, -25.86,-16.14,-8.10,  0.00,  8.10, 16.14, 25.86,  34.30;
             -36.16, -28.12,-16.14,-8.10,  0.00,  8.10, 16.14, 28.12,  36.16;
             -36.16, -28.12,-16.14,-8.10,  0.00,  8.10, 16.14, 28.12,  36.16;
             -38.37, -29.47,-20.76,-10.61, 0.00, 10.61, 20.76, 29.47,  38.37;
             -38.37, -29.47,-20.76,-10.61, 0.00, 10.61, 20.76, 29.47,  38.37} ! x of holes
*
         offCrdIo = 0.0  ! card no special case
    EndFill
*
*

      Use  TPCG  


*
*  TPC basic dimensions are the full system size, the gas volume length
*  and inner radius are derived from them and from material thicknesses.
*  The outer gas radius is also an input ( as this is used for winding ),
*  but the remaining clearance is checked to be positive (PN, 16 Mar 96).
*
      tofcLENG = tpcg_Length-2*tpcg_WheelTHK-2*trdo_RdoVthk  ! gas plus endcaps
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
*      Whblkwid=tecw_Whlipwid
*      Whblkthk=WheelTHK-tecw_Whlipthk
*     block position dx=lipwid/2 *sqrt(1+slope^2), and between cardrows

      data Mntcylht /30./               ! tpc mounting bracket ht
      data Mntcylid /16./               ! tpc mounting brkt id
      data Mntcylod /20./               ! tpc mounting brkt od
*     mounting bracket is in ??? volume, not in wheel
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
      write(*,*) '*** Building the Underwood version of the TPC ***'
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
      tpcwz  = (tpcg_LENGth-2*trdo_RdoVthk-tpcg_WheelTHK)/2    " support wheels " 
      Create and position  TPCW  z=+tpcwz,
                                thetax=90 thetay=90 thetaz=0,
                                phix = 75 phiy =-15 phiz=0
                 position  TPCW  z=-tpcwz,
                                thetax=90 thetay=90 thetaz=180,
                                phix =105 phiy =195 phiz=0
      tprdoz  = (tpcg_LENGth-trdo_RdoVthk)/2    " rdo card volume " 
      Create and position  TRDV  z=+tprdoz,
                                thetax=90 thetay=90 thetaz=0,
                                phix = 75 phiy =-15 phiz=0
                 position  TRDV  z=-tprdoz,
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
      Call     GSTPAR(ag_imed,'STRA',1.)
*
*     The following is the corrected hits definition: 25-dec-98 (PN)
      HITS    TPAD   Z:.0005:S  Y:.0005:  X:.0005:   cx:10: cy:10: cz:10:,
                     LPtot:18:(-3,2)      Sleng:.1:(0,800),
                     ToF:16:(0,1.e-6)     LGAM:16:(-2,6),    
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
      SHAPE     TUBE   rmin=tikaIR rmax=tikaOR
      Create and position TIAL
endblock
*
Block TIAL is the inner Aluminum cylinder
      material  Aluminium
      Attribute TIAL   seen=1  colo=7
      SHAPE     TUBE   rmin=tialIR rmax=tialOR
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
         Use  TROV  sec=i_sec
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
*
      create and position TMWC  y=-(tecw_PpDepth+tecw_AsDepth)/2
*
*    PCB is not created separately, so it is now included in Al sector volume,
*    it can be created in this block to replace an additional 0.32 layer of Al 
*    like:  create and position TPCB  y=(tecw_GgDepth+tecw_PcbDepth)/2
*
      create TIAG                                   " air opennings "
*    trapezoids defining gaps have same angles as sector boundary
      dx_dz = (tecw_ouwidth - tecw_inwidth)/(2*tecw_Height )
*    boundary width is given parallel to sector edge, get it parallel to x-axis
*      dxb = tecw_boundary / sqrt( 1. + dx_dz**2 )
      dxb = tecw_boundary * sqrt( 1. + dx_dz**2 )

      do i = 1,nint(tecw_n)
*       opening in pad back plane are at outside and they contain extra pieces:
         position TIAG  Konly='MANY',  
                  z=-tecw_Height/2+tecw_z(i)  y=+tecw_ppdepth/2+tecw_GgDepth/2,
                  dx1=tecw_inwidth/2-dxb+(tecw_z(i)-tecw_dz(i)/2)*dx_dz, 
                  dx2=tecw_inwidth/2-dxb+(tecw_z(i)+tecw_dz(i)/2)*dx_dz, 
                  dy=tecw_asdepth/2,
                  dz=tecw_dz(i)/2
      enddo
*        the code below adds G10, cooling, and holes for the PC cards

     Create TCEX
     Create TAEC
     Create THOL
     Create THRA
     Create THLA

      Do i_nhp=1,16
       hpla = -tecw_Height/2+tecw_z(1)-tecw_dz(1)/2+trov_offCrdIo !first pl
       zvalue = Hpla+4.0*(i_nhp-1)

       if(trov_Nhp(i_nhp)>0) then
        Do j_nho=1,trov_Nhp(i_nhp)
             position TCEX   x=trov_Hx(j_nho,i_nhp),
                  y=+tecw_PpDepth/2+tecw_GgDepth/2   z=zvalue+tecw_cardoff
             position TAEC    x=trov_Hx(j_nho,i_nhp),
                  y=+tecw_PpDepth/2+tecw_GgDepth/2   z=zvalue+tecw_cooloff
              position THOL  x=trov_Hx(j_nho,i_nhp),
                  y=+tecw_GgDepth/2-tecw_AsDepth/2   z=zvalue+tecw_cardoff
             position THRA    x=trov_Hx(j_nho,i_nhp)+tecw_slotw/2,
                  y=+tecw_GgDepth/2-tecw_AsDepth/2,
                  z=zvalue+tecw_cardoff   Ort=ZXY
             position THLA  x=trov_Hx(j_nho,i_nhp)-tecw_slotw/2,
                  y=+tecw_GgDepth/2-tecw_AsDepth/2,
                  z=zvalue+tecw_cardoff   Ort=ZXY
        enddo
        endif 
      enddo

*
*
      Create TOAE                                  " extra aluminum pieces "
      Do i = 1,nint(tecw_nex)
         Position TOAE x=tecw_Xex(i)      y=+tecw_ppdepth/2+tecw_GgDepth/2,
                       z=tecw_Zex(i)-tecw_Height/2,
                       dx=tecw_DXex(i)/2  dy=tecw_asdepth/2  dz=tecw_DZex(i)/2
      enddo
*

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
      Call      GSTPAR(ag_imed,'STRA',1.)
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
                     LGAM:16:(-2,6)    Elos:16:(0,0.01) 
      else
      HITS    TMSE   Z:.01:S  Y:.01:   X:.01:,
                     cx:10:   cy:10:   cz:10:,
                     Sleng:0.1:(0,800) Step:.01:,  
                     ToF:16:(0,1.e-6)  Ptot:16:(0,100), 
                     LGAM:16:(-2,6)    Elos:16:(0,0.01) 
      endif
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block THOL   is part of a hole for PC boards
      material  Air
      SHAPE     BOX   dx=tecw_slotw/2   dy=tecw_PpDepth/2   dz=tecw_slotrad
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block THRA   is part of a hole for PC boards
      material  Air
      SHAPE     TUBS  rmin=0.0   rmax=tecw_slotrad   dz=tecw_PpDepth/2,
                      phi1=0.0   phi2=180.0
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block THLA   is part of a hole for PC boards
      material  Air
      SHAPE     TUBS  rmin=0.0   rmax=tecw_slotrad   dz=tecw_PpDepth/2,
                      phi1=180.0   phi2=360.0
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

Block TIAG is an air gap in inner sector aluminum structure
*     in reality they are filled with electronics PCB (PN!)
      material  Air
      SHAPE     TRD1  dx1=0  dx2=0  dy=0  dz=0
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block TOAE is extra aluminum supports in the air opennings
      material  Aluminium
      SHAPE     BOX   dx=0   dy=0   dz=0
endblock
*
Block TCEX   is part of the G10 for the PC boards
*   G10 is given as 60% SiO2 and 40% epoxy in ftpcgeo.g, from which 
*   the following is taken
      Component  Si  A=28.08  Z=14  W=0.6*1*28./60.
      Component  O   A=16     Z=8   W=0.6*2*16./60.
      Component  C   A=12     Z=6   W=0.4*8*12./174.
      Component  H   A=1      Z=1   W=0.4*14*1./174.
      Component  O   A=16     Z=8   W=0.4*4*16./174.
      Mixture G10  Dens=1.7
      material  G10
      SHAPE     BOX  dx=tecw_cardw/2   dy=tecw_AsDepth/2   dz=tecw_cardth/2
endblock
* 
Block TAEC   is part of the heat shield for the PC boards
      material  Aluminium
      SHAPE     BOX  dx=tecw_coolw/2   dy=tecw_AsDepth/2   dz=tecw_coolth/2
endblock
* 
*******************************************************************************
*                   section four - support wheels
*******************************************************************************
*
Block TPCW is the TPC supporting endcap Wheel
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
         Use  TROV  sec=i_sec
         create and position TWGI   x=tecw_GapRad   ORT=YZX
      enddo
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
block TWGI is the Inner air gap in Wheel
      Material  Air
      attribute TWGI   seen=1
      SHAPE     TRD1   dx1=tecw_GapWidI/2   dx2=tecw_GapWidO/2,
                       dy=tpcg_WheelTHK/2   dz=tecw_GapHeit/2
     
*   next do cutouts to make lip on wheel
      dx_dz=(tecw_GapWidO/2-tecw_GapWidI/2)/tecw_GapHeit
      xlip=tecw_Whlipwid*sqrt(1+dx_dz**2)
*
    xbottom =tecw_GapWidI/2+xlip-dx_dz*(tecw_Whlipwid-tecw_GapShft)
     create and position TWGC dx1=xbottom,
         dx2=tecw_GapWidO/2+xlip+dx_dz*tecw_Whlipwid,
         dy=(tpcg_WheelTHK-tecw_Whlipthk)/2,
         dz=tecw_GapHeit/2+tecw_Whlipwid-tecw_GapShft,
         y=tecw_Whlipthk/2  z=tecw_GapShft
*  next  put in the bolt blocks above wheel lips
         create and position TWGB x=0 y=tecw_Whlipthk/2,
            z=(tecw_GapHeit/2+tecw_Whlipwid/2)  Alphay=90
        if(tecw_Whblkin==1) then
         position TWGB x=0 y=tecw_Whlipthk/2,
            z=-(tecw_GapHeit/2+tecw_Whlipwid/2)  Alphay=90
         endif
*      
       position TWGB x=tecw_GapWidI/2+xlip/2+dx_dz*(tecw_Whblkpos+tecw_GapHeit/2),
            y=tecw_Whlipthk/2,
            z=tecw_Whblkpos Alphay=15 
       position TWGB x=tecw_GapWidI/2+xlip/2+dx_dz*(-tecw_Whblkpos+tecw_GapHeit/2),
            y=tecw_Whlipthk/2,
            z=-tecw_Whblkpos Alphay=15 
*
       position TWGB x=-tecw_GapWidI/2-xlip/2-dx_dz*(tecw_Whblkpos+tecw_GapHeit/2),
            y=tecw_Whlipthk/2,
            z=tecw_Whblkpos Alphay=-15 
       position TWGB x=-tecw_GapWidI/2-xlip/2-dx_dz*(-tecw_Whblkpos+tecw_GapHeit/2),
            y=tecw_Whlipthk/2,
            z=-tecw_Whblkpos Alphay=-15 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
*          this is the addition of the G10, cooling, and PC cards, which
*          extend into the wheel region.
*
      radoff = tecw_Rcenter-tecw_GapRad-tecw_Height/2+tecw_GapHeit/2
*          radoff hopefully corrects for differences between the endcap 
*          and wheel radial offsets
*  next put in water pipes
      Do i_nhp=1,16
       if(trov_Nhp(i_nhp)>0) then
 hpla = -tecw_GapHeit/2+radoff+tecw_z(1)-tecw_dz(1)/2+trov_offCrdIo !first pl
          zvalue = Hpla+4.0*(i_nhp-1)
          pipeoff=tecw_pipeht/2+tecw_coolth+tecw_cardth/2      
          xtemp=tecw_GapWidI/2
          ztemp=zvalue+tecw_cardoff-pipeoff
          Create and Position TPIP dx1=xtemp+dx_dz*(ztemp+tecw_GapHeit/2-tecw_pipeht/2),
          dx2=xtemp+dx_dz*(ztemp+tecw_GapHeit/2+tecw_pipeht/2),
          dy=tecw_pipethk/2   dz=tecw_pipeht/2,
          y=-tpcg_WheelTHK/2+tecw_pipethk/2,
          z=ztemp
        endif 
      enddo
*
     create and position TMAN  dx = tecw_maniwid/2  dy=tecw_manithk/2,
                    dz=(1.+dx_dz**2)*(tecw_GapHeit/2-tecw_maniwid),
         x=tecw_GapWidI/2.+dx_dz*(tecw_GapHeit/2)-(1+dx_dz**2)*tecw_maniwid/2,
            y=-tpcg_WheelTHK/2+tecw_pipethk+tecw_manithk/2,
            z=0.0  Alphay=15 
   position TMAN  x=-tecw_GapWidI/2.-dx_dz*(tecw_GapHeit/2)+(1+dx_dz**2)*tecw_maniwid/2,
            y=-tpcg_WheelTHK/2+tecw_pipethk+tecw_manithk/2,
            z=0.0  Alphay=-15 
*
* next put in cards
     Create TCRX
     Create TALC
      Do i_nhp=1,16
       if(trov_Nhp(i_nhp)>0) then
 hpla = -tecw_GapHeit/2+radoff+tecw_z(1)-tecw_dz(1)/2+trov_offCrdIo !first pl
          zvalue = Hpla+4.0*(i_nhp-1)
        Do j_nho=1,trov_Nhp(i_nhp)
             position TCRX   x=trov_Hx(j_nho,i_nhp),
                    z=zvalue+tecw_cardoff
             position TALC    x=trov_Hx(j_nho,i_nhp),
                    z=zvalue+tecw_cooloff
        enddo
        endif 
      enddo
*
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
block TWGC is the larger Inner air gap in Wheel
      Material  Air
      attribute TWGC   seen=1 colo=7
      SHAPE     TRD1   dx1=0   dx2=0 dy=0 dz=0
endblock
block TWGB is added alum blocksin Wheel
      Material  Aluminium
      attribute TWGB   seen=1
      SHAPE     BOX   dx=tecw_Whlipwid/2,
                      dy=(tpcg_WheelTHK-tecw_Whlipthk)/2,
                      dz=tecw_Whblklen/2
endblock
block TMAN is aluminum water manifold
      Material  Aluminium
      attribute TWGB   seen=1
      SHAPE     BOX   dx=0  dy=0  dz=0
endblock
*
Block TPIP is a water pipe there are lots per sect
      Material Aluminium
      attribute TPIP  seen=1 colo=4
      SHAPE    TRD1  dx1=0  dx2=0  dy=0  dz=0
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Block TCRX   is part of the G10 for the PC boards
      Material  G10
      attribute TCRX   seen=1 colo=4
      SHAPE     BOX   dx=tecw_cardw/2   dy=tpcg_WheelThk/2   dz=tecw_cardth/2
endblock
*
Block TALC   is part of the heat shield for the PC boards
      Material  Aluminium
      attribute TALC   seen=1 colo=6
      SHAPE     BOX   dx=tecw_coolw/2   dy=tpcg_WheelThk/2   dz=tecw_coolth/2
endblock
* 
*******************************************************************************
*                   section five- readout boards
*******************************************************************************
*

*
Block TRDV is the RDO board volume 
      material  Air
      attribute TRDV    seen=1 colo=2
      shape     TUBE Rmin=tpcg_WheelIR  Rmax=tpcg_WheelOR  dz=trdo_RdoVthk/2
      create    TRDS
endblock


* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
block TRDS is a division of rdo board volume corresponding to one supersector
      attribute TRDS  seen=1
      shape Division  NDIV=12  IAXIS=2
      USE TRDO 
      do kk=1,trdo_NRdobrd
         create and position TRDC dx=(trdo_Rdoht(kk)*tecw_tan15)-tecw_clearance,
               dy=trdo_Rdolen/2  dz=trdo_Rdothk/2,
           X=trdo_Rdoht(kk)  Ort=YZX  
      enddo
endblock
Block  TRDC is an RDO Card
*   G10 is given as 60% SiO2 and 40% epoxy in ftpcgeo.g, from which 
*   the following is taken
      Component  Si  A=28.08  Z=14  W=0.6*1*28./60.
      Component  O   A=16     Z=8   W=0.6*2*16./60.
      Component  C   A=12     Z=6   W=0.4*8*12./174.
      Component  H   A=1      Z=1   W=0.4*14*1./174.
      Component  O   A=16     Z=8   W=0.4*4*16./174.
      Mixture G10  Dens=1.7
      material  G10
      SHAPE     BOX  dx=0  dy=0   dz=0

endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
