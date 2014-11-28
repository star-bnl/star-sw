* $Id: mutdgeo2.g,v 1.1 2007/09/21 20:29:14 perev Exp $
* $Log: mutdgeo2.g,v $
* Revision 1.1  2007/09/21 20:29:14  perev
* lijuan updates
*
* Revision 1.3  2006/03/24 22:54:24  potekhin
* After a discussion with Hank, the TOF slats seem to be
* unnecessary in this simulation, hence I simplify the geo.
*
* Revision 1.1  2006/03/21 23:49:01  potekhin
* We MUST add this geom to our system in order to account
* for an existing prototype of the Muon trigger system and
* accomodate its possible expansion into a full blown detector.
* First version in need of final touches.
*
*
*
********************************************************************************
Module  MUTDGEO2 is the geometry of the STAR muon trigger system
********************************************************************************
+CDE,AGECOM,GCUNIT.
   Author    Maxim Potekhin
   Created   21 March 2006

   Content   MUTD,MUSC,MPMT,MMRP

*MMAA,MMTD,MASS,MCSB

   Structure MTDG { version, Rpmtin, Rpmtout, Rmrpcin, Rmrpcout, Rmin, Rmax, dz, Length, Radii(2) }


   Integer Ntray, iwid;
   Integer is;
   Real BarPhi, xpos, ypos, zpos, sublen, subcen, totlen;
********************************************************************************
*
   Fill MTDG                ! Muon system basic dimensions
      version  = 1          ! version number
*      Rmin     = 390.00     ! inner radius of the magnet system

* added by Lijuan 
      Rpmtin     = 364.25     ! pmt box inner radius
      Rpmtout     = 386.15     ! pmt box outer radius      
      Rmrpcin     = 403.60     ! mrpc box inner radius
      Rmrpcout     = 411.22     ! mrpc box outer radius      
*added by Lijuan end

      Rmin     = 390.00     ! inner radius of the magnet system
      Rmax     = 435.00     ! outer radius of the magnet system
      dz       = 246.0      ! CTB/TOF tube half length
      Length   = 500.00     ! slightly longer than full length of the trays
      Radii    = {390.093, 420.093}          ! radii of trays
   Endfill


*
*
       Use    MTDG

* RPCgas material for TOFr gas
      Component H    A=1      Z=1    W=0.90*2*1./102.  + 0. + 0.05*10*1./58.
      Component C    A=12     Z=6    W=0.90*2*12./102. + 0. + 0.05*4*12./58.
      Component F    A=19     Z=9    W=0.90*4*19./102. + 0.05*6*19./146. + 0.
      Component S    A=32     Z=16   W=0.              + 0.05*1*32./146. + 0.
      Mixture   RPCgas  Dens=4.55E-3

       create and position MUTD in Cave

* --------------------------------------------------------------------------
Block MUTD is the muon detector mother
       material  Air
       medium    Standard
       Attribute MAGP     seen=0  colo=1

       Shape     TUBE  Rmin=MTDG_Rpmtin+1  Rmax=MTDG_Rmax  dz=MTDG_Length/2

* the number of slabs is already hardocoded in magp, so why bother:
       BarPhi = 360.0/30.0;

* added by Lijuan
    do is=1,60
        Create MUSC
        Position MUSC AlphaZ=BarPhi/2.0+BarPhi*is
    enddo
* added by Lijuan end




EndBlock
* ---------------------------------------------------------------------------

* ---------------------------------------------------------------------------
Block MUSC is a sector of MUON Trigger Barrel Scintillators
      Attribute MUSC      seen=0   colo=1

      Shape     Tubs      phi1 = -5.0 phi2 = 5.0

      Create and Position MPMT X=3.12/2+1.5+MTDG_Rpmtin Y=0 Z=0
      Create and Position MMRP X=2.50/2.+MTDG_Rpmtout Y=0 Z=0

      

EndBlock


*added by Lijuan
*------------------------------------------------------------------------------
Block MPMT  is a Main TRay covering box for PMT
      Attribute  MPMT     seen=1   colo=3
      Material   Aluminium
      Shape      BOX     dx=3.12/2.,
                         dy=57.20/2.,
                         dz=MTDG_length/2  
EndBlock
*------------------------------------------------------------------------------
Block MMRP  is a Main TRay covering box for MRPC
      Attribute  MMRP     seen=1   colo=3
      Material   RPCgas
      Medium    sensitive IsVol=1
      Shape      BOX      dx=2.50/2.,
                          dy=57.20/2.,
                          dz=MTDG_length/2  
      HITS    MMRP   X:.01:HS   Y:.01:   Z:.01:,
                     Ptot:18:(0,100),
                     Sleng:.1:(0,500)   ToF:16:(0,1.e-7)  Step:.01:,
                     Eloss:16:(0,1.e-6)
EndBlock
*------------------------------------------------------------------------------
*added by Lijuan added


End
