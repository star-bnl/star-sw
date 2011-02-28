* $Id: sisdgeo.g,v 1.5 2011/02/28 16:36:38 jwebb Exp $
* $Log: sisdgeo.g,v $
* Revision 1.5  2011/02/28 16:36:38  jwebb
* Cosmetic changes needed for AgML syntax matching.
*
* Revision 1.4  2005/01/03 22:09:57  potekhin
* Need to optionally position the Strip Detector in
* the CAVE, when the SVT is missing form the configuration
* (typical for the tracker upgrade)
*
* Revision 1.3  2004/10/28 22:02:18  potekhin
* Improved the diagnostic message
*
* Revision 1.2  2004/01/29 19:57:37  potekhin
* Positioned the laddre mother volume with the 'many'
* option to guard ourselves against the quite possible
* overlap.
* Also, deleted the operator that would create the full
* version every time
*
* Revision 1.1  2003/11/20 03:02:23  potekhin
* The first functional version of the rewritten
* Silicon Strip Geometry, separated from the
* previous code in the SVT, and capable of
* handling multiple configuration with
* programmatic versioning via the configuration
* flag.
*
* Some mother volumes are left visible on purpose
* as this is work in progress. Positions of
* ladders wrt rotation point needs to be checked.
*
* The volume numbering was tested and is presumed
* to be correct.
*
*
*******************************************************************************
*              originally part of the SVTTGEO code, but rewritten             *
*              and placed into a separate module for better maintainability   *
*******************************************************************************
Module  SISDGEO  is the Silicon Strip Detector
   Author  Maxim Potekhin
   created 17 Nov 03

+CDE,AGECOM,GCONST,GCUNIT.
* SSD Volumes
      Content   SFMO,SFLM,SFDM,SFSW,SFSD,SFSM,SFSS,SFCP,SFCW,SFCF,SFCT,SFCX
* SSD Parameters:
      Structure SSDP { Version,  Int Config, Int Placement}
      Structure SFPA { Version,  rmin,     rmax,     Len,
                       rad,      nssd,     dmWid,    dmThk,
		       dmLen,    smWid,    smThk,    smLen,
                       ssLen,    wpLen,    sdlen,    tilt,     
                       cprad,    cpral,    cfrad,    gpThk,
                       Int ladderMap(20),
                       ladderAngle(20), ladderTilt(20),LadderRadius(20)  }

*

      Integer   ilad,iwaf,nc
      Real      wafpckLen,dthk,radtilt,ang

      Fill SSDP               ! Silicon Strips
        Version  = 1          ! Version
        Config   = 1          ! There are a few configuraions possible
        Placement= 0          ! 0=cave, 1=svtt
      EndFill

      Fill SFPA               ! Silicon Strip detector parameters
        version  = 1          ! geometry version
        rmin     = 21.8       ! mother rmin
        rmax     = 29.5       ! mother rmax
        Len      = 100.       ! mother Len
        rad      = 23.        ! distance from beam axis to detector center
        nssd     = 16         ! number of silicon strip detectors 
        dmWid    = 7.5        ! detector mother width 
        dmThk    = 0.03       ! detector mother thickness
        dmLen    = 90.        ! detector mother length (detectors + adc board) 
        smWid    = 7.5        ! structure mother width
        smThk    = 3.5        ! structure mother thickness
        smLen    = 95.        ! structure mother length (cool. pipe+carbon fiber)
        ssLen    = 95./20.    ! length of a subvolume of the structure
        wpLen    = 68.8       ! length of wafer pack
        sdlen    = 4.2        ! lenght of one strip detector (along beam axis)
        tilt     = 5.0        ! tiling angle (degrees)
        cprad    = 0.1        ! cooling pipe outer radius
        cpral    = 0.09       ! cooling pipe inner radius
        cfrad    = 0.1        ! carbon fiber tube radius (support structure)
        gpThk    = 0.5        ! gap between structure mother and detector

        ladderMap   = {     0,     0,     1,     0,     0,
                            0,     0,     0,     0,     0,
                            0,     0,     0,     0,     0,
                            0,     0,     0,     0,     0} ! presence of ladders
        ladderAngle = {  -1.0,  -1.0,  45.0,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,  -1.0,  -1.0} ! individual angles
        ladderTilt  = {  -1.0,  -1.0,   0.0,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,  -1.0,  -1.0} ! individual tilts
        ladderRadius= {  -1.0,  -1.0,23.000,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,  -1.0,  -1.0} ! individual radii
*
      Fill SFPA               ! Silicon Strip detector parameters
        version  = 2          ! geometry version
        ladderMap   = {     1,     1,     1,     0,     0,
                            0,     0,     0,     1,     1,
                            1,     1,     1,     0,     0,
                            0,     0,     0,     1,     1} ! presence of ladders
        ladderAngle = {  90.0, 108.3, 130.0,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0, 230.0, 251.7,
                        270.0, 288.3, 310.0,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,  50.0,  71.7} ! individual angles
        ladderTilt  = {   0.0,  -6.0,   0.0,  -1.0,  -1.0,
                         -1.0,  -1.0,   0.0,   0.0,   6.0,
                          0.0,  -6.0,   0.0,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,   0.0,   6.0} ! individual tilts
        ladderRadius= {23.174,22.800,24.600,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,24.600,22.800,
                       23.174,22.800,24.600,  -1.0,  -1.0,
                         -1.0,  -1.0,  -1.0,24.600,22.800} ! individual radii
      EndFill
*
      Fill SFPA               ! Silicon Strip detector parameters
        version  = 3          ! geometry version
        ladderMap   = {     1,     1,     1,     1,     1,
                            1,     1,     1,     1,     1,
                            1,     1,     1,     1,     1,
                            1,     1,     1,     1,     1} ! presence of ladders
        ladderAngle = {  90.0, 108.3, 126.6, 144.4, 162.2,
                        180.0, 197.8, 215.6, 233.4, 251.7,
                        270.0, 288.3, 306.6, 324.4, 342.2,
                          0.0,  17.8,  35.6,  53.4,  71.7} ! individual angles
        ladderTilt  = {   0.0,  -6.0,  -7.0,  -7.0,  -7.0,
                          0.0,   7.0,   7.0,   7.0,   6.0,
                          0.0,  -6.0,  -7.0,  -7.0,  -7.0,
                          0.0,   7.0,   7.0,   7.0,   6.0} ! individual tilts
        ladderRadius= {23.177,22.800,22.800,22.800,23.800,
                       22.500,23.800,22.800,22.800,22.800,
                       23.177,22.800,22.800,22.800,23.800,
                       22.500,23.800,22.800,22.800,22.800} ! individual radii
      EndFill
*
*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

      USE SSDP
      USE SFPA version=SSDP_Config

      Component H2     A=1   Z=1   W=2
      Component O      A=16  Z=8   W=1
      Mixture   Water  Dens=1.0

      write(*,*) 'Level 0 of the SSD geometry'
      if(ssdp_Placement==1) then
         write(*,*) 'Positioining the Silicon Strip Detector in SVT'
         Create and Position SFMO in SVTT
      else
         write(*,*) 'Positioining the Silicon Strip Detector in CAVE'
         Create and Position SFMO in CAVE
      endif
*******************************************************************************
*
Block SFMO is the mother of all Silicon Strip Detector volumes
      Material   Air
      Attribute  SISD   Seen=1 Colo=1
      Shape TUBE Rmin=SFPA_rmin,
		 Rmax=SFPA_rmax,
		 dz=SFPA_Len/2

      dthk=SFPA_smThk+SFPA_gpThk

      Create SFLM " ladder mother"
      Do ilad = 1,20
        if(SFPA_ladderMap(ilad).gt.0) then ! only position ladders marked as present

           ang     = (SFPA_ladderAngle(ilad)*pi)/180.0
           radtilt = (SFPA_ladderTilt(ilad) *pi)/180.0
           if(ilad.eq.1) then
              nc=1
           else
              nc=20-ilad+2
           endif
           Position SFLM x= (SFPA_ladderRadius(ilad)*cos(ang) + (dthk*cos(ang+radtilt))/2.0),
                         y= (SFPA_ladderRadius(ilad)*sin(ang) + (dthk*sin(ang+radtilt))/2.0),
                         z=0, AlphaZ=SFPA_ladderAngle(ilad)-90.0+SFPA_ladderTilt(ilad) Ncopy=nc Konly='MANY'
        endif
      EndDo
Endblock 
*
*------------------------------------------------------------------------------
* 
Block SFLM is the mother of the ladder
* (dets,adc's and struct.)
      Material Air
      Attribute SFLM Seen=1 Colo=1
      Shape BOX dx=SFPA_dmWid/2,
		dy=(SFPA_dmThk+SFPA_gpThk+SFPA_smThk)/2,
		dz=SFPA_smLen/2 
      Create   SFDM " the detectors and adcs mother volume "
      Position SFDM y=-(SFPA_smThk+SFPA_gpThk)/2
		    

      Create   SFSM " the structure mother volume"
      Position SFSM y=(SFPA_dmThk+SFPA_gpThk)/2
                    
Endblock 
*
*------------------------------------------------------------------------------
* 
Block SFDM is the mother of the detectors 
      Material Air
      Attribute SFDM Seen=0 Colo=1
      Shape BOX         dx=SFPA_dmWid/2,
			dy=SFPA_dmThk/2,
			dz=SFPA_wpLen/2 
      
      wafpckLen=SFPA_wpLen/(SFPA_nssd*1.)
      Do iwaf=1,SFPA_nssd
        Create    SFSW " single wafer container"
        Position  SFSW z=-(SFPA_wpLen+wafpckLen)/2+iwaf*wafpckLen
     EndDo 
Endblock 
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
Block SFSW is a single wafer container
      Attribute SFSW Seen=0 Colo=1
      Shape BOX dx=SFPA_dmWid/2,
	        dy=SFPA_dmThk/2,
	        dz=wafpckLen/2
 
     Create and position  SFSD " strip detector"

Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
Block SFSD is the strip detector
      Material  Silicon  
      Material  Sensitive  Isvol=1       
      Attribute SFSD       seen=2  Colo=4
      Shape   BOX dx=SFPA_dmWid/2,
 		  dy=SFPA_dmThk/2,
		  dz=SFPA_sdlen/2
      call      GSTPAR (%Imed,'STRA',1.)

*     The following is the corrected hits definition: 25-apr-99 (PN)
      HITS    SFSD   X:.001:S   Y:.001:   Z:.001:   cx:10:   cy:10:   cz:10:,
                     Step:.01:   Sleng:16:(0,500)   ToF:16:(0,1.e-6),  
                     Ptot:16:(0,100)      Eloss:16:(0,0.001) 

Endblock
*
*------------------------------------------------------------------------------
* 
Block SFSM is the mother of the ladder struct. 
* (cool. pipe and carbon fiber)
      Material Air
      Attribute SFSM  Seen=0 Colo=1
      Shape BOX dx=SFPA_smWid/2,
		dy=SFPA_smThk/2,
		dz=SFPA_smLen/2 
      Create   SFSS " subvolume structure"
Endblock 
*
*------------------------------------------------------------------------------
* 
Block SFSS is the subvolume of the mother struct. 
* (cool. pipe and carbon fiber)
      Material Air
      Attribute SFSS Seen=0 Colo=1
      Shape   division     Iaxis=3  Ndiv=20

      Create   SFCP " cooling pipes"
      Position SFCP x=SFPA_smWid/2-5.*SFPA_cprad,
                    y=-SFPA_smThk/2+SFPA_cprad
      Position SFCP x=-SFPA_smWid/2+5.*SFPA_cprad,
                    y=-SFPA_smThk/2+SFPA_cprad

      Create   SFCF " carbon fiber"
      Position SFCF
Endblock 
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block SFCP is the cooling pipe 
      Material Carbon
      Attribute SFCP Seen=1 Colo=6
      Shape TUBE  rmin=0 rmax=SFPA_cprad dz=SFPA_ssLen/2
		 
      Create and Position SFCW " water cylinder"

Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block SFCW is the water cylinder in the cooling pipe
      Attribute SFCW Seen=1 Colo=6
      Material  Water
      Shape     TUBE rmax=SFPA_cpral
Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block SFCF is the carbon fiber structure container
      Material Air
      Attribute SFCF Seen=0  Colo=3
      Shape BOX  dx=SFPA_smThk*tan(pi/6.),
		 dy=SFPA_smThk/2.,
		 dz=SFPA_ssLen/2

      Create   SFCT "Carbon Tube"
      Position SFCT y= SFPA_smThk/2.-SFPA_cfrad
       
      Position SFCT x= SFPA_smThk*tan(pi/6.)-SFPA_cfrad, y=-SFPA_smThk/2.+SFPA_cfrad
      Position SFCT x=-SFPA_smThk*tan(pi/6.)+SFPA_cfrad, y=-SFPA_smThk/2.+SFPA_cfrad

      Create   SFCX "Carbon Tube (crossing)"
      Position SFCX y=-SFPA_smThk/2.+SFPA_cfrad, ort=yzx
      Position SFCX x= SFPA_smThk*tan(pi/6.)/2.-SFPA_cfrad/3., ort=yzx AlphaZ=-60
      Position SFCX x=-SFPA_smThk*tan(pi/6.)/2.+SFPA_cfrad/3., ort=yzx AlphaZ=+60

Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block SFCT is the carbon fiber tube
      Material Carbon
      Attribute SFCT Seen=1 Colo=6
      Shape TUBE rmin=0  rmax=SFPA_cfrad, dz=SFPA_ssLen/2
Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block  SFCX is the carbon fiber tube
      Material Carbon
      Attribute SFCX Seen=1 Colo=7
      Shape TUBE rmin=0  rmax=SFPA_cfrad, dz=SFPA_smThk*tan(pi/6.)-SFPA_cfrad
Endblock
*
*******************************************************************************
                                 End
