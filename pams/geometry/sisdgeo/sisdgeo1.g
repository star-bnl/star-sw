* $Id: sisdgeo1.g,v 1.7 2011/02/28 16:36:38 jwebb Exp $
* $Log: sisdgeo1.g,v $
* Revision 1.7  2011/02/28 16:36:38  jwebb
* Cosmetic changes needed for AgML syntax matching.
*
* Revision 1.6  2008/11/19 04:08:28  perev
*  updates to the corrected(vp) starsim
*
* Revision 1.5  2005/01/03 22:09:57  potekhin
* Need to optionally position the Strip Detector in
* the CAVE, when the SVT is missing form the configuration
* (typical for the tracker upgrade)
*
* Revision 1.4  2004/10/28 22:01:32  potekhin
* Corrected a typo and improved the diagnostic message
*
* Revision 1.3  2004/10/26 21:15:28  potekhin
* Changed the print statement to look nicer,
* and noted that the radius of the mother volume has been
* changed to conform with the shield of the SVT
*
* Revision 1.2  2004/10/26 21:13:22  potekhin
* Started improving the structure of the code by replacing
* hardcoded numbers with variables which encapsulate formulas.
*
* However, much work is needed here from the SSD group to rectify th rest
* of the code, and add much needed documentation
*
* Revision 1.1  2004/04/23 18:32:21  potekhin
* A significantly enhanced new version by the SSD group,
* including the previously missing material and structures.
* Note that the shield radius in the SVTGEO3 should be
* changed as would be required by the new mother vol
* radius defined here. Deferred.
*

************* NOTE: NEW UPDATED VERSION APR 13 2004 ***************************
*************  Inherited from the original sisdgeo: ***************************
* Revision 1.2  2004/01/29 19:57:37  potekhin
* Positioned the ladder mother volume with the 'many'
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
*
*******************************************************************************
*              originally part of the SVTTGEO code, but rewritten             *
*              and placed into a separate module for better maintainability   *
*                                                                             *
*              Major changes (additions od structures, materials etc) Apr'04  *
*******************************************************************************
Module  SISDGEO1  is the Silicon Strip Detector
   Author  Lilian Martin
   created 23 Apr 04

+CDE,AGECOM,GCONST,GCUNIT.
* SSD Volumes
      Content   SFMO,SFLM,SFDM,SFSW,SFSD,SFSM,SFLT,SFLU,
		SFFK,SFFL,SFKK,SFKL,
                SFRA,SFRS,SFFX,SFPI,SFPJ,SFAA,
                SFAM,SFAB,SFAS,SAPP,SAPC,SAPS,SAPT,
		SFLA,SFLB,SFLC,SFES,SFEB,
                SFCO,SFCM,SFCB,SFCS,SFKF,SFKS,SSBS,SSBB,
	        SFPR,SFPB,SSST,SSSS,SSRS,SSRT,SSLB,SSLT,
		SCMP,SCVM,SCVB,SCVS
* SSD Parameters:
      Structure SSDP { Version,  Int Config, Int Placement }
      structure SFPB { Hhight,   Khight,   Hbase,   Kbase, Fsize,
		       Zcoor}
      Structure SFPA { Version,  rmin,     rmax,     Len,
                       rad,      nssd,     dmWid,    dmThk,
		       dmLen,    smWid,    smThk,    smLen,
                       ssLen,    wpLen,    sdlen,    tilt,     
                       cprad,    cpral,    cfrad,    gpThk,
                       Int ladderMap(20),
                       ladderAngle(20), ladderTilt(20),LadderRadius(20)  }

*

      Integer ilad,iwaf,jwaf,nc
      Real    wafpckLen,dthk,radtilt,ang
      Real    essai,hight,lng,leng,yoffset
      Real    zS1, zS2, zS3, zS4, zS5, yS1, yS2, yS3, yS4

      Fill SSDP               ! Silicon Strips
        Version  = 1          ! Version
        Config   = 1          ! There are a few configuraions possible
        Placement= 0          ! 0=cave, 1=svtt
      EndFill

      Fill SFPB ! Some SSD Shell dimensions
        Hhight   = (0.02/tan(54*pi/180)+0.02/(2*tan(pi/5))+0.2)*sin(pi/5) ! haut 1
        Khight   = 0.02/sin(54*pi/180) ! haut 2
        Hbase    = (0.02/tan(63*pi/180)+0.01/tan(27*pi/180)+0.2)*sin(27*pi/180) ! bas 1
        Kbase    = 0.02/sin(63*pi/180) ! bas 2
        Fsize    = 0.6/cos(15*pi/180)+0.02*tan(15*pi/180) ! lenght of the side kapton film
        Zcoor    = sqrt((4.4-2.6/tan(56.89*pi/180))**2+(2.6/cos(54*pi/180))**2) ! z coord 
      EndFill

* Original number for the SFPA_rmax was 31.8 (not sure why)
* I changed this to fit into the SVT shielding --maxim--

      Fill SFPA               ! Silicon Strip detector parameters
        version  = 1          ! geometry version
        rmin     = 21.8       ! mother rmin
        rmax     = 29.5       ! mother rmax
        Len      = 120.       ! mother Len along the z direction
        rad      = 23.        ! distance from beam axis to detector center
        nssd     = 16         ! number of silicon strip detectors 
        dmWid    = 7.8        ! detector mother width 
        dmThk    = 2.0        ! detector mother thickness
        dmLen    = 90.        ! detector mother length (detectors + adc board) 
        smWid    = 7.5        ! structure mother width
        smThk    = 5.0        ! structure mother thickness
        smLen    = 101.9      ! structure mother length 
        ssLen    = 95./20.    ! length of a subvolume of the structure
        wpLen    = 68.8       ! length of wafer pack
        sdlen    = 4.2        ! length of one strip detector (along beam axis)
        tilt     = 5.0        ! tiling angle (degrees)
        cprad    = 0.1        ! cooling pipe outer radius
        cpral    = 0.09       ! cooling pipe inner radius
        cfrad    = 0.1        ! carbon fiber tube radius (support structure)
        gpThk    =-1.0        ! gap between structure mother and detector

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

*     G10 is about 60% SiO2 and 40% epoxy (stolen from ftpcgeo.g)
        Component Si  A=28.08  Z=14   W=0.6*1*28./60.
        Component O   A=16     Z=8    W=0.6*2*16./60.
        Component C   A=12     Z=6    W=0.4*8*12./174.
        Component H   A=1      Z=1    W=0.4*14*1./174.
        Component O   A=16     Z=8    W=0.4*4*16./174.
        Mixture   G10   Dens=1.7

*     G5 is G10 with half its density
        Component Si  A=28.08  Z=14   W=0.6*1*28./60.
        Component O   A=16     Z=8    W=0.6*2*16./60.
        Component C   A=12     Z=6    W=0.4*8*12./174.
        Component H   A=1      Z=1    W=0.4*14*1./174.
        Component O   A=16     Z=8    W=0.4*4*16./174.
        Mixture   G5   Dens=0.85

      write(*,*) 'Level 1 of the SSD geometry'
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
      Attribute  SFMO   Seen=1 Colo=6
      Shape TUBE Rmin=SFPA_rmin,
		 Rmax=SFPA_rmax,
		 dz=SFPA_Len/2

      dthk=SFPA_smThk+SFPA_gpThk

*** Putting the ladders in place
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


** Define some prepackaged constants.This still needs work from the SSD group!
** I have no idea what 49.8 means --maxim--

        zS1=49.8+0.95+2.5/2.+0.2
        zS2=49.8+0.95+0.5/2.+0.2
        zS3=49.8+0.95+0.5+0.2+2.5/2
        zS4=49.8+0.95+0.5+0.2+2.5+5.05/2.
        zS5=57.5

        yS1=19.1
        yS2=18.1
        yS3=15.945
        yS4=17.0

***********  Putting the mechanical structure in place *************
** Top parts of the small sectors
     	Create SSST
     	Position SSST x=0.,   y=0.,   z=-zS1
      	Position SSST x=0.,   y=0.,   z= zS1
      	Position SSST x=0.,   y=0.,   z=-zS1, AlphaZ=180.
      	Position SSST x=0.,   y=0.,   z= zS1, AlphaZ=180.
** Side parts of the small sectors
      	Create SSSS
      	Position SSSS x=0.,   y=0.,   z=-zS2
      	Position SSSS x=0.,   y=0.,   z= zS2
      	Position SSSS x=0.,   y=0.,   z=-zS2, AlphaZ=180.
      	Position SSSS x=0.,   y=0.,   z= zS2, AlphaZ=180.

** Top parts of the ribs
	Create SSRT
	Position SSRT x=0.,   y=0.,   z=-zS1
	Position SSRT x=0.,   y=0.,   z= zS1
      	Position SSRT x=0.,   y=0.,   z=-zS1, AlphaZ=180.
      	Position SSRT x=0.,   y=0.,   z= zS1, AlphaZ=180.

** Side parts of the ribs
	Create SSRS
      	Position SSRS x=0.,   y=0.,   z=-zS2
      	Position SSRS x=0.,   y=0.,   z= zS2
      	Position SSRS x=0.,   y=0.,   z=-zS2, AlphaZ=180.
      	Position SSRS x=0.,   y=0.,   z= zS2, AlphaZ=180.

** SSD to cone linking box
	Create SSLB
	Position SSLB x= yS1, y= yS1, z=-zS3, AlphaZ= 45.
	Position SSLB x=-yS1, y=-yS1, z=-zS3, AlphaZ= 45.
	Position SSLB x= yS1, y=-yS1, z=-zS3, AlphaZ=-45.
	Position SSLB x=-yS1, y= yS1, z=-zS3, AlphaZ=-45.
	Position SSLB x= yS1, y= yS1, z= zS3, AlphaZ= 45.
	Position SSLB x=-yS1, y=-yS1, z= zS3, AlphaZ= 45.
	Position SSLB x= yS1, y=-yS1, z= zS3, AlphaZ=-45.
	Position SSLB x=-yS1, y= yS1, z= zS3, AlphaZ=-45.

** SSD to cone linking tube
	Create SSLT
	Position SSLT x= yS2, y= yS2, z=-zS4
	Position SSLT x=-yS2, y=-yS2, z=-zS4
	Position SSLT x= yS2, y=-yS2, z=-zS4
	Position SSLT x=-yS2, y= yS2, z=-zS4
	Position SSLT x= yS2, y= yS2, z= zS4
	Position SSLT x=-yS2, y=-yS2, z= zS4
	Position SSLT x= yS2, y=-yS2, z= zS4
	Position SSLT x=-yS2, y= yS2, z= zS4

** SSD mounting plate inserted in the cone
	Create SCMP
	Position SCMP x= yS3, y= yS3, z= zS5, AlphaZ=-45.
	Position SCMP x=-yS3, y=-yS3, z= zS5, AlphaZ=-45.
	Position SCMP x= yS3, y=-yS3, z= zS5, AlphaZ= 45.
	Position SCMP x=-yS3, y= yS3, z= zS5, AlphaZ= 45.
	Position SCMP x= yS3, y= yS3, z=-zS5, AlphaZ=-45.
	Position SCMP x=-yS3, y=-yS3, z=-zS5, AlphaZ=-45.
	Position SCMP x= yS3, y=-yS3, z=-zS5, AlphaZ= 45.
	Position SCMP x=-yS3, y= yS3, z=-zS5, AlphaZ= 45.

** SSD V-shape mouting piece
	Create SCVM
	Position SCVM x= yS4, y= yS4, z= zS5, AlphaZ=-45.
	Position SCVM x=-yS4, y=-yS4, z= zS5, AlphaZ= 135.
	Position SCVM x= yS4, y=-yS4, z= zS5, AlphaZ=-135.
	Position SCVM x=-yS4, y= yS4, z= zS5, AlphaZ= 45.
	Position SCVM x= yS4, y= yS4, z=-zS5, AlphaZ=-45.
	Position SCVM x=-yS4, y=-yS4, z=-zS5, AlphaZ= 135.
	Position SCVM x= yS4, y=-yS4, z=-zS5, AlphaZ=-135.
	Position SCVM x=-yS4, y= yS4, z=-zS5, AlphaZ= 45.
Endblock 
*
*------------------------------------------------------------------------------
* 
Block SFLM is the mother of the ladder
* (dets,adc's and struct.)
      Material Air
      Attribute SFLM Seen=0 Colo=6
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
      Attribute SFDM Seen=0 Colo=6
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
      Material Air
      Attribute SFSW Seen=0 Colo=6
	Shape BOX dx=SFPA_dmWid/2,
		  dy=1.,
		  dz=2.25

Create and Position SFSD " strip detector" 
         
Create SFRA " hybrid stiffneer" 
Position SFRA x=0., y=0.099, z=1.1  
Position SFRA x=0., y=0.099, z=-1.1  
 
Create SFRS "two parts of the hybrid stiffneer (support)" 
Position SFRS x=3.8, y=0.0265, z=1.1 
Position SFRS x=3.8, y=0.0265, z=-1.1 
Position SFRS x=-3.8, y=0.0265, z=1.1 
Position SFRS x=-3.8, y=0.0265, z=-1.1 

 
Create SFFX "flex on the hybrid stiffneer" 
Position SFFX x=0., y=0.099+0.0190+0.0007, z=1.1 
Position SFFX x=0., y=0.099+0.0190+0.0007, z=-1.1 

Create SFPI "four pions" 
Position SFPI x=3.2, y=0.099+0.019+0.35-0.0250, z=0.1+0.6, 
              AlphaX=90 
Position SFPI x=-3.2, y=0.099+0.019+0.35-0.0250, z=0.7, 
              AlphaX=90 
Position SFPI x=3.2, y=0.099+0.019+0.35-0.0250, z=-0.7, 
              AlphaX=90 
Position SFPI x=-3.2, y=0.099+0.019+0.35-0.0250, z=-0.7, 
              AlphaX=90 

Create SFAA "A128C chips"
Position SFAA x=-0.325-0.3, y=0.099+0.0190+0.0007*2+0.0150, z= 1.1+1.0-0.02-0.4 
Position SFAA x=-0.325-0.3-0.65-0.6, y=0.099+0.0190+0.0007*2+0.0150, z= 1.1+1.0-0.02-0.4 
Position SFAA x=-0.325-0.3-0.65-0.6-0.65-0.6, y=0.099+0.0190+0.0007*2+0.0150, z= 1.1+1.0-0.02-0.4 
Position SFAA x=+0.325+0.3, y=0.099+0.0190+0.0007*2+0.0150, z= 1.1+1.0-0.02-0.4  
Position SFAA x=+0.325+0.3+0.65+0.6, y=0.099+0.0190+0.0007*2+0.0150, z= 1.1+1.0-0.02-0.4 
Position SFAA x=+0.325+0.3+0.65+0.6+0.65+0.6, y=0.099+0.0190+0.0007*2+0.0150, z= 1.1+1.0-0.02-0.4  
Position SFAA x=-0.325-0.3, y=0.099+0.0190+0.0007*2+0.0150, z= -1.1-1.0+0.02+0.4 
Position SFAA x=-0.325-0.3-0.65-0.6, y=0.099+0.0190+0.0007*2+0.0150, z= -1.1-1.0+0.02+0.4 
Position SFAA x=-0.325-0.3-0.65-0.6-0.65-0.6, y=0.099+0.0190+0.0007*2+0.0150, z= -1.1-1.0+0.02+0.4 
Position SFAA x=+0.325+0.3, y=0.099+0.0190+0.0007*2+0.0150, z= -1.1-1.0+0.02+0.4  
Position SFAA x=+0.325+0.3+0.65+0.6, y=0.099+0.0190+0.0007*2+0.0150, z= -1.1-1.0+0.02+0.4 
Position SFAA x=+0.325+0.3+0.65+0.6+0.65+0.6, y=0.099+0.0190+0.0007*2+0.0150, z= -1.1-1.0+0.02+0.4  

EndBlock

*------------------------------------------------------------------------

Block SFRA is the hybrid stiffneer
      Material Carbon
      Attribute SFRA Seen=2 Colo=1
*      Shape BOX dx=1., dy=1., dz=1.
      Shape BOX dx=3.76,
                dy=0.019,
                dz=1.
Endblock

*-----------------------------------------------------------------------

Block SFRS two supports of the hybrid stiffneer (piece of it)
	Material Carbon
	Attribute SFRS Seen=2 Colo=1
	Shape BOX dx=0.04,
		  dy=0.0915,
		  dz=1.
Endblock

*-----------------------------------------------------------------------
Block SFFX is the flex
* It describe the flex+coverlay+strips+vias = 0.093 % X0 normalised to the wafer surface
* equal to 0.095 %X0 nomalized to the flex surface (76.8 mm x 20 mm).
* tranformed into a 14 microns thick copper plate. 
* use aluminised mylar mixture instead of kapton
* 
	Material Copper
	Attribute SFFX Seen=1, Colo=42
	Shape BOX dx=3.84,
		  dy=0.0007,
		  dz=1.
*		  dy=(0.065-0.038)/2,
Endblock
*-----------------------------------------------------------------------
Block SFAA is the A128C chip

	Material Silicon
	Attribute SFAA Seen=1, Colo=41
	Shape BOX dx=0.3,
		  dy=0.015,
		  dz=0.4
Endblock

*=====================================================================

Block SFPI are the pions
	Material Aluminium
	Attribute SFPI seen=2 Colo=33
	Shape TUBE rmin=0.15/2, rmax=0.2/2, dz=0.35	

Create SFPJ "pions bases"
Position SFPJ x=0., z=-0.35+0.0125, y=0

Endblock

*----------------------------------------------------------------------

Block SFPJ is the base of the pions
	Material Aluminium
	Attribute SFPJ seen=2, Colo=33
	Shape TUBE rmin=0.075+0.025, rmax=0.3, dz=0.0125
Endblock

*======================================================================

Block SFSD is the strip detector
      Material  Silicon  
      Material  Sensitive  Isvol=1 
      
      Attribute SFSD       seen=2  Colo=41
      Shape   BOX dx=3.75,
                  dy=0.015,
                  dz=2.1
      call      GSTPAR (%Imed,'STRA',1.)

*     The following is the corrected hits definition: 25-apr-99 (PN)
      HITS    SFSD   X:.001:S   Y:.001:   Z:.001:   cx:10:   cy:10:   cz:10:,
                     Step:.01:   Sleng:16:(0,500)   ToF:16:(0,1.e-6),  
                     Ptot:16:(0,100)      Eloss:16:(0,0.001) 
Endblock
*
*------------------------------------------------------------------------------
* 
Block SFSM is the mother volume of the ladder (mechanic structure)
	Material Air
	Attribute SFSM Seen=0 Colo=6
	Shape BOX dx=SFPA_dmWid/2, dy=SFPA_smThk/2, dz=SFPA_smLen/2.

        yoffset=-1.7

Create SFLT "ladder skeleton : top corner of the triangle"

	hight = 2.6*tan(54*pi/180)-SFPB_Hhight/tan(pi/5)-0.02

Position SFLT x=-(SFPB_Hhight-SFPB_Khight),
	      y=hight+yoffset, z=0, 
	      AlphaZ=-90-36  

Position SFLT x=(SFPB_Hhight-SFPB_Khight),
	      y=hight+yoffset, z=0,
	      AlphaY=180., AlphaZ=90.+36.

Create SFLU "ladder skeleton : side corner of the triangle" 

Position SFLU x=0.-(2.6-(0.2+0.01/tan(pi/5))), y=0.+yoffset, z=0.
	essai=180
	essai=essai+27+27

Position SFLU x=-cos(63*pi/180)*2*(SFPB_Hbase-SFPB_Kbase)-(2.6-(0.2+0.01/tan(pi/5))),
              y=cos(27*pi/180)*2*(SFPB_Hbase-SFPB_Kbase)+yoffset, 
              z=0, 
              Alphay=180, Alphaz=essai 

Position SFLU x=0.+(2.6-(0.2+0.01/tan(pi/5))), y=0.+yoffset, z=0., Alphay=180.

Position SFLU x=cos(63*pi/180)*2*(SFPB_Hbase-SFPB_Kbase)+(2.6-(0.2+0.01/tan(pi/5))),
	      y=cos(27*pi/180)*2*(SFPB_Hbase-SFPB_Kbase)+yoffset,
	      z=0,
	      Alphaz=-essai



Create SFFK "carbon base under the ladder"
Position SFFK x=3.5/2+0.4, y=-0.02-0.04-0.02+yoffset, z=0
Position SFFK x=-3.5/2-0.4, y=-0.02-0.04-0.02+yoffset, z=0

Create SFFL "tilted carbon base under the ladder"
Position SFFL x=-2.55-SFPB_Fsize*cos(15*pi/180),
	      y=-0.02-0.04-0.02-SFPB_Fsize*cos(75*pi/180)+yoffset, z=0,
	      AlphaZ=15
Position SFFL x=2.55+SFPB_Fsize*cos(15*pi/180), 
  	      y=-0.02-0.04-0.02-SFPB_Fsize*cos(75*pi/180)+yoffset, z=0,
              AlphaZ=-15

Create SFKK "kapton film under the ladder"
Position SFKK x=0., y=-0.02-0.04-0.04+yoffset-0.0025/2., z=0.
Create SFKL "tilted kapton film under the ladder"
Position SFKL x=-2.55-SFPB_Fsize*cos(15*pi/180)+0.005,
	      y=-0.02-0.04-0.02-SFPB_Fsize*cos(75*pi/180)+yoffset-0.021, z=0,
	      AlphaZ=15
Position SFKL x=+2.55+SFPB_Fsize*cos(15*pi/180)-0.005,
	      y=-0.02-0.04-0.02-SFPB_Fsize*cos(75*pi/180)+yoffset-0.021, z=0,
	      AlphaZ=-15


Create SAPP "Mother volume of the adc board appendice" 
Position SAPP x=0., y= -0.04-(0.5+0.08)/2.+yoffset, z=69.75/2+1.275/2., AlphaY=180.
Position SAPP x=0., y= -0.04-(0.5+0.08)/2.+yoffset, z=-(69.75/2+1.275/2.)

Create SFAM "Adc mother volume"
Position SFAM x=0., y=-0.04-(0.5+0.08)/2.+yoffset, z=-(69.75/2+1.275)-(12.4)/2.
Position SFAM x=0., y=-0.04-(0.5+0.08)/2.+yoffset, z=+(69.75/2+1.275)+(12.4)/2., AlphaY=180.

Create SFCO "Connection board 1"
Position SFCO x=-1.7, y=0.2, z=-(69.75/2-0.6)-15.8/2., AlphaZ=+54.
Position SFCO x=+1.7, y=0.2, z=+(69.75/2-0.6)+15.8/2., AlphaZ=-54.

Create SFCM "Connection board 2"
Position SFCM x=+1.7, y=0.2, z=-(69.75/2-0.6)-15.8/2., AlphaZ=-54.
Position SFCM x=-1.7, y=0.2, z=+(69.75/2-0.6)+15.8/2., AlphaY=180, AlphaZ=+54.

Create SFKF "Kapton flex 1"
Position SFKF  x=-0.16, y=2.07, z=-(69.75/2-0.6)-8.1/2-0.2, AlphaZ=+54.
Position SFKF  x=+0.16, y=2.07, z=+(69.75/2-0.6)+8.1/2+0.2, AlphaZ=-54.

Create SFKS "Kapton flex 2"
Position SFKS x=+0.505, y=1.585, z=-(69.75/2-0.6)-8.1/2-0.2, AlphaZ=-54.
Position SFKS x=-0.505, y=1.585, z=+(69.75/2-0.6)+8.1/2+0.2, AlphaZ=+54.

Create SFPR "End mechanical part 1"
Position SFPR x=0, y=+1.79, z=-49.8
Position SFPR x=0, y=+1.79, z=+49.8-3.7

Create SFPB "End mechanical part 2"

*** The y calculation below is strange (good value by chance ??)
Position SFPB x=0, y=+1.79-3.48+0.08, z=-49.8-0.95/2.
Position SFPB x=0, y=+1.79-3.48+0.08, z=+49.8+0.95/2.

Create SSBS "Aluminum plate 1"
Position SSBS x=0., y=+1.79-3.48+0.08-0.8/2.-0.5/2., z=-49.8-0.95-0.2+1.9+2.5/2.
Position SSBS x=0., y=+1.79-3.48+0.08-0.8/2.-0.5/2., z=+49.8+0.95+0.2-1.9-2.5/2.

Create SSBB "Aluminum plate 2"
Position SSBB x=0., y=+1.79-3.48+0.08-0.8/2.-0.5/2.,  z=-49.8-0.95-0.2+1.9/2.
Position SSBB x=0., y=+1.79-3.48+0.08-0.8/2.-0.5/2.,  z=+49.8+0.95+0.2-1.9/2.

Create SFLA "long bus"
Create SFLB "part of the bus"
Create SFLC "end of the long bus"
Create SFEB "big elbow bus"
Create SFES "small elbow bus"
wafpckLen=SFPA_wpLen/(SFPA_nssd*1.)
Do iwaf=1,8
	Do jwaf=1,iwaf
*** Top row the start
		Position SFLA x=+1.1+0.02*jwaf, y=1.35+0.02*jwaf, z=-(SFPA_wpLen+wafpckLen)/2+iwaf*wafpckLen, AlphaZ=-54
		Position SFLA x=-1.1-0.02*jwaf, y=1.35+0.02*jwaf, z=+(SFPA_wpLen+wafpckLen)/2-iwaf*wafpckLen, AlphaZ=+54

*** Top row the end on the connector
		Position SFLC x=+1.1+0.02*jwaf, y=1.35+0.02*jwaf, 
				z=-(SFPA_wpLen+wafpckLen)/2+16*wafpckLen+wafpckLen/2.+7.5+8.5*0.93-iwaf*0.93,
				AlphaZ=-54
		Position SFLC x=-1.1-0.02*jwaf, y=1.35+0.02*jwaf, 
				z=+(SFPA_wpLen+wafpckLen)/2-16*wafpckLen-wafpckLen/2.-7.5-8.5*0.93+iwaf*0.93,
				AlphaZ=+54

*** Bottom row the start
		Position SFLA x=+2.3+0.02*jwaf, y=-0.3+0.02*jwaf, z=-(SFPA_wpLen+wafpckLen)/2+(iwaf+8)*wafpckLen, AlphaZ=-54
		Position SFLA x=-2.3-0.02*jwaf, y=-0.3+0.02*jwaf, z=+(SFPA_wpLen+wafpckLen)/2-(iwaf+8)*wafpckLen, AlphaZ=+54

*** Bottom row the end on the connector
		Position SFLC x=+2.3+0.02*jwaf, y=-0.3+0.02*jwaf, 
				z=-(SFPA_wpLen+wafpckLen)/2+16*wafpckLen+wafpckLen/2.+8.5*0.93-iwaf*0.93,
				AlphaZ=-54
		Position SFLC x=-2.3-0.02*jwaf, y=-0.3+0.02*jwaf, 
				z=+(SFPA_wpLen+wafpckLen)/2-16*wafpckLen-wafpckLen/2.-8.5*0.93+iwaf*0.93,
				AlphaZ=+54

	Enddo

*** Top row : Common part 8 Times on the active area*
	Do jwaf=1,8
		Position SFLA x=+1.1+0.02*jwaf, y=1.35+0.02*jwaf, 
		z=-(SFPA_wpLen+wafpckLen)/2+(iwaf+8)*wafpckLen, AlphaZ=-54
		Position SFLA x=-1.1-0.02*jwaf, y=1.35+0.02*jwaf, 
		z=+(SFPA_wpLen+wafpckLen)/2-(iwaf+8)*wafpckLen, AlphaZ=+54
	Enddo
** Top row : Common part of the long bus on the connection baord

	Position SFLB x=+1.1+0.02*iwaf, y=1.35+0.02*iwaf, 
		      z=-(SFPA_wpLen+wafpckLen)/2+16*wafpckLen+wafpckLen/2.+7.5/2., AlphaZ=-54
	Position SFLB x=-1.1-0.02*iwaf, y=1.35+0.02*iwaf, 
		      z=+(SFPA_wpLen+wafpckLen)/2-16*wafpckLen-wafpckLen/2.-7.5/2., AlphaZ=+54

** Small elbow bus
	Position SFES x=-3.32, y=-1.6,
		      z=-(SFPA_wpLen+wafpckLen)/2+(iwaf)*wafpckLen+wafpckLen/2.-1.5/2.-0.1/2., AlphaZ=57.66
	Position SFES x=+3.32, y=-1.6,
		      z=+(SFPA_wpLen+wafpckLen)/2-(iwaf)*wafpckLen-wafpckLen/2.+1.5/2.+0.1/2., AlphaZ=-57.66

** Big elbow bus
	Position SFEB x=-2.71, y=-0.75,
		      z=-(SFPA_wpLen+wafpckLen)/2+(iwaf+8)*wafpckLen+wafpckLen/2.-1.5/2.-0.1/2., AlphaZ=55.35
	Position SFEB x=+2.71, y=-0.75,
		      z=+(SFPA_wpLen+wafpckLen)/2-(iwaf+8)*wafpckLen-wafpckLen/2.+1.5/2.+0.1/2., AlphaZ=-55.35

EndDo 

Endblock

*--------------------------------------------------------------------------------------------

Block SFLT is (half) the top corner of the triangular ladder skeleton
	Material Carbon
	Attribute SFLT Seen=1, Colo=1 
	Shape TRAP Dz=49.8, thet=0, phi=0,         h1=0.02, bl1=0.2, tl1=0.2-0.02/tan(pi/5),
	 alp1=(pi/2-atan(2*tan(pi/5)))*360/(2*pi), h2=0.02, bl2=0.2, tl2=0.2-0.02/tan(pi/5),
	 alp2=(pi/2-atan(2*tan(pi/5)))*360/(2*pi)
Endblock 

*-------------------------------------------------------------------------------------------

Block SFLU is (half) the side corner of the triangular ladder skeleton
        Material Carbon 
        Attribute SFLU Seen=1, Colo=1   
        Shape TRAP dz=49.8, thet=0, phi=0, h1=0.02, bl1=0.2, tl1=0.2-0.02/tan(27*2*pi/360),
	 	   alp1=(pi/2-atan(2*tan(27*2*pi/360)))*360/(2*pi), h2=0.02, bl2=0.2,
		   tl2=0.2-0.02/tan(27*2*pi/360), 
		   alp2=(pi/2-atan(2*tan(27*2*pi/360)))*360/(2*pi)   
Endblock  

*======================================================================================

Block SFFK horizontal part of the ladder skeleton carbon base
	Material Carbon
	Attribute SFFK Seen=1, Colo=1
	Shape BOX dx=0.4, dy=0.02, dz=69.75/2
Endblock

*--------------------------------------------------------------------------------------

Block SFFL titled part of the ladder skeleton carbon base
        Material Carbon
        Attribute SFFL Seen=1, Colo=1 
        Shape BOX dx=0.6/cos(15*pi/180), dy=0.02, dz=69.75/2 
Endblock 

*======================================================================================

Block SFKK horizontal part of the kapton film under the ladder base
	Material Mylar
	Attribute SFKK Seen=1, Colo=1
	Shape BOX dx=5.1/2., dy=0.0025/2., dz=69.75/2
Endblock

*--------------------------------------------------------------------------------------

Block SFKL titled part of the kpaton film under the ladder base
        Material Mylar
        Attribute SFKL Seen=1, Colo=1 
        Shape BOX dx=0.6/cos(15*pi/180), dy=0.0025/2., dz=69.75/2 
Endblock 

*======================================================================================

Block SFLA is the long part of the bus cable linking the modules to the connection board 
        Material Mylar 
        Attribute SFLA Seen=1, Colo=1 
        Shape BOX dx=1.5/2., dy=0.0130/2., dz=4.3/2. 
Endblock 

Block SFLB is the part of the long bus cable on the connection board 
        Material Mylar 
        Attribute SFLB Seen=1, Colo=1 
        Shape BOX dx=1.5/2., dy=0.0130/2., dz=7.5/2. 
Endblock 

Block SFLC is the part of the long bus cable on the connection board up to the connector
        Material Mylar 
        Attribute SFLC Seen=1, Colo=1 
        Shape BOX dx=1.5/2., dy=0.0130/2., dz=0.93/2. 
Endblock 

Block SFEB is the big bus elbow 
        Material Mylar 
        Attribute SFEB Seen=1, Colo=1 
        Shape BOX dx=3.89/2., dy=0.0130/2., dz=1.5/2. 
Endblock 

Block SFES is the small bus elbow 
        Material Mylar 
        Attribute SFES Seen=1, Colo=1 
        Shape BOX dx=1.875/2., dy=0.0130/2., dz=1.5/2. 
Endblock 


*======================================================================================
Block SFAM is the mother volume of the adc board
	Material Air
	Attribute SFAM Seen=0, Colo=6
	Shape BOX dx=7.2/2., dy=0.3/2., dz=12.4/2
	Create SFAB
	Position SFAB x= 0., y=0., z=12.4/2-10.25/2
	Create SFAS
	Position SFAS x= 7.2/2.-2.1/2., y=0., z=-12.4/2+2.15/2
	Position SFAS x= -7.2/2.+2.1/2., y=0., z=-12.4/2+2.15/2
Endblock

Block SFAB is the big volume of the adc board
	Material G10
	Attribute SFAB Seen=1, Colo=30
	Shape BOX dx=7.2/2., dy=0.3/2., dz=10.25/2
Endblock
Block SFAS is the small volume of the adc board
	Material G10
	Attribute SFAS Seen=1, Colo=30
	Shape BOX dx=2.1/2., dy=0.3/2., dz=2.15/2
Endblock
*--------------------------------------------------------------------------------------
*
Block SAPP is the mother volume of the adc board appendice
 	Material Air
	Attribute SAPP Seen=0, Colo=6
	Shape BOX dx=(7.6+0.08)/2., dy=(0.5+0.08)/2., dz=1.275/2.
	
	Create SAPC 
	Position SAPC x=0., y=0., z=(-1.275+0.5)/2.
	Create SAPS
	Position SAPS x=7.6/2.+0.04/2, y=0, z=0.
	Position SAPS x=-(7.6/2.+0.04/2), y=0, z=0.
	Create SAPT
	Position SAPT x=0., y=(0.5+0.04)/2., z=0.
	Position SAPT x=0., y=-(0.5+0.04)/2., z=0.
Endblock

Block SAPC is the core (Epoxy) of the adc board appendice
 	Material Carbon
	Attribute SAPC Seen=1, Colo=1
	Shape BOX dx=7.6/2., dy=0.5/2., dz=0.5/2.
Endblock

Block SAPS is the side shell (Carbon) of the adc board appendice
 	Material Carbon
	Attribute SAPS Seen=1, Colo=1
	Shape BOX dx=0.04/2., dy=(0.5+0.08)/2., dz=1.275/2.
Endblock

Block SAPT is the top-bottom shell (Carbon) of the adc board appendice
 	Material Carbon
	Attribute SAPT Seen=1, Colo=1
	Shape BOX dx=7.6/2., dy=0.04/2., dz=1.275/2.
Endblock

*======================================================================================

Block SFCO is the connection board (rectangular with Hirose connectors)
	Material G10
	Attribute SFCO Seen=1, Colo=30
	Shape BOX dx=4.4/2., dy=0.37/2., dz=15.8/2
Endblock

Block SFCM is the mother volume of the second connection board 
	Material Air
	Attribute SFCM Seen=0, Colo=6
	Shape BOX dx=4.0/2., dy=0.37/2., dz=15.8/2

        Create SFCB 
	Position SFCB x=+0.5, y=0, z=0
        Create SFCS 
	Position SFCS x=-1.5, y=0, z=-15.8/2+7.5/2
Endblock

Block SFCB is the big part of the second connection board 
	Material G10
	Attribute SFCB Seen=1, Colo=30
	Shape BOX dx=3.0/2., dy=0.37/2., dz=15.8/2
Endblock

Block SFCS is the big part of the second connection board 
	Material G10
	Attribute SFCS Seen=1, Colo=30
	Shape BOX dx=1.0/2., dy=0.37/2., dz=7.5/2
Endblock

Block SFKF is the first part of the kapton flex circuit
	Material G10
	Attribute SFKF Seen=1, Colo=30
	Shape BOX dx=0.44/2., dy=0.065/2., dz=8.1/2
Endblock

Block SFKS is the second part of the kapton flex circuit
	Material G10
	Attribute SFKS Seen=1, Colo=30
	Shape BOX dx=1.65/2., dy=0.065/2., dz=8.1/2
Endblock

Block SFPR is the ladder end inside mechanical part (prism with g10 with half density)
	Material G5
	Attribute SFPR Seen=1, Colo=4
	SHAPE PGON phi1=-126, dphi=72, NPDV=1, Nz=2, Zi={0,3.7},
               Rmn={0,0},      Rmx={3.48,3.48}
Endblock

Block SFPB is the ladder end outside mechanical part (rectangle with g10)
	Material G10
	Attribute SFPB Seen=1, Colo=4
	SHAPE BOX dx=4.3/2., dy=0.8/2., dz=0.95/2.

Endblock

Block SSBS is the small part of the aluminium plate linking the ladder to the sector
	Material Aluminium
	Attribute SSBS Seen=1, Colo=1
	SHAPE BOX dx=2.5/2., dy=0.5/2., dz = 2.5/2.
EndBlock

Block SSBB is the Big part of the aluminium plate linking the ladder to the sector
	Material Aluminium
	Attribute SSBB Seen=1, Colo=1
	SHAPE BOX dx=4.3/2., dy=0.5/2., dz = 1.9/2.
EndBlock
*--------------------------------------------------------------------------------------
*
* SSD mechanical support

Block SSST is the top of the small sector
	Material Aluminium
	Attribute SSST Seen=1, Colo=1
	SHAPE TUBS rmin=31.285, rmax=31.8, dz=2.5/2., phi1=45., phi2=135.
Endblock 

Block SSSS is the side of the small sector
	Material Aluminium
	Attribute SSSS Seen=1, Colo=1
	SHAPE TUBS rmin=23.3, rmax=31.285, dz=0.5/2., phi1=45., phi2=135.
Endblock 
Block SSRT is the top of the side rib
	Material Aluminium
	Attribute SSRT Seen=1, Colo=1
	SHAPE TUBS rmin=31.285, rmax=31.8, dz=2.5/2., phi1=-45., phi2=+45.
Endblock 

Block SSRS is the side of the small rib
	Material Aluminium
	Attribute SSRS Seen=1, Colo=1
	SHAPE TUBS rmin=28.0, rmax=31.285, dz=0.5/2., phi1=-45., phi2=+45.
Endblock 

Block SSLB is the linking (sector to the cone) box
	Material Aluminium
	Attribute SSLB Seen=1, Colo=1
	SHAPE BOX dx=7.8/2., dy=7.3/2., dz=2.5/2.
Endblock 

Block SSLT is the linking (sector to the cone) tube
	Material Aluminium
	Attribute SSLT Seen=1, Colo=1
	SHAPE TUBE rmin=0., rmax=0.8, dz=5.05/2.
Endblock 

Block SCMP is the mounting plate inserted in the cones.
	Material Aluminium
	Attribute SCMP Seen=1, Colo=1
	SHAPE BOX dx=6.3/2., dy=0.4/2., dz=3.6/2
Endblock 

Block SCVM is the mother volume of the V-shape piece
	Material Air
	Attribute SCVM Seen=0, Colo=6
	SHAPE BOX dx=6.4/2., dy=2.6/2., dz=3.6/2
	
	Create SCVB
	Position SCVB x=0., y=-2.6/2.+0.5/2., z=0.
	Create SCVS
	Position SCVS x= 1.0, y=0.22, z=0. AlphaZ=45.
	Position SCVS x=-1.0, y=0.22, z=0. AlphaZ=-45.
	
Endblock 
Block SCVB is the base plate of the V-shape piece
	Material Aluminium
	Attribute SCVB Seen=1, Colo=1
	SHAPE BOX dx=6.4/2., dy=0.5/2., dz=3.6/2
Endblock 
Block SCVS is the side plate of the V-shape piece
	Material Aluminium
	Attribute SCVS Seen=1, Colo=1
	SHAPE BOX dx=2.3/2., dy=0.6/2., dz=3.6/2
Endblock 


*******************************************************************************
                                 End
