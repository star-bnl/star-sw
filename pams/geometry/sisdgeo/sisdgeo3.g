* $Id: sisdgeo3.g,v 1.2 2011/02/28 16:36:38 jwebb Exp $
* $Log: sisdgeo3.g,v $
* Revision 1.2  2011/02/28 16:36:38  jwebb
* Cosmetic changes needed for AgML syntax matching.
*
* Revision 1.1  2006/03/21 23:44:36  potekhin
* A new and much improved source by Lilian Martin, valid for
* years 2005 onward. Specs are in the comment below. Minor
* corrections by Maxim Potekhin
*
*******************************************************************************
*              This is an enhanced version updated after the sisdgeo2         *
*              More structures and parameters, no more hardcoded numbers      *
*                                                                             *  
* - The spacing between every wafer on each ladder is corrected (must be      *
* 1.5mm). The wafer pack length variable is increased accordingly             *
*                                                                             *
* - The radius of the most of the big sector ladders is corrected. It was     * 
* overestimated by 2mm. The SFPA (version 5) is changed accordingly.          * 
*                                                                             *
* - The big sector support structure is updated for the run V and later       * 
* It was correct in Run IV geometry but changed in the run V and later        * 
* (since ladders were attached to it). The vertical part of the structure is  *
* simply extended to match the new geometry.                                  *
*                                                                             *  
* - The intermediate pieces linking the SSD sectors to the cone have changed  * 
* between the Run IV and Run V. Some parts stay the same namely :             *
* - the mechanical support on the cone itself (SCMP and SCVM)                 *
* - the tube linking the SSD to this support (SSBT)                           *
* The piece connecting this tube to the SSD has changed :                     *  
* During the Run IV is it a simple aluminium rectangle orientation at 45 dgr  *
* aroung the z axis.                                                          *
* Starting at the Run V, the piece is replaced by a set of two crosse with the*   
* same shape but a different thickness. The first cross connected to the      *
* sectors is made of DELRIN while the second (and thicker) is made of ALU.    *
* and is connected to the tube mentioned above.                               *
* New variables have been added to the SFJP structure to hold the new values  * 

*******************************************************************************
Module  SISDGEO3  is the Silicon Strip Detector
   Author  Lilian Martin
   created 14 March 06

+CDE,AGECOM,GCONST,GCUNIT.
* SSD Volumes
      Content   SFMO,SFLM,SFDM,SFSW,SFSD,SFSM,SFLT,SFLU,
		SFFK,SFFL,SFKK,SFKL,
                SFRA,SFRS,SFFX,SFPI,SFPJ,SFAA,
                SFAM,SFAB,SFAS,SAPP,SAPC,SAPS,SAPT,
		SFLA,SFLB,SFLC,SFES,SFEB,
                SFCO,SFCM,SFCB,SFCS,SFKF,SFKS,SSBS,SSBB,
	        SFPR,SFPB,SSST,SSSS,SSRS,SSRT,SSLB,SSBQ,SBCH,SBCV,SBFH,SBFV,SSLT,
		SCMP,SCVM,SCVB,SCVS
* SSD Parameters:
      Structure SSDP { Version,  Int Config , Int Placement}

      Structure SFJP { Version, AlphaZ,   AlphaZH,  SSST_Rmin,  SSST_Rmax,  SSST_Width,  SSST_Pz, SSSS_Rmin,
                       SSSS_Width,  SSRS_Rmin,  SSLB_Dx,  SSLB_Px, SSLB_Dy, SSLB_Dz, 
                       SSBQ_Dx,  SSBQ_Dy, SSBQ_Dz,
                       SSCR_ThA,  SSCR_ThD,  SSCR_Wd,  SSCR_Hl,  SSCR_Vl,  
                       SSLT_Px,  SSLT_Rmax,  SSLT_Dz,  SCMP_Dx,
                       SCMP_Dy,  SCMP_Dz,  SCMP_Px,  SCVM_Dz,  SCVM_Dx,  SCVM_Px, 
                       SCVM_Dy,  SCVM_Pz,  SCVB_Dy,  SCVS_Dx,  SCVS_Dy,  SCVS_Px,  SCVS_Py,
                       SFCO_Dx,  SFCO_Dy,  SFCO_Dz,  SFCO_Px,  SFCO_Py,  SFCM_Dx,
                       SFCS_Dz,  SFKF_Dy,  SFKF_Dz,  SFKF_Dx,
                       SFKF_Px,  SFKF_Py,  SFKS_Dx,  SFKS_Px,
                       SFKS_Py,  SFPR_Py,  SFPB_Py,  SFPB_Py2, SFCB_Dx,  SSBS_Dy,  SSBS_Dx,
                       SSBB_Dx,  SSBB_Dz,  FLEX_Di,  SFPB_Dx,
                       SFPB_Dy,  SFPBDz,   SAPP_Dxe, SAPP_Dxz,
                       SAPP_Dy,  SAPP_Dz,  SAPP_Py1,  SAPP_Py2, SAPP_Py3, SFAM_Dxe, SFAM_Dxz,
                       SFAM_Dy,  SFAM_Dz,  SFAM_DZs, SFLA_Dx,
                       SFLA_Dy,  SFLA_Dz,  SFLB_Dz,  SFLC_Dz,
                       SFEB_Dx,  SFEB_Dz,  SFES_Dx,  SFFK_Dxe, SFFK_Dy,
                       SFFX_Dye, SFFK_Dz,  SFFL_Dx,  SFFK_Dxz, SFFK_Px,  SFFK_Py1, SFFK_Py2,
                       SFKL_Px,  SFKK_Dy,  SFLU_Dz,  SFLU_h1,  SFLU_bl1,
                       SFRA_Dx,  SFRA_Dy,  SFRA_Py,  SFRA_Dz,
                       SFRA_Pz,  SFSW_Dy,  SFSW_Dz,  SFRS_Dx,
                       SFRS_Dy,  SFRS_Px,  SFRS_Py,  SFSM_Ll,
                       SFFX_Dx,  SFFX_Dyz, SFPI_Rmin,SFPI_Rmax,SFPI_Px,  SFPI_Py1, SFPI_Py2, SFPI_Pz, 
                       SFPJ_Dx1, SFPJ_Dx2, SFPJ_Dy,  SFPJ_Dz,
                       SFAA_Dx,  SFAA_Dy,
                       SFAA_Dz,  SFAA_Px1, SFAA_Px2, SFAA_Px3, 
                       SFAA_Pz1, SFAA_Pz2, SFAA_Pz3, SFSD_Dx, SFSD_Dy, SFSD_Dz,  SFLA_Px,  SFLA_Py,
                       SFLC_Px,  SFLC_Py,  SFES_Px,  SFES_Py, SFES_Pz,
                       SFEB_Px ,  SFEB_Py        }



      structure SFPB { Hhight,   Khight,   Hbase,   Kbase, Fsize,
		       Zcoor}
      Structure SFPA { Version,  rmin,     rmax,     Len,
                       rad,      nssd,     dmWid,    dmThk,
		       dmLen,    smWid,    smThk,    smLen,
                       ssLen,    wpLen,    sdlen,    tilt,     
                       cprad,    cpral,    cfrad,    gpThk,
                       Int ladderMap(20),
                       ladderAngle(20), ladderTilt(20),LadderRadius(20)    }


*
      Integer   ilad,iwaf,jwaf,nc
      Real      wafpckLen,dthk,radtilt,ang
      Real           essai,hight,lng,leng,yoffset

      Fill SSDP               ! Silicon Strips
        Version  = 1          ! Version
        Config   = 1          ! There are a few configuraions possible
        Placement= 0          ! 0=cave, 1=svtt
      EndFill


      Fill SFJP                ! Different Dimension parameters
       Version    = 1          ! new version information 
       AlphaZ     = 54         ! Alpha of the SFLA and other parts
       AlphaZH    = 45         ! Alpha of other parts
       SSST_Rmin  = 31.285     ! Minimum Radius of the Small aluminium sector (outside structure)
       SSST_Rmax  = 31.8       ! Maximum Radios of the Small aluminium sector (outside structure)
       SSST_Width = 2.5        ! Width of the small aluminium sector (outside structure)
       SSST_Pz    = 0.2        ! Additional Z-Position of the aluminium sectors
       SSSS_Rmin  = 23.3       ! Width of the small inner aluminium sector (outside structure)
       SSSS_Width = 0.5        ! Width of the small inner aluminium sector (outside structure)
       SSRS_Rmin  = 28.0       ! Width of the side of the small rib (outside structure)
       SSLB_Dx    = 8.5        ! Mother volume of the linking box (sector to the cone)
       SSLB_Px    = 19.1       ! X-Position = Y-Position of the mother volume of the linking box
       SSLB_Dy    = 7.3        ! Mother volume of the linking box (sector to the cone)
       SSLB_Dz    = 2.5        ! Mother volume of the linking box (sector to the cone)
       SSBQ_Dx    = 7.8        ! Linking box (sector to the cone)
       SSBQ_Dy    = 7.3        ! Linking box (sector to the cone)
       SSBQ_Dz    = 2.5        ! Linking box (sector to the cone)
       SSCR_ThA   = 1.5        ! Linking cross total thickness (aluminum part)
       SSCR_ThD   = 1.0        ! Linking cross total thickness (DELRIN part)
       SSCR_Wd    = 1.2        ! Linking cross total width
       SSCR_Hl    = 8.5        ! Linking cross horizontal arm full length
       SSCR_Vl    = 1.6        ! Linking cross vertical arm full length
       SSLT_Px    = 18.1       ! X-Position = Y-Position of the linking tube
       SSLT_Rmax  = 0.8        ! Radius of the linking tube (sector to the cone)
       SSLT_Dz    = 5.05       ! Width of the linking tube (sector of the cone)
       SCMP_Dx    = 6.3        ! Width of the mounting plate inserted in the cones
       SCMP_Dy    = 0.4        ! Width of the mounting plate inserted in the cones
       SCMP_Dz    = 3.6        ! Width of the mounting plate inserted in the cones
       SCMP_Px    = 15.945     ! X-Position = Y-Position of the mounting plate
       SCVM_Dz    = 3.6        ! Mother volume of the V-shape piece length
       SCVM_Dx    = 6.4        ! Mother volume of the V-shape piece dx
       SCVM_Px    = 17.00      ! X-Position = Y-Position of the V-Shape piece
       SCVM_Dy    = 2.6        ! Mother volume of the V-Shape piece dy
       SCVM_Pz    = 57.5       ! Z-Position of the mother volume
       SCVB_Dy    = 0.5        ! Height of the base of the V-Shape piece
       SCVS_Dx    = 2.3        ! Dx of the V-plates of the V-shape piece 45 degrees
       SCVS_Dy    = 0.6        ! Medium Height of the V-plates of the V-shape piece 45 degrees
       SCVS_Px    = 1.0        ! x-Position of the V-plates of the V-shape piece 45 degrees
       SCVS_Py    = 0.22       ! x-Position of the V-plates of the V-shape piece 45 degrees
       SFCO_Dx    = 4.4        ! Dx of the connection board (end of the ladder)
       SFCO_Dy    = 0.37       ! Width of the connection board (end of the ladder)
       SFCO_Dz    = 15.8       ! Length of the connection board (end of the ladder)
       SFCO_Px    = 1.7        ! X-Position of the connection board (end of the ladder)
       SFCO_Py    = 0.2        ! Y-Position of the connection board (end of the ladder)
       SFCM_Dx    = 4.0        ! Dx of the second connection board (smaller than the above one!!)
       SFCS_Dz    = 7.5        ! Dx of the small part of the second connection board
       SFKF_Dy    = 0.065      ! Width of the kapton flex circuit between the connection boards
       SFKF_Dz    = 8.1        ! Length of the kapton flex circuit between the connection boards
       SFKF_Dx    = 0.44       ! Dx of the first part of the kapton flex circuit
       SFKF_Px    = 0.16       ! x-Position of the first part of the kapton flex circuit
       SFKF_Py    = 2.07       ! y-Position of the first part of the kapton flex circuit
       SFKS_Dx    = 1.65       ! Dx of the second part of the kapton flex circuit
       SFKS_Px    = 0.505      ! x-Position of the second part ot the kapton flex
       SFKS_Py    = 1.585      ! y-Position of the second part ot the kapton flex
       SFPR_Py    = 1.79       ! Y-Position of the mechanical parts
       SFPB_Py    = 3.48       ! Y-Position of the mechanical parts 2
       SFPB_Py2   = 0.08       ! Y-Position of the mechanical parts 2
       SFCB_Dx    = 3.0        ! Dx of the big part of the second connection board
       SSBS_Dy    = 0.5        ! Width of the aluminium plates linking the ladder to the sector
       SSBS_Dx    = 2.5        ! Dx = Dz = small part of the aluminium plate linking the ladder
       SSBB_Dx    = 4.3        ! Dx of the Big part of the aluminium plate linking the ladder 
       SSBB_Dz    = 1.9        ! Dz of the Big part of the aluminium plate linking the ladder 
       FLEX_Di    = 0.02       ! Distanze between overlaying flexes
       SFPB_Dx     = 4.3       ! Dx of the outside mechanical part (G10 rectangle - box base)
       SFPB_Dy     = 0.8       ! Width of the outside mechanical part (G10 rectangle - box base)
       SFPBDz      = 0.95       ! Length of the outside mechanical part (G10 rectangle - box base)
       SAPP_Dxe    = 7.6       ! Dx of the epoxy core of the adc board appendice mother volume
       SAPP_Dxz    = 0.04      ! Dx of the carbon side shell of the adc board appendice
       SAPP_Dy     = 0.5       ! Width of the epoxy core of the adc board appendice mother volume
       SAPP_Dz     = 1.275     ! Length of the adc board appendice mother volume
       SAPP_Py1    = 0.04       ! 1. y-Position of the mother colume of the adc board
       SAPP_Py2    = 0.5        ! 2. y-Position of the mother colume of the adc board
       SAPP_Py3    = 0.08       ! 3. y-Position of the mother colume of the adc board
       SFAM_Dxe    = 7.2       ! Dx of the mother volume of the adc board
       SFAM_Dxz    = 2.1       ! Dx of the small volume of the adc board
       SFAM_Dy     = 0.3       ! Width of the mother volume of the adc board
       SFAM_Dz     = 12.4      ! Length of the mother volume of the adc board
       SFAM_DZs    = 2.15      ! Length of the small volume of the adc board
       SFLA_Dx     = 1.5       ! Dx of the long part of the bus cable linking the modules
       SFLA_Dy     = 0.0130    ! Width of the long part of the bus cable linking the modules
       SFLA_Dz     = 4.3       ! Length of the long part of the bus cable linking the modules
       SFLB_Dz     = 7.5       ! Length of the long bus cable
       SFLC_Dz     = 0.93      ! Length of the long bus cable on the cb up to the connector
       SFEB_Dx     = 3.89      ! Dx of the big bus elbow
       SFEB_Dz     = 1.5       ! Length of the bus elbow
       SFES_Dx     = 1.875     ! Dx of the big bus elbow
       SFFK_Dxe     = 0.4       ! Dx of the horizontal part of the ladder skeleton carbon base
       SFFK_Dy     = 0.02      ! Width of the horizontal part of the ladder skeleton carbon base
       SFFK_Dz     = 69.75     ! Lengthof of the horizontal part of the ladder skeleton carbon base
       SFFL_Dx     = 0.6       ! Dx of the tilted part of the ladder
       SFFK_Dxz     = 5.1       ! Dx of the horizontal part of the kapton film

       SFFK_Px     = 3.5       !    x-Position of the carbon base under the ladder
       SFFK_Py1    = 0.02      ! 1. y-Position of the carbon base under the ladder
       SFFK_Py2    = 0.04      ! 2. y-Position of the carbon base under the ladder
       SFKL_Px     = 2.55       !    x-Position of the tilted kapton film under the ladder
       SFKK_Dy     = 0.0025    ! Width of the kapton film
       SFLU_Dz     = 49.8      ! Length of the triangular ladder skeleton
       SFLU_h1     = 0.02      ! Height of the triangular ladder skeleton
       SFLU_bl1    = 0.2       ! Base length  of the triangular ladder skeleton
       SFRA_Dx     = 3.76      !         Dx of the hybrid stiffner
       SFRA_Dy     = 0.019     !      Width of the hybrid stiffner
       SFRA_Py    = 0.099     ! Y-Position of the hybrid stiffner
       SFRA_Dz     = 1.0       !     Length of the hybrid stiffner
       SFRA_Pz     = 1.1       ! Z-Position of the hybrid stiffner
       SFSW_Dy     = 1.0       ! Width of a single wafer container
       SFSW_Dz     = 2.25      ! Lengthof of a single wafer container
       SFRS_Dx     = 0.04      ! Dx of the two supports of the hybrid stiffner
       SFRS_Dy     = 0.0915    ! Dy of the two supports of the hybrid stiffner
       SFRS_Px     = 3.8       ! x-Position of the two supports of the hybrid stiffner
       SFRS_Py     = 0.0265    ! y-Position of the two supports of the hybrid stiffner
       SFSM_Ll     = 2.6       ! Length of the triangular base
       SFFX_Dx     = 3.84      ! Dx of the flex
       SFFX_Dyz    = 0.0007    ! Width of the flex
       SFPI_Rmin   = 0.15      ! Rmin of the pions
       SFPI_Rmax   = 0.2       ! Rmax of the pions
       SFPI_Px     = 3.2       ! X-Position of the four pions 
       SFPI_Py1    = 0.35      ! 1. Y-Position of the four pions 
       SFPI_Py2    = 0.025     ! 2. Y-Position of the four pions 
       SFPI_Pz     = 0.7       ! Z-Position of the four pions 
       SFPJ_Dx1    = 0.075     ! 1. Dx of the base of the pions
       SFPJ_Dx2    = 0.025     ! 2. Dx of the base of the pions
       SFPJ_Dy     = 0.3       ! Dy of the base of the pions
       SFPJ_Dz     = 0.0125    ! Dz of the base of the pions
       SFAA_Dx     = 0.3       ! Dx of the A128C chip
       SFAA_Dy     = 0.015     ! Dy of the A128C chip
       SFAA_Dz     = 0.4       ! Dz of the A128C chip       
       SFAA_Px1    = 0.325    ! 1. x-=Position of the A128C chips
       SFAA_Px2    = 0.65     ! 2. x-=Position of the A128C chips
       SFAA_Px3    = 0.6      ! 3. x-=Position of the A128C chips
       SFAA_Pz1    = 1.1      ! 1. z-=Position of the A128C chips
       SFAA_Pz2    = 1.0      ! 2. z-=Position of the A128C chips
       SFAA_Pz3    = 0.02     ! 3. z-=Position of the A128C chips

       SFSD_Dx     = 3.75      ! Dx of the strip detector
       SFSD_Dy     = 0.015     ! Dy of the strip detector
       SFSD_Dz     = 2.1       ! Dz of the strip detector

       SFLA_Px     = 1.1       ! x-Position of the top flex along the z-axis    
       SFLA_Py     = 1.35      ! y-Position of the top flex along the z-axis
       SFLC_Px     = 2.3       ! x-Position of the bottom flex along the z-axis
       SFLC_Py     = 0.3       ! y-Position of the bottom flex along the z-axis
       SFES_Px     = 3.32      ! x-Position of the small elbow bus
       SFES_Py     = 1.6       ! y-Position of the small elbow bus
       SFES_Pz     = 0.1       ! z-Position of the small elbow bus
       SFEB_Px     = 2.71      ! x-Position of the big elbow bus
       SFEB_Py     = 0.75      ! y-Position of the big elbow bus
      EndFill
* The new version is introduced to defined the new geometry of the
* big sectors (SSRS) and the mechanical pieces linking the sectors to the cones. 
* Everything else stays the same...
      Fill SFJP                ! Different Dimension parameters
       Version    = 2          ! Version 
       SSRS_Rmin  = 23.3       ! Width of the side of the small rib (outside structure)
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
        wpLen    = 69.6       ! length of wafer pack
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
* In the following, ladders 5,7,15,17 have their radii corrected as per
* Lilian Martin   instructions  --maxim--
      Fill SFPA               ! Silicon Strip detector parameters
        version  = 4          ! geometry version
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
        ladderRadius= {23.177,22.800,22.800,22.800,22.800,
                       22.500,22.800,22.800,22.800,22.800,
                       23.177,22.800,22.800,22.800,22.800,
                       22.500,22.800,22.800,22.800,22.800} ! individual radii
      EndFill
*
* In the following, all the big sector ladders (3 to 9 and 13 to 19) have their radius
* reduced by 2 mm. EXCEPTED the ladder 16 which stays at 22.5 cm.
* LM 21-Feb-06
      Fill SFPA               ! Silicon Strip detector parameters
        version  = 5          ! geometry version
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
        ladderRadius= {23.177,22.800,22.600,22.600,22.600,
                       22.300,22.600,22.600,22.600,22.800,
                       23.177,22.800,22.600,22.600,22.600,
                       22.500,22.600,22.600,22.600,22.800} ! individual radii
      EndFill
*
*************************************************************************************************
*************************************************************************************************
*************************************************************************************************

      USE SSDP
* was hardcoded to 5:
      USE SFPA version=SSDP_Config
      USE SFJP version=2

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

*     Delrin is modeled as polymethylmethacralate - correct radlength is ensured
        Component O   A=16     Z=8    W=2*16./100.
        Component C   A=12     Z=6    W=5*12./100.
        Component H   A=1      Z=1    W=8*1.0/100.
        Mixture   DELRIN   Dens=1.2


      write(*,*) 'Level 3 of the SSD geometry, Configuration: ',SSDP_Config
      write(*,*) 'WARNING: this geometry is only useable for 2005 and onward'

      if(ssdp_Placement==1) then
         write(*,*) '*** Positioining the Silicon Strip Detector in SVT'
         Create and Position SFMO in SVTT
      else
         write(*,*) '*** Positioining the Silicon Strip Detector in CAVE'
         Create and Position SFMO in CAVE
      endif
*******************************************************************************
*
Block SFMO is the mother of all Silicon Strip Detector volumes
      Material   Air
      Attribute  SFMO   Seen=0 Colo=6
      Shape TUBE Rmin = SFPA_rmin,
                 Rmax = SFPA_rmax,
                 dz   = SFPA_Len/2
                 dthk = SFPA_smThk + SFPA_gpThk

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
            Position SFLM x = (SFPA_ladderRadius(ilad)*cos(ang)+(dthk*cos(ang+radtilt))/2.0),
                          y = (SFPA_ladderRadius(ilad)*sin(ang)+(dthk*sin(ang+radtilt))/2.0),
                          z = 0, 
                     AlphaZ = SFPA_ladderAngle(ilad)-90.0+SFPA_ladderTilt(ilad),
                      Ncopy = nc, 
                      Konly = 'MANY'
        endif
      EndDo

*** Putting the mechanical structure in place

** Top parts of the small sectors
    Create SSST
    Position SSST x = 0., y = 0., z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSST_Width/2.-SFJP_SSST_Pz
    Position SSST x = 0., y = 0., z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSST_Width/2.+SFJP_SSST_Pz
    Position SSST x = 0., y = 0., z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSST_Width/2.-SFJP_SSST_Pz, AlphaZ=180.
    Position SSST x = 0., y = 0., z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSST_Width/2.+SFJP_SSST_Pz, AlphaZ=180.
** Side parts of the small sectors
    Create SSSS
    Position SSSS x = 0., y = 0., z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width/2.-SFJP_SSST_Pz
    Position SSSS x = 0., y = 0., z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width/2.+SFJP_SSST_Pz
    Position SSSS x = 0., y = 0., z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width/2.-SFJP_SSST_Pz, AlphaZ=180.
    Position SSSS x = 0., y = 0., z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width/2.+SFJP_SSST_Pz, AlphaZ=180.

** Top parts of the ribs or the big sectors
    Create SSRT
    Position SSRT  x = 0., 
                   y = 0., 
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSST_Width/2.-SFJP_SSST_Pz
    Position SSRT  x = 0., 
                   y = 0., 
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSST_Width/2.+SFJP_SSST_Pz
    Position SSRT  x = 0., 
                   y = 0., 
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSST_Width/2.-SFJP_SSST_Pz, 
              AlphaZ = 180.
    Position SSRT  x = 0., 
                   y = 0., 
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSST_Width/2.+SFJP_SSST_Pz, 
              AlphaZ = 180.

** Side parts of the ribs or the big sectors
    Create SSRS
    Position SSRS  x = 0.,      
                   y = 0.,     
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width/2.-SFJP_SSST_Pz
    Position SSRS  x = 0.,      
                   y = 0.,     
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width/2.+SFJP_SSST_Pz
    Position SSRS  x = 0.,      
                   y = 0.,     
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width/2.-SFJP_SSST_Pz,       
              AlphaZ = 180.
    Position SSRS  x = 0.,      
                   y = 0.,     
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width/2.+SFJP_SSST_Pz,       
              AlphaZ = 180.

** SSD to cone linking box
	Create SSLB
if (SFJP_version==1) then
*---- Run IV version. The piece is just a box rotated by 45dgr around z. With its
*---- own positioning.
    Position SSLB  x =  SFJP_SSLB_Px,   
                   y =  SFJP_SSLB_Px,  
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width-SFJP_SSST_Pz-SFJP_SSLB_Dz/2., 
                   AlphaZ =  SFJP_AlphaZH
    Position SSLB  x = -SFJP_SSLB_Px,   
                   y = -SFJP_SSLB_Px,  
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width-SFJP_SSST_Pz-SFJP_SSLB_Dz/2., 
                   AlphaZ =  SFJP_AlphaZH
    Position SSLB  x =  SFJP_SSLB_Px,   
                   y = -SFJP_SSLB_Px,  
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width-SFJP_SSST_Pz-SFJP_SSLB_Dz/2., 
                   AlphaZ = -SFJP_AlphaZH
    Position SSLB  x = -SFJP_SSLB_Px,   
                   y =  SFJP_SSLB_Px,  
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width-SFJP_SSST_Pz-SFJP_SSLB_Dz/2., 
                   AlphaZ = -SFJP_AlphaZH
    Position SSLB  x =  SFJP_SSLB_Px,   
                   y =  SFJP_SSLB_Px,  
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width+SFJP_SSST_Pz+SFJP_SSLB_Dz/2., 
                   AlphaZ =  SFJP_AlphaZH
    Position SSLB  x = -SFJP_SSLB_Px,   
                   y = -SFJP_SSLB_Px,  
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width+SFJP_SSST_Pz+SFJP_SSLB_Dz/2., 
                   AlphaZ =  SFJP_AlphaZH
    Position SSLB  x =  SFJP_SSLB_Px,   
                   y = -SFJP_SSLB_Px,  
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width+SFJP_SSST_Pz+SFJP_SSLB_Dz/2., 
                   AlphaZ = -SFJP_AlphaZH
    Position SSLB  x = -SFJP_SSLB_Px,   
                   y =  SFJP_SSLB_Px,  
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width+SFJP_SSST_Pz+SFJP_SSLB_Dz/2., 
                   AlphaZ = -SFJP_AlphaZH
else
*---- Run V version. The piece is made of two crosses. the largest arms are horizontal. The cross
*---- is centered to the linking tube axis (placed below)...
    Position SSLB  x =  SFJP_SSLT_Px,   
                   y =  SFJP_SSLT_Px,  
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width-SFJP_SSST_Pz-SFJP_SSLB_Dz/2., 
                   AlphaZ =  0
    Position SSLB  x = -SFJP_SSLT_Px,   
                   y = -SFJP_SSLT_Px,  
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width-SFJP_SSST_Pz-SFJP_SSLB_Dz/2., 
                   AlphaZ =  0
    Position SSLB  x =  SFJP_SSLT_Px,   
                   y = -SFJP_SSLT_Px,  
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width-SFJP_SSST_Pz-SFJP_SSLB_Dz/2., 
                   AlphaZ =  0
    Position SSLB  x = -SFJP_SSLT_Px,   
                   y =  SFJP_SSLT_Px,  
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width-SFJP_SSST_Pz-SFJP_SSLB_Dz/2., 
                   AlphaZ = 0
    Position SSLB  x =  SFJP_SSLT_Px,   
                   y =  SFJP_SSLT_Px,  
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width+SFJP_SSST_Pz+SFJP_SSLB_Dz/2., 
                   AlphaY =  180
    Position SSLB  x = -SFJP_SSLT_Px,   
                   y = -SFJP_SSLT_Px,  
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width+SFJP_SSST_Pz+SFJP_SSLB_Dz/2., 
                   AlphaY =  180
    Position SSLB  x =  SFJP_SSLT_Px,   
                   y = -SFJP_SSLT_Px,  
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width+SFJP_SSST_Pz+SFJP_SSLB_Dz/2., 
                   AlphaY =  180
    Position SSLB  x = -SFJP_SSLT_Px,   
                   y =  SFJP_SSLT_Px,  
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width+SFJP_SSST_Pz+SFJP_SSLB_Dz/2., 
                   AlphaY = 180
endif

** SSD to cone linking tube
	Create SSLT
    Position SSLT  x =  SFJP_SSLT_Px,   
                   y =  SFJP_SSLT_Px,  
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width-SFJP_SSST_Pz-SFJP_SSLB_Dz-SFJP_SSLT_Dz/2.
    Position SSLT  x = -SFJP_SSLT_Px,   
                   y = -SFJP_SSLT_Px,  
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width-SFJP_SSST_Pz-SFJP_SSLB_Dz-SFJP_SSLT_Dz/2.
    Position SSLT  x =  SFJP_SSLT_Px,   
                   y = -SFJP_SSLT_Px,  
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width-SFJP_SSST_Pz-SFJP_SSLB_Dz-SFJP_SSLT_Dz/2.
    Position SSLT  x = -SFJP_SSLT_Px,   
                   y =  SFJP_SSLT_Px,  
                   z = -SFJP_SFLU_Dz-SFJP_SFPBDz-SFJP_SSSS_Width-SFJP_SSST_Pz-SFJP_SSLB_Dz-SFJP_SSLT_Dz/2.
    Position SSLT  x =  SFJP_SSLT_Px,   
                   y =  SFJP_SSLT_Px,  
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width+SFJP_SSST_Pz+SFJP_SSLB_Dz+SFJP_SSLT_Dz/2.
    Position SSLT  x = -SFJP_SSLT_Px,   
                   y = -SFJP_SSLT_Px,  
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width+SFJP_SSST_Pz+SFJP_SSLB_Dz+SFJP_SSLT_Dz/2.
    Position SSLT  x =  SFJP_SSLT_Px,   
                   y = -SFJP_SSLT_Px,  
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width+SFJP_SSST_Pz+SFJP_SSLB_Dz+SFJP_SSLT_Dz/2.
    Position SSLT  x = -SFJP_SSLT_Px,   
                   y =  SFJP_SSLT_Px,  
                   z = +SFJP_SFLU_Dz+SFJP_SFPBDz+SFJP_SSSS_Width+SFJP_SSST_Pz+SFJP_SSLB_Dz+SFJP_SSLT_Dz/2.

** SSD mounting plate inserted in the cone
	Create SCMP
	Position SCMP  x =  SFJP_SCMP_Px, y =  SFJP_SCMP_Px, z =  SFJP_SCVM_Pz , AlphaZ = -SFJP_AlphaZH
	Position SCMP  x = -SFJP_SCMP_Px, y = -SFJP_SCMP_Px, z =  SFJP_SCVM_Pz , AlphaZ = -SFJP_AlphaZH
	Position SCMP  x =  SFJP_SCMP_Px, y = -SFJP_SCMP_Px, z =  SFJP_SCVM_Pz , AlphaZ =  SFJP_AlphaZH
	Position SCMP  x = -SFJP_SCMP_Px, y =  SFJP_SCMP_Px, z =  SFJP_SCVM_Pz , AlphaZ =  SFJP_AlphaZH
	Position SCMP  x =  SFJP_SCMP_Px, y =  SFJP_SCMP_Px, z = -SFJP_SCVM_Pz , AlphaZ = -SFJP_AlphaZH
	Position SCMP  x = -SFJP_SCMP_Px, y = -SFJP_SCMP_Px, z = -SFJP_SCVM_Pz , AlphaZ = -SFJP_AlphaZH
	Position SCMP  x =  SFJP_SCMP_Px, y = -SFJP_SCMP_Px, z = -SFJP_SCVM_Pz , AlphaZ =  SFJP_AlphaZH
	Position SCMP  x = -SFJP_SCMP_Px, y =  SFJP_SCMP_Px, z = -SFJP_SCVM_Pz , AlphaZ =  SFJP_AlphaZH

** SSD V-shape mouting piece
	Create SCVM
	Position SCVM  x =  SFJP_SCVM_Px, y =  SFJP_SCVM_Px, z =  SFJP_SCVM_Pz , AlphaZ = - SFJP_AlphaZH
	Position SCVM  x = -SFJP_SCVM_Px, y = -SFJP_SCVM_Px, z =  SFJP_SCVM_Pz , AlphaZ =  (180.-SFJP_AlphaZH)
	Position SCVM  x =  SFJP_SCVM_Px, y = -SFJP_SCVM_Px, z =  SFJP_SCVM_Pz , AlphaZ = -(180.-SFJP_AlphaZH)
	Position SCVM  x = -SFJP_SCVM_Px, y =  SFJP_SCVM_Px, z =  SFJP_SCVM_Pz , AlphaZ =   SFJP_AlphaZH
	Position SCVM  x =  SFJP_SCVM_Px, y =  SFJP_SCVM_Px, z = -SFJP_SCVM_Pz , AlphaZ = - SFJP_AlphaZH
	Position SCVM  x = -SFJP_SCVM_Px, y = -SFJP_SCVM_Px, z = -SFJP_SCVM_Pz , AlphaZ =  (180.-SFJP_AlphaZH)
	Position SCVM  x =  SFJP_SCVM_Px, y = -SFJP_SCVM_Px, z = -SFJP_SCVM_Pz , AlphaZ = -(180.-SFJP_AlphaZH)
	Position SCVM  x = -SFJP_SCVM_Px, y =  SFJP_SCVM_Px, z = -SFJP_SCVM_Pz , AlphaZ =   SFJP_AlphaZH
Endblock 
*
*------------------------------------------------------------------------------
* 
Block SFLM is the mother of the ladder
* (dets,adcs and struct.)
      Material Air
      Attribute SFLM Seen=0 Colo=6
      Shape BOX dx = SFPA_dmWid/2,
                dy = (SFPA_dmThk+SFPA_gpThk+SFPA_smThk)/2,
                dz = SFPA_smLen/2 
      Create   SFDM " the detectors and adcs mother volume "
      Position SFDM y = -(SFPA_smThk+SFPA_gpThk)/2
		    

      Create   SFSM " the structure mother volume"
      Position SFSM y = (SFPA_dmThk+SFPA_gpThk)/2
                    
Endblock 
*
*------------------------------------------------------------------------------
* 
Block SFDM is the mother of the detectors 
      Material Air
      Attribute SFDM Seen=0 Colo=6
      Shape BOX dx = SFPA_dmWid/2,
                dy = SFPA_dmThk/2,
                dz = SFPA_wpLen/2 
      
      wafpckLen=SFPA_wpLen/(SFPA_nssd*1.)
      Do iwaf=1,SFPA_nssd
        Create    SFSW " single wafer container"
        Position  SFSW z = -(SFPA_wpLen+wafpckLen)/2+iwaf*wafpckLen
     EndDo 
Endblock 
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
Block SFSW is a single wafer container
      Material Air
      Attribute SFSW Seen=0 Colo=6
    Shape BOX dx = SFPA_dmWid/2,
              dy = SFJP_SFSW_Dy,
              dz = SFJP_SFSW_Dz

Create and Position SFSD " strip detector" 
         
Create SFRA " hybrid stiffneer" 
Position SFRA x =  0.0,           y = SFJP_SFRA_Py, z=  SFJP_SFRA_Pz  
Position SFRA x =  0.0,           y = SFJP_SFRA_Py, z= -SFJP_SFRA_Pz  
 
Create SFRS "two parts of the hybrid stiffneer (support)" 
Position SFRS x =  SFJP_SFRS_Px,  y = SFJP_SFRS_Py, z=  SFJP_SFRA_Pz 
Position SFRS x =  SFJP_SFRS_Px,  y = SFJP_SFRS_Py, z= -SFJP_SFRA_Pz 
Position SFRS x = -SFJP_SFRS_Px,  y = SFJP_SFRS_Py, z=  SFJP_SFRA_Pz 
Position SFRS x = -SFJP_SFRS_Px,  y = SFJP_SFRS_Py, z= -SFJP_SFRA_Pz 

 
Create SFFX "flex on the hybrid stiffneer" 
Position SFFX x =  0.0,  y = SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz, z =  SFJP_SFRA_Pz 
Position SFFX x =  0.0,  y = SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz, z = -SFJP_SFRA_Pz 

Create SFPI "four pions" 
Position SFPI x =  SFJP_SFPI_Px, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFPI_Py1-SFJP_SFPI_Py2, 
              z =  SFJP_SFPI_Pz, 
         AlphaX =  90 
Position SFPI x = -SFJP_SFPI_Px, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFPI_Py1-SFJP_SFPI_Py2, 
              z =  SFJP_SFPI_Pz, 
         AlphaX =  90 
Position SFPI x =  SFJP_SFPI_Px, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFPI_Py1-SFJP_SFPI_Py2, 
              z = -SFJP_SFPI_Pz, 
         AlphaX =  90 
Position SFPI x = -SFJP_SFPI_Px, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFPI_Py1-SFJP_SFPI_Py2, 
              z = -SFJP_SFPI_Pz, 
         AlphaX =  90 

Create SFAA "A128C chips"
Position SFAA x = -SFJP_SFAA_Px1-SFJP_SFAA_Dx,         
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz*2+SFJP_SFAA_Dy, 
              z =  SFJP_SFAA_Pz1+SFJP_SFAA_Pz2-SFJP_SFAA_Pz3-SFJP_SFAA_Dz 
Position SFAA x = -SFJP_SFAA_Px1-SFJP_SFAA_Dx-SFJP_SFAA_Px2-SFJP_SFAA_Px3, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz*2+SFJP_SFAA_Dy, 
              z =  SFJP_SFAA_Pz1+SFJP_SFAA_Pz2-SFJP_SFAA_Pz3-SFJP_SFAA_Dz 
Position SFAA x = -SFJP_SFAA_Px1-SFJP_SFAA_Dx-SFJP_SFAA_Px2-SFJP_SFAA_Px3-SFJP_SFAA_Px2-SFJP_SFAA_Px3, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz*2+SFJP_SFAA_Dy, 
              z =  SFJP_SFAA_Pz1+SFJP_SFAA_Pz2-SFJP_SFAA_Pz3-SFJP_SFAA_Dz 
Position SFAA x = +SFJP_SFAA_Px1+SFJP_SFAA_Dx, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz*2+SFJP_SFAA_Dy, 
              z =  SFJP_SFAA_Pz1+SFJP_SFAA_Pz2-SFJP_SFAA_Pz3-SFJP_SFAA_Dz  
Position SFAA x = +SFJP_SFAA_Px1+SFJP_SFAA_Dx+SFJP_SFAA_Px2+SFJP_SFAA_Px3, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz*2+SFJP_SFAA_Dy, 
              z =  SFJP_SFAA_Pz1+SFJP_SFAA_Pz2-SFJP_SFAA_Pz3-SFJP_SFAA_Dz 
Position SFAA x = +SFJP_SFAA_Px1+SFJP_SFAA_Dx+SFJP_SFAA_Px2+SFJP_SFAA_Px3+SFJP_SFAA_Px2+SFJP_SFAA_Px3, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz*2+SFJP_SFAA_Dy, 
              z =  SFJP_SFAA_Pz1+SFJP_SFAA_Pz2-SFJP_SFAA_Pz3-SFJP_SFAA_Dz  
Position SFAA x = -SFJP_SFAA_Px1-SFJP_SFAA_Dx, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz*2+SFJP_SFAA_Dy, 
              z = -SFJP_SFAA_Pz1-SFJP_SFAA_Pz2+SFJP_SFAA_Pz3+SFJP_SFAA_Dz 
Position SFAA x = -SFJP_SFAA_Px1-SFJP_SFAA_Dx-SFJP_SFAA_Px2-SFJP_SFAA_Px3, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz*2+SFJP_SFAA_Dy, 
              z = -SFJP_SFAA_Pz1-SFJP_SFAA_Pz2+SFJP_SFAA_Pz3+SFJP_SFAA_Dz 
Position SFAA x = -SFJP_SFAA_Px1-SFJP_SFAA_Dx-SFJP_SFAA_Px2-SFJP_SFAA_Px3-SFJP_SFAA_Px2-SFJP_SFAA_Px3, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz*2+SFJP_SFAA_Dy, 
              z = -SFJP_SFAA_Pz1-SFJP_SFAA_Pz2+SFJP_SFAA_Pz3+SFJP_SFAA_Dz 
Position SFAA x = +SFJP_SFAA_Px1+SFJP_SFAA_Dx, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz*2+SFJP_SFAA_Dy, 
              z = -SFJP_SFAA_Pz1-SFJP_SFAA_Pz2+SFJP_SFAA_Pz3+SFJP_SFAA_Dz  
Position SFAA x = +SFJP_SFAA_Px1+SFJP_SFAA_Dx+SFJP_SFAA_Px2+SFJP_SFAA_Px3, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz*2+SFJP_SFAA_Dy, 
              z = -SFJP_SFAA_Pz1-SFJP_SFAA_Pz2+SFJP_SFAA_Pz3+SFJP_SFAA_Dz 
Position SFAA x = +SFJP_SFAA_Px1+SFJP_SFAA_Dx+SFJP_SFAA_Px2+SFJP_SFAA_Px3+SFJP_SFAA_Px2+SFJP_SFAA_Px3, 
              y =  SFJP_SFRA_Py+SFJP_SFRA_Dy+SFJP_SFFX_Dyz*2+SFJP_SFAA_Dy, 
              z = -SFJP_SFAA_Pz1-SFJP_SFAA_Pz2+SFJP_SFAA_Pz3+SFJP_SFAA_Dz  

EndBlock

*------------------------------------------------------------------------

Block SFRA is the hybrid stiffneer
      Material Carbon
      Attribute SFRA Seen=2 Colo=1
*      Shape BOX dx=1., dy=1., dz=1.
      Shape BOX dx = SFJP_SFRA_Dx,
                dy = SFJP_SFRA_Dy,
                dz = SFJP_SFRA_Dz
Endblock

*-----------------------------------------------------------------------

Block SFRS two supports of the hybrid stiffneer (piece of it)
	Material Carbon
	Attribute SFRS Seen=2 Colo=1
    Shape BOX dx = SFJP_SFRS_Dx,
              dy = SFJP_SFRS_Dy,
              dz = SFJP_SFRA_Dz
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
    Shape BOX dx = SFJP_SFFX_Dx,
              dy = SFJP_SFFX_Dyz,
              dz = SFJP_SFRA_Dz
*             dy = (0.065-0.038)/2,
Endblock
*-----------------------------------------------------------------------
Block SFAA is the A128C chip

	Material Silicon
	Attribute SFAA Seen=1, Colo=41
    Shape BOX dx = SFJP_SFAA_Dx,
              dy = SFJP_SFAA_Dy,
              dz = SFJP_SFAA_Dz
Endblock

*=====================================================================

Block SFPI are the pions
	Material Aluminium
	Attribute SFPI seen=2 Colo=33
    Shape TUBE rmin = SFJP_SFPI_Rmin/2., 
               rmax = SFJP_SFPI_Rmax/2., 
               dz   = SFJP_SFPI_Pz/2.0	

Create SFPJ "pions bases"
Position SFPJ x =  0., 
              y =  0.,
              z = -SFJP_SFPI_Pz/2.0+SFJP_SFPJ_Dz 
Endblock

*----------------------------------------------------------------------

Block SFPJ is the base of the pions
	Material Aluminium
	Attribute SFPJ seen=2, Colo=33
    Shape TUBE rmin = SFJP_SFPJ_Dx1+SFJP_SFPJ_Dx2, 
               rmax = SFJP_SFPJ_Dy, 
               dz   = SFJP_SFPJ_Dz
Endblock

*======================================================================

Block SFSD is the strip detector
      Material  Silicon  
      Material  Sensitive  Isvol=1 
      
      Attribute SFSD       seen=2  Colo=41
      Shape   BOX dx = SFJP_SFSD_Dx,
                  dy = SFJP_SFSD_Dy,
                  dz = SFJP_SFSD_Dz
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
    Shape BOX dx =  SFPA_dmWid/2, 
              dy =  SFPA_smThk/2, 
              dz =  SFPA_smLen/2.
         yoffset = -1.7

Create SFLT "ladder skeleton : top corner of the triangle"
	   hight = SFJP_SFSM_Ll*tan(54*pi/180)-SFPB_Hhight/tan(pi/5)-0.02
Position SFLT x = -(SFPB_Hhight-SFPB_Khight),
              y =  hight+yoffset, 
              z =  0.0, 
         AlphaZ = -90-36  
Position SFLT x = (SFPB_Hhight-SFPB_Khight),
              y = hight+yoffset, 
              z = 0,
         AlphaY = 180., 
         AlphaZ = 90.+36.

Create SFLU "ladder skeleton : side corner of the triangle" 
Position SFLU x =   0.-(SFJP_SFSM_Ll-(SFJP_SFLU_bl1+SFJP_SFLU_h1/2*1/tan(pi/5))), 
              y =   0.+yoffset, 
              z =   0.
          essai = 180
          essai = essai + 27 + 27
Position SFLU x = -cos(63*pi/180)*2*(SFPB_Hbase-SFPB_Kbase)-(SFJP_SFSM_Ll-(0.2+0.01/tan(pi/5))),
              y =  cos(27*pi/180)*2*(SFPB_Hbase-SFPB_Kbase)+yoffset, 
              z =    0, 
         Alphay =  180, 
         Alphaz =  essai 
Position SFLU x =   0.+(SFJP_SFSM_Ll-(0.2+0.01/tan(pi/5))), 
              y =   0.+yoffset, 
              z =   0., 
         Alphay = 180.
Position SFLU x =  cos(63*pi/180)*2*(SFPB_Hbase-SFPB_Kbase)+(SFJP_SFSM_Ll-(0.2+0.01/tan(pi/5))),
              y =  cos(27*pi/180)*2*(SFPB_Hbase-SFPB_Kbase)+yoffset,
              z =  0,
         Alphaz = -essai



Create SFFK "carbon base under the ladder"
Position SFFK x =  SFJP_SFFK_Px/2+SFJP_SFFK_Dxe, 
              y = -SFJP_SFFK_Py1-SFJP_SFFK_Py2-SFJP_SFFK_Dy+yoffset, 
              z =  0.0
Position SFFK x = -SFJP_SFFK_Px/2-SFJP_SFFK_Dxe, 
              y = -SFJP_SFFK_Py1-SFJP_SFFK_Py2-SFJP_SFFK_Dy+yoffset, 
              z =  0.0

Create SFFL "tilted carbon base under the ladder"
Position SFFL x = -SFJP_SFKL_Px-SFPB_Fsize*cos(15*pi/180),
              y = -SFJP_SFFK_Py1-SFJP_SFFK_Py2-SFJP_SFFK_Dy-SFPB_Fsize*cos(75*pi/180)+yoffset, 
              z =  0,
         AlphaZ =  15
Position SFFL x =  SFJP_SFKL_Px+SFPB_Fsize*cos(15*pi/180), 
              y = -SFJP_SFFK_Py1-SFJP_SFFK_Py2-SFJP_SFFK_Dy-SFPB_Fsize*cos(75*pi/180)+yoffset, 
              z =  0,
         AlphaZ = -15

Create SFKK "kapton film under the ladder"
Position SFKK x =  0., 
              y = -SFJP_SFFK_Py1-SFJP_SFFK_Py2-SFJP_SFFK_Py2+yoffset-SFJP_SFKK_Dy/2., 
              z =  0.

Create SFKL "tilted kapton film under the ladder"
Position SFKL x = -SFJP_SFKL_Px-SFPB_Fsize*cos(15*pi/180)+0.005,
              y = -SFJP_SFFK_Py1-SFJP_SFFK_Py2-SFJP_SFFK_Dy-SFPB_Fsize*cos(75*pi/180)+yoffset-0.021, 
              z =   0,
         AlphaZ =  15
Position SFKL x = +SFJP_SFKL_Px+SFPB_Fsize*cos(15*pi/180)-0.005,
              y = -SFJP_SFFK_Py1-SFJP_SFFK_Py2-SFJP_SFFK_Dy-SFPB_Fsize*cos(75*pi/180)+yoffset-0.021, 
              z =   0,
         AlphaZ = -15


Create SAPP "Mother volume of the adc board appendice" 
Position SAPP x =  0., 
              y = -SFJP_SAPP_Py1-(SFJP_SAPP_Py2+SFJP_SAPP_Py3)/2.+yoffset, 
              z = SFJP_SFFK_Dz/2+SFJP_SAPP_Dz/2., 
         AlphaY = 180.
Position SAPP x =  0., 
              y = -SFJP_SAPP_Py1-(SFJP_SAPP_Py2+SFJP_SAPP_Py3)/2.+yoffset, 
              z = -(SFJP_SFFK_Dz/2+SFJP_SAPP_Dz/2.)

Create SFAM "Adc mother volume"
Position SFAM x =  0., 
              y = -SFJP_SAPP_Py1-(SFJP_SAPP_Py2+SFJP_SAPP_Py3)/2.+yoffset, 
              z = -(SFJP_SFFK_Dz/2+SFJP_SAPP_Dz)-SFJP_SFAM_Dz/2.
Position SFAM x =  0., 
              y = -SFJP_SAPP_Py1-(SFJP_SAPP_Py2+SFJP_SAPP_Py3)/2.+yoffset, 
              z = +(SFJP_SFFK_Dz/2+SFJP_SAPP_Dz)+SFJP_SFAM_Dz/2., 
         AlphaY = 180.

Create SFCO "Connection board 1"
Position SFCO x = -SFJP_SFCO_Px, 
              y =  SFJP_SFCO_Py, 
              z = -(SFJP_SFFK_Dz/2-(SFJP_SFKF_Dz-SFJP_SFCS_Dz))-SFJP_SFCO_Dz/2., 
         AlphaZ = +SFJP_AlphaZ
Position SFCO x = +SFJP_SFCO_Px, 
              y =  SFJP_SFCO_Py, 
              z = +(SFJP_SFFK_Dz/2-(SFJP_SFKF_Dz-SFJP_SFCS_Dz))+SFJP_SFCO_Dz/2., 
         AlphaZ = -SFJP_AlphaZ

Create SFCM "Connection board 2"
Position SFCM x = +SFJP_SFCO_Px, 
              y =  SFJP_SFCO_Py, 
              z = -(SFJP_SFFK_Dz/2-(SFJP_SFKF_Dz-SFJP_SFCS_Dz))-SFJP_SFCO_Dz/2., 
         AlphaZ = -SFJP_AlphaZ
Position SFCM x = -SFJP_SFCO_Px, 
              y =  SFJP_SFCO_Py, 
              z = +(SFJP_SFFK_Dz/2-(SFJP_SFKF_Dz-SFJP_SFCS_Dz))+SFJP_SFCO_Dz/2., 
         AlphaY = 180, 
         AlphaZ = +SFJP_AlphaZ

Create SFKF "Kapton flex 1"
Position SFKF x = -SFJP_SFKF_Px, 
              y =  SFJP_SFKF_Py, 
              z = -(SFJP_SFFK_Dz/2-(SFJP_SFKF_Dz-SFJP_SFCS_Dz))-SFJP_SFKF_Dz/2-0.2, 
         AlphaZ = +SFJP_AlphaZ
Position SFKF x = +SFJP_SFKF_Px, 
              y =  SFJP_SFKF_Py, 
              z = +(SFJP_SFFK_Dz/2-(SFJP_SFKF_Dz-SFJP_SFCS_Dz))+SFJP_SFKF_Dz/2+0.2, 
         AlphaZ = -SFJP_AlphaZ
Create SFKS "Kapton flex 2"
Position SFKS x = +SFJP_SFKS_Px, 
              y =  SFJP_SFKS_Py, 
              z = -(SFJP_SFFK_Dz/2-(SFJP_SFKF_Dz-SFJP_SFCS_Dz))-SFJP_SFKF_Dz/2-0.2, 
         AlphaZ = -SFJP_AlphaZ
Position SFKS x = -SFJP_SFKS_Px, 
              y =  SFJP_SFKS_Py, 
              z = +(SFJP_SFFK_Dz/2-(SFJP_SFKF_Dz-SFJP_SFCS_Dz))+SFJP_SFKF_Dz/2+0.2, 
         AlphaZ = +SFJP_AlphaZ

Create SFPR "End mechanical part 1"
Position SFPR x = 0, 
              y = +SFJP_SFPR_Py, 
              z = -SFJP_SFLU_Dz
Position SFPR x = 0, 
              y = +SFJP_SFPR_Py, 
              z = +SFJP_SFLU_Dz-3.7

Create SFPB "End mechanical part 2"
*** The y calculation below is strange (good value by chance ??)
Position SFPB x = 0, 
              y = +SFJP_SFPR_Py-SFJP_SFPB_Py+SFJP_SFPB_Py2, 
              z = -SFJP_SFLU_Dz-SFJP_SFPBDz/2.
Position SFPB x = 0, 
              y = +SFJP_SFPR_Py-SFJP_SFPB_Py+SFJP_SFPB_Py2, 
              z = +SFJP_SFLU_Dz+SFJP_SFPBDz/2.

Create SSBS "Aluminum plate 1"
Position SSBS x = 0., 
              y = +SFJP_SFPR_Py-SFJP_SFPB_Py+SFJP_SFPB_Py2-0.8/2.-SFJP_SSBS_Dy/2., 
              z = -SFJP_SFLU_Dz-SFJP_SFPBDz-0.2+SFJP_SSBB_Dz+SFJP_SSBS_Dx/2.
Position SSBS x = 0., 
              y = +SFJP_SFPR_Py-SFJP_SFPB_Py+SFJP_SFPB_Py2-0.8/2.-SFJP_SSBS_Dy/2., 
              z = +SFJP_SFLU_Dz+SFJP_SFPBDz+0.2-SFJP_SSBB_Dz-SFJP_SSBS_Dx/2.

Create SSBB "Aluminum plate 2"
Position SSBB x = 0., 
              y = +SFJP_SFPR_Py-SFJP_SFPB_Py+SFJP_SFPB_Py2-0.8/2.-SFJP_SSBS_Dy/2.,  
              z = -SFJP_SFLU_Dz-SFJP_SFPBDz-0.2+SFJP_SSBB_Dz/2.
Position SSBB x = 0., 
              y = +SFJP_SFPR_Py-SFJP_SFPB_Py+SFJP_SFPB_Py2-0.8/2.-SFJP_SSBS_Dy/2.,  
              z = +SFJP_SFLU_Dz+SFJP_SFPBDz+0.2-SFJP_SSBB_Dz/2.

Create SFLA "long bus"
Create SFLB "part of the bus"
Create SFLC "end of the long bus"
Create SFEB "big elbow bus"
Create SFES "small elbow bus"
wafpckLen=SFPA_wpLen/(SFPA_nssd*1.)
Do iwaf=1,8
	Do jwaf=1,iwaf
*** Top row the start
        Position SFLA x = +SFJP_SFLA_Px+SFJP_FLEX_Di*jwaf, 
                      y =  SFJP_SFLA_Py+SFJP_FLEX_Di*jwaf, 
                      z = -(SFPA_wpLen+wafpckLen)/2+iwaf*wafpckLen, 
                 AlphaZ = -SFJP_AlphaZ
        Position SFLA x = -SFJP_SFLA_Px-SFJP_FLEX_Di*jwaf, 
                      y =  SFJP_SFLA_Py+SFJP_FLEX_Di*jwaf, 
                      z = +(SFPA_wpLen+wafpckLen)/2-iwaf*wafpckLen, 
                 AlphaZ = +SFJP_AlphaZ

*** Top row the end on the connector
        Position SFLC x = +SFJP_SFLA_Px+SFJP_FLEX_Di*jwaf, 
                      y =  SFJP_SFLA_Py+SFJP_FLEX_Di*jwaf, 
 z = -(SFPA_wpLen+wafpckLen)/2.+16*wafpckLen+wafpckLen/2.+SFJP_SFLB_Dz+8.5*SFJP_SFLC_Dz-iwaf*SFJP_SFLC_Dz,
                 AlphaZ = -SFJP_AlphaZ
        Position SFLC x = -SFJP_SFLA_Px-SFJP_FLEX_Di*jwaf, 
                      y =  SFJP_SFLA_Py+SFJP_FLEX_Di*jwaf, 
 z = +(SFPA_wpLen+wafpckLen)/2.-16*wafpckLen-wafpckLen/2.-SFJP_SFLB_Dz-8.5*SFJP_SFLC_Dz+iwaf*SFJP_SFLC_Dz,
                 AlphaZ = +SFJP_AlphaZ

*** Bottom row the start
        Position SFLA x = +SFJP_SFLC_Px+SFJP_FLEX_Di*jwaf, 
                      y = -SFJP_SFLC_Py+SFJP_FLEX_Di*jwaf, 
                      z = -(SFPA_wpLen+wafpckLen)/2+(iwaf+8)*wafpckLen, 
                 AlphaZ = -SFJP_AlphaZ
        Position SFLA x = -SFJP_SFLC_Px-SFJP_FLEX_Di*jwaf, 
                      y = -SFJP_SFLC_Py+SFJP_FLEX_Di*jwaf, 
                      z = +(SFPA_wpLen+wafpckLen)/2-(iwaf+8)*wafpckLen, 
                 AlphaZ = +SFJP_AlphaZ

*** Bottom row the end on the connector
        Position SFLC x = +SFJP_SFLC_Px+SFJP_FLEX_Di*jwaf, 
                      y = -SFJP_SFLC_Py+SFJP_FLEX_Di*jwaf, 
                      z = -(SFPA_wpLen+wafpckLen)/2+16*wafpckLen+wafpckLen/2.+8.5*SFJP_SFLC_Dz-iwaf*SFJP_SFLC_Dz,
                 AlphaZ = -SFJP_AlphaZ
        Position SFLC x = -SFJP_SFLC_Px-SFJP_FLEX_Di*jwaf, 
                      y = -SFJP_SFLC_Py+SFJP_FLEX_Di*jwaf, 
                      z = +(SFPA_wpLen+wafpckLen)/2-16*wafpckLen-wafpckLen/2.-8.5*SFJP_SFLC_Dz+iwaf*SFJP_SFLC_Dz,
                 AlphaZ = +SFJP_AlphaZ
	Enddo

*** Top row : Common part 8 Times on the active area*
	Do jwaf=1,8
        Position SFLA x = +SFJP_SFLA_Px+SFJP_FLEX_Di*jwaf, 
                      y =  SFJP_SFLA_Py+SFJP_FLEX_Di*jwaf, 
                      z = -(SFPA_wpLen+wafpckLen)/2+(iwaf+8)*wafpckLen, 
                 AlphaZ = -SFJP_AlphaZ
        Position SFLA x = -SFJP_SFLA_Px-SFJP_FLEX_Di*jwaf, 
                      y =  SFJP_SFLA_Py+SFJP_FLEX_Di*jwaf, 
                      z = +(SFPA_wpLen+wafpckLen)/2-(iwaf+8)*wafpckLen, 
                 AlphaZ = +SFJP_AlphaZ
	Enddo
** Top row : Common part of the long bus on the connection baord

    Position SFLB x = +SFJP_SFLA_Px+SFJP_FLEX_Di*iwaf, 
                  y =  SFJP_SFLA_Py+SFJP_FLEX_Di*iwaf, 
                  z = -(SFPA_wpLen+wafpckLen)/2+16*wafpckLen+wafpckLen/2.+SFJP_SFLB_Dz/2., 
             AlphaZ = -SFJP_AlphaZ
    Position SFLB x = -SFJP_SFLA_Px-SFJP_FLEX_Di*iwaf, 
                  y =  SFJP_SFLA_Py+SFJP_FLEX_Di*iwaf, 
                  z = +(SFPA_wpLen+wafpckLen)/2-16*wafpckLen-wafpckLen/2.-SFJP_SFLB_Dz/2., 
             AlphaZ = +SFJP_AlphaZ

** Small elbow bus
    Position SFES x = -SFJP_SFES_Px, 
                  y = -SFJP_SFES_Py,
                  z = -(SFPA_wpLen+wafpckLen)/2+(iwaf)*wafpckLen+wafpckLen/2.-SFJP_SFEB_Dz/2.-SFJP_SFES_Pz/2., 
             AlphaZ =  57.66
    Position SFES x = +SFJP_SFES_Px, 
                  y = -SFJP_SFES_Py,
                  z = +(SFPA_wpLen+wafpckLen)/2-(iwaf)*wafpckLen-wafpckLen/2.+SFJP_SFEB_Dz/2.+SFJP_SFES_Pz/2., 
             AlphaZ = -57.66

** Big elbow bus
    Position SFEB x = -SFJP_SFEB_Px, 
                  y = -SFJP_SFEB_Py,
                  z = -(SFPA_wpLen+wafpckLen)/2+(iwaf+8)*wafpckLen+wafpckLen/2.-SFJP_SFEB_Dz/2.-SFJP_SFES_Pz/2., 
             AlphaZ =  55.35
    Position SFEB x = +SFJP_SFEB_Px, 
                  y = -SFJP_SFEB_Py,
                  z = +(SFPA_wpLen+wafpckLen)/2-(iwaf+8)*wafpckLen-wafpckLen/2.+SFJP_SFEB_Dz/2.+SFJP_SFES_Pz/2., 
             AlphaZ = -55.35

EndDo 

Endblock

*--------------------------------------------------------------------------------------------

Block SFLT is (half) the top corner of the triangular ladder skeleton
	Material Carbon
	Attribute SFLT Seen=1, Colo=1 
    Shape TRAP Dz   = SFJP_SFLU_Dz, 
               thet = 0, 
               phi  = 0, 
               h1   = SFJP_SFLU_h1, 
               bl1  = SFJP_SFLU_bl1, 
               tl1  = SFJP_SFLU_bl1-SFJP_SFLU_h1/tan(pi/5),
               alp1 = (pi/2-atan(2*tan(pi/5)))*360/(2*pi), 
               h2   = SFJP_SFLU_h1, 
               bl2  = SFJP_SFLU_bl1, 
               tl2  = SFJP_SFLU_bl1-SFJP_SFLU_h1/tan(pi/5),
               alp2 = (pi/2-atan(2*tan(pi/5)))*360/(2*pi)
Endblock 

*-------------------------------------------------------------------------------------------

Block SFLU is (half) the side corner of the triangular ladder skeleton
        Material Carbon 
        Attribute SFLU Seen=1, Colo=1   
        Shape TRAP dz   = SFJP_SFLU_Dz, 
                   thet = 0, 
                   phi  = 0, 
                   h1   = SFJP_SFLU_h1, 
                   bl1  = SFJP_SFLU_bl1, 
                   tl1  = SFJP_SFLU_bl1-SFJP_SFLU_h1/tan(27*2*pi/360),
                   alp1 = (pi/2-atan(2*tan(27*2*pi/360)))*360/(2*pi), 
                   h2   = SFJP_SFLU_h1, 
                   bl2  = SFJP_SFLU_bl1,
                   tl2  = SFJP_SFLU_bl1-SFJP_SFLU_h1/tan(27*2*pi/360), 
                   alp2 = (pi/2-atan(2*tan(27*2*pi/360)))*360/(2*pi)   
Endblock  

*======================================================================================

Block SFFK horizontal part of the ladder skeleton carbon base
	Material Carbon
	Attribute SFFK Seen=1, Colo=1
    Shape BOX dx = SFJP_SFFK_Dxe, 
              dy = SFJP_SFFK_Dy, 
              dz = SFJP_SFFK_Dz/2
Endblock

*--------------------------------------------------------------------------------------

Block SFFL titled part of the ladder skeleton carbon base
        Material Carbon
        Attribute SFFL Seen=1, Colo=1 
        Shape BOX dx = SFJP_SFFL_Dx/cos(15*pi/180), 
                  dy = SFJP_SFFK_Dy, 
                  dz = SFJP_SFFK_Dz/2 
Endblock 

*======================================================================================

Block SFKK horizontal part of the kapton film under the ladder base
	Material Mylar
	Attribute SFKK Seen=1, Colo=1
    Shape BOX dx = SFJP_SFFK_Dxz/2., 
              dy = SFJP_SFKK_Dy/2., 
              dz = SFJP_SFFK_Dz/2
Endblock

*--------------------------------------------------------------------------------------

Block SFKL titled part of the kpaton film under the ladder base
        Material Mylar
        Attribute SFKL Seen=1, Colo=1 
        Shape BOX dx = SFJP_SFFL_Dx/cos(15*pi/180), 
                  dy = SFJP_SFKK_Dy/2., 
                  dz = SFJP_SFFK_Dz/2 
Endblock 

*======================================================================================

Block SFLA is the long part of the bus cable linking the modules to the connection board 
        Material Mylar 
        Attribute SFLA Seen=1, Colo=1 
        Shape BOX dx = SFJP_SFLA_Dx/2., 
                  dy = SFJP_SFLA_Dy/2., 
                  dz = SFJP_SFLA_Dz/2. 
Endblock 

Block SFLB is the part of the long bus cable on the connection board 
        Material Mylar 
        Attribute SFLB Seen=1, Colo=1 
        Shape BOX dx = SFJP_SFLA_Dx/2., 
                  dy = SFJP_SFLA_Dy/2., 
                  dz = SFJP_SFLB_Dz/2. 
Endblock 

Block SFLC is the part of the long bus cable on the connection board up to the connector
        Material Mylar 
        Attribute SFLC Seen=1, Colo=1 
        Shape BOX dx = SFJP_SFLA_Dx/2., 
                  dy = SFJP_SFLA_Dy/2., 
                  dz = SFJP_SFLC_Dz/2. 
Endblock 

Block SFEB is the big bus elbow 
        Material Mylar 
        Attribute SFEB Seen=1, Colo=1 
        Shape BOX dx = SFJP_SFEB_Dx/2., 
                  dy = SFJP_SFLA_Dy/2., 
                  dz = SFJP_SFEB_Dz/2. 
Endblock 

Block SFES is the small bus elbow 
        Material Mylar 
        Attribute SFES Seen=1, Colo=1 
        Shape BOX dx = SFJP_SFES_Dx/2., 
                  dy = SFJP_SFLA_Dy/2., 
                  dz = SFJP_SFEB_Dz/2. 
Endblock 


*======================================================================================
Block SFAM is the mother volume of the adc board
	Material Air
	Attribute SFAM Seen=0, Colo=6
    Shape BOX dx = SFJP_SFAM_Dxe/2., 
              dy = SFJP_SFAM_Dy/2., 
              dz = SFJP_SFAM_Dz/2

	Create SFAB
    Position SFAB x = 0., 
                  y = 0., 
                  z = SFJP_SFAM_DZs/2

	Create SFAS
    Position SFAS x = (SFJP_SFAM_Dxe-SFJP_SFAM_Dxz)/2., 
                  y = 0., 
                  z = (-SFJP_SFAM_Dz+SFJP_SFAM_DZs)/2
    Position SFAS x = (-SFJP_SFAM_Dxe+SFJP_SFAM_Dxz)/2., 
                  y = 0., 
                  z = (-SFJP_SFAM_Dz+SFJP_SFAM_DZs)/2
Endblock

Block SFAB is the big volume of the adc board
	Material G10
	Attribute SFAB Seen=1, Colo=30
    Shape BOX dx = SFJP_SFAM_Dxe/2., 
              dy = SFJP_SFAM_Dy/2., 
              dz = (SFJP_SFAM_Dz-SFJP_SFAM_DZs)/2
Endblock
Block SFAS is the small volume of the adc board
	Material G10
	Attribute SFAS Seen=1, Colo=30
    Shape BOX dx = (SFJP_SFAM_Dxz)/2., 
              dy = (SFJP_SFAM_Dy)/2., 
              dz = (SFJP_SFAM_DZs)/2
Endblock
*--------------------------------------------------------------------------------------
*
Block SAPP is the mother volume of the adc board appendice
 	Material Air
	Attribute SAPP Seen=0, Colo=6
    Shape BOX dx =  (SFJP_SAPP_Dxe+2*SFJP_SAPP_Dxz)/2., 
              dy =  (SFJP_SAPP_Dy+2*SFJP_SAPP_Dxz)/2., 
              dz =  SFJP_SAPP_Dz/2.
	
	Create SAPC 
    Position SAPC x =  0., 
                  y =  0., 
                  z =  (-SFJP_SAPP_Dz+SFJP_SAPP_Dy)/2.

	Create SAPS
    Position SAPS x =  SFJP_SAPP_Dxe/2.+SFJP_SAPP_Dxz/2, 
                  y =  0, 
                  z =  0.
    Position SAPS x = -(SFJP_SAPP_Dxe/2.+SFJP_SAPP_Dxz/2), 
                  y =  0, 
                  z =  0.

	Create SAPT
    Position SAPT x =  0., 
                  y =  (SFJP_SAPP_Dy+SFJP_SAPP_Dxz)/2., 
                  z =  0.
    Position SAPT x =  0., 
                  y = -(SFJP_SAPP_Dy+SFJP_SAPP_Dxz)/2., 
                  z =  0.
Endblock

Block SAPC is the core (Epoxy) of the adc board appendice
 	Material Carbon
	Attribute SAPC Seen=1, Colo=1
    Shape BOX dx = SFJP_SAPP_Dxe/2., 
              dy = SFJP_SAPP_Dy/2., 
              dz = SFJP_SAPP_Dy/2.
Endblock

Block SAPS is the side shell (Carbon) of the adc board appendice
 	Material Carbon
	Attribute SAPS Seen=1, Colo=1
    Shape BOX dx = SFJP_SAPP_Dxz/2., 
              dy = (SFJP_SAPP_Dy+2*SFJP_SAPP_Dxz)/2., 
              dz = SFJP_SAPP_Dz/2.
Endblock

Block SAPT is the top-bottom shell (Carbon) of the adc board appendice
 	Material Carbon
	Attribute SAPT Seen=1, Colo=1
    Shape BOX dx = SFJP_SAPP_Dxe/2., 
              dy = SFJP_SAPP_Dxz/2., 
              dz = SFJP_SAPP_Dz/2.
Endblock

*======================================================================================

Block SFCO is the connection board (rectangular with Hirose connectors)
	Material G10
	Attribute SFCO Seen=1, Colo=30
    Shape BOX dx = SFJP_SFCO_Dx/2., 
              dy = SFJP_SFCO_Dy/2., 
              dz = SFJP_SFCO_Dz/2
Endblock

Block SFCM is the mother volume of the second connection board 
	Material Air
	Attribute SFCM Seen=0, Colo=6
    Shape BOX dx = SFJP_SFCM_Dx/2., 
              dy = SFJP_SFCO_Dy/2., 
              dz = SFJP_SFCO_Dz/2

        Create SFCB 
    Position SFCB x = +0.5, 
                  y =  0, 
                  z =  0
        Create SFCS 
    Position SFCS x = -1.5, 
                  y =  0, 
                  z = -SFJP_SFCO_Dz/2 + SFJP_SFCS_Dz/2
Endblock

Block SFCB is the big part of the second connection board 
	Material G10
	Attribute SFCB Seen=1, Colo=30
    Shape BOX dx = SFJP_SFCB_Dx/2., 
              dy = SFJP_SFCO_Dy/2., 
              dz = SFJP_SFCO_Dz/2
Endblock

Block SFCS is the big part of the second connection board 
	Material G10
	Attribute SFCS Seen=1, Colo=30
    Shape BOX dx = (SFJP_SFCM_Dx-SFJP_SFCB_Dx)/2., 
              dy = SFJP_SFCO_Dy/2., 
              dz = SFJP_SFCS_Dz/2
Endblock

Block SFKF is the first part of the kapton flex circuit
	Material G10
	Attribute SFKF Seen=1, Colo=30
    Shape BOX dx = SFJP_SFKF_Dx/2., 
              dy = SFJP_SFKF_Dy/2., 
              dz = SFJP_SFKF_Dz/2
Endblock

Block SFKS is the second part of the kapton flex circuit
	Material G10
	Attribute SFKS Seen=1, Colo=30
    Shape BOX dx = SFJP_SFKS_Dx/2., 
              dy = SFJP_SFKF_Dy/2., 
              dz = SFJP_SFKF_Dz/2
Endblock

Block SFPR is the ladder end inside mechanical part (prism with g10 with half density)
	Material G5
	Attribute SFPR Seen=1, Colo=4
    SHAPE PGON phi1 = -126, 
               dphi =   72, 
               NPDV =    1, 
               Nz   =    2, 
               Zi   = {0,3.7},
               Rmn  = {0,0},      
               Rmx  = {3.48,3.48}
Endblock

Block SFPB is the ladder end outside mechanical part (rectangle with g10)
	Material G10
	Attribute SFPB Seen=1, Colo=4
    SHAPE BOX dx = SFJP_SFPB_Dx/2., 
              dy = SFJP_SFPB_Dy/2., 
              dz = SFJP_SFPBDz/2.
Endblock

Block SSBS is the small part of the aluminium plate linking the ladder to the sector
	Material Aluminium
	Attribute SSBS Seen=1, Colo=1
    SHAPE BOX dx = SFJP_SSBS_Dx/2., 
              dy = SFJP_SSBS_Dy/2., 
              dz = SFJP_SSBS_Dx/2.
EndBlock

Block SSBB is the Big part of the aluminium plate linking the ladder to the sector
	Material Aluminium
	Attribute SSBB Seen=1, Colo=1
    SHAPE BOX dx = SFJP_SSBB_Dx/2., 
              dy = SFJP_SSBS_Dy/2., 
              dz = SFJP_SSBB_Dz/2.
EndBlock
*--------------------------------------------------------------------------------------
* The ring structure where the ladders are placed
* SSD mechanical support

Block SSST is the top of the small sector
	Material Aluminium
	Attribute SSST Seen=1, Colo=1
    SHAPE TUBS rmin = SFJP_SSST_Rmin, 
               rmax = SFJP_SSST_Rmax, 
               dz   = SFJP_SSST_Width/2., 
               phi1 =  SFJP_AlphaZH, 
               phi2 = (180.-SFJP_AlphaZH)
Endblock 

Block SSSS is the side of the small sector
	Material Aluminium
	Attribute SSSS Seen=1, Colo=1
    SHAPE TUBS rmin = SFJP_SSSS_Rmin, 
               rmax = SFJP_SSST_Rmin, 
               dz   = SFJP_SSSS_Width/2., 
               phi1 =  SFJP_AlphaZH, 
               phi2 = (180.-SFJP_AlphaZH)
Endblock 
Block SSRT is the top of the side rib
	Material Aluminium
	Attribute SSRT Seen=1, Colo=1
    SHAPE TUBS rmin = SFJP_SSST_Rmin, 
               rmax = SFJP_SSST_Rmax, 
               dz   = SFJP_SSST_Width/2., 
               phi1 = -SFJP_AlphaZH, 
               phi2 = +SFJP_AlphaZH
Endblock 

Block SSRS is the side of the small rib
	Material Aluminium
	Attribute SSRS Seen=1, Colo=1
    SHAPE TUBS rmin =  SFJP_SSRS_Rmin, 
               rmax =  SFJP_SSST_Rmin, 
               dz   =  SFJP_SSSS_Width/2., 
               phi1 = -SFJP_AlphaZH, 
               phi2 = +SFJP_AlphaZH
Endblock 
*----------------------------------------------------------------------
* Was in Aluminium in the previous version. Now it is the mother volume
* of different real volumes depending on the Run considered.

Block SSLB is the linking (sector to the cone) box
	Material Air
	Attribute SSLB Seen=0, Colo=6
    SHAPE BOX dx = SFJP_SSLB_Dx/2., 
              dy = SFJP_SSLB_Dy/2., 
              dz = SFJP_SSLB_Dz/2.

*    if(SFJP_Version==1) then
*      write(*,*) '*** The Run IV geometry is assumed here for the SSD mechanical support'
*      Create SSBQ
*     Position SSBQ
*    else
*      write(*,*) '*** The Run V geometry is assumed here for the SSD mechanical support'
*** The horizontal arms are centered in x,y within their mother volume.
      Create SBCH
      Position SBCH x=0, y=0, z=SFJP_SSLB_Dz/2.-SFJP_SSCR_ThD/2.
      Create SBCV
      Position SBCV x=0, y=(SFJP_SSCR_Wd+SFJP_SSCR_Vl)/2, z=SFJP_SSLB_Dz/2.-SFJP_SSCR_ThD/2.
      Position SBCV x=0, y=-(SFJP_SSCR_Wd+SFJP_SSCR_Vl)/2, z=SFJP_SSLB_Dz/2.-SFJP_SSCR_ThD/2.
      Create SBFH
      Position SBFH x=0, y=0, z=SFJP_SSLB_Dz/2.-SFJP_SSCR_ThD-SFJP_SSCR_ThA/2.
      Create SBFV
      Position SBFV x=0, y=(SFJP_SSCR_Wd+SFJP_SSCR_Vl)/2, z=SFJP_SSLB_Dz/2.-SFJP_SSCR_ThD-SFJP_SSCR_ThA/2.
      Position SBFV x=0, y=-(SFJP_SSCR_Wd+SFJP_SSCR_Vl)/2, z=SFJP_SSLB_Dz/2.-SFJP_SSCR_ThD-SFJP_SSCR_ThA/2.
*    endif

Endblock 

*---------------------------------------------------------------------
* The Run IV plate linking the SSD to the cone. Almost fully filling
* its mother volume and in aluminium.

*Block SSBQ is the run IV linking (sector to the cone) box
*	Material Aluminium
*	Attribute SSBQ Seen=1, Colo=1
*    SHAPE BOX dx = SFJP_SSBQ_Dx/2., 
*              dy = SFJP_SSBQ_Dy/2., 
*              dz = SFJP_SSBQ_Dz/2.
*Endblock 

*---------------------------------------------------------------------
* The Run V piece linking the SSD to the cone. It has a cross shape
* and splitted in two pieces made of DELRIN and aluminium.
Block SBCH is the horizontal branch of the DELRIN cross
	Material Delrin
	Attribute SBCH Seen=1, Colo=2
    SHAPE BOX dx = SFJP_SSCR_Hl/2., 
              dy = SFJP_SSCR_Wd/2., 
              dz = SFJP_SSCR_ThD/2.
Endblock 
Block SBFH is the horizontal branch of the Alumimium cross
	Material Aluminium
	Attribute SBFH Seen=1, Colo=1
    SHAPE BOX dx = SFJP_SSCR_Hl/2., 
              dy = SFJP_SSCR_Wd/2., 
              dz = SFJP_SSCR_ThA/2.
Endblock 
Block SBCV is the vertical branch of the DELRIN cross
	Material Delrin
	Attribute SBCV Seen=1, Colo=2
    SHAPE BOX dx = SFJP_SSCR_Wd/2., 
              dy = SFJP_SSCR_Vl/2., 
              dz = SFJP_SSCR_ThD/2.
Endblock 
Block SBFV is the vertical branch of the Alumimium cross
	Material Aluminium
	Attribute SBFV Seen=1, Colo=1
    SHAPE BOX dx = SFJP_SSCR_Wd/2., 
              dy = SFJP_SSCR_Vl/2., 
              dz = SFJP_SSCR_ThA/2.
Endblock 

Block SSLT is the linking (sector to the cone) tube
	Material Aluminium
	Attribute SSLT Seen=1, Colo=1
    SHAPE TUBE rmin = 0., 
               rmax = SFJP_SSLT_Rmax, 
               dz   = SFJP_SSLT_Dz/2.
Endblock 

Block SCMP is the mounting plate inserted in the cones.
	Material Aluminium
	Attribute SCMP Seen=1, Colo=1
    SHAPE BOX dx = SFJP_SCMP_Dx/2., 
              dy = SFJP_SCMP_Dy/2., 
              dz = SFJP_SCMP_Dz/2
Endblock 

Block SCVM is the mother volume of the V-shape piece
	Material Air
	Attribute SCVM Seen=0, Colo=6
    SHAPE BOX dx = SFJP_SCVM_Dx/2., 
              dy = SFJP_SCVM_Dy/2., 
              dz = SFJP_SCVM_Dz/2
	
	Create SCVB
    Position SCVB x =  0., 
                  y = -SFJP_SCVM_Dy/2.+SFJP_SCVB_Dy/2., 
                  z =  0.
    Create SCVS
    Position SCVS x =  SFJP_SCVS_Px, y = SFJP_SCVS_Py, z = 0.0, AlphaZ =  SFJP_AlphaZH
    Position SCVS x = -SFJP_SCVS_Px, y = SFJP_SCVS_Py, z = 0.0, AlphaZ = -SFJP_AlphaZH
	
Endblock 
Block SCVB is the base plate of the V-shape piece
	Material Aluminium
	Attribute SCVB Seen=1, Colo=1
    SHAPE BOX dx = SFJP_SCVM_Dx/2., 
              dy = SFJP_SCVB_Dy/2., 
              dz = SFJP_SCVM_Dz/2
Endblock 
Block SCVS is the side plate of the V-shape piece
	Material Aluminium
	Attribute SCVS Seen=1, Colo=1
    SHAPE BOX  dx = SFJP_SCVS_Dx/2.,
               dy = SFJP_SCVS_Dy/2., 
               dz = SFJP_SCVM_Dz/2
Endblock 


*******************************************************************************
                                 End
