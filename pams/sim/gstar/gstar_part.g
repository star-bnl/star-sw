***********************************************************************
*
                      Subroutine gstar_PART
*
* Remark: the particle ID numbers used by the original STAR Geant v3.15 
* can no longer be used in v3.21 because of a significant expansion of 
* idenitfied particles in the core Geant (CONS300-1). 
* Gstar uses particles codes up to 50 as defined by GEANT standards
* All remaining particles (including quarks) are unknown to GEANT, 
* BUT nevertheless, they all ARE loaded in the program KINE bank
* as geantinos with IPART=1000000+Icode
* You can redefine their properties BEFORE tracking in aGuTRAC routine
* You can decay particles provided without decay modes in aGuDCAY routine
*
* Branching ratios taken from Particle Data Book of July '94. pmj 15/2/95
***********************************************************************
+CDE,agecom,gconst.
   print *,' *             omega,phi,rho(0,+,-) and laserino defined. *'
   print *,' *             Read program comments for more information *'
*
* --------------------------------------------------------------------------

 End
* 
* --------------------------------------------------------------------------
*
* Subroutine aGuTRACK
*  check here for IPART: if>200, this particle is unknown to GEANT  
*  and needs correct tracking parameters (TrkTyp,mass,charge,tlife) 
*  to be set here.
* end
*
* --------------------------------------------------------------------------
*
* Subroutine aGuDCAY
*  If decay modes are not set, this routine will be called to decay the particle 
* end
*
* --------------------------------------------------------------------------
*
 
 
