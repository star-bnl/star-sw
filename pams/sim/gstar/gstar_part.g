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
*
  Particle omega     code=150 TrkTyp=3 mass=.782   charge=0  tlife=7.79E-23,
                     pdg=223  bratio  = { .888, .085, .021 },  
                              mode    = { 70809, 107,  809 }

*    group rho-pi and pi-pi-pi together (in practice indistinguishable)
  Particle phi       code=151 TrkTyp=3 mass=1.0194 charge=0  tlife=1.482e-22,
                     pdg=333  bratio = { .491, .343, .154, .0128, .00131 },
                              mode   = { 1112, 1016, 70809, 1701,   701  }
 
  Particle rho       code=152 TrkTyp=3 mass=.770   charge=0  tlife=4.35E-24,
                     pdg=113  bratio= { 1, }       mode= { 809, }
 
  Particle rho_plus  code=153 TrkTyp=4 mass=.767   charge=1  tlife=4.35E-24,
                     pdg=213  bratio= { 1, }       mode= { 807, }
 
  Particle rho_minus code=154 TrkTyp=4 mass=.767   charge=-1 tlife=4.35E-24,
                     pdg=-213 bratio= { 1, }       mode= { 907, }
 
*    For D+, include only K0_S + pi+ decay mode  pmj 16/2/95
* Particle D_plus    code=35  TrkTyp=4 mass=1.869  charge=1 tlife=1.057e-12,
*                    pdg=411  bratio= { 1, }       mode= { 1608, }
 
*    For D0, include only  K+ + pi- decay mode   pmj 16/2/95
* Particle D0        code=37  TrkTyp=3 mass=1.865  charge=0 tlife=0.415e-12,
*                    pdg=421  bratio= { 1, }       mode= { 1208, }

  Particle LASERINO  code=170         TrkTyp=6 mass=0     charge=0 tlife=big
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
 
 
