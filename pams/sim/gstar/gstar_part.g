* $Id: gstar_part.g,v 1.9 2003/12/19 17:51:11 potekhin Exp $
*
* $Log: gstar_part.g,v $
* Revision 1.9  2003/12/19 17:51:11  potekhin
* Added the CVS tag, and repeating the comment from the previous check-in:
* Including the straglet nomenclature developed by Ron, with a few corresctions.
* The PDG codes assigned are essentialy our declared GEANT codes for
* simplicity.
*
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
 
  Particle K0        code=155 TrkTyp=4 mass=.4977  charge=0  tlife= 1.e-24,
                     pdg=311  bratio= { .5, .5}    mode= { 16, 10 }

  Particle anti_K0   code=156 TrkTyp=4 mass=.4977  charge=0  tlife= 1.e-24,
                     pdg=-311  bratio= { .5, .5}    mode= { 16, 10 }
 
*    For D+, include only K0_S + pi+ decay mode  pmj 16/2/95
* Particle D_plus    code=35  TrkTyp=4 mass=1.869  charge=1 tlife=1.057e-12,
*                    pdg=411  bratio= { 1, }       mode= { 1608, }
 
*    For D0, include only  K+ + pi- decay mode   pmj 16/2/95
* Particle D0        code=37  TrkTyp=3 mass=1.865  charge=0 tlife=0.415e-12,
*                    pdg=421  bratio= { 1, }       mode= { 1208, }


* Heavy flavor embedding studies -- July 2003, M.Potekhin

  Particle Jpsi      code=160 TrkTyp=4 mass=3.096  charge=0  tlife=7.48e-21,
                     pdg=443  bratio= { 1, }       mode= { 203, }

  Particle Upsilon   code=161 TrkTyp=4 mass=9.460   charge=0  tlife=1.254e-20,
                     pdg=553  bratio= { 1, }       mode= { 203, }

  Particle LASERINO  code=170         TrkTyp=6 mass=0     charge=0 tlife=big


* the Stranglet World, authored by Ron Longacre and remastered by M.Potekhin
* As per Ron Longacre's recommendation, we assign PDG codes which do not
* overlap with existing ones, and do not stick to the rigorous definitions.

  Particle H0       code=801  TrkTyp=3 mass=2.21   charge=0 tlife=1.335E-10,
                     pdg=801  bratio = { 0.89, 0.11 }, mode   = { 1000014021, 1009014018 }
 
  Particle H0P      code=802  TrkTyp=3 mass=3.117  charge=0 tlife=1.335E-10,
                     pdg=802  bratio= { 1, } mode= { 1013014021,}
 
  Particle H3L2     code=803 TrkTyp=4 mass=3.117  charge=1 tlife=1.335E-10,
                     pdg=803  bratio= { 1, } mode= { 1014014021,}
 
  Particle H5L3     code=804  TrkTyp=4 mass=5.09   charge=1 tlife=1.335E-10,
                     pdg=804  bratio= { .333,.333,.334 } mode= { 1000046801, 1000045802, 1013013803 }
 
  Particle H6L4     code=805  TrkTyp=4 mass=6.17  charge=1  tlife=1.335E-10,
                     pdg=805  bratio= { 1, } mode= { 1009014804,}
 
  Particle H7L5     code=806  TrkTyp=4 mass=7.244  charge=1 tlife=1.335E-10,
                     pdg=806  bratio= { 1, } mode= { 1014021804,}
 
  Particle PIPN     code=807  TrkTyp=3 mass=2.052  charge=0  tlife=9.00E-24,
                     pdg=807  bratio= { 1, } mode= { 1009013014,}
 
  Particle PIPP     code=808  TrkTyp=4 mass=2.052  charge=1  tlife=9.00E-24,
                     pdg=808  bratio= { 1, } mode= { 1009014014,}
 
  Particle H9L6     code=809  TrkTyp=4 mass=9.297  charge=1 tlife=1.335E-10,
                     pdg=809  bratio= { 1, } mode= { 1000806807,}
 
  Particle He9L6    code=810  TrkTyp=4 mass=9.297  charge=2 tlife=1.335E-10,
                     pdg=810  bratio= { 1, } mode= { 1000806808,}
 
  Particle SIGL     code=811  TrkTyp=4 mass=2.315  charge=-1 tlife=9.00E-24,
                     pdg=811  bratio= { 1, } mode= { 1000018021, }
 
  Particle H03      code=812  TrkTyp=3 mass=6.405  charge=0 tlife=1.000E-08,
                     pdg=812  bratio= { 1, } mode= { 1045801811, }
 
  Particle H0H0     code=813  TrkTyp=3 mass=4.4201 charge=0  tlife=9.00E-24,
                     pdg=813  bratio= { 1, } mode= { 1000801801, }
 
  Particle H03H03   code=814  TrkTyp=3 mass=12.8101 charge=0 tlife=9.00E-24,
                     pdg=814  bratio= { 1, } mode= { 1000812812, }
 
  Particle 2PIM     code=815  TrkTyp=4 mass=.291   charge=-2 tlife=9.00E-24,
                     pdg=815  bratio= { 1, } mode= { 1000009009, }
 
  Particle 2PIP     code=816  TrkTyp=4 mass=.330   charge=2  tlife=9.00E-24,
                     pdg=816  bratio= { 1, } mode= { 1000008008, }
 
  Particle PIPM     code=817  TrkTyp=3 mass=.287   charge=0  tlife=9.00E-24,
                     pdg=817  bratio= { 1, } mode= { 1000008009, }
 
  Particle  PP      code=818  TrkTyp=4 mass=1.884  charge=2  tlife=9.00E-24,
                     pdg=818  bratio= { 1, } mode= { 1000014014, }
 
  Particle 3PIM     code=819  TrkTyp=4 mass=.436  charge=-3  tlife=9.00E-24,
                     pdg=819  bratio= { 1, } mode= { 1009009009, }
 
  Particle 3PIMT    code=820  TrkTyp=4 mass=.41871 charge=-3 tlife=9.00E-24,
                     pdg=820  bratio= { 1, } mode= { 1009009009, }
 
  Particle 3PIP     code=821  TrkTyp=4 mass=.490  charge=3   tlife=9.00E-24,
                     pdg=821  bratio= { 1, } mode= { 1008008008, }
 
  Particle 4PIM     code=822  TrkTyp=4 mass=.583  charge=-4  tlife=9.00E-24,
                     pdg=822  bratio= { 1, } mode= { 1000815815, }

  Particle 5PIM     code=823  TrkTyp=4 mass=.728  charge=-5  tlife=9.00E-24,
                     pdg=823  bratio= { 1, } mode= { 1000815819, }

  Particle 6PIM     code=824  TrkTyp=4 mass=.873  charge=-6  tlife=9.00E-24,
                     pdg=824  bratio= { 1, } mode= { 1000819819, }
 
  Particle 6PIMT    code=825  TrkTyp=4 mass=.83742 charge=-6 tlife=9.00E-24,
                     pdg=825  bratio= { 1, } mode= { 1000820820, }
 
  Particle 5PIP     code=826  TrkTyp=4 mass=.821  charge=5   tlife=9.00E-24,
                     pdg=826  bratio= { 1, } mode= { 1000816821, }
 
  Particle H02L     code=827 TrkTyp=3 mass=5.515  charge=0 tlife=1.335E-10,
                     pdg=827  bratio= { 1, } mode= { 1009014813, }
 
  Particle H02LN    code=828 TrkTyp=3 mass=6.446  charge=0 tlife=1.335E-10,
                     pdg=828 bratio= { 1, } mode= { 1009045813, }
 
  Particle H6L5     code=829 TrkTyp=4 mass=6.446  charge=1 tlife=1.335E-10,
                     pdg=829 bratio= { 1, } mode= { 1009818813, }
 
  Particle H02LCM   code=830 TrkTyp=4 mass=6.779  charge=-1 tlife=1.34E-10,
                     pdg=830 bratio= { 1, } mode= { 1009801813, }
 
  Particle H02LC0   code=831 TrkTyp=3 mass=6.779  charge=0 tlife=1.335E-10,
                     pdg=831 bratio= { 1, } mode= { 1007801813, }
 
  Particle H04SM5   code=832 TrkTyp=4 mass=14.516 charge=-5 tlife=1.34E-10,
                     pdg=832 bratio= { 1, } mode= { 1013814823, }
 
 Particle H04SM4CM  code=833 TrkTyp=4 mass=14.640 charge=-5 tlife=1.34E-10,
                     pdg=833 bratio= { 1, } mode= { 1021814822, }
 
 Particle H03LSM5CM code=834 TrkTyp=4 mass=14.738 charge=-6 tlife=1.34E-10,
                     pdg=834 bratio= { 1, } mode= { 1021814823, }
 
 Particle B13SP5L6C02 code=835 TrkTyp=4 mass=14.961 charge=5  tlife=1.335E-10,
                     pdg=835 bratio= { 1, } mode= { 1022814826, }
 
 Particle H13SPL10C02 code=836 TrkTyp=4 mass=14.609 charge=1 tlife=1.335E-10,
                     pdg=836 bratio= { 1, } mode= { 1023814816, }
 
 Particle H05SMCM2  code=837 TrkTyp=4 mass=14.612 charge=-3 tlife=1.34E-10,
                     pdg=837  bratio= { 1, } mode= { 1023814815, }
 
 Particle H03LSM4CM2 code=838 TrkTyp=4 mass=14.874 charge=-6 tlife=1.34E-10,
                     pdg=838  bratio= { 1, } mode= { 1023814823, }
 
 Particle H03SM5CM2 code=839 TrkTyp=4 mass=14.970 charge=-7 tlife=1.34E-10,
                     pdg=839  bratio= { 1, } mode= { 1023814825, }
 
 Particle BE13L10C03 code=840 TrkTyp=4 mass=15.105 charge=4 tlife=1.000E-09,
                     pdg=840  bratio= { 1, } mode= { 1023814826, }
 
 Particle H05LC0OM  code=841 TrkTyp=4 mass=14.771 charge=-1 tlife=1.34E-10,
                     pdg=841  bratio= { 1, } mode= { 1024814817, }
 
 Particle H05LCMOM  code=842 TrkTyp=4 mass=14.773 charge=-2 tlife=1.34E-10,
                     pdg=842  bratio= { 1, } mode= { 1009024814, }
 
 Particle H03SM4CM3 code=843 TrkTyp=4 mass=15.118 charge=-7 tlife=1.00E-09,
                     pdg=843  bratio= { 1, } mode= { 1023814824, }
 
 Particle H03SM3CM4 code=844  TrkTyp=4 mass=15.281 charge=-7 tlife=1.00E-07,
                     pdg=844  bratio= { 1, } mode= { 1023814824, }
 
 



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
      Subroutine aGuDCAY
+CDE,gctrak,gckine,gcking.
*
*  If decay modes are not set, this routine will be called to decay the particle
*
*  What follows is a snippet of code to explore a few variables the
* user might need:

*      real P_PART(4)

*      DO I=1,4
*         P_PART(I)=VECT(I+3)*VECT(7)
*      ENDDO

*      write(*,*) 'ivert, ipart: ', ivert, ipart
*      write(*,*) 'p1, p2, p3: ', P_PART(1), P_PART(2), P_PART(3)

*      NGKINE = NGKINE + 1
*      DO I = 1, 4
*          GKIN(I,NGKINE) = 1.0
*      END DO
*     GKIN(5,NGKINE)   = 2
*     TOFD(NGKINE)     = 0.
* copy over the decay position:
*     DO I = 1, 3
*         GPOS(I,NGKINE) = VECT(I)
*     ENDDO

      end
*
* --------------------------------------------------------------------------
*
 
 
