* $Id: gstar_part.g,v 1.50 2014/07/17 15:17:44 jwebb Exp $
*
* $Log: gstar_part.g,v $
* Revision 1.50  2014/07/17 15:17:44  jwebb
* Added (Xi0Xi-) dibaryon with hacked decay modes... should be 4 body phase space, but G3 doesn't support easily.
*
* Revision 1.49  2014/07/16 21:30:40  jwebb
* Added J/Psi --> mu+m- 100% branching ratio with geant ID 168.
*
* Revision 1.48  2014/07/09 18:29:28  jwebb
* Added (pnXi-) "dibaryon" and it's antiparticle to gstar_part.g.  Required
* modification of gdecay.F in order to accept arbitrary geant IDs for decay
* daughters.
*
* Revision 1.47  2014/07/07 20:10:04  jwebb
* Forgot to specify particle tracking type (used to determine which routine
* is used to handle physics processes, e.g. decays, for the particle).
*
* Revision 1.46  2014/06/25 14:19:25  jwebb
* Added psi prime --> e+e-
*
* Revision 1.45  2014/01/29 16:21:43  jwebb
* Added D_star_plus (minus) --> D0 (bar) pi+ (-) 100% BR
*
* Revision 1.44  2014/01/28 15:12:57  jwebb
* Corrected charge states for two hypernuclei
*
* Revision 1.43  2013/07/22 20:45:56  jwebb
* Changed lifetime... previous values were the width of the particle in GeV
* not the time in seconds.
*
* Revision 1.42  2013/06/19 22:17:48  jwebb
* Improved properties of lambda xi hyperon.
*
* Revision 1.41  2013/06/19 22:15:27  jwebb
* Fixed decay modes.
*
* Revision 1.40  2013/06/13 18:44:03  jwebb
* Added Lambda Xi dibaryon.
*
* Revision 1.39  2013/04/08 19:57:20  jwebb
* Updated mass and lifetime of the phi to more recent PDG values.
*
* Revision 1.38  2013/03/14 18:27:29  jwebb
* Added pi0    --> e+e- gamma 100% gid=10007
* Added K0long --> nu e- pi+  100% gid=10010
* Added K0long --> nu e+ pi-  100% gid=10110
*
* http://www.star.bnl.gov/rt2/Ticket/Display.html?id=2549
*
* Revision 1.37  2013/02/04 20:41:15  jwebb
* Update to H-Dibaryon mass, to provide sufficient CMS energy for phase space
* decay.
*
* Revision 1.36  2013/01/31 18:21:50  jwebb
* Updated StarClassLibrary and gstar_part.g to add the H Dibaryon.
*
* Revision 1.35  2012/06/25 16:02:09  jwebb
* Added Xi0(1530).
*
* Revision 1.34  2011/08/29 20:30:54  jwebb
* Added K+ --> e+ pi0 nu and K- --> e- pi0 nu to satisfy an emedding
* request http://drupal.star.bnl.gov/STAR/starsimrequests/2010/aug/31/ke3-pp-200-gev-run9
*
* Also added the other top 6 decay modes as 10011 -- 15011 and 10012 -- 15012.
*
* 10011 : K+ --> m+ nu
* 11011 : K+ --> pi+ pi0
* 12011 : K+ --> pi+ pi+ pi-
* 13011 : K+ --> e+ nu pi0
* 14011 : K+ --> m+ nu pi0
* 15011 : K+ --> pi+ pi0 pi0
*
* and similar for K- decay modes
*
* Revision 1.33  2011/08/12 15:33:40  jwebb
*
* Added anti-hypertriton.  Mapped hyper-triton and anti-hyper-triton to
* geant IDs 6[12]053 and 6[12]054, with two decay modes:
*
*     H3(lambda) --> He3 pi-    61053    antiparticle=61054
*     H3(lambda) --> d p pi-    62053    antiparticle=62054
*
* Added antideuteron (gid=53) and antitriton (gid=54) to enable inclusion
* in hypertriton decays.
*
* Revision 1.32  2011/03/15 22:39:54  jwebb
* (1) Corrected mistakes in the Omega_plus, Omega_minus and XiMinus and XiPlus
*     definitions of particles.
* (2) Removed pdg ID from all embedding versions of particles, as these
*     conflict with pythia simulations.
*
* Revision 1.31  2010/11/18 15:12:04  jwebb
* Added omega(728) w/ 100% decay to e+e-.
*
* Revision 1.30  2010/09/07 15:09:21  jwebb
* RT 1999: Adding XiMinus, XiMinusBar, XiZero and XiZeroBar
*
* Revision 1.29  2010/09/02 19:25:53  jwebb
* Increased the precision of the lambda and lambdabar masses in gstar_part.g,
* requested by Xianglei Zhu.
*
* Revision 1.28  2010/07/20 15:32:43  jwebb
* Correcting gstar_part.g for problems discussed here:
* http://www.star.bnl.gov/HyperNews-star/protected/get/heavy/2819/1/2.html
*
* 1) Corrected the D*- decay
* 2) Did not change branching ratios, as this would adversly impact standard
*    simulations.
* 3) The mass of the D*+/- and D0 now have full precision quoted in PDG (2009).
*
* Revision 1.27  2010/06/24 22:37:39  jwebb
* Say no to Mortran90.
*
* Revision 1.26  2010/06/24 18:35:20  jwebb
* Corrected mistake in Omega+- definition and forced daughter lamda to decay to specific channel.
*
* Revision 1.25  2010/06/24 18:05:12  jwebb
* Update of gstar_part.g for embedding (ticket 1928).  Added test macro.
*
* Revision 1.24  2010/05/05 21:54:17  perev
* K0S->Pi+ Pi- only added
*
* Revision 1.23  2010/04/06 14:16:25  jwebb
* Redefined the geant ID of the lambda(1520) from 706 to 995, to make
* consistent with usage in embedding group.
*
* Revision 1.22  2010/03/19 22:10:47  perev
* Lambda1520 added (Hiroshi request)
*
* Revision 1.21  2009/11/10 19:54:54  fisyak
* pams Cleanup
*
* Revision 1.20  2009/01/10 02:45:04  perev
* Hypertriton added
*
* Revision 1.19  2007/05/18 16:37:39  potekhin
* a) shifted the PDG code for the Dalitz particle by adding a large
* integer, thus hoping to resolve the previous clash
* b) Introduced 3 particles corresponding to 2 states of the Upsilon,
* albeit with decays exclusively into th mu+/mu- channel
*
* Revision 1.18  2007/05/15 21:20:42  potekhin
* The introduction of the Dalitz particle
* had unforeseen consequence in that all
* pi0's in Pythia simulations were counted
* as Dalitz, which we clearly don't want.
* I disable this feature until a better
* solution is found.
*
* Revision 1.17  2007/04/02 18:09:27  potekhin
* For the pi0 "embedding" studies, we need to enrich the
* pi0 sample with Dalitz decays to enhance statistics.
* To this end, we have created a particle "DALITZ", which
* is identical to pi0 except that it goes into gamma gamma
* and Dalitz in 50%/50% proprotion
*
* Revision 1.16  2007/03/05 20:53:57  potekhin
* Corrected the B decay definitions as per Mark Heinz
*
* Revision 1.15  2007/03/02 21:13:16  potekhin
* Corrected wrong masses for B-mesons, and adding the
* omitted comment about moving the stranglet definitions
* into a separate file in the same directory (stranglet.txt).
*
* Revision 1.14  2007/03/02 19:41:57  potekhin
* Added D0bar, Dstar+-0 and 0 bar, B+-0 and 0bar
*
* Revision 1.13  2007/02/06 16:48:21  potekhin
* Bring the particle definitions up to date,
* with new Upsilon levels and strange lambda plus
*
* Revision 1.12  2004/12/20 18:04:22  potekhin
* Corrected a typo (missing comma)
*
* Revision 1.11  2004/12/20 17:55:31  potekhin
* Requested by Sevil and heavily used in embedding,
* sigma(1385) family has been added to the particle list
*
* Revision 1.10  2004/06/06 14:08:35  fisyak
* make LASERINO charged
*
* Revision 1.9  2003/12/19 17:51:11  potekhin
* Added the CVS tag, and repeating the comment from the previous check-in:
* Including the straglet nomenclature developed by Ron, with a few corresctions.
* The PDG codes assigned are essentialy our declared GEANT codes for
* simplicity.
*
***********************************************************************
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
*

MODULE gstar_part Is the STAR Particle Database

   CREATED A long time ago
   AUTHOR  Several
   
+CDE,agecom,gconst,gcunit.

   Integer UNDEFINED / 0       /
   Real    STABLE    / 1.0E+15 /

   !-- Flags which specify the propagator used for the particle
   Integer kGtGAMA / 1 /! A photon
   Integer kGtELEC / 2 /! An electron or positron
   Integer kGtNEUT / 3 /! A neutral hadron
   Integer kGtHADR / 4 /! A charged hadron
   Integer kGtMUON / 5 /! A muon
   Integer kGtNINO / 6 /! A geantino
   Integer kGtHION / 8 /! A heavy ion
   Integer kGtCKOV / 7 /! A cherenkov photon (note mistake in geant manual)

   Real *8 HBAR / 6.58211928E-16 / "eV * s"

   Integer, Parameter :: nw = 3     ! Number of user words for gspart direct calls
   Real               :: uw(nw)     ! buffer


* For meaning of paramters see G3 manual
* http://wwwasdoc.web.cern.ch/wwwasdoc/geant_html3/node72.html#SECTION024000000000000000000000

* --------------------------------------------------------------------------
*
* Particle ID 54 is reserved for anti-He3 to enable anti-hypertriton decay mode






* --------------------------------------------------------------------------
*
* redefinition of pre-coded D's with the desired branchings
*
* (K-)+2(pi+) decay ;  Alt.  K0_S + pi+   pdg=411  bratio= { 1, }       mode= { 1608, }
  Particle D_plus    code=35  TrkTyp=4 mass=1.869  charge=1 tlife=1.057e-12,
                     pdg=411  bratio= { 1, }       mode= { 120808, }

* (K+)+2(pi-) decay
  Particle D_minus   code=36  TrkTyp=4 mass=1.869  charge=1 tlife=1.057e-12,
                     pdg=-411  bratio= { 1, }       mode= { 110909, }

 
* (K-)+(pi+) decay
  Particle D0        code=37  TrkTyp=3 mass=1.86484  charge=0 tlife=0.415e-12,
                     pdg=421  bratio= { 1, }       mode= { 1208, }

* (K+)+(pi-) decay
  Particle D0_bar    code=38  TrkTyp=3 mass=1.86484  charge=0 tlife=0.415e-12,
                     pdg=-421  bratio= { 1, }      mode= { 1109, }

************************************************************************************ 
* D0 and pi+, D+ and pi0
  Particle D_star_plus   code=60  TrkTyp=4 mass=2.01027  charge=1  tlife=6.86e-21,
                         pdg=413  bratio= { 0.69, 0.31}       mode= { 3708, 3507 }

* decay into D0 and pi-, D- and pi0
*
  Particle D_star_minus  code=61  TrkTyp=4 mass=2.01027  charge=-1 tlife=6.86e-21,
                         pdg=-413 bratio= { 0.69, 0.31}       mode= { 3809, 3607 }

* decay into D0 and pi0, D0 and gamma
  Particle D_star_0       code=62  TrkTyp=4 mass=2.007  charge=0 tlife=3.13e-22,
                          pdg=423  bratio= { 0.62, 0.38}       mode= { 3507, 3501}

* D0_bar and pi0, D0_bar and gamma
  Particle D_star_0_bar   code=63  TrkTyp=4 mass=2.007  charge=0 tlife=3.13e-22,
                          pdg=-423 bratio= { 0.62, 0.38}       mode= { 3807, 3801}
************************************************************************************ 

* decay into D_star_0_bar, positron and neutrino, or D0_bar, positron and neutrino
  Particle B_plus    code=70         TrkTyp=4 mass=5.279  charge=1 tlife=1.671e-12,
                     pdg=521  bratio= { 0.75, 0.25}  mode= { 630204, 380204}

* decay into D_star_0, electron and neutrino, or D0, electron and neutrino
  Particle B_minus   code=71         TrkTyp=4 mass=5.279 charge=-1 tlife=1.671e-12,
                     pdg=-521 bratio= { 0.75, 0.25}  mode= { 620304, 370304}

* decay into D_star_minus, positron and neutrino, or D-, positron and neutrino
  Particle B0        code=72         TrkTyp=4 mass=5.279  charge=0 tlife=1.536e-12,
                     pdg=511  bratio= { 0.72, 0.28}  mode= { 610204, 360204}

* decay into D_star_plus,  electron and neutrino, or D+, electron and neutrino
  Particle B0_bar    code=73         TrkTyp=4 mass=5.279  charge=0 tlife=1.536e-12,
                     pdg=-511 bratio= { 0.72, 0.28}  mode= { 600304, 350304}

************************************************************************************ 
* special pi0 decay: 50% normal, 50% dalitz
  Particle Dalitz    code=149        TrkTyp=4 mass=0.135  charge=0 tlife=8.4e-17,
                     pdg=100111 bratio= { 0.5, 0.5}  mode= { 101, 10203}

************************************************************************************ 
  Particle omega     code=150 TrkTyp=3 mass=.782   charge=0  tlife=7.79E-23,
                     pdg=223  bratio  = { .888, .085, .021 },  
                              mode    = { 70809, 107,  809 }

* group rho-pi and pi-pi-pi together (in practice indistinguishable)
  Particle phi       code=151 TrkTyp=3 mass=1.01946 charge=0  tlife=1.545e-22,
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



************************************************************************************ 
* Heavy flavor studies -- July 2003 - May 2007, M.Potekhin
  Particle Jpsi       code=160 TrkTyp=4 mass=3.096  charge=0  tlife=7.48e-21,
                      pdg=443  bratio= { 1, }       mode= { 203, }

  Particle Jpsi_mu    code=168 TrkTyp=4 mass=3.096  charge=0  tlife=7.48e-21,
                      pdg=0    bratio= { 1, }       mode= { 0506, }

  Particle Upsilon    code=161 TrkTyp=4 mass=9.460  charge=0  tlife=1.254e-20,
                      pdg=553  bratio= { 1, }       mode= { 203, }

  Particle Ups2S      code=162 TrkTyp=4 mass=10.023 charge=0  tlife=1.545e-20,
                      pdg=100553  bratio= { 1, }    mode= { 203, }

  Particle Ups3S      code=163 TrkTyp=4 mass=10.355 charge=0  tlife=2.556e-20,
                      pdg=200553  bratio= { 1, }    mode= { 203, }

* have to use different PDG codes to avoid a clash

  Particle Upsilon_mu code=164 TrkTyp=4 mass=9.460  charge=0  tlife=1.254e-20,
                      pdg=300553  bratio= { 1, }    mode= { 506, }

  Particle Ups2S_mu   code=165 TrkTyp=4 mass=10.023 charge=0  tlife=1.545e-20,
                      pdg=400553  bratio= { 1, }    mode= { 506, }

  Particle Ups3S_mu   code=166 TrkTyp=4 mass=10.355 charge=0  tlife=2.556e-20,
                      pdg=500553  bratio= { 1, }    mode= { 506, }

  Particle psi2s_ee   "Psi(2S)-->e+e-" code=167,  TrkTyp=4,
                      mass=3.68609 charge=0 tlife=hbar/2380.0,
                      pdg=100443 bratio={ 1, } mode={ 0203, }

************************************************************************************ 

  Particle LASERINO  code=170         TrkTyp=6 mass=0     charge=1 tlife=big

  Particle chasrino  code=171         TrkTyp=6 mass=0     charge=1 tlife=big

************************************************************************************ 
  Particle Lac2      code=207       TrkTyp=3 mass=2.285   charge=1  tlife=0.206e-12,
                     pdg=4122       bratio= { 1, }       mode= { 141208, }


  Particle S1385plus     code=701   TrkTyp=4 mass=1.3828  charge=1   tlife=1.84e-23,
                         pdg=3224   bratio= { 1, }       mode= { 1808, }

  Particle S1385minus    code=702   TrkTyp=4 mass=1.3872  charge=-1  tlife=1.67e-23,
                         pdg=3114   bratio= { 1, }       mode= { 1809, }

  Particle S1385barplus  code=703   TrkTyp=4 mass=1.3872  charge=-1  tlife=1.67e-23,
                         pdg=-3114  bratio= { 1, }       mode= { 2608, }

  Particle S1385barminus code=704   TrkTyp=4 mass=1.3828  charge=1   tlife=1.84e-23,
                         pdg=-3224  bratio= { 1, }       mode= { 2609, }

  Particle Monopole      code=800       TrkTyp=9 mass=1.0    charge=0 tlife=big

***2009***************************************************************************** 
*  requested by Jinhui
*  Hypertriton decay into helium 3 and minus pion
  Particle H3_Lambda     code = 52  TrkTyp=8, mass = 2.991, charge=1, tlife=2.6320e-10, 
                         pdg=0      bratio = {1,}        mode={4909,}
***2010***************************************************************************** 
*  requested by Hiroshi
  Particle StLambda1520  code=995  TrkTyp=3  mass=1.5195  charge=0   tlife=4.22e-23,
                         pdg=0     bratio= {1,}           mode= {1412,}

  Particle StLambdaBar1520  code=996  TrkTyp=3  mass=1.5195  charge=0   tlife=4.22e-23,
                         pdg=0     bratio= {1,}           mode= {1511,}

  Particle StK0s         code=707  TrkTyp=3  mass= 0.497671 charge=0 tlife=8.922E-11,
                         pdg=0     bratio= {1,}           mode= {809,}





*************************************************************************************
* Definition of 'nonstandard' particles are assigned GID 40000+
* Redefinition of 'standard' particles are assigned GID 10000+


*************************************************************************************
*  Pion0 GID=6

  Particle Dalitz    code=10007       TrkTyp=4 mass=0.135  charge=0 tlife=8.4e-17,
                     pdg=100111 bratio= { 1.0,}  mode= {10203,}

*  KAON+ GID=11

   PARTICLE KAON_PLUS code=10011 pdg=0 mass=0.4937E+00 charge=+1 tlife=0.12370E-07 ,
                      trktyp=4 bratio={1.0,} mode={0504,}

   PARTICLE KAON_PLUS code=11011 pdg=0 mass=0.4937E+00 charge=+1 tlife=0.12370E-07 ,
                      trktyp=4 bratio={1.0,} mode={0807,}

   PARTICLE KAON_PLUS code=12011 pdg=0 mass=0.4937E+00 charge=+1 tlife=0.12370E-07 ,
                      trktyp=4 bratio={1.0,} mode={080809,}

   PARTICLE KAON_PLUS code=13011 pdg=0 mass=0.4937E+00 charge=+1 tlife=0.12370E-07 ,
                      trktyp=4 bratio={1.0,} mode={020407,}

   PARTICLE KAON_PLUS code=14011 pdg=0 mass=0.4937E+00 charge=+1 tlife=0.12370E-07 ,
                      trktyp=4 bratio={1.0,} mode={050407,}

   PARTICLE KAON_PLUS code=15011 pdg=0 mass=0.4937E+00 charge=+1 tlife=0.12370E-07 ,
                      trktyp=4 bratio={1.0,} mode={080707,}

   PARTICLE KAON_MINUS code=10012 pdg=0 mass=0.4937E+00 charge=-1 tlife=0.12370E-07 ,
                      trktyp=4 bratio={1.0,} mode={0604,}

   PARTICLE KAON_MINUS code=11012 pdg=0 mass=0.4937E+00 charge=-1 tlife=0.12370E-07 ,
                      trktyp=4 bratio={1.0,} mode={0907,}

   PARTICLE KAON_MINUS code=12012 pdg=0 mass=0.4937E+00 charge=-1 tlife=0.12370E-07 ,
                      trktyp=4 bratio={1.0,} mode={090908,}

   PARTICLE KAON_MINUS code=13012 pdg=0 mass=0.4937E+00 charge=-1 tlife=0.12370E-07 ,
                      trktyp=4 bratio={1.0,} mode={030407,}

   PARTICLE KAON_MINUS code=14012 pdg=0 mass=0.4937E+00 charge=-1 tlife=0.12370E-07 ,
                      trktyp=4 bratio={1.0,} mode={060407,}

   PARTICLE KAON_MINUS code=15012 pdg=0 mass=0.4937E+00 charge=-1 tlife=0.12370E-07 ,
                      trktyp=4 bratio={1.0,} mode={090707,}


* KAON 0 LONG GID=10

   Particle K0L_pi_ele_nu code=10010 pdg=0 mass=0.4977E+00 charge=0 tlife=0.51700E-07 ,
                       trktyp=3 bratio={1.0,} mode={040803,}
   Particle K0L_pi_pos_nu code=10110 pdg=0 mass=0.4977E+00 charge=0 tlife=0.51700E-07 ,
                       trktyp=3 bratio={1.0,} mode={040902,}


* D_star_plus, D_star_minus 100% BR to pi+ D0 or pi- D0bar

  PARTICLE D_star_plus code=10060 TrkTyp=4 mass=2.01027  charge=1  tlife=6.86e-21,
                       pdg=413  bratio= { 1.00, }       mode= { 3708,  }

  PARTICLE D_star_minus  code=10061  TrkTyp=4 mass=2.01027  charge=-1 tlife=6.86e-21,
                         pdg=-413 bratio= { 1.0, }       mode= { 3809,  }



  """ Omega_plus --> lambdaBar K+ substitutes for standard GID 32 """
  PARTICLE Omega_plus  code=40001 pdg=0     ,
                  mass=1.67245              ,
                  tlife=0.0821E-9           ,
                  charge=+1                 ,
                  trktyp=3                  ,
                  bratio={1.0,}             ,
                  mode={9711,} 
 
        PARTICLE _lam_to_p_piminus_ code=98 ,
             pdg     = 0                    ,
             mass    = 1.115683             ,  
             tlife   = 0.26320E-9           ,
             charge  = 0                    ,
             bratio  = {1.000,}             ,
             mode    = {1409,}              ,
             trktyp  = kGtNEUT
 
  """ Omega_minus --> lambda + K- substitutes for standard GID 24 """
  PARTICLE Omega_minus code=40002 pdg=0     ,
                  mass=1.67245              ,
                  tlife=0.0821E-9           ,
                  charge=-1                 ,
                  trktyp=3                  ,
                  bratio={1.0,}             ,
                  mode={9812,} 

        PARTICLE _lam_to_pb_piplus_ code=97 ,
             pdg     = 0                    ,
             mass    = 1.115683             ,  
             tlife   = 0.26320E-9           ,
             charge  = 0                    ,
             bratio  = {1.000,}             ,
             mode    = {1508,}              ,
             trktyp  = kGtNEUT



  Particle XiMinus    code   = 40003     pdg   = 0         ,
                      trktyp = kGtHADR   mass  = 1.32171   ,
                      charge = -1        tlife = 0.1639E-9 ,
                      bratio = {1,}      mode  = {9809,}           

  Particle XiMinusBar code   = 40004     pdg   = 0         ,
                      trktyp = kGtHADR   mass  = 1.32171   ,
                      charge = +1        tlife = 0.1639E-9 ,
                      bratio = {1,}      mode  = {9708,}           
                     
  Particle XiZero     code   = 40005     pdg   = 0         ,
                      trktyp = kGtNEUT   mass  = 1.31486   ,
                      charge = 0         tlife = 0.290E-9  ,
                      bratio = {1,}      mode  = {9807,}

  Particle XiZeroBar  code   = 40006     pdg   = 0         ,
                      trktyp = kGtNEUT   mass  = 1.31486   ,
                      charge = 0         tlife = 0.290E-9  ,
                      bratio = {1,}      mode  = {9707,}


  PARTICLE XiZero1530 code   = 40007     pdg   = 3324      ,
                      trktyp = kGtNeut   mass  = 1.53180   ,
                      charge = 0         tlife = 0         ,
                      bratio = {1.,}     mode  = {2308,}

  PARTICLE XiZeroBar1530 _
                      code   = 40008     pdg   = -3324     ,
                      trktyp = kGtNeut   mass  = 1.53180   ,
                      charge = 0         tlife = 0         ,
                      bratio = {1.,}     mode  = {3109,}
  

  ! omega(782) --> e+ e- 100%
  Particle omega  code=10150 TrkTyp=3 mass=.782   charge=0  tlife=7.79E-23,
                  pdg=0    bratio  = { 1.00, },  
                           mode    = { 0203, }

  ! phi --> K+ K- 100%
  PARTICLE phi    code=10151 trktyp=3 mass=1.0194 charge=0  tlife=1.482e-22,
                  pdg=0    bratio = { 1.00, },
                  mode   = { 1112, }  

  ! phi --> e+e- 100%
  PARTICLE phi    code=11151 trktyp=3 mass=1.0194 charge=0  tlife=1.482e-22,
                  pdg=0    bratio = { 1.00, } mode   = { 0203, }  



  PARTICLE Lambda     code    = 10018                ,
                      pdg     = 0                    ,
                      mass    = 1.115683             ,  
                      tlife   = 0.26320E-9           ,
                      charge  = 0                    ,
                      bratio  = {1.000,}             ,
                      mode    = {1409,}              ,
                      trktyp  = kGtNEUT
 
  PARTICLE LambdaBar  code    = 10026                ,
                      pdg     = 0                    ,
                      mass    = 1.115683             ,  
                      tlife   = 0.26320E-9           ,
                      charge  = 0                    ,
                      bratio  = {1.000,}             ,
                      mode    = {1508,}              ,
                      trktyp  = kGtNEUT

  ! The eta dalitz decay
  PARTICLE eta_dalitz code    = 10017                ,
                      pdg     = 0                    ,
                      mass    = 0.5475               ,
                      tlife   = 0.54850E-18          ,
                      charge  = 0                    , 
                      bratio  = {1.0,}               ,
                      mode    = {010203,}            ,
                      trktyp  = kGtNEUT
 
  ! Ds+ --> phi pi+          100%
  !          |
  !          +--> K+K-       100%
  PARTICLE D_s_plus   code    = 10039                ,
                      pdg     = 0                    ,
                      mass    = 1.969                ,
                      tlife   = 0.43600E-12          ,
                      charge  = +1                   ,
                      bratio  = {1.0,}               ,
                      mode    = {9908,}              ,
                      trktyp  = kGtHADR

  ! Ds- --> phi pi+          100%
  !          |
  !          +--> K+K-       100%
  PARTICLE D_s_minus  code    = 10040                ,
                      pdg     = 0                    ,
                      mass    = 1.969                ,
                      tlife   = 0.43600E-12          ,
                      charge  = -1                   ,
                      bratio  = {1.0,}               ,
                      mode    = {9909,}              ,
                      trktyp  = kGtHADR


           PARTICLE _phi_to_KK_ code      = 99             ,
                                trktyp    = 3              ,
                                mass      = 1.0194         ,
                                charge    = 0              ,
                                tlife     = 1.482e-22      ,
                                pdg       = 0              ,
                                bratio    = {1.00,}        ,     
                                mode      = {1112,}        ,
                                trktyp    = kGtNEUT   

  ! Some nice anti-nuclei defined w/ offset 50000

  PARTICLE antiDeuteron code      = 50045            , 
                        mass      = 1.876            ,
                        charge    = -1.0             ,
                        tlife     = STABLE           ,
                        pdg       = UNDEFINED        ,
                        trktyp    = kGtHION

  PARTICLE antiDeuteron code      = 53               , 
                        mass      = 1.876            ,
                        charge    = -1.0             ,
                        tlife     = STABLE           ,
                        pdg       = UNDEFINED        ,
                        trktyp    = kGtHION



  PARTICLE antiTriton   code      = 50046            ,
                        mass      = 2.809            ,
                        charge    = -1.0             ,
                        tlife     = STABLE           ,
                        pdg       = UNDEFINED        ,
                        trktyp    = kGtHION                

  PARTICLE antiAlpha    code      = 50047            ,
                        mass      = 3.727            ,
                        charge    = -2.0             ,
                        tlife     = STABLE           ,
                        pdg       = UNDEFINED        ,
                        trktyp    = kGtHION

  PARTICLE antiHelium3  code      = 50049            ,
                        mass      = 2.809            ,
                        charge    = -2.0             ,
                        tlife     = STABLE           ,
                        pdg       = UNDEFINED        ,
                        trktyp    = kGtHION

       PARTICLE antiHelium3  code      = 54               ,
                             mass      = 2.809            ,
                             charge    = -2.0             ,
                             tlife     = STABLE           ,
                             pdg       = UNDEFINED        ,
                             trktyp    = kGtHION


   """Define all hyper-nuclei and exotics with offset 60000"""
* Particle hyperTriton  code      = 60053            ,   ! Placeholder for hypertriton
*                       mass      = 2.911            ,   ! with all decay modes
*                       charge    = 1                ,
*                       tlife     = 2.6320e-10       ,
*                       pdg       = UNDEFINED        ,
*                       trktyp    = kGtHION          ,
*                       bratio    = { , ...}         ,
*                       mode      = { , ...}      
                        
* Particle antiHyperTriton code   = 60054 , ...

Particle hyperTriton_he3_pi_minus code      = 61053            ,
                                  mass      = 2.99131          , 
                                  charge    = 1                ,
                                  tlife     = 2.6320e-10       ,
                                  pdg       = UNDEFINED        ,
                                  trktyp    = kGtHADR          ,
                                  bratio    = {1,}             ,
                                  mode      = {004909,}

Particle anti_hyperTriton_he3_pi_plus  code      = 61054       ,
                                  mass      = 2.99131          , 
                                  charge    = -1               ,
                                  tlife     = 2.6320e-10       ,
                                  pdg       = UNDEFINED        ,
                                  trktyp    = kGtHADR          ,
                                  bratio    = {1,}             ,
                                  mode      = {005408,}


Particle hyperTriton_d_p_pi_minus code      = 62053            ,
                                  mass      = 2.99131          ,
                                  charge    = +1               ,
                                  tlife     = 2.6320e-10       ,
                                  pdg       = UNDEFINED        ,
                                  trktyp    = kGtHADR          ,
                                  bratio    = {1,}             ,
                                  mode      = {091445,}

Particle anti_hyperTriton_db_pb_pi code      = 62054  ,
                                  mass      = 2.99131          ,
                                  charge    = -1               ,
                                  tlife     = 2.6320e-10       ,
                                  pdg       = UNDEFINED        ,
                                  trktyp    = kGtHADR          ,
                                  bratio    = {1,}             ,
                                  mode      = {081553,}


   """Define all dibaryons / dimesons with offset=60000 """

Particle H_dibaryon               code      = 60001,
                                  mass      = 2.232,
                                  charge    = 0,
                                  tlife     = 0.26320E-9/2,
                                  pdg       = UNDEFINED,
                                  trktyp    = kGtHADR,
                                  bratio    = {1,},
                                  mode      = { 98 14 09, }



  PARTICLE LamXi2430 "Lambda0 Xi0 bound state " _
                      code = 60002       pdg=0             ,
                      trktyp = kGtNeut   mass=2.430543-0.002,
                      charge = 0         tlife    = 1.0e-10,
                      bratio = {0.5,  0.5 }                ,   
                      mode   = {1423, 1818}                    

  PARTICLE AntiLamXi2430 "Lambda0 Xi0 bound state " _
                      code = 60003       pdg=0                ,
                      trktyp = kGtNeut   mass=2.430543-0.002  ,
                      charge = 0         tlife = 1.0e-10      ,    
                      bratio = {0.5,  0.5 }                   ,
                      mode   = {1531, 2626}


  PARTICLE P_N_XI     "(pnXi-) --> (hyper triton) pi-" _ 
                      code   = 60004     pdg   = 0             ,
                      trktyp = kGtNeut   mass  = 3.1975        ,
                      charge = 0         tlife = 1.6378e-10    

     """ Append actual decay mode in user buffer.  Up to 3 body decay """
     """ (phase space decay) supported """
     uw = { 0, 61053, 09 }
     Call GSPART( %code, %title, %trktyp, %mass, %charge, %tlife, uw, nw )

   PARTICLE Anti_P_N_XI     "(pnXi-) --> (hyper triton) pi-" _ 
                      code   = 60005     pdg   = 0             ,
                      trktyp = kGtNeut   mass  = 3.1975        ,
                      charge = 0         tlife = 1.6378e-10    

     """ Append actual decay mode in user buffer.  Up to 3 body decay """
     """ (phase space decay) supported """
     uw = { 0, 61054, 08 }
     Call GSPART( %code, %title, %trktyp, %mass, %charge, %tlife, uw, nw )



   PARTICLE Xi0_XiMinus "(Xi0Ximinus)" _
                       code = 60006      pdg = 0             ,
                       trktyp = kGtHadr  mass = 2.6346       ,
                       charge = -1       tlife = 1.6378e-10  ,
                       bratio = {0.5, 0.5}  mode = { 22 18 09, 23 18 07 }

   PARTICLE AntiXi0_XiMinus _
                       code = 60007      pdg = 0             ,
                       trktyp = kGtHadr  mass = 2.6346       ,
                       charge = +1       tlife = 1.6378e-10  ,
                       bratio = {0.5, 0.5}  mode = { 30 26 08, 31 26 07 }
                        


 End! Module gstar_part


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
*$$$  Subroutine aGuDCAY
*CDE,gctrak,gckine,gcking.
*
*  If decay modes are not set, this routine will be called to decay the particle
*
*  What follows is a snippet of code to explore a few variables the
* user might need:
*
*      real P_PART(4)
*
*      DO I=1,4
*         P_PART(I)=VECT(I+3)*VECT(7)
*      ENDDO
*
*      write(*,*) 'ivert, ipart: ', ivert, ipart
*      write(*,*) 'p1, p2, p3: ', P_PART(1), P_PART(2), P_PART(3)
*
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
*
*$$$  end
*
* --------------------------------------------------------------------------
*
 
 
