
      SUBROUTINE DT_GLAUBE(Na,Nb,Ijproj,B,Intt,Inta,Intb,Js,Jt,Nidx)
 
C***********************************************************************
C Calculation of configuartion of interacting nucleons for one event.  *
C    NB / NB    mass numbers of proj./target nuclei           (input)  *
C    B          impact parameter                              (output) *
C    INTT       total number of wounded nucleons                 "     *
C    INTA / INTB number of wounded nucleons in proj. / target    "     *
C    JS / JT(i) number of collisions proj. / target nucleon i is       *
C                                                   involved  (output) *
C    NIDX       index of projectile/target material             (input)*
C This is an update of the original routine SHMAKO by J.Ranft/HJM      *
C This version dated 22.03.96 is revised by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION B , ONE , rate , ratq , TINY10 , TINY14 , TWO , 
     &                 ZERO
      INTEGER i , i1 , i2 , Ijproj , Inta , Intb , Intt , j1 , j2 , Js , 
     &        Jt , MAXINT , MAXNCL , MAXSQU , MAXVQU , Na , Nb , Nidx , 
     &        ntarg
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY14=1.0D-14,ZERO=0.0D0,ONE=1.0D0,
     &           TWO=2.0D0)
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
 
 
      PARAMETER (MAXNCL=260,MAXVQU=MAXNCL,MAXSQU=20*MAXVQU,
     &           MAXINT=MAXVQU+MAXSQU)
C Glauber formalism: parameters
      INCLUDE 'inc/dtglam'
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C properties of photon/lepton projectiles
      INCLUDE 'inc/dtgpro'
C Glauber formalism: collision properties
      INCLUDE 'inc/dtglcp'
C Glauber formalism: flags and parameters for statistics
      INCLUDE 'inc/dtglgp'
 
      DIMENSION Js(MAXNCL) , Jt(MAXNCL)
 
      ntarg = ABS(Nidx)
 
C get actual energy from /DTLTRA/
      ECMnow = UMO
      Q2 = VIRt
C
#ifdef FOR_CORSIKA
      if (LPRI.GT.4) write(LOUT,*)'DT_GLAUBE:IOGLB=',IOGLB,' NIDX=',NIDX
#endif
C new patch for pre-initialized variable projectile/target/energy runs
      IF ( IOGlb.NE.100 ) THEN
         i1 = 1
         i2 = 1
         rate = ONE
         IF ( NEBini.GT.1 ) THEN
            IF ( ECMnow.GE.ECMnn(NEBini) ) THEN
               i1 = NEBini
               i2 = NEBini
               rate = ONE
            ELSE IF ( ECMnow.GT.ECMnn(1) ) THEN
               DO i = 2 , NEBini
                  IF ( ECMnow.LT.ECMnn(i) ) THEN
                     i1 = i - 1
                     i2 = i
                     rate = (ECMnow-ECMnn(i1))/(ECMnn(i2)-ECMnn(i1))
                     GOTO 50
                  END IF
               END DO
            END IF
         END IF
 50      j1 = 1
         j2 = 1
         ratq = ONE
         IF ( NQBini.GT.1 ) THEN
            IF ( Q2.GE.Q2G(NQBini) ) THEN
               j1 = NQBini
               j2 = NQBini
               ratq = ONE
            ELSE IF ( Q2.GT.Q2G(1) ) THEN
               DO i = 2 , NQBini
                  IF ( Q2.LT.Q2G(i) ) THEN
                     j1 = i - 1
                     j2 = i
                     ratq = LOG10(Q2/MAX(Q2G(j1),TINY14))
     &                      /LOG10(Q2G(j2)/MAX(Q2G(j1),TINY14))
C                    RATQ = (Q2-Q2G(J1))/(Q2G(J2)-Q2G(J1))
                     GOTO 100
                  END IF
               END DO
            END IF
         END IF
 
 100     DO i = 1 , KSITEB
            BSIte(0,1,ntarg,i) = BSIte(i1,j1,ntarg,i)
     &         + rate*(BSIte(i2,j1,ntarg,i)-BSIte(i1,j1,ntarg,i))
     &         + ratq*(BSIte(i1,j2,ntarg,i)-BSIte(i1,j1,ntarg,i))
     &         + rate*ratq*(BSIte(i2,j2,ntarg,i)-BSIte(i1,j2,ntarg,i)
     &         +BSIte(i1,j1,ntarg,i)-BSIte(i2,j1,ntarg,i))
         END DO
 
Cc                                                            mar'04 (26-02:55)
Cc -----------------------------------------------------------------------------
Cc  by selecting KKMAT == -2 all the usual Glauber initialization routines are
Cc  skipped and we make use of pre-processed Glauber impact parameter
Cc  distributions - see also  >>>  dt_modb.f
Cc
      ELSE IF ( Nidx.NE.-2 ) THEN
C          write(0,*) ' -- dt_glaube -- skip call into dt_glbset()'
Cc -------------------
 
         CALL DT_GLBSET(Ijproj,Na,Nb,EPRoj,1)
 
Cc -------------------
Cc
Cc -----------------------------------------------------------------------------
Cc
 
C
C variable energy run, interpolate profile function
      END IF
 
      CALL DT_DIAGR(Na,Nb,Ijproj,B,Js,Jt,Intt,Inta,Intb,IDIrec,Nidx)
      IF ( Nidx.LE.-1 ) THEN
         RPRoj = RASh(1)
         RTArg = RBSh(ntarg)
      ELSE
         RPRoj = RASh(ntarg)
         RTArg = RBSh(1)
      END IF
 
      END SUBROUTINE
