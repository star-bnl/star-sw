
      SUBROUTINE DT_DECPI0
 
C***********************************************************************
C Decay of pi0 handled with JETSET.                                    *
C This version dated 11.11.12 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION costh , dum , ener , ONE , p1 , phi , pt , ptot , 
     &                 PYP , theta , TINY10 , TINY3 , twopi , ZERO
      INTEGER i , i0 , i1 , iact , id , idum , ihismo , ii , ini , 
     &        irej1 , kc , kk , ktemp , mo , nlines , nn , nnmax
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY3=1.0D-3,ONE=1.0D0,ZERO=0.0D0)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
 
      INCLUDE 'inc/pydat3'
 
      INCLUDE 'inc/pydat1'
 
      INCLUDE 'inc/pyjets'
 
C flags for input different options
      INCLUDE 'inc/dtflg1'
 
      INTEGER PYCOMP , PYK
 
      DIMENSION ihismo(NMXHKK) , p1(4)
 
      twopi = 2.0D0*ATAN2(0.0D0,-1.0D0)
 
      CALL DT_INITJS(2)
C allow pi0 decay
 
      kc = PYCOMP(111)
      MDCy(kc,1) = 1
 
      ini = 0
      i0 = 1
      i1 = NHKk
      iact = 0
      nnmax = MAXLND/4
 
 100  nn = 0
      DO i = i0 , i1
         IF ( nn.EQ.nnmax ) THEN
            i0 = i
            GOTO 200
         END IF
         IF ( (ISThkk(i).EQ.1) .AND. (IDHkk(i).EQ.111) ) THEN
            IF ( ini.EQ.0 ) THEN
               ini = 1
            ELSE
               ini = 2
            END IF
            IF ( LEMcck ) CALL DT_EVTEMC(PHKk(1,i),PHKk(2,i),PHKk(3,i),
     &           PHKk(4,i),ini,idum,idum)
            pt = SQRT(PHKk(1,i)**2+PHKk(2,i)**2)
            ptot = SQRT(pt**2+PHKk(3,i)**2)
            costh = PHKk(3,i)/(ptot+TINY10)
            IF ( costh.GT.ONE ) THEN
               theta = ZERO
            ELSE IF ( costh.LT.-ONE ) THEN
               theta = twopi/2.0D0
            ELSE
               theta = ACOS(costh)
            END IF
            phi = ASIN(PHKk(2,i)/(pt+TINY10))
 
            IF ( PHKk(1,i).LT.0.0D0 )
     &           phi = SIGN(twopi/2.0D0-ABS(phi),phi)
 
            ener = PHKk(4,i)
            nn = nn + 1
            ktemp = MSTu(10)
            MSTu(10) = 1
            P(nn,5) = PHKk(5,i)
 
            CALL PY1ENT(nn,111,ener,theta,phi)
 
            MSTu(10) = ktemp
            ihismo(nn) = i
         END IF
         iact = i
      END DO
 
 200  IF ( nn.GT.0 ) THEN
         CALL PYEXEC
         nlines = PYK(0,1)
         DO ii = 1 , nlines
            IF ( PYK(ii,7).EQ.1 ) THEN
               DO kk = 1 , 4
                  p1(kk) = PYP(ii,kk)
               END DO
               id = PYK(ii,8)
               mo = ihismo(PYK(ii,15))
               CALL DT_EVTPUT(1,id,mo,0,p1(1),p1(2),p1(3),p1(4),0,0,0)
               IF ( LEMcck ) CALL DT_EVTEMC(-p1(1),-p1(2),-p1(3),-p1(4),
     &              2,idum,idum)
Csr: flag with neg. sign (for HELIOS p/A-W jobs)
               ISThkk(mo) = -2
            END IF
         END DO
         IF ( LEMcck ) CALL DT_EVTEMC(dum,dum,dum,dum,4,7000,irej1)
      END IF
      IF ( iact.NE.i1 ) GOTO 100
 
      MDCy(kc,1) = 0
 
      END SUBROUTINE
