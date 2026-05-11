
      SUBROUTINE PHO_PARTPT(Imode,If,Il,Ptcut,Irej)
C********************************************************************
C
C    assign to soft partons
C
C    input:  IMODE   -2   output of statistics
C                    -1   initialization
C                     0   sampling of pt for soft partons belonging to
C                         soft Pomerons
C                     1   sampling of pt for soft partons belonging to
C                         hard Pomerons
C            IF           first entry in /POEVT1/ to check
C            IL           last entry in /POEVT1/ to check
C            PTCUT        current value of PTCUT to distinguish
C                         between soft and hard
C
C    output: IREJ     0   success
C                     1   failure
C
C    (soft pt is sampled by call to PHO_SOFTPT)
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION am , amsqr , DEPS , DT_RNDM , dum , emin , gam , 
     &                 gambez , plong , psumx , psumy , ptcut2 , ptm , 
     &                 ptmx , ptot , ptrem
      INTEGER i , i1 , i2 , ic1 , ientry , ii , ipeak , istart , iter , 
     &        k , l
      SAVE 
 
      PARAMETER (DEPS=1.D-15)
 
      INTEGER Imode , If , Il , Irej
      DOUBLE PRECISION Ptcut
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  some constants
      INCLUDE 'inc/pocons'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
      DOUBLE PRECISION pts , pb , xp , xpb , pc
      DIMENSION pts(0:2,50) , pb(0:2,2) , xp(50) , xpb(2) , pc(4)
 
      INTEGER modify , iv , ivb
      DIMENSION modify(50) , iv(50) , ivb(2)
 
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(6).GE.10 )
     &      WRITE (LO,'(1X,A,3I4,1P,E11.3)')
     &      'PHO_PARTPT: called with IMODE,IF,IL,PTCUT' , Imode , If , 
     &     Il , Ptcut
 
      IF ( Imode.LT.0 ) THEN
 
C  initialization / output of statistics
         CALL PHO_SOFTPT(Imode,ptm,ptm,xp,iv,pts)
         GOTO 99999
      END IF
 
      Irej = 0
      IF ( (ISWmdl(3).EQ.10) .AND. (ISWmdl(4).EQ.10) ) RETURN
 
C  count entries to modify
      ientry = 0
      ptcut2 = Ptcut**2
      emin = 1.D20
      ipeak = 1
      istart = If
 
C  soft Pomerons
 
      IF ( Imode.EQ.0 ) THEN
         DO i = istart , Il
            IF ( (ISThep(i).EQ.-1) .AND. (ABS(IPHist(1,i)).LT.100) )
     &           THEN
               ientry = ientry + 1
               modify(ientry) = i
               xp(ientry) = SIGN(PHEp(4,i)/ECMp,PHEp(3,i))
               iv(ientry) = 0
               IF ( IDHep(i).NE.21 ) iv(ientry) = ICOlor(2,i)
               IF ( PHEp(4,i).LT.emin ) THEN
                  emin = PHEp(4,i)
                  ipeak = ientry
               END IF
            END IF
         END DO
 
C  hard Pomeron associated remnants (IPHIST(1,)=100,200,...)
 
      ELSE IF ( Imode.EQ.1 ) THEN
 
         DO i = istart , Il
            IF ( (ISThep(i).EQ.-1) .AND. (IPHist(1,i).GE.100) ) THEN
               IF ( MOD(IPHist(1,i),100).EQ.0 ) THEN
                  ientry = ientry + 1
                  modify(ientry) = i
                  xp(ientry) = SIGN(PHEp(4,i)/ECMp,PHEp(3,i))
                  IF ( ISWmdl(24).EQ.0 ) THEN
                     iv(ientry) = 0
                     IF ( IDHep(i).NE.21 ) iv(ientry) = ICOlor(2,i)
                  ELSE IF ( ISWmdl(24).EQ.1 ) THEN
                     iv(ientry) = -1
                  ELSE
                     iv(ientry) = 1
                  END IF
                  IF ( PHEp(4,i).LT.emin ) THEN
                     emin = PHEp(4,i)
                     ipeak = ientry
                  END IF
               END IF
            END IF
         END DO
 
C  something wrong
 
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &         'PHO_PARTPT:ERROR: invalid mode' , Imode
         CALL PHO_ABORT
      END IF
 
C  debug output
      IF ( IDEb(6).GE.5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,3I4)') 'PHO_PARTPT: ' , 
     &        'number of partons, IPEAK,MODE' , ientry , ipeak , Imode
         IF ( IDEb(6).GE.20 ) CALL PHO_PREVNT(0)
      END IF
 
C  nothing to do
      IF ( ientry.LE.1 ) RETURN
 
C  sample pt of soft partons
 
      IF ( ISWmdl(5).LE.1 ) THEN
         iter = 0
         ipeak = DT_RNDM(dum)*ientry + 1
         CALL PHO_SWAPI(modify(ipeak),modify(1))
         CALL PHO_SWAPD(xp(ipeak),xp(1))
         CALL PHO_SWAPI(iv(ipeak),iv(1))
C  energy limited sampling
 50      psumx = 0.D0
         psumy = 0.D0
         iter = iter + 1
         IF ( iter.GE.1000 ) THEN
            IF ( IDEb(6).GE.3 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,3I5)')
     &               'PHO_PARTPT: rejection for MODE,ENTRY,ITER' , 
     &              Imode , ientry , iter
               IF ( LPRi.GT.4 ) WRITE (LO,'(8X,A,I5)')
     &               'I  II  IV       XP         EP' , ipeak
               DO i = 1 , ientry
                  ii = modify(i)
                  IF ( LPRi.GT.4 ) WRITE (LO,'(5X,3I5,1P,2E13.4)') i , 
     &                 ii , iv(i) , xp(i) , PHEp(4,ii)
               END DO
               IF ( IDEb(6).GE.5 ) CALL PHO_PREVNT(0)
            END IF
            Irej = 1
            RETURN
         END IF
         DO i = 2 , ientry
            ii = modify(i)
            ptmx = MIN(PHEp(4,ii),Ptcut)
            xpb(1) = xp(i)
            ivb(1) = iv(i)
            IF ( ISWmdl(5).EQ.0 ) THEN
               CALL PHO_SOFTPT(1,Ptcut,ptmx,xpb,ivb,pb)
            ELSE
               CALL PHO_SOFTPT(1,ptmx,ptmx,xpb,ivb,pb)
            END IF
            pts(0,i) = pb(0,1)
            pts(1,i) = pb(1,1)
            pts(2,i) = pb(2,1)
            psumx = psumx + pb(1,1)
            psumy = psumy + pb(2,1)
         END DO
         ptrem = SQRT(psumx**2+psumy**2)
         IF ( ptrem.GT.MIN(PHEp(4,modify(1)),Ptcut) ) GOTO 50
         pts(1,1) = -psumx
         pts(2,1) = -psumy
      ELSE IF ( (ISWmdl(5).EQ.2) .OR. 
     &          ((Imode.EQ.1) .AND. (ISWmdl(5).EQ.3)) ) THEN
C  unlimited sampling
         ipeak = DT_RNDM(psumx)*ientry + 1
         CALL PHO_SWAPI(modify(ipeak),modify(1))
         CALL PHO_SWAPD(xp(ipeak),xp(1))
         CALL PHO_SWAPI(iv(ipeak),iv(1))
         CALL PHO_SOFTPT(ientry,Ptcut,Ptcut,xp,iv,pts)
      ELSE IF ( ISWmdl(5).EQ.3 ) THEN
C  each string has balanced pt
         DO k = 1 , ientry
            IF ( iv(k).GT.-90 ) THEN
               i1 = modify(k)
               ic1 = -ICOlor(1,i1)
               DO l = k + 1 , ientry
                  IF ( ICOlor(1,modify(l)).EQ.ic1 ) GOTO 60
               END DO
               IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A,I5)')
     &               'PHO_PARTPT:ERROR: no color found for (line,color)'
     &              , i1 , -ic1
               CALL PHO_ABORT
 60            i2 = modify(l)
               amsqr = (PHEp(4,i1)+PHEp(4,i2))
     &                 **2 - (PHEp(1,i1)+PHEp(1,i2))
     &                 **2 - (PHEp(2,i1)+PHEp(2,i2))
     &                 **2 - (PHEp(3,i1)+PHEp(3,i2))**2
               am = SQRT(amsqr)
               ptmx = am/2.D0
               ivb(1) = MAX(iv(k),iv(l))
               xpb(1) = xp(k)
               CALL PHO_SOFTPT(1,Ptcut,ptmx,xpb,ivb,pb)
               pts(1,k) = pb(1,1)
               pts(2,k) = pb(2,1)
               pts(1,l) = -pb(1,1)
               pts(2,l) = -pb(2,1)
               gam = (PHEp(4,i1)+PHEp(4,i2))/am
               gambez = (PHEp(3,i1)+PHEp(3,i2))/am
               pc(1) = pb(1,1)
               pc(2) = pb(2,1)
               plong = SQRT(ptmx**2-pb(0,1)**2+1.D-12)
               pc(3) = SIGN(plong,PHEp(3,i1))
               pc(4) = ptmx
               CALL PHO_ALTRA(gam,0.D0,0.D0,gambez,pc(1),pc(2),pc(3),
     &                        pc(4),ptot,PHEp(1,i1),PHEp(2,i1),
     &                        PHEp(3,i1),PHEp(4,i1))
               pc(1) = -pc(1)
               pc(2) = -pc(2)
               pc(3) = -pc(3)
               CALL PHO_ALTRA(gam,0.D0,0.D0,gambez,pc(1),pc(2),pc(3),
     &                        pc(4),ptot,PHEp(1,i2),PHEp(2,i2),
     &                        PHEp(3,i2),PHEp(4,i2))
               iv(k) = iv(k) - 100
               iv(l) = iv(l) - 100
            END IF
         END DO
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I4)')
     &         'PHO_PARTPT:ERROR: invalid value of ISWMDL(5):' , 
     &        ISWmdl(5)
         CALL PHO_ABORT
      END IF
 
C  change partons in /POEVT1/
      DO ii = 1 , ientry
         IF ( iv(ii).GT.-90 ) THEN
            i = modify(ii)
            PHEp(1,i) = PHEp(1,i) + pts(1,ii)
            PHEp(2,i) = PHEp(2,i) + pts(2,ii)
            amsqr = PHEp(4,i)**2 - PHEp(1,i)**2 - PHEp(2,i)
     &              **2 - PHEp(3,i)**2
            PHEp(5,i) = SIGN(SQRT(ABS(amsqr)),amsqr)
         END IF
      END DO
 
C  debug output
      IF ( IDEb(6).GE.15 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,8X,A,I5)')
     &         'PHO_PARTPT: table of momenta' , 
     &        'I  II  IV    XP    EP    PTS   PTX   PTY' , ipeak
         DO i = 1 , ientry
            ii = modify(i)
            IF ( LPRi.GT.4 ) WRITE (LO,'(2X,3I5,1P,5E12.4)') i , ii , 
     &           iv(i) , xp(i) , PHEp(4,ii) , pts(0,i) , pts(1,i) , 
     &           pts(2,i)
         END DO
         CALL PHO_PREVNT(0)
      END IF
 
99999 END SUBROUTINE
