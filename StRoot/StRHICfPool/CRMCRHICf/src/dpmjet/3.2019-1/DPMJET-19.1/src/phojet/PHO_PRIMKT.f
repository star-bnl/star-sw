
      SUBROUTINE PHO_PRIMKT(Imode,If,Il,Ptcut,Irej)
C***********************************************************************
C
C    give primordial kt to partons entering hard scatterings and
C    remants connected to hard parton-parton interactions by color flow
C
C    input:  IMODE   -2   output of statistics
C                    -1   initialization
C                     1   sampling of primordial kt
C            IF           first entry in /POEVT1/ to check
C            IL           last entry in /POEVT1/ to check
C            PTCUT        current value of PTCUT to distinguish
C                         between soft and hard
C
C    output: IREJ     0   success
C                     1   failure
C
C***********************************************************************
 
      IMPLICIT NONE
 
      SAVE 
 
      DOUBLE PRECISION DEPS
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
C  hard scattering data
      INCLUDE 'inc/pohslt'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
      DOUBLE PRECISION pts , xp , xp2 , pold , pnew , ga , pp
      DIMENSION pts(0:2,5) , xp(5) , xp2(5,2) , pold(2,2) , pnew(4,2) , 
     &          ga(4) , pp(4)
 
      INTEGER irott , ibalt , ibal , iv , iv2 , IRMAX
 
      PARAMETER (IRMAX=200)
      DIMENSION irott(IRMAX) , ibalt(5,2) , ibal(2) , iv(5) , iv2(5,2)
 
      DOUBLE PRECISION si , ei , sf , ef , ptot , ee , xx , yy , zz , 
     &                 anorf , fac , del , pt2 , del2 , gae , gaz , 
     &                 sid , cod , sif , cof , esum
      INTEGER irot , i , j , k , nhd , istart , inext , icom
 
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(10).GE.10 )
     &      WRITE (LO,'(1X,A,3I4,1P,E11.3)')
     &      'PHO_PRIMKT: called with IMODE,IF,IL,PTCUT' , Imode , If , 
     &     Il , Ptcut
 
C  give primordial kt to partons engaged in a hard scattering
 
      IF ( Imode.EQ.1 ) THEN
 
         istart = If
 
 
 50      nhd = 0
         ibal(1) = 0
         ibal(2) = 0
         irot = 0
         icom = 0
         DO i = istart , Il
            IF ( ISThep(i).EQ.25 ) THEN
C  hard scattering number
               nhd = IPHist(1,i+1)
               icom = i
               k = LSIdx(nhd/100)
C  calculate momenta of incoming partons
               pold(1,1) = XHD(k,1)*ECMp/2.D0
               pold(2,1) = pold(1,1)
               pold(1,2) = -XHD(k,2)*ECMp/2.D0
               pold(2,2) = -pold(1,2)
               istart = i + 3
               GOTO 100
            END IF
         END DO
         RETURN
 
 
C  search for partons involved in hard interaction
 100     inext = 0
         irot = 0
         DO i = istart , Il
            IF ( ABS(ISThep(i)).EQ.1 ) THEN
C  hard scatterd partons (including ISR)
               IF ( (IPHist(1,i).EQ.-nhd) .OR. (IPHist(1,i).EQ.nhd+1)
     &              .OR. (IPHist(1,i).EQ.nhd+2) ) THEN
                  irot = irot + 1
 
                  IF ( irot.GT.IRMAX ) THEN
                     IF ( LPRi.GT.4 ) WRITE (LO,'(1X,/,2A,2I5)')
     &                     'PHO_PRIMKT: ' , 
     &              'no memory left in IROTT, event rejected (max/IROT)'
     &              , IRMAX , irot
                     CALL PHO_PREVNT(0)
                     Irej = 1
                     RETURN
                  END IF
 
                  irott(irot) = i
C  hard remnant
               ELSE IF ( IPHist(1,i).EQ.nhd ) THEN
                  IF ( PHEp(3,i).GT.0.D0 ) THEN
                     j = 1
                  ELSE
                     j = 2
                  END IF
                  ibal(j) = ibal(j) + 1
                  ibalt(ibal(j),j) = i
                  xp2(ibal(j),j) = PHEp(3,i)/ECMp
                  IF ( ISWmdl(24).EQ.0 ) THEN
                     iv2(ibal(j),j) = 0
                     IF ( IDHep(i).NE.21 ) iv2(ibal(j),j) = ICOlor(2,i)
                  ELSE IF ( ISWmdl(24).EQ.1 ) THEN
                     iv2(ibal(j),j) = -1
                  ELSE
                     iv2(ibal(j),j) = 1
                  END IF
               END IF
C  possibly further hard scattering
            ELSE IF ( ISThep(i).EQ.25 ) THEN
               inext = 1
               istart = i
               GOTO 150
            END IF
         END DO
 
C debug output
 150     IF ( IDEb(10).GE.15 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,I4)') 'PHO_PRIMKT: ' , 
     &           'hard scattering number: ' , nhd/100
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,I5)') 'PHO_PRIMKT: ' , 
     &           'number of entries to rotate: ' , irot
            DO i = 1 , irot
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I5)')
     &              'PHO_PRIMKT: ' , 'entries to rotate: ' , i , 
     &              irott(i)
            END DO
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I5)') 'PHO_PRIMKT: ' , 
     &           'number of entries to balance: ' , ibal
            DO j = 1 , 2
               DO i = 1 , ibal(j)
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,I2,2I5)')
     &                  'PHO_PRIMKT: entries to balance (side,no,line)'
     &                 , j , i , ibalt(i,j)
               END DO
            END DO
         END IF
 
C  incoming partons (comment lines), skip direct interacting particles
         DO k = 1 , 2
            IF ( (IDHep(icom+k).NE.22) .AND. (IDHep(icom+k).NE.990) )
     &           THEN
               IF ( PHEp(3,icom+k).GT.0.D0 ) THEN
                  j = 1
               ELSE
                  j = 2
               END IF
               ibal(j) = ibal(j) + 1
               ibalt(ibal(j),j) = -icom - k
               xp2(ibal(j),j) = pold(1,j)/ECMp
               iv2(ibal(j),j) = -1
            END IF
         END DO
 
C  check consistency
         IF ( (ibal(1).GT.4) .OR. (ibal(2).GT.4) ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,I10)') 'PHO_PRIMKT: ' , 
     &           'inconsistent hard scattering remnant for event: ' , 
     &           KEVent
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,3I4,1P,E11.3)')
     &            'PHO_PRIMKT called with IMODE,IF,IL,PTCUT' , Imode , 
     &           If , Il , Ptcut
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I4)')
     &            'hard scattering number: ' , nhd/100
            DO i = 1 , irot
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I5)')
     &               'entries to rotate' , i , irott(i)
            END DO
            DO j = 1 , 2
               DO i = 1 , ibal(j)
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I2,2I5)')
     &                  'entries to balance (side,no,line)' , j , i , 
     &                 ibalt(i,j)
               END DO
            END DO
            IF ( ibal(1)+ibal(2).GT.0 ) CALL PHO_PREVNT(0)
         END IF
 
C  calculate primordial kt
 
C  something to do?
         IF ( (ibal(1).GT.1) .OR. (ibal(2).GT.1) ) THEN
 
C  add transverse momentum (overwrite /POEVT1/ entries)
            DO j = 1 , 2
               IF ( ibal(j).GT.1 ) THEN
C  sample from truncated distribution
                  k = ibal(j)
                  DO i = 1 , k
                     iv(i) = iv2(i,j)
                     xp(i) = xp2(i,j)
                  END DO
 155              CALL PHO_SOFTPT(k,Ptcut,Ptcut,xp,iv,pts)
                  IF ( pts(0,k).GE.PARmdl(100) ) GOTO 155
C  transform incoming partons of hard scattering
                  del = ABS(pold(1,j)) + pold(2,j)
                  pt2 = pts(0,k)**2
                  del2 = del*del
                  pnew(1,j) = pts(1,k)
                  pnew(2,j) = pts(2,k)
                  pnew(3,j) = (-1)**j*(pt2-del2)/(2.D0*del)
                  pnew(4,j) = (del2+pt2)/(2.D0*del)
C  spectator partons
                  esum = 0.D0
                  DO i = 1 , ibal(j) - 1
                     k = ibalt(i,j)
                     PHEp(1,k) = PHEp(1,k) + pts(1,i)
                     PHEp(2,k) = PHEp(2,k) + pts(2,i)
                     esum = esum + PHEp(4,k)
                  END DO
C  long. momentum transfer
                  pp(3) = pnew(3,j) - pold(1,j)
                  pp(4) = pnew(4,j) - pold(2,j)
                  DO i = 1 , ibal(j) - 1
                     k = ibalt(i,j)
                     fac = PHEp(4,k)/esum
                     PHEp(3,k) = PHEp(3,k) - fac*pp(3)
                     PHEp(4,k) = PHEp(4,k) - fac*pp(4)
                  END DO
 
C  debug output
                  IF ( IDEb(10).GE.15 ) THEN
                     IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,I3,1P,4E11.3)')
     &                     'PHO_PRIMKT: ' , 'old incoming:' , j , 0.D0 , 
     &                    0.D0 , (pold(i,j),i=1,2)
                     IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,I3,1P,4E11.3)')
     &                     'PHO_PRIMKT: ' , 'new incoming:' , j , 
     &                    (pnew(i,j),i=1,4)
                  END IF
 
               ELSE
                  pnew(1,j) = 0.D0
                  pnew(2,j) = 0.D0
                  pnew(3,j) = pold(1,j)
                  pnew(4,j) = pold(2,j)
               END IF
            END DO
 
C  transformation of hard scattering final states (including ISR)
 
C  old parton c.m. energy
            si = (pold(2,1)+pold(2,2))**2 - (pold(1,1)+pold(1,2))**2
            ei = SQRT(si)
C  new parton c.m. energy
            sf = (pnew(4,1)+pnew(4,2))**2 - (pnew(1,1)+pnew(1,2))
     &           **2 - (pnew(2,1)+pnew(2,2))**2 - (pnew(3,1)+pnew(3,2))
     &           **2
            ef = SQRT(sf)
            fac = ef/ei
C  debug output
            IF ( LPRi.GT.4 .AND. IDEb(10).GE.25 )
     &            WRITE (LO,'(1X,A,1P,E12.4)')
     &            'PHO_PRIMKT: scaling factor (E-final/E-initial): ' , 
     &           fac
 
C  calculate Lorentz transformation
            gaz = -(pold(1,1)+pold(1,2))/ei
            gae = (pold(2,1)+pold(2,2))/ei
            DO i = 1 , 4
               ga(i) = (pnew(i,1)+pnew(i,2))/ef
            END DO
            CALL PHO_ALTRA(ga(4),-ga(1),-ga(2),-ga(3),pnew(1,1),
     &                     pnew(2,1),pnew(3,1),pnew(4,1),ptot,pp(1),
     &                     pp(2),pp(3),pp(4))
            ptot = MAX(DEPS,ptot)
            cod = pp(3)/ptot
            sid = SQRT(pp(1)**2+pp(2)**2)/ptot
            cof = 1.D0
            sif = 0.D0
            IF ( ptot*sid.GT.1.D-5 ) THEN
               cof = pp(1)/(sid*ptot)
               sif = pp(2)/(sid*ptot)
               anorf = SQRT(cof*cof+sif*sif)
               cof = cof/anorf
               sif = sif/anorf
            END IF
 
C  debug output
C  check consistency initial/final configuration before rotation
            IF ( IDEb(10).GE.25 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,4E11.3)')
     &               'PHO_PRIMKT: ini. momentum (1):' , 0.D0 , 0.D0 , 
     &              (pold(i,1)+pold(i,2),i=1,2)
               DO i = 1 , 4
                  pp(i) = 0.D0
               END DO
               DO i = 1 , irot
                  k = irott(i)
                  DO j = 1 , 4
                     pp(j) = pp(j) + PHEp(j,k)
                  END DO
               END DO
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,4E11.3)')
     &               'PHO_PRIMKT: fin. momentum (1):' , pp
            END IF
 
C  apply rotation/boost to scattered particles
            DO i = 1 , irot
               k = irott(i)
               DO j = 1 , 4
                  pp(j) = fac*PHEp(j,k)
               END DO
               CALL PHO_ALTRA(gae,0.D0,0.D0,gaz,pp(1),pp(2),pp(3),pp(4),
     &                        ptot,PHEp(1,k),PHEp(2,k),PHEp(3,k),
     &                        PHEp(4,k))
               CALL PHO_TRANS(PHEp(1,k),PHEp(2,k),PHEp(3,k),cod,sid,cof,
     &                        sif,xx,yy,zz)
               ee = PHEp(4,k)
               CALL PHO_ALTRA(ga(4),ga(1),ga(2),ga(3),xx,yy,zz,ee,ptot,
     &                        PHEp(1,k),PHEp(2,k),PHEp(3,k),PHEp(4,k))
            END DO
 
C  debug output
C  check consistency initial/final configuration after rotation
            IF ( IDEb(10).GE.25 ) THEN
               DO i = 1 , 4
                  pp(i) = pnew(i,1) + pnew(i,2)
               END DO
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,4E11.3)')
     &               'PHO_PRIMKT: ini. momentum (2):' , pp
               DO i = 1 , 4
                  pp(i) = 0.D0
               END DO
               DO i = 1 , irot
                  k = irott(i)
                  DO j = 1 , 4
                     pp(j) = pp(j) + PHEp(j,k)
                  END DO
               END DO
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,4E11.3)')
     &               'PHO_PRIMKT: fin. momentum (2):' , pp
            END IF
 
         END IF
 
         IF ( inext.EQ.1 ) GOTO 50
 
C  initialization
 
      ELSE IF ( Imode.EQ.-1 ) THEN
 
C  output of statistics etc.
 
      ELSE IF ( Imode.NE.-2 ) THEN
 
C  something wrong
 
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I4)')
     &         'PHO_PRIMKT:ERROR: invalid value of IMODE:' , Imode
         CALL PHO_ABORT
      END IF
 
      END SUBROUTINE
