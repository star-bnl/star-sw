
      SUBROUTINE DT_CHASTA(Mode)
 
C***********************************************************************
C This subroutine performs CHAin STAtistics and checks sequence of     *
C partons in dtevt1 and sorts them with projectile partons coming      *
C first if necessary.                                                  *
C                                                                      *
C This version dated  8.5.00  is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , i0 , i1 , ichcfg , ichsta , ichtyp , id , idchk , 
     &        idx , idx1 , idx2 , imo1 , imo2 , ist1 , ist2 , isum , 
     &        itmp , itot , ityp1 , ityp2
      INTEGER itype , j , k , Mode , ngluon , nptn
      DOUBLE PRECISION rtmp1 , rtmp2 , rtmp3
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      CHARACTER*5 cchtyp
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C pointer to chains in hkkevt common (used by qq-breaking mechanisms)
      INCLUDE 'inc/dtixch'
 
      DIMENSION ichcfg(10,10,9,2) , ichtyp(5,5) , cchtyp(9) , ichsta(10)
     &          , itot(10)
      DATA ichcfg/1800*0/
      DATA (ichtyp(1,k),k=1,5)/0 , 1 , 3 , 0 , 0/
      DATA (ichtyp(2,k),k=1,5)/2 , 0 , 0 , 5 , 0/
      DATA (ichtyp(3,k),k=1,5)/4 , 0 , 0 , 7 , 0/
      DATA (ichtyp(4,k),k=1,5)/0 , 6 , 8 , 0 , 0/
      DATA (ichtyp(5,k),k=1,5)/0 , 0 , 0 , 0 , 9/
      DATA ichsta/21 , 22 , 31 , 32 , 41 , 42 , 51 , 52 , 61 , 62/
      DATA cchtyp/' q aq' , 'aq q ' , ' q d ' , ' d q ' , 'aq ad' , 
     &     'ad aq' , ' d ad' , 'ad d ' , ' g g '/
C
C initialization
C
      IF ( Mode.EQ.-1 ) THEN
         NCHain = 0
C
C loop over DTEVT1 and analyse chain configurations
C
      ELSE IF ( Mode.EQ.0 ) THEN
         DO idx = NPOint(3) , NHKk
            idchk = IDHkk(idx)/10000
            IF ( ((idchk.EQ.7) .OR. (idchk.EQ.8)) .AND. 
     &           (IDHkk(idx).NE.80000) .AND. (ISThkk(idx).NE.2) .AND. 
     &           (IDRes(idx).EQ.0) ) THEN
               IF ( JMOhkk(1,idx).GT.JMOhkk(2,idx) ) THEN
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &                  ' CHASTA: JMOHKK(1,x) > JMOHKK(2,x) ' , 
     &                 ' at entry ' , idx
                  GOTO 50
               END IF
C
               ist1 = ABS(ISThkk(JMOhkk(1,idx)))
               ist2 = ABS(ISThkk(JMOhkk(2,idx)))
               imo1 = ist1/10
               imo1 = ist1 - 10*imo1
               imo2 = ist2/10
               imo2 = ist2 - 10*imo2
C   swop parton entries if necessary since we need projectile partons
C   to come first in the common
               IF ( imo1.GT.imo2 ) THEN
                  nptn = JMOhkk(2,idx) - JMOhkk(1,idx) + 1
                  DO k = 1 , nptn/2
                     i0 = JMOhkk(1,idx) - 1 + k
                     i1 = JMOhkk(2,idx) + 1 - k
                     itmp = ISThkk(i0)
                     ISThkk(i0) = ISThkk(i1)
                     ISThkk(i1) = itmp
                     itmp = IDHkk(i0)
                     IDHkk(i0) = IDHkk(i1)
                     IDHkk(i1) = itmp
                     IF ( JDAhkk(1,JMOhkk(1,i0)).EQ.i0 )
     &                    JDAhkk(1,JMOhkk(1,i0)) = i1
                     IF ( JDAhkk(2,JMOhkk(1,i0)).EQ.i0 )
     &                    JDAhkk(2,JMOhkk(1,i0)) = i1
                     IF ( JDAhkk(1,JMOhkk(2,i0)).EQ.i0 )
     &                    JDAhkk(1,JMOhkk(2,i0)) = i1
                     IF ( JDAhkk(2,JMOhkk(2,i0)).EQ.i0 )
     &                    JDAhkk(2,JMOhkk(2,i0)) = i1
                     IF ( JDAhkk(1,JMOhkk(1,i1)).EQ.i1 )
     &                    JDAhkk(1,JMOhkk(1,i1)) = i0
                     IF ( JDAhkk(2,JMOhkk(1,i1)).EQ.i1 )
     &                    JDAhkk(2,JMOhkk(1,i1)) = i0
                     IF ( JDAhkk(1,JMOhkk(2,i1)).EQ.i1 )
     &                    JDAhkk(1,JMOhkk(2,i1)) = i0
                     IF ( JDAhkk(2,JMOhkk(2,i1)).EQ.i1 )
     &                    JDAhkk(2,JMOhkk(2,i1)) = i0
                     itmp = JMOhkk(1,i0)
                     JMOhkk(1,i0) = JMOhkk(1,i1)
                     JMOhkk(1,i1) = itmp
                     itmp = JMOhkk(2,i0)
                     JMOhkk(2,i0) = JMOhkk(2,i1)
                     JMOhkk(2,i1) = itmp
                     itmp = JDAhkk(1,i0)
                     JDAhkk(1,i0) = JDAhkk(1,i1)
                     JDAhkk(1,i1) = itmp
                     itmp = JDAhkk(2,i0)
                     JDAhkk(2,i0) = JDAhkk(2,i1)
                     JDAhkk(2,i1) = itmp
                     DO j = 1 , 4
                        rtmp1 = PHKk(j,i0)
                        rtmp2 = VHKk(j,i0)
                        rtmp3 = WHKk(j,i0)
                        PHKk(j,i0) = PHKk(j,i1)
                        VHKk(j,i0) = VHKk(j,i1)
                        WHKk(j,i0) = WHKk(j,i1)
                        PHKk(j,i1) = rtmp1
                        VHKk(j,i1) = rtmp2
                        WHKk(j,i1) = rtmp3
                     END DO
                     rtmp1 = PHKk(5,i0)
                     PHKk(5,i0) = PHKk(5,i1)
                     PHKk(5,i1) = rtmp1
                     itmp = IDRes(i0)
                     IDRes(i0) = IDRes(i1)
                     IDRes(i1) = itmp
                     itmp = IDXres(i0)
                     IDXres(i0) = IDXres(i1)
                     IDXres(i1) = itmp
                     itmp = NOBam(i0)
                     NOBam(i0) = NOBam(i1)
                     NOBam(i1) = itmp
                     itmp = IDBam(i0)
                     IDBam(i0) = IDBam(i1)
                     IDBam(i1) = itmp
                     itmp = IDCh(i0)
                     IDCh(i0) = IDCh(i1)
                     IDCh(i1) = itmp
                     itmp = IHIst(1,i0)
                     IHIst(1,i0) = IHIst(1,i1)
                     IHIst(1,i1) = itmp
                     itmp = IHIst(2,i0)
                     IHIst(2,i0) = IHIst(2,i1)
                     IHIst(2,i1) = itmp
                  END DO
               END IF
               ist1 = ABS(ISThkk(JMOhkk(1,idx)))
               ist2 = ABS(ISThkk(JMOhkk(2,idx)))
C
C   parton 1 (projectile side)
               IF ( ist1.EQ.21 ) THEN
                  idx1 = 1
               ELSE IF ( ist1.EQ.22 ) THEN
                  idx1 = 2
               ELSE IF ( ist1.EQ.31 ) THEN
                  idx1 = 3
               ELSE IF ( ist1.EQ.32 ) THEN
                  idx1 = 4
               ELSE IF ( ist1.EQ.41 ) THEN
                  idx1 = 5
               ELSE IF ( ist1.EQ.42 ) THEN
                  idx1 = 6
               ELSE IF ( ist1.EQ.51 ) THEN
                  idx1 = 7
               ELSE IF ( ist1.EQ.52 ) THEN
                  idx1 = 8
               ELSE IF ( ist1.EQ.61 ) THEN
                  idx1 = 9
               ELSE IF ( ist1.EQ.62 ) THEN
                  idx1 = 10
               ELSE
C                 WRITE(LOUT,*)
C    &               ' CHASTA: unknown parton status flag (',
C    &               IST1,') at entry ',JMOHKK(1,IDX),'(',IDX,')'
                  GOTO 50
               END IF
               id = IDHkk(JMOhkk(1,idx))
               IF ( ABS(id).LE.4 ) THEN
                  IF ( id.GT.0 ) THEN
                     ityp1 = 1
                  ELSE
                     ityp1 = 2
                  END IF
               ELSE IF ( ABS(id).GE.1000 ) THEN
                  IF ( id.GT.0 ) THEN
                     ityp1 = 3
                  ELSE
                     ityp1 = 4
                  END IF
               ELSE IF ( id.EQ.21 ) THEN
                  ityp1 = 5
               ELSE
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &                  ' CHASTA: inconsistent parton identity (' , id , 
     &                 ') at entry ' , JMOhkk(1,idx) , '(' , idx , ')'
                  GOTO 50
               END IF
C
C   parton 2 (target side)
               IF ( ist2.EQ.21 ) THEN
                  idx2 = 1
               ELSE IF ( ist2.EQ.22 ) THEN
                  idx2 = 2
               ELSE IF ( ist2.EQ.31 ) THEN
                  idx2 = 3
               ELSE IF ( ist2.EQ.32 ) THEN
                  idx2 = 4
               ELSE IF ( ist2.EQ.41 ) THEN
                  idx2 = 5
               ELSE IF ( ist2.EQ.42 ) THEN
                  idx2 = 6
               ELSE IF ( ist2.EQ.51 ) THEN
                  idx2 = 7
               ELSE IF ( ist2.EQ.52 ) THEN
                  idx2 = 8
               ELSE IF ( ist2.EQ.61 ) THEN
                  idx2 = 9
               ELSE IF ( ist2.EQ.62 ) THEN
                  idx2 = 10
               ELSE
C                 WRITE(LOUT,*)
C    &               ' CHASTA: unknown parton status flag (',
C    &               IST2,') at entry ',JMOHKK(2,IDX),'(',IDX,')'
                  GOTO 50
               END IF
               id = IDHkk(JMOhkk(2,idx))
               IF ( ABS(id).LE.4 ) THEN
                  IF ( id.GT.0 ) THEN
                     ityp2 = 1
                  ELSE
                     ityp2 = 2
                  END IF
               ELSE IF ( ABS(id).GE.1000 ) THEN
                  IF ( id.GT.0 ) THEN
                     ityp2 = 3
                  ELSE
                     ityp2 = 4
                  END IF
               ELSE IF ( id.EQ.21 ) THEN
                  ityp2 = 5
               ELSE
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &                  ' CHASTA: inconsistent parton identity (' , id , 
     &                 ') at entry ' , JMOhkk(1,idx) , '(' , idx , ')'
                  GOTO 50
               END IF
C
C   fill counter
               itype = ichtyp(ityp1,ityp2)
               IF ( itype.NE.0 ) THEN
                  ichcfg(idx1,idx2,itype,1) = ichcfg(idx1,idx2,itype,1)
     &               + 1
                  ngluon = JMOhkk(2,idx) - JMOhkk(1,idx) - 1
                  ichcfg(idx1,idx2,itype,2) = ichcfg(idx1,idx2,itype,2)
     &               + ngluon
 
                  NCHain = NCHain + 1
                  IF ( NCHain.GT.MAXCHN ) THEN
 
                     IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &                     ' CHASTA: NCHAIN > MAXCHN ! ' , NCHain , 
     &                    MAXCHN
                     STOP
                  END IF
                  IDXchn(1,NCHain) = idx
                  IDXchn(2,NCHain) = itype
               ELSE
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &                  ' CHASTA: inconsistent chain at entry ' , idx
               END IF
            END IF
 50      END DO
C
C write statistics to output unit
C
      ELSE IF ( Mode.EQ.1 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(/,A)')
     &         ' CHASTA: generated chain configurations'
         DO i = 1 , 10
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(/,2A)')
     &            ' -----------------------------------------' , 
     &           '------------------------------------'
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(2A)')
     &            ' p\\t         21     22     31     32     41' , 
     &           '     42     51     52     61     62'
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(2A)')
     &            ' -----------------------------------------' , 
     &           '------------------------------------'
            DO j = 1 , 10
               itot(j) = 0
               DO k = 1 , 9
                  itot(j) = itot(j) + ichcfg(i,j,k,1)
               END DO
            END DO
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,I2,5X,10I7,/)') ichsta(i)
     &           , (itot(j),j=1,10)
            DO k = 1 , 9
               isum = 0
               DO j = 1 , 10
                  isum = isum + ichcfg(i,j,k,1)
               END DO
 
               IF ( isum.GT.0 .AND. LPRi.GT.4 )
     &               WRITE (LOUt,'(1X,A5,2X,10I7)') cchtyp(k) , 
     &              (ichcfg(i,j,k,1),j=1,10)
            END DO
C           WRITE(LOUT,'(2A)')
C    &         ' -----------------------------------------',
C    &         '-------------------------------'
         END DO
C
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) ' CHASTA: MODE ' , Mode , 
     &        ' not supported !'
         STOP
      END IF
 
      END SUBROUTINE
