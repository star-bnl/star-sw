
      SUBROUTINE DT_VV2SCH
 
C***********************************************************************
C Change Valence-Valence chain systems to Single CHain systems for     *
C hadron-nucleus collisions with meson or antibaryon projectile.       *
C (Reggeon contribution)                                               *
C The single chain system is approximately treated as one chain and a  *
C meson at rest.                                                       *
C This version dated 18.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION amch1 , amch1n , amch2 , amch2n , DT_PHNSCH , 
     &                 DT_RNDM , pch1 , pch2 , pp1 , pp2 , ppe , ppz , 
     &                 pt1 , pt2 , ptot , sichap , TINY3 , TINY7 , ZERO
      INTEGER i , idchai , idr1 , idr2 , IDT_ICIHAD , IDT_IPDG2B , 
     &        idum , idxr1 , idxr2 , if , ifsc , irej1 , itmp , k , k1 , 
     &        k2 , kch , kproj , ktarg , mo
      INTEGER mo1 , mo2 , n1 , n2 , nc , nstop
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY7=1.0D-7,TINY3=1.0D-3)
 
      LOGICAL lstart
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C statistics
      INCLUDE 'inc/dtsta1'
 
      DIMENSION if(4,2) , mo(4) , pp1(4) , pp2(4) , pt1(4) , pt2(4) , 
     &          pch1(4) , pch2(4)
 
      DATA lstart/.TRUE./
 
      ifsc = 0
      IF ( lstart ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010    FORMAT (/,1X,'VV2SCH:  Reggeon contribution to valance-',
     &           'valence chains treated')
         lstart = .FALSE.
      END IF
 
      nstop = NHKk
 
C get index of first chain
      DO i = NPOint(3) , NHKk
         IF ( IDHkk(i).EQ.88888 ) THEN
            nc = i
            GOTO 100
         END IF
      END DO
 
 100  DO WHILE ( (IDHkk(nc).EQ.88888) .AND. (IDHkk(nc+3).EQ.88888) .AND. 
     &           (nc.LT.nstop) )
C get valence-valence chains
         IF ( (IDCh(nc).EQ.8) .AND. (IDCh(nc+3).EQ.8) ) THEN
C   get "mother"-hadron indices
            mo1 = JMOhkk(1,JMOhkk(1,JMOhkk(1,nc)))
            mo2 = JMOhkk(1,JMOhkk(1,JMOhkk(2,nc)))
            kproj = IDT_ICIHAD(IDHkk(mo1))
            ktarg = IDT_ICIHAD(IDHkk(mo2))
C   Lab momentum of projectile hadron
            CALL DT_LTNUC(PHKk(3,mo1),PHKk(4,mo1),ppz,ppe,-3)
            ptot = SQRT(PHKk(1,mo1)**2+PHKk(2,mo1)**2+PHKk(3,mo1)**2)
 
            sichap = DT_PHNSCH(kproj,ktarg,ptot)
            IF ( DT_RNDM(ptot).LE.sichap ) THEN
               ICVv2s = ICVv2s + 1
C   single chain requested
C      get flavors of chain-end partons
               mo(1) = JMOhkk(1,nc)
               mo(2) = JMOhkk(2,nc)
               mo(3) = JMOhkk(1,nc+3)
               mo(4) = JMOhkk(2,nc+3)
               DO i = 1 , 4
                  if(i,1) = IDT_IPDG2B(IDHkk(mo(i)),1,2)
                  if(i,2) = 0
                  IF ( ABS(IDHkk(mo(i))).GE.1000 ) if(i,2)
     &                 = IDT_IPDG2B(IDHkk(mo(i)),2,2)
               END DO
C      which one is the q-aq chain?
C        N1,N1+1 - DTEVT1-entries for q-aq system
C        N2,N2+1 - DTEVT1-entries for the other chain
               IF ( (if(1,2).EQ.0) .AND. (if(2,2).EQ.0) ) THEN
                  k1 = 1
                  k2 = 3
                  n1 = nc - 2
                  n2 = nc + 1
               ELSE IF ( (if(3,2).EQ.0) .AND. (if(4,2).EQ.0) ) THEN
                  k1 = 3
                  k2 = 1
                  n1 = nc + 1
                  n2 = nc - 2
               ELSE
                  GOTO 150
               END IF
               DO k = 1 , 4
                  pp1(k) = PHKk(k,n1)
                  pt1(k) = PHKk(k,n1+1)
                  pp2(k) = PHKk(k,n2)
                  pt2(k) = PHKk(k,n2+1)
               END DO
               amch1 = PHKk(5,n1+2)
               amch2 = PHKk(5,n2+2)
C      get meson-identity corresponding to flavors of q-aq chain
               itmp = IREsrj
               IREsrj = 0
               CALL DT_CH2RES(if(k1,1),if(k1+1,1),0,0,idr1,idxr1,ZERO,
     &                        amch1n,1,idum)
               IREsrj = itmp
C      change kinematics of chains
               CALL DT_CHKINE(pp1,IDHkk(n1),pp2,IDHkk(n2),pt1,
     &                        IDHkk(n1+1),pt2,IDHkk(n2+1),amch1,amch1n,
     &                        amch2,irej1)
               IF ( irej1.EQ.0 ) THEN
C      check second chain for resonance
                  idchai = 2
                  IF ( (if(k2,2).NE.0) .AND. (if(k2+1,2).NE.0) )
     &                 idchai = 3
                  CALL DT_CH2RES(if(k2,1),if(k2,2),if(k2+1,1),if(k2+1,2)
     &               ,idr2,idxr2,amch2,amch2n,idchai,irej1)
                  IF ( irej1.EQ.0 ) THEN
                     IF ( idr2.NE.0 ) idr2 = 100*idr2
C      add partons and chains to DTEVT1
                     DO k = 1 , 4
                        pch1(k) = pp1(k) + pt1(k)
                        pch2(k) = pp2(k) + pt2(k)
                     END DO
                     CALL DT_EVTPUT(ISThkk(n1),IDHkk(n1),n1,0,pp1(1),
     &                  pp1(2),pp1(3),pp1(4),0,0,0)
                     CALL DT_EVTPUT(ISThkk(n1+1),IDHkk(n1+1),n1+1,0,
     &                  pt1(1),pt1(2),pt1(3),pt1(4),0,0,0)
                     kch = ISThkk(n1+2) + 100
                     CALL DT_EVTPUT(kch,88888,-2,-1,pch1(1),pch1(2),
     &                  pch1(3),pch1(4),idr1,idxr1,IDCh(n1+2))
                     IDHkk(n1+2) = 22222
                     CALL DT_EVTPUT(ISThkk(n2),IDHkk(n2),n2,0,pp2(1),
     &                  pp2(2),pp2(3),pp2(4),0,0,0)
                     CALL DT_EVTPUT(ISThkk(n2+1),IDHkk(n2+1),n2+1,0,
     &                  pt2(1),pt2(2),pt2(3),pt2(4),0,0,0)
                     kch = ISThkk(n2+2) + 100
                     CALL DT_EVTPUT(kch,88888,-2,-1,pch2(1),pch2(2),
     &                  pch2(3),pch2(4),idr2,idxr2,IDCh(n2+2))
                     IDHkk(n2+2) = 22222
                  END IF
               END IF
            END IF
         END IF
 150     nc = nc + 6
      END DO
 
 
      END SUBROUTINE
