
      SUBROUTINE DT_SIGTBL(Jp,Jt,Ptot,Sige,Mode)
 
C***********************************************************************
C This version dated 18.11.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DLARGE , dplab , dumzer , OHALF , ONE , pbin , 
     &                 PHI , plab , plab1x , plab2x , plabhx , plablx , 
     &                 plabx , PLO , Ptot , ratx , sig1 , sig2 , Sige , 
     &                 sigen
      DOUBLE PRECISION sigep , sigtot , TINY10 , TINY2 , ZERO
      INTEGER i , i1 , i2 , idsig , idx , iproj , Jp , Jt , Mode , NBINS
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY2=1.0D-2,ZERO=0.0D0,DLARGE=1.0D10,
     &           OHALF=0.5D0,ONE=1.0D0)
      PARAMETER (PLO=0.01D0,PHI=20.0D0,NBINS=150)
 
      LOGICAL linit
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
 
      DIMENSION sigep(5,NBINS+1) , sigen(5,NBINS+1) , idsig(23)
      DATA idsig/1 , 0 , 0 , 0 , 0 , 0 , 0 , 2 , 0 , 0 , 0 , 0 , 3 , 4 , 
     &     0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 5/
      DATA linit/.FALSE./
 
C precalculation and tabulation of elastic cross sections
      IF ( ABS(Mode).EQ.1 ) THEN
         IF ( Mode.EQ.1 ) OPEN (LDAt,FILE='outdata0/sigtab.out',
     &        STATUS='UNKNOWN')
         plablx = LOG10(PLO)
         plabhx = LOG10(PHI)
         dplab = (plabhx-plablx)/DBLE(NBINS)
         DO i = 1 , NBINS + 1
            plab = plablx + DBLE(i-1)*dplab
            plab = 10**plab
            DO iproj = 1 , 23
               idx = idsig(iproj)
               IF ( idx.GT.0 ) THEN
C                 CALL DT_SIHNEL(IPROJ,1,PLAB,SIGEP(IDX,I))
C                 CALL DT_SIHNEL(IPROJ,8,PLAB,SIGEN(IDX,I))
                  dumzer = ZERO
                  CALL DT_XSHN(iproj,1,plab,dumzer,sigtot,sigep(idx,i))
                  CALL DT_XSHN(iproj,8,plab,dumzer,sigtot,sigen(idx,i))
               END IF
            END DO
            IF ( Mode.EQ.1 ) THEN
               WRITE (LDAt,99010) plab , (sigep(idx,i),idx=1,5) , 
     &                (sigen(idx,i),idx=1,5)
99010          FORMAT (F5.1,10F7.2)
            END IF
         END DO
         IF ( Mode.EQ.1 ) CLOSE (LDAt)
         linit = .TRUE.
      ELSE
         Sige = -ONE
         IF ( linit .AND. (Jp.LE.23) .AND. (Ptot.GE.PLO) .AND. 
     &        (Ptot.LE.PHI) ) THEN
            idx = idsig(Jp)
            IF ( (idx.GT.0) .AND. ((Jt.EQ.1) .OR. (Jt.EQ.8)) ) THEN
               plabx = LOG10(Ptot)
               IF ( plabx.LE.plablx ) THEN
                  i1 = 1
                  i2 = 1
               ELSE IF ( plabx.GE.plabhx ) THEN
                  i1 = NBINS + 1
                  i2 = NBINS + 1
               ELSE
                  i1 = INT((plabx-plablx)/dplab) + 1
                  i2 = i1 + 1
               END IF
               plab1x = plablx + DBLE(i1-1)*dplab
               plab2x = plablx + DBLE(i2-1)*dplab
               pbin = plab2x - plab1x
               IF ( pbin.GT.TINY10 ) THEN
                  ratx = (plabx-plab1x)/(plab2x-plab1x)
               ELSE
                  ratx = ZERO
               END IF
               IF ( Jt.EQ.1 ) THEN
                  sig1 = sigep(idx,i1)
                  sig2 = sigep(idx,i2)
               ELSE
                  sig1 = sigen(idx,i1)
                  sig2 = sigen(idx,i2)
               END IF
               Sige = sig1 + ratx*(sig2-sig1)
            END IF
         END IF
      END IF
 
      END SUBROUTINE
