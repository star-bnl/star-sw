
      SUBROUTINE DT_GETSPT(Pp1i,Ifpr1,Ifp1,Pp2i,Ifpr2,Ifp2,Pt1i,Ifta1,
     &                     Ift1,Pt2i,Ifta2,Ift2,Am1,Idch1,Am2,Idch2,
     &                     Idchai,Irej)
 
C***********************************************************************
C This version dated 12.12.94 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION Am1 , am1n , Am2 , am2n , amp1 , amp2 , amt1 , 
     &                 amt2 , b33p , b33t , cfe1 , cfe2 , diff , 
     &                 DT_RNDM , dum , ech , es , hpsp , hpst , p1
      DOUBLE PRECISION p2 , pp1 , Pp1i , pp2 , Pp2i , pt1 , Pt1i , pt2 , 
     &                 Pt2i , ptoch , ptotf , ptoti , ptotp1 , ptotp2 , 
     &                 ptott1 , ptott2 , PYMASS , redu , sfe1 , sfe2
      DOUBLE PRECISION TINY10 , TINY3 , TINY5 , xmp1 , xmp2 , xmt1 , 
     &                 xmt2 , ZERO
      INTEGER i , ic , Idch1 , Idch2 , Idchai , idum , Ifp1 , Ifp2 , 
     &        Ifpr1 , Ifpr2 , Ift1 , Ift2 , Ifta1 , Ifta2 , Irej , 
     &        irej1 , irej2 , jmshl , mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY5=1.0D-5,TINY3=1.0D-3,ZERO=0.0D0)
 
C various options for treatment of partons (DTUNUC 1.x)
C (chain recombination, Cronin,..)
      INCLUDE 'inc/dtchai'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C flags for diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtflg3'
 
      DIMENSION pp1(4) , Pp1i(4) , pp2(4) , Pp2i(4) , pt1(4) , Pt1i(4) , 
     &          pt2(4) , Pt2i(4) , p1(4) , p2(4) , Ifp1(2) , Ifp2(2) , 
     &          Ift1(2) , Ift2(2) , ptoti(4) , ptotf(4) , diff(4)
 
      ic = 0
      Irej = 0
C     B33P = 4.0D0
C     B33T = 4.0D0
C     IF ((IDCHAI.EQ.6).OR.(IDCHAI.EQ.7).OR.(IDCHAI.EQ.8)) B33P = 2.0D0
C     IF ((IDCHAI.EQ.4).OR.(IDCHAI.EQ.5).OR.(IDCHAI.EQ.8)) B33T = 2.0D0
      redu = 1.0D0
C     B33P = 3.5D0
C     B33T = 3.5D0
      b33p = 4.0D0
      b33t = 4.0D0
      IF ( IDIff.NE.0 ) THEN
         b33p = 16.0D0
         b33t = 16.0D0
      END IF
 
      DO i = 1 , 4
         ptoti(i) = Pp1i(i) + Pp2i(i) + Pt1i(i) + Pt2i(i)
         pp1(i) = Pp1i(i)
         pp2(i) = Pp2i(i)
         pt1(i) = Pt1i(i)
         pt2(i) = Pt2i(i)
      END DO
C get initial chain masses
      ptoch = SQRT((pp1(1)+pt1(1))**2+(pp1(2)+pt1(2))**2+(pp1(3)+pt1(3))
     &        **2)
      ech = pp1(4) + pt1(4)
      Am1 = (ech+ptoch)*(ech-ptoch)
      ptoch = SQRT((pp2(1)+pt2(1))**2+(pp2(2)+pt2(2))**2+(pp2(3)+pt2(3))
     &        **2)
      ech = pp2(4) + pt2(4)
      Am2 = (ech+ptoch)*(ech-ptoch)
      IF ( (Am1.LT.0.0D0) .OR. (Am2.LT.0.0D0) ) THEN
 
         IF ( IOUlev(1).GT.0 .AND. LPRi.GT.4 )
     &         WRITE (LOUt,'(1X,A,2G10.3)')
     &         'GETSPT: too small chain masses 1' , Am1 , Am2
 
         Irej = 1
         GOTO 99999
      END IF
      Am1 = SQRT(Am1)
      Am2 = SQRT(Am2)
      am1n = ZERO
      am2n = ZERO
 
      mode = 0
C      IF ((AM1.GE.3.0D0).AND.(AM2.GE.3.0D0)) THEN
C        MODE = 0
C      ELSE
C         MODE = 1
C         IF (AM1.LT.0.6) THEN
C            B33P = 10.0D0
C         ELSEIF ((AM1.GE.1.2).AND.(AM1.LT.3.0D0)) THEN
CC           B33P = 4.0D0
C         ENDIF
C         IF (AM2.LT.0.6) THEN
C            B33T = 10.0D0
C         ELSEIF ((AM2.GE.1.2).AND.(AM2.LT.3.0D0)) THEN
CC           B33T = 4.0D0
C         ENDIF
C      ENDIF
 
C check chain masses for very low mass chains
C     CALL DT_CH2RES(IFP1(1),IFP1(2),IFT1(1),IFT1(2),IDUM,IDUM,
C    &            AM1,DUM,-IDCH1,IREJ1)
C     CALL DT_CH2RES(IFP2(1),IFP2(2),IFT2(1),IFT2(2),IDUM,IDUM,
C    &            AM2,DUM,-IDCH2,IREJ2)
C     IF ((IREJ1.NE.0).OR.(IREJ2.NE.0)) THEN
C        B33P = 20.0D0
C        B33T = 20.0D0
C     ENDIF
 
      jmshl = IMShl
 
 100  ic = ic + 1
      IF ( MOD(ic,15).EQ.0 ) b33p = 2.0D0*b33p
      IF ( MOD(ic,15).EQ.0 ) b33t = 2.0D0*b33t
      IF ( MOD(ic,18).EQ.0 ) redu = 0.0D0
C     IF (MOD(IC,19).EQ.0) JMSHL = 0
      IF ( MOD(ic,20).NE.0 ) THEN
C        WRITE(LOUT,'(1X,A)') 'GETSPT: rejection '
C        RETURN
C        GOTO 9999
C     ENDIF
 
C get transverse momentum
         IF ( LINtpt ) THEN
            es = -2.0D0/(b33p**2)
     &           *LOG(ABS(DT_RNDM(Am1)*DT_RNDM(Am2))+TINY10)
            hpsp = SQRT(es*es+2.0D0*es*0.94D0)
            hpsp = hpsp*redu
            es = -2.0D0/(b33t**2)
     &           *LOG(ABS(DT_RNDM(Am1)*DT_RNDM(Am2))+TINY10)
            hpst = SQRT(es*es+2.0D0*es*0.94D0)
            hpst = hpst*redu
         ELSE
            hpsp = ZERO
            hpst = ZERO
         END IF
         CALL DT_DSFECF(sfe1,cfe1)
         CALL DT_DSFECF(sfe2,cfe2)
         IF ( mode.EQ.0 ) THEN
            pp1(1) = Pp1i(1) + hpsp*cfe1
            pp1(2) = Pp1i(2) + hpsp*sfe1
            pp2(1) = Pp2i(1) - hpsp*cfe1
            pp2(2) = Pp2i(2) - hpsp*sfe1
            pt1(1) = Pt1i(1) + hpst*cfe2
            pt1(2) = Pt1i(2) + hpst*sfe2
            pt2(1) = Pt2i(1) - hpst*cfe2
            pt2(2) = Pt2i(2) - hpst*sfe2
         ELSE
            pp1(1) = Pp1i(1) + hpsp*cfe1
            pp1(2) = Pp1i(2) + hpsp*sfe1
            pt1(1) = Pt1i(1) - hpsp*cfe1
            pt1(2) = Pt1i(2) - hpsp*sfe1
            pp2(1) = Pp2i(1) + hpst*cfe2
            pp2(2) = Pp2i(2) + hpst*sfe2
            pt2(1) = Pt2i(1) - hpst*cfe2
            pt2(2) = Pt2i(2) - hpst*sfe2
         END IF
 
C put partons on mass shell
         xmp1 = 0.0D0
         xmt1 = 0.0D0
         IF ( jmshl.EQ.1 ) THEN
 
            xmp1 = PYMASS(Ifpr1)
            xmt1 = PYMASS(Ifta1)
 
         END IF
         CALL DT_MASHEL(pp1,pt1,xmp1,xmt1,p1,p2,irej1)
         IF ( irej1.NE.0 ) GOTO 100
         DO i = 1 , 4
            ptotf(i) = p1(i) + p2(i)
            pp1(i) = p1(i)
            pt1(i) = p2(i)
         END DO
         xmp2 = 0.0D0
         xmt2 = 0.0D0
         IF ( jmshl.EQ.1 ) THEN
 
            xmp2 = PYMASS(Ifpr2)
            xmt2 = PYMASS(Ifta2)
 
         END IF
         CALL DT_MASHEL(pp2,pt2,xmp2,xmt2,p1,p2,irej1)
         IF ( irej1.NE.0 ) GOTO 100
         DO i = 1 , 4
            ptotf(i) = ptotf(i) + p1(i) + p2(i)
            pp2(i) = p1(i)
            pt2(i) = p2(i)
         END DO
 
C check consistency
         DO i = 1 , 4
            diff(i) = ptoti(i) - ptotf(i)
         END DO
         IF ( (ABS(diff(1)).GT.TINY5) .OR. (ABS(diff(2)).GT.TINY5) .OR. 
     &        (ABS(diff(3)).GT.TINY5) .OR. (ABS(diff(4)).GT.TINY5) )
     &        THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,4G10.3)')
     &            'GETSPT: inconsistencies ' , diff
            Irej = 1
            GOTO 99999
         END IF
         ptotp1 = SQRT(pp1(1)**2+pp1(2)**2+pp1(3)**2)
         amp1 = SQRT(ABS((pp1(4)-ptotp1)*(pp1(4)+ptotp1)))
         ptotp2 = SQRT(pp2(1)**2+pp2(2)**2+pp2(3)**2)
         amp2 = SQRT(ABS((pp2(4)-ptotp2)*(pp2(4)+ptotp2)))
         ptott1 = SQRT(pt1(1)**2+pt1(2)**2+pt1(3)**2)
         amt1 = SQRT(ABS((pt1(4)-ptott1)*(pt1(4)+ptott1)))
         ptott2 = SQRT(pt2(1)**2+pt2(2)**2+pt2(3)**2)
         amt2 = SQRT(ABS((pt2(4)-ptott2)*(pt2(4)+ptott2)))
         IF ( (ABS(amp1-xmp1).GT.TINY3) .OR. (ABS(amp2-xmp2).GT.TINY3)
     &        .OR. (ABS(amt1-xmt1).GT.TINY3) .OR. 
     &        (ABS(amt2-xmt2).GT.TINY3) ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,2(4G10.3,/))')
     &            'GETSPT: inconsistent masses' , amp1 , xmp1 , amp2 , 
     &           xmp2 , amt1 , xmt1 , amt2 , xmt2
C sr 22.11.00: commented. It should only have inconsistent masses for
C ultrahigh energies due to rounding problems
C        GOTO 9999
         END IF
 
C get chain masses
         ptoch = SQRT((pp1(1)+pt1(1))**2+(pp1(2)+pt1(2))
     &           **2+(pp1(3)+pt1(3))**2)
         ech = pp1(4) + pt1(4)
         am1n = (ech+ptoch)*(ech-ptoch)
         ptoch = SQRT((pp2(1)+pt2(1))**2+(pp2(2)+pt2(2))
     &           **2+(pp2(3)+pt2(3))**2)
         ech = pp2(4) + pt2(4)
         am2n = (ech+ptoch)*(ech-ptoch)
         IF ( (am1n.LT.0.0D0) .OR. (am2n.LT.0.0D0) ) THEN
 
            IF ( IOUlev(1).GT.0 .AND. LPRi.GT.4 )
     &            WRITE (LOUt,'(1X,A,2G10.3)')
     &            'GETSPT: too small chain masses 2' , am1n , am2n
            GOTO 100
         END IF
         am1n = SQRT(am1n)
         am2n = SQRT(am2n)
 
C check chain masses for very low mass chains
         CALL DT_CH2RES(Ifp1(1),Ifp1(2),Ift1(1),Ift1(2),idum,idum,am1n,
     &                  dum,-Idch1,irej1)
         IF ( irej1.NE.0 ) GOTO 100
         CALL DT_CH2RES(Ifp2(1),Ifp2(2),Ift2(1),Ift2(2),idum,idum,am2n,
     &                  dum,-Idch2,irej2)
         IF ( irej2.NE.0 ) GOTO 100
      END IF
 
      IF ( am1n.GT.ZERO ) THEN
         Am1 = am1n
         Am2 = am2n
      END IF
      DO i = 1 , 4
         Pp1i(i) = pp1(i)
         Pp2i(i) = pp2(i)
         Pt1i(i) = pt1(i)
         Pt2i(i) = pt2(i)
      END DO
 
99999 END SUBROUTINE
