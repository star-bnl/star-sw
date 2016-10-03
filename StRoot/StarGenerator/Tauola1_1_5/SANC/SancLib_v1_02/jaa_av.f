      COMPLEX*16 FUNCTION JAA(Q2,P2,M12,M22)
*-------------------------------------------
      IMPLICIT NONE!
*-------------------
      REAL*8 Q2,P2,S,PI,DLOG,DREAL
      COMPLEX*16 M12,M22,LOG,XSPENZ,DCMPLX
      DATA PI/3.14159265358979D0/
*-
      S=-Q2
      IF(DREAL(M12).EQ.0D0) THEN
***  next rows with m_e cancels in sum -> look test_box_me.frm
       JAA=1D0/(P2+M22)*(DCMPLX(0d0,PI)*LOG((P2+M22)**2/S/M22)
***  &    +1D0/(P2+M22)*(DCMPLX(0d0,PI)*LOG(S/M12)
***  & -.5D0*LOG(M12/S)**2
     & -.5D0*LOG(M22/S)**2
     & + LOG((P2+M22)**2/P2/S)*DLOG(P2/S)
     & + LOG(1D0+M22/P2)**2-2D0*XSPENZ(P2/(P2+M22)) )
      ELSE
***  complete expression
       JAA=1D0/(P2+M22)*(DCMPLX(0d0,PI)*LOG((P2+M22)**2/M12/M22)
     & -.5D0*LOG(M12/S)**2
     & -.5D0*LOG(M22/S)**2
     & + LOG((P2+M22)**2/P2/S)*DLOG(DABS(P2/S))
     & + LOG(1D0+M22/P2)**2-2D0*XSPENZ(P2/(P2+M22)) )
      ENDIF
      RETURN    
      END
 
      COMPLEX*16 FUNCTION JAV(Q2,P2,M12,M22,MZ2)
*-----------------------------------------------
      IMPLICIT NONE!
*-------------------
      REAL*8 Q2,P2,S,DREAL
      COMPLEX*16 M12,M22,MZ2
      COMPLEX*16 LOG
*-
      S=-Q2
      IF(DREAL(M12).EQ.0D0) THEN
       JAV=1D0/(P2+M22)*(
***  next raw with m_e cancel in sum -> look test_box_me.frm
***  & -LOG((Q2+MZ2)/MZ2)*LOG(S/M12)
     & -LOG((Q2+MZ2)/MZ2)*LOG((P2+M22)**2/S/M22))
      ELSE
       JAV=1D0/(P2+M22)*(
***  complete expression
     & -LOG((Q2+MZ2)/MZ2)*LOG((P2+M22)**2/M12/M22))
      ENDIF
      RETURN    
      END
 
      COMPLEX*16 FUNCTION JAVSUB(Q2,P2,MT2,MB2,MV2)
*--------------------------------------------------
      IMPLICIT NONE!
*-------------------
      REAL*8 Q2,P2,DREAL,D2
      COMPLEX*16 CZ2,MT2,MB2,MV2,RAT
      COMPLEX*16 LOG,C01,XSPENZ
      DATA D2/1.6449340668482D0/
      DATA CZ2/(0D0,-1D-40)/
*-
      IF(DREAL(MT2).EQ.0D0) THEN
       JAVSUB=-2D0/P2*(LOG((Q2+MV2)/MV2)*LOG(P2/MV2)+XSPENZ(-Q2/MV2))
      ELSE
***    complete expression
       RAT=MV2/(Q2+MV2)
       JAVSUB=1D0/(P2+MT2)*(
     & (Q2+MT2)*C01(-DREAL(MT2),-DREAL(MB2),Q2,CZ2,MT2,MV2)
     & +LOG(RAT)*LOG((P2+MT2)**2/Q2/MT2)
     & -1D0/2*LOG(RAT)**2-XSPENZ(RAT)+D2)   
      ENDIF
      RETURN    
      END

      COMPLEX*16 FUNCTION JAVLIM(Q2,P2,M12,M22,MV2)
*--------------------------------------------------
* VERSION OF JAV FOR VERY SMALL MASSES M1 AND M2
      IMPLICIT NONE!
*-------------------
      REAL*8 Q2,P2
      COMPLEX*16 M12,M22,MV2
      COMPLEX*16 LOG
*
      JAVLIM=1D0/P2*(-LOG((Q2+MV2)/MV2)*LOG(P2**2/M12/M22))
*
      RETURN    
      END
