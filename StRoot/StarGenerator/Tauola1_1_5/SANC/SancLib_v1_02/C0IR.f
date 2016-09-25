      COMPLEX*16 FUNCTION C0IR(tHla2,M12,M22,P2)
*     ------------------------------------------
* All cases of C0IR: 
*     last changes: DB&SG on July 12, 2007 fixed CC-case
*
      IMPLICIT NONE
      INTEGER*4 Lim
      REAL*8 tHla2,P2,DRM12,DRM22,D2,DREAL
      COMPLEX*16 M12,M22,SQR,P2P,P2M,X1,X2,OMX1,OMX2,F1
*      COMPLEX*16 LOG,SQRT,DCMPLX,
      COMPLEX*16 XSPENZ
*      PARAMETER (D2=1.64493406684822618D0)
      PARAMETER (D2=1.6449340668482264364724151666460251D0)
*
      DRM12=DREAL(M12)
      DRM22=DREAL(M22) 
*
* Limiting cases ! 1d-1 for mass; 1d-2 for mass^2
*
      IF(DRM12.LE.1D-2.AND.DRM22.LE.1D-2) THEN
* M1.EQ.M2
       IF(DRM12.EQ.DRM22) THEN
        C0IR=1D0/P2*(LOG(M12/tHla2)*LOG((P2-DCMPLX(0D0,1D-30))/M12)
     &      +1D0/2D0*(LOG((P2-DCMPLX(0D0,1D-30))/M12))**2-D2)
*	print *, "1:1"
        RETURN
* M1.NE.M2
       ELSE
        C0IR=1D0/2/P2*(
     &      +(LOG(P2/M12)+LOG(P2/M22))*LOG((P2-DCMPLX(0D0,1D-30))/tHla2)
     &      -.5D0*LOG(M12/P2)**2-.5D0*LOG(M22/P2)**2-2D0*D2)
*	print *, "1:2"
        RETURN
       ENDIF
      ENDIF
*
* Limiting case M12 > 1D-2, M22 < 1D-2, P2 > 0D0
*
      IF(DRM12.GT.1D-2.AND.DRM22.LE.1D-2.AND.P2.GT.0D0) THEN
       X1=1D0
       X2=-M12/P2
       OMX2=1D0+M12/P2
       OMX1=-M22/(P2+M12)
*
       F1=1D0/(P2+M12)*(LOG(OMX2/(-X2))-LOG(OMX1/(-X1)))
       C0IR=1D0/2*F1*LOG((P2-DCMPLX(0D0,1D-30))/tHla2)  
     &     +1D0/2/(P2+M12)*(
     &  +.5D0*LOG( OMX2*(X1-X2))**2
     &  -.5D0*LOG(  -X2*(X1-X2))**2
     &  -.5D0*LOG(-OMX1*(X1-X2))**2
     &  +.5D0*LOG(   X1*(X1-X2))**2
     &  -XSPENZ(OMX2/(X1-X2))+XSPENZ((-X2)/(X1-X2))
     &  -XSPENZ((-X1)/(X2-X1)) )
*       print *, "2"
       RETURN
      ENDIF
*
* Limiting case M12 < 1D-2, M22 > 1D-2, P2 > 0D0
*
      IF(DRM12.LE.1D-2.AND.DRM22.GT.1D-2.AND.P2.GT.0D0) THEN
       X1=1D0+M22/P2
       X2=-M12/(P2+M22)
       OMX2=1D0
       OMX1=-M22/P2
*
       F1=1D0/(P2+M22)*(LOG(OMX2/(-X2))-LOG(OMX1/(-X1)))
       C0IR=1D0/2*F1*LOG((P2-DCMPLX(0D0,1D-30))/tHla2)  
     &    +1D0/2/(P2+M22)*(
     & +.5D0*LOG( OMX2*(X1-X2))**2
     & -.5D0*LOG(  -X2*(X1-X2))**2
     & -.5D0*LOG(-OMX1*(X1-X2))**2
     & +.5D0*LOG(   X1*(X1-X2))**2
     & -XSPENZ(OMX2/(X1-X2))+XSPENZ((-X2)/(X1-X2))
     & +XSPENZ(OMX1/(X2-X1))-XSPENZ((-X1)/(X2-X1)) )
*       print *, "3"
      ENDIF
*
* Special case for W->\nu\lept and W->\ud\dn decay
*
      IF(ABS(P2).LT.1D-2.AND.DRM12.NE.DRM22) THEN
         IF (DRM12.LT.1D-2) THEN
            C0IR=1D0/4/DREAL(M22)*LOG(DRM22/DRM12)
     &           *LOG(DRM22*DRM12/tHla2**2)
*       print *, "4:1"
            RETURN
         ELSEIF (DRM22.LT.1D-2) THEN
            C0IR=1D0/4/DREAL(M12)*LOG(DRM12/DRM22)
     &           *LOG(DRM12*DRM22/tHla2**2)
*       print *, "4:2"
            RETURN
         ELSE
            C0IR=1D0/4/DREAL(M12-M22)*LOG(DRM12/DRM22)
     &           *LOG(DRM12*DRM22/tHla2**2)
*       print *, "4:3"
            RETURN
         ENDIF
      ENDIF
*
* General, exact case of C0IR
*
*      print *, "5"
      SQR=SQRT(P2**2+2D0*P2*(M12+M22)+(M12-M22)**2)
      P2P=P2-M12+M22+SQR
      P2M=P2-M12+M22-SQR
      IF(ABS(DREAL(P2P)).GE.ABS(DREAL(P2M))) THEN
        X1=P2P/2D0/P2
        X2=-2D0*M12/P2P
      ELSE
        X2=P2M/2D0/P2
        X1=-2D0*M12/P2M
      ENDIF
      P2P=P2+M12-M22+SQR  
      P2M=P2+M12-M22-SQR
      IF(ABS(DREAL(P2P)).GE.ABS(DREAL(P2M))) THEN
        OMX2=P2P/2D0/P2
        OMX1=-2D0*M22/P2P
      ELSE
        OMX1=P2M/2D0/P2
        OMX2=-2D0*M22/P2M
      ENDIF
*
      F1=1D0/SQR*(LOG(OMX2/(-X2))-LOG(OMX1/(-X1)))
*
      C0IR=1D0/2*F1*LOG((P2-DCMPLX(0D0,1D-30))/tHla2)  
     &    +1D0/2/SQR*(
     & +.5D0*LOG( OMX2*(X1-X2))**2
     & -.5D0*LOG(  -X2*(X1-X2))**2
     & -.5D0*LOG(-OMX1*(X1-X2))**2
     & +.5D0*LOG(   X1*(X1-X2))**2
     & -XSPENZ(OMX2/(X1-X2))+XSPENZ((-X2)/(X1-X2))
     & +XSPENZ(OMX1/(X2-X1))-XSPENZ((-X1)/(X2-X1)) )
*
      RETURN    
      END
