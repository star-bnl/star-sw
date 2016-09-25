      COMPLEX*16 FUNCTION B0D(Q2,MU2,M12,M22)
*     ---------------------------------------
* general B0D (subtracted functions); CalcPHEP group
*
      IMPLICIT NONE
      REAL*8 MU2,Q2,RBB,RTB,LNRBB,LNRTB,DREAL,DLOG,DRM12,DRM22
      COMPLEX*16 M12,M22,B0F,B0P,B0D1,B0D2,B0DP,B0DW,DCMPLX
      RETURN
*
      ENTRY B0D1(Q2,MU2,M12,M22)
*     --------------------------
      RTB=-Q2/DREAL(M22)
      IF(RTB.LE.1D-3) THEN
       LNRTB=DLOG(RTB)
       B0D=DCMPLX(LNRTB+1D0/2+RTB*(2D0*LNRTB+5D0/3)
     &            +RTB**2*(5D0*LNRTB+59D0/12)
     &            +RTB**3*(14D0*LNRTB+449D0/30)
     &            +RTB**4*(42D0*LNRTB+1417D0/30)
     &            +RTB**5*(132D0*LNRTB+16127D0/105),0D0)
      ELSE
       B0D=1D0/RTB*(B0F(Q2,MU2,M12,M22)+DLOG(DREAL(M22)/MU2)-1D0)
      ENDIF
      RETURN
*     
      ENTRY B0D2(Q2,MU2,M12,M22)
*     --------------------------
      RTB=-Q2/DREAL(M22)
      IF(RTB.LE.1D-3) THEN
       LNRTB=DLOG(RTB)
       B0D=DCMPLX(2D0*LNRTB+5D0/3+RTB*(5D0*LNRTB+59D0/12)
     &            +RTB**2*(14D0*LNRTB+449D0/30)
     &            +RTB**3*(42D0*LNRTB+1417D0/30)
     &            +RTB**4*(132D0*LNRTB+16127D0/105)
     &            +RTB**5*(429D0*LNRTB+429697D0/840),0D0)
      ELSE
       B0D=1D0/RTB**2*(B0F(Q2,MU2,M12,M22)+DLOG(DREAL(M22)/MU2)-1D0
     &                 -RTB*(DLOG(DREAL(M12)/DREAL(M22))+1D0/2))
      ENDIF
      RETURN
*
      ENTRY B0DP(Q2,MU2,M12,M22)
*     --------------------------
      RTB=-Q2/DREAL(M22)
      IF(RTB.LE.1D-3) THEN
       LNRTB=DLOG(RTB)
       B0D=DCMPLX(-LNRTB-17D0/6-RTB*(7D0*LNRTB+133D0/12)
     &            -RTB**2*(31D0*LNRTB+2657D0/60)
     &            -RTB**3*(126D0*LNRTB+5231D0/30)
     &            -RTB**4*(498D0*LNRTB+142991D0/210)
     &            -RTB**5*(1947D0*LNRTB+2225231D0/840),0D0)
      ELSE
       B0D=1D0/RTB*((1D0+2D0*RTB)*DREAL(M22)*B0P(Q2,M12,M22)+1D0/2)
      ENDIF
      RETURN
*
      ENTRY B0DW(Q2,MU2,M12,M22)
*     --------------------------
      DRM12=DREAL(M12)
      DRM22=DREAL(M22)
      RTB=-Q2/DRM22
      RBB=DRM12/DRM22
      IF(RTB.LT.1D-3) THEN
       IF(RBB.NE.0D0.AND.RBB.LT.1D-3) THEN
        LNRBB=DLOG(RBB)
        B0D=DCMPLX(1D0/2+RBB+RBB**2+RBB**3+RBB**4
     &            +LNRBB*RBB*(1D0+2D0*RBB+3D0*RBB**2+4D0*RBB**3)
     &  +RTB*(1D0/6+13D0/6*RBB+37D0/6*RBB**2+73D0/6*RBB**3 
     &            +LNRBB*RBB*(1D0+5D0*RBB+14D0*RBB**2))
     &  +RTB**2*(1D0/12+17D0/6*RBB+63D0/4*RBB**2
     &            +LNRBB*RBB*(1D0+9D0*RBB))
     &  +RTB**3*(1D0/20+199D0/60*RBB+LNRBB*RBB)
     &  +RTB**4*1D0/30,0D0)
       ELSEIF(RBB.EQ.0D0) THEN
        B0D=DCMPLX(1D0/2+1D0/6*RTB+1D0/12*RTB**2+1D0/20*RTB**3
     &                                          +1D0/30*RTB**4,0D0)
       ELSEIF(DRM12.GE.1D0) THEN
        B0D=1D0/RTB*(B0F(Q2,MU2,M12,M22)+DLOG(DREAL(M22)/MU2)-1D0) 
       ELSE
        PRINT *,'RTB,BB=',RTB,RBB
        PRINT *,'1) Non-forseen situation with B0DW'
         STOP
       ENDIF
      ELSE
       IF(RBB.NE.0D0) THEN
        B0D=1D0/RTB*((1D0-RBB)*(B0F(Q2,MU2,M12,M22)
     &      +DLOG(DREAL(M22)/MU2)-1D0)-RBB*DLOG(DREAL(M12)/DREAL(M22)))
       ELSEIF(RBB.EQ.0D0) THEN
        B0D=1D0/RTB*(B0F(Q2,MU2,M12,M22)+DLOG(DREAL(M22)/MU2)-1D0)
       ELSE
        PRINT *,'RTB,BB=',RTB,RBB
        PRINT *,'2) Non-forseen situation with B0DW'
         STOP
       ENDIF
      ENDIF
      RETURN
*
      END
