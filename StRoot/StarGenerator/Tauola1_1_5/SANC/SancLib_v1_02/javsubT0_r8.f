      COMPLEX*16 FUNCTION javsubT0(Qs,Ts,mbt2,mtp2,mv2)
*------------------------------------------------------
      IMPLICIT NONE!
      REAL*8 Qs,Ts
      REAL*8 rmtp2,rmbt2,rmv2,zet2,eps,P,slamb2,x12,x22
      COMPLEX*16 mtp2,mbt2,mv2,C0,C02
      COMPLEX*16 YR,YP,YM,ieps,XSPENZ
      PARAMETER (zet2=1.6449340668482264364724151666460251D0)
      PARAMETER (eps=1D-30)
*
      rmtp2=DREAL(mtp2)
      rmbt2=DREAL(mbt2)
      rmv2 =DREAL(mv2)
      P=Qs+rmv2
      ieps=DCMPLX(0d0,eps)
      YR= (Qs+rmv2)/(Qs+rmtp2)
      YP= SQRT(ieps)
      YM=-SQRT(ieps)
*
      javsubT0=
     & -1d0/Ts*(
     & +DLOG(rmtp2*(-Qs)/(rmtp2+Qs)**2)*CDLOG((rmtp2-mv2)/(-Qs-mv2))
     &-CDLOG(rmtp2/(Ts-ieps)*(Ts/(rmtp2-rmv2)+1))*CDLOG(rmtp2/(Ts-ieps))
     & +CDLOG(rmtp2*(-P/rmtp2/(Ts-eps)-ieps))*CDLOG(rmtp2/(Ts-ieps))
     & -DLOG((rmtp2-rmv2)/rmtp2)*CDLOG(rmtp2/(Ts-ieps))
     & -1d0/2*CDLOG(-(Ts-ieps)/rmtp2)**2
     & +1d0/2*CDLOG(-(Ts-ieps)/rmtp2/(Ts/(rmtp2-rmv2)+1))**2
     & -(CDLOG(ieps/Ts-1d0-ieps/(rmtp2-rmv2))
     &  +CDLOG(ieps/Ts-rmv2/rmtp2+ieps/(rmtp2-rmv2))
     &  -CDLOG(P/rmtp2-ieps))*CDLOG((-Qs)/(Ts-ieps))
     & +CDLOG((P-ieps)/rmv2)*CDLOG((Qs-ieps)/(-Qs))
     & -1d0/2*CDLOG(rmv2/(Qs+rmv2-ieps))**2
     & -XSPENZ(rmv2/(Qs+rmv2-ieps))
     & +XSPENZ(1d0/(Ts-ieps)/(1/(rmtp2-rmv2)+1/Ts))
     & -XSPENZ(1d0/(rmv2/rmtp2-ieps/(rmtp2-rmv2)+ieps/Ts))
     & -XSPENZ(1d0/((rmtp2-rmv2)/rmtp2-ieps*(1/Ts-1/(rmtp2-rmv2))))
     &         )
*
     & +1d0/(rmtp2+Qs)*(
     &(-1d0/2*CDLOG(-1d0/((P-ieps)/(rmtp2-rmv2)))
     & -DLOG((-Qs)/(rmtp2+Qs)) )*CDLOG((-rmtp2+rmv2)/(P-ieps))
     & -CDLOG(-(1+Qs/rmtp2+ieps/(rmtp2-rmv2)))
     &               *DLOG(DABS((1+Qs/rmtp2)/(Qs/rmtp2)))
     & -CDLOG(-((rmv2+Qs)/rmtp2-ieps/(rmtp2-rmv2)))
     &               *DLOG(DABS((1+Qs/rmtp2)/(Qs/rmtp2)))
     & +CDLOG((P-ieps)/rmtp2*(1+Qs/rmtp2)-ieps)
     &               *DLOG(DABS((1+Qs/rmtp2)/(Qs/rmtp2)))
     & +XSPENZ((1+Qs/rmtp2)/(1+Qs/rmtp2+ieps/(rmtp2-rmv2)))
     & -XSPENZ((Qs/rmtp2)/(1+Qs/rmtp2+ieps/(rmtp2-rmv2)))
     & +XSPENZ((1+Qs/rmtp2)/((rmv2+Qs)/rmtp2-ieps/(rmtp2-rmv2)))
     & -XSPENZ((Qs/rmtp2)/((rmv2+Qs)/rmtp2-ieps/(rmtp2-rmv2)))
     & +XSPENZ(Qs/rmtp2/((1+Qs/rmtp2)-ieps))
     & -XSPENZ(DCMPLX(-1d0/((rmtp2+Qs)/(rmtp2-rmv2)-1),0d0))
     & +XSPENZ(1d0/((rmtp2-rmv2)/rmtp2+ieps/(rmtp2-rmv2)))
     &                 )
*
     & -(1d0/(rmtp2+Qs)+1d0/Ts)*(
     &+1d0/2*CDLOG((-rmtp2+mv2)/rmv2)
     &  *( CDLOG((-Qs-mv2)/rmv2)+2d0*CDLOG(-rmtp2/Qs*((Qs+mv2)/rmv2)**2)
     &    -CDLOG(-(-rmtp2+mv2)**2/rmv2**2))
     &-1d0/2*CDLOG((Qs+mv2)/rmv2)
     &     *(-CDLOG(-(Qs+mv2)*rmv2*rmv2/(Qs+rmv2)**3)
     &       +DLOG((+rmtp2-rmv2)/rmv2))
     &   -XSPENZ((Qs+mv2)/rmv2)
     &   +2d0*zet2
     &   +XSPENZ((-rmtp2+mv2)/rmv2)
     &   -XSPENZ((rmv2-rmtp2)/(Qs+rmtp2)/(YR-YP))
     &   -XSPENZ((rmv2-rmtp2)/(Qs+rmtp2)/(YR-YM))
     &                          )
*
      RETURN
      END
