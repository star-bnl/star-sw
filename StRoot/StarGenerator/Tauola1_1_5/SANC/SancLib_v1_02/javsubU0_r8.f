      COMPLEX*16 FUNCTION javsubU0(Qs,Us,mtp2,mbt2,mv2)
*------------------------------------------------------
      REAL*8 Qs,Us,zet2,rmv2,eps
      COMPLEX*16 mtp2,mbt2,mv2,ieps,XSPENZ
      PARAMETER (eps=1D-20)
      PARAMETER (zet2=1.6449340668482264364724151666460251D0)
      ieps=DCMPLX(0d0,eps)
      rmv2=DREAL(mv2)
*
      javsubU0=
     &+1d0/Qs*(
     &   +CDLOG(-Qs/(Qs-ieps))*CDLOG((Qs+mv2)/rmv2)
     &   -1d0/2*CDLOG(-mv2/(Qs+mv2))**2 
     &   +1d0/2*CDLOG(+mv2/(Qs+mv2))**2
     &   -3*zet2)
     &-1d0/(mtp2+Us)*(
     &   +CDLOG(Qs*mtp2/(mtp2+Us)**2)*CDLOG((Qs+mv2)/mv2)  
     &   -1d0/2*CDLOG(mv2/(Qs+mv2))**2     
     &   -XSPENZ((Us+mtp2-mv2)/(-mv2))     
     &   -XSPENZ(mv2/(Qs+mv2))
     &   +2*zet2     )
*---------------------
      RETURN
      END
