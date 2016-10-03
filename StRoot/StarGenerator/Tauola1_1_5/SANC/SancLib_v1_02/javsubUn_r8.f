      COMPLEX*16 FUNCTION javsubUn(Qs,Us,mtp2,mbt2,mv2)
*------------------------------------------------------
      REAL*8 Qs,Us,rmtp2,rmbt2,rmv2,eps,zet2,DDILOG
      REAL*8 a1,b1,slam11,yc1sp,yc1spm,yc1smp,yc1smm
      REAL*8 a2,b2,slam21
      COMPLEX*16 mtp2,mbt2,mv2,XSPENZ
      COMPLEX*16 cc1,cc2,ymin1,ymax1,ymin2,ymax2,yplus,ymins
      COMPLEX*16 ieps
      PARAMETER (eps=1D-30)
      PARAMETER (zet2=1.6449340668482264364724151666460251D0)
      rmtp2=DREAL(mtp2)
      rmbt2=DREAL(mbt2)
      rmv2 =DREAL(mv2)
      ieps=DCMPLX(0d0,eps)
*
      a1=(Us+rmbt2)**2
      b1=(Us+rmbt2)*(Us+rmtp2)-rmv2*(Us-rmbt2)
      cc1=rmtp2-rmv2+Us+ieps
      slam11=DSQRT(+rmtp2**2+rmv2**2+rmbt2**2
     &             -2*rmbt2*rmtp2-2*rmv2*rmbt2-2*rmtp2*rmv2)
      ymin1=1d0/2*(a1/cc1**2-(b1/cc1**2)**2)
      ymax1=+1d0/cc1*(slam11-cc1+b1/cc1)
      yc1spp=2*(rmtp2*rmbt2+rmtp2*Us+rmbt2*Us-rmv2*Us+Us**2)
      yc1spm=2*(rmtp2*rmbt2+rmbt2*Us)
      yc1smp=2*(rmbt2*rmv2+rmv2*Us-rmv2**2+rmtp2*rmv2**2/(Us+rmtp2))
      yc1smm=2*rmbt2*rmv2
*
      a2=a1
      b2=(Us+rmbt2)*rmbt2*(1d0+rmtp2/Us)+rmv2*(Us-rmbt2)
      cc2=rmv2-rmbt2*(1d0+rmtp2/Us)+ieps
      ymax2=1d0/cc2*((   cc1-cc2)/(1d0+rmtp2/Us)+b2/cc2)
      ymin2=1d0/cc2*((slam11-cc2)/(    rmtp2/Us)+b2/cc2)
      yplus=(a2*cc2**2-b2**2)/cc2**2/(b2+cc2*(Us+rmbt2))
      ymins=(b2+cc2*(Us+rmbt2))/cc2**2
*
      javsubUn=
     &+1d0/Qs*(CDLOG(-Qs/(Qs-ieps))*CDLOG((Qs+rmv2-ieps)/rmv2)
*              In this term ^ we trust
     & -1d0/2*CDLOG(-rmv2/(Qs+rmv2-ieps))**2
     & +1d0/2*CDLOG(+rmv2/(Qs+rmv2-ieps))**2
     & -3*zet2)
*
     &+1d0/(rmtp2+Us)*(
     &+CDLOG((rmtp2+Us)**2/(Qs-ieps)/rmtp2)*CDLOG((Qs+rmv2-ieps)/rmv2)
     &+XSPENZ(-(Us+rmtp2-rmv2+ieps)/rmv2)-zet2
     & +1d0/2*CDLOG(rmv2/(Qs+rmv2-ieps))**2
     & +XSPENZ(rmv2/(Qs+rmv2-ieps))-zet2
     &                )
* mbt addition
     &+1d0/2/(rmtp2+Us)*(
     & +DLOG((rmtp2+Us)/rmv2)*CDLOG((ymax1/ymin1))
     & +DDILOG(Us/(rmtp2+Us))
     & -XSPENZ(cc1**2*ymax1/yc1spp)-XSPENZ(cc1**2*ymax1/yc1spm)
     & +XSPENZ(cc1**2*ymax1/yc1smp)+XSPENZ(cc1**2*ymax1/yc1smm)
     &-(
     & +1d0/2*DLOG((rmtp2+Us)/rmtp2)*DLOG((rmtp2+Us)*rmtp2/rmv2**2)
     & +CDLOG((yplus))*CDLOG((ymax2/ymin2))
     & -1d0/2*CDLOG((ymax2))**2+1d0/2*CDLOG((ymin2))**2
     & -XSPENZ(-ymax2/yplus)+XSPENZ(-ymin2/yplus)
     & +XSPENZ( ymax2/ymins)-XSPENZ( ymin2/ymins)
     & )
     &                )
*-------------------
      RETURN
      END
