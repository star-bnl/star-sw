      COMPLEX*16 FUNCTION D040WZ(Qs,Ps,mw2,mz2)
      REAL*8 Qs,Ps,p2s,pi,TET
      COMPLEX*16 mz2,mw2,mt2,XSPENZ
      COMPLEX*16 adx,bdx,ddx,sddx,xdpl,xdmi,blx,sdlx,xlpl,xlmi,xrt1,xrt2
      PARAMETER (pi=3.1415926535897932D0)
      p2s=1d-12
      adx=Qs*(p2s-Ps+mw2)
      bdx=p2s*mz2+Ps*(Qs+mw2-mz2)
      ddx=(Ps*(Qs+mw2+mz2)-p2s*mz2)**2-4d0*mw2*mz2*Ps*(Qs+Ps-p2s)
      sddx=SQRT(ddx)
      xdpl=(-bdx+sddx)/2d0/adx
      xdmi=(-bdx-sddx)/2d0/adx
      xrt1=-mz2/(mw2-mz2+p2s)
      xrt2=-Ps/(mw2+p2s-Ps)
      blx=Qs+mw2-mz2
      sdlx=SQRT(blx**2+4d0*mz2*Qs)
      xlpl=(blx+sdlx)/2d0/Qs
      xlmi=(blx-sdlx)/2d0/Qs
*
      D040WZ=+1d0/sddx*(
     &  +2d0*CMPLX(0d0,1d0)*PI*(
     &  +TET(-DIMAG(sdlx/Qs))*TET(+DIMAG(sdlx)*DREAL(sdlx))
     &  -TET(+DIMAG(sdlx/Qs))*TET(-DIMAG(sdlx)*DREAL(sdlx)))
     &  *(LOG((1d0-xdpl)/(-xdpl))-LOG((1d0-xdmi)/(-xdmi)))
     & +LOG(xlpl-xdpl)*LOG((1d0-xdpl)/(-xdpl))
     & -XSPENZ((1d0-xdpl)/(xlpl-xdpl))
     & +XSPENZ((   -xdpl)/(xlpl-xdpl))
     & +LOG(-xlmi+xdpl)*LOG((1d0-xdpl)/(-xdpl))
     & -XSPENZ((1d0-xdpl)/(xlmi-xdpl))
     & +XSPENZ((   -xdpl)/(xlmi-xdpl))
     & -LOG(xlpl-xdmi)*LOG((1d0-xdmi)/(-xdmi))
     & +XSPENZ((1d0-xdmi)/(xlpl-xdmi))
     & -XSPENZ((   -xdmi)/(xlpl-xdmi))
     & -LOG(-xlmi+xdmi)*LOG((1d0-xdmi)/(-xdmi))
     & +XSPENZ((1d0-xdmi)/(xlmi-xdmi))
     & -XSPENZ((   -xdmi)/(xlmi-xdmi))
     & +LOG(Qs/mw2)*LOG((1d0-xdpl)/(-xdpl))
     & -LOG(Qs/mw2)*LOG((1d0-xdmi)/(-xdmi))
     & -LOG(xdpl)*LOG((1d0-xdpl)/(-xdpl))
     & +LOG(xdmi)*LOG((1d0-xdmi)/(-xdmi))
     & +XSPENZ((1d0-xdpl)/(-xdpl))
     & -XSPENZ((1d0-xdmi)/(-xdmi)) 
     & -LOG(mz2+(mw2-mz2+p2s)*xdpl)*LOG((1d0-xdpl)/(-xdpl))
     & +LOG(Ps+(mw2+p2s-Ps)*xdpl)*LOG((1d0-xdpl)/(-xdpl))
     & +LOG(mz2+(mw2-mz2+p2s)*xdmi)*LOG((1d0-xdmi)/(-xdmi))
     & -LOG(Ps+(mw2+p2s-Ps)*xdmi)*LOG((1d0-xdmi)/(-xdmi))
     & +XSPENZ((1d0-xdpl)/(xrt1-xdpl))
     & -XSPENZ((   -xdpl)/(xrt1-xdpl))
     & -XSPENZ((1d0-xdmi)/(xrt1-xdmi))
     & +XSPENZ((   -xdmi)/(xrt1-xdmi))
     & -XSPENZ((1d0-xdpl)/(xrt2-xdpl))
     & +XSPENZ((   -xdpl)/(xrt2-xdpl))
     & +XSPENZ((1d0-xdmi)/(xrt2-xdmi))
     & -XSPENZ((   -xdmi)/(xrt2-xdmi))
     &                )
      RETURN
      END
