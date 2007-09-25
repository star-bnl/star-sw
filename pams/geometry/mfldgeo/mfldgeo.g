#if 0
******************************************************************************
Module     MFLDGEO  is the actual GUFLD routine for GSTAR
  author   Pavel Nevski
  created  december 1, 1998
* created  11-apr-96
******************************************************************************
+CDE,GCBANK,AGCLINK,GCUNIT.
      structure MFLG { version,  Bfield,  RmaxInn, ZmaxInn,
                       Bdipole,  RmaxDip, ZminDip, ZmaxDip,
                       int nrp, int nzp, rm, zm , BBZ(nzp,nrp),BBR(nzp,nrp) }
      Complex          Brz,BBTOT
      Integer          Iprin,iz,ir,nz
      real             Real,Imag,r,z,B0/5.0/,z0 
      Common /brcontr/ Iprin
* --------------------------------------------------------------------------
*
   Begin
   FILL MFLG(1)      ! Magnetic Field description
      version  =  2        ! field version
      Bfield   =  B0       !  field value
      RmaxInn  = 264.265   !  Inner field volume radius
      ZmaxInn  = 312.500   !  Inner field volume length
      nrp      = 200       !  number of R nodes in the map
      nzp      = 800       !  number of Z nodes in the map
      zm       = 800.0     !  map max length
      rm       = 400.0     !  map max radius
      BBZ      = 0         !  axial field map
      BBR      = 0         !  radial field map
      Bdipole  = -42.67    !  field value (kG)
      Rmaxdip  =  15.34    !  Inner field volume radius
      Zmindip  = 980.0     !  StArt of the DX mAgnet in Z
      Zmaxdip  = 1350.0    !  End of the DX mAgnet in Z
   endfill

   USE  MFLG       " version=%Igeom "
*  drop the previous field routine, if any 
   Call CSRMSL ('AGUFLD')   
   if (mflg_version<=1) return
*
   call mflddat
   call mfldmap

   do iz = 1,mflg_nzp
   {  do ir = 1,mflg_nrp
      {  z    = mflg_zm/mflg_nzp*(iz-.5)
         if (mflg_version>=3) z=-mflg_zm + 2*z
         r    = mflg_rm/mflg_nrp*(ir-.5)
         brz  = BBTOT(z,r)*mflg_Bfield/B0
         mflg.BBZ(iz,ir) = Real(Brz)
         mflg.BBR(iz,ir) = Imag(Brz)
   }  }

end
*
* ---------------------------------------------------------------------------
*
      real function   AGUFLD (x,F)
+CDE,TYPING,GCBANK,AGCLINK.
      structure MFLG { version,  Bfield,  RmaxInn, ZmaxInn,
                       Bdipole,  RmaxDip, ZminDip, ZmaxDip,
                       int nrp, int nzp, rm, zm , BBZ(nzp,nrp),BBR(nzp,nrp) }
      real         x(3),F(3),dr/0/,dz/0/,Br,BZ,r,z,a
      Integer      Ievent_old/-1/,ir,iz,Istat/0/
      logical      first/.true./
*
      F = {0,0,0}
*
      if (first) then
         if (NZEBRA<=0) then
           print *,' AGUFLD: ZEBRA is not initialised yet '
           return
         endif
         Call RbPUSHD
*     get parameter bank locally
         USE  MFLDGEO/MFLG stat=Istat  
         if (Istat<0) then
           print *,' AGUFLD: Magnetic Field Description is not available '
           return
         endif
         Call RbPOPD
         if (mflg_Nrp>0) dr=mflg_Rm/mflg_Nrp
         if (mflg_Nzp>0) dz=mflg_Zm/mflg_Nzp
         if (mflg_version>=3) dz=2*dz
         first = .false.
      endif
*
      r = sqrt(x(1)*x(1)+x(2)*x(2))
      z = x(3)
      a = abs(z)
      if (mflg_version==1 ) then

         If ( r < mflg_RmaxInn & a < mflg_ZmaxInn ) F(3) = mflg_Bfield

      else if (mflg_version>1 ) then

         if (r<mflg_Rm & a<mflg_Zm) then
            ir   = 1+r/dr
            iz   = 1+a/dz
            if (mflg_version>=3) iz=1+(z+mflg_Zm)/dz

            Br   = mflg.BBR(iz,ir)
            Bz   = mflg.BBZ(iz,ir)
            If (r>0) then
               F(1) = BR*x(1)/r
               F(2) = BR*x(2)/r
            endif
            F(3) = BZ
         endif
      endif

*     beam dipole   
      if (a>mflg_ZminDip & a<mflg_ZmaxDip & r<mflg_RmaxDip) 
         { F(2)=sign(mflg_BDipole,x(3));  F(3)=abs(F(2))/1000. }
      AGUFLD = 0
      END

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      Function  BBTOT (z,r)
+CDE, TYPING,GCUNIT,GCBANK,AGCLINK.
      structure MFLG { version,  Bfield,  RmaxInn, ZmaxInn }

      common    structure BFLD 
                { version, char code, date, int kz, 
                  rmaxx, zmaxx, rrm, zz1, zz2 }
      common    structure BFIT 
                { int nr, int nzc, int nzs, rmax, zmax,
                  bzcorn, zterm(20), rterm(20), bn(20) }
      common    structure BMAP 
                { int Nr, int Nz, int Np, RR(nr), ZZ(nz), PP(Np),
                  BBR(Nr,Nz), BBZ(Nr,Nz), BBP(Nr,Nz) }
      Complex   BBTOT,BSINTER,BBINTER,BBEXTER,zero/0/
      Real      z, r, az,rm,zm,wr,wz,w, d/0/,zmax/0/,rmax/0/
      Integer   Istat /0/
      logical   first /.true./
*
      if (first) then
         call  RBPUSHD
         USE  /detm/mfld/MFLG        stat=istat
         USE  /detm/mfld/BFLD
         USE             BFIT  
         USE  /detm/mfld/MFLM/BMAP   
         CALL  RBPOPD       
         d     = bfld_zmaxx-bfld_zz1
         zmax  = bmap.zz(bmap_nz)
         rmax  = bmap.rr(bmap_nr)
         first =.false.
      endif

      if   bfld_version<1    { BBTOT = zero; return  }
*     BBTOT = zero
      az    = abs(z)  
      if mflg_version <= 2  
      {
        if      az<bfld_zz1    { BBTOT = BBINTER(az,r) }
        elseif  az>bfld_zz2    { BBTOT = zero          }
        elseif  az>bfld_zmaxx  { BBTOT = BBEXTER(az,r) }
        else  { BBTOT = ((bfld_zmaxx-az)*BBINTER(az,r) +
     >                   (az-bfld_zz1)*BBEXTER(az,r))/d} 
      }
      else
      { 
        if      az<zmax & r<rmax             { BBTOT = BSINTER(z,r) }
        elseif  az>bfld_zz2                  { BBTOT = zero         }
        elseif  az>bfld_zmaxx | r>bfld_rmaxx { BBTOT = BBEXTER(z,r) }
        else   { wz = (az-zmax)/(bfld_zmaxx-zmax)
                 wr = ( r-rmax)/(bfld_rmaxx-rmax)
                 w  = min(max(0.,max(wz,wr)),1.)
                 rm = min(r,rmax)    
                 zm = sign(min(az,zmax),z)    
                 BBTOT = (1-w)*BSINTER(zm,rm)+w*BBEXTER(z,r)
      }        }

      BBTOT=BBTOT/1000.
      end

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      Function BSINTER(z,r)
*     measured field map interpolation
+CDE, TYPING,GCUNIT,GCBANK,AGCLINK.
      common structure BMAP { int Nr, int Nz, int Np, RR(nr), ZZ(nz), PP(Np),
                                          BBR(Nr,Nz), BBZ(Nr,Nz), BBP(Nr,Nz) }
      complex          BSINTER,zero/0/
      Real             z,r,r1,r2,zi,br1,br2,bz1,bz2,br,bz,wz1,wz2,dz/0/
      Integer          ir,iz
      Logical          first/.true./

      if (first) then
         USE MFLDGEO/MFLM/BMAP
         first = .false.
         dz = (bmap.zz(bmap_nz)-bmap.zz(1))/(bmap_nz-1)
      endif

      BSINTER=zero
      if (z<bmap.zz(1) | z>bmap.zz(bmap_nz) | R>bmap.rr(bmap_nr)) return
 
      do ir=1,bmap_Nr
         if (R<=bmap.rr(ir)) break
      enddo
      zi=(z-bmap.zz(1))/dz
      iz=min(zi+1,real(bmap_nz-1))
      wz1=iz-zi;  wz2=1-wz1;

      if (ir>1) then
         r1  = bmap.rr(ir-1)  
         br1 = bmap.bbr(ir-1,iz)*wz1+ bmap.bbr(ir-1,iz+1)*wz2
         bz1 = bmap.bbz(ir-1,iz)*wz1+ bmap.bbz(ir-1,iz+1)*wz2
      else
         r1  =-bmap.rr(1)    
         br1 =-bmap.bbr(1,iz)*wz1   - bmap.bbr(1,iz+1)*wz2
         bz1 = bmap.bbz(1,iz)*wz1   + bmap.bbz(1,iz+1)*wz2
      endif
      r2 =bmap.rr(ir)
      br2=bmap.bbr(ir,iz)*wz1+bmap.bbr(ir,iz+1)*wz2
      bz2=bmap.bbz(ir,iz)*wz1+bmap.bbz(ir,iz+1)*wz2

      br=(br1*(r2-r)+br2*(r-r1))/(r2-r1)
      bz=(bz1*(r2-r)+bz2*(r-r1))/(r2-r1)

      BSINTER = CMPLX(Bz,Br)

      end

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      Function BBINTER(z,r)
*     measured field interpolation
+CDE,TYPING,GCUNIT.
      common structure BFLD { version, char code, date, int kz, 
                       rmaxx, zmaxx, rrm, zz1, zz2 }
      common structure BFIT { int nr, int nzc, int nzs, rmax, zmax,
                       bzcorn, zterm(20), rterm(20), bn(20) }
      complex          BBINTER,BEVALB,zero/0/
      integer          Iprin,mr,mz,n,ir,iz,nn/0/
      real             r,z,za,ri,zi,dr/5./, dz/5./, dd/1/
      common /brcontr/ iprin      

      za=abs(z)
      BBINTER=zero
      if (za>bfld_zmaxx | R>bfld_rmaxx) return
 
      n =0
      mz=dd*(r-bfit_rmax)/dr; mz=max(mz,0)
      mr=dd*(za-bfit_zmax)/dz; mr=max(mr,0)
      do iz=-mz,mz
      { do ir=-mr,mr
        {  ri=r+dr*ir;    zi=za+dz*iz
           if (mz>0&ir>0 | mr>0&iz>0 | ri<0 | zi<0 ) next 
           if (zi>bfld_zmaxx | ri>bfld_rmaxx)        next
           n+=1;  BBINTER+=BEVALB(zi,ri)
      } }
      if (n>0) BBINTER=BBINTER/n;
*     if (n>nn|n==0) { nn=n; print *,'r,z,n =',r,z,n,mr,mz }
      end

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      function BEVALB(z,r)
      implicit none
      complex  BEVALB,zero/0/
      common   structure BFIT _
               { int nr, int nzc, int nzs, rmax, zmax,
                 bzcorn, zterm(20), rterm(20), bn(20) }
      real     FITz, FITr, c,r,z, rh,zet, rho,zeta, rho1,zeta1
      real     besj0,besj1,besi,derbsi, pi/3.141592653/
      integer  i,j,ier
*
      rho     = pi*r/bfit_rmax
      zeta    = pi*z/bfit_rmax
      rh      = pi*r/bfit_zmax
      zet     = pi*z/bfit_zmax
      zeta1   = pi*bfit_zmax/bfit_rmax
      rho1    = pi*bfit_rmax/bfit_zmax
      FITz    = bfit_Bzcorn
      FITr    = 0.0
      j       = 0

      do  I   = 1,bfit_nr
        c=bfit_rterm(I)/cosh(bfit_bn(i)*zeta1)
        FITz += c*BESJ0(  bfit_bn(i)*rho)*cosh(bfit_bn(i)*zeta)
        FITr -= c*BESJ1(  bfit_bn(i)*rho)*sinh(bfit_bn(i)*zeta)
      enddo
*
      do  I   = 1,bfit_nzc
          j+=1; c=bfit_zterm(I)/BESI((i-.5)*rho1,0,ier)
        FITz += c*cos((i-.5)*zet)*BESI  ((i-.5)*rh,0,ier)
        FITr += c*sin((i-.5)*zet)*DERBSI((i-.5)*rh,0,ier)
      enddo
*
      do  i   = 1,bfit_nzs
          j+=1; c=bfit_zterm(j)/BESI(i*rho1,0,ier)
        FITz += c*sin(i*zet)*BESI(i*rh,0,ier)
        FITr += c*cos(i*zet)*DERBSI(i*rh,0,ier)
      enddo

      BEVALB = CMPLX(FitZ,FitR)
*     print *,' f,r,z,BEVALB = ',f,r,z,BEVALB
      end

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      Function  BBEXTER(z,r)
      implicit  NONE 
+CDE,GCUNIT.
      structure BDAT { int N, Zi, Ri(20), Bzi(20), Bri(20) }
      structure BDOT { int N, Zi, Ri(20), Bzi(20), Bri(20) }
      complex   BBEXTER, zero/0/
      real      a,z,r,wz,br,bz,wr1,wr2,br1,br2,bz1,bz2
      integer   JBBB,ier,ir1,ir2,nz/0/,iprin
      common /brcontr/ iprin      
*
      a=abs(z)
      Unless bdot_Zi <= a&a <= bdat_Zi 
      {  call  RBPUSHD
         if a<=bdot_Zi { nz=0; Bdat_Zi=0; }
         for (ier=0;  a>=bdat_zi & Ier==0;) 
         {
            call   UCOPY (bank_BDAT,bank_BDOT,len_BDAT)
            nz+=1; USE /detm/mfld/BFLD/BDAT(nz) Stat=ier 
            prin3  z,nz,bdot_zi,bdat_zi,ier; 
            (' z,nz,z1-z2 =',f10.1,i5,2f10.1,' ier=',i5)
         }
         CALL  RBPOPD;  if ier!=0
         { prin0 z,bdat_zi; (' bexter: Z not found ',2F10.1)
           nz=0;  Bdat_Zi=0;  BBEXTER = zero;  return; 
         }
      }
*
      ir1 = jbbb(r,bdot_Ri,bdot_N)    
      wr1 = (bdot_Ri(ir1+1)-r)/(bdot_Ri(ir1+1)-bdot_Ri(ir1))  
      Br1 = wr1*bdot_Bri(ir1)+(1-wr1)*bdot_Bri(ir1+1)
      Bz1 = wr1*bdot_Bzi(ir1)+(1-wr1)*bdot_Bzi(ir1+1)

      ir2 = jbbb(r,bdat_Ri,bdat_N)    
      wr2 = (bdat_Ri(ir2+1)-r)/(bdat_Ri(ir2+1)-bdat_Ri(ir2))  
      Br2 = wr2*bdat_Bri(ir2)+(1-wr2)*bdat_Bri(ir2+1)
      Bz2 = wr2*bdat_Bzi(ir2)+(1-wr2)*bdat_Bzi(ir2+1)

      wz  = (bdat_Zi-a)/(bdat_Zi-bdot_Zi)
      Br  = wz*Br1+(1-wz)*Br2      
      Bz  = wz*Bz1+(1-wz)*Bz2      
      if (z<0) Br=-Br
      BBEXTER = cmplx(Bz,Br)

      if (Iprin>0) then
         unless (0<=wr1<=1) <w> r,wr1; (' bexter: wrong r1-node for r=',2f10.2)
         unless (0<=wr2<=1) <w> r,wr2; (' bexter: wrong r2-node for r=',2f10.2)
         unless (0<=wz <=1) <w> z,wz ; (' bexter: wrong  z-node for z=',2f10.2)
         prin5 ir1,ir2,wr1,wr2,wz,br,bz
         (' in bexter: ir=',2i3,' wr,wz=',3f6.3,' B=',3f12.1)
      endif
      end
      
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      Function JBBB(z,zzr,kz)
      implicit none
      integer  JBBB,kz,i1,i2,iz
      real     z,zzr(kz)

      if  z <zzr(1)   { jbbb=0;    return }
      if  z>=zzr(kz)  { jbbb=kz-1; return }
      i1=1; i2=kz; while i2-i1>1 
      { iz=(i2+i1)/2; if z>zzr(iz) { i1=iz } else {i2=iz } }
      jbbb=i1
      end
      

      subroutine btest(z,r)
      complex           bbtot,b
      common /brcontr/  iprin 
      iprin = 10     
      b  = bbtot(z,r)
      bz = Bzz(z,r)
      br = Brr(z,r)
      print *,' z,r =',z,r,'  B=> ',b,bz,br
      iprin =  0     
      end

      function   BBR(z,r)
      implicit   none
      real dummy,agufld
      complex    BBTOT
      real       IMAG,REAL,BBR,BBZ,BRR,BZZ,z,r,x(3),B(3)
      BBR = Imag(BBTOT(abs(z),abs(r)))
      return
      entry      BBZ(z,r)
      BBZ = Real(BBTOT(abs(z),abs(r)))
      return
      entry      BRR(z,r)
      x   = {0.,r,z};  dummy = agufld(x,B);  BRR = abs(B(2))
      return
      entry      BZZ(z,r)
      x   = {0.,r,z};  dummy = agufld(x,B);  BZZ = B(3)
      return
      end
#endif
