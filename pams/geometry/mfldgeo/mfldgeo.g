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
      Integer          Iprin,iz,ir
      real             r,z,B0/5.0/ 
      Common /brcontr/ Iprin
* --------------------------------------------------------------------------
*
   FILL MFLG(1)      ! Magnetic Field description
      version  =  2        ! field version
      Bfield   =  B0       !  field value
      RmaxInn  = 264.265   !  Inner field volume radius
      ZmaxInn  = 229.685   !  Inner field volume langth
      nrp      = 100       !  number of R nodes in the map
      nzp      = 200       !  number of Z nodes in the map
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
   do iz = 1,mflg_nzp
   {  do ir = 1,mflg_nrp
      {  z    = mflg_zm/mflg_nzp*(iz-.5)
         r    = mflg_rm/mflg_nrp*(ir-.5)
         brz  = BBTOT(z,r)*mflg_Bfield/B0
         mflg.BBZ(iz,ir) = Real(Brz)
         mflg.BBR(iz,ir) = Imag(Brz)
   }  }

end
*
* ---------------------------------------------------------------------------
*
      Subroutine   AGUFLD (x,F)
+CDE,TYPING,GCBANK,AGCLINK.
      structure MFLG { version,  Bfield,  RmaxInn, ZmaxInn,
                       Bdipole,  RmaxDip, ZminDip, ZmaxDip,
                       int nrp, int nzp, rm, zm , BBZ(nzp,nrp),BBR(nzp,nrp) }
      real         x(3),F(3),dr/0/,dz/0/,Br,BZ,r,a
      Integer      Ievent_old/-1/,ir,iz
      logical      first/.true./
*
      if (first) then
         first = .false.
*     get parameter bank locally
         Call RbPUSHD
         USE  MFLDGEO/MFLG  
         Call RbPOPD
         if (mflg_Nrp>0) dr=mflg_Rm/mflg_Nrp
         if (mflg_Nzp>0) dz=mflg_Zm/mflg_Nzp
      endif
*
      F = {0,0,0}
      r = sqrt(x(1)*x(1)+x(2)*x(2))
      a = abs(x(3))
      if (mflg_version==1 ) then

         If ( r < mflg_RmaxInn & a < mflg_ZmaxInn ) F(3) = mflg_Bfield

      else if (mflg_version>1 ) then

         if (r<mflg_Rm & a<mflg_Zm) then
*            ir   = nint(r/dr)
*            iz   = nint(a/dz)
             ir   = 1+r/dr
             iz   = 1+a/dz
            Br   = mflg.BBR(iz,ir)
            Bz   = mflg.BBZ(iz,ir)
            F(1) = BR*x(1)/r
            F(2) = BR*x(2)/r
            F(3) = BZ
         endif
      endif

*     beam dipole   
      if (a>mflg_ZminDip & a<mflg_ZmaxDip & r<mflg_RmaxDip) 
         { F(2)=sign(mflg_BDipole,x(3));  F(3)=abs(F(2))/1000. }
      END

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      Function  BBTOT (z,r)
+CDE,typing.
      common    structure BFLD 
                { version, char code, date, int kz, 
                  rmaxx, zmaxx, rrm, zz1, zz2 }
      common    structure BFIT 
                { int nr, int nzc, int nzs, rmax, zmax,
                  bzcorn, zterm(20), rterm(20), bn(20) }
      complex   BBTOT,BBINTER,BBEXTER,zero/0/
      real      z, r, az, d/0/
      logical   first /.true./
*
      if (first) then
         call  RBPUSHD
         USE  /detm/mfld/BFLD
         USE   BFIT  
         CALL  RBPOPD
         d     = bfld_zmaxx-bfld_zz1
         first =.false.
      endif

      if   bfld_version<1    { BBTOT = zero; return  }
*
      az = abs(z)  
      if      az<bfld_zz1    { BBTOT = BBINTER(az,r) }
      elseif  az>bfld_zz2    { BBTOT = zero          }
      elseif  az>bfld_zmaxx  { BBTOT = BBEXTER(az,r) }
      else  { BBTOT = ((bfld_zmaxx-az)*BBINTER(az,r) +
     >                 (az-bfld_zz1)*BBEXTER(az,r))/d} 
      BBTOT=BBTOT/1000.
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
      real             r,z,ri,zi,dr/5./, dz/5./, dd/1/
      common /brcontr/ iprin      

      BBINTER=zero
      if (z>bfld_zmaxx | R>bfld_rmaxx) return
 
      n =0
      mz=dd*(r-bfit_rmax)/dr; mz=max(mz,0)
      mr=dd*(z-bfit_zmax)/dz; mr=max(mr,0)
      do iz=-mz,mz
      { do ir=-mr,mr
        {  ri=r+dr*ir;    zi=z+dz*iz
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
      real     besjn,besi,derbsj,derbsi, pi/3.141592653/
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
        FITz += c*BESJN (0,bfit_bn(i)*rho)*cosh(bfit_bn(i)*zeta)
        FITr += c*DERBSJ(0,bfit_bn(i)*rho)*sinh(bfit_bn(i)*zeta)
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
      real      z,r,wz,br,bz,wr1,wr2,br1,br2,bz1,bz2
      integer   JBBB,ier,ir1,ir2,nz/0/,iprin,Lout/6/
      common /bfcontr/ iprin      
*
      Unless bdot_Zi <= z&z <= bdat_Zi 
      {  call  RBPUSHD
         if z<=bdot_Zi { nz=0; Bdat_Zi=0; }
         for (ier=0;  z>=bdat_zi & Ier==0;) 
         {
            call   UCOPY (bank_BDAT,bank_BDOT,len_BDAT)
            nz+=1; USE /detm/mfld/BFLD/BDAT(nz) Stat=ier 
            prin3  z,nz,bdot_zi,bdat_zi,ier; 
            (' z,nz,z1-z2 =',f10.1,i5,2f10.1,' ier=',ier)
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

      wz  = (bdat_Zi-z)/(bdat_Zi-bdot_Zi)
      Br  = wz*Br1+(1-wz)*Br2      
      Bz  = wz*Bz1+(1-wz)*Bz2      
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
      common /bfcontr/  iprin 
      iprin = 10     
      b = bbtot(z,r)
      print *,' z,r =',z,r,'  B=> ',b
      iprin =  0     
      end

      function   BBR(z,r)
      implicit   none
      complex    BBTOT
      real       BBR,BBZ,BRR,BZZ,z,r,x(3),B(3)
      BBR = Imag(BBTOT(z,r))
      return
      entry      BBZ(z,r)
      BBZ = Real(BBTOT(z,r))
      return
      entry      BRR(z,r)
      x   = {0.,r,z};  call agufld(x,B);  BRR = B(2)
      return
      entry      BZZ(z,r)
      x   = {0.,r,z};  call agufld(x,B);  BZZ = B(3)
      return
      end
