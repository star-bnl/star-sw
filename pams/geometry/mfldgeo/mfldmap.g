#if 0
Module    MFLDMAP  is the realistic field data map
author    Pavel Nevski
created   may 20, 2001
+CDE, GCBANK,AGCLINK.
structure MFLM { int version, char level, char map, field }

structure BMAP { int Nr, int Nz, int Np, RR(nr), ZZ(nz), PP(Np),
                         BBR(Nr,Nz), BBZ(Nr,Nz), BBP(Nr,Nz) }

character*80 star     /'.'/,
             magF     /'/StarDb/StMagF/'/,
             fnames(2)/'bfield_half_positive_3D.dat',
                       'bfield_full_positive_3D.dat'/

character*120 file
integer       LENOCC,iprin,Li/36/,ip,iz,ir,np,nz,nr,i,K
Real          B0/5/,r,z,p,bz,br,bp,B,D

call getenv('STAR',star)

fill MFLM ! STAR measured map
  version = 3     ! version
  field   = B0    ! field value
  level   = 'dev' ! star level
endfill

USE  MFLM

I=nint(2*MFLM_field/B0); B=I*B0/2; d=abs(B-MFLM_Field); K=2/I
file = %L(star)//%L(magf)//%L(fnames(abs(i)))
open (li,file=file,status='old',err=:e:)
Read(Li,'(/3(/i2)/)',err=:e:) nz,nr,np; 

Fill BMAP ! Steve Trentelange map
  nz=nz   ! number of z-nodes 
  nr=nr   ! number of R-nodes  
  np=np   ! number of phi-nodes  
  RR=0    ! list od R-nodes
  ZZ=0    ! list od Z-nodes
  PP=0    ! list od Phi-nodes
  BBZ=0   ! longitudinal field component
  BBR=0   ! radial   field component
  BBP=0   ! azimouth field component
  do ip=1,np
    do iz=1,nz
      do ir=1,nr
        read(li,*) r,z,p,br,bz,bp
        bmap.rr(ir)=r
        bmap.zz(iz)=z
        bmap.pp(ip)=p
        bmap.bbr(ir,iz)+=br/np*K
        bmap.bbz(ir,iz)+=bz/np*K
        bmap.bbp(ir,iz)+=bp/np*K
      enddo
    enddo
  enddo
endfill
close (li)
return
:e: STOP '< error in MFLMDAT: field map not found or bad >'
end
#endif
