      character*40 ntfilename

      integer numevt
      real northnt
      real smdx,smdy
      real axsum,axmean,axsig
      real aysum,aymean,aysig
      real bbct
      common/stordat/
     $     numevt,
     $     northnt(12),
     $     smdx(60),smdy(100),                       !160x R*4
     $     axsum,axmean,axsig,                       !  3x R*4
     $     aysum,aymean,aysig,                       !  3x R*4
     $     bbct(3)                                   !  3x R*4

*     Definitions for Intuple=12,40
      integer n4evt
      integer npkpar
      real recon_mass,recon_ener,recon_phi,recon_eta
      real recon_zg
      real smx1_sum,smx1_cnt,smx1_wid,smx2_sum,smx2_cnt,smx2_wid
      real smy1_sum,smy1_cnt,smy1_wid,smy2_sum,smy2_cnt,smy2_wid
      real smxq,smyq
      real towen
      real prent1,prent2
      real southnt,topnt,botnt
      real tnsum,tssum,ttsum,tbsum
      real smxsum,smysum
      real nttim
      real tpczv, lcppt, lcpeta, lcpphi
      integer token, trgwd, bunch, bunch48, nprim
      integer bbcadc, bbctdc
      common/st11dat/
     $     n4evt,                                         ! 1x I*4
     $     npkpar,                                        ! 1x I*4
     $     recon_mass,recon_ener,recon_phi,recon_eta,     ! 4x R*4
     $     recon_zg,                                      ! 1x R*4
     $     smx1_sum,smx1_cnt,smx1_wid,                    ! 3x R*4
     $     smx2_sum,smx2_cnt,smx2_wid,                    ! 3x R*4
     $     smy1_sum,smy1_cnt,smy1_wid,                    ! 3x R*4
     $     smy2_sum,smy2_cnt,smy2_wid,                    ! 3x R*4
     $     smxq,smyq,                                     ! 2x R*4
     $     towen(12),                                     !12x R*4
     $     prent1(12),prent2(12),                         !24x R*4
     $     southnt(16),topnt(16),botnt(16),               !48x R*4
     $     tnsum,tssum,ttsum,tbsum,                       ! 4x R*4
     $     smxsum,smysum,                                 ! 2x R*4
     $     nttim(7),                                      ! 7x R*4
     $     tpczv,                                         ! 1x R*4
     $     token,bunch,bunch48,trgwd,                     ! 2x I*4
     $     nprim,lcppt,lcpeta,lcpphi,                     ! 2x I*4
     $     bbcadc(32),bbctdc(32)                          
*                                                          -------
*                                                   total 121x R*4
      integer runnum
      integer ntcnt,ntfilenum,f_cwn
      common/ntpar/runnum,ntcnt,ntfilenum,f_cwn

