#if 0
      program SIMSIN            ! 28.12.2001
#else
      subroutine SIMSIN            ! 28.12.2001
#endif
Calculate single spectrum with noise
#include "simsin.inc"

C open (unit=14, file='SIMSIN.OPA',status='NEW')
      open (unit=14, file='SIMSIN.OPA')
      
      call TIMDAT(TEE)
      write (14,*) 'SIMSIN.OPA   ', TEE

      call EPSI 

      print*, ' give exth, bg'
      read *,        exth, bg

      write (14,1401) exth,bg 
 1401 format (' th=',f7.3,'cm,  bg=',f9.4)
          
      call ALCOV
      call LCON
      
      END

      subroutine ALCOV 
Convolution program with realistic energy loss spectrum for gases
C using FVP method, modified for SIMSIN
#include "simsin.inc"

C open (unit=3, file='ALCOV1.OPA',status='NEW')
C open (unit=3, file='ALCOV1.OPA')
 
        call TIMDAT(TEE)
C write (3,*) 'ALCOV1.OPA   ', TEE
 
        call PREPE

C open (unit=21, file='ALCOV1.DMP',status='new')
C open (unit=21, file='ALCOV1.DMP')
D       write (21,*) '   ALCOV1.DMP, ',TEE
D       write (21,*)   ZA, AW, rho,k1,' Z, A, rho, k1'

      nee = 1
        call PREP
        call SRUL
        call SPECT
        stpp = 1000. * tdedx
D       write (21,*)  '         ',pke,pmom,'= T,pc'
D       write (21,321) bg, beta, Emax/1.e6, stpp
 321  format (11x,1p2e11.4,'=bg, beta;  EM=',e11.4,' S=',0pf9.3)
C     write (21,*) '          ',fsg, tdedx, ' # coll/cm, S'

      if (exth .le. 0) STOP
      ners = 1
C open (unit=9, file='ALCOV1.SPE',status='new')
      open (unit=9, file='ALCOV1.SPE')
      write (9,*) '   ALCOV1.SPE, ',TEE
      write (9,*)   ZA, AW, rho
      elim = 0
      write (9,911) ners, elim,betasq, tdedx
 911      format (i3,1p3e12.5,' ners, elim,beta**2, S')

C open (unit=19, file='ALCOV1.INT',status='new')    
C open (unit=19, file='ALCOV1.INT')    
C write (19,*) '  ALCOV1.INT, f and F(delta) ',TEE
C write (19,*)   ZA, AW, rho,k1,' Z, A, rho, k1'
C write (19,*)   ners, betasq, tdedx,' ners, beta**2, S'

      call CONV

C close (3)
close (9)
C close (19)
C close (21)

        END
 
      subroutine EPSI
#include "simsin.inc"
C read epsilon data from EP1 and energy scale 

      character*8 tit(9)

 411  format (9a8)

      open (unit=11, file='BREP.DAT' ,status='OLD')

      read (11,411) tit
      write (14,411) tit
      print 411, tit
      
      read (11,411) tit
      write (14,411) tit
      print 411, tit
      
      read (11,*) ZA, AW, rhn, rho
      write (14,1411) ZA, AW, rhn, rho
 1411 format (f9.2,f9.4, 1p2e12.4,' Z, A, rho')
      print 1411,    ZA, AW, rhn, rho

      read (11,*) ear1, elsh, sbb
      write (14,1412) ear1, elsh, sbb
 1412 format (3f9.3,' E1 in MARR, Lshell, sbb')
      print 411, tit

      read (11,*) N2, k1, Emin, um, u
      write (14,1414) N2, k1, Emin, um, u
      print 1414,    N2, k1, Emin, um, u
 1414 format (i6,i5,3f10.5,'  N2, k1, Emin, um, u')
 
      read (11,*) eei, eec, coel
C     write (14,1416) eei, eec, coel
C     print 1416,    eei, eec, coel
 1416 format ( f8.3, f10.5, f8.3,' df/dE coefficients')
 
C     print*, ' which ep1? give 0, 1 or 2:  '
C     read *, nep1
      nep1 = 1
C     write (14,*) ' ep-1 used:', nep1

      do j=1,ndim
         read (11,*,end=69) ebw, ars(j), ep(j,2), ep11, ep12
         dfdE(j) = ars(j) 
         ep(j,1) = 1.0
         leh     = j
         if (nep1 .eq. 1) ep(j,1) = 1.0 + ep11
         if (nep1 .eq. 2) ep(j,1) = 1.0 + ep12
C        if ((j/50)*50 .eq. j) print*, j,ebw, E(j), dfdE(j), ep(j,1)
      end do

 69   print*, ' EPSI, leh=', leh
      
      END
 
      subroutine PREP
#include "simsin.inc"
C     include 'SIMSIN.CMM/list'
 
      print*, ' particle: p=1,pi=2,k=3,e=4,5=d:'
      read*, npm
C     print*, ' give absorber thickness (cm):  '
C     read*, exth

      PTM  = pion
      if (npm .eq. 1) PTM = prm
      if (npm .eq. 3) PTM = 493.667
      if (npm .eq. 4) PTM = elmm
      if (npm .eq. 5) PTM = 1875.587
C     if (nee .eq. 1) write (21,*) ' mass=', PTM,' MeV'
      zi   = 1.0
      atnu = 6.0222e23 * rho / Aw
 
C     write (3,601) PTM,zi
 601  format (1x,'PREP;  particle mass=',F10.3,' MeV, charge=',f3.0)

      call EVANS

      Efin = Emax
      if (Emax .gt. Etop) Efin = Etop
      saxk = 153540 * zi**2 * rho / (betasq*AW)
      Emk  = saxk * Emax
      
C     write (3,602) Za,Aw,exth,saxk,Emk
 602  format (/ '  Z=',F6.2,'  A=',f9.4,'  t=',0pf7.2,'cm',
     1     '  k/Z=',f10.5,'  k*Emax/Z=',1pe12.5)

      END
 
      subroutine EVANS
#include "simsin.inc"
C     dimension ebb(11)
C     data ebb/ 1.0,3.0,10., 30.,100.,300., 1.e3, 3.e3,1.e4,3.e4,1.e5/
 
      print*, ' Evans: ptM=',ptM
      print*, ' define input: 1=kin E/MeV, 2=momentum/MeV/c, 3=bg:   '
C     read*,    jkm
C     print*, ' give the value:  '
C     read*,    xxx

      jkm = 3
      xxx = bg

      go to (11,22,33), jkm
 
 11   pkE = xxx
      W = xxx/ptM + 1.0
      bg = sqrt(W**2 - 1.)
      go to 34
 
 22   pmom = xxx
      bg = xxx / ptM
      W = sqrt(bg**2 + 1.0)
      pkE = ptM * (W - 1.0)
      go to 35
 33   bg = xxx                
      W = sqrt(bg**2 + 1.0)
      pkE = ptM * (W - 1.0)
      
 34   pmom = ptM * bg
      
 35   betasq = bg**2 / (1 + bg**2)
      beta = bg / W
      gam  = W
      ptE  = ptM * W
      telm = 2 * elmm
      Emax = ptM * (W**2 - 1) / (ptM/telm + telm/ptM + W)
      if (npm .eq. 4) Emax = pkE / 2
      print*, npm,' Emax',Emax
      Emx  = telm * bg**2
      
C     write (3,333) bg,pmom,pkE
 333  format (/,3x,'beta-gamma=',f11.4,3x,'momentum=',f13.4,' MeV/c'/
     1     3x,'kinetic E of incident particle=',f15.3,' MeV')
C     write (3,334) betasq,gam,Emax,Emx, Etop/1.e6
 334  format (3x,'beta**2=',f9.6,3x,'gamma=',f12.5,3x,'Emax=',
     1     1p3e12.4,' MeV')
      Emax = 1.e6 * Emax
      
      END
 
      subroutine PREPE
#include "simsin.inc"
      
      nume = 1250
      u    = log(2.) / N2
      um   = exp(u)
      E(1) = Emin
      EXS  = 1.
C     print*, ' PREPE', n2, E(1),emin
C     write (3,609) N2,Emin, u,um
 609  format(/' PREPE;   N2=',I4,3x,
     1     3x,'Emin=',f8.3,3X,'u=',F9.6,' e**u=',f10.6)
      lemx = nume + 450

      do  L=1,lemx
        EXS = EXS * um
        E(L+1) = E(L) * um
C     if ((L/50)*50 .eq. L) print*, ' L,E=',L,E(L),exs,um
        DI(L)  = -log(1.0 - 1.0/EXS) / u
        DE(L)  = E(L+1) - E(L)
        H(L)   = 0.
      end do

      if (leh .gt. nume) leh = nume
      Etop = E(nume) * sqrt(um)
C     print*, ' PREPE: Efin, Etop, Emax=',Efin,Etop,Emax
      
      END
      
      subroutine SRUL
C Define parameters and calculate Inokuti's sums, 
C Sect 3.3 in Rev Mod Phys 43, 297 (1971)
#include "simsin.inc"
      dimension avi(3),sru(3)
      
      elm = 1.0e6 * elmm
      fac = 8.0 * pi * Ry**2 * bohr**2 / elm
      DEC = zi**2 * atnu * fac / betasq
      racc= 1.0 / (atnu * hbc * fanco)
C     Conversion factor for second density effect term
C     write (3,305) atnu, fac,racc, dec
 305  format ('  subroutine SRUL: # of atoms per cm**3=',1pe12.4,3x,
     1 'fac=',e11.4,'eV*cm**2,  racc=',e11.4,/'  DEC=',0pf11.4,' eV/cm')
 
      blg  = log ((2.*elm) * bg**2) - betasq
      bemx = betasq / Emax
      pf   = pkE * 1.e6
      tmcb = 2. * elm * betasq
C     write (3,307) betasq, blg, bemx, tmcb
 307  format ('  beta**2=',f12.10,'  blg=',f9.4,
     1     '  betsq/Emax=',1pe11.4,'  2mv**2=',e11.4,' eV')
 
      do  L=1,3
         avi(L)  = 0
         sru(L)  = 0
      end do
      do j=1,nume
         do L=1,3
            sru(L)  = sru(L) + dfdE(j) * dE(j) / E(j)**(2-L)
 5          avI(L)  = avI(L) + dfdE(j) * log(E(j)) * dE(j) / E(j)**(2-L)
         end do
      end do
      sso     = sru(2)
      print*, ' SRUL, sso=', sso
C     write (3,375) sru, avI
 375  format (/' S(L)=',2f12.5,f12.2,'  ln(I)=',2f12.5,f12.2)

      rIv = exp (avi(2) / sru(2) )
      smi = Ry * sru(1)
      xLmi= RY * (avi(1) - log(Ry) * sru(1) )
C     write (3,377) rIv, smi,xLmi
 377  format (3x,'I-value=', f7.2,' Ino values:',2f9.3)

C     write (3,*) '  following data without density effect'
C     these values are auxiliary output related to dE/dx etc.
C     write (3,*) '  S(0)*blg=',sru(2)*blg,'   2*L(0)=',2*avI(2)
C     write (3,*) '  S(1)*blg=',sru(3)*blg,'   2*L(1)=',2*avI(3)
      
      END
 
      subroutine SPECT
C generate collision spectrum from sigma, ep-1 and ep-2 with Allison's method
#include "simsin.inc"

D       write (3,*) 
D       write (3,*) '  subroutine SPECT'
D       write (3,308)
 308  format (3x,'j',5x,'E/eV',5x,'df/dE ',5x,'sgg',6x,'sgh',
     17x,'S 1',6x,'S 2',6x,'S 3',5x,'sum S',5x,'S(0)',3x,'dE/dx'/)
 
      jpr = 10
      jpd = 10
      do L=1,4
         rM2(L)  = 0
         Tsig(L) = 0
         STP(L)  = 0
      end do
      S0      = 0
 
C do loop for Fano Eq 47
      do  5 j=1,nume
         leh = j
         S0  = S0   + dfdE(j) * dE(j)
         if (E(j) .gt. EMAX) go to 11
         if (npm .eq. 4) uef = 1 + (E(j)/(pf-E(j)))**2 + (((gam-1)/gam)
     1        * E(j)/pf)**2 - (2*gam - 1)*E(j)/(gam**2 * (pf - E(j)))
         if (npm .ne. 4) uef = 1 - E(j) * bemx     
C     uef from Uehling Eqs. 9 & 2
C     if (j .eq. 1) print*, ' uef=',uef
         sig(j,1) = E(j) * dfdE(j) * log(tmcb / E(j) ) 
 14      epbe = 1.0 - betasq * ep(j,1)               
C     Fano Eq 47
         if (epbe .eq. 0) epbe = 1e-20
         sgg = E(j) * dfdE(j)*(-.5)*log(epbe**2+(betasq*ep(j,2))**2)
         thet = atan (ep(j,2) * betasq / epbe)
         if (thet .lt. 0) thet = thet + pi         
C     plausible-otherwise I'd have a jump
C     Fano says [p 21]: 'arctan approaches pi for betasq*eps1 > 1'

         dgh = ep(j,1)**2 + ep(j,2)**2
         if (abs(dgh) .le. 0) print*, ' dgh',j,dgh, ep(j,1), ep(j,2)
         if (abs(dgh) .le. 0) dgh = 1.e-15
         sgh = E(j)**2 *(betasq - ep(j,1) / dgh) * thet
         sgh = racc * sgh
         sig(j,2) = sgg + sgh
         if (abs(sso) .le. 0) print*, ' sso=',sso
         sig(j,3) = S0 * uef * ZA / sso
C     the integral was over  d lnK rather than  d lnQ
C     if ((j/50)*50 .eq. j) print 327, E(j),sgg,sgh,(sig(j,ii),ii=1,3)
D327            format (1x,f10.1, 1p6e11.3)

      sig(j,4) = 0

      do  L=1,3
         Tsig(L)  = Tsig(L)  + sig(j,L) * dE(j) / E(j)**2
         STP(L)   = STP(L)   + sig(j,L) * dE(j) / E(j)
         rM2(L)   = rM2(L)   + sig(j,L) *  dE(j)
 27      sig(j,4) = sig(j,4) + sig(j,L)
      end do
      Tsig(4) = Tsig(4) + sig(j,4) * dE(j) / E(j)**2
      STP(4)  = STP(4)  + sig(j,4) * dE(j) / E(j)  
      rM2(4)  = rM2(4)  + sig(j,4) * dE(j)
D               if (j .eq. 1) go to 28
C               if (j .ge. 320 .and. j .le. 326) go to 28
D               if ((j/10)*10 .ne. j) go to 5
D28     write (3,608) j,E(j),dfdE(j),sgg,sgh,(sig(j,L),L=1,4),
D    1          S0,STP(4)
D608            format (1x,i4, f10.1,1pe11.3,0p9f9.4)
 5    continue

 11   continue
C     write (3,*) '  uef=',uef
C     write (3,374) Tsig,STP,rm2
 374  format (/9x,'Integ. over sig =',4F13.4/2(26x,4f13.2/))
      FSG  = Tsig(4) * DEC
      dEdx = STP(4) * (dec/1E3)
      rmf  = rM2(4) * (dec/1e6)

      print 388,    S0,FSG,dEdx,rmf
C     write (3,388) S0,FSG,dEdx,rmf
 388            format (/,10X,'Zeff=',F7.3,4X,'# coll/cm=',f11.3,4x,
     1          'dE/dx=',F9.4,' keV/cm',3x,'M2=',f12.4,' keV**2/cm')
 
C     write (3,*) ' DEC=',dec,'  # atoms/cm**3=',atnu,'  fac=',fac

                call SPTS

        END
 
      subroutine SPTS
#include "simsin.inc"
 
D               write (3,333)
D333    format (/4X,'SPTS,F.333:',/15X,'E',7x,'sig*E**2',6x,'sig',
D    1       10X,'M 0',11X,'M 1',11X,'M 2',10X,'<E>',/)
      SGM  = 0
      Stpw = 0
      SECM = 0
      jpr  = 10
      ja   = 10
      
      do 75 j=1,nume
         if (E(j) .gt. Emax) go to 76
C     he2  = sig(j,4) / ZA
         he2  = sig(j,4) * dec
         H(j) = he2 / E(j)**2
         SGM  = SGM + H(j)*dE(j)
         STPW = STPW + H(j) * E(j) * dE(j)
         SECM = SECM + he2 * dE(j)
         eps  = STPW / SGM
 653     format (1x,i4,f12.2,1pe11.4, e16.8, 2e11.4, 0pf7.2)
D               if (j .lt. 5) go to 11
D               if (j .eq. nume) go to 11
D               if (jpr .ne. j) go to 75
D               jpr = jpr + ja
D11   write (3,654) j,E(j), he2,H(j), SGM,STPW, SECM,eps
 654  format (1x,i5,f12.2, 1p5e13.5, 0pf7.2)
 75   continue
        
 76   continue 
C     write (3,610)  nume,SGM,STPW,SECM
 610  format (/1X,'F.610: nume  ',i5,' total cross section=',1PE15.5,
     1     3X,'dE/dx=',E15.5,', M2=',E15.5/2x,'see FSR-99 and CCS-9'/)
C     write (3,*) ' final E=', Efin,' Emax=',Emax,' he2=',he2

Cee FSR-99
      bbb = 1. - sbb*betasq / Emax
      fft = ZA * dec
C     write (3,*) ' sbb=',sbb,' eV,   bbb=',bbb,'  fft=',fft
      rm0 = bbb * ((1/Efin - 1/Emax) + 2 * (1/Efin**2 - 1/Emax**2))
     1     - betasq * log(Emax/Efin) / Emax
C write (3,*) ' residual M0=',rm0,rm0*fft,'/cm'
C write (3,*) '  If residual M0 is large, look for error'
C write (3,*)

      call HPART(fft,stpw,secm)

      END
 
      subroutine HPART(fft,stpw,secm)
#include "simsin.inc"
Calculate total dE/dx, including contributions from E>Emax
Cee FSR-101

      if (npm .ne. 4) rst = bbb * log (Emax/Efin) + 
     1     sbb * (1./Efin - 1./Emax) - betasq*(1. - Efin/Emax)
      
      ffu = fft / 1.e6
      if (npm .ne. 4) go to 6 
      
C for electrons, see FSR-143 and Uehling Eq 9:
      TE = pkE * 1.e6
      print*,' ele',efin,emax,TE,gam
      rst=log(Emax/Efin)+log(Emax)-log(TE-Efin)  - 1/(1.-Efin/TE)
     1     + 2 + ((gam-1)/gam)**2 * (1./8. - .5 * (Efin/TE)**2)
     2     + ((2*gam - 1) / gam**2) * (log(Emax) - log(TE-Efin))
      
 6    continue
C     write (3,*) ' residual dE/dx=',rst,rst*ffu,' MeV/cm'
 
      tdedx = stpw/1.e6 + rst*ffu
C     write(3,*) ' dE/dx=',tdedx,' MeV/cm ',tdedx/rho,' MeV cm**2/g'
      print*, ' dE/dx=',tdedx,tdedx/rho
      
      secmv = secm / 1.e6
      rM2p  = Efin * (1. - 0.5 * betasq * Efin / Emax)
      del2  = secmv - ffu * rM2p
      print *,    ' M2=',secmv,'  M"=',ffu*rM2p,' M2-M2"=',del2
C     write (3,*) ' M2=',secmv,'  M"=',ffu*rM2p,' M2-M2"=',del2

      END

      subroutine CONV
#include "simsin.inc"
 
C     write (3,303)
 303    format (/1x, 50(' *'))
C     write (3,*) '  CONV','  t=',exth,' coll/cm=fsg=',fsg
D     print*, ' coll/cm=', fsg
      XMC = exth * FSG
C  number of collisions in thickness exth
      jxt = log(xmc) / log(2.0) + 1
      print*, ' CONV: jxt=',jxt
      xx = xmc / 2.0**jxt
 86   CZ0 = xx / 2**10
      NU  = jxt + 10
C     write (3,386) jxt,NU,exth,XMC,XX,CZ0
 386  format (3x,'F.386: NU=',2i3,4x,'t=',f10.7,' cm',
     1     4X,'# coll=',F11.3,4X,'xx=',F9.5,4X,'CZ0=',1pe12.5)

      write (9,*) CZ0,Emax,betasq
      write (9,909) pkE,bg,PTM,pmom
 909  format (4f16.6)
      write (9,911) lemx,NU,U,um,Emin
 911  format (2i9, 3f16.8)
C     write (19,*) CZ0,Emax,betasq,' CZ0, Emax, beta**2'
C     write (19,*) pkE,bg,PTM,pmom,'  KE, bg, M, p'
C     write (19,*) lemx,NU,U,um,Emin,' lemx,nu'

      do 83 k=1,lemx,6
 83      write (9,384) (E(kl),kl=k,k+5)
 384     format (1p6e13.6)

        H0  = 0.
        CM1 = 1.0
        CM2 = 1.0
        XN  = 1
        EX  = 1e-15              
        MIE = 0                 
        MIF = 0
        MIH = 0

        call NORMAL

        xn = 1
        D1 = CM1
        D2 = CM2 + CM1**2
        D3 = CM3 + 3.0*CM2*CM1 + CM1**3
        D4 = CM4 + 4.0*CM3*CM1 + 6.0*CM1**2*CM2 + CM1**4
        S  = D2 / D1
C     write (3,612) D1,S,um
 612    format (2x,'conv  F.612:',3X,'Initial distribution',/,
     1  '  delta 1=',1PE12.4,'  delta 2 =',E12.4,'  exp(u)=',E12.5)
C     write (3,635) (H(J),J=1,nume,5)
C    set parameters from single collision spectrum

C     call SHRINK
 
        H0 = 1. - CZ0
        do 15 L=1,leh
 15     H(L) = H(L) * CZ0         
C the first convolution is: H0 + H(E)

        CN  = CZ0
        CM1 = CZ0 * D1
        CM2 = CZ0 * D2
        thi = exth / 2**(jxt+10)
        xi  = saxk * thi * Za
        rkap = xi / Emax
C     write (3,401)  NU,leh,Emin,Emax, CM1
 401    format(/2x,'F 401: ',2i5,1p2e14.7,'  CM1=',e12.5)
C     write (21,2121)
 2121   format ('  x/cm  xi/eV',6x,'kap',6x,'<del>',5x,'dmp(eV)',5x,'w')

        do 16 N1 = 1,NU
        thi  = 2. * thi
        xi   = 2. * xi
        rkap = 2. * rkap
        CN   = 2. * CN
D     print*, 'convol: N1',N1,NU,'  t=',thi
C     if (N1 .le. NU-ners) go to 696
      if (N1 .le. 13) go to 696
C     write (3,303)
C     write (3,317) N1,leh,MIE,MIH,CN,CZ0
 317  format (/' CONVOL # =',i3,' leh=',i4,2x,'MIE,MIH=',
     1     2i5, '  <coll #>=',1PE12.4,'  CZ0=',0pf9.6/)
 696  N2P = N2

      call FOLD

C  Landau-Vavilov parameters, PB = lambda-bar, eq 11 of Saxon
      PB = -.4227843351 - log(rkap) - betasq
      if (n1 .gt. nu-ners) write (9,910) thi,xi,rkap,pb, CMQ
 910  format (1x,1p3e12.5, 0pf12.6,f12.3,' t,xi,kap,lam,<del>')
C     write (3,310) thi,xi,rkap,pb, CMQ
 310  format (/'  t =',1pe12.5,'  xi=',e12.5,'  kappa=',e12.5,
     1     '  <lam>=',0pf12.6, ' <del>=',f12.3)
      dmmpl = xi * (pb + .22278)
      codp  = CMQ - dmmpl
C     write (3,313) Codp, dmmpl
 313  format ('  Landau theory del-p=',f12.3,' <del>-dmp=',f12.3)
 631  N = MIH - MIE
C     write (3,408) N1,leh,CN,xi,rkap,PB
 408  format (' CONV, F.408',2i5,f9.3,1p4e11.4/)

      call OUTPUT

 16   CONTINUE

      END
 
      subroutine OUTPUT               
Calculation of moments of straggling functions and comparison with cumulants 
#include "simsin.inc"
 
        B = CM2 / (CM1**2)
        C = CM3 / sqrt(CM2**3)
        D = CM4 / (CM2**2)
        S1 = CN * D1
C     write (3,*) ' OUT',b,c,d,s1,cn
        S2 = CN * D2 / S1**2
        S3 = D3 / sqrt(D2**3 * CN)
        S4 = D4 / (D2**2 * CN) + 3.
C     write (3,350) H0,cm1,B,C,D,S1,S2,S3,S4
 350            format (2x,'OUTP: zero component =',1pe16.4/30x,'mean',
     1          9x,'variance/mean**2',4x,'central3/var**1.5',
     2          20H     central4/var**2/3x,'actual values=',4e20.4,/
     3          3x,'theoret values',4e20.4,/)

C     write (3,340) cm1,s1,cm1/s1
C     if (N1 .ge. nu-ners) print 340, cm1,s1,cm1/s1
 340            format (1x,'OUTPUT:',1p2e13.6,'  ratio=',0pf10.6)

        X = 1.
        N = MIH - MIE
C     write (3,*) '  N=',N,'  MIH, MIE=', MIH, MIE
        if (N1 .le. nu-ners) RETURN
        write (9,*) leh,N1,N2,N2P,' leh, N1, N2, N2p' 
        write (9,*) N,H0, thi,xi,' N, H0, t, xi'
        if (N2 .eq. N2P) go to 42
        write (9,*) lemx,'   lemx'

        do 43 k=1,lemx,8
 43        write (9,950) (E(kl),kl=k,k+7)
 950       format (8f10.3)
           
 42        if (N2 .ne. N2P) N2P = N2


           call FWHM(lup,llow,N)

           NPP   = (lup + llow) / 2
           nskip = 1
           nrp = (lup - llow) / 2
           if (nrp .gt. 70) nskip = 2
           nrp = nrp + nskip
D     print*, ' N,npp,nrp,nskip=',N,npp,nrp,nskip
C     write (3,344)
 344  format ( 2(7x,'j',6x,'E/eV',6x,'phi(E)',10x,'M0',6x,'dE/dx',3x))

        do 56 L=llow,NPP,nskip
        J  = L + N
        J2 = J + nrp
        L2 = L + nrp
 56   continue
C write (3,356)   J ,E(J), H(L),  ASP(L), ASS(L),
C     1 J2,E(J2),H(L2), ASP(L2),ASS(L2)
 356    format (3x,i5,0pF10.1,1p3E13.4,3x,i5,0pF10.1,1p3E13.4)

        END
 
      subroutine FWHM(lup,llow,N)
#include "simsin.inc"

C     write (3,*) ' fwhm, leh=',leh,CZ0,CN
        ASP(1) = 0
        ASS(1) = 0
        hmax = 0
        
        ndx = N1 + ners - nu
        do 44 L=1,leh
           if (h(L) .lt. hmax) go to 53
           hmax = h(L)
           lmax = L
           if (lmax .lt. 3) lmax = 3
 53        J = L + N

           ASP(L+1) = ASP(L) + H(L) * DE(J)
Calculate zeroth, ASP, and first moment, ASS, of straggling function H(L)
           ASS(L+1) = ASS(L) + H(L) * E(J) * DE(J)
C     if (N1 .eq. NU-1) write (19,*) L, E(J), asp(L), ass(L)
C     if (N1 .eq. NU) write (19,*) L, E(J), H(L), asp(L)
 44     continue

D     print 346,  lmax+N,E(lmax+N),h(lmax)
 346  format (1x,'lmax+N=',i4,'  E=',f11.2,'  h=',1pe12.5)
C     write (3,346) lmax+N,E(lmax+N),h(lmax)
      bax = (h(lmax) - h(lmax-1)) / (h(lmax) - h(lmax+1) )
Calculate the most probable energy loss, dmp, see FSR-75
      dmp = E(lmax+N) + 0.5 * dE(lmax+N) * (bax - 1) / (bax + 1)
      bax1 = (h(lmax-1) - h(lmax-2)) / (h(lmax-1) - h(lmax) )
      dmp1 = E(lmax+N-1) + 0.5 * dE(lmax+N-1)*(bax1 - 1)/(bax1 + 1)

      do 50 L=1,lemx
         L1 = L - N + (ndx-2) * N2 ! shift by factor 2
         if (L1 .lt. 1) L1 = 1
C     if (N1 .ge. ndx) BEdx(L,ndx) = H(L1)
         if (N1 .ge. ndx) BEdx(L,ndx) = H(L1) / H(lmax)
 50   continue

      hhun = 0.01 * h(lmax)
C     write ( 3,*) ' ndx=', ndx, h(lmax)
C     write (21,*) ' ndx=', ndx, h(lmax)
      do 55 L=1,leh
         if (L .lt. lmax .and. h(L) .lt. hhun) llow = L
         if (L .gt. lmax .and. h(L) .gt. hhun) lup  = L
 55   continue

      if (llow .lt. 1) llow = 1
      if (llow .lt. 2) lup = 200
C     if (llow .lt. 1) go to 71 ! added on 26.7.1999 to avoid 0

C     write (3,355) llow,E(llow+N),h(llow),lup,E(lup+N),h(lup)
D     print 355, llow,E(llow+N),h(llow),lup,E(lup+N),h(lup)
 355  format (' lower & upper cutoff:',2(i6,0pf9.1,1pe12.4))

      phih = hmax / 2.0

      kph  = 0
      kpi  = 0
C     print*, 'llow, lup=', llow,lup,' phih=',phih
      do 69 K=llow, lup
         vft  = H(K)
         if (vft .lt. phih .and. kph .eq. 0) go to 30
         kph = kph + 1
         if (kph .gt. 1) go to 40
         dhd = dE(K) * (phih - H(K-1)) / (H(K) - H(K-1))
         dlhl= E(K-1+N) + dhd
C     write (3,*) '  K',K,'  E=',E(K-1+N),' dlhl=      ',dlhl
C     print*, ' a',k,dlhl,' h=',H(K)
         go to 30
 40      if (vft .gt. phih .and. kpi .eq. 0) go to 30
         kpi = kpi + 1
         if (kpi .gt. 1) go to 30
         dhd = dE(K) * (phih - H(K-1)) / (H(K) - H(K-1))
         dlhu= E(K-1+N) + dhd
         ek1 = E(K-1+N)
C     write (3,*) '  K',K,'  E=',EK1,' dlhu=      ',dlhu
C     print*, ' b',k,dlhu,'h=', H(K)
 30      continue
 69   continue
      wha = dlhu - dlhl

C     write (3,*) ' bax,dmp=',bax,dmp,'  lower:',bax1,dmp1,' w=',wha
      print 369, thi,0.001*dmp,0.001*wha, 0.001*dmp/thi
 369  format (' t=',f9.3,' dmp=',f8.3,' w=', f8.3,' dE/dx=',f9.3)
      write (9,*) lmax+N,E(lmax+N),h(lmax),' lmax+N, E, h'
      write (9,*) leh,dmp, CMQ,'  leh, dmp, <del>'
      write (9, *) dlhl,wha,' Dl, fwhm'
C     write (21,2113) thi, xi, rkap, cmq, dmp, wha, dmp/thi
 2113 format (1x,f6.2, f8.1, 1pe11.4, 0p2f10.2 ,f9.2,f9.3 )

 71   do 47 kl=1,leh,6
 47      write (9,944) (H(kk),kk=kl,kl+5)
 944     format (1p6e12.5)

C     if (N1 .ne. NU-1) RETURN
         if (N1 .ne. NU) RETURN
C     write (19,*) dmp, wha,' dmp, w; spectrum for t=', thi
         do 49 L=1,leh                  
            J = L + N
            if (ASP(L) .lt. 0.00005) go to 49
            if (ASP(L) .gt. 0.9999)  go to 49
C     if (N1 .eq. NU-1) write (19,*) L,E(J), H(L),ASP(L),ASS(L)
C     if (N1 .eq. NU) write (19,*) L,E(J), H(L),ASP(L),ASS(L)
 49      continue
         
      END
 
      subroutine FOLD
C On 18 July 1984, I have some questions whether ST 62 is correct
#include "simsin.inc"
 
      do 60 L=1,1250
         F(L) = H(L)
 60      H(L) = 0.

        F0  = H0
        LEF = leh
        MIF = MIH
        if (LEF .lt. 80)       call RESET
        H0  = F0**2
        MIH = MIF + N2
        LEH = LEF
        if (leh .gt. 1250) leh = 1250

D        print*,     '  FOLD: MIH,lef,leh,nume=',MIH,lef,leh,nume
C     write (3,*) '  FOLD: MIH,lef,leh,nume=',MIH,lef,leh,nume

        do 61 LH=1,leh
        JH = LH + MIH
        do 62 LF=1,LH
        JF = LF + MIF
        K  = JH - JF
        FLF = JH - MIF - DI(K) + 1.E-20
        LFF = FLF
        LE = JF - MIE
        S = FLF - LFF
        if (LFF .eq. 0) LFF = 1
 62     H(LH) = H(LH) + F(LF) * ((1.0-S)*F(LFF)+S*F(LFF+1)) * DE(LE)
 61     H(LH) = H(LH) - F(LH)**2 * 0.5*DE(LE)

        do 63 L=1,leh
 63     H(L) = H(L) * 2.0
C     print*, ' t=', thi

        if (F0 .gt. EX)       call ZERO
 65   continue
C     call SHRINK
      call NORMAL

      END
 
      subroutine RESET                
C     called from FOLD
#include "simsin.inc"             
C     LEF = LEH initially
 
      if (N2 .ge. 128) go to 702
 701  N2 = N2 * 2
      U = log(2.) / float(N2)
C     write (3,375) MIE,MIH,N2
 375  format  (1x,'RESET: ',2i4,'  Coordinate change: doubling of ',
     1     'point grid',i4,' points on a factor of 2',/)
C     write (3,*) ' LEH,MIH,MIE=',LEH,MIH,MIE

      do 72 LL=1,LEF                  
C     this is from top down
         L = LEF + 1 - LL
 72      F(2*L) = F(L)

         N = 2*LEF
         do 73 L=4,N,2                   
C     N is just some number
 73         F(L-1) = (F(L) + F(L-2)) / 2.

        LEF = 2*LEF + 1
        MIF = 2*MIF
        MIE = MIF

        do 74 J=1,leh
        S = float(J+MIE)
        E(J) = exp(S*U) * Emin
        DE(J) = E(J)*U
        S = J
 74     DI(J) = - log(1. - exp(-S*U)) / U

 702    MIE = MIF                       
C   from 4th Line above
        do 76  J=1,leh
        S = float(J+MIE)
        E(J) = exp(S*U) * Emin
 76     DE(J) = E(J)*U

C     write (3,*) ' LEH,MIH,MIE=',LEH,MIH,MIE

        END
 
        subroutine ZERO
Calculate the fraction of particles not making any collision
#include "simsin.inc"
 
 81     xs = 0.
        N = MIH - MIE
C     write (3,*) ' zero ',mih,mie,n,leh,F0

        do 82 L=1,leh
        K = L+N
 82     xs = xs + H(L)*DE(K)

        xs = (1. - F0)**2 / xs 
C     write (3,*) k,xs,H(l),dE(K)

        do 83 L=1,leh
 83     H(L) = H(L) * xs

        N = MIH - MIF
        MIH = MIF
        LEH = LEH+N
        if (leh .gt. nume) leh = nume
        LEX = LEH + 1

        do 84 LL=1,leh
        LA = LEX - LL
        K  = LA + N
 84     H(K) = H(LA)

        do 85 LB=1,N
 85     H(LB) = 0.

D       print 684, N,LEF,leh,LEX,MIE,MIF,MIH
 684  format  (' zero:',i3,2(3x,3i5))
      do 86 M=1,LEF
 86      H(M) = H(M) + 2*F0*F(M)         
C     F(M) is the H of the previous convolution
C     This seems to be correct (18 July 1984)

         END
 
      subroutine NORMAL
#include "simsin.inc"
 
      Y = CM1*2.0
      Z = CM2*2.
C     write (3,714) MIE,MIH,H0,xn,y,z
 714  format(' NORM',2i5,'  H0=',1pe12.5,'  xn,y,z=',0p2f12.3,1pe12.5)
      CM0 = H0
      CM1 = 0.
      CMA = 0.
      N = MIH - MIE

C     write (3,*) ' N,leh=',N,leh
       do 35 L = 1,leh
        LE = L + N
        S = H(L) * dE(LE)
        CM0 = CM0 + S
        CM1 = CM1 + S*E(LE)
 35     cma = cma + s*E(LE)**2

        if (cm0 .ne. 0) cmq = cm1 / cm0
C write (3,635) CM0,CM1,CMQ
 635    format  (7x,'M0=',1pe12.5,3x,'straight mean=M1=',
     1          1pe12.5,3X,'<E>=',0pf14.4)
        if (cm0 - H0 .ne. 0) T = (1. - H0) / (cm0 - H0)
        CM1 = CM1*T
        CM2 = 0.
        CM3 = 0.
        CM4 = 0.

        do 31  L=1,leh
 31     H(L) = H(L) * T

D     print*,     ' N,leh=',N,leh
C     write (3,*) ' N,leh=',N,leh

        do 30  L=1,leh
        LE = L + N
        EC = E(LE) - CM1
        S  = H(L) * DE(LE)
        CM2 = CM2 + S*EC**2
        CM3 = CM3 + S*EC**3
 30     CM4 = CM4 + S*EC**4

        XN  = XN * CM0
        if (Y .ne. 0) Y = CM1 / Y
        if (z .ne. 0) Z = CM2 / Z
C     34             write (3,332) XN,CM0,CM1,CM2,CM3,CM4,Y,Z
 332    format  (7X,'Precision control, ',1P6E13.5,
     1       /24x,'mean=',e12.5,'   variance=',e12.5)

        END
 
        subroutine SHRINK
#include "simsin.inc"
 
        S = 0.
        N = MIH - MIE

C    write (3,*) ' SHRINK-a: N,MIH,MIE,leh',N,MIH,MIE,leh
        do 40 L=1,leh
        lla = L
        K = L + N
        S = S + H(L)*DE(K)
        if (S .gt. EX) GO TO 42
 40     CONTINUE

 42     M = lla - 1
        MIH = MIH + M
        S = 0.
        LA = LEH + 1

        do 43 K=1,leh
        L = LA - K
        KK = L + N
        S = S + H(L) * DE(KK)
        if (S - EX)  43,43,44
 43     CONTINUE

 44     LEH = L - M
        do 45  L=1,leh
        K = L + M
 45     H(L) = H(K)
C     write (3,*) ' SHRINK-b: L,M,leh',L,M,leh

        K = LEH + 1
        do 46  L=K,nume
 46     H(L) = 0.

        END

      subroutine LCON           ! copied from BNL CD on 26.11.2000
C     checked on 28 Aug 1999 by h. bichsel on PC
C     Converting the straggling functions with log abscissa from COV to linear
C     abscissa, including detector noise and calculating most probable energyloss
C     and full width at half maximum, for comparison with experimental data.
C     The file "LCON.PLT" can be used for plotting. 

#include "simsin.inc"
                   
C     open ( 3,file='LCOS.OPA',status='new')    ! the output file
C     open ( 5,file='LCOS.SPR',status='new')
C     open (15,file='LCOS.PLT',status='new')
C     open (16,file='LCOS.DAT',status='new')
C     this file can be used for plotting the straggling function
      open ( 3,file='LCOS.OPA')
      open ( 5,file='LCOS.SPR') ! 3000 values
C     open (15,file='LCOS.PLT')   ! 600  values only
C     open (16,file='LCOS.daT')   ! for Adelberger's plot
C     e difference between .spr & .plt is just in the length of the file

      jsp  = 1
      scal = 1.0

      call TIMDAT(TEE)
C     write (16,604) TEE, jsp,reso
 604  format (' LCOS.DAT ',a22,5x,'jsp=',i4,' resol=',f9.4)
C     write (15,603) TEE, jsp,reso
 603  format (' (LCOS.PLT ',a22,5x,'jsp=',i4,' resol=',f9.4,')')
      write (5,601) TEE, jsp,reso
 601  format (' (LCOS.SPR ',a22,5x,'jsp=',i4,' resol=',f9.4,')')
      write (3,602) TEE,jsp,reso
 602  format (' (LCOS.OPA ',a22,5x,'jsp=',i4,' resol=',f9.4,')')
      
      reso = reso / 1000.0      ! conversion to keV

      call INPU                 !       calls ESCAL

      do m=1,ksp
         sp(m,1) = FS(m)
         h(m)    = FS(m)
      end do
      nst = 1
         
      call SPEC                 ! finds dmp & fwhm

      do  Nst=2,6
         reso = 0.02 * (nst-1)
         
         do J=1,ksp
            GS(J) = 0
            H(J)  = 0
         end do
         call NOIS              ! calculates spectrum with noise reso
         call SPEC
      end do      
      call OUTPU
               
*     close (5)

      END

      subroutine NOIS
#include "simsin.inc"
Calculate resolution function, stand. dev = reso keV

      snoi = sqrt(emmi * reso)
      mgau = 3.5 * snoi / det
      ner  = int(emmi) + 1
      if (mgau .ge. ksn) mgau = ksn - 1
      if (mgau .lt. 1)   mgau = 1

      call GAUS(mgau,ksn)

      print 313, snoi, mgau,Tsp
 313  format (' first snoi=',f8.5,' keV, m=',i3,' Tsp',f7.3)
      print 314, emmi,ner, ksn,ksp
 314  format (' emmi,ner=',f8.5,i3,'  ksn,ksp=',2i6)
      write (3,313) snoi, mgau,Tsp
      write (3,314) emmi,ner, ksn,ksp

      do 15 k=1,ksp-1           ! ksp = 6000
C     if (int(ES(K)/ner) .ne. 1) go to 5
         snoi = sqrt(ES(K) * reso)
         mgau = 3.5 * snoi / det
C     print*, '          K, ES(K)=', K, ES(K)
C     print*, ' snoi=', snoi,' keV, ner, m=', ner,mgau

      call GAUS(mgau,K)   ! the resolution changes with E-loss

      ner = ner + 1
 5    continue
D     if ((K/100)*100 .eq. K) print*, ' K=',k,ES(K),FS(K)
      h1   = h(K)
      H(K) = H(K) + FS(K) * Tsp * det ! central value
      h2   = h(K)
      do 9 M=1,mgau
         ddH = FS(K) * GS(M) * det
         if (K+M .ge. ksp) go to 9
         if (K-M .le. 0)   go to 9
         H(K+M) = H(K+M) + ddH
         H(K-M) = H(K-M) + ddH
D        if ((K/100)*100 .ne. K) go to 6
D        if (M .eq. 1)           go to 6
D        if (M .eq. 2*mgau+1)    go to 6
D        if ((M/5)*5   .ne. M)   go to 6
D 6      print 307, M,M, FS(K), GS(M), H(K+M)
 307     format (' fold:',2i5, f9.6,'  g,h', 2f9.6)
C        if (K .eq. ksn .and. M .eq. 1) print*, 'mg',K,M,H(K+M),FS(K),GS(M)
 9    continue
C     if (K .le. Ksn+9) print*, ' end:', K,ES(K), FS(K), H(K)
C     if ((K/100)*100 .eq. K) print*, ' end:', K,ES(K), FS(K), H(K)
C     if (K .ge. 90 .and.K.lt.125) print 317, K,mgau,ES(K),h1,fs(K),H(K)
 15   continue
 317  format (' H=',2i5,f7.3,1p3e12.5)

      do  k=jp1,ksp-1
         sp(K,nst) = H(K)
      end do
      END

      subroutine GAUS(mgau,K)
#include "simsin.inc"
Calculate resolution function, stand. dev = reso keV

      WD    = 2. * SNOI**2
      TSP   = 1. / (SNOI * SQRT(2*3.14159))
      GS(mgau+1) = TSP
      sam   = TSP * det / 2
      if (ner .eq. 10) print 363, ner,k, ES(K),tsp, sam
 363  format (' snoi',2i6,' ES(K)=',f9.5,' tsp, sam=',2f10.5)
      do  J=1,mgau+1
         agau  = J * det
         GS(J) = TSP * exp(-(agau**2) / WD)
         sam = sam + GS(J) * det
D        if (ner .eq. 10) print*, j,agau/snoi, gs(j), sam, som
      end do
      if (K .eq. ksn) print*, ' gaus, sam =',sam

      cof = 0.5 / sam
      sem = cof * TSP * det / 2
      do J=1,mgau+1
         GS(J) = GS(J) * cof
         sem = sem + GS(J) * det
      end do
      if (K .eq. ksn) print*, ' sem=', sem
      
      END

      subroutine SPEC
#include "simsin.inc"

      bmax = 0
      do 11 j=1,ksp
         fgx(j) = h(j)
         if (fgx(j) .lt. bmax) go to 11
         bmax = fgx(j)
         jsi  = j 
C     jsi is the index for the approximate location of maximum
 11   continue
C     parabolic fit to peak, see FSR-75
      print*, ' jsi', jsi, H(jsi), bmax
      bxx = (fgx(jsi) - fgx(jsi-1)) / (fgx(jsi) - fgx(jsi+1))
      dmp = ES(jsi) + 0.5 * det * (bxx-1) / (bxx+1)
      write (3,311) nst,dmp,jsi,bmax
 311  format (/' L=',i2,'  dmp=',f9.3,5x,i4,1pe11.4)

      phih = bmax / 2
      kph  = 0
      kpi  = 0
      do K=1,ksp
         vft  = fgx(K)
         if ((K/10)*10 + 1 .eq. K) write (3,*) ES(K), vft
         if (vft .lt. phih .and. kph .eq. 0) go to 30
         kph = kph + 1
         if (kph .gt. 1) go to 40
         dhd = det * (phih - fgx(K-1)) / (fgx(K)-fgx(K-1))
         dlhl= ES(K-1) + dhd
         write (3,*) '  K',K,'  E=',ES(K-1),' dlhl=      ',dlhl
         print*, ' a',k,dlhl
         go to 30
 40      if (vft .gt. phih .and. kpi .eq. 0) go to 30
         kpi = kpi + 1
         if (kpi .gt. 1) go to 30
         dhd = det * (phih - fgx(K-1)) / (fgx(K)-fgx(K-1))
         dlhu= ES(K-1) + dhd
         ek1 = ES(K-1)
         write (3,*) '  K',K,'  E=',EK1,' dlhu=      ',dlhu
 30      continue
      end do
      wha = dlhu - dlhl
 15   write (3,*) ' jsi=',jsi,'   w=',wha
      print 315, dmp,wha,wha/dmp
 315  format (' dmp, w=', 2f9.3,' r=', f9.3)
      write (5,569)  nst,dmp,wha,wha/dmp,reso
C     write (15,569) nst,dmp,wha,wha/dmp,reso
 569  format (i3,2f9.3,f9.4,f9.3,' L, D_p, w, w/D_p, reso')
      
      END

      subroutine INPU
#include "simsin.inc"
      character*8 tit(9)

      open (11,file='ALCOV1.SPE',status='old')
C     this is the file produced in ALCNE.for, SPE stands for "SPEctrum".

 301  format (9a8)
 300  format (5x,9a8/)
      
      read (11,301) tit
      write (3,300) tit
      print 300, tit
      read (11,*) ZA, AW, rho
C     print 333,     ZA, AW, rho
      write (3,333)  ZA, AW, rho
      write (5,333)  ZA, AW, rho
C     write (15,333) ZA, AW, rho 
 333  format (' ( ',f7.2,f9.4,f12.6,' Z, A, rho )')

      read (11,*) ners,xy1, betsq,  dedx
      write (3,*) ners, betsq,  dedx,' betsq, dE/dx'

      dEdx = 1000 * dEdx
      sxk  = 153.536 * rho * ZA / (AW * betsq)
      stn  = dEdx / sxk 
C     stn is twice Bethe stopping number B

D     write (3,*)
 600  format (8a10)
      read (11,*) cz0,Emax, betsq
      read (11,*) PE,bg, ptM,pmom
C     print 303,     pmom, dedx,stn, sxk
      write (5,303)  pmom, dedx,stn, sxk
C     write (15,303) pmom, dedx,stn, sxk
 303  format (' (pc=',f10.2,' dE/dx=',f8.5,' keV/cm, 2*B=',f7.4,
     1     ' k=',f8.6,' ) ')
      write (3,303)  PE, dedx,stn, sxk
C     print 504,     betsq,PE,bg,ptM
      write (5,504)  betsq,PE,bg,ptM
C     write (15,504) betsq,PE,bg,ptM
 504  format ('  ( bsq=',f9.7,' T=',f10.2,
     1     ' bg=',f10.4,' Mass=',f8.3,' )')
      write (3,604) cz0,Emax/1e6,betsq,PE,bg,ptM,pmom
 604  format (3X,'cz0=',f9.6,'  Emax=',f11.3,
     1     '  beta**2=',f10.8,'  energy=',f10.2,
     2     ' MeV'/,3x,'beta*gamma=',0pf10.4,'  mass=',
     3     f8.3,' MeV    p=',f9.2,/)
      do je=1,ket
 2       et(je) = 0
      end do
      read (11,*) lemx,NU,U,EXT,E1
      read (11,*) (ET(kl),kl=1,lemx)
      write (3,606) lemx,NU,U,EXT,E1,ET(1)
 606  format (3X,'lemx=',I5,'  NU=',I4,'  U=',F8.6,
     1     '  EXT=',F8.6,'  E1=',F9.4,'  ET(1)=',F8.3)

      HL0 = 0
      NSP = NU-JSP
      do 9 J=1,ners
         do je=1,ket          ! ket=1705
 7          ft(je) = 0                 
         end do
         read (11,*) thi,xi, rkap, PB, CMQ
         write (3,307)   thi,xi, rkap, PB, CMQ
 307     format (/'  t=',f9.3,' xi=',f9.3,' kap=',1pe11.4, 
     1        ' PB=',0pf10.5,' <del>=',f12.3,' eV')
         read (11,*) leh,N1,N2,N2P
         read (11,*) NS,H0, thi,xi
         write (3,612)  leh,NSP,N1, N2,N2P,NS,H0
 612     format (1x,'leh=',i4,'  NSP=',I3,'  N1=',I3,'  N2=',I3,
     1        '   N2P=',I3,'   NS=',I4,'   H0=',F10.6)
         if (ns .eq. 0) etns = ET(1)
         if (ns .gt. 0) etns = ET(ns)
         
         write (3,*) ' ET(ns)=',etns
         
         if (N2P .ne. N2) print *, ' something is screwed up'
         if (N2P .ne. N2) pause
         if (N2P .eq. N2) go to 37
         
         read (11,*) lemx
         read (11,*) (ET(kl),kl=1,lemx)
D          print*, ' n2p',lemx,et(lemx)
      write (3,*) ' ***************************'
      write (3,*) ' N2, N2p=', N2,N2p,' lemx=',lemx,ET(lemx)

 37   read (11,*) lmax,Elx,hlx
      write (3,*) '  lmax=',lmax,'  E=',Elx,'  h=',hlx
      read (11,*) leh,dmpb, delm
      read (11,*) Dlhc, whac
C     print 337,    leh,dmpb, Dlhc,whac, delm
      write (3,337) leh,dmpb, Dlhc, whac, delm
 337  format (1x,'COV,leh=',i5,' dmp=',2f9.3,' w=',f9.3,' delm=',f9.3)

      read (11,*) (FT(kl),kl=1,Leh)
D     print*, ' leh',leh,ET(leh),ft(leh),' t=', thi
D     write (3,*) ET(1),FT(1),ET(10),FT(10)
      if (N1 .eq. NSP) go to 15
 9    CONTINUE
 15   lee = leh + NS + 1

      close (11)
      delmp = xi * (stn + log(rkap) + 0.200 + betsq) 
C     delmp is the Landau most probable energy loss, see CCS-5
      write (3,321) thi, stn,delmp/1000,dmpb/1000
 321  format (/' t=',f9.2,' cm, 2*B=',f8.4,' Landau dmp=',f9.3,'keV,',
     1     '  dmp(COV)=',f10.3,' keV'/)
      dmmpl = xi * (PB + 0.22278)
      dllp  = CMQ - dmmpl
      write (3,*) ' from COV: dmmpl, dllp=', dmmpl,dllp

      write (5,1521)  thi, 0.001*dmpb,0.001*whac
 1521 format (' x=',f7.3,' cm,  D_p=',f8.3,'  w=',f8.3,' keV')
C     write (15,1521) thi, 0.001*dmpb,0.001*whac

      call ESCAL
      
      END

      subroutine ESCAL
Calculate energy scale for spectrum on linear plot so that there are 
C ksp values between the lowest and highest delta
#include "simsin.inc"

C     if (leh .gt. ket) print*, ' leh,ket=', leh, ket,' ns=',ns
C     print*, ' leh,ket=', leh, ket,' ns=',ns
      ftx = 0
      do j=1,leh
         if (FT(J) .gt. ftx) jfx = j
         if (FT(J) .gt. ftx) ftx = FT(j)
         FT(j)  = 1000.0 * FT(j)
         ETT(J) = ET(NS+j) / 1000.0 ! convert eV to keV
         if (j .le. 5) write (3,*) ' ESCAL:',j,ns,ett(J)
D        if (j .lt. 10) print*, j, ett(j), ft(j)
D        if ((j/10)*10 .eq. j) print*, j, ett(j), ft(j)
      end do
      write (3,*) ' max F @',jfx,FT(jfx),' F(1)=',FT(1)
C     print *,    ' max F @',jfx,FT(jfx),' F(1)=',FT(1)
      asz = 0
      do 3 j=1,leh    
         asz    = asz + FT(j) * (ETT(j+1) - ETT(j) )
         if (j .le. 5)         write (3,*) j,ett(j),asz
C        if (j .le. 5)         print *,    j,ett(j),asz
D        if ((j/10)*10 .eq. j) print*, j,ett(j),asz
D        if (asz .gt. 0.988)   print*, j,ett(j),asz
C        if (asz .ge. 1.e-7) go to 2
      emmi = ETT(1)
      if (J .lt. jfx .and. FT(J) .le. 0.002*FT(jfx)) emmi = ETT(j) 
      if (J .lt. jfx .and. FT(J) .le. 0.002*FT(jfx)) jmi  = j 
      assi = asz
      jii  = j
C     go to 3
 2    if (asz .le. 0.9999) go to 3
      assx = asz
      emmx = ETT(j) 
      jxx  = j
      go to 5
 3    continue
      
 5    continue

C     print *,    ' emmi, emmx=', jmi,emmi,jxx,emmx,' sum=', assi,assx
      if (emmx .gt. 4.0 * dmpb/1000.0) emmx = 4.0 * dmpb / 1000.0
      write (3,*) ' emmi, emmx=', jmi,emmi,jxx,emmx,' sum=', assi,assx
C     print *,    ' emmi, emmx=', jmi,emmi,jxx,emmx,' sum=', assi,assx
      snit = sqrt (reso*emmi)
      if (emmi .gt. snit) emma = emmi - 3.6 * snit
C     is makes the spectrum long enough to include the noise
      det  = (emmx - emma ) / ksp
      det  = 0.01               ! keV

      Ksn = 3.6 * snit / det
C     E1  = emma - det
      E1  = 0
C     print 306,     det, E1,snit, Ksn

      write (3,306)  det, E1,snit, Ksn
 306  format (' det=',f7.3,' E1=',f7.3,' snit,Ksn=',f8.5,i5)

      if (E1 .lt. 0) E1 = 0

      call SPLINE(ETT, FT, leh, 0.d0,0.d0, phin)

      jws = jii - ksn
      bss = 0
      do j=1,ksp
         ES(j) = E1 + j * det
         if (j .eq. 1) go to 6
D        if (j .le. 10) print*, j, ES(J), ett(j+jws), ft(j+jws),FS(j-1)
D        if ((j/200)*200 .eq. j) print*, j, ES(J), FS(J-1)
         
 6    call SPLINT(ETT, FT, phin, leh, ES(J), FS(J) )

C     if (ES(j) .lt. emmi) FS(J) = 0
C     if (ES(j) .lt. emmi) jp1   = j
      jp1 = 1
      bss = bss + FS(J) * det
      end do

      write (3,307) det,  E1, ES(ksp),snit,bss
C     print 307,    det,  E1, ES(ksp),snit,bss
 307  format (' det=',f7.3,' E1=',f7.3,' Emax=',f9.3,' snit,bss=',2f8.5)
      
      END

      subroutine OUTPU
C     include 'SIMSIN.CMM/list'
#include "simsin.inc"

      asz = 0
      ast = 0
      do 12 k=jp1,ksp
         if (sp(K,1) .le. 1.e-21) sp(K,1) = 1.e-20
         if (sp(K,2) .le. 1.e-21) sp(K,2) = 1.e-20
         asz = asz + sp(k,1) * det
         ast = ast + sp(k,2) * det
         if (K .gt. 600 .and. ast .le. 1.e-7) go to 12
C        if (ES(K) .lt. emmi) go to 12
D        if ((k/100)*100 .eq. k) print 512, k,ES(K),sp(k,1),asz,sp(k,2),ast
C     if ((K/10)*10 .eq. K .and. sp(K,1) .ge. 0.002) 
C     * write (15,512) k,ES(K),sp(k,1), asz,sp(k,2), ast
C     if (K .le.1000) write (5,512)  k,ES(K),sp(k,1), asz,sp(k,2), ast [before 27.6.2000]
      write (5,512)  k,ES(K),(sp(k,L),L=1,6)
 512  format (1x,i6, f10.4, 1p6e12.5)
 12   continue
      do 15 k=jp1,ksp,5
         if (ES(k) .gt. 12.0) go to 15
C     if (K .gt. 150) write (16,1612) ES(K),sp(k,1)
 1612    format (1x, f10.4, 1pe12.5)
 15   continue
      
C     do 17 k=jp1,ksp,10
C     if (ES(k) .gt. 12.0) go to 17
C     if (K .gt. 150) write (16,1612) ES(K),sp(k,2)
C     17 continue
      
      END

      subroutine TIMDAT(TEE)
      character*8 timx
      character*9 day
      character*22 TEE

      call TIME(timx)
      day = ''
*      call DATE(day)

      write (TEE,333) timx,day
 333  format (2x,a8,3x,a9)

      END


      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)
      implicit double precision (a-h,o-z)
      PARAMETER (NMAX = 2000)
      DIMENSION X(N),Y(N),Y2(N),U(nmax)
      IF (YP1 .gt. 0.99E30) THEN
        Y2(1)=0.
        U(1)=0.
      ELSE
        Y2(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      IF (YPN .gt. 0.99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
      RETURN
      END

      SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y)
      implicit double precision (a-h,o-z)
      DIMENSION XA(N),YA(N),Y2A(N)

      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.) PAUSE 'Bad XA input.'
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     *      ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      RETURN
      END


      SUBROUTINE SPLINE2(X,Y,N,YP1,YPN,Y2)
      implicit double precision (a-h,o-z)
      PARAMETER (NMAX = 3003)
      DIMENSION X(N),Y(N),Y2(N),U(nmax)

C     print*,' spli2',N,yp1,ypn
      IF (YP1 .gt. 0.99E30) THEN
        Y2(1)=0.
        U(1)=0.
      ELSE
        Y2(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      IF (YPN .gt. 0.99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
      RETURN
      END

      SUBROUTINE SPLINT2(XA,YA,Y2A,N,X,Y)
      implicit double precision (a-h,o-z)
      DIMENSION XA(N),YA(N),Y2A(N)

      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.) PAUSE 'Bad XA input.'
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     *      ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      RETURN
      END
