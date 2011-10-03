	program P10MCsicol			! 28.4.2004 from P10MCsicol.for
C Function: Monte Carlo simulation of particle segments in a TPC filled 
C with P10 gas, as a function of particle speed \beta\gamma=bg. 
Cingle segments of length EXTH(1). The method used is described in 
C Bichsel, Nucl. Inst. Meth. B 52 (1990) 136.
C The number of particles used is given by NTR [100,000].
C Nominally the calculation is made for pions by NPM=2 in subroutine PREP,
C but the result will be valid for all particles with speed bg.
Cis produces a MC representation of the energy loss spectrum for x.
Ce initial random number  iraf  is an input. 
Ce primary collision spectrum is calculated in subroutine SPECTRA, using the
C Fermi-Virtual-Photon method C Ref: Bichsel, Rev Mod Phys 60 (1988) 663, &
C Allison & Cobb, Ann. Rev. Nucl. Sci. 30 [1980] 253.

C The energy deposition DP(j) in a segment is calculated in subroutine TRACK.
C An ADC number NNP is assigned and placed into ADC bin KTOT, the # of 
collisions per segment is stored in ntot, and the energy loss spectrum is given
C by sm0.

C Spectra for KTOT and various descriptors are calculated in loop do 81 and
C loop do 87 in the main program.

Ce output files P10MCsicol.opa, .opp  & .inp  are used to record details of the
C operation of the execution. The file P10MCsicol.tab gives the descriptive
C parameters <D/x> and sigma for the truncated mean <D/x> & the rms of the
Cimulated track, the most probable values D_p and FWHM w for exth(1) are also 
cuiven. exth(2) is not used here.

C Hans Bichsel, 1211 22nd Ave East, Seattle WA, 98112-3534; Tel: 206-329-2792
C linear scale for spectra made with cubic SPLINE

C	include 'P10MCsicol.inc/list'
#include "P10MCsicol.inc"
	
C	open (3, file='P10MCsicol.OPA', status='new')
C	open (9, file='P10MCsicol.tab', status='new')
C	open (16,file='P10MCsicol.dat', status='new')
	open (3, file='P10MCsicol.OPA')
	open (9, file='P10MCsicol.tab')
 	open (16,file='P10MCsicol.dat')
      
	call TIMDAT(TEE)

	write (3,*) ' P10MCsicol.OPA, time ', TEE 
	write (9,*) '( P10MCsicol.tab ', TEE,' )'

		print*, ' give exth[cm],bg & iraf'
		read *,        exth(1), bg!, iraf
	iraf = 75931
C	iraf = 15935
	cch  = 0.2	     ! depends on x
C	     print*, ' give ntr'
C	     read *,        ntr

	ntr = 1
	ntr = ntr * 10000

		write (9,901)  exth(1),iraf
 901		format('( x=',f8.3,' cm.  iraf=',i8,' )')
		write (3,647) iraf
  647		format ('( Random # seed=', i6,' )')
 
		write (16,300)  TEE, ntr
 300	format ('( P10MCsicol ',a22,'  ntr=',i7,' )')

	call EPSI			

C	do 69 N=1,26		! bg loop
	do 69 N=1,1
C	bg  = 3. + (N-1)*0.7
C	pcm = bg * pion 
C	pcm = 650. 
C	bg  = pcm / pion
C	bg  = 0.1
	     print*, ' ++++++++++ N,bg=', N,bg,' p=',pcm

	kx = 1
	do 1 KI=1,1		! this program originally was for inner and outer
 1 	call INP		! segments in the STAR TPC

		write (3,301)  cch, bg,pcm
		write (16,301) cch, bg,pcm
 301	format('( ch=',f6.3,' bg,p',f8.4,f8.2,')')
C	     print*, ' before ntr'

	do 63 j=1,nxp		    ! nxp = 2000
	ktot(J) = 0
	ntot(J) = 0
 63	ms0(J)  = 0

	xlam = 1.0 / fsg
		write (9,963) fsg, xlam,exth(1)
 963		format (' fsg, xlam=', f9.4,f9.5,'cm.   x=',f8.3,' )')
	do 65 i=1,ntr		! ntr is input above

	call TRACK(xlam)

	if ((i/50000)*50000 .eq. i) print *, ' i=',i
 65	continue

	lst = 0
	ldt = ntr
	ltt = 0
	jmx = 0
	Jlt = 0	 ! index for lower 10%
	Jut = 0	 ! index for upper 10%
	kup = ntr / 100
	k50 = ntr / 5
	bst = 0
		write (9,902) 
 902	format(' (  Delta   ktot     ntot     ms0  )')

	do 81 L=1,nxp
	del = L*cch
	if (ktot(L) .gt. jmx) LMX = L
	if (ktot(L) .gt. jmx) JMX = ktot(L)
	ldt = ldt - ktot(L)
	lst = lst + ktot(L)
	kst(L,1) = ldt
	kst(L,2) = lst
	if (lst .le. kup .and. lst .lt. k50) Jlt = L
	if (ldt .ge. kup .and. ldt .lt. k50) Jut = L
	ltt = ltt + L*ktot(L)
 75	bst = bst + L**2 * float(ktot(L)) / 100

	if (ktot(L)+ntot(L)+ms0(L) .le. 0) go to 81
		write (9,381) deL, ktot(L),ntot(L),ms0(L)
 381		format (f10.2, i7,3i9)
 382		format (i5,i7,2i9)
 81	continue

	     print *,    ' JMX,LMX=',JMX,LMX
	     write (3,*) ' JMX,LMX=',JMX,LMX

	avt = float(ltt) / (100 * float(lst) )
	sgt = bst / (100* float(lst)) - avt**2
	sst = sqrt(sgt)
	     write (16,1680) Jlt,LMX,Jut
	     print 1680,     Jlt,LMX,Jut
 1680      format ('(  1% limits & peak',3i5,' )')
	sgper = 100.0 * sst / avt
	     write (16,1681) avt,sgper, sst
	     print 1681,     avt,sgper, sst
 1681	format ('( <D/x>=',f7.3,' sig=',f8.3,' D',f8.4,' )')

	cst = 0
	mst = 0
	do 85 L=1,nxp
	mst = mst + ktot(L)
	cst = cst + (0.01*L - avt)**2 * float(ktot(L)) / lst
	ast(L) = sqrt(cst)

	if (ktot(L) .gt. 0) write (3,385) L, ktot(L),kst(L,1),sqrt(cst)
 385		format (I6,i7,i9,f11.5)
C	if (ktot(L) .gt. 0) write (16,382) log(float(L)), ktot(L)
C	if (ktot(L) .gt. 0) write (16,382) L, ktot(L),lst
 85	continue


	do 87 L=1,nxp
 87	if (ktot(L).gt.0) write (16,385) L, ktot(L),kst(L,1),ast(L)

 69		write (9,969) N, bg,pcm, avt, sst, dmp,wha
 969		format (i3,f9.4,f8.2,f9.4,f8.4,' ',4f9.3)

	call TIMDAT(TEE)
	write (3,*)  ' P10MCsicol.OPA, time ', TEE,' end'
	write(9,*) ' ( P10MCsicol.tab, time ', TEE,' end )'
C	stop 

	END

	subroutine TRACK(xlam)
#include "P10MCsicol.inc"

	xt    = 0		! travel along segment 
	mx    = 0		! # of collisions in this segment [=track]
	DEL   = 0		! cumulative E-loss per segment
	do 25 j=1,999		! number of collision
	rx    = rndm(iraf)	! random # for distance to next collision
	xj    = -xlam * log(rx)	! distance to next collision
	xt    = xt + xj			! total distance so far
	if (xt .gt. exth(1)) go to 35	! distance now greater than segment length
	mx = mx + 1

	yy(j) = rndm(iraf)	! random # for E-loss in this collision
	jj    = 20000.0 * yy(j)		! index for energy loss table 
	if (jj .le. 0) jj = 1
	if (jj .gt. 20000) jj = 20000
	if (yy(j) .gt. .9990000) elss(JJ,1) = .0057 / (1.d0 - yy(j))
Calculate energy loss for large random # - approximate, see M_0 in .opa
	DEL = DEL + elss(jj,1)		! sum of energy losses in this segment
	me  =  elss(jj,1) / cch + 0.5	! convert E-loss into ADC #
	if (me .gt. nxp) me = nxp
	ms0(me) = ms0(me) + 1			! accumulate E=-loss spectrum
C		print 327, j,jj,elss(jj,2)
 327		format (i4, i6,4f10.4)
 25	continue

 35	continue		! end of accumulation for this segment

C	print 325, k,DP(k),gup		! used for diagnostics
 325	     format (2i7, f7.3, 1p4e11.3,1x,0p3f9.3)
C	if (K .eq. 40) pause
 326	     format (i5,2f10.3,f9.1)

	nnp = DEL / cch + 0.5	! convert sum of E-loss into ADC #
	if (nnp .lt. 1) nnp  = 1
	if (nnp .gt. 9999) nnp  = 9999
C	if (nnp.eq.14) print *,nnp,gup,' ml=',ml   ! used for diagnostics
C	if (nnp.eq.14) pause
C	     print 325, jj,nnp,DP(1),elss(jj,1),phg(jj,1),gup
	ktot(nnp)  = ktot(nnp) + 1		! ADC spectrum of Eloss in segments
	ntot(mx )  = ntot(mx)  + 1		! ADC spectrum of # of collisions

	END

	subroutine INP
#include "P10MCsicol.inc"

C	if (KI .eq. 1) open (15, file='P10MCsicol.inp', status='new')
	if (KI .eq. 1) open (15, file='P10MCsicol.inp')
		write (15,*) ' P10MCsicol.inp, INP ', TEE,' K=',KI
	 open (17, file='P10MCsicol.yur')
		write (17,*) ' P10MCsicol.yur, INP ', TEE

	call ROOrev			! produces single collision spectrum

		print *,' after ROOrev '
	do 9 j=1,leh			! defined in subroutine SPECT
C	if ((j/20)*20+1 .eq. j) print*, j,EZ(j), sf(j)
C EZ is energy loss E in a single collision 
C ph the collision cross section differential in E
C sf = integral over ph from 0 to E
	EZ(J) = 0.001  * E(J)		! convert eV into keV
	H(J)  = 1000.0 * H(J)	! renormalize spectrum
		write (17,1611) EZ(J), H(J),sf(J)
 1611		format (1p3e15.7)
	if (J .eq. 1) jtK(KI) = 1
	if ((j/100)*100 .eq. j) print*, j,EZ(j), sf(j)
	jx(KI) = j-1
 9	continue
Cetup for SPLINE used to invert ph(EZ) into EZ(ph)
		print*, ' jx(K)=',KI, jx(KI)
	dd1 = (sf(2) - sf(1)) / (EZ(2) - EZ(1))
	ddL = (sf(jx(KI)) - sf(jx(KI)-1)) / (EZ(jx(KI)) - EZ(jx(KI)-1))
D		write (3,*) ' derivatives=',dd1, ddL
		print*,     ' derivatives=',dd1, ddL

	call SPLINE(sf, ez, jx(KI), dd1,ddL, phn1)
               
	dmn(KI) = EZ(1)
	dmx(KI) = EZ(jx(KI))
	nix = 20000.0 * sf(jx(KI))	! sf(jx(KI)) could be less than 1.00000
Cis is a questionable call - think about it
	if (nix .gt. 0) dep = (dmx(KI) - dmn(KI)) / nix
	if (nix .gt. 0) dsp = (dmx(KI) - dmn(KI)) / msp
	if (dsp .ne. 0) ksp = dmn(KI) / dsp
	     print 310,     nix,dep,dmn(KI),dmx(KI)
	     write (3,310) nix,dep,dmn(KI),dmx(KI) 
	     write (3,310)  nix,dep,dmn(KI),dmx(KI)
 310	     format (' nix=',i6,' dep=',f9.6,' dmn,dmx[KI]=', 2f9.3)

	write (3,313)  ksp,dsp, sf(jx(KI))
 313		format (' (ksp, dsp=',i6,f8.3 ,' sf[jx]=',f8.2 ,' )')
	do 23 n=1,20000		! number of steps for random E-loss
	pp  = 0.00005 * n

	call SPLINT (sf, ez, phn1, jx(KI), pp, elss(n,KI))
C	if (n.le.nix) call SPLINT (sf, ez, phn1, jx(KI), pp, elss(n,KI))

C	if (n.gt.nix.and.nix.gt.0) elss(n,KI) = elss(nix,KI)
	if (pp .le. sf(1)) elss(n,KI) = sf(1) 

	nr = n/5 + jtk(KI) - 1		! lines to st. 23 produce print for checks
	if (nr .lt. 1) go to 44
	if (n .ge. 5) pd = ph(n/5,KI)
	if (n .ge. 5) edd= EZ(n/5)
D	if ((n/10)*10 .eq. n) print*, n, EZ(n), phn1(n),elss(n,KI)
D	if ((n/10)*10 .eq. n) print*, pp, EZ(n), ph(n,KI)
 44	continue
C	if (n .gt. 20) go to 73
		write (15,1523) n,pp,elss(n,KI),EZ(N),h(n),sf(n)
C 73	if ((n/5)*5 .ne. n) go to 23
C		write (15,1523) n,pp,elss(n,KI),EZ(N),h(n),sf(n)
 1523		format (1x,i6,f12.5, f12.5,3x,f11.4,1p2e12.4 )
 23	continue
	
	END

 	subroutine ROOrev	! 12.6.2003 on PC [modified from ROOsgl
C	program ROOrev		! 12.6.2003 on PC [modified from ROOsgl
C	ROOSGL	! 5.6.2003 on PC [modified from ROON64, ROO data]
Calculate f (D), D_p & w, with YXINT to find wha, MAIN OUTPUT file
C is ROOrev.dat;  print of spectra suppressed in LClin.*. 5.6.2003: add SOLV
#include "P10MCsicol.inc"

C	open (3, file='ROOrev.OPA',status='NEW')
C	open (3, file='ROOrev.OPA')
  
	call TIMDAT(TEE)
                write (3,*)  'ROOrev.OPA   ', TEE,'  N2=64'

	th(KI) = exth(KI)
 
        call PREPE

	nee = 1
        call PREP
        call SRUL
        call SPECT

	stpp = 1000. * tdedx

	ners = 1

        END
 
        subroutine EPSI
#include "P10MCsicol.inc"
C read epsilon & df/dE data from EP1, also energy loss scale 

 411            format (9a8)

	open (11, file='P10ep1.DAT', status='OLD')

	do 1 m=1,11
                read (11,411) tit
                write (3,411) tit
 1		print 411, tit

	ZA  = 17.2	        ! these number are added together:
	Aw  = 37.5575	    ! ZA = 0.1*10 + 0.9*18
	rho = .0015616      ! AW = 0.1*16.043 + 0.9*39.948
	N2  = 64
	 
	sua = 0 
        do 3 j=1,ndim
           read (11,*,end=69)  iyy,etb(J), ep(j,2), ep11, ars(j)
	dfdE(j) = ars(j) 
	if (J.gt.1) sua = sua + (etb(J) - etb(J-1)) * ars(J)

	leh     = j
	ep(j,1) = 1.0 + ep11
C	if ((j/50)*50 .eq. j) print*, j,ebw, E(j), dfdE(j), ep(j,1)
 3      continue

 69	close (11) 
	Emin = etb(1)
		print *, ' EPSI ',leh,Emin,sua
		print 369,     leh,Emin,sua
		write (3,369)  leh,Emin,sua
 369	     format (' EPSI, leh=',	i5,' Emin=',f8.4,' sua=',f12.3)
 
        END

        subroutine PREP	! prepare particle and kinematic data
#include "P10MCsicol.inc"
C        include 'P10MCsicol.inc/list'
 
C		print*, ' particle: p=1,pi=2,k=3,e=4,5=d:'
C		read*, npm
	npm = 2
C		print*, ' give absorber thickness (cm):  '

        PTM  = pion
        if (npm .eq. 1) PTM = prm
        if (npm .eq. 3) PTM = 493.667
        if (npm .eq. 4) PTM = elmm
        if (npm .eq. 5) PTM = 1875.587

        zi   = 1.0
        atnu = 6.0222e23 * rho / Aw
 
                write (3,601) PTM,zi
 601    format (' PREP;  particle mass=',F10.3,' MeV, charge=',f3.0)

        call EVANS

	Efin = Emax
        if (Emax .gt. Etop) Efin = Etop
        saxk = 153540 * zi**2 * rho / (betasq*AW)
        Emk  = saxk * Emax
 
                write (3,602) Za,Aw,exth,saxk,Emk
 602    format (/ '  Z=',F6.2,'  A=',f9.4,'  t=',0p2f7.2,'cm',
     1  '  k/Z=',f10.5,'  k*Emax/Z=',1pe12.5)

        END
 
        subroutine EVANS		! calculate kinematic properties
#include "P10MCsicol.inc"

	jkm = 3		! originally a selection: 1=kinetic energy, 2=momentum
	xxx = bg	! 3=\beta \gamma

        go to (11,22,33), jkm
 
 11     pkE = xxx			! p-article k-inetic E-nergy
        W   = xxx/ptM + 1.0
        bg  = sqrt(W**2 - 1.)
                go to 34
 
 22     pmom = xxx		! p-article mom-entum
        bg   = xxx / ptM
        W    = sqrt(bg**2 + 1.0)
        pkE  = ptM * (W - 1.0)
                go to 35
 33     bg  = xxx                
        W   = sqrt(bg**2 + 1.0)
        pkE = ptM * (W - 1.0)

 34     pmom = ptM * bg

 35     betasq = bg**2 / (1 + bg**2)	! \beta**2
        beta = bg / W
        gam  = W						! \gamma
        ptE  = ptM * W
        telm = 2 * elmm				! twice el-ectron mmass
        Emax = ptM * (W**2 - 1) / (ptM/telm + telm/ptM + W)	! 2mv^2
        if (npm .eq. 4) Emax = pkE / 2		! for electrons
                print*, npm,' Emax',Emax
        Emx  = telm * bg**2			! other approximation for 2mv^2
 
                write (3,333) bg,pmom,pkE
 333    format (/,'  beta-gamma=',f11.4,'  momentum=',f13.4,' MeV/c'/
     1       '   kinetic E of incident particle=',f15.3,' MeV')
                write (3,334) betasq,gam,Emax,Emx, Etop/1.e6
 334    format ('  beta**2=',f9.6,'  gamma=',f12.5,' E_Max=',
     1          1p3e12.4,' MeV')
        Emax = 1.e6 * Emax		! in eV
      
        END
 
        subroutine PREPE		! Calculate the energy-loss table
#include "P10MCsicol.inc"
 
        nume = 1250
        u    = log(2.) / N2		! N2 from P10ep1.dat
        um   = exp(u)
        E(1) = Emin
        EXS  = 1.
C		print*, ' PREPE', n2, E(1),emin
                write (3,609) N2,Emin, u,um
 609            format(/' PREPE;   N2=',I4,
     1  	 '   Emin=',f8.3,'   u=',F9.6,'  e**u=',f10.6)
        lemx = nume + 450		! additional length for convolutions

        do  11 L=1,lemx
        EXS = EXS * um
        E(L+1) = E(L) * um
C	if ((L/50)*50 .eq. L) print*, ' L,E=',L,E(L),exs,um
        DI(L)  = -log(1.0 - 1.0/EXS) / u	! used in convolutions
        DE(L)  = E(L+1) - E(L)
	H(L)   = 0.
 11     continue

        if (leh .gt. nume) leh = nume
        Etop = E(nume) * sqrt(um)
C                print*, ' PREPE: Efin, Etop, Emax=',Efin,Etop,Emax

        END
 
        subroutine SRUL
C Define parameters and calculate Inokuti's sums, 
C Sect 3.3 in Rev Mod Phys 43, 297 (1971)
#include "P10MCsicol.inc"
        dimension avi(3),sru(3)
 
        elm = 1.0e6 * elmm
        fac = 8.0 * pi * Ry**2 * bohr**2 / elm
        DEC = zi**2 * atnu * fac / betasq
	racc= 1.0 / (atnu * hbc * fanco)
Conversion factor for second density effect term
                write (3,305) atnu, fac,racc, dec
 305	format ('  subroutine SRUL: # of atoms per cm**3=',1pe12.4,3x,
     1	'fac=',e11.4,'eV*cm**2,  racc=',e11.4,/'  DEC=',0pf11.4,' eV/cm')
 
        blg  = log ((2.*elm) * bg**2) - betasq
        bemx = betasq / Emax
        pf   = pkE * 1.e6
        tmcb = 2. * elm * betasq
                write (3,307) betasq, blg, bemx, tmcb
 307	format ('  beta**2=',f12.10,'  blg=',f9.4,
     1  '  betsq/Emax=',1pe11.4,'  2mv**2=',e11.4,' eV')
 
        do 3 L=1,3
        avi(L)  = 0
 3      sru(L)  = 0
 
        do 5 j=1,nume
        do 5 L=1,3
        sru(L)  = sru(L) + dfdE(j) * dE(j) / E(j)**(2-L)
 5      avI(L)  = avI(L) + dfdE(j) * log(E(j)) * dE(j) / E(j)**(2-L)
	sso     = sru(2)
C		print*, ' SRUL, sso=', sso
                write (3,375) sru, avI
 375            format (/' S(L)=',2f12.5,f12.2,'  ln(I)=',2f12.5,f12.2)

	rIv = exp (avi(2) / sru(2) )
	smi = Ry * sru(1)
	xLmi= RY * (avi(1) - log(Ry) * sru(1) )
                write (3,377) rIv, smi,xLmi
 377		format (3x,'I-value=', f7.2,' Ino values:',2f9.3)

                write (3,*) '  following data without density effect'
C these values are auxiliary output related to dE/dx etc.
        write (3,*) '  S(0)*blg=',sru(2)*blg,'   2*L(0)=',2*avI(2)
        write (3,*) '  S(1)*blg=',sru(3)*blg,'   2*L(1)=',2*avI(3)

        END
 
        subroutine SPECT
C generate collision spectrum from sigma, ep-1 and ep-2 with Allison's method
#include "P10MCsicol.inc"
 
D		write (3,*) 
D		write (3,*) '  subroutine SPECT'
D		write (3,308)
 308    format (3x,'j',5x,'E/eV',5x,'df/dE ',5x,'sgg',6x,'sgh',
     1  7x,'S 1',6x,'S 2',6x,'S 3',5x,'sum S',5x,'S(0)',3x,'dE/dx'/)
 
                jpr = 10		! parameters used in print-out of tables
                jpd = 10
        do 3 L=1,4
        rM2(L)  = 0
        Tsig(L) = 0
 3      STP(L)  = 0
        S0      = 0
 
C do loop for Fano Eq 47, look at Eq. [  ] in Allison & Cobb
        do  5 j=1,nume
        S0  = S0   + dfdE(j) * dE(j)		! sum for DOS
	if (E(j) .gt. EMAX) go to 11
	leh = j
        if (npm .eq. 4) uef = 1 + (E(j)/(pf-E(j)))**2 + (((gam-1)/gam)
     1  * E(j)/pf)**2 - (2*gam - 1)*E(j)/(gam**2 * (pf - E(j)))
        if (npm .ne. 4) uef = 1 - E(j) * bemx     
C  uef from Uehling Eqs. 9 & 2
C	if (j .eq. 1) print*, ' uef=',uef
        sig(j,1) = E(j) * dfdE(j) * log(tmcb / E(j) ) 
 14     epbe = 1.0 - betasq * ep(j,1)               
C  Fano Eq 47
        if (epbe .eq. 0) epbe = 1e-20
        sgg = E(j) * dfdE(j)*(-.5)*log(epbe**2+(betasq*ep(j,2))**2)
        thet = atan (ep(j,2) * betasq / epbe)
        if (thet .lt. 0) thet = thet + pi         
C  plausible-otherwise I'd have a jump
C  Fano says [p 21]: 'arctan approaches pi for betasq*eps1 > 1'

        dgh = ep(j,1)**2 + ep(j,2)**2
C	if (abs(dgh) .le. 0) print*, ' dgh',j,dgh, ep(j,1), ep(j,2)
	if (abs(dgh) .le. 0) dgh = 1.e-15
        sgh = E(j)**2 *(betasq - ep(j,1) / dgh) * thet
        sgh = racc * sgh
        sig(j,2) = sgg + sgh
	if (abs(sso) .le. 0) print*, ' sso=',sso
        sig(j,3) = S0 * uef * ZA / sso
C the integral was over  d lnK rather than  d lnQ
C       if ((j/50)*50 .eq. j) print 327, E(j),sgg,sgh,(sig(j,ii),ii=1,3)
D327            format (1x,f10.1, 1p6e11.3)

        sig(j,4) = 0

        do  27 L=1,3
                Tsig(L)  = Tsig(L)  + sig(j,L) * dE(j) / E(j)**2
                STP(L)   = STP(L)   + sig(j,L) * dE(j) / E(j)
                rM2(L)   = rM2(L)   + sig(j,L) *  dE(j)
 27             sig(j,4) = sig(j,4) + sig(j,L)
                                            
        Tsig(4) = Tsig(4) + sig(j,4) * dE(j) / E(j)**2
	sf(J) = Tsig(4)	* DEC
        STP(4)  = STP(4)  + sig(j,4) * dE(j) / E(j)  
        rM2(4)  = rM2(4)  + sig(j,4) * dE(j)
D               if (j .eq. 1) go to 28
C               if (j .ge. 320 .and. j .le. 326) go to 28
D               if ((j/10)*10 .ne. j) go to 5
D28     write (3,608) j,E(j),dfdE(j),sgg,sgh,(sig(j,L),L=1,4),
D    1          S0,STP(4)
D608            format (1x,i4, f10.1,1pe11.3,0p9f9.4)
 5      continue

 11		write (3,*) '  uef=',uef
                write (3,374) Tsig,STP,rm2
 374    format (/9x,'Integ. over sig =',4F13.4/2(26x,4f13.2/))
        FSG  = Tsig(4) * DEC
        dEdx = STP(4) * (dec/1E3)
        rmf  = rM2(4) * (dec/1e6)

		write (17,1674) FSG
 1674		format (1pe15.7,'  Sigma_t')		
C                print 388,    S0,FSG,dEdx,rmf
                write (3,388) S0,FSG,dEdx,rmf
 388            format (/,10X,'Zeff=',F7.3,4X,'# coll/cm=',f11.3,4x,
     1          'dE/dx=',F9.4,' keV/cm',3x,'M2=',f12.4,' keV**2/cm')
 
        write (3,*) ' DEC=',dec,'  # atoms/cm**3=',atnu,'  fac=',fac

        call SPTS

        END
 
        subroutine SPTS
#include "P10MCsicol.inc"
 
D               write (3,333)
D333    format (/4X,'SPTS,F.333:',/15X,'E',7x,'sig*E**2',6x,'sig',
D    1       10X,'M 0',11X,'M 1',11X,'M 2',10X,'<E>',/)
                SGM  = 0
                Stpw = 0
                SECM = 0
                jpr  = 10
                ja   = 10

        do 75 j=1,leh
	if (E(j) .gt. Emax) go to 76
	sf(J) = sf(J) / sf(leh)
C	he2  = sig(j,4) / ZA
	he2  = sig(j,4) * dec
        H(j) = he2 / E(j)**2
        SGM  = SGM + H(j)*dE(j)
        STPW = STPW + H(j) * E(j) * dE(j)
        SECM = SECM + he2 * dE(j)
        eps  = STPW / SGM
 653		format (1x,i4,f12.2,1pe11.4, e16.8, 2e11.4, 0pf7.2)
               if (j .lt. 5) go to 11
               if (j .eq. nume) go to 11
               if (jpr .ne. j) go to 75
               jpr = jpr + ja
 11		write (3,654) j,E(j), he2,H(j), SGM,sf(J)
 654            format (1x,i5,f12.2, 1p5e13.5, 0pf7.2)
 75     continue

 76		write (3,610)  nume,SGM,STPW,SECM
 610    format (/1X,'F.610: nume  ',i5,' total cross section=',1PE15.5,
     1       3X,'dE/dx=',E15.5,', M2=',E15.5/2x,'see FSR-99 and CCS-9'/)
                write (3,*) ' final E=', Efin,' Emax=',Emax,' he2=',he2

Cee FSR-99
        bbb = 1. - sbb*betasq / Emax
        fft = ZA * dec
                write (3,*) ' sbb=',sbb,' eV,   bbb=',bbb,'  fft=',fft
        rm0 = bbb * ((1/Efin - 1/Emax) + 2 * (1/Efin**2 - 1/Emax**2))
     1        - betasq * log(Emax/Efin) / Emax
                write (3,*) ' residual M0=',rm0,rm0*fft,'/cm'
                write (3,*) '  If residual M0 is large, look for error'
		write (3,*)

        call HPART(fft,stpw,secm)

        END
 
        subroutine HPART(fft,stpw,secm)
#include "P10MCsicol.inc"
Calculate total dE/dx, including contributions from E>Emax
Cee FSR-101

        if (npm .ne. 4) rst = bbb * log (Emax/Efin) + 
     1          sbb * (1./Efin - 1./Emax) - betasq*(1. - Efin/Emax)
 
	ffu = fft / 1.e6
        if (npm .ne. 4) go to 6 

C for electrons, see FSR-143 and Uehling Eq 9:
        TE = pkE * 1.e6
                print*,' ele',efin,emax,TE,gam
        rst=log(Emax/Efin)+log(Emax)-log(TE-Efin)  - 1/(1.-Efin/TE)
     1       + 2 + ((gam-1)/gam)**2 * (1./8. - .5 * (Efin/TE)**2)
     2       + ((2*gam - 1) / gam**2) * (log(Emax) - log(TE-Efin))
 
 6              write (3,*) ' residual dE/dx=',rst,rst*ffu,' MeV/cm'
 
        tdedx = stpw/1.e6 + rst*ffu
        write(3,*) ' dE/dx=',tdedx,' MeV/cm ',tdedx/rho,' MeV cm**2/g'
                 print*, ' dE/dx=',tdedx,tdedx/rho
 
        secmv = secm / 1.e6
        rM2p  = Efin * (1. - 0.5 * betasq * Efin / Emax)
        del2  = secmv - ffu * rM2p
C        print *,    ' M2=',secmv,'  M"=',ffu*rM2p,' M2-M2"=',del2
        write (3,*) ' M2=',secmv,'  M"=',ffu*rM2p,' M2-M2"=',del2

        END

	subroutine TIMDAT(TEE)
	character*8 timx
	character*9 day
	character*22 TEE

	call TIME(timx)
*	call DATE(day)

		write (TEE,333) timx,day
 333		format (2x,a8,3x,a9)

	END

      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)
	parameter (nmax = 10005)
	implicit double precision (a-h,o-z)
      DIMENSION X(N),Y(N),Y2(N),U(NMAX)

      IF (YP1.GT..99E30) THEN
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
      IF (YPN.GT..99E30) THEN
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
      
      END

      SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y)
	implicit double precision (a-h,o-z)
      DIMENSION XA(N),YA(N),Y2A(N)

      KLO=1
      KHI=N
1     IF (KHI-KLO .GT. 1) THEN
        K=(KHI+KLO)/2
        IF(XA(K) .GT. X) THEN
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
