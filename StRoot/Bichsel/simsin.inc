Common block SIMUP.CMM   27.12.2001
Common block ALCOV.CMM  for calculating collision spectra, 11 Dec. 1992.
	implicit double precision (a-h,o-z)
C include 'PARAM.INC/list'
#include "param.inc"

	parameter (kdim=300, ndim=6000, nsig=1252)
	common /mar/  D1,ars(ndim), rl(kdim), art(kdim), etb(kdim)
        common  f(ndim),h(ndim),E(ndim),DI(ndim),dE(ndim),xn
        common /ut/   F0,H0,U,um,EX,CZ0,CN,CM0,CM1,CM2,CM3,CM4,CMQ
        common /uta/  N1,NU,jma,k1,nels(2),jbh,klm,ners,nee,nm,npm,nzch
        common /IND/  N2,N2P,MIE,MIF,MIH,LEF,LEH,nume,lemx, klim
        common /ABC/  Aw,atnu, dEdx,exth, rho,thi,Za,xi,rkap, pc
        common /NML/  saxk,Etop,bemx, FSG,zi,su(8)
        common /MEAN/ CMA,CMB,CMD,D2,D3,D4, tdedx,tDD(2,4),xkmn(200)
        common /SPTT/ sig(nsig,6),stp(4),tsig(4),rM2(4),rim(nsig)
        common /EP12/ ep(nsig,2),dfdE(nsig),BB(2), racc
        common /ENER/ bgg(100),Elim, Emin,Efin,Emau,Emax,gam,pkE
        common /ASSE/ ASP(ndim), ASS(ndim), BEdx(ndim,5)
        common /const/blg,dec, elm,telm,fac,pf,tmcb, sso, pmom
	common /EVA/  ptM,beta, betasq,bett(8), bbb,sbb
	common /chrr/  TEE
	character*22 TEE

*	common file    LCONA.CMM   !       27 June 2000, @ BNL
	parameter (ket = 1705, ksp=6000) ! changed to 4000 on 4.12.2000
	common /part/ ETT(ket)
	common /absr/ PE,betsq
	common /alco/ dmpb,delmp,dlhl,Dlhc,scal,PB,sp(ksp,6),fgx(ksp) 
	common /spc/  det,ES(ksp),FS(ksp),GS(ksp), Tsp,hk0,hl0,stn
	common /gauu/ snoi,reso, snit, emmi
	common /indx/ jp1,jsp,nint,nik,ner, Ksn,lee,ns,nst
	common /tabl/ ET(ket), FT(ket), phin(ket)

	common/sima/ bg,bg1,dbg
	common/simi/ NN,nfi
