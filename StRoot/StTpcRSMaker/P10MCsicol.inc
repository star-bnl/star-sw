	implicit double precision (a-h,o-z)
C	include 'PARAM.CMM/list'
#include "PARAM.inc"
	parameter (ndim = 20005, kdi=501, msp=3001, nxp=2000, KL=7,nx=10)
	parameter (kdim=300, nsig=1252)
	common /stet/ dmx(nx),dmn(nx), th(nx), elss(ndim,nx), dep
	common /inte/ phn1(ndim),pc(nx),pmm(nx)
	common /spe1/ ex(ndim), ph(ndim,nx), DMP(2)
	common /spe2/ fd,sg(ndim),wid(nx),xng
	common /pia/  yy(kdi),DES(kdi),Dp(kdi),DK(kdi),dsp, aph(ndim)
	common /spec/ cch, phg(ndim,nx),av(nx)
	common /sped/ kals(nxp,nx),ktot(nxp),ntot(nxp),lks(nx,nx)
	common /segm/ jtk(nx),inn, ksp, mr,kp, nk1,nk2,np(4),kst(nxp,2)
	common /Data/ iraf, JG,jx(nx), ntr, nseg, ngu,mpt,ms0(nxp)
  	common /mar/  D1,ars(ndim), rl(kdim), art(kdim), etb(ndim)
        common  f(ndim),h(ndim),E(ndim),DI(ndim),dE(ndim)
        common  bg,EZ(ndim),sf(ndim)
        common /ut/   F0,H0,U,um,Euu,CZ0,CN,CM0,CM1,CM2,CM3,CM4,CMQ,xn
        common /uta/  N1,NU,jma,k1,nels(2),jbh,klm,ners,nee,nm,npm,nzch
        common /IND/  KI,N2,N2P,MIE,MIF,MIH,LEF,LEH,nume,lemx, klim
        common /ABC/  Aw,atnu, dEdx,exth(2), rho,thi,Za,xi,rkap
        common /NML/  saxk,Etop,bemx, FSG,zi,su(8), rfD,rfw,Duu
        common /MEAN/ CMA,CMB,CMD,D2,D3,D4, tdedx,tDD(2,4),xkmn(200)
        common /SPTT/ sig(nsig,6),stp(4),tsig(4),rM2(4),rim(nsig)
        common /EP12/ ep(nsig,2),dfdE(nsig),BB(2), racc
        common /ENER/ bgg(100),Elim, Emin,Efin,Emau,Emax,gam,pkE
        common /ASSE/ ASt(ndim), ASP(ndim), ASS(ndim), BEdx(ndim,5)
        common /const/blg,bglg,dec, elm,telm,fac,pf,tmcb, sso, pmom
	common /EVA/  ptM,beta, betasq,bett(8), bbb,sbb,rs1,drs
	common /alco/ wha(2),dlhl,Dlhc,PB
	character*22 TEE
	common /adta/ tit(9), TEE
	Character*8,  tit
