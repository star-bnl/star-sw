C
      INTEGER FUNCTION XI_RECONSTRUCT(nok,idd,trak,
     +                                ev0par_h,ev0par,
     +                                mvertex_h,mvertex,
     +                                ev0out_h,ev0out,
     +                                exiout_h,exiout   )
 
      IMPLICIT NONE
C   Functional Description : Search for secondaries 
C                            X and Omega particles and
C			     and anti-particles are 
C			     considered. Only charged decay modes.
C   Created  MARCH-1994	  S. Margetis 
C   Error conditions	: None-Yet
C   Status		: Tested with svt_hits
C----------------------------------------------------------------------
#include "/star/starlib/star/dev/sys/inc/tas_structures.inc"
#include "/star/starlib/star/dev/sys/inc/tas_user_codes.inc"
#include "ev0_ev0par_pars.inc"
#include "ev0_ev0par_st.inc"
#include "evr_privert_pars.inc"
#include "evr_privert_st.inc"
#include "ev0_ev0out_pars.inc"
#include "ev0_ev0out_st.inc"
#include "exi_exiout_pars.inc"
#include "exi_exiout_st.inc"
C----------------------------------------------------------------------
      RECORD/ table_head_st/ ev0par_h
      RECORD/ ev0par_row_st/ ev0par(*)
      RECORD/ table_head_st/ mvertex_h
      RECORD/ privert_row_st/ mvertex(*)
      RECORD/ table_head_st/ ev0out_h
      RECORD/ ev0out_row_st/ ev0out(*)
      RECORD/ table_head_st/ exiout_h
      RECORD/ exiout_row_st/ exiout(*)

C---for testing purposes only
      STRUCTURE/local_track/
        INTEGER id
        REAL*4 r
        REAL*4 rf
        REAL*4 z
        REAL*4 f
        REAL*4 tanl
        REAL*4 qR
      END STRUCTURE
 
      RECORD /local_track/ trak(10000)
C---------------
 
      INTEGER nok,id,idd(10000),id1,id2
      INTEGER ierr,ii,kk,ll,iflag,iflag1
      INTEGER i,j,k,l,q
 
      REAL	rd_cut,rv_cut,rmin_cut,dmass,mass_id(4)

      INTEGER 	maxtr,matchl,npid1,npid2,ok
      PARAMETER	(maxtr=5000)
      INTEGER 	ev0_flag(maxtr),pid(maxtr)
      INTEGER  	ntracks,ntracx,jk,itrc,vidc,itr(maxtr),vid(maxtr)
      INTEGER	jump(maxtr),ev0_hits,index
      REAL	dr,mass1(3),mass2(3),charge(maxtr),pi,ph,pst1(3),pst2(3)
      REAL	vert(maxtr,3),pvert(maxtr,3),true,rv,xv0(3)
      REAL	qrg(maxtr),phig(maxtr),tanlg(maxtr),zcom(2)
      REAL      x0(maxtr),y0(maxtr),z0(maxtr),dx,RG32
      REAL	ptrk,trk1(6),trk2(6),pdotp,ndotp,pv0(3),ppar,npar,pper
      REAL	trki(6),trkj(6),qr(maxtr),phi(maxtr),tanl(maxtr)
      REAL	xc1(2),xc2(2),r1,r2,xcom(2),ycom(2),inv_mass,rmin
      REAL	xp(2),xn1(2),xn2(2),rd,xpp(3),massl(2),pp(3)
      REAL      env0,bv0,gv0,ep,ppar_cm,mainv(3)
      LOGICAL	tpc
      COMMON	/local/	ptrk(maxtr,6)
      REAL MASST(50),QQT(50)
*
      DATA MASST/7*0.0005,.139567,.139567
     +,            3*0.493667,2*.93828,.93828
     +,            35*0./
      DATA MASS1/0.139567,.93828,.139567/
      DATA MASS2/0.139567,.139567,.93828/
      DATA QQT/0.,1.,-1.,0.,1.,-1.,0. ,1.,-1.,0.,1.,-1.,0.
     +,        1.,-1.,35*0./
*
*
*-----executable statements--------------------------------------------
*
      pi=acos(-1.)
*
      id=ev0par(1).id
      IF(id.eq.23) THEN
	mass_id(1)=1.32132
        id1=9
        id2=18
        index=2
      ELSEIF(id.eq.31) THEN
	mass_id(2)=1.32132
        id1=8
        id2=26
        index=3
      ELSEIF(id.eq.24) THEN
	mass_id(3)=1.67245
        id1=12
        id2=18
        index=2
      ELSEIF(id.eq.32) THEN
	mass_id(4)=1.67245
        id1=11
        id2=26
        index=3
      ENDIF
	rd_cut=exipar(1).dca
	rv_cut=exipar(1).dlen
	rmin_cut=exipar(1).dcaxi
*
c        mainv(1) = mvertex(1).x
c        mainv(2) = mvertex(1).y
c        mainv(3) = mvertex(1).z
        mainv(1) = 0.
        mainv(2) = 0.
        mainv(3) = 0.
*
*-----steering routine------------------------------------------------
*

        ok=0

      DO 10 i=1,nok

*
*decide here if dE/dx,TOF can say anything about PID
*if yes .and. it is a pion/proton pair proceed if not
*take GEANT ID or a (hypothetical) mass will be assigned
*
        IF(id.ne.0) THEN
	IF(idd(i).ne.id1) GOTO 10
        ENDIF

        trki(1)=trak(i).r
        trki(2)=trak(i).rf
        trki(3)=trak(i).z
        trki(4)=trak(i).f
        trki(5)=trak(i).tanl
        trki(6)=trak(i).qR


	DO 20 j=1,ev0out_h.nok

        IF(id.ne.0) THEN
	IF(ev0out(j).inv_mass(index).eq.0.0) GOTO 20
        ENDIF

        trkj(1)=ev0out(j).p(1)
        trkj(2)=ev0out(j).p(2)
        trkj(3)=ev0out(j).p(3)
        trkj(4)=0.
        trkj(5)=0.
        trkj(6)=0.
        pv0(1) =ev0out(j).p(1)
        pv0(2) =ev0out(j).p(2)
        pv0(3) =ev0out(j).p(3)
        xv0(1) =ev0out(j).x(1)
        xv0(2) =ev0out(j).x(2)
        xv0(3) =ev0out(j).x(3)

		call circle_param(trki,xc1,r1)

	 	CALL VZERO(xcom,2)
	 	CALL VZERO(ycom,2)
		iflag1=0
		call casc_geom(rd_cut,pv0,xv0,xc1,r1,xcom,ycom,zcom,iflag1)
		IF(iflag1.eq.5) GOTO 20

C----two possible intersection points ---------------------------

			DO jk=1,2
		xp(1)=xcom(jk)
		xp(2)=ycom(jk)
111		call ev0_project_track(xc1,r1,xp,xn1)
		call ev0_update_track_param(xc1,r1,xn1,trki,trk1)
		rd=sqrt( (xn1(1)-xp(1))**2 + (xn1(2)-xp(2))**2
     +			+(trk1(3)-zcom(jk))**2 )

		xpp(1)=(xn1(1)+xp(1))/2.
		xpp(2)=(xn1(2)+xp(2))/2.
		xpp(3)=(trk1(3)+zcom(jk))/2.
		rv=sqrt(xpp(1)**2+xpp(2)**2+xpp(3)**2)
*
*---decide here if it is a good candidate
*
		IF( (rd.le.rd_cut).and.(rv.gt.rv_cut).and.(rv.lt.30.) ) THEN
		massl(1)=0.13
		massl(2)=0.13
		call inv_mass(trk1,trk2,massl,pp,inv_mass,8)
* be careful. Impact parameter needs main vertex at (0,0,0)
		call casc_impact_param(mainv,xpp,pp,rmin,iflag)
                IF((rmin.le.rmin_cut).and.(iflag.ne.2)) THEN

                ok=ok+1
                exiout_h.nok= exiout_h.nok+1
                exiout(ok).p(1)=pp(1)
                exiout(ok).p(2)=pp(2)
                exiout(ok).p(3)=pp(3)
                exiout(ok).x(1)=xpp(1)
                exiout(ok).x(2)=xpp(2)
                exiout(ok).x(3)=xpp(3)
                exiout(ok).id=ok
                exiout(ok).dcapn=rd
                exiout(ok).dcav0=rmin
                exiout(ok).idba=trak(i).id
                exiout(ok).idv0=ev0out(j).id
C--calculate Armenteros variables---
        	call track_mom(trk1,0,pst1)
                pdotp=pst1(1)*pp(1)+pst1(2)*pp(2)+pst1(3)*pp(3)   !dot product of p+ and p(v0)
                ndotp=pv0(1)*pp(1)+pv0(2)*pp(2)+pv0(3)*pp(3)     !dot product of v0 and p(Xi)
                pxi  =sqrt(pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3)) 
                ppar=pdotp/pxi
                npar=ndotp/pxi
                pper=sqrt(pst1(1)*pst1(1)+pst1(2)*pst1(2)+pst1(3)*pst1(3)-ppar*ppar)
                exiout(ok).pt=pper
                exiout(ok).alpha=(ppar-npar)/(ppar+npar)
*
*---if blind run test all mass hypothesis
*

                DO ll=1,3
                IF(id.eq.0) THEN
                massl(1)=mass1(ll)
                massl(2)=mass2(ll)
                ELSE
		massl(1)=masst(id1)
		massl(2)=masst(id2)
                ENDIF
		call inv_mass(trk1,trk2,massl,pp,inv_mass,8)

                exiout(ok).inv_mass(ll)=inv_mass

C--calculate proper lifetime assuming that particle originated at main vertex
                exiout(ok).tau(ll)=inv_mass*rv/pxi     
                exiout(ok).tmin(ll)=inv_mass*rv_cut/pxi
c--calculate cos(theta_c.m.)
                env0 = sqrt(pv0*pv0+inv_mass*inv_mass)
                bv0  =pv0/env0
                gv0  =env0/inv_mass
                ep   =sqrt(pst1(1)*pst1(1)+pst1(2)*pst1(2)+
     +                     pst1(3)*pst1(3)+massl(1)*massl(1))
                ppar_cm=gv0*(ppar-bv0*ep)
                exiout(ok).cos_th_cm(ll)=cos(atan2(pper,ppar_cm))
c
                 IF(id.ne.0) GOTO 30            !blind or not?
                ENDDO

			GOTO 30			!next j
		 ENDIF				!rmin and iflag check

		ENDIF				!rd.and.rv check

		IF(iflag1.eq.3) GOTO 30		!touching circles-loop once

			ENDDO			!jk=1,2

 30	CONTINUE

 20	CONTINUE

 10   CONTINUE	


999   RETURN
      END


