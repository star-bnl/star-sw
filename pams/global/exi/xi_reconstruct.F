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
 
      REAL	rd_cut,rv_cut,rmin_cut,dmass,mass_id(3)

      INTEGER 	maxtr,matchl,npid1,npid2,ok
      PARAMETER	(maxtr=5000)
      INTEGER 	ev0_flag(maxtr),pid(maxtr)
      INTEGER  	ntracks,ntracx,jk,itrc,vidc,itr(maxtr),vid(maxtr)
      INTEGER	jump(maxtr),ev0_hits
      REAL	dr,mass1(3),mass2(3),charge(maxtr),pi,ph,pst1(3),pst2(3)
      REAL	vert(maxtr,3),pvert(maxtr,3),true,rv
      REAL	qrg(maxtr),phig(maxtr),tanlg(maxtr)
      REAL      x0(maxtr),y0(maxtr),z0(maxtr),dx,RG32
      REAL	ptrk,trk1(6),trk2(6),pdotp,ndotp,pv0,ppar,npar,pper
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
      IF(id.eq.16) THEN
	mass_id(1)=0.49767
        id1=8
        id2=9
      ELSEIF(id.eq.18) THEN
	mass_id(2)=1.11563
        id1=14
        id2=9
      ELSEIF(id.eq.26) THEN
	mass_id(3)=1.11563
        id1=8
        id2=15
      ENDIF
	rd_cut=ev0par(1).dca
	rv_cut=ev0par(1).dlen
	rmin_cut=ev0par(1).dcav0
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

*first track is a positive
        IF(trki(6).lt.0) GOTO 10

*
*-----reject all tracks that are compatible with vertex within dr-----
*
c	 	CALL VZERO(trki,6)
c		call pack_track(i,trki)
c		call ev0_circle_param(trki,xc1,r1)
c		xp(1)=0.
c		xp(2)=0.
c		call ev0_project_track(xc1,r1,xp,xn1)
c		call ev0_update_track_param(xc1,r1,xn1,trki,trk1)
c		rd=sqrt( xn1(1)**2 + xn1(2)**2
c     +			+trk1(3)**2 )
c		rmin=sqrt( trakk(i,3)**2+trakk(i,4)**2
c     +			+trakk(i,5)**2 )
CSP		write (7),rd,rmin
CSP	if( (abs(rd).gt.0.).or.(abs(rmin).gt.5.) ) then
C		print*,'closest approach',rd,rmin,i,ev0_flag(i)
CSP		goto 10
C	else
CSP	endif

	DO 20 j=1,nok

        IF(id.ne.0) THEN
	IF(idd(j).ne.id2) GOTO 20
        ENDIF

        trkj(1)=trak(j).r
        trkj(2)=trak(j).rf
        trkj(3)=trak(j).z
        trkj(4)=trak(j).f
        trkj(5)=trak(j).tanl
        trkj(6)=trak(j).qR

*second track is a negative
        IF(trkj(6).gt.0) GOTO 20


		call ev0_circle_param(trki,xc1,r1)
		call ev0_circle_param(trkj,xc2,r2)

	 	CALL VZERO(xcom,2)
	 	CALL VZERO(ycom,2)
		iflag1=0
		call ev0_vzero_geom(rd_cut,xc1,xc2,r1,r2,xcom,ycom,iflag1)
		IF(iflag1.eq.5) GOTO 20

C----two possible intersection points ---------------------------

			DO jk=1,2
		xp(1)=xcom(jk)
		xp(2)=ycom(jk)
111		call ev0_project_track(xc1,r1,xp,xn1)
		call ev0_update_track_param(xc1,r1,xn1,trki,trk1)
		call ev0_project_track(xc2,r2,xp,xn2)
		call ev0_update_track_param(xc2,r2,xn2,trkj,trk2)
		rd=sqrt( (xn1(1)-xn2(1))**2 + (xn1(2)-xn2(2))**2
     +			+(trk1(3)-trk2(3))**2 )

		xpp(1)=(xn1(1)+xn2(1))/2.
		xpp(2)=(xn1(2)+xn2(2))/2.
		xpp(3)=(trk1(3)+trk2(3))/2.
		rv=sqrt(xpp(1)**2+xpp(2)**2+xpp(3)**2)
cs		true=sqrt( (vert(i,1)-vert(j,1))**2
cs     ++(vert(i,2)-vert(j,2))**2+(vert(i,3)-vert(j,3))**2 )
*
*---decide here if it is a good candidate
*
		IF( (rd.le.rd_cut).and.(rv.gt.rv_cut).and.(rv.lt.30.) ) THEN
		massl(1)=0.13
		massl(2)=0.13
		call ev0_inv_mass(trk1,trk2,massl,pp,inv_mass,0)
* be careful. Impact parameter assumes main vertex at (0,0,0)
		call ev0_v0_impact_param(mainv,xpp,pp,rmin,iflag)
                IF((rmin.le.rmin_cut).and.(iflag.ne.2)) THEN

                ok=ok+1
                ev0out_h.nok= ev0out_h.nok+1
                ev0out(ok).p(1)=pp(1)
                ev0out(ok).p(2)=pp(2)
                ev0out(ok).p(3)=pp(3)
                ev0out(ok).x(1)=xpp(1)
                ev0out(ok).x(2)=xpp(2)
                ev0out(ok).x(3)=xpp(3)
                ev0out(ok).id=ok
                ev0out(ok).dcapn=rd
                ev0out(ok).dcav0=rmin
                ev0out(ok).idpos=trak(i).id
                ev0out(ok).idneg=trak(j).id
C--calculate Armenteros variables---
        	call ev0_track_mom(trk1,0,pst1)
        	call ev0_track_mom(trk2,0,pst2)
                pdotp=pst1(1)*pp(1)+pst1(2)*pp(2)+pst1(3)*pp(3)   !dot product of p+ and p(v0)
                ndotp=pst2(1)*pp(1)+pst2(2)*pp(2)+pst2(3)*pp(3)   !dot product of p- and p(v0)
                pv0  =sqrt(pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3)) 
                ppar=pdotp/pv0
                npar=ndotp/pv0
                pper=sqrt(pst1(1)*pst1(1)+pst1(2)*pst1(2)+pst1(3)*pst1(3)-ppar*ppar)
                ev0out(ok).pt=pper
                ev0out(ok).alpha=(ppar-npar)/(ppar+npar)
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
		call ev0_inv_mass(trk1,trk2,massl,pp,inv_mass,0)

                ev0out(ok).inv_mass(ll)=inv_mass

C--calculate proper lifetime assuming that particle originated at main vertex
                ev0out(ok).tau(ll)=inv_mass*rv/pv0      
                ev0out(ok).tmin(ll)=inv_mass*rv_cut/pv0                
c--calculate cos(theta_c.m.)
                env0 = sqrt(pv0*pv0+inv_mass*inv_mass)
                bv0  =pv0/env0
                gv0  =env0/inv_mass
                ep   =sqrt(pst1(1)*pst1(1)+pst1(2)*pst1(2)+
     +                     pst1(3)*pst1(3)+massl(1)*massl(1))
                ppar_cm=gv0*(ppar-bv0*ep)
                ev0out(ok).cos_th_cm(ll)=cos(atan2(pper,ppar_cm))
c
c	WRITE(7) rd,rv,rmin,true,inv_mass,itr(i),itr(j)
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


