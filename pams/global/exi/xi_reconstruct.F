C>---------------------------------------------------------------------
      INTEGER FUNCTION XI_RECONSTRUCT(nok,idd,trak,
     +                                exipar_h,exipar,
     +                                privert_h,privert,
     +                                ev0out_h,ev0out,
     +                                exiout_h,exiout   )
c--------------------------------------------------------------------- 
C   Functional Description : Search for secondaries 
C                            X and Omega particles and
C			     and anti-particles are 
C			     considered. Only charged decay modes.
C   Created  MARCH-1994         S. Margetis 
C   Modified by                 WK.Wilson
C   Moved to STAF JUNE-1997     S. Margetis
C   Error conditions	: None-Yet
C   Status		: Tested 
C<----------------------------------------------------------------------
      IMPLICIT NONE
#include "PAM.inc"
#include "exi_exipar.inc"
#include "evr_privert.inc"
#include "ev0_ev0out.inc"
#include "exi_exiout.inc"
C----------------------------------------------------------------------
      RECORD/ TABLE_HEAD_ST/ exipar_h
      RECORD/ EXI_EXIPAR_ST/ exipar(*)
      RECORD/ TABLE_HEAD_ST/ privert_h
      RECORD/ EVR_PRIVERT_ST/ privert(*)
      RECORD/ TABLE_HEAD_ST/ ev0out_h
      RECORD/ EV0_EV0OUT_ST/ ev0out(*)
      RECORD/ TABLE_HEAD_ST/ exiout_h
      RECORD/ EXI_EXIOUT_ST/ exiout(*)

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
      INTEGER i,j,k,l
 
      REAL	rd_cut,rv_cut,rmin_cut,dmass,mass_id(4)
      REAL      mass_error

      INTEGER 	maxtr,matchl,npid1,npid2,ok
      PARAMETER	(maxtr=5000)
      INTEGER 	ev0_flag(maxtr),pid(maxtr)
      INTEGER  	ntracks,ntracx,jk,itrc,vidc,itr(maxtr),vid(maxtr)
      INTEGER	jump(maxtr),ev0_hits,index
      REAL	dr,mass1(4),mass2(4),charge(maxtr),pi,ph,pst1(3),pst2(3)
      REAL	vert(maxtr,3),pvert(maxtr,3),true,rv,xv0(3)
      REAL	qrg(maxtr),phig(maxtr),tanlg(maxtr),zcom(2)
      REAL      x0(maxtr),y0(maxtr),z0(maxtr),dx,pxi
      REAL	ptrk,trk1(6),trk2(6),pdotp,ndotp,pv0(3),ppar,npar,pper
      REAL	trki(6),trkj(6),qr(maxtr),phi(maxtr),tanl(maxtr)
      REAL	xc1(2),xc2(2),r1,r2,xcom(2),ycom(2),invms,rmin
      REAL	xp(2),xn1(2),xn2(2),rd,xpp(3),massl(2),pp(3)
      REAL      env0,bv0,gv0,ep,ppar_cm,mainv(3)
      real      p1,p2,dot,open ! wkw
      LOGICAL	tpc
      COMMON	/local/	ptrk(maxtr,6)
      REAL MASST(50),QQT(50)
*
      DATA MASST/7*0.0005,.139567,.139567
     +,            3*0.493667,2*.93828,.93828
     +,            35*0./
      DATA MASS1/0.139567,.139567,0.4936,0.4936/
      DATA MASS2/4*1.11563/
      DATA QQT/0.,1.,-1.,0.,1.,-1.,0. ,1.,-1.,0.,1.,-1.,0.
     +,        1.,-1.,35*0./
*
*
*-----executable statements--------------------------------------------
*
      pi=acos(-1.)
*
      id=exipar(1).id
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
      mass_error=exipar(1).dmass ! wkw cut for lambda mass
*
c       mainv(1) = privert(1).x
c       mainv(2) = privert(1).y
c       mainv(3) = privert(1).z
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
c test for xi...must be a negative
        IF(trki(6).ge.0.0) GOTO 10

	DO 20 j=1,ev0out_h.nok

c*** pgj: moved back wkw's mass check to start of loop
          IF( abs(ev0out(j).inv_mass(2)-1.11563).gt.mass_error ) GOTO 20

c*** pgj: don't use tracks already used in the v0
          IF( ev0out(j).idneg .eq. trak(i).id .or.
     >        ev0out(j).idpos .eq. trak(i).id ) GOTO 20

          IF(id.ne.0) THEN
	    IF( ev0out(j).inv_mass(index) .eq. 0.0 ) GOTO 20
c	     IF( abs(ev0out(j).inv_mass(index)-1.11563) ! put elsewhere by wkw
c     +          .gt.0.020 ) GOTO 20
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

          CALL circle_param(trki,xc1,r1)

          CALL VZERO(xcom,2)
          CALL VZERO(ycom,2)
          iflag1=0
          CALL casc_geom(rd_cut,pv0,xv0,xc1,r1,xcom,ycom,zcom,iflag1)
          IF(iflag1.eq.5) GOTO 20

C----two possible intersection points ---------------------------

          DO jk=1,2
            xp(1)=xcom(jk)
            xp(2)=ycom(jk)
111            CALL exi_project_track(xc1,r1,xp,xn1)
            CALL update_track_param(xc1,r1,xn1,trki,trk1)
            rd=sqrt( (xn1(1)-xp(1))**2 + (xn1(2)-xp(2))**2 +
     >               (trk1(3)-zcom(jk))**2 )

            xpp(1)=(xn1(1)+xp(1))/2.
            xpp(2)=(xn1(2)+xp(2))/2.
            xpp(3)=(trk1(3)+zcom(jk))/2.
            rv=sqrt(xpp(1)**2+xpp(2)**2+xpp(3)**2)
*
*---decide here if it is a good candidate (pgj: removed lambda mass check)
*
            IF( (rd.le.rd_cut).and.(rv.gt.rv_cut).and.(rv.lt.30.) ) THEN 
              massl(1)=0.139
              massl(2)=1.11563
              CALL inv_mass(trk1,trkj,massl,pp,invms,8)
* be careful. Impact parameter needs main vertex at (0,0,0)
              CALL casc_impact_param(mainv,xpp,pp,rmin,iflag)
              IF((rmin.le.rmin_cut).and.(iflag.ne.2)) THEN

c               wkw: breakout if ok=maxlen
                if (ok.eq.exiout_h.maxlen) then
                  return
                endif

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
                exiout(ok).dcaxi=rmin
                exiout(ok).idba=trak(i).id
                exiout(ok).idv0=ev0out(j).id
                exiout(ok).inv_mass(1)=invms
                exiout(ok).tau(1)=invms*rv/pxi     
                exiout(ok).tmin(1)=invms*rv_cut/pxi
C--calculate Armenteros variables---
        	call track_mom(trk1,0,pst1)
        	call track_mom(trk2,0,pst2)
                pdotp=pst1(1)*pp(1)+pst1(2)*pp(2)+pst1(3)*pp(3)   !dot product of p+ and p(Xi)
                ndotp=pv0(1)*pp(1)+pv0(2)*pp(2)+pv0(3)*pp(3)     !dot product of v0 and p(Xi)
                pxi  =sqrt(pp(1)*pp(1)+pp(2)*pp(2)+pp(3)*pp(3)) 
                ppar=pdotp/pxi
                npar=ndotp/pxi
                pper=sqrt(pst1(1)*pst1(1)+pst1(2)*pst1(2)+pst1(3)*pst1(3)-ppar*ppar)
                exiout(ok).pt=pper
                exiout(ok).alpha=(ppar-npar)/(ppar+npar)
C--calculate impact parameters--- wkw
                xp(1)=0.
                xp(2)=0.
c               negative track
                CALL exi_project_track(xc1,r1,xp,xn1)
c		 CALL ev0_update_track_param(xc1,r1,xn1,trki,trk1)
                exiout(ok).chi2=sqrt(xn1(1)**2.+xn1(2)**2.)

c     put some ev0 info in exiout cov and unused variables (wkw)
                exiout(ok).cov(1)=ev0out(j).dcapn
                exiout(ok).cov(2)=ev0out(j).dcav0
                exiout(ok).cov(3)=ev0out(j).tau(1)/ev0out(j).tmin(1)
                exiout(ok).cov(4)=ev0out(j).inv_mass(2)
                exiout(ok).cov(5)=ev0out(j).alpha
                exiout(ok).cov(6)=ev0out(j).pt
                exiout(ok).inv_mass(2)=ev0out(j).cos_th_cm(1)
                exiout(ok).inv_mass(3)=ev0out(j).dcap
                exiout(ok).inv_mass(4)=ev0out(j).dcan

*
*---if blind run test all mass hypothesis
*
                IF(rmin.le.3000.) GOTO 41       !go out here for now
                DO ll=1,4
c                  IF(id.eq.0) THEN
                  massl(1)=mass1(ll)
                  massl(2)=mass2(ll)
c                  ELSE
c		     massl(1)=masst(id1)
c		     massl(2)=masst(id2)
c                  ENDIF
                  CALL inv_mass(trk1,trk2,massl,pp,invms,8)
                  exiout(ok).inv_mass(ll)=invms

C--calculate proper lifetime assuming that particle originated at main vertex
                  exiout(ok).tau(ll)=invms*rv/pxi     
                  exiout(ok).tmin(ll)=invms*rv_cut/pxi

C--calculate cos(theta_c.m.)
c                  env0 = sqrt(pv0*pv0+invms*invms)
c                  bv0  =pv0/env0
c                  gv0  =env0/invms
c                  ep   =sqrt(pst1(1)*pst1(1)+pst1(2)*pst1(2)+
c     +                       pst1(3)*pst1(3)+massl(1)*massl(1))
c                  ppar_cm=gv0*(ppar-bv0*ep)
c                  exiout(ok).cos_th_cm(ll)=cos(atan2(pper,ppar_cm))
c
                   IF(id.ne.0) GOTO 30		!blind or not?
                ENDDO

41              GOTO 30				!next j
              ENDIF				!rmin and iflag check

            ENDIF				!rd.and.rv check

            IF(iflag1.eq.3) GOTO 30		!touching circles-loop once

          ENDDO					!jk=1,2

 30	  CONTINUE

 20     CONTINUE

 10   CONTINUE	


999   XI_RECONSTRUCT = 0
      RETURN
      END


