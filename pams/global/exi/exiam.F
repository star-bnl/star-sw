C>----------------------------------------------------------------------
      INTEGER*4 FUNCTION EXIAM( exipar_h,    exipar,
     +                          globtrk_h,   globtrk,
     +                          vertex_h,    vertex,
     +                          v0_vertex_h, v0_vertex,
     +                          xi_vertex_h, xi_vertex,
     +                          exiaux_h,    exiaux )
C-----------------------------------------------------------------------
C   Functional Description : Search for (Anti-)Xi and (Anti-)Omega decay
C                            vertices. Only charged decay modes.
C
C   Input arguments:
C     exipar_h:     control structure header
C     exipar:       control structure
C     globtrk_h:    dst track structure header
C     globtrk:      dst track structure 
C     vertex_h:     vertex information header
C     vertex:       vertex information
C     v0_vertex_h:  auxiliary v0 output structure header
C     v0_vertex:    auxiliary v0 output structure 
C     xi_vertex_h:  auxiliary xi output structure header
C     xi_vertex:    auxiliary xi output structure 
C     exiaux_h:     additional xi output structure header
C     exiaux:       additional xi output structure 
C
C   Output arguments : 
C     values in track, vertex and exiaux tables
C     iok = user status code
C     
C   Created  MARCH-1994         S. Margetis 
C   Modified by                 WK.Wilson
C   Moved to STAF JUNE-1997     S. Margetis
C   26-Aug-1998 PGJ Clean-up and modified to use dst tables.
C   10-Feb-1999 PGJ Made ready for MDC2. 
C
C   Error conditions	: None-Yet
C   Status		: Tested 
C<----------------------------------------------------------------------
      IMPLICIT NONE

#include "exiam.inc"
#include "math_constants.inc"
#include "phys_constants.inc"

#define MXDET 3
#define TPC_ONLY 1
#define SVT_ONLY 2
#define TPC_PLUS_SVT 3

      STRUCTURE/local_track/
        INTEGER id
        REAL*4 r
        REAL*4 rf
        REAL*4 z
        REAL*4 f
        REAL*4 tanl
        REAL*4 qR
      END STRUCTURE
 
      INTEGER   id, id1, id2
      INTEGER   ierr, iflag, iflag1
      INTEGER   i, j, k, jk
      INTEGER   id_vertex, id_v0
      INTEGER   n_v, n_xi
      INTEGER   idet_v0, idet_b, id_det
 
      REAL	dca_max(MXDET), rv_xi(MXDET), rv_v0(MXDET)
      REAL      bxi_max(MXDET), dmass(MXDET), bpn_v0(MXDET)
      REAL	dr, pb(3), rv, xv0(3), zcom(2)
      REAL      dx,ptot, ptot_2, ptot_b, ptot_b2, ptot_v0, ptot_v02
      REAL      epi, ek, ela, exi, eom
      REAL	ptrk, trkb(6), bdotx, vdotx, pv0(3), ppar, npar, pper
      REAL	trki(6), trkj(6)
      REAL	xc1(2), r1, r2, xcom(2), ycom(2), bxi, bb, bv0
      REAL      invs_xi, invms_om, mala, malb, v0_mass
      REAL	xp(2), xn1(2), xn2(2), dca, xpp(3), pxi(3)
      REAL      energy_v0, beta_v0, gamma_v0, ep, ppar_cm, vmain(3)
      REAL      r0, phi0, charge
      REAL      xlocal(3), bfield(3), bscale
      REAL      ptotp2, ptotn2, pdotn, eprp, eprn, epip, epin

      LOGICAL   first

      DATA      xlocal /3*0./
C-----------------------------------------------------------------------
c*** pgj: copy parameters

      first = .true.

      do i=1,MXDET
        dca_max(i) = exipar(i).dca_max
        rv_xi(i)   = exipar(i).rv_xi
        rv_v0(i)   = exipar(i).rv_v0
        bxi_max(i) = exipar(i).bxi_max
        dmass(i)   = exipar(i).dmass     ! wkw cut for lambda mass
        bpn_v0(i)  = exipar(i).bpn_v0
      enddo

c*** pgj: get magnetic field via call to gufld

      call gufld(xlocal,bfield)
      write(6,*) 'EXI - using a field of ',bfield(3),' KGauss'

c*** pgj: current row counts

      n_v = vertex_h.nok
      n_xi = xi_vertex_h.nok

c      exiaux_h.nok = 0

c*** pgj: loop to find main vertex and maximum key value

      vmain(1) = 0.
      vmain(2) = 0.
      vmain(3) = 0.

      id_vertex = 0

      if( vertex_h.nok .gt. 0 ) then
        do i= 1, vertex_h.nok
          if( vertex(i).vtx_id .eq. 1 ) then
            vmain(1) = vertex(i).x
            vmain(2) = vertex(i).y
            vmain(3) = vertex(i).z
c          else if( vertex(i).vtx_id .eq. 2 ) then
          endif
          id_vertex = max(id_vertex,vertex(i).id)
        enddo
      endif

c*** pgj: loop over all v0 vertices

      DO 10 j = 1, v0_vertex_h.nok

        id_v0 = v0_vertex(j).id_vertex

c*** pgj: fast lookup for locating vertex entry

        k = id_v0
        if( vertex(k).id .eq. id_v0 ) GO TO 101
        write(6,*) 'EXI - Warning: fast lookup for vertex entry failed'

c*** pgj: slow failsafe method for locating vertex index

        do k = 1, vertex_h.nok
          if( vertex(k).id .eq. id_v0 ) GO TO 101
        enddo

101     CONTINUE
        if( k .le. vertex_h.nok ) then
          xv0(1) =vertex(k).x
          xv0(2) =vertex(k).y
          xv0(3) =vertex(k).z
          rv = sqrt( (xv0(1)-vmain(1))**2 + 
     >               (xv0(2)-vmain(2))**2 + 
     >               (xv0(3)-vmain(3))**2 )
        else
          write(6,*) 'EXI - Error: vertex entry not found'
          return
        endif

c*** pgj: check detector id

        if( vertex(k).det_id.eq.33 ) then
          idet_v0 = TPC_PLUS_SVT
        elseif( vertex(k).det_id.eq.11 .or. 
     >          vertex(k).det_id.eq.31 .or.
     >          vertex(k).det_id.eq.13 ) then
          idet_v0 = TPC_ONLY
        elseif( vertex(k).det_id.eq.22 .or.
     >          vertex(k).det_id.eq.12 .or.
     >          vertex(k).det_id.eq.21 .or.
     >          vertex(k).det_id.eq.32 .or.
     >          vertex(k).det_id.eq.23 ) then
          idet_v0 = SVT_ONLY
        else
          GO TO 10
        endif

c*** pgj: apply M_LAMBDA mass check

        ptotp2 = ( v0_vertex(j).pos_px*v0_vertex(j).pos_px +
     >             v0_vertex(j).pos_py*v0_vertex(j).pos_py +
     >             v0_vertex(j).pos_pz*v0_vertex(j).pos_pz )
        ptotn2 = ( v0_vertex(j).neg_px*v0_vertex(j).neg_px +
     >             v0_vertex(j).neg_py*v0_vertex(j).neg_py +
     >             v0_vertex(j).neg_pz*v0_vertex(j).neg_pz )
        pdotn  = ( v0_vertex(j).pos_px*v0_vertex(j).neg_px +
     >             v0_vertex(j).pos_py*v0_vertex(j).neg_py +
     >             v0_vertex(j).pos_pz*v0_vertex(j).neg_pz )

        epip = sqrt(M_PION_PLUS*M_PION_PLUS + ptotp2)
        eprn = sqrt(M_ANTIPROTON*M_ANTIPROTON + ptotn2)
        malb = sqrt(M_PION_PLUS*M_PION_PLUS + 
     >              M_ANTIPROTON*M_ANTIPROTON + 2*(epip*eprn - pdotn))

        eprp = sqrt(M_PROTON*M_PROTON + ptotp2)
        epin = sqrt(M_PION_MINUS*M_PION_MINUS + ptotn2)
        mala = sqrt(M_PROTON*M_PROTON + M_PION_MINUS*M_PION_MINUS +
     >              2*(eprp*epin - pdotn))

c*** pgj: also apply cuts on v0 pion daughter impact param. and decay dist.

        if( abs(mala-M_LAMBDA).le.dmass(idet_v0) ) then
          v0_mass = mala
          charge = -1.
          if( abs(v0_vertex(j).dcan).lt.bpn_v0(idet_v0) .or.
     >        rv.lt.rv_v0(idet_v0) ) GO TO 10
        elseif( abs(malb-M_LAMBDA).le.dmass(idet_v0) ) then
          v0_mass = malb
          charge = +1.
          if( abs(v0_vertex(j).dcap).lt.bpn_v0(idet_v0) .or.
     >        rv.lt.rv_v0(idet_v0) ) GO TO 10
        else
          GO TO 10
        endif

c*** pgj: loop over all global tracks

        DO 20 i = 1, globtrk_h.nok

c*** pgj: select correct charge tracks

          if( globtrk(i).icharge*int(charge).le.0 ) GO TO 20

c*** pgj: don't use tracks already used in the v0

          IF( v0_vertex(j).idneg .eq. globtrk(i).id .or.
     >        v0_vertex(j).idpos .eq. globtrk(i).id ) GO TO 20

          if( globtrk(i).det_id .eq. 1 ) then
            idet_b = TPC_ONLY
          elseif( globtrk(i).det_id .eq. 2 ) then
            idet_b = SVT_ONLY
          elseif( globtrk(i).det_id .eq. 3 ) then
            idet_b = TPC_PLUS_SVT
          else
            GO TO 20
          endif         

          id_det = min(idet_b,idet_v0)

c*** pgj: convert to local parameters

          r0 = sqrt(globtrk(i).x0**2 + globtrk(i).y0**2)
          phi0 = atan2(globtrk(i).y0,globtrk(i).x0)

          trki(1) = r0
          trki(2) = r0*phi0
          trki(3) = globtrk(i).z0
          trki(4) = globtrk(i).psi*C_RAD_PER_DEG
          trki(5) = globtrk(i).tanl
          trki(6) = charge*C_D_CURVATURE*bfield(3)*globtrk(i).invpt

c*** pgj: calculate distance(s) of closest approach

          trkj(1) = v0_vertex(j).pos_px + v0_vertex(j).neg_px
          trkj(2) = v0_vertex(j).pos_py + v0_vertex(j).neg_py
          trkj(3) = v0_vertex(j).pos_pz + v0_vertex(j).neg_pz
          trkj(4) = 0.
          trkj(5) = 0.
          trkj(6) = 0.

          pv0(1) = trkj(1)
          pv0(2) = trkj(2)
          pv0(3) = trkj(3)

          CALL circle_param(trki,xc1,r1,bfield(3))

          CALL vzero(xcom,2)
          CALL vzero(ycom,2)

          iflag1=0
          CALL casc_geom(dca_max(id_det),pv0,xv0,xc1,r1,xcom,ycom,zcom,iflag1)
          IF( iflag1.eq.5 ) GO TO 20

c*** loop over the two intersection points

          DO jk = 1,2

            xp(1)=xcom(jk)
            xp(2)=ycom(jk)

            CALL exi_project_track(xc1,r1,xp,xn1)
            CALL update_track_param(xc1,r1,xn1,trki,trkb)

            dca=sqrt( (xn1(1)-xp(1))**2 + (xn1(2)-xp(2))**2 +
     >                (trkb(3)-zcom(jk))**2 )

            xpp(1)=(xn1(1)+xp(1))/2.
            xpp(2)=(xn1(2)+xp(2))/2.
            xpp(3)=(trkb(3)+zcom(jk))/2

C*** Problem with vmain not being a 0,0,0 again fixed HLC 3/3/98.
            rv=sqrt((xpp(1)-vmain(1))**2+(xpp(2)-vmain(2))**2+
     >           (xpp(3)-vmain(3))**2)

c*** decide here if it is a good candidate (pgj: removed lambda mass check)

            IF( (dca .le. dca_max(id_det)) .and. 
     >          (rv .gt. rv_xi(id_det))    ) THEN

c*** calculate xi impact parameter - careful, needs main vertex at (0,0,0)

              CALL track_mom(trkb,0,pb,bfield(3))

              pxi(1) = pv0(1) + pb(1)
              pxi(2) = pv0(2) + pb(2)
              pxi(3) = pv0(3) + pb(3)

              CALL casc_impact_param(vmain,xpp,pxi,bxi,iflag,charge,bfield(3))

              IF( (bxi.le.bxi_max(id_det)) .and. (iflag.ne.2) ) THEN

c*** wkw: breakout if n_xi=maxlen

                if( vertex_h.nok .ge. vertex_h.maxlen ) then
                  write(6,*) 'EXI - Error: vertex table overflow'
                  return
                else
                  n_v = vertex_h.nok + 1
                endif

                if( xi_vertex_h.nok .ge. xi_vertex_h.maxlen ) then
                  write(6,*) 'EXI - Error: exi_dst_vertex table overflow'
                  return
                else
                  n_xi = xi_vertex_h.nok + 1
                endif

                vertex_h.nok = n_v
                xi_vertex_h.nok = n_xi

c*** pgj: fill vertex table

                vertex(n_v).id = id_vertex + n_xi
                vertex(n_v).det_id = 100*idet_b+vertex(k).det_id
                vertex(n_v).vtx_id = 3
                vertex(n_v).x = xpp(1)
                vertex(n_v).y = xpp(2)
                vertex(n_v).z = xpp(3)
                vertex(n_v).n_daughters = 2
                vertex(n_v).id_aux_ent = n_xi

c*** calculate parent and daughter kinematics

                ptot_b2 = pb(1)**2 + pb(2)**2 + pb(3)**2
                epi = sqrt(ptot_b2 + M_PION_MINUS**2)
                ek  = sqrt(ptot_b2 + M_KAON_MINUS**2)

                ptot_v02 = pv0(1)**2 + pv0(2)**2 + pv0(3)**2
                ela = sqrt(ptot_v02 + M_LAMBDA**2)

                ptot_2 = pxi(1)**2 + pxi(2)**2 + pxi(3)**2
                ptot   = sqrt(ptot_2)
                exi = sqrt( ptot_2 + M_XI_MINUS**2 )
                eom = sqrt( ptot_2 + M_OMEGA_MINUS**2 )

c*** calculate Armenteros variables

                bdotx   = pb(1)*pxi(1)   + pb(2)*pxi(2)   + pb(3)*pxi(3)
                vdotx   = pv0(1)*pxi(1)  + pv0(2)*pxi(2)  + pv0(3)*pxi(3)

                if( globtrk(i).icharge .gt. 0 ) then
                  ppar = bdotx/ptot
                  npar = vdotx/ptot
                  pper = sqrt(ptot_b2 - ppar*ppar)
                else
                  ppar = vdotx/ptot
                  npar = bdotx/ptot
                  pper = sqrt(ptot_v02 - ppar*ppar)
                endif

c*** calculate daughter impact parameters (wkw)

                CALL exi_project_track(xc1,r1,vmain,xn1)
                CALL update_track_param(xc1,r1,xn1,trki,trkb)

                bb = sqrt( (xn1(1)-vmain(1))**2 + 
     >                     (xn1(2)-vmain(2))**2 +
     >                     (trkb(3)-vmain(3))**2 )

                CALL ev0_v0_impact_param(vmain,xpp,pxi,ptot_2,bv0,iflag)
                if( iflag.eq.2 ) bv0 = -999.

c*** calculate cos(theta_c.m.)

c                energy_v0 = sqrt(pv0*pv0+invms*invms)
c                beta_v0   = pv0/energy_v0
c                gamma_v0  = energy_v0/invms
c                ep   = sqrt(pb(1)*pb(1) + pb(2)*pb(2)+
c     +                      pb(3)*pb(3) + massl(1)*massl(1))
c                ppar_cm = gv0*(ppar-bv0*ep)
c                costh = cos(atan2(pper,ppar_cm))

c*** pgj: fill auxiliary table

                xi_vertex(n_xi).id    = n_xi
                xi_vertex(n_xi).id_xi = id_vertex + n_xi
                xi_vertex(n_xi).id_v0 = id_v0 
                xi_vertex(n_xi).id_b  = globtrk(i).id
                xi_vertex(n_xi).dca   = dca
                xi_vertex(n_xi).b_xi  = bxi
                xi_vertex(n_xi).b_b   = bb
                xi_vertex(n_xi).b_v0  = bv0
                xi_vertex(n_xi).px_b  = pb(1)
                xi_vertex(n_xi).py_b  = pb(2)
                xi_vertex(n_xi).pz_b  = pb(3)

c*** pgj: fill exiaux table if present (note: default allocation is 1)

                if( exiaux_h.maxlen .eq. 1 ) then
                  if( first ) then
                    write(6,*) 
     >                'EXI - Warning: exi_aux table will not be filled'
                    first = .false.
                  endif
                elseif( n_xi .le. exiaux_h.maxlen ) then
                  exiaux_h.nok = n_xi
                  exiaux(n_xi).id      = n_xi
                  exiaux(n_xi).id_xi   = id_vertex + n_xi
                  exiaux(n_xi).id_b    = globtrk(i).id
                  exiaux(n_xi).id_v0   = id_v0
                  exiaux(n_xi).det_id  = 100*idet_b+vertex(k).det_id
c                  exiaux(n_xi).gpid_b  =
c                  exiaux(n_xi).ndegf   =
                  exiaux(n_xi).charge  = globtrk(i).icharge
                  exiaux(n_xi).dca     = dca
                  exiaux(n_xi).rv      = rv
                  exiaux(n_xi).b_xi    = bxi
                  exiaux(n_xi).b_b     = bb
                  exiaux(n_xi).b_v0    = bv0
c                  exiaux(n_xi).sag_b   =
                  exiaux(n_xi).px      = pxi(1)
                  exiaux(n_xi).py      = pxi(2)
                  exiaux(n_xi).pz      = pxi(3)
                  exiaux(n_xi).px_b    = pb(1)
                  exiaux(n_xi).py_b    = pb(2)
                  exiaux(n_xi).pz_b    = pb(3)
                  exiaux(n_xi).px_v    = pv0(1)
                  exiaux(n_xi).py_v    = pv0(2)
                  exiaux(n_xi).pz_v    = pv0(3)
                  exiaux(n_xi).ptarm   = pper
                  exiaux(n_xi).alpha   = (ppar-npar)/(ppar+npar)
                  exiaux(n_xi).mass_xi = sqrt((ela+epi)**2 - ptot_2)
                  exiaux(n_xi).mass_om = sqrt((ela+ek)**2  - ptot_2)
                  exiaux(n_xi).mass_v0 = v0_mass
                  exiaux(n_xi).tau_xi  = exiaux(n_xi).mass_xi*rv/ptot     
                  exiaux(n_xi).tau_om  = exiaux(n_xi).mass_om*rv/ptot 
                  exiaux(n_xi).rap_xi  = 0.5*log((exi+pxi(3))/(exi-pxi(3)))
                  exiaux(n_xi).rap_om  = 0.5*log((eom+pxi(3))/(eom-pxi(3)))
c                  exiaux(n_xi).costh   =
                else
                  if( first ) then
                    write(6,*) 'EXI - Warning: exi_aux table overflow'
                    first = .false.
                  endif
                endif

              ENDIF				! bxi and iflag check

            ENDIF				! dca and rv check

            IF( iflag1.eq.3 ) GO TO 30		! touching circles-loop once

          ENDDO					! jk=1,2

 30	  CONTINUE

 20     CONTINUE

 10   CONTINUE	

      write(6,*) 'EXI - found ',n_xi,' candidates'

c*** return a normal success code

      exiam = STAFCV_OK

      RETURN
      END

