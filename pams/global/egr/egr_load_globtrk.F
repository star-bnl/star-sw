
C>--------------------------------------------------------------------
C     EGR_FILL_GLOBTRK - fills the global track table
C<--------------------------------------------------------------------
       INTEGER FUNCTION EGR_LOAD_GLOBTRK(globtrk_h,globtrk,globtrk_aux_h,
     +     globtrk_aux,igtrk,gtrk,tphit_h,tphit,svthit_h,svthit,
     +     covar,itpct,isvtt,ipriv)
       
C>--------------------------------------------------------------------
C
C     Input arguments
C     globtrk_h    - header for the globtrk table
C     globtrk     - rows of the globtrk table
C     globtrk_aux_h    - header for the auxiallry globtrk table
C     globtrk_aux     - rows of the auxillary globtrk table
C     igtrk    - current fitting array id
C     gtrk     - the refitting array
C     tphit_h     - header for the tphit (tpc hits) table
C     hit      - rows of the tphit table
C     covar    - the covariance matrix

C     Functional description:
C     Fills the global track array for different situations
C<--------------------------------------------------------------------
      IMPLICIT NONE
#include "PAM.inc"
#include "dst_track.inc"
#include "dst_track_aux.inc"
#include "tcl_tphit.inc"
#include "scs_spt.inc"
#include "egr_track_pointers.inc"
#include "math_constants.inc"
#include "phys_constants.inc"
C______________________________________________________________________
 
      RECORD /table_head_st/     globtrk_h
      RECORD /dst_track_st/      globtrk(*)
      RECORD /table_head_st/     globtrk_aux_h
      RECORD /dst_track_aux_st/  globtrk_aux(*)
      RECORD /table_head_st/     tphit_h
      RECORD /tcl_tphit_st/      tphit(*)
      RECORD /table_head_st/     svthit_h
      RECORD /scs_spt_st/      svthit(*)
      
C_____________________________________________________________________

C     Local variables

      RECORD /track_pointers/ gtrk(mxtrack)
      
      INTEGER igtrk,sgn,lasthit,firsthit,iglob,itpct,isvtt
      INTEGER imsg,ipriv
      REAL    covar(3,3)
      DATA    imsg /0/

 
      globtrk_h.nok = globtrk_h.nok + 1
      globtrk_aux_h.nok = globtrk_aux_h.nok +1
      iglob = globtrk_h.nok
      globtrk(iglob).id = iglob
      globtrk(iglob).n_point = gtrk(igtrk).nhit
      globtrk(iglob).n_max_point = 0
      globtrk(iglob).n_fit_point = gtrk(igtrk).nfit
      globtrk(iglob).icharge = sign(1.0,gtrk(igtrk).p(2))
      globtrk(iglob).x0 = abs(gtrk(igtrk).p(2))*cos(gtrk(igtrk).p(5))+
     +     gtrk(igtrk).p(3)
      globtrk(iglob).y0 = abs(gtrk(igtrk).p(2))*sin(gtrk(igtrk).p(5))+
     +     gtrk(igtrk).p(4)
      globtrk(iglob).z0 = gtrk(igtrk).p(6)
      sgn = sign(1.0,gtrk(igtrk).p(2))
      globtrk(iglob).psi = gtrk(igtrk).p(5)-sgn*C_PI_2
      globtrk(iglob).psi = globtrk(iglob).psi*C_DEG_PER_RAD  ! to degrees
      if( globtrk(iglob).psi.gt.180.0) then
         globtrk(iglob).psi=  globtrk(iglob).psi-360.0
      elseif( globtrk(iglob).psi .lt. -180.0) then
         globtrk(iglob).psi = globtrk(iglob).psi + 360.0
      endif
      globtrk(iglob).tanl = gtrk(igtrk).p(7)
      globtrk(iglob).invpt =1.0/(C_D_CURVATURE*0.5*10.*
     +     (abs(gtrk(igtrk).p(2))))
      globtrk(iglob).covar_diag(1) = covar(1,1)
      globtrk(iglob).covar_diag(2) = covar(2,2)
      globtrk(iglob).covar_diag(3) = covar(3,3)
      globtrk(iglob).covar_diag(4) = 0
      globtrk(iglob).covar_diag(5) = 0
      globtrk(iglob).chisq(1) = gtrk(igtrk).p(8)
      globtrk(iglob).chisq(2) = gtrk(igtrk).p(9)
      if( isvtt .eq. 0) then
         firsthit = gtrk(igtrk).ipnt(gtrk(igtrk).ntpc)
         globtrk(iglob).x_first(1) = tphit(firsthit).x
         globtrk(iglob).x_first(2) = tphit(firsthit).y
         globtrk(iglob).x_first(3) = tphit(firsthit).z
      else
         firsthit = gtrk(igtrk).ipnt(gtrk(igtrk).ntpc+1)
         globtrk(iglob).x_first(1) = svthit(firsthit).x(1)
         globtrk(iglob).x_first(2) = svthit(firsthit).x(2)
         globtrk(iglob).x_first(3) = svthit(firsthit).x(3)

      endif
C     Now find last point on track and calculate track length.
      if (itpct.ne.0) then
         lasthit = gtrk(igtrk).ipnt(1)
         globtrk(iglob).x_last(1) = tphit(lasthit).x
         globtrk(iglob).x_last(2) = tphit(lasthit).y
         globtrk(iglob).x_last(3) = tphit(lasthit).z
         globtrk(iglob).length = 
     +        sqrt((globtrk(iglob).x_last(1)- 
     +        globtrk(iglob).x_first(1))**2 +
     +        (globtrk(iglob).x_last(2)-globtrk(iglob).x_first(2))**2)

      else
         lasthit = gtrk(igtrk).ipnt(gtrk(igtrk).nhit)
         globtrk(iglob).x_last(1) = svthit(lasthit).x(1)
         globtrk(iglob).x_last(2) = svthit(lasthit).x(2)
         globtrk(iglob).x_last(3) = svthit(lasthit).x(3)
         globtrk(iglob).length = 
     +        sqrt((globtrk(iglob).x_last(1)- 
     +        globtrk(iglob).x_first(1))**2 +
     +        (globtrk(iglob).x_last(2)-globtrk(iglob).x_first(2))**2)        
         
      endif
      if( itpct .ne. 0 .and. isvtt .ne. 0) then
         globtrk(iglob).det_id = 3
         if( ipriv .eq. 0) then
            globtrk(iglob).iflag = (sign(1.0,float(gtrk(igtrk).
     +           flag)))*500.+gtrk(igtrk).flag 
         else
            globtrk(iglob).iflag = (sign(1.0,float(gtrk(igtrk).
     +           flag)))*600.+gtrk(igtrk).flag 
         endif
      elseif( isvtt .eq. 0) then
         globtrk(iglob).det_id = 1
         if( ipriv .eq. 0) then
            globtrk(iglob).iflag = (sign(1.0,float(gtrk(igtrk).
     +           flag)))*100.+gtrk(igtrk).flag 
         else
            globtrk(iglob).iflag = (sign(1.0,float(gtrk(igtrk).
     +           flag)))*300.+gtrk(igtrk).flag 
         endif
      elseif( itpct .eq. 0) then
         globtrk(iglob).det_id = 2
         if( ipriv .eq. 0) then
            globtrk(iglob).iflag = (sign(1.0,float(gtrk(igtrk).
     +           flag)))*200.+gtrk(igtrk).flag 
         else
            globtrk(iglob).iflag = (sign(1.0,float(gtrk(igtrk).
     +           flag)))*400.+gtrk(igtrk).flag 
         endif
      endif
      if (globtrk(iglob).length.gt.99999.9) then
         globtrk(iglob).length=99999.9
      endif
      globtrk(iglob).impact = 0.
      globtrk(iglob).ndegf =  2*(gtrk(igtrk).nfit) - 5
      globtrk(iglob).id_global_pid = 0
      globtrk(iglob).id_hypo_pid = 0
      globtrk(iglob).id_start_vertex = 0
      globtrk(iglob).id_stop_vertex = 0
      globtrk(iglob).id_emc = 0
      globtrk(iglob).id_smd = 0

      globtrk_aux(globtrk_aux_h.nok).id_track = iglob
      globtrk_aux(globtrk_aux_h.nok).residuals(1)=0
      globtrk_aux(globtrk_aux_h.nok).residuals(2)=0
      globtrk_aux(globtrk_aux_h.nok).covar_off_diag(1)=covar(1,2)
      globtrk_aux(globtrk_aux_h.nok).covar_off_diag(2)=covar(2,1)
      globtrk_aux(globtrk_aux_h.nok).covar_off_diag(3)=covar(3,1)
      globtrk_aux(globtrk_aux_h.nok).covar_off_diag(4)=covar(3,2)
      globtrk_aux(globtrk_aux_h.nok).covar_off_diag(5)=0
      globtrk_aux(globtrk_aux_h.nok).covar_off_diag(6)=0
      globtrk_aux(globtrk_aux_h.nok).covar_off_diag(7)=0
      globtrk_aux(globtrk_aux_h.nok).covar_off_diag(8)=0
      globtrk_aux(globtrk_aux_h.nok).covar_off_diag(9)=0
      globtrk_aux(globtrk_aux_h.nok).covar_off_diag(10)=0

      EGR_LOAD_GLOBTRK = 0
      return
      end
