
C>--------------------------------------------------------------------
C     EGR_FILL_GLOBTRK - fills the global track table
C<--------------------------------------------------------------------
       INTEGER FUNCTION EGR_LOAD_GLOBTRK_KAL(globtrk_h,globtrk,
     +     igtrk,gtrk,tphit_h,tphit,svthit_h,svthit,
     +     covar,itpct,isvtt,ipriv,bfield)
       
C>--------------------------------------------------------------------
C
C     Input arguments
C     globtrk_h    - header for the globtrk table
C     globtrk     - rows of the globtrk table
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
#include "StDetectorId.inc"
C______________________________________________________________________
 
      RECORD /table_head_st/     globtrk_h
      RECORD /dst_track_st/      globtrk(*)
      RECORD /table_head_st/     tphit_h
      RECORD /tcl_tphit_st/      tphit(*)
      RECORD /table_head_st/     svthit_h
      RECORD /scs_spt_st/      svthit(*)
      
C_____________________________________________________________________

C     Local variables

      RECORD /track_pointers/ gtrk(1)
      
      INTEGER igtrk,sgn,lasthit,firsthit,iglob,itpct,isvtt
      INTEGER imsg,ipriv,i,det
      DOUBLE PRECISION    covar(5,5)
      DATA    imsg /0/
      real bfield,gseed,gtanl,ginvpt,glength

 
      globtrk_h.nok = globtrk_h.nok + 1
      iglob = globtrk_h.nok
      globtrk(iglob).id = iglob
      globtrk(iglob).n_point = gtrk(igtrk).nhit
      globtrk(iglob).n_max_point = 0
      globtrk(iglob).n_fit_point = gtrk(igtrk).nfit
      globtrk(iglob).icharge = sign(1.0,gtrk(igtrk).p(5))
      globtrk(iglob).r0 = gtrk(igtrk).p(6)
      globtrk(iglob).phi0 = gtrk(igtrk).p(1)*C_DEG_PER_RAD  ! to degrees     
      globtrk(iglob).phi0 = AMOD(globtrk(iglob).phi0, 360.0)
      if( globtrk(iglob).phi0 .lt. 0) then
         globtrk(iglob).phi0 = globtrk(iglob).phi0 +360.
      endif
      globtrk(iglob).z0 = gtrk(igtrk).p(2)
      sgn = sign(1.0,gtrk(igtrk).p(5))
      globtrk(iglob).psi = gtrk(igtrk).p(3)
      globtrk(iglob).psi = globtrk(iglob).psi*C_DEG_PER_RAD  ! to degrees
      
      globtrk(iglob).psi = AMOD(globtrk(iglob).psi, 360.0)
      if( globtrk(iglob).psi .lt. 0) then
         globtrk(iglob).psi = globtrk(iglob).psi +360.
      endif
      globtrk(iglob).tanl = gtrk(igtrk).p(4)
      globtrk(iglob).invpt = ABS(gtrk(igtrk).p(5))
      globtrk(iglob).covar(1) = REAL(covar(1,1))
      globtrk(iglob).covar(2) = REAL(covar(1,2))
      globtrk(iglob).covar(3) = REAL(covar(1,3))
      globtrk(iglob).covar(4) = REAL(covar(1,4))
      globtrk(iglob).covar(5) = REAL(covar(1,5))
      globtrk(iglob).covar(6) = REAL(covar(2,2))
      globtrk(iglob).covar(7) = REAL(covar(2,3))
      globtrk(iglob).covar(8) = REAL(covar(2,4))
      globtrk(iglob).covar(9) = REAL(covar(2,5))
      globtrk(iglob).covar(10) = REAL(covar(3,3))
      globtrk(iglob).covar(11) = REAL(covar(3,4))
      globtrk(iglob).covar(12) = REAL(covar(3,5))
      globtrk(iglob).covar(13) = REAL(covar(4,4))
      globtrk(iglob).covar(14) = REAL(covar(4,5))
      globtrk(iglob).covar(15) = REAL(covar(5,5))


      globtrk(iglob).chisq(1) = gtrk(igtrk).p(7)
      globtrk(iglob).chisq(2) = 0.0
      globtrk(iglob).length=9999.9
CCC*** This has to be modified if points are thrown away during a refit
       if( isvtt .eq. 0) then
         firsthit = gtrk(igtrk).pos(gtrk(igtrk).nhit)
         i = gtrk(igtrk).nhit
         det = gtrk(igtrk).det(i)        
         do while (firsthit .lt. 0 .or. det .gt. 1)
            i = i-1
            firsthit = gtrk(igtrk).pos(i)
            det = gtrk(igtrk).det(i)
         enddo
         firsthit = gtrk(igtrk).ipnt(i)
         if( det .eq. 1) then
            globtrk(iglob).x_first(1) = tphit(firsthit).x
            globtrk(iglob).x_first(2) = tphit(firsthit).y
            globtrk(iglob).x_first(3) = tphit(firsthit).z
         else
            globtrk(iglob).x_first(1) = -9999.
            globtrk(iglob).x_first(2) = -9999.
            globtrk(iglob).x_first(3) = -9999.       
            
         endif
      else
         firsthit = gtrk(igtrk).pos(gtrk(igtrk).nhit)
         i = gtrk(igtrk).nhit
         det = gtrk(igtrk).det(i)
         do while (firsthit .lt. 0 .or. det .gt. 2)
            i = i-1
            firsthit = gtrk(igtrk).pos(i)
            det = gtrk(igtrk).det(i)
         enddo
         
         firsthit = gtrk(igtrk).ipnt(i)
         if( det .eq. 2) then
            do while (det .eq. 2 .or. gtrk(igtrk).pos(i) .lt. 0 )
               i = i-1
               firsthit = gtrk(igtrk).ipnt(i)
               det = gtrk(igtrk).det(i)
            enddo  
            if( gtrk(igtrk).det(i+1) .eq. 2) then
               firsthit =  gtrk(igtrk).ipnt(i+1)
               globtrk(iglob).x_first(1) = svthit(firsthit).x(1)
               globtrk(iglob).x_first(2) = svthit(firsthit).x(2)
               globtrk(iglob).x_first(3) = svthit(firsthit).x(3)
            else
              globtrk(iglob).x_first(1) = tphit(firsthit).x
              globtrk(iglob).x_first(2) = tphit(firsthit).y
              globtrk(iglob).x_first(3) = tphit(firsthit).z
           endif
        elseif( det .eq. 1) then
            globtrk(iglob).x_first(1) = tphit(firsthit).x
            globtrk(iglob).x_first(2) = tphit(firsthit).y
            globtrk(iglob).x_first(3) = tphit(firsthit).z
         else
            globtrk(iglob).x_first(1) = -9999.
            globtrk(iglob).x_first(2) = -9999.
            globtrk(iglob).x_first(3) = -9999.       
            
         endif
      endif
C     Now find last point on track and calculate track length.
      if (firsthit .gt. 0) then
         if (itpct.ne.0) then
            i = 1
            do while (gtrk(igtrk).pos(i) .lt. 0)
               i=i+1
            enddo           
            lasthit = gtrk(igtrk).ipnt(i)          
            globtrk(iglob).x_last(1) = tphit(lasthit).x
            globtrk(iglob).x_last(2) = tphit(lasthit).y
            globtrk(iglob).x_last(3) = tphit(lasthit).z
C----->>>>  new calculation of .length   S.Margetis
            gseed = sqrt((globtrk(iglob).x_last(1) 
     +           - globtrk(iglob).x_first(1))**2 +
     +           (globtrk(iglob).x_last(2) 
     +           - globtrk(iglob).x_first(2))**2)
            
            gtanl = globtrk(iglob).tanl
            ginvpt= globtrk(iglob).invpt
            call egr_trk_length(gseed,gtanl,ginvpt,bfield,glength)
            globtrk(iglob).length=glength
C---  <<<<<<<
            
         else
            i = gtrk(igtrk).nhit
            det = gtrk(igtrk).det(i)
            do while (gtrk(igtrk).pos(i) .lt. 0)
               i = i-1
             enddo
            lasthit = gtrk(igtrk).ipnt(i)
            if (lasthit .gt. 0) then
               globtrk(iglob).x_last(1) = svthit(lasthit).x(1)
               globtrk(iglob).x_last(2) = svthit(lasthit).x(2)
               globtrk(iglob).x_last(3) = svthit(lasthit).x(3)
C----->>>>  new calculation of .length   S.Margetis
               gseed = sqrt((globtrk(iglob).x_last(1) 
     +              - globtrk(iglob).x_first(1))**2 +
     +              (globtrk(iglob).x_last(2) 
     +              - globtrk(iglob).x_first(2))**2)
               
               gtanl = globtrk(iglob).tanl
               ginvpt= globtrk(iglob).invpt
               call egr_trk_length(gseed,gtanl,ginvpt,bfield,glength)
               globtrk(iglob).length=glength
C---  <<<<<<<         
            endif
         endif
      endif
      if( itpct .ne. 0 .and. isvtt .ne. 0) then
         globtrk(iglob).det_id =  kTpcSsdSvtIdentifier
         if( ipriv .eq. 0) then
            globtrk(iglob).iflag = (sign(1.0,float(gtrk(igtrk).
     +           flag)))*500.+gtrk(igtrk).flag 
         else
            globtrk(iglob).iflag = (sign(1.0,float(gtrk(igtrk).
     +           flag)))*600.+gtrk(igtrk).flag 
         endif
      elseif( isvtt .eq. 0) then
         globtrk(iglob).det_id = kTpcId
         if( ipriv .eq. 0) then
            globtrk(iglob).iflag = (sign(1.0,float(gtrk(igtrk).
     +           flag)))*100.+gtrk(igtrk).flag 
         else
            globtrk(iglob).iflag = (sign(1.0,float(gtrk(igtrk).
     +           flag)))*300.+gtrk(igtrk).flag 
         endif
      elseif( itpct .eq. 0) then
         globtrk(iglob).det_id = kSsdSvtId
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
      globtrk(iglob).id_start_vertex = 0
      globtrk(iglob).mathod = 0
      globtrk(iglob).map(1) = 0
      globtrk(iglob).map(2) = 0
      globtrk(iglob).curvature = abs(C_D_CURVATURE*bfield*
     +     gtrk(igtrk).p(5))

      EGR_LOAD_GLOBTRK_KAL = 0
      return
      end







