       INTEGER*4 FUNCTION EV0_AM2(
     1            ev0par_h,            ev0par ,
     2           globtrk_h,            globtrk ,
     4            vertex_h,            vertex ,
     5            ev0out_h,            ev0out,
     6              trak_h,            trak) 
C>-----------------------------------------------------------------------
C   Input arguments  (One to a line with definition after ! ) 
C
C     ev0par_h:     control structure header
C     ev0par:       control structure
C     globtrk_h:    global track structure header
C     globtrk:      global track structure 
C     vertex_h:     dst vertex table header
C     vertex:       dst vertex table, co-ords and pointers to aux tables
C     ev0out_h:     dst auxiallary vertex structure header
C     ev0out:       dst auxilliary vertex structure 
C     trak_h:       local ev0 track table header
C     track         local ev0 track table
C
C   Output arguments : 
C     values in ev0out table
C     iok = user status code
C     
C   DESCRIPTION  : Search for secondary vertices (V0).
C                            K_0_short and Lambda particles
C                            and anti-particles are
C                            considered. Only charged decay modes.
C   Created  MARCH-1994   S. Margetis
C   Put into STAF : End of 1996
C   Altered coding for speed : May 1997 Helen Caines
C  Removed monte-carlo input evaluation to seperate modules and moved
C  local trak table be input by user at ami/call rather than hard-wired
C   in code sp can now enter as many tracks as desired : Dec 1997 Helen Caines
C   Now finds b-fld values from call to gufld not from bmag table: Sept 1998 HC
C   Cut determinations now based on det_id of track pairs: Sept 1998 HC 
C   Error conditions    : None-Yet
C<----------------------------------------------------------------------
      IMPLICIT NONE
#include "ev0_am2.inc"
#include "phys_constants.inc"
#include "math_constants.inc"
#include "StDetectorId.inc"

      INTEGER nok,detect_id_i,points(3)
      INTEGER i

      CHARACTER*132 m132

      REAL    xc(2),xlocal(3),bfield(3)

C---executable statements
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
C  set counters for number of rows of good data to 0
         ev0out_h.nok  = 0
         trak_h.nok = 0
C
C  set return code to normal success
      EV0_AM2=STAFCV_OK

c
c Transform tracking parameters to local parameters

c B-field obtained via gufld call

      xlocal(1) = 0.
      xlocal(2) = 0.
      xlocal(3) = 0.
 
      call gufld(xlocal,bfield)
      write(m132,*) 'Using a field of',bfield(3),' KGauss'
      call StInfo(m132)

C convert to Tesla
      bfield(1) = bfield(1)/10.
      bfield(2) = bfield(2)/10.
      bfield(3) = bfield(3)/10.

 
        nok=0
        if((globtrk_h.nok).eq.0) return
        DO i=1,globtrk_h.nok

C     Only write tracks that have more than a certain number of hits in a detector

           if( globtrk(i).det_id .eq. kTpcId )then
              detect_id_i = 1
              points(1) = globtrk(i).n_point - (1000 * int(globtrk(i).n_point/1000))
           elseif( globtrk(i).det_id .eq. kSvtId .or.
     +             trak(i).det_id .eq.  kSsdSvtId .or.
     )             trak(i).det_id .eq. kSsdId )then
              detect_id_i = 2
              points(2) = ((((globtrk(i).n_point - (10000 * int(globtrk(i).n_point/10000)) - points(1))/1000)) 
     +        + (int(globtrk(i).n_point/10000)))
           else
              detect_id_i = 3
              points(3) = (points(1) + points(2))
           endif

           IF(points(detect_id_i) .ge. ev0par(detect_id_i).n_point) then

              if (globtrk(i).iflag .gt. 0 .and. 
     +             globtrk(i).det_id .eq. kTpcId .or.
     +             globtrk(i).det_id .eq. kSvtId .or.  
     +             globtrk(i).det_id .eq. kSsdId .or.
     +             globtrk(i).det_id .eq. kTpcSsdId .or.
     +             globtrk(i).det_id .eq. kTpcSvtId .or.
     +             globtrk(i).det_id .eq. kTpcSsdSvtId .or.
     +             globtrk(i).det_id .eq. kSsdSvtId) then
                 IF(nok .ge. trak_h.maxlen)   then
                call StError('Internal track table too small quiting')
                EV0_AM2= STAFCV_BAD
                return
             endif
             nok=nok+1
             trak(nok).id  = globtrk(i).id
             trak(nok).det_id = globtrk(i).det_id
             trak(nok).r0   = globtrk(i).r0
             trak(nok).phi  = globtrk(i).phi0*C_RAD_PER_DEG
             trak(nok).z0   = globtrk(i).z0
             trak(nok).psi   = globtrk(i).psi*C_RAD_PER_DEG !degrees to radians
             trak(nok).tanl= globtrk(i).tanl
             trak(nok).rc = 1./(C_D_CURVATURE*bfield(3)*10.
     +            *globtrk(i).invpt)
             trak(nok).q  = float(globtrk(i).icharge)
             call ev0_circle_param(trak(nok).r0,xc,
     +            trak(nok).rc,bfield(3))
             trak(nok).xc(1) = xc(1)
             trak(nok).xc(2) = xc(2)
             trak(nok).pid = globtrk(i).pid
             trak(nok).sagitta = globtrk(i).length**2/(8*trak(nok).rc)
             trak(nok).dca = globtrk(i).impact
          endif
       endif
      ENDDO
 35   CONTINUE
      
      trak_h.nok = nok
      write(m132,*) ' No. of trak rows is:',trak_h.nok
      call StInfo(m132)
c     
c     call reconstruction program
c     
      CALL V0_RECONSTRUCT2(trak_h, trak,
     +     ev0par_h,ev0par,
     +     vertex_h,vertex,
     +     ev0out_h,ev0out,
     +     bfield)
c     
c     call analysis (clean up) module, ev0par(1).iflag=1
c     

      
      if( ev0out_h.nok .gt. ev0out_h.maxlen )then
         vertex_h.nok = vertex_h.nok-(ev0out_h.nok-ev0out_h.maxlen)
         ev0out_h.nok = ev0out_h.maxlen
         EV0_AM2= STAFCV_BAD
         return
      endif

      if( vertex_h.nok .gt. vertex_h.maxlen) then
         ev0out_h.nok = ev0out_h.nok-(vertex_h.nok-vertex_h.maxlen)
         vertex_h.nok = vertex_h.maxlen
         EV0_AM2 = STAFCV_BAD
         return
      endif
      
C     V0_CLEAN does nothing??? - gvb (2/25/99)
C     IF( ev0par(1).iflag.eq.1) THEN
C     CALL V0_CLEAN(ev0out_h,ev0out)
C     ENDIF
      
      
      trak_h.nok = 0
C     return a normal success code
      
      write(m132,*) 'Found ',ev0out_h.nok,' v0 candidates'
      call StInfo(m132)
      EV0_AM2=STAFCV_OK
 999  CONTINUE
      RETURN
      END
      
      



