       INTEGER*4 FUNCTION EV0_AM(
     1            ev0par_h,            ev0par ,
     2           globtrk_h,            globtrk ,
     4            vertex_h,            vertex ,
     5            ev0out_h,            ev0out,
     6              trak_h,            trak,
     7              bmag_h,            bmag) 
C>-----------------------------------------------------------------------
C   Input arguments  (One to a line with definition after ! ) 
C
C     ev0par_h:     control structure header
C     ev0par:       control structure
C     globtrk_h:    global track structure header
C     globtrk:      global track structure 
C     vertex_h:     dst vertex table header
C     vertex:       dst vertex table, co-ords and pointers to aux tables
C     ev0out_h:     auxiallary vertex structure header
C     ev0out:       auxilliary vertex structure 
C     trak_h:       local ev0 track table header
C     track         local ev0 track table
C     bmag_h:       Magnetic field table header
C     bmag:         Value of magnetic field
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
C   Error conditions    : None-Yet
C<----------------------------------------------------------------------
      IMPLICIT NONE
#include "ev0_am.inc"
#include "phys_constants.inc"
#include "math_constants.inc"

      INTEGER nok
      INTEGER i


      REAL    xc(2)

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
      EV0_AM=STAFCV_OK
c
c Transform tracking parameters to local parameters
      if( bmag(1).field_type .ne. 1) then
         write(6,*) 
     +        'Dont know how to work without a uniform field yet..'
               ev0_am=STAFCV_BAD
               goto 999
      endif

      write(6,*) 'Using a field of',bmag(1).B,'Tesla'

        nok=0
        if((globtrk_h.nok).eq.0) return
        DO i=1,globtrk_h.nok
           if (globtrk(i).iflag .gt. 0 ) then
              nok=nok+1
              IF(nok .ge. trak_h.maxlen)   GOTO 35
              trak(nok).id  = globtrk(i).id
              trak(nok).r0   = sqrt(globtrk(i).x0**2+globtrk(i).y0**2)
              trak(nok).phi  = atan2(globtrk(i).y0,globtrk(i).x0)
              trak(nok).z0   = globtrk(i).z0
              trak(nok).psi   = globtrk(i).psi*C_RAD_PER_DEG !degrees to radians
              trak(nok).tanl= globtrk(i).tanl
              trak(nok).rc = 1./(C_D_CURVATURE*bmag(1).B*10*globtrk(i).invpt)
              trak(nok).q  = float(globtrk(i).icharge)
              call ev0_circle_param(trak(nok).r0,xc,
     +             trak(nok).rc,bmag(1).B)
              trak(nok).xc(1) = xc(1)
              trak(nok).xc(2) = xc(2)
              trak(nok).pid = globtrk(i).id_global_pid
              trak(nok).sagitta = globtrk(i).length**2/(8*trak(nok).rc)

           endif
        ENDDO
 35     CONTINUE
        
        trak_h.nok = nok
        write(6,*) ' No. of trak rows is:',trak_h.nok
c     
c call reconstruction program
c
        CALL V0_RECONSTRUCT(trak_h, trak,
     +                      ev0par_h,ev0par,
     +                      vertex_h,vertex,
     +                      ev0out_h,ev0out,
     +                      bmag_h, bmag)
c
c call analysis (clean up) module, ev0par(1).iflag=1
c
        if( ev0out_h.nok .gt. ev0out_h.maxlen )then
           ev0out_h.nok = ev0out_h.maxlen
           EV0_AM= STAFCV_BAD
           return
        endif
        if( vertex_h.nok .gt. vertex_h.maxlen) then
           vertex_h.nok = vertex_h.maxlen
           EV0_AM = STAFCV_BAD
           return
        endif

        IF( ev0par(1).iflag.eq.1) THEN
           CALL V0_CLEAN(ev0out_h,ev0out)
        ENDIF
        

        trak_h.nok = 0
C     return a normal success code
        
        write(6,*) 'Found',ev0out_h.nok,' v0 candidates'
        EV0_AM=STAFCV_OK
 999    CONTINUE
        RETURN
        END

