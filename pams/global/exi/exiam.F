CC:>--------------------------------------------------------------------
CC: FILE:       exiam.F
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION EXIAM(
     1            exipar_h,            exipar ,
     2           globtrk_h,           globtrk ,
     3           privert_h,           privert ,
     4            ev0out_h,            ev0out ,
     5            exiout_h,            exiout ,
     6         g2t_track_h,         g2t_track ,
     7        g2t_vertex_h,        g2t_vertex ,
     8       g2t_svt_hit_h,       g2t_svt_hit ) 
      IMPLICIT NONE
#include "exiam.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    EXIAM
C   Input arguments  (One to a line with definition after ! ) 
C
C     exipar_h:     control structure header
C     exipar:       control structure
C     globtrk_h:    global track structure header
C     globtrk:      global track structure 
C     privert_h:    main vertex information header
C     privert:      main vertex information
C     ev0out_h:     V0 input structure header
C     ev0out:       V0 input structure 
C     exiout_h:     result Ksi structure header
C     exiout:       result Ksi structure 
C     g2t_track_h:      kine structure header
C     g2t_track:        kine structure
C     g2t_vertex_h:      vert structure header
C     g2t_vertex:        vert structure
C     g2t_svt_hit_h:  svt hit structure header
C     g2t_svt_hit:    svt hit structure
C
C   Output arguments : 
C     values in ev0out table
C     iok = user status code
C     
C   Functional Description : Search for cascades
C                            Only charged decay modes.
C   Created  MARCH-1994         S. Margetis
C   Modified by                 WK.Wilson
C   Moved to STAF JUNE-1997     S. Margetis
C   Error conditions    : None-Yet
CC:<----------------------------------------------------------------------

C---for internal use  only
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
C--

      INTEGER nok,idd(10000)
      INTEGER ierr
      INTEGER i,j,k,l

      REAL    pt,rr,pi,degrad,raddeg

C---executable statements
C
      pi=acos(-1.)
      degrad = pi/180.
      raddeg = 180./pi      
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
C  set counters for number of rows of good data to 0
         exiout_h.nok  = 0
C
C  set return code to normal success
         exiam = STAFCV_OK
c
c Transform tracking parameters to local parameters
c
        nok=0
        if((globtrk_h.nok).eq.0) return
        DO i=1,globtrk_h.nok        
        nok=nok+1
        trak(nok).id  = globtrk(nok).id
        trak(nok).r   = globtrk(nok).r0
        trak(nok).rf  = globtrk(nok).r0*globtrk(nok).phi0*degrad
        trak(nok).z   = globtrk(nok).z0
        trak(nok).f   = globtrk(nok).psi*degrad    !degrees to radias
        trak(nok).tanl= globtrk(nok).tanl
        trak(nok).qR  = FLOAT(globtrk(nok).icharge)*
     +                  (0.29979*5.)*globtrk(nok).invpt/1000.
        idd(nok)=globtrk(nok).id_global_pid
        IF(nok.ge.10000) GOTO 35
        ENDDO
  35    CONTINUE
c
c call reconstruction program
c
        CALL XI_RECONSTRUCT(nok,idd,trak,       !new by Ken Wilson
     +                      exipar_h,exipar,
     +                      privert_h,privert,
     +                      ev0out_h,ev0out,
     +                      exiout_h,exiout   )
c
c call analysis (clean up) module, exipar(2).iflag=1
c
c        IF( ev0par(2).iflag.eq.1) THEN
c        CALL XI_CLEAN(nok,idd,trak,
c     +                ev0par_h,ev0par,
c     +                ev0out_h,ev0out   )
c        ENDIF
c
c call evaluation module, ev0par(3).iflag=1
c
c        IF( ev0par(3).iflag.eq.1) THEN
                CALL XI_EVAL( nok,idd,trak,
     +                exipar_h,exipar,
     +                exiout_h,exiout,   
     +                g2t_track_h,  g2t_track,
     +                g2t_vertex_h,  g2t_vertex)
C         ENDIF
C
C return a normal success code
      exiam = STAFCV_OK
  999 CONTINUE
      RETURN
      END

