       Integer*4 FUNCTION EV0_AM3(
     1           ev0par_h,            ev0par ,
     2           globtrkpi_h,          globtrkpi ,
     4            vertex_h,            vertex ,
     5            ev0out_h,            ev0out,
     6              trak_h,            trak) 
C>-----------------------------------------------------------------------
C   Input arguments  (One to a line with definition after ! ) 
C
C     ev0par_h:     control structure header
C     ev0par:       control structure
C     globtrkpi_h:  global pion track structure header
C     globtrkpi:    global pion track structure 
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
C   Modified April 2000   A.C. Saulys
C     to use constrained fits
C<----------------------------------------------------------------------
      IMPLICIT NONE
#include "ev0_am3.inc"
#include "phys_constants.inc"
#include "math_constants.inc"
#include "StDetectorId.inc"

      INTEGER nok, nokpi
      INTEGER i,j,ic
      INTEGER iret
      INTEGER ndegf
      INTEGER detect_id_i,points(3)

      REAL    xc(2),xlocal(3),bfield(3)
      REAL    mainv(3)
      REAL    Prob_cern

      CHARACTER*132 m132

      DOUBLE PRECISION covi(15)

C---executable statements
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
CC:  Load Primary vertex position coordinates:


      mainv(1) = 0.0
      mainv(2) = 0.0
      mainv(3) = 0.0
         do i = 1,vertex_h.nok
            if(vertex(i).vtx_id .eq. 1) then
               mainv(1) = vertex(i).x
               mainv(2) = vertex(i).y
               mainv(3) = vertex(i).z
               go to 100
            end if
         end do
100      continue

C  set counters for number of rows of good data to 0
         ev0out_h.nok  = 0
         trak_h.nok = 0
C
C  set return code to normal success
      EV0_AM3=STAFCV_OK

c
c Transform tracking parameters to local parameters

c B-field obtained via gufld call

      xlocal(1) = 0.
      xlocal(2) = 0.
      xlocal(3) = 0.
 
      call gufld(xlocal,bfield)
      write(6,*) 'Using a field of',bfield(3),' KGauss'

C convert to Tesla
      bfield(1) = bfield(1)/10.
      bfield(2) = bfield(2)/10.
      bfield(3) = bfield(3)/10.

 
        nok=0
        if((globtrkpi_h.nok).eq.0) return
        DO i=1,globtrkpi_h.nok


C     Only write tracks that have more than a certain number of hits in a detector

           if( globtrkpi(i).det_id .eq. kTpcId )then
              detect_id_i = 1
              points(1) = globtrkpi(i).n_point - (1000 * int(globtrkpi(i).n_point/1000))
           elseif( globtrkpi(i).det_id .eq. kSvtId .or.
     +             trak(i).det_id .eq.  kSsdSvtId .or.
     )             trak(i).det_id .eq. kSsdId )then
              detect_id_i = 2
              points(2) = ((((globtrkpi(i).n_point - (10000 * int(globtrkpi(i).n_point/10000)) - points(1))/1000)) 
     +        + (int(globtrkpi(i).n_point/10000)))
           else
              detect_id_i = 3
              points(3) = (points(1) + points(2))
           endif

           IF(points(detect_id_i) .ge. ev0par(detect_id_i).n_point) then

              if (globtrkpi(i).iflag .gt. 0 .and. 
     +             globtrkpi(i).det_id .eq. kTpcId .or.
     +             globtrkpi(i).det_id .eq. kSvtId .or.  
     +             globtrkpi(i).det_id .eq. kSsdId .or.
     +             globtrkpi(i).det_id .eq. kTpcSsdId .or.
     +             globtrkpi(i).det_id .eq. kTpcSvtId .or.
     +             globtrkpi(i).det_id .eq. kTpcSsdSvtId .or.
     +             globtrkpi(i).det_id .eq. kSsdSvtId) then

              nok=nok+1
              IF(nok .gt. trak_h.maxlen)THEN
        write(m132,*) ' No. of trak rows is:',nok,' max= ',trak_h.maxlen
                call StInfo(m132)
                call StError('Internal track table too small quiting')
                EV0_AM3= STAFCV_BAD
                nok=nok-1
                return
              endif

              trak(nok).id  = globtrkpi(i).id
              trak(nok).det_id = globtrkpi(i).det_id
              trak(nok).r0   = globtrkpi(i).r0
              trak(nok).phi  = globtrkpi(i).phi0*C_RAD_PER_DEG
              trak(nok).z0   = globtrkpi(i).z0
              trak(nok).psi   = globtrkpi(i).psi*C_RAD_PER_DEG !degrees to radians
              trak(nok).tanl= globtrkpi(i).tanl
              trak(nok).rc = 1./(C_D_CURVATURE*bfield(3)*10.
     +             *globtrkpi(i).invpt)
              trak(nok).q  = float(globtrkpi(i).icharge)
              call ev0_circle_param(trak(nok).r0,xc,
     +             trak(nok).rc,bfield(3))
              trak(nok).xc(1) = xc(1)
              trak(nok).xc(2) = xc(2)
              trak(nok).pid = globtrkpi(i).pid
              trak(nok).sagitta = globtrkpi(i).length**2/(8*trak(nok).rc)
              trak(nok).chisq = globtrkpi(i).chisq(1) 
              trak(nok).ndef = 2*globtrkpi(i).n_fit_point-5
              trak(nok).dcachis = globtrkpi(i).chisq(2)

              do ic = 1,15
                trak(nok).cov(ic) = globtrkpi(i).covar(ic)
              enddo
            endif
           endif
        ENDDO

 35     CONTINUE
                
        trak_h.nok = nok
        write(m132,*) ' No. of trak rows is:',trak_h.nok
        call StInfo(m132)

CCC        write(6,*) ' No. of trak rows is:',trak_h.nok
c     
c     call reconstruction program
c     
        CALL V0_RECONSTRUCT3(trak_h, trak,
     +       ev0par_h,ev0par,
     +       vertex_h,vertex,
     +       ev0out_h,ev0out,
     +       bfield)
c
c call analysis (clean up) module, ev0par(1).iflag=1
c

        if( ev0out_h.nok .gt. ev0out_h.maxlen )then
           ev0out_h.nok = ev0out_h.maxlen
           EV0_AM3= STAFCV_BAD
           return
        endif
        if( vertex_h.nok .gt. vertex_h.maxlen) then
           vertex_h.nok = vertex_h.maxlen
           EV0_AM3 = STAFCV_BAD
           return
        endif

        trak_h.nok = 0
C     return a normal success code
        
        write(m132,*) 'Found ',ev0out_h.nok,' v0 candidates'
        call StInfo(m132)

        EV0_AM3=STAFCV_OK
 999    CONTINUE
        RETURN
        END





