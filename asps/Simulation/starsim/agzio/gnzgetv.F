* $Id: gnzgetv.F,v 1.1.1.1 2004/01/12 23:49:37 potekhin Exp $
* $Log: gnzgetv.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:37  potekhin
*
* Revision 1.2  2001/03/02 21:03:13  nevski
* agmer splitted, gnz restored
*
* Revision 1.1  2001/02/27 10:14:17  nevski
*  first working release
*
      SUBROUTINE GNZGETV(IL,IE,IP,VSTAR,IMVRT,PRET,AROT,ROTMA)
C-----------------------------------------------------------------------
C
C Input : IL    - Link number : 1 = primary link
C                               2 = secondary link
C         IE    - Number of event in gate
C         IP    - Index of particle in event record
C
C
C Output: VSTAR(4) - starting position,time for particle relative to
C                    primary interaction. In metres, seconds.
C                    (Note that VSTAR(4) is just taken from particle IP's
C                    event record.)
C
C         IMVRT    - Index of particle whose end point is the vertex for
C                    particle IP. IMVRT should be a unique vertex tag for
C                    this vertex. 0 is returned for the primary vertex.
C                     -1 is returned as an error condition.
C
C         PRET(3)  - returned 3-momentum after taking the effects of a
C                    uniform magnetic field (set using GNZPARR) into account.
C                    This 3-momentum is the same as that from GNZGETP if there
C                    is less than the minimum magnetic field present.
C
C         AROT     - angle of rotation generated during the flights of the
C                    ancestors of particle IP (and not IP's own flight) measured
C                    in radians about the magnetic field axis. This number
C                    can be used along with the rotation matrix ROTMA to rotate
C                    all particles from the current physical vertex, IMVRT.
C                    Note that the rotation has already been applied to PRET.
C
C         ROTMA(3,3) - "unit" rotation matrix describing the rotation of
C                    generated during the flights of ancestors of particle IP.
C                    The user may use this along with AROT and subroutine
C                    GNZROTA to rotate sister particles of IP.
C
C
C This routine uses the parameter TVTMIN (minimum vertex time), which
C can be set using GNZSETP, as a cut-off to very short tracks. VSTAR(4)
C is calculated by looping back through generations until the current
C point is within TVTMIN of zero, ie closer than TVTMIN times the speed
C of light to the primary vertex point.
C
C The index IMVRT is the particle index of the particle first encountered
C in this traceback that has laboratory lifetime greater than TVTMIN.
C A check is done that this particle is unique mother of IP. This means
C that IMVRT can be used as a vertex label. 0 is returned for the
C primary vertex.
C
C The magnetic field return arguments AROT will be returned as zero for
C no rotation (for instance, if there is no magnetic field ). ROTMA will
C only be set, however, if there is a field.
C
C If the magnetic field is weak, or the maximum decay length small ( for
C instance of Kaons are counted as stable), not setting a magnetic field
C in GENZ should be considered according to the subsequent errors in decay
C position and direction of secondary particles; using a magnetic field in GENZ
C may result in significant CPU increases during this routine.
C
C The flag that turns on the handling of the magnetic field is LGNMAG. This
C is set if the magnetic field set by GNZPARR is greater than the parameter
C BMIVAL, also settable by GNZPARR (Key = 'BMIN'). The default value for
C this is 0.0001 Tesla ( 1 Gauss), a rather low value.
C
C Original Version.                                R. DeWolf   25/09/91
C-----------------------------------------------------------------------
#include "commons/typing.inc"
*CDE,GNCPAR
#include "gncsto.inc"
#include "genz/gnccon.inc"
#include "gncpar.inc"
#include "genz/gncevt.inc"
#include "slac_do.inc"
#include "genp_do.inc"

C
      INTEGER IL,IE,IP,IMVRT
      REAL    ROTMA(3,*),VSTAR(*),PRET(*),AROT
C
      REAL    VSTEP(3),DELT,DELTE,ENER,VMOD,TNOW,TDIFF
      REAL    QQ,PMASS,QNEG,PIN(3),AM,ANGR,POUT(3),TSTAR
      INTEGER LGENE,LGENP,LGNFIND,LZFIND,IER
      INTEGER I1,I2,IOFF,IOFF1,IOFF2,NPART,MODUL,IGEN,MAXGEN
      LOGICAL LUNIQ
      CHARACTER*8 CPNAM
      PARAMETER (MAXGEN=1000)
C
      VSTAR(1) = 0.
      VSTAR(2) = 0.
      VSTAR(3) = 0.
      VSTAR(4) = 0.
      VSTEP(1) = 0.
      VSTEP(2) = 0.
      VSTEP(3) = 0.
      IMVRT = 0
C Zero total rotation
      AROT = 0.
      ANGR = 0.
      LUNIQ=.TRUE.
      TSTAR = 0.
C
      IF(IL.EQ.1.OR.IL.EQ.2)THEN
         LGENE = LGNFIND(IL,'GENE')
      ELSE
         WRITE(LGNPRI,10000) IL
10000    FORMAT(' GNZGETV: Link #',I4,' not recognized by GENZ!')
         IMVRT = -1
         GOTO 30
      ENDIF
C
      IF(LGENE.LE.0)THEN
         WRITE(LGNPRI,10100)IL
10100    FORMAT(' GNZGETV: Link value for link #',I4,' is <= 0 !')
         IMVRT = -1
         GOTO 30
      ENDIF
C
C Find the event with the desired IDN:
      LGENE = LZFIND(IGNSTO,LGENE,IE,-5)
      IF(LGENE.LE.0)THEN
         WRITE(LGNPRI, 10200)IE
10200    FORMAT(' GNZGETV: GENE bank for event',I5,' not found.')
         IMVRT = -1
         GOTO 30
      ENDIF
C
      LGENP = LGN(LGENE+IGNOFF-1)
      IF(LGENP.LE.0)THEN
         WRITE(LGNPRI, 10300)IE
10300    FORMAT(' GNZGETV: GENP bank for event',I5,' not found.')
         IMVRT = -1
         GOTO 30
      ENDIF
C
C Go to GENZ frame of reference
C
      LGENP = LGENP + IGNOFF
      NPART = MOD(IGN(LGENP+JOSYS2),NPAKAC)
      MODUL = MOD(IGN(LGENP+JOSYS3),NPAKAC)
C
C add offset for SLAC system words
C
      LGENP = LGENP + NHDWAC
C
      IOFF = MODUL*(IP-1)
C
C Start time is available immediately:
C
C Default momentum ready for returning
      CALL UCOPY(GN(LGENP+IOFF+JOPXPX),PRET,3)
      VSTAR(4) = GN(LGENP+IOFF+JOTIME)
C
      IGEN = 1
C
C Jump out if this particle starts within the minimum time TVTMIN.
      IF(VSTAR(4).LT.TVTMIN) GOTO 20
C
C Loop back through maximum of MAXGEN generations
C Initialize I1 so that FORTRAN checkers are satisfied
      I1 = 0
C
      DO 10 IGEN = 1,MAXGEN
         IF(IGEN.EQ.1)THEN
C For first generation (going backwards), the end point is the present
C particle:
            I2 = IP
         ELSE
C else it is the last generation's start:
            I2 = I1
         ENDIF
C
         IOFF2 = MODUL*(I2-1)
C
C Get track of mother
C
         I1 = IGN(LGENP+IOFF2+JOMOT1)
C Turn off LUNIQ if more than one mother found:
         IF(IGN(LGENP+IOFF2+JOMOT2).NE.0)LUNIQ=.FALSE.

         IF(I1 .GT.0.AND.I1.LE.NPART)THEN
            IOFF1 = MODUL*(I1-1)
C Time difference ( in sec) for last section
            DELT = GN(LGENP+IOFF2+JOTIME)-GN(LGENP+IOFF1+JOTIME)
C If laboratory lifetime of mother is greater than cut-off and mother
C has been unique, then store this as mother particle in the measureable
C sense. First check that IMVRT not yet set:
            IF(IMVRT.EQ.0)THEN
               IF(DELT.GT.TVTMIN.AND.LUNIQ)THEN
                  IMVRT = I1
               ENDIF
            ENDIF

C Calculate distance travelled by mother:
            IF(DELT.GT.0.0)THEN
C
C Do curving step if Magnetic field corrections are turned on:
               IF(LGNMAG)THEN
                  CALL VSCALE(GN(LGENP+IOFF1+JOPXPX),-1.,PIN,3)
C Get current rotation...
                  CALL GNZGROT(AROT,GNZMAG,ROTMA)
C Apply current rotation..
                  CALL GNZROTA(PIN,ROTMA,PIN)
C We need to know the charge...
                  CALL GNZPDAT(IGN(LGENP+IOFF1+JOPDGC),CPNAM,PMASS,QQ,
     +            IER)
                  IF(IER.NE.0)THEN
                     WRITE(LGNPRI,10400)IGN(LGENP+IOFF1+JOPDGC)
10400                FORMAT(' GNZSTEP: Warning! Unknown PDG code',I10)
                     QNEG = 0.
                  ELSE
                     QNEG = - QQ
                  ENDIF
                  AM = GN(LGENP+IOFF1+JOMASS)
                  CALL GNZSTEP(PIN,GNZMAG,DELT,QNEG,AM,POUT,VSTEP,ANGR)
                  AROT = AROT + ANGR
               ELSE
C Form ratio of Delta(t)/E for mother particle:
C Multiply this ratio by mother's momentum, propogating backwards:
                  DELTE=0.
                  IF(GN(LGENP+IOFF1+JOMASS).GT.0.0)THEN
                     ENER = VMOD(GN(LGENP+IOFF1+JOPXPX),4)
                     IF(ENER.GT.0.0)DELTE = DELT/ENER * GNCELE
                  ENDIF
                  CALL VSCALE(GN(LGENP+IOFF1+JOPXPX),-DELTE,VSTEP(1),3)
               ENDIF
C Now add this to original particle
               CALL VADD(VSTAR(1),VSTEP(1),VSTAR(1),3)
C record total time to compare with VSTAR(4) at the end:
               TSTAR = TSTAR + DELT
               TNOW  = GN(LGENP+IOFF1+JOTIME)

               IF(TNOW.LT.TVTMIN)GOTO 20
            ENDIF
         ELSE
C
C Escape from loop
            GOTO 20
         ENDIF
C
   10 CONTINUE
C
   20 CONTINUE
C
C reverse VSTAR for no magnetic field; reverse and then rotate for the
C field ON case:
C
      VSTAR(1)=-VSTAR(1)
      VSTAR(2)=-VSTAR(2)
      VSTAR(3)=-VSTAR(3)

      IF(LGNMAG)THEN
C
C AROT arrived at above is the negative of the true rotation, so
C change it back:
C
         AROT = - AROT
C Get the rotation:
         CALL GNZGROT(AROT,GNZMAG,ROTMA)
         CALL GNZROTA(VSTAR,ROTMA,VSTAR)
C Rotate current particle's momentum for returning:
         CALL GNZROTA(GN(LGENP+IOFF+JOPXPX),ROTMA,PRET)
      ENDIF

      TDIFF = ABS(VSTAR(4) - TSTAR)
C     IF(TDIFF.GT.TVTMIN)THEN
C MORE LENIENT THRESHOLD FOR GENERATORS WHERE INTERNAL PRECISION IS BAD
      IF(TDIFF.GT.1E-12)THEN
         WRITE(LGNPRI,10500)VSTAR(4),TSTAR,TDIFF
10500    FORMAT(' GNZGETV: ERROR! VSTAR,TSTAR,DIFF : ',/,
     +   E13.7,2X,E13.7,2X,E13.7)
      ENDIF
C
   30 CONTINUE
      END

