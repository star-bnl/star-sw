*
* $Id: gsstak.F,v 1.2 1998/04/16 16:59:42 fisyak Exp $
*
* $Log: gsstak.F,v $
* Revision 1.2  1998/04/16 16:59:42  fisyak
* 2nd pass with gstar
*
*
#include "geant321/pilot.h"
*CMZ :          25/03/98  00.20.33  by  Pavel Nevski
*CMZ :  1.30/00 04/05/97  23.58.54  by  Pavel Nevski
*CMZ :  3.21/02 29/03/94  15.41.23  by  S.Giani
*-- Author :
      SUBROUTINE GSSTAK (IFLAG)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. GSSTAK (IFLAG)                                        *
C.    *                                                                *
C.    *   Stores in auxiliary stack JSTAK the particle currently       *
C.    *    described in common /GCKINE/.                               *
C.    *                                                                *
C.    *   On request, creates also an entry in structure JKINE :       *
C.    *    IFLAG =                                                     *
C.    *     0 : No entry in JKINE structure required (user)            *
C.    *     1 : New entry in JVERTX / JKINE structures required (user) *
C.    *    <0 : New entry in JKINE structure at vertex -IFLAG (user)   *
C.    *     2 : Entry in JKINE structure exists already (from GTREVE)  *
C.    *                                                                *
C.    *   Called by : GSKING, GTREVE                                   *
C.    *   Author    : S.Banerjee, F.Bruyant                            *
C.    *   Modifications: PN - put protection against lack of memory.   *
C.    *   - reject Cerenkov photons if mechanism is not activated.     *
C.    *   - save interaction description (MECAT) in stack and NTTARG   *
C.    *   - accept all unknown IPART as geantino                       *
C.    ******************************************************************
C.
#include "gst/atcdes/typing.inc"
#include "geant321/gcbank.inc"
#include "geant321/gctrak.inc"
#include "geant321/gckine.inc"
#include "geant321/gcking.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcjloc.inc"
#include "geant321/gcmzfo.inc"
#include "geant321/gcnum.inc"
#include "geant321/gcstak.inc"
#include "geant321/gcvolu.inc"
#include "geant321/gctmed.inc"
#if defined(CERNLIB_USRJMP)
#include "geant321/gcjump.inc"
#endif
*
      INTEGER         NVTX,ITR
      COMMON /VTXKIN/ NVTX,ITR
      REAL    UBUF(1)
      DATA    UBUF/0./
      INTEGER         IFLAG,IER,ISKIP,LP,ITRT,JST,I,MECAT
C.
C.    ------------------------------------------------------------------
*
      IF (IEOTRI.NE.0) RETURN
*
*     IF (IPART.LE.0.OR.IPART.GT.NPART) ...
      IF (IPART.LE.0)  THEN
         PRINT *, ' GSSTAK - Unknown particle code, skip track ', IPART
         GO TO 999
      ENDIF
*
* ***                          protect against lack of memory faults
      IF (IFLAG.NE.0) THEN
         CALL AGNEED(IER)
         IF (IER.NE.0) RETURN
      ENDIF
*
* ***                   create target description for GEANT produced vertices
      Mecat = 0
      If (NGKINE.GT.0 .and. NMEC.GT.0) then
          Mecat = min(LMEC(NMEC),49) + LVOLUM(NLEVEL)*100
          If (ISTOP.EQ.0) Mecat = Mecat+50 ! flag non-stopped particles
          If (ISTAK.GT.0) Mecat = -Mecat   ! flag disconnected vertex
      ENDIF
*
* ***                          Give control to user for track selection
#if !defined(CERNLIB_USRJMP)
      CALL GUSKIP(ISKIP)
#endif
#if defined(CERNLIB_USRJMP)
      CALL JUMPT1(JUSKIP,ISKIP)
#endif
      IF (ISKIP.NE.0) GO TO 999
*
* *** Check if an entry in JKINE structure is required
*
      IF (IFLAG.EQ.1) THEN
         CALL GSVERT (VERT,  ITRA, 0, UBUF, 0, NVTX)
         If (NVTX.LE.0) THEN
            Print *,' GSSTAK cannot set Vertex for ',ITRA,Mecat
            Print *,' ****** event simulation abandoned ******'
            IEOTRI=1
            RETURN
         ENDIF
         Q(6+LQ(JVERTX-NVTX))=MECAT
         CALL GSKINE (PVERT, IPART, NVTX, UBUF, 0, ITR)
      ELSE IF (IFLAG.LT.0) THEN
         NVTX = -IFLAG
         CALL GSKINE (PVERT, IPART, NVTX, UBUF, 0, ITR)
      ELSE
         IF (IFLAG.EQ.0) THEN
*          Store -ITRA in stack for a track without entry in JKINE
            ITR = -ITRA
         ELSE
            ITR = ITRA
         ENDIF
      ENDIF
*
* *** Store information in stack
*
      IF (JSTAK.EQ.0) THEN
         NDBOOK = NTSTKP*NWSTAK +3
         NDPUSH = NTSTKS*NWSTAK
         CALL MZBOOK (IXCONS,JSTAK,JSTAK,1,'STAK', 0,0,NDBOOK, IOSTAK,3)
         IQ(JSTAK+2) = NTSTKP
      ELSE IF (IQ(JSTAK+1).EQ.IQ(JSTAK+2)) THEN
         CALL AGNEED (IER)
         IF (IER.NE.0) RETURN
         CALL MZPUSH (IXCONS, JSTAK, 0, NDPUSH, 'I')
         IQ(JSTAK+2) = IQ(JSTAK+2) +NTSTKS
      ENDIF
*
      LP   = 0
      IF (IPART.LE.IQ(JPART-2)) LP = LQ(JPART-Ipart)
      IF (LP.LE.0)              LP = LQ(JPART-48)
 
*PN:  do not save Cerenkov photons if the mechanism is not activated
      ITRT =  Q(6+LP)
      IF (ITRT.EQ.7 .and. ITCKOV.EQ.0) GO TO 999
*
      JST = JSTAK +IQ(JSTAK+1)*NWSTAK +3
      IQ(JSTAK+1) = IQ(JSTAK+1) +1
      IF (IQ(JSTAK+3).EQ.0)  IQ(JSTAK+3) = IQ(JSTAK+1)
      IF (IQ(JSTAK+1).GT.NSTMAX)  NSTMAX = IQ(JSTAK+1)
*
      IQ(JST+1)   = ITR       ! track number(or parent), STACK_ONLY flag
      IQ(JST+2)   = IPART     ! particle type
      IQ(JST+3)   = Mecat     ! its production history
      DO 90 I = 1,3
         Q(JST+3+I) = VERT(I)
         Q(JST+6+I) = PVERT(I)
   90 CONTINUE
      Q(JST+10) = TOFG
      Q(JST+11) = SAFETY
      Q(JST+12) = UPWGHT
*
      NALIVE = NALIVE +1
*                                                             END GSSTAK
  999 END
