*
* $Id: gfinds.F,v 1.1.1.1 2004/01/12 23:49:40 potekhin Exp $
*
* $Log: gfinds.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:40  potekhin
*
* Revision 1.1  2001/02/27 10:15:35  nevski
*  first working release
*
* Revision 1.3  2000/01/09 21:30:47  nevski
* fortran-style memgetf
*
* Revision 1.2  1998/04/16 16:59:31  fisyak
* 2nd pass with gstar
*
*
#include "geant321/pilot.h"
*CMZ :          06/12/97  17.19.33  by  Pavel Nevski
*CMZ :  3.21/02 06/07/94  18.26.03  by  S.Giani
*-- Author :
      SUBROUTINE GFINDS
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Returns the set/volume parameters corresponding to       *
C.    *       the current volume in /GCVOLU/ and fill common /GCSETS/  *
C.    *                                                                *
C.    *       IHSET  user set identifier                               *
C.    *       IHDET  user detector identifier                          *
C.    *       ISET   set number in JSET                                *
C.    *       IDET   detector number in JS=LQ(JSET-ISET)               *
C.    *       IDTYPE subsystem numeber                                 *
C.    *       NUMBV  detector volume numbers (array of length NVNAME)  *
C.    *       NVNAME number of volume levels                           *
C.    *                                                                *
C.    *    ==>Called by : GTRACK                                       *
C.    *       Author    R.Brun  *********                              *
C.    *       Modified  for AGI - PN                                   *
C.    *       Jattf corrected by Bagdan Pavlik (Cracow)                *
C.    ******************************************************************
C.
#include "commons/typing.inc"
#include "geant321/gcbank.inc"
#include "geant321/gcsets.inc"
#include "geant321/gcvolu.inc"
C.
      INTEGER  JATTF,JV,JVO,JAT,JS,JD,I,J,NAME
      JATTF(JV) = JV + Nint(Q(JV+5)) + 6
C.
C.    ------------------------------------------------------------------
C.
      IHSET = 0
      IHDET = 0
      ISET  = 0
      IDET  = 0
      IDTYPE = 0
      NVNAME = 0
*
      JVO    = LQ(JVOLUM-LVOLUM(NLEVEL))
      JAT    = JATTF(JVO)
      ISET   = Q(JAT+7)
      IF (ISET.LE.0) RETURN
      IDET   = Q(JAT+8)
      IF (IDET.LE.0) RETURN
      IDTYPE = Q(JAT+9)
      IHSET  = IQ(JSET+ISET)
      JS     = LQ(JSET-ISET)
      IHDET  = IQ(JS+IDET)
      JD     = LQ(JS-IDET)
      NVNAME = IQ(JD+2)
      J      = 0
      DO 40 I=1,NVNAME
         NAME     = IQ(JD+2*I+9)
         NUMBV(I) = 0
         DO J=J+1,NLEVEL
            JVO = LQ(JVOLUM-LVOLUM(J))
            IF (IQ(JVO-4).EQ.NAME) THEN
*              write (6,'(i5,2x,a4)')  i,name
               NUMBV(I)=NUMBER(J)
               GO TO 40
            ENDIF
         ENDDO
  40  CONTINUE
C
      END
