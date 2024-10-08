*
* $Id: ublow.F,v 1.1.1.1 2005/05/25 20:36:46 fisyak Exp $
*
* $Log: ublow.F,v $
* Revision 1.1.1.1  2005/05/25 20:36:46  fisyak
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  1999/05/18 15:55:32  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/02/15 17:50:23  mclareni
* Kernlib
*
*
      SUBROUTINE UBLOW (MS,MT,NCHP)
C
C CERN PROGLIB# M409    UBLOW           .VERSION KERNVMI  1.01  891208
C ORIG. 03/02/89  K.M.STORR
C

      DIMENSION    MS(99), MT(99), NCHP(9)
      DATA IBLAN1  /z'20202000'/
      DATA MASK1   /z'000000ff'/

      NCH = NCHP(1)
      IF   (NCH)             91, 29, 11
   11 NWS    = ishft (NCH,-2)
      NTRAIL = IAND (NCH,3)
      JT     = 0
      IF (NWS.EQ.0)          GO TO 26

C--                Unpack the initial complete words

      DO 24 JS=1,NWS
      MWD      = MS(JS)
      MT(JT+1) = IOR (IBLAN1,IAND(MASK1,MWD))
      MT(JT+2) = IOR (IBLAN1,IAND(MASK1,ISHFT(MWD, -8)))
      MT(JT+3) = IOR (IBLAN1,IAND(MASK1,ISHFT(MWD,-16)))
      MT(JT+4) = IOR (IBLAN1,           ISHFT(MWD,-24) )
   24 JT = JT + 4

      IF (NTRAIL.EQ.0)       RETURN

C--                Unpack the trailing word

   26 MWD = MS(NWS+1)

      DO 28 JS=1,NTRAIL
      MT(JT+1) = IOR (IBLAN1,IAND(MASK1,MWD))
      MWD = ISHFT (MWD,-8)
   28 JT = JT + 1
   29 RETURN

   91 CALL ABEND
      END
