*
* $Id: part2.inc,v 1.2 1998/04/16 16:58:49 fisyak Exp $
*
* $Log: part2.inc,v $
* Revision 1.2  1998/04/16 16:58:49  fisyak
* 2nd pass with gstar
*
*
*
* part2.inc
*
#if defined(CERNLIB_HADR)
*CMZ :          16/12/97  14.29.01  by  Pavel Nevski
*-- Author :
*
*=== part2 ============================================================*
*
*----------------------------------------------------------------------*
*     Include file: part2 copy        Revised on 20-7-90 by A. Ferrari *
*     Note: see also part copy and part3 copy                          *
*     Changes: none                                                    *
*     Included in the following subroutines or functions: not updated  *
*                                                                      *
*     Description of the common block(s) and variable(s)               *
*                                                                      *
*         Kptoip = conversion from part to paprop numbering            *
*         Iptokp = conversion from paprop to part numbering            *
*                                                                      *
*----------------------------------------------------------------------*
*
      PARAMETER (IDMAX8=183)
      CHARACTER*8  ANAME
      COMMON / FKPART / AAM  (IDMAX8), GA     (IDMAX8), TAU    (IDMAX8),
     &                AAMDSC (IDMAX8), IICH   (IDMAX8), IIBAR  (IDMAX8),
     &                K1     (IDMAX8), K2     (IDMAX8), KPTOIP (IDMAX8),
     &                IPTOKP (39)
      COMMON / FKCHPA / ANAME (IDMAX8)
#endif
