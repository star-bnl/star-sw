* $Id: trbank.inc,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: trbank.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:07  nevski
*  first working release
*
*   - Track point bank
      INTEGER         LTB,NTB,LPNT,ISPOINT
      PARAMETER       (LTB=10000,LPNT=12)
      REAL            TRPOINT, DTRPOINT
      COMMON /TRBANK/ NTB,TRPOINT(LPNT,LTB)
* ISPOINT contains IPP={IP,IZ,IF} of counter, negative if point rejected
      COMMON /TRBANI/     ISPOINT(LTB)
* DTRPOINT contains the cluster position error (ask A.Rozanov for details)
      COMMON /TRBANJ/     DTRPOINT(LTB)
      REAL            OPNT,XPNT,DPNT,SPNT,APNT,EPNT,PPNT
      COMMON /TRKPNT/ OPNT,XPNT(3),DPNT,SPNT(2),APNT(3),EPNT,PPNT
*  1  : Opnt    - 1+tilt for barrel, 2 for end-caps
* 2-4 : Xpnt(i) - r,phi,z of the element center
*  5  : Dpnt    - stereo angle for strips, drift distance for TRT
* 6-7 : Spnt(i) - error in phi and Z/R
*  8  : Apnt(1) - detector code as follows
*  9  :     (2) - plane number in AgCRTAB
* 10  :     (3) - first digi reference number
* 11  : Epnt    - energy or discriminator value
* 12  : Ppnt    - MC reference track number
      REAL            PIXB,PIXE,GAAS,SCTT,ZSCT,SITV,SIT,MSGC,XTRT
      DATA            PIXB,PIXE,GAAS,SCTT,ZSCT,SITV,SIT,MSGC,XTRT
     +                / 1,   2,   3,   4,   5,   6,  7,   8,   9  /
*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
