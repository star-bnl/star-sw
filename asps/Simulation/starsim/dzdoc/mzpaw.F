* $Id: mzpaw.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: mzpaw.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:26  nevski
*  first working release
*
* Revision 1.6  2000/01/09 21:30:48  nevski
* fortran-style memgetf
*
      SUBROUTINE MZPAW (NWORDS,CHOPT)
C-    Initialize the store /PAWC/ of at least 10000 words;
C--------------------------------------------------------------------
*CDE,PAWC. - some variables are renamed !  (wws->ipaw etc)
      COMMON /PAWC/  NWPAW,IXPAWC,IHBOOK,IXHIGZ,IXKU,IFENCE(5),
     +               LMAIN, IPAW(9989)
      CHARACTER*(*) CHOPT

      NW   = abs (NWORDS)
      NW   = max (NW,100000)
      I1   = LOCF(IPAW)
      I2   = MEMGETF (NW+1000)
      If (I2.LT.I1) PRINT *,' MZPAW: requested memory not found '
      LL   = max    (I2-I1,0)
      NW   = NW + LL
      KWW  = LL + 100
C
      CALL MZSTOR (IXPAWC,'/PAWC/',' ',IFENCE,LMAIN,IPAW,IPAW,
     +                                 IPAW(KWW+100),IPAW(NW-100))
      CALL MZWORK (IXPAWC,IPAW(9989),IPAW(KWW),5)
      CALL MZWORK (IXPAWC,IPAW(50),IPAW(5000),0)
c
      NWPAW  = NW
      IHBOOK = 0
      IXHIGZ = 0
      IXKU   = 0
      RETURN
      END


