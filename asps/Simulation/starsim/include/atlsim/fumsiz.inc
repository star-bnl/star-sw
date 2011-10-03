* $Id: fumsiz.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
* $Log: fumsiz.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.1  2001/02/27 10:15:55  nevski
*  first working release
*
*   fumili internal commons
      INTEGER        L, LL
      REAL           ZZ,Z0,A,DA,RR,GG,SIGM,AMN,AMX,PLU,PL0
      PARAMETER      (L=10,LL=L*(L+1)/2)
      COMMON /FUMIZ/  ZZ(LL)
      COMMON /FUMZ0/  Z0(LL)
      COMMON /FUMILA/ A(L)
      COMMON /FUMIR/  RR(L)
      COMMON /FUMIG/  GG(L)
      COMMON /FUMDA/  DA(L)
      COMMON /FSIGMA/ SIGM(L)
      COMMON /FUMAU/  AMX(L)
      COMMON /FUMAL/  AMN(L)
      COMMON /FUMPL/  PL0(L)
      COMMON /FUMPLU/ PLU(L)
*     - - - - - - - - - - - - - - - - -
