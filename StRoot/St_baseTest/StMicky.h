//*-- Author :    Valery Fine(fine@bnl.gov)   25/09/99  
//  This class is to provide "micky" test for RMath.h methods
//  derived from CERNLIB 
//  http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 
// $Id: StMicky.h,v 1.2 1999/09/26 19:35:12 fine Exp $
// $Log: StMicky.h,v $
// Revision 1.2  1999/09/26 19:35:12  fine
// Micky test has been rearranged
//
// Revision 1.1  1999/09/24 17:25:04  fine
// Missing StMicky.h
//
// Revision 1.1  1999/09/23 18:33:10  fine
// test system for RMath class has been introduced
//

// #include "Rtypes.h"

typedef struct {
         int iqbitw, iqchaw, itb, nlines, itimes;
         float timerd;
         int iflgu, lungu;
         float zergu, zerov[5], zerlev;
         int loglev, nfaipr, neachp, nfailt, nfail, ntest, mtestv[20];
      } PARAM;

typedef struct {
          float a[1000], b[1010];
          int ibcd[47], intg[100];
        } BLNK;

 class StMicky {
   private:
      struct PARAM param_1;
      struct BLNK  _BLNK__1;
      int *ia; int *ib;
   public:
     StMicky();
     BLNK  &Blank() { return *(&(_BLNK__1)); }     
     void Minit();
     void Mverif(int ntt, float *have, float *amust, int nn);
     void Newguy(const char *t1, const char *t2);
     void Prtest();
     PARAM &Param() { return *(&param_1);}
     void Timing(int *) const; 
     void Timed(float *)  const; 
//--  tests
     void StMicky::Tmxm(); // F110
 };

