// $Id: StiELossTrk.h,v 1.2 2009/10/13 17:19:35 perev Exp $
//
//
// Class StiELossTrk
// ------------------


#ifndef STIELOSSTRK_H
#define STIELOSSTRK_H
#include "TObject.h"

static const double PiMASS=0.13956995;
 class StiELossTrk : public TObject
{
public:
         StiELossTrk(){Reset();}
        ~StiELossTrk(){;}
    void Reset();
    void Set(double p2,double mass = PiMASS);
    void Add(double len,double x0);
  double GetTheta2() const;
  double GetOrt2()   const;
    void GetCoef(double coe[3])   const;
static  double GetOrt2(double coe[3],double len) ;
private:
char   fBeg[1];
double fP2;
double fMas2;
double fFak;
double fTotLen;
double fMCS[3];
char   fEnd[1];
ClassDef(StiELossTrk,0) 
};
#endif //STIELOSSTRK_H   
   
