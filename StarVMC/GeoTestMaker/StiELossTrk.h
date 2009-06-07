// $Id: StiELossTrk.h,v 1.1 2009/06/07 02:28:36 perev Exp $
//
//
// Class StiELossTrk
// ------------------


#ifndef STIELOSSTRK_H
#define STIELOSSTRK_H
static const double PiMASS=0.13956995;
 class StiELossTrk
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
};
#endif //STIELOSSTRK_H   
   
