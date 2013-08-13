// $Id: StiELossTrak.h,v 2.1 2013/08/13 21:51:11 perev Exp $
//
//
// Class StiELossTrak
// ------------------


#ifndef STIELOSSTRAK_H
#define STIELOSSTRAK_H
#include "TObject.h"
class TGeoMaterial;
static const double PiMASS=0.13956995;
 class StiELossTrak : public TObject
{
public:
         StiELossTrak(){Reset();}
        ~StiELossTrak(){;}
    void Reset();
    void Clear(const char *opt);
    void Set(const TGeoMaterial *mate,double mass=PiMASS,double charge=1);
    void Set(double A, double Z, double Dens, double x0 
            ,double mass = PiMASS, double charge=1);
    void Set(double p); 
    void Add(double len);
  double GetTheta2() const;
  double GetOrt2()   const;

  double ELoss    (double len) const;
  double ELossErr2(double len) const;
  double ELoss    () const	{return fTotELoss;}
  double ELossErr2() const	{return fTotELossErr2;}

private:
char   fBeg[1];
double fdEdX,fdEdXErr2;
double fP;		//momentum 
double fM;		//mass 
double fCharge2;	//particle charge **2
double fFak;
double fA,fZ,fDens,fX0;
//
char   fMed[1];
double fTotELoss; 	///accumulated energy loss
double fTotELossErr2;	///accumulated error of energy loss
double fTotLen;		///accumulated track length
double fMCS[3];
char   fEnd[1];
ClassDef(StiELossTrak,0) 
};
#endif //STIELOSSTRAK_H   
   
