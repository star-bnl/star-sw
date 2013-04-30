// $Id: StvELossTrak.h,v 1.2 2013/04/30 15:27:43 perev Exp $
//
//
// Class StvELossTrak
// ------------------


#ifndef STIELOSSTRAK_H
#define STIELOSSTRAK_H
#include "TObject.h"
class TGeoMaterial;
static const double PiMASS=0.13956995;
 class StvELossTrak : public TObject
{
public:
         StvELossTrak(){Reset();}
        ~StvELossTrak(){;}
    void Reset();
    void Clear(const char *opt);
    void Set(double A, double Z, double Dens, double x0 
            ,double p,double mass = PiMASS, double charge=1);
    void Add(double len);
  double GetTheta2() const;
  double GetOrt2()   const;

  double ELoss    (double len) 	const;
  double ELoss    () const 		{return ELoss(fTotLen);}
  double ELossErr2(double len) 	const;
  double ELossErr2() 		const	{return fTotELossErr2;}
  double dPP     () 		const;	
  double dPPErr2() 		const;	
  double TotLen() 		const	{return fTotLen;}	

private:
char   fBeg[1];
double fdEdX,fdEdXErr2;
double fP;		//momentum 
double fM;		//mass 
double fE;		//energy 
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
ClassDef(StvELossTrak,0) 
};
#endif //STIELOSSTRAK_H   
   
