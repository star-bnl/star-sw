// $Id: StvELossTrak.h,v 1.8 2014/03/28 15:24:55 perev Exp $
//
//
// Class StvELossTrak
// ------------------


#ifndef STIELOSSTRAK_H
#define STIELOSSTRAK_H
#include "TObject.h"
#include "vector"
class TGeoMaterial;
static const double PiMASS=0.13956995;

class Aux {
public:
const TGeoMaterial* fMat;
double fLen,fA,fZ,fDens,fX0;
};

class StvELossTrak : public TObject
{
public:
class Aux {
public:
const TGeoMaterial* fMat;
float fLen,fA,fZ,fDens,fX0,fP;
};
typedef std::vector<Aux> AuxVect;
public:
         StvELossTrak();
        ~StvELossTrak(){;}
    void Reset(int dir,double mass=PiMASS, double charge=1);
    void Clear(const char *opt="");
     int Same(const TGeoMaterial *mate) const;
    void Set(double A, double Z, double Dens, double x0, double p);
    void Set(const TGeoMaterial *mate,                   double p);
    void Add(double len);
    void Update(int dir,double Pmom);
  double GetTheta2() const;
  double GetOrt2()   const;

  double dEdX () const			{return fdEdX    ;}
  double ELoss() const 			{return fTotELoss;}
  double ELossErr2() 		const	{return fTotELossErr2;}
  double PLoss(double p) 	const;	
  double dPLossdP0(double p) 	const;	
  double TotLen() 		const	{return fTotLen;}	
  double P() 		        const	{return fP[0];}	
  double M() 		        const	{return fM;}	
     int GetNMats() 		const	{return fMats.size();}	
  const Aux &GetMate(int idx);
  void reset();
  void unset();

private:
char   fBeg[1];
char   fDir;
double fdEdX,fdEdXErr2;
double fP[2];		//momentum start & end
double fM;		//mass 
double fE;		//energy 
double fCharge;	//particle charge
double fFak;
//
char   fMed[1];
double fLstELoss; 	///last energy loss
double fTotELoss; 	///accumulated energy loss
double fTotELossErr2;	///accumulated error of energy loss
double fTotLen;		///accumulated track length
double fTheta2;
double fOrth2;
double fdLogEdLogP;	/// d(log(dEdx)/dLog(p)
char   fEnd[1];
AuxVect fMats;
ClassDef(StvELossTrak,0) 
};
#endif //STIELOSSTRAK_H   
   
