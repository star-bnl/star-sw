// $Id: StvELossTrak.h,v 1.11 2015/06/18 02:10:39 perev Exp $
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

class StvELossTrak : public TObject
{
public:
class Aux {
public:
Aux(){memset(this,0,sizeof(*this));}

const TGeoMaterial* fMat;
float fLen,fP,fdEdX,fA,fZ,fDens,fX0;
};
typedef std::vector<Aux> AuxVect;
public:
         StvELossTrak();
        ~StvELossTrak(){;}
    void Reset(int dir,double mass=PiMASS, double charge=1);
    void Clear(const char *opt="");
     int Same(double A, double Z, double dens, double x0, double p) const;
    void Set (double A, double Z, double Dens, double x0, double p,const TGeoMaterial *mate);
    void Set (const TGeoMaterial *mate,                   double p);
    void Set (double p);
    void Add(double len);
    void Update(int dir,double Pmom);
    void Update() const;
  double GetTheta2() const;
  double GetOrt2()   const;

  double dEdX () const			{return fTotELoss/fTotLen;}
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
  void Print(const char *opt) const;
static double gdrelx(double A,double Z,double DENS,double T,double HMASS);


private:
char   fBeg[1];
char   fDir;
char   fCharge;		//particle charge
double fM;		//mass 
double fdEdX,fdEdXErr2;
double fP[2];		//momentum start & end
double fE;		//energy 
double fFak;
//
char   fMed[1];
double fTotELoss; 	///accumulated energy loss
double fTotELossErr2;	///accumulated error of energy loss
double fTotLen;		///accumulated track length
double fMCS[3];		///
mutable double fdLogEdLogP;	/// d(log(dEdx)/dLog(p)
char   fEnd[1];
AuxVect fMats;
ClassDef(StvELossTrak,0) 
};
#endif //STIELOSSTRAK_H   
   
