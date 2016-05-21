// $Id: TGeoSwim.h,v 1.2 2016/05/21 02:37:38 perev Exp $
//
//
// Class StTGeoSwim
// ------------------
// _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ __


#ifndef ST_TGESWIM_H
#define ST_TGESWIM_H
#include "TString.h"
#include "TNamed.h"
class THelixTrack;
class TGeoNode;
class TGeoMaterial;

class TGeoSwimMag {
public:
TGeoSwimMag(){};
virtual void operator()(const double X[3],double B[3]);
};

class TGeoSwimLoss {
public:
TGeoSwimLoss(double charge=1,double mass=0.13956995){mCharge = charge;mMass = mass;};
//		returns momentum loss
virtual double operator()(const TGeoMaterial* mate,double P,double len
                       ,double *theta2=0);
protected:
double mCharge;
double mMass;
};
class TGeoSwimEnd {
public:
TGeoSwimEnd(){;}
virtual int operator()();
};

class TGeoSwim: public TNamed
{
public:
//   Main cases (KaZes)
  enum SteppingKaze {
  kNEWtrack 		= 1,
  kENTERtrack		= 2,
  kCONTINUEtrack	= 4,
  kEXITtrack		= 8,
  kENDEDtrack		=16,
  kOUTtrack		=32,
  kSTOP			=64,
  kIgnore    		=128};

TGeoSwim(const char *name="");

void Set(TGeoSwimMag *mag=0, TGeoSwimLoss *loss=0,TGeoSwimEnd* endd=0) {fMag=mag;fLoss=loss;fEnd=endd;}
void Set(double Rmax,double Zmin,double Zmax,double sMax=10);
 int Set(THelixTrack *inHelx,THelixTrack *otHelx);
 int Set(const double* pos,const double* dir, double curv);
 int OutScene(const double *x) const ;
 int Swim(double maxLen);
double             GetLen  (int idx=1) const 	{return fInOutLen[idx];}
const TGeoNode    *GetNode (int idx=1) const 	{return fNode[idx];}
      THelixTrack *GetHelix(int idx=1) const 	{return fHelx[idx];}
const double      *GetPos  (int idx=1) const;   
const double      *GetDir  (int idx=1) const;
const char        *GetPath() const;
const TGeoMaterial*GetMate() const; 
const double       GetTime() const		{return fTimeFly;} 
const double       GetPt()   const		{return fPt     ;} 
const double       GetP()    const		{return fP      ;} 

void Swap() { THelixTrack *h=fHelx[0];fHelx[0]=fHelx[1];fHelx[1]=h;}
protected:

TGeoSwimMag  *fMag;
TGeoSwimLoss *fLoss;
TGeoSwimEnd  *fEnd;
double fSmax;	// Max step size
double fRmax;
double fZmin;
double fZmax;
double fInOutLen[2];
double fC;			//curvature 1/cm 
double fP;			//momentum  loss(GeV) 
double fPt;			//momentum  loss(GeV) 
double fPLoss;			//momentum  loss(GeV) 
double fTimeFly;		//time in seconds 
double fB[3];
THelixTrack 	*fHelx[2];
const TGeoNode  *fNode[2];
ClassDef(TGeoSwim,0)

};
#endif
