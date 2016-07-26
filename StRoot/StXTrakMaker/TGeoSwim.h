// $Id: TGeoSwim.h,v 1.5 2016/07/26 00:59:22 perev Exp $
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
virtual void operator()(const double X[3],double B[3])=0;
};

class TGeoSwimLoss {
public:
TGeoSwimLoss(double charge=1,double mass=0.13956995){mCharge = charge;mMass = mass;};
//		returns momentum loss
virtual double operator()(const TGeoMaterial* mate,double P,double len
                       ,double *theta2=0)=0;
protected:
double mCharge;
double mMass;
};


class TGeoSwimEnd {
public:
//   Main output cases (
  enum EndKase {
  kUndef 		= 0,//Undefined
  kDefault 		= 1,//Immediate return after any step
  kNewVolu		= 2,//Return when new vollume started
  kNameVolu		= 3,//Return, if mother volume name from the given list
  };
TGeoSwimEnd(){;}
virtual void Reset()=0;
virtual int operator()()=0;
virtual const char* GetName() const {return 0;};
virtual       int   GetExit() const {return mEnd;}
protected:
EndKase mEnd;

};

class TGeoSwim: public TNamed
{
public:
enum SwimExit {kEndRange=-1,kNormal=0,kManySteps=13,kNoConv=14,kBadMom=15,kFailed=16,kOutScene=99};
#define kMinMom 0.05	// minimal allowed Pt

TGeoSwim(const char *name="");

void Set(TGeoSwimMag *mag=0, TGeoSwimLoss *loss=0,TGeoSwimEnd* endd=0) 
        {fMag=mag;fLoss=loss;fEnd=endd;}
        
TGeoSwimMag  *GetMag() {return fMag; }
TGeoSwimLoss *GetLoss(){return fLoss;}
TGeoSwimEnd  *GetEnd() {return fEnd; }


void Set(double Rmax,double Zmin,double Zmax,double sMax=10);
 int Set(const double* pos,const double* dir, double curv);
 int OutScene(const double *x) const ;

 int Swim(double maxLen);
double             GetLen  (int idx=2) const 	{return fInOutLen[idx];}
const TGeoNode    *GetNode (int idx=1) const 	{return fNode[idx];}
      THelixTrack *GetHelix(int idx=1) const 	{return fHelx[idx];}
const double      *GetPos  (int idx=1) const;   
const double      *GetDir  (int idx=1) const;
const char        *GetPath() const;
const TGeoMaterial*GetMate() const; 
const double       GetTime() const		{return fTimeFly;} 
const double       GetPti()  const		{return fPti    ;} 
const double       GetPt()   const		{return fPt     ;} 
const double       GetP()    const		{return fP      ;} 
const double       GetPLoss()const		{return fPLoss  ;} 
const double       GetCurv() const		{return fC      ;} 

void Swap() { THelixTrack *h=fHelx[0];fHelx[0]=fHelx[1];fHelx[1]=h;}
protected:

TGeoSwimMag  *fMag;
TGeoSwimLoss *fLoss;
TGeoSwimEnd  *fEnd;
double fSmax;	// Max step size
double fRmax;
double fZmin;
double fZmax;
double fInOutLen[3];
double fC;			//curvature 1/cm 
double fP;			//momentum  loss(GeV) 
double fPti;			//signed invers pt
double fPt;			//momentum  loss(GeV) 
double fPLoss;			//momentum  loss(GeV) 
double fTimeFly;		//time in seconds 
double fB[3];
THelixTrack 	*fHelx[2];
const TGeoNode  *fNode[2];
ClassDef(TGeoSwim,0)

};

class TGeoSwimDefaultEnd: public TGeoSwimEnd
{
public:
TGeoSwimDefaultEnd(TGeoSwimEnd::EndKase kase
                  ,const char ** tits=0){mKase=kase;mTits=tits;mJit=-1;}
virtual void Reset(){mPrevPath="";mJit=-1; mTit=0;}
virtual int operator()();
virtual const char* GetName() const {return mTit;};
protected:
TGeoSwimEnd::EndKase mKase;
TString  mPrevPath;
const char ** mTits;
const char *  mTit;
int           mJit;
};







#endif
