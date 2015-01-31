#ifndef __StiNodePars_h_
#define __StiNodePars_h_
#include "Rtypes.h"
class StiNodePars {
 public:	
  enum {kX=0,kY=1,kZ=2,kPhi=3,kPtin=4,kTan=5,kCurv=6,kHz=7};
  void reset(){memset(this,0,sizeof(StiNodePars));_cosCA=1;}
  void ready();
   int isZeroH() const { return fabs(P[kHz]) <= 2e-6;}
  StiNodePars &merge(Double_t wt,StiNodePars &other);
  StiNodePars &operator=(const StiNodePars& fr);
  void rotate(double alfa);
    /// accessors
  Double_t  operator[](Int_t idx) const {return P[idx];}
  Double_t &operator[](Int_t idx)       {return P[idx];}
  Double_t x()    const {return P[kX];} 
  Double_t y()    const {return P[kY];}//  local Y-coordinate of this track (reference plane)           		     
  Double_t z()    const {return P[kZ];}//  local Z-coordinate of this track (reference plane)			     
  Double_t eta()  const {return P[kPhi];}//  phi angle
  Double_t phi()  const {return P[kPhi];}//  phi angle again
  Double_t ptin() const {return P[kPtin];}//  signed invert pt [sign = sign(-qB)]					     
  Double_t tanl() const {return P[kTan];}//  tangent of the track momentum dip angle
  Double_t curv() const {return P[kCurv];}//  signed curvature [sign = sign(-qB)]					     
  Double_t rxy2() const {return     (P[kX]*P[kX]+P[kY]*P[kY]);}
  Double_t rxy()  const {return sqrt(P[kX]*P[kX]+P[kY]*P[kY]);}
  Double_t hz()   const {return P[kHz];}//  Z component magnetic field in units Pt(Gev) = Hz * RCurv(cm)                  
  Double_t &x()    	{return P[kX];}
  Double_t &y()    	{return P[kY];}
  Double_t &z()    	{return P[kZ];}
  Double_t &eta()  	{return P[kPhi];} 
  Double_t &ptin() 	{return P[kPtin];}
  Double_t &tanl() 	{return P[kTan];}
  Double_t &curv() 	{return P[kCurv];}
  Double_t &hz()   	{return P[kHz];}
  Double_t *A(Int_t i = 0)  {return &P[i];}
  
  Int_t     check(const char *pri=0) const;
  void      print() const;

    /// sine and cosine of cross angle
  Double_t _cosCA;
  Double_t _sinCA;
  Double_t P[8]; // array of parameters. see below
};

class StiELoss 
{
public:
float mELoss,mLen,mDens,mX0;
};

inline void StiNodePars::ready()
{
_cosCA=cos(P[kPhi]);_sinCA=sin(P[kPhi]); 
if (fabs(P[kHz])<=2e-6) {P[kHz] = 2e-6; P[kPtin]=1./0.350;}
P[kCurv]  = P[kHz]*P[kPtin];
}


#endif
