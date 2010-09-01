#ifndef __StiNodePars_h_
#define __StiNodePars_h_
#include "Rtypes.h"
class StiNodePars {
 public:	
  void reset(){memset(this,0,sizeof(StiNodePars));_cosCA=1;}
  void ready(){_cosCA=cos(eta());_sinCA=sin(eta()); curv() = hz()*ptin();
    if (fabs(curv()) < 1e-6) curv()=1e-6;}
  StiNodePars &merge(Double_t wt,StiNodePars &other);

    /// accessors
  Double_t  operator[](Int_t idx) const {return P[idx];}
  Double_t &operator[](Int_t idx)       {return P[idx];}
  Double_t x()    const {return P[0];}
  Double_t y()    const {return P[1];}
  Double_t z()    const {return P[2];}
  Double_t eta()  const {return P[3];} 
  Double_t ptin() const {return P[4];}
  Double_t tanl() const {return P[5];}
  Double_t curv() const {return P[6];}
  Double_t hz()   const {return P[7];}
  Double_t &x()    {return P[0];}
  Double_t &y()    {return P[1];}
  Double_t &z()    {return P[2];}
  Double_t &eta()  {return P[3];} 
  Double_t &ptin() {return P[4];}
  Double_t &tanl() {return P[5];}
  Double_t &curv() {return P[6];}
  Double_t &hz()   {return P[7];}
  Double_t *A(Int_t i = 0)  {return &P[i];}
  
  Int_t     check(const char *pri=0) const;
  void      print() const;

  enum {kX=0,kY,kZ,kEta,kCurv,kTanL};
  
    /// sine and cosine of cross angle
  Double_t _cosCA;
  Double_t _sinCA;
#if __GNUC__ == 4 && __GNUC_MINOR__ >= 5 || __GNUC__ > 4 
  Double_t P[8]; // array of parameters. see below
#else  /* don't work with gcc451 and optimization, keep for documentation */
  union{Double_t P[1];Double_t _x;}; 
  Double_t _y;     //  local Y-coordinate of this track (reference plane)           
  Double_t _z;     //  local Z-coordinate of this track (reference plane)
  Double_t _eta;   //  (signed curvature)*(local Xc of helix axis - X current point on track)
  Double_t _ptin;  //  signed invert pt [sign = sign(-qB)]
  Double_t _tanl;  //  tangent of the track momentum dip angle
  Double_t _curv;  //  signed curvature [sign = sign(-qB)]
  Double_t _hz;    //  Z component magnetic field in units Pt(Gev) = Hz * RCurv(cm)
#endif
};
#endif
