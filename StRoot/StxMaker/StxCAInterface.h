#ifndef __StxCAInterface_h__
#define __StxCAInterface_h__
#include "TPCCATracker/StTPCCAInterface.h"
#define kEC       2.99792458e-4
#define kZEROHZ   2e-6			//minimal/zero mag field
#define kZEROPTI  1e-3			//minimal/zero 1/pt
#define kZEROCURV (kZEROHZ*kZEROPTI) 	//minimal/zero curvature
#define kBIGPT    10                    //Pt when energy loss ignored
class AliHLTTPCCATrackParam;
class StHit;
struct SeedHit_t {
  Int_t padrow, status, taken, track_key ; //"m" for modified
  const StHit  *hit;
  Int_t      Id; // from CA
};
class StxNodePars {
 public:	
  enum {kX=0,kY=1,kZ=2,kPhi=3,kPtin=4,kTan=5,kCurv=6,kHz=7};
  void reset(){memset(this,0,sizeof(StxNodePars));_cosCA=1;}
  int isZeroH() const { return fabs(P[kHz]) <= kZEROHZ;}
  void ready () {
    _cosCA=cos(P[kPhi]);_sinCA=sin(P[kPhi]); 
    if (fabs(P[kHz])<= kZEROHZ) {
      P[kCurv] =  kZEROCURV; P[kPtin]= kZEROPTI;
    } else {
      P[kCurv]  = P[kHz]*P[kPtin];
    }
  }
  /// accessors
  operator const Double_t *() const	{return P;}
  operator       Double_t *() 		{return P;}
  Double_t x()    const 	{return P[kX];} 
  Double_t y()    const 	{return P[kY];}//  local Y-coordinate of this track (reference plane)           		     
  Double_t z()    const 	{return P[kZ];}//  local Z-coordinate of this track (reference plane)			     
  Double_t eta()  const 	{return P[kPhi];}//  phi angle
  Double_t phi()  const 	{return P[kPhi];}//  phi angle again
  Double_t ptin() const 	{return P[kPtin];}//  signed invert pt [sign = sign(-qB)]					     
  Double_t tanl() const 	{return P[kTan];}//  tangent of the track momentum dip angle
  Double_t curv() const 	{return P[kCurv];}//  signed curvature [sign = sign(-qB)]					     
  Double_t rxy2() const 	{return     (P[kX]*P[kX]+P[kY]*P[kY]);}
  Double_t rxy()  const 	{return sqrt(P[kX]*P[kX]+P[kY]*P[kY]);}
  Double_t hz()   const 	{return P[kHz];}//  Z component magnetic field in units Pt(Gev) = Hz * RCurv(cm)                  
  Double_t &x()    	{return P[kX];}
  Double_t &y()    	{return P[kY];}
  Double_t &z()    	{return P[kZ];}
  Double_t &eta()  	{return P[kPhi];} 
  Double_t &ptin() 	{return P[kPtin];}
  Double_t &tanl() 	{return P[kTan];}
  Double_t &curv() 	{return P[kCurv];}
  Double_t &hz()   	{return P[kHz];}
  Double_t *A(Int_t i)    {return &P[i];}
  Double_t *A()           {return P;}
  Int_t     check(const char *pri=0) const;
  void      print() const;

    /// sine and cosine of cross angle
  Double_t _cosCA;
  Double_t _sinCA;
  Double_t P[8]; // array of parameters. see below
};


class StxNodeErrs {
 public:	
  enum {kBigLen = 20};
  enum eTkPars {kNPars=6,kNErrs=21};
  void reset()				{memset(this,0,sizeof(StxNodeErrs));}
  Double_t getDelta()  const 		{return sqrt(_cXX+_cYY+_cZZ);}
  Double_t getDelta2() const 		{return     (_cXX+_cYY+_cZZ);}
  const Double_t* G() const { return &_cXX;} 
  Double_t* G()       { return &_cXX;} 
  StxNodeErrs &operator*=(Double_t f) 	{for (int i=0;i<kNErrs;i++){G()[i]*=f;}; return *this;}
  StxNodeErrs &merge(Double_t wt,StxNodeErrs &other);
  void get00(      Double_t *a) const;
  void set00(const Double_t *a)      ;
  void get10(      Double_t *a) const;
  void set10(const Double_t *a)      ;
  void get11(      Double_t *a) const;
  void set11(const Double_t *a)      ;
 public:	
  Double_t _cXX;
  Double_t _cYX,_cYY;                       
  Double_t _cZX,_cZY, _cZZ;                 
  Double_t _cEX,_cEY, _cEZ, _cEE;           
  Double_t _cPX,_cPY, _cPZ, _cPE, _cPP;     
  Double_t _cTX,_cTY, _cTZ, _cTE, _cTP, _cTT;
};

class StxCApar {
 public:
  StxNodePars pars;
  StxNodeErrs errs;
  Float_t chi2;
  Int_t   NDF;
};
class StxCAInterface : public StTPCCAInterface {
 public:
  StxCAInterface();
  virtual ~StxCAInterface() {fgStxCAInterface = 0;}
  static StxCAInterface &Instance();
  virtual void SetNewEvent() {fSeedHits.clear(); StTPCCAInterface::SetNewEvent();}
  virtual vector<SeedHit_t>        &GetSeedHits()    { return fSeedHits;}
#ifndef __CINT__
  static  void ConvertPars(const AliHLTTPCCATrackParam& caPar, Double_t _alpha, StxCApar& stxPar); 
#else
  static  void ConvertPars(const AliHLTTPCCATrackParam* caPar, Double_t _alpha, StxCApar* stxPar); 
#endif
 protected:
  virtual void MakeHits();     // fill fCaHits & fSeedHits
  virtual void MakeSeeds(){}   // fill fSeeds & fTrackParameters

  vector<SeedHit_t>       fSeedHits;          // hits to make seeds
  static StxCAInterface  *fgStxCAInterface;
};
#endif //  __StxCAInterface_h__
