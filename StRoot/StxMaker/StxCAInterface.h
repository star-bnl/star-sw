#ifndef __StxCAInterface_h__
#define __StxCAInterface_h__
#include "TPCCATracker/AliHLTTPCCAGBTracker.h"
#include "TPCCATracker/StTPCCAInterface.h"
#define kEC       2.99792458e-4
#define kZEROHZ   2e-6			//minimal/zero mag field
#define kZEROPTI  1e-3			//minimal/zero 1/pt
#define kZEROCURV (kZEROHZ*kZEROPTI) 	//minimal/zero curvature
#define kBIGPT    10                    //Pt when energy loss ignored
class StTpcHit;
struct SeedHit_t {
  Int_t padrow, status, taken, track_key ; //"m" for modified
  const StTpcHit  *hit;
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
  operator const double *() const	{return P;}
  operator       double *() 		{return P;}
  double x()    const 	{return P[kX];} 
  double y()    const 	{return P[kY];}//  local Y-coordinate of this track (reference plane)           		     
  double z()    const 	{return P[kZ];}//  local Z-coordinate of this track (reference plane)			     
  double eta()  const 	{return P[kPhi];}//  phi angle
  double phi()  const 	{return P[kPhi];}//  phi angle again
  double ptin() const 	{return P[kPtin];}//  signed invert pt [sign = sign(-qB)]					     
  double tanl() const 	{return P[kTan];}//  tangent of the track momentum dip angle
  double curv() const 	{return P[kCurv];}//  signed curvature [sign = sign(-qB)]					     
  double rxy2() const 	{return     (P[kX]*P[kX]+P[kY]*P[kY]);}
  double rxy()  const 	{return sqrt(P[kX]*P[kX]+P[kY]*P[kY]);}
  double hz()   const 	{return P[kHz];}//  Z component magnetic field in units Pt(Gev) = Hz * RCurv(cm)                  
  double &x()    	{return P[kX];}
  double &y()    	{return P[kY];}
  double &z()    	{return P[kZ];}
  double &eta()  	{return P[kPhi];} 
  double &ptin() 	{return P[kPtin];}
  double &tanl() 	{return P[kTan];}
  double &curv() 	{return P[kCurv];}
  double &hz()   	{return P[kHz];}
  double *A(Int_t i)    {return &P[i];}
  double *A()           {return P;}
  Int_t     check(const char *pri=0) const;
  void      print() const;

    /// sine and cosine of cross angle
  double _cosCA;
  double _sinCA;
  double P[8]; // array of parameters. see below
};


class StxNodeErrs {
 public:	
  enum {kBigLen = 20};
  enum eTkPars {kNPars=6,kNErrs=21};
  void reset()				{memset(this,0,sizeof(StxNodeErrs));}
  double getDelta()  const 		{return sqrt(_cXX+_cYY+_cZZ);}
  double getDelta2() const 		{return     (_cXX+_cYY+_cZZ);}
  const double* G() const { return &_cXX;} 
  double* G()       { return &_cXX;} 
  StxNodeErrs &operator*=(double f) 	{for (int i=0;i<kNErrs;i++){G()[i]*=f;}; return *this;}
  StxNodeErrs &merge(double wt,StxNodeErrs &other);
  void get00(      double *a) const;
  void set00(const double *a)      ;
  void get10(      double *a) const;
  void set10(const double *a)      ;
  void get11(      double *a) const;
  void set11(const double *a)      ;
 public:	
  double _cXX;
  double _cYX,_cYY;                       
  double _cZX,_cZY, _cZZ;                 
  double _cEX,_cEY, _cEZ, _cEE;           
  double _cPX,_cPY, _cPZ, _cPE, _cPP;     
  double _cTX,_cTY, _cTZ, _cTE, _cTP, _cTT;
};

class StxCAInterface : public StTPCCAInterface {
 public:
  StxCAInterface();
  virtual ~StxCAInterface() {fgStxCAInterface = 0;}
  static StxCAInterface &Instance();
  virtual void SetNewEvent() {fSeedHits.clear(); StTPCCAInterface::SetNewEvent();}
  virtual vector<SeedHit_t>        &GetSeedHits()    { return fSeedHits;}
  static  void ConvertPars(const AliHLTTPCCATrackParam& caPar, double _alpha, StxNodePars& nodePars, StxNodeErrs& nodeErrs); 
 protected:
  virtual void MakeHits();     // fill fCaHits & fSeedHits
  virtual void MakeSeeds(){}   // fill fSeeds & fTrackParameters

  vector<SeedHit_t>       fSeedHits;          // hits to make seeds
  static StxCAInterface  *fgStxCAInterface;
};
#endif //  __StxCAInterface_h__
