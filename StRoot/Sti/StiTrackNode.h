#ifndef StiTrackNode_H
#define StiTrackNode_H 1

#include <Stiostream.h>
#include <stdlib.h>

#include "StiTreeNode.h"
#include "StiHit.h"
#include "StiNodePars.h"
class StiDetector;

enum eTkPars {kNPars=6,kNErrs=21};
class StiNodeMtx {
public:	
  void reset(){memset(this,0,sizeof(StiNodeMtx));}
  double A[kNPars][kNPars];
};



class StiNodeErrs {
 public:	
  enum {kBigLen = 20};


  void reset()				{memset(this,0,sizeof(StiNodeErrs));}
  double getDelta()  const 		{return sqrt(cXX()+cYY()+cZZ());}
  double getDelta2() const 		{return     (cXX()+cYY()+cZZ());}
  StiNodeErrs &operator*=(double f) 	{for (int i=0;i<kNErrs;i++){A[i]*=f;}; return *this;}
  StiNodeErrs &merge(double wt,StiNodeErrs &other);
  void rotate(double alpha,const StiNodePars &pars );
  int nan() const;
  
  void get00(      double *a) const;
  void set00(const double *a)      ;
  void get10(      double *a) const;
  void set10(const double *a)      ;
  void get11(      double *a) const;
  void set11(const double *a)      ;

  void recov(int force=0); 
  int  check(const char *pri=0) const; 
  double sign() const; 
  double zign() const; 
  double operator()(int i,int j) const;
  void zeroX();
  void print() const;
  
 public:	
  enum {kXX=0,
	kYX, kYY,
	kZX, kZY,  kZZ,                 
	kEX, kEY,  kEZ,  kEE,           
	kPX, kPY,  kPZ,  kPE,  kPP,     
	kTX, kTY,  kTZ,  kTE,  kTP,  kTT, kToT};
  double A[kToT];
  double  cXX() const {return A[kXX];}
  double &cXX()       {return A[kXX];}
  double  cYX() const {return A[kYX];}
  double &cYX()       {return A[kYX];}
  double  cYY() const {return A[kYY];}
  double &cYY()       {return A[kYY];}
  double  cZX() const {return A[kZX];}
  double &cZX()       {return A[kZX];}
  double  cZY() const {return A[kZY];}
  double &cZY()       {return A[kZY];}
  double  cZZ() const {return A[kZZ];}
  double &cZZ()       {return A[kZZ];}
  double  cEX() const {return A[kEX];}
  double &cEX()       {return A[kEX];}
  double  cEY() const {return A[kEY];}
  double &cEY()       {return A[kEY];}
  double  cEZ() const {return A[kEZ];}
  double &cEZ()       {return A[kEZ];}
  double  cEE() const {return A[kEE];}
  double &cEE()       {return A[kEE];}
  double  cPX() const {return A[kPX];}
  double &cPX()       {return A[kPX];}
  double  cPY() const {return A[kPY];}
  double &cPY()       {return A[kPY];}
  double  cPZ() const {return A[kPZ];}
  double &cPZ()       {return A[kPZ];}
  double  cPE() const {return A[kPE];}
  double &cPE()       {return A[kPE];}
  double  cPP() const {return A[kPP];}
  double &cPP()       {return A[kPP];}
  double  cTX() const {return A[kTX];}
  double &cTX()       {return A[kTX];}
  double  cTY() const {return A[kTY];}
  double &cTY()       {return A[kTY];}
  double  cTZ() const {return A[kTZ];}
  double &cTZ()       {return A[kTZ];}
  double  cTE() const {return A[kTE];}
  double &cTE()       {return A[kTE];}
  double  cTP() const {return A[kTP];}
  double &cTP()       {return A[kTP];}
  double  cTT() const {return A[kTT];}
  double &cTT()       {return A[kTT];}
};  

class StiHitErrs{
 public:
  void reset()			 {memset(this,0,sizeof(*this));}
  StiHitErrs &operator*=(double f) {for (int i=0;i<6;i++){A[i]*=f;};return *this;}
  void rotate(double angle);
  enum {kXX, 
	kYX, kYY, 
	kZX, kZY, kZZ};
  double A[6];
  double &hXX()       {return A[kXX];}
  double  hYX() const {return A[kYX];}
  double &hYX()       {return A[kYX];}
  double  hYY() const {return A[kYY];}
  double &hYY()       {return A[kYY];}
  double  hZX() const {return A[kZX];}
  double &hZX()       {return A[kZX];}
  double  hZY() const {return A[kZY];}
  double &hZY()       {return A[kZY];}
  double  hZZ() const {return A[kZZ];}
  double &hZZ()       {return A[kZZ];}
  
  /*! \class StiNode2pars
    Axiliary class for StiKalmanTrackNode only. 
    Contains only  Node_Y and Node_Z in local frame
    with error matrix. Currently used to keep NOT FITTED to this hit values.
    So difference with hit is not a residuals. Could be used for allignment
    or hit errors fiting 
    \author Victor Perev
  */
};
class StiNode2Pars
{
 public:
  void set(const StiNodePars &pars,StiNodeErrs &errs);
 public:
  float mPar[2];  // Node_Y , Node_Z 
  float mErr[3];  // yy,yz,zz
};


/*! \class StiHitContino
  Axiliary class for StiKalmanTrackNode only. Small container
  of about 3 best hits for this node. Used for refit.
  \author Victor Perev
*/

class StiHitContino {
 public:
  StiHitContino()			{reset();}
  void   reset(); 		
  StiHit *getHit (int idx) const	{return mHits[idx];}
  int    getNHits () const;		
  double getChi2 (int idx=0) const	{return mChi2[idx];}
  double getDetr (int idx=0) const	{return mDetr[idx];}
  void   add (StiHit *hit,double chi2,double detr=1.);
  void   print (const char* opt="") const;
 private:
  enum {kMaxSize=10};
  StiHit *mHits[kMaxSize+1];
  double  mChi2[kMaxSize+1];
  double  mDetr[kMaxSize+1];
};



class StiTrackNode : public StiTreeNode
{ 
 public:
  enum eTrackNodeStatus {
    kTNReset 	= 0, // TrackNode after reset
    kTNRotBeg 	= 1, // TrackNode start rotation
    kTNRotEnd 	= 2, // TrackNode rotated, errmatrix 3d
    kTNProBeg     = 3, // TrackNode start propagation, errmatrix wrong
    kTNProEnd     = 4, // TrackNode end propagation, errmatrix still wrong
    kTNNudBeg     = 5, // TrackNode start nudge
    kTNFitBeg 	= 6, // TrackNode start fit
    
    kTNReady	=10, // TrackNode propagateError called,errmatrix OK
    kTNNudEnd     =11, // TrackNode end nudge
    kTNFitEnd 	=12, // TrackNode end fit
    kTNInit 	=13}; // TrackNode initialized
  enum eTrackNodeFlags {
  kTNRefit 	= 1 // TrackNode in refit stage
  };
  
  virtual ~StiTrackNode(){};    
  virtual double getPt() const=0;
  const StiTrackNode& operator=(const StiTrackNode& node);  
  void reset();
  void unset(){;}
  
  virtual double x_g()  const	=0;
  virtual double y_g()  const	=0;
  virtual double z_g()  const	=0;
  virtual double x()    const	=0;			
  virtual double y()    const	=0;			 
  virtual double z()    const	=0;			
  
  StiHit * getHit() const 		{return _hit;}
  void  setHit(StiHit* hit)		{_hit   =hit;}
  const StiDetector *getDetector() const; 
  void  setDetector(const StiDetector *detector);
  double getChi2 ()  const		{return _chi2;} 		
  double getDeterm() const		{return _det ;} 		
  void  setChi2(double chi2)		{_chi2  =chi2;}
  int   getState() const 		{return _state;}
  void  setInvalid()  			{ _state=0;}
  void  setReady()  			{ _state=kTNReady;}
  int isValid()  const 			{return _state>=kTNReady;}
  int isFitted() const 			{return (_hit && _chi2<1e3);}
  double getRefPosition() const;
  double getLayerAngle()  const;
 public:   
  static void mult6(double Rot[kNPars][kNPars],const double Pro[kNPars][kNPars]); 
  static void errPropag6(double G[21],const double F[6][6],int nF);
  static  int cylCross(const double Xp[2],const double Dp[2], const double Rho, const double r
		       ,         int dir,        double out[2][3]);
  
  static double  sinX(double x);  // (sin(x)-x)/x**3
  
  StiTrackNode()			{reset();}    
  int _state;
  const StiDetector * _detector; 
  StiHit* _hit;
  double _det;
  double _chi2;
 public:
  static int mgFlag;
};


inline void StiTrackNode::reset()
{ 
   StiTreeNode::reset();
  _state = kTNReset;
  _detector = 0;
  _hit=0;
  _chi2 = 1e60;
}


inline const StiDetector * StiTrackNode::getDetector() const
{
  return _detector;
}

inline void StiTrackNode::setDetector(const StiDetector *detector)
{
  _detector = detector; 
}
inline  const StiTrackNode& StiTrackNode::operator=(const StiTrackNode& node)  
{
   //not called StiTreeNode::operator=();    //Relationship is not changed
  _state    = node._state;	
  _detector = node._detector;	
  _hit      = node._hit;
  _chi2     = node._chi2;
  return *this;
}	
	
#endif



