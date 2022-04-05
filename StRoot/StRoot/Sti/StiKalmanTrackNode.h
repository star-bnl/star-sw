#ifndef StiKalmanTrackNode_H
#define StiKalmanTrackNode_H 1
#define STI_NODE_DEBUG

#include <Stiostream.h>
#include <stdlib.h>
#include <stdexcept>
#include <math.h>
#include "StiTrackNode.h"
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StDetectorDbMaker/StiKalmanTrackFinderParameters.h"
#include "StiShape.h"
#include "StiPlanarShape.h"
#include "StiCylindricalShape.h"
#include "StiPlacement.h"
#include "StiHit.h"
#include "StiMaterial.h"
class StiDetector;
class StiMaterial;
//class StiElossCalculator;
  
typedef enum {
  kFailed = -1,         // could not find intersection
  kHit,                                
  kEdgePhiPlus, kEdgeZminus, kEdgePhiMinus, kEdgeZplus, 
  kMissPhiPlus, kMissZminus, kMissPhiMinus, kMissZplus,
  kEnded
} StiIntersection;
  
class StiNodeStat {
public:	
  StiNodeStat(){reset();}
void reset(){memset(this,0,sizeof(StiNodeStat));}
  double x1;		// local x position of current node
  double y1;		// local y position of current node
  double cosCA1,sinCA1;	// cos & sin of local pfi angle
  double x2,y2;		// x & y of track in the next node but in current local
  double cosCA2,sinCA2; // cos & sin in this point
  double dx, dy;	// x2-x1 & y2-y1
  double dl0;		// linear distance
  double dl;		// curved distance
  double sumSin, sumCos;
};

class StiNodeExt {
public:
void reset(){mPP.reset();mPE.reset();mMtx.reset();}
void unset(){;}
public:
  StiNodePars mPP; 	//Predicted params
  StiNodeMtx  mMtx;
  StiNodeErrs mPE;	//Predicted params errs
};

class StiNodeInf {
public:
void reset(){mPP.reset();mPE.reset();mHrr.reset();}
void unset(){;}
public:
  StiNodePars mPP; 
  StiNodeErrs mPE;
  StiHitErrs  mHrr;
};





/*! \class StiKalmanTrackNode
  Work class used to handle Kalman filter information while
  constructing track nodes.  A node may or may not own a hit
  depending whether it lies on a measurement layer where a hit
  was found. A node can have 0, 1, or many children. 
  Nodes are nominally sequenced outside-in i.e. with decreasing 
  radius (or independent variable). The order can however be reversed.
  In anycase, the order should always be monotonically increasing 
  or decreasing.
  \author Claude A Pruneau
*/
class StiKalmanTrackNode : public StiTrackNode 
{
friend class StiTrackNodeHelper;
public:
  StiKalmanTrackNode(){_inf=0; _ext=0; reset();}
  StiKalmanTrackNode(const StiKalmanTrackNode &node);
  virtual ~StiKalmanTrackNode(){reduce();mId=-1;};
  const StiKalmanTrackNode& operator=(const StiKalmanTrackNode &node);  
  
  static double mcs2(double relRadThickness, double beta2, double p2) {return 14.1*14.1*relRadThickness/(beta2*p2*1e6);}
  /// Resets the node to a "null" un-used state
  void reset();
  void unset();
  /// Resets errors for refit
  void resetError(double fak=0);
  /// Initialize this node with the given hit information
  void initialize(StiHit*h);
  void initialize(StiDetector *d);
  void initialize(const double dir[3]); //dir[0]**2+dir[1]**2 ==1
//void initialize(StiHit*h,double alpha, double eta, double curvature, double tanl);
  
  /// Sets the Kalman state of this node equal to that of the given node. 
  void setState(const StiKalmanTrackNode * node);
  /// Extract state information from this node.
  void get(double& alpha, double& xRef, double x[kNPars], double cc[kNErrs], double& chi2);

  /// Extract state information from this node in Radial representation.
  void getGlobalRadial(double  x[6],double  e[15]);

  /// Extract state information from this node in TPT representation.
  void getGlobalTpt   (float   x[6],float   e[15]);

  /// Get the charge (sign) of the track at this node
    int getCharge() const {return (mFP.ptin() > 0) ? -1 : 1;}
  /// Convenience Method that returns the track momentum at this node
  StThreeVectorF getMomentumF() const;
  /// Convenience Method that returns the track momentum at this node
  /// in global coordinates.
  StThreeVectorF getGlobalMomentumF() const;
  StThreeVector<double> getMomentum() const;
  StThreeVector<double> getGlobalMomentum() const;
  /// Calculates and returns the momentum and error of the track at this node. The momentum is 
  /// in the local reference frame of this node.
  void getMomentum(double p[3], double e[6]=0) const;
  /// Calculates and returns the tangent of the track pitch angle at this node.
    double getCurvature() const {return mFP.curv();}
    void setCurvature(double curvature) {mFP.curv()=curvature;}
  double getDipAngle() const {return atan(mFP.tanl());}
  double getTanL() const {return mFP.tanl();}
  /// Calculates and returns the transverse momentum of the track at this node.
    double getPt() const;
  /// Calculates and returns the momentum of the track at this node.
    double getP() const {return (getPt()*::sqrt(1.+mFP.tanl()*mFP.tanl()));}
  /// Calculates and returns the Z mag field in the current point.
  /// units: PGeV = Hz*Radcurv_in_CM
  double getHz() const;
  double getField() const {return getHz();}
  double x_g() const;
  double y_g() const;
  double z_g() const;
  double getX() const 			{ return mFP.x();}
  double getY() const 			{ return mFP.y();}  
  double getZ() const 			{ return mFP.z();}
  double x() const 			{ return mFP.x();}
  double y() const 			{ return mFP.y();}  
  double z() const 			{ return mFP.z();}
  double getRxy() const 		{ return sqrt(mFP.x()*mFP.x()+mFP.y()*mFP.y());}
  
  double getEta  () const 		{return mFP.eta();  }
  double getSin  () const 		{return mFP._sinCA;}
  double getCos  () const 		{return mFP._cosCA;}
  double getAlpha() const 		{return _alpha;  }
  const double *hitErrs() const         {return mHrr.G();  }
  double getEyy()   const 		{return mHrr.hYY;}
  double getEzz()   const 		{return mHrr.hZZ;}
  double getCyy()   const 		{return mFE._cYY;}
  double getCzz()   const 		{return mFE._cZZ;}
  double const *getPars()const          {return mFP.P;}
  int    getHitCount () const		{return hitCount;}
  int    getNullCount() const       	{return nullCount;}
  int    getContigHitCount () const 	{return contiguousHitCount ;}
  int    getContigNullCount() const 	{return contiguousNullCount;}
  //const char  &getHitCount () const 		{return hitCount;}
  //const char  &getNullCount()  const		{return nullCount;}
  //const char  &getContigHitCount () const 	{return contiguousHitCount ;}
  //const char  &getContigNullCount() const 	{return contiguousNullCount;}
  int incHitCount () 			{return ++hitCount;}
  int incNullCount() 			{return ++nullCount;}
  int incContigHitCount () 		{return ++contiguousHitCount ;}
  int incContigNullCount() 		{return ++contiguousNullCount;}
  void setHitCount (char c=0)      	{ hitCount=c;}
  void setNullCount(char c=0)       	{ nullCount=c;}
  void setContigHitCount (char c=0) 	{ contiguousHitCount=c ;}
  void setContigNullCount(char c=0) 	{ contiguousNullCount=c;}
  double getTime() const;

  void   setHitCand(int nhits)		{mHitCand = nhits;}
  void   setIHitCand(int ihit)		{mIHitCand = ihit;}
  int    getHitCand()const		{return mHitCand;}
  int    getIHitCand()const		{return mIHitCand;}
      StiELoss *getELoss()  		{return mELoss;}
const StiELoss *getELoss()const		{return mELoss;}
  static void PrintStep();
  StThreeVector<double>getPoint() const;
  StThreeVector<double>getGlobalPoint() const;
  /// Calculates and returns the momentum and error of the track at this node in global coordinates.
  void getGlobalMomentum(double p[3], double e[6]=0) const;
  int  isEnded() const;	
  int  isDca()   const;	
  
  /// Propagates a track encapsulated by the given node "p" to the given detector "tDet".
  int  propagate(StiKalmanTrackNode *p, const StiDetector * tDet, int dir);	//throw (Exception);
  
  /// Propagates a track encapsulated by the given node "p" to the given vertex
  bool propagate(const StiKalmanTrackNode *p, StiHit * vertex, int dir);

  bool propagateToBeam(const StiKalmanTrackNode *p, int dir);
  int  propagateToRadius(StiKalmanTrackNode *pNode, double radius,int dir);
  int  locate();
  int  propagate(double x,int option,int dir);
  void propagateMtx();
  void propagateError();
  void saveInfo(int kase=1);
const StiNodeInf *getInfo() const 	{return _inf;}
  int  testError(double *emx,int begend);
  void numeDeriv(double val,int kind,int shape=0,int dir=0);
  int  testDeriv(double *der);
  void propagateMCS(StiKalmanTrackNode * previousNode, const StiDetector * tDet);
  int nudge(StiHit *hit=0);
  double evaluateChi2    (const StiHit *hit); 
  double evaluateChi2Info(const StiHit *hit) const; 
  int updateNode(); 
  int rotate(double alpha); 
  int    getHelicity()const {return (mFP.curv() < 0) ? -1 : 1;}
  double getPhase()   const;
  double getPsi()     const;
  double getWindowY();
  double getWindowZ();
  double pitchAngle() const {return atan(mFP.tanl());}
  double crossAngle() const {return asin(mFP._sinCA);}
  double sinCrossAngle() const {return mFP._sinCA;}
  double pathlength() const;
  double pathLToNode(const StiKalmanTrackNode * const oNode);
  StThreeVectorD* getLengths(StiKalmanTrackNode *nextNode);

  double length(const StThreeVector<double>& delta, double curv);
  double getDedx() const;
  static double nice(double angle);
  /// Return center of helix circle in global coordinates
  StThreeVector<double> getHelixCenter() const;
  void setHitErrors(const StiHit *hit=0);
  StiHitErrs getGlobalHitErrs(const StiHit *hit) const;
  friend ostream& operator<<(ostream& os, const StiKalmanTrackNode& n);

  double getX0() const;
  double getGasX0() const;
  double getDensity() const;
  double getGasDensity() const;

  void   extend();
  void   reduce();
  static Int_t  debug()           {return _debug;}
  static void   setDebug(Int_t m) {_debug = m;}
  static void   SetLaser(Int_t m) {_laser = m;}
  static Int_t  IsLaser()         {return _laser;}
  void   PrintpT(const Char_t *opt="") const ;
  int    getFlipFlop() const 			{return mFlipFlop;}
  static void   ResetComment(const Char_t *m = "") 	{comment = m; commentdEdx = "";}
  static const Char_t *Comment() 		{return comment.Data();}
  /// rotation angle of local coordinates wrt global coordinates
  int   print(const char *opt) const;
  
 private:   
  void   extinf();				//add inf block
  void static saveStatics(double *sav);
  void static backStatics(double *sav);
  static StiNodeExt *nodeExtInstance();
  static StiNodeInf *nodeInfInstance();
  void propagateCurv(const StiKalmanTrackNode *parent);

//  Extended members 
 public:
 
  const StiNodePars &fitPars() const                  {return mFP; } 
        StiNodePars &fitPars()                        {return mFP; } 
  const StiNodeErrs &fitErrs() const                  {return mFE; } 
        StiNodeErrs &fitErrs()                        {return mFE; } 
  const StiNodePars &mPP() const                      {return _ext->mPP; } 
        StiNodePars &mPP()       {if (!_ext) extend(); return _ext->mPP; } 
  const StiNodeErrs &mPE() const                      {return _ext->mPE; } 
        StiNodeErrs &mPE()       {if (!_ext) extend(); return _ext->mPE; } 
  const StiNodeMtx  &mMtx()const                      {return _ext->mMtx;} 
        StiNodeMtx  &mMtx()      {if (!_ext) extend();return _ext->mMtx;} 
  const StiNode2Pars &unTouched() const               {return  mUnTouch;} 
        void         setUntouched();
 protected:   

  char _beg[1];  
  double _alpha;
///  Z mag field in units PGev = Hz*Rcm
  mutable double mHz;
  StiNodePars mFP; 
  /// covariance matrix of the track parameters
  StiNodeErrs  mFE;
  StiNode2Pars mUnTouch;
  StiHitErrs   mHrr;
  StiELoss mELoss[2];
  char hitCount;
  char nullCount;
  char contiguousHitCount;
  char contiguousNullCount;
  char mFlipFlop;
  char mHitCand;
  char mIHitCand;
  char   _end[1];
  StiNodeExt *_ext;
  StiNodeInf *_inf;
  static StiNodeStat  mgP;
  static bool   useCalculatedHitError;
//  debug variables
  static int    fDerivTestOn;   
  static double fDerivTest[kNPars][kNPars];   
  static int   _debug;
  static TString comment;
  static TString commentdEdx;
  static int   _laser;
public:
  int mId;  //for debug only 
};


inline double StiKalmanTrackNode::nice(double angle)
{ 
  if (angle <= -M_PI) angle += 2*M_PI;
  if (angle >   M_PI) angle -= 2*M_PI;
  return angle;
}

inline StThreeVector<double> StiKalmanTrackNode::getMomentum() const
{
  double pt = getPt();
  return StThreeVector<double>(pt*mFP._cosCA,pt*mFP._sinCA,pt*mFP.tanl());
}

inline StThreeVectorF StiKalmanTrackNode::getMomentumF() const
{
  double pt = getPt();
  return StThreeVectorF(pt*mFP._cosCA,pt*mFP._sinCA,pt*mFP.tanl());
}

inline StThreeVector<double> StiKalmanTrackNode::getGlobalMomentum() const
{
  StThreeVector<double> p = getMomentum();
  p.rotateZ(_alpha);
  return p;
}

inline StThreeVectorF StiKalmanTrackNode::getGlobalMomentumF() const
{
  StThreeVectorF p = getMomentumF();
  p.rotateZ(_alpha);
  return p;
}


//stl helper functor

struct StiKTNXLessThan
{
    bool operator()(const StiKalmanTrackNode& lhs, const StiKalmanTrackNode& rhs) const;
};

struct StreamX 
{
  void operator()(const StiKalmanTrackNode& node) 
  {
    cout <<node.getX()<<endl;
  }
};

///Return the radiation length (in cm) of the 
///the detector volume at this node.
inline double StiKalmanTrackNode::getX0() const
{
  const StiDetector * det = getDetector();
  if (!det)
    return 0.;
  return det->getMaterial()->getX0();
}

///Return the radiation length (in cm) of the gas
///surrounding the detector volume at this node.
inline double StiKalmanTrackNode::getGasX0() const
{
  const StiDetector * det = getDetector();
  if (!det)
    return 0.;
  return det->getGas()->getX0();
}

inline double StiKalmanTrackNode::getDensity() const
{
  const StiDetector * det = getDetector();
  if (!det)
    return 0.;
  return det->getMaterial()->getDensity();
}

inline double StiKalmanTrackNode::getGasDensity() const
{
  const StiDetector * det = getDetector();
  if (!det)
    return 0.;
  return det->getGas()->getDensity();
}


inline StThreeVectorD* StiKalmanTrackNode::getLengths(StiKalmanTrackNode* nextNode)
{
  double x1=pathlength()/2.;
  double x3=nextNode->pathlength()/2.;
  double x2=pathLToNode(nextNode);
  if (x2> (x1+x3)) x2=x2-x1-x3;
  else x2=0;

  return new StThreeVectorD(x1/getX0(),
			    x2/getDetector()->getMaterial()->getX0(), 
			    x3/nextNode->getX0());
}

inline double StiKalmanTrackNode::getDedx() const
{

  StiHit *hit = getHit();
  if (!hit) return -1;
  double de=hit->getEloss();
  double dx=pathlength();
  if(dx>0 && de>0) return de/dx;
  return -1;
}

#endif

