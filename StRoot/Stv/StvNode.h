#ifndef StvNode_H
#define StvNode_H 1
#define STI_NODE_DEBUG

#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StvUtil/StvNodePars.h"
#include "StEvent/StEnumerations.h"

class StvHit;
class StHitPlane;

  
  
/*! \class StvNode
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
class StvNode 
{
public:
enum ENodeType {kRegNode=0,kDcaNode=1,kPrimNode=2};

public:
  StvNode(){;}
  StvNode(const StvNode &node);
  virtual ~StvNode(){mId=-1;};
  /// Resets the node to a "null" un-used state
  void reset();
  void unset(){;}
  const StvNodePars &GetFP() const {return mFP[2];}
  const StvFitErrs  &GetFE() const {return mFE[2];}

  StvNodePars &GetPP(int dir) 	{return mPP[dir];}
  StvFitErrs  &GetPE(int dir) 	{return mPE[dir];}
  StvNodePars &GetFP(int dir) 	{return mFP[dir];}
  StvFitErrs  &GetFE(int dir) 	{return mFE[dir];}
  const double *GetHE() const	{return mHrr    ;}
        void  SetHE(const double he[3]) 	
	                        {mHrr[0]=he[0]; mHrr[1]=he[1];mHrr[2]=he[2];}

  /// Extract state information from this node in TPT representation.
  void GetGlobalTpt   (float   x[6],float   e[15]);

  /// Calculates and returns the momentum and error of the track at this node. The momentum is 
  /// in the local reference frame of this node.
  void GetMomentum(double p[3], double e[6]=0) const;
  /// Calculates and returns the Z mag field in the current point.
  /// units: PGeV = Hz*Radcurv_in_CM
  double GetHz() const;
  const double *x() const;
  double GetTime() const;

        StvHit *GetHit() const 			{ return mHit;}
  void  SetHit(StvHit *hit) 			{ mHit=hit   ;}
  const StHitPlane *GetHitPlane() const 	{ return mHitPlane  ;}
  void  SetHitPlane(const StHitPlane *hitPlane) { mHitPlane=hitPlane;}
  void  SetELoss(const StvELossData &el,int   ) {mELossData=el;}  
const StvELossData &GetELoss() const		{return mELossData;}  
  
 double GetXi2() const 				{ return mXi2;}
 double GetLen() const 				{ return mLen;}
   void SetXi2(double Xi2) 			{ mXi2=Xi2   ;}
   void SetPre(StvNodePars &par,StvFitErrs &err,int dir); 	
   void SetFit(StvNodePars &par,StvFitErrs &err,int dir); 
   void SetDer(const Mtx55D_t &der, int dir);

 StvNode::ENodeType GetType() const 			{return (StvNode::ENodeType)mType;}
               void SetType(StvNode::ENodeType ty) 	{mType =(char)ty;}
StDetectorId GetDetId() const;  
void Print(const char *opt) const;
 private:   

 public:   

 char mBeg[1];  
 char mType; 			//0=regular,1=dca,2=primary
const StHitPlane *mHitPlane;
StvHit *mHit;

///  Z mag field in units PGev = Hz*Rcm
  mutable double mHz;
  StvNodePars mPP[2]; 	// Predicted Parameters
  StvFitErrs  mPE[2];	// Predicted errors
  StvNodePars mFP[3];   // Fitted    Parameters
  StvFitErrs  mFE[3];	// Fitted    errors
  Mtx55D_t    mDer[2];
  double      mHrr[3];  // Hit errors in DCA frame
  double      mXi2; 	// Xi2 of fit to hit
  double      mLen; 	// Length
  StvELossData mELossData; //EnergyLoss&MCS from the upper node 
  char   mEnd[1];
public:
  int mId;  //for debug only 
};

#endif

