#ifndef StiNode_H
#define StiNode_H 1
#define STI_NODE_DEBUG

#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "Sti/StiNodePars.h"
#include "StEvent/StEnumerations.h"

class StiHit;
class StHitPlane;

  
  
/*! \class StiNode
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
class StiNode 
{
public:
enum ENodeType {kRegNode=0,kDcaNode=1,kPrimNode=2};

public:
  StiNode(){;}
  StiNode(const StiNode &node);
  virtual ~StiNode(){mId=-1;};
  /// Resets the node to a "null" un-used state
  void reset();
  void unset(){;}
  const StiNodePars &GetFP() const {return mFP[2];}
  const StiFitErrs  &GetFE() const {return mFE[2];}

  StiNodePars &GetPP(int dir) 	{return mPP[dir];}
  StiFitErrs  &GetPE(int dir) 	{return mPE[dir];}
  StiNodePars &GetFP(int dir) 	{return mFP[dir];}
  StiFitErrs  &GetFE(int dir) 	{return mFE[dir];}
  StiHitErrs  &GetHE() 		{return mHrr;}

  /// Extract state information from this node in TPT representation.
  void GetGlobalTpt   (float   x[6],float   e[15]);

  /// Calculates and returns the momentum and error of the track at this node. The momentum is 
  /// in the local reference frame of this node.
  void GetMomentum(double p[3], double e[6]=0) const;
  /// Calculates and returns the Z mag field in the current point.
  /// units: PGeV = Hz*Radcurv_in_CM
  double GetHz() const;
  const double *x_g() const;
  double GetTime() const;

        StiHit *GetHit() const 			{ return mHit;}
  void  SetHit(StiHit *hit) 			{ mHit=hit   ;}
  const StHitPlane *GetHitPlane() const 	{ return mHitPlane  ;}
  void  SetHitPlane(const StHitPlane *hitPlane){ mHitPlane=hitPlane;}
  
 double GetXi2() const 				{ return mXi2;}
   void SetXi2(double Xi2) 			{ mXi2=Xi2   ;}
   void SetPre(StiNodePars &par,StiFitErrs &err,int dir); 	
   void SetFit(StiNodePars &par,StiFitErrs &err,int dir); 
   void SetDer(Mtx55D_t &der, int dir) 	{Copy(mDer[dir],der);}

 StiNode::ENodeType GetType() const 			{return (StiNode::ENodeType)mType;}
               void SetType(StiNode::ENodeType ty) 	{mType =(char)ty;}
StDetectorId GetDetId() const;  
void Print(const char *opt) const;
 private:   

 public:   

 char mBeg[1];  
 char mType; 			//0=regular,1=dca,2=primary
 char mIsFit[3];
const StHitPlane *mHitPlane;
StiHit *mHit;

///  Z mag field in units PGev = Hz*Rcm
  mutable double mHz;
  StiNodePars mPP[2]; 	// Predicted Parameters
  StiFitErrs  mPE[2];	// Predicted errors
  StiNodePars mFP[3];   // Fitted    Parameters
  StiFitErrs  mFE[3];	// Fitted    errors
  Mtx55D_t    mDer[2];
  StiHitErrs  mHrr;    	// Hit errors in DCA frame
  double      mXi2; 	// Xi2 of fit to hit
  char   mEnd[1];
public:
  int mId;  //for debug only 
};

#endif

