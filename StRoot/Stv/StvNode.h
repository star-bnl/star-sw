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
class StvELossTrak;
  
  
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
friend class StvTrack;
enum ENodeType {kRegNode=0,kDcaNode=1,kPrimNode=2};

public:
  StvNode(){mBeg[0]='@';}
  StvNode(const StvNode &node);
  virtual ~StvNode(){mId=-1;};
  StvNode &operator=(const StvNode &from);
  /// Resets the node to a "null" un-used state
  void reset();
  void unset();
  const StvNodePars &GetFP() const {return mFP[2];}
  const StvFitErrs  &GetFE() const {return mFE[2];}

  StvNodePars &GetPP(int dir) 	{return mPP[dir];}
  StvFitErrs  &GetPE(int dir) 	{return mPE[dir];}
  StvNodePars &GetFP(int dir) 	{return mFP[dir];}
  StvFitErrs  &GetFE(int dir) 	{return mFE[dir];}
  const StvNodePars &GetFP(int dir) const	{return mFP[dir];}
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
  double GetHz() 		const {return mFP[2]._hz;}
  double GetTime() 		const;
     int IsFitted(int dir)	const           { return (mHit && mXi2[dir]<1000);}

        StvHit *GetHit() 	const 		{ return mHit;}
  void  SetHit(StvHit *hit); 			
  void  SetMem(StvHit *hit[2],double xi2[2]); 			
  const StHitPlane *GetHitPlane() const 	{ return mHitPlane  ;}
  void  SetHitPlane(const StHitPlane *hitPlane) { mHitPlane=hitPlane;}
  void  SetELoss(StvELossTrak *el,int  )        { mELoss=el         ;}  
  const StvELossTrak *GetELoss() const		{ return mELoss     ;}  
  
 double GetXi2(int dir=2) const 		{ return mXi2[dir]  ;}
 double GetLen() const 				{ return mLen       ;}
   void SetXi2(double Xi2,int dir=2) 		{ mXi2[dir]=Xi2; mXi2[2]=Xi2;}
   void SetPre(StvNodePars &par,StvFitErrs &err,int dir); 	
   void SetFit(StvNodePars &par,StvFitErrs &err,int dir); 
   void SetDer(const StvFitDers &der, int dir);
   void SetXDive(const double xdive[3])		{memcpy(mXDive,xdive,sizeof(mXDive));}
    int ResetELoss(const StvNodePars &pars,int dir);
 StvNode::ENodeType GetType() const 			{return (StvNode::ENodeType)mType;}
               void SetType(StvNode::ENodeType ty) 	{mType =(char)ty;}
StDetectorId GetDetId() const;  
void UpdateDca();
 int Check(const char *tit="",int dirs=3) const; 
void Print(const char *opt) const;
 private:   

 public:   

 char mBeg[1];  
 char mType; 			//0=regular,1=dca,2=primary
 double mXDive[3];		// xyz from Diver
const StHitPlane *mHitPlane;
StvHit *mHit;
StvHit *memHit[2];
 float  memXi2[2];

///  Z mag field in units PGev = Hz*Rcm
  mutable double mHz;
///  indices of arrays 0=moving in, 1=moving out,2=join result of in & out
  StvNodePars mFP[4];   // Fitted    Parameters
  StvNodePars mPP[2]; 	// Predicted Parameters+last id for helix
  StvFitErrs  mFE[4];	// Fitted    errors
  StvFitErrs  mPE[2];	// Predicted errors
  StvFitDers  mDer[2];  // Derivative matrix 0=from outer to this; 1=from this to outer
  double      mHrr[3];  // Hit errors in DCA frame
  float       mXi2[4]; 	// Xi2 of fit to hit,join,helix
  float       mLen; 	// Length
  StvELossTrak *mELoss; //EnergyLoss&MCS from the upper node 
  char   mEnd[1];
public:
  int mId;  //for debug only 
};

#endif

