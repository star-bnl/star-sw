#ifndef StiTrackNode_H
#define StiTrackNode_H 1

#include <Stiostream.h>
#include <stdlib.h>

#include "StiDefaultMutableTreeNode.h"
#include "StiHit.h"
class StiDetector;
/*! \class StiHitContino
  Axiliary class for StiKalmanTrackNode only. Small container
  of about 3 best hits for this node. Used for refit.
  \author Victor Perev
*/

class StiHitContino {
public:
  StiHitContino()			{reset();}
  void   reset(); 		
  StiHit *getHit (int idx) const		{return (idx<kMaxSize)?mHits[idx]:0;}
  int    getNHits () const;		
  void   setHit (StiHit *hit) 		{mHits[0]=hit;}
  double getChi2 (int idx=0) const	{return mChi2[idx];}
  void   setChi2 (double chi2) 		{mChi2[0]=chi2;}
  void   add (StiHit *hit,double chi2);
  void   print (const char* opt="") const;
private:
  enum {kMaxSize=3};
  StiHit *mHits[kMaxSize];
  double  mChi2[kMaxSize];
};



enum eTkPars {kNPars=6,kNErrs=21};
class StiTrackNode : public StiDefaultMutableTreeNode
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

  ~StiTrackNode(){};    
  const StiTrackNode& operator=(const StiTrackNode& node);  
  void reset();
  StiHit * getHit(int idx=0) const;
  void setHit(StiHit* hit);
  void add(StiTrackNode * node);
  const StiDetector *getDetector() const; 
  void setDetector(const StiDetector *detector);
  double getChi2 () const; 		
  void setChi2(double chi2);
  StiHitContino &hits() 		{return _hits;}
  int getState() const 			{return _state;}
  int isValid()  const 			{return _state>=kTNReady;}
protected:   
static void errPropag6(double G[21],const double F[6][6],int nF);
static int  cylCross(double r, const double dx[4],double Rho,double out[4]);

  StiTrackNode()			{reset();}    
  int _state;
  const StiDetector * _detector; 
  StiHitContino _hits;
};


inline void StiTrackNode::reset()
{ 
  StiDefaultMutableTreeNode::reset();
  _state = kTNReset;
  _detector = 0;
  _hits.reset();
}

inline void StiTrackNode::add(StiTrackNode * node)
{	
  node->setParent(this);
  children.push_back(node);
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
//VP  StiDefaultMutableTreeNode::operator=(node);
  _state    = node._state;	
  _detector = node._detector;	
  _hits     = node._hits;
  return *this;
}	
	
#endif



