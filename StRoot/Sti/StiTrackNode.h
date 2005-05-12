#ifndef StiTrackNode_H
#define StiTrackNode_H 1

#include <Stiostream.h>
#include <stdlib.h>

#include "StiTreeNode.h"
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
  StiHit *getHit (int idx) const	{return mHits[idx];}
  int    getNHits () const;		
  double getChi2 (int idx=0) const	{return mChi2[idx];}
  void   add (StiHit *hit,double chi2);
  void   print (const char* opt="") const;
private:
  enum {kMaxSize=10};
  StiHit *mHits[kMaxSize+1];
  double  mChi2[kMaxSize+1];
};



enum eTkPars {kNPars=6,kNErrs=21};
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

  virtual ~StiTrackNode(){};    
  const StiTrackNode& operator=(const StiTrackNode& node);  
  void reset();
  StiHit * getHit() const 		{return _hit;}
  void setHit(StiHit* hit)		{_hit   =hit;}
  const StiDetector *getDetector() const; 
  void  setDetector(const StiDetector *detector);
  double getChi2 () const		{return _chi2;} 		
  double getDet () const		{return _det ;} 		
  void setChi2(double chi2)		{_chi2  =chi2;}
  int getState() const 			{return _state;}
 void setReady()  			{ _state=kTNReady;}
  int isValid()  const 			{return _state>=kTNReady;}
protected:   
static void errPropag6(double G[21],const double F[6][6],int nF);
static int  cylCross(double r, const double dx[4],double Rho,double out[4]);
static double  sinX(double x);  // (sin(x)-x)/x**3

  StiTrackNode()			{reset();}    
  int _state;
  const StiDetector * _detector; 
  StiHit* _hit;
  double _det;
  double _chi2;
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



