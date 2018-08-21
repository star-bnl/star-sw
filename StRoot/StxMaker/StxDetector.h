#ifndef STI_DETECTOR_H
#define STI_DETECTOR_H
#include <vector>
#include <math.h>
#include "Stiostream.h"
#include "StxIsActiveFunctor.h"
#include "StDetectorDbMaker/StiTrackingParameters.h"
#include "TGeoPhysicalNode.h"
#include "TGeoMatrix.h"
#include "TNamed.h"
class StxDetectorNode;
class StiHitErrorCalculator;
enum StxShapeCode {kPlanar = 1, kCylindrical, kConical, kDisk}; 
class StxDetector : public TNamed {
 public:
  enum StxRegion {kBackwardRapidity, kMidRapidity, kForwardRapidity, kUndefined};
  friend class StxHit;
  // con/destructor
  StxDetector() : TNamed("","") {Reset();}
  virtual ~StxDetector() {}
  void Reset() {SetName(""); memset(mBeg,0,mEnd-mBeg+1); _key0 = _key1 = _key2 = -1;}
  void Unset(){;}
    
  // accessors
  Bool_t IsActive(Double_t dYlocal, Double_t dZlocal) const {return (*_isActiveFunctor)(dYlocal, dZlocal);}
  Bool_t IsActive()                               const {return _isActiveFunctor->IsActive();}
  Float_t      NormalRadius()     const { return normalRadius;     }
  StxIsActiveFunctor* IsActiveFunctor() {return _isActiveFunctor;}

  // mutators
  void SetIsActive(StxIsActiveFunctor *val){ _isActiveFunctor = val; }
  void SetPhysicalNode(TGeoPhysicalNode *nodeP);
  TGeoPhysicalNode *GetPhysicalNode() const {return _nodeP;}
  TGeoHMatrix      *GetMatrix() const {return _rotm;}
  //This is a bit of a hack, but we leave ourselves a reverse connection between
  // a detector and the tree node that it's stored on.
  void SetTreeNode( StxDetectorNode * val) {mNode=val;}
  StxDetectorNode * TreeNode() const {return mNode;}
    
  void SetHitErrorCalculator(const StiHitErrorCalculator * calculator) {_hitErrorCalculator = calculator;}
  const StiHitErrorCalculator * HitErrorCalculator() const {return _hitErrorCalculator;}

  void  SetGroupId(Int_t id) {  _groupId = id;}
  Int_t GroupId() const   {return _groupId;}
  void SetTrackingParameters(const StiTrackingParameters * pars) {_pars = pars;}
  const StiTrackingParameters * TrackingParameters() const {return _pars;}
  friend ostream& operator<<(ostream&os, const StxDetector & det);
  
  void SetKey(Int_t index,Int_t value)  {
    switch (index) {
    case 0: _key0 = value; break;
    case 1: _key1 = value; break;
    case 2: _key2 = value; break;
    default: assert(0);
    }
  }

  Int_t Key(Int_t index) const  {
    switch (index) {
    case 0: return _key0;
    case 1: return _key1;
    case 2: return _key2;
    default: return -1;
    }
  }
 protected:
  Char_t mBeg[1];
  /// Toggle switch determining whether this detector is to be added to the detector tree.
  /// The detector is added if the switch is "true"
  Bool_t on;    
  /// Functor used to calculate whether the posistion reached by a track is 
  /// to be considered within the active area of the detector, and
  /// is thus susceptible of providing hit information.
  StxIsActiveFunctor *_isActiveFunctor; 
  const StiHitErrorCalculator * _hitErrorCalculator;
  StxDetectorNode  * mNode;
#if 1
  /// Convenience storage of cos(refAngle) 
  Double_t _cos;
  /// Convenience storage of sin(refAngle)
  Double_t _sin;
#endif
  /// Detector group identifier.
  Int_t _groupId;
  const StiTrackingParameters * _pars;
  Int_t _key0, _key1, _key2;
  TGeoPhysicalNode *_nodeP;
  TGeoHMatrix      *_rotm;
  Float_t normalRadius;   // >= 0
  Char_t mEnd[1];
};
#endif
