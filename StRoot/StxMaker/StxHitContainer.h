//StxHitContainer.h
//M.L. Miller (Yale Software)
//03/01

/*! \class StxHitContainer
  StxHitContainer is exactly that--a container for StxHits!  Because of the
  sheer number of hits in a STAR event, StxHitContainer is designed to
  provide efficient access to a subset of hits that are within a user
  specified volume.  This is accomplished by mapping between a two
  dimensional key and an STL container of hits.  This mapping will be
  discussed in further detail below. There is a natural connection
  between the hits and the detector from which came.  However, for
  implementation purposes, it is convenient to keep these two entities
  (HitContainer and DetectorContainer) separate, but keep a well defined
  method of communication between the two.  In such a way one can maintain a
  HitContainer and DetectorContainer that can exist in different
  representations.  That is, one can have a hit container that is
  well behaved in local coordinates which map very naturally to the detector
  model <b>as well as</b> a HitContainer that can behave naturally in global
  coordinates, which do not map naturally to the detector model.
  <p>
  The coordinates of the hits stored are described in StxHit.  It should be
  noted that the ITTF project treats the STAR TPC as if it were 12 sectors,
  each Extending to +-200 cm.  That is, we map hits from sectors 13-24 to
  a coordinate system defined by sectors 1-12.  That way we do not need to
  make any distinction between hits that come from different sides of the TPC
  central membrane.  This is motivated by the fact that the east and west
  sectors mark a Clear distinction in data taking, but that distinction is
  unimportant in pattern recognition.  
  <p>
  First we describe the storage of the hits.  StxHitContainer treats the hits
  from a common detector plane (e.g., TPC padrow 13, sector 12) as a sorted
  std::vector<StxHit*>.  The hits are sorted via the functor StxzHitLessThan.
  This functor has a binary predicate that orders hits in a strict less than
  ordering
  based upon global z value (see above).  Next, StxHitContainer stores these
  hit-vectors in a std::map<HitMapKey, std::vector<StxHit*>, MapKeyLessThan >.
  Class
  HitMapKey is a simple struct that stores two values: refAngle and position,
  and MapKeyLessThan is a simple struct that defines a stric less-than
  ordering for HitMapKey objects.
  These values are described in StxHit.  By specifying a HitMapKey, then, one
  can achieve <b>extremely</b> efficient retrieval of the hit-vector for
  a given detector plane.
  <p>
  Next we will discuss the retrieval of hits from the container.  As stated
  above, one can gain access to a hit-vector for a given detector plane by
  specifying the position and refAngle of a detector (see method
  Hits(double,double).  Additionally, one can access the hit-map itself (or
  at least a const reference to it!) via the method Hits().  However, as
  stated before, StxHitContainer is capable of efficient retrievl of a
  subset of the hits in an event.  This subset can be defined as a subset
  of the hits from a given detector plane.  Perhaps it is easier to
  elucidate via an example.  Suppose one is interested in the hits
  corresponding to TPC sector 12, padrow 13.  Then, this sector/padrow
  combination can be easily mapped to a position and refAngle.  
  In this detector one can always specify a
  'local' coordinate system where any hit is then fully described by two
  numbers: local y and z (see StxHit for more inforamtion), where y is
  the distance along the plane (padrow) and z is the global z.  Now,
  suppose one is interested in hits that are within some volume centered at
  (y0,z0) and bounded by +-deltaD in y and +-deltaZ in z (deltaD is now
  poorly named--it was meant to represent distance D along a plane).  Then,
  to retrieve the hits from this volume one must call the setDeltaD() and
  setDeltaZ() methods to establish the bounds.  Then one must call
  one of the setRefPoint() methods.  After this, the container has selected
  the hits within the specified volume, and they can be retreived via the
  iterator like interface specified by hasMore() and Hit().
  <p>
  Additionally, StxHitContainer has been modified to provide a similar
  interface to the primary vertices in a STAR event.  Access to the
  verices is via the methods addVertex() and vertices().  Each vertex is
  mapped to an StxHit object and stored in a  hit-vector.
  <p>
  StxHitContainer must be Cleared, filled, and sorted for each
  event.  A manual call to SortHits() is necessary to achieve the most
  efficient container implementation.  
  <p>
  
  author M.L. Miller (Yale Software)

  note StxHitContainer does not own the hits that it stores.

  warning  struct MapKeyLessThan is used for ordering the HitMapKey objects.
  For a map, this means that this less-than operation is also used to defined
  equality.  Because this involves operations on doubles that are not
  guarunteed to be <b>exactly</b> identical (the values can come from hits as
  well as detectors), struct MapKeyLessThan has a built in tolerance that
  allows for the situation when two HitMapKey objects are actually equal but
  have very slightly differing values for the doubles they store.  These
  tolerances are currently set in the definition of the struct HitMapKey.

 */

#ifndef StxHitContainer_HH
#define StxHitContainer_HH

#include <vector>
#include <map>
#include <time.h>
#include <Stiostream.h>

#include "StxFactory.h"
#include "StxMapUtilities.h"
#include "StxHit.h"
#include "StxDetector.h"
#include "StxKalmanTrackNode.h"
using namespace std;

///We define this globally for convenience of users.
class VectorAndEnd {	
 private:
  Bool_t fEffectiveEndValid;
  vector<StxHit*>::iterator theEffectiveEnd;
 protected:
  friend class StxHitContainer;
  vector<StxHit*>::iterator TheEffectiveEnd() {return fEffectiveEndValid ? theEffectiveEnd : theHitVec.end();}
 public:
  VectorAndEnd();
  void TestId(Int_t id) {if ( fId == id) printf(" Id = %d \n",fId);}
  vector<StxHit*> theHitVec;
  Int_t  fId;
  static Int_t fIdCounter;
  void Clear() {theHitVec.clear(); InvalidateEnd();}
  void setEnd(vector<StxHit*>::iterator &endHit) {
    theEffectiveEnd    = endHit;
    fEffectiveEndValid = kTRUE;
  }
  void InvalidateEnd(){ fEffectiveEndValid = kFALSE; }
  vector<StxHit*>  &hits() { return theHitVec; }
  const vector<StxHit*>  &hits() const { return theHitVec; }
  size_t  size() const  { return theHitVec.size(); }
  void push_back(StxHit *hit) { theHitVec.push_back(hit); }
  vector<StxHit*>::iterator begin() { return theHitVec.begin() ; }
};
///We define this globally for convenience of users.
typedef map<HitMapKey, VectorAndEnd, MapKeyLessThan> HitMapToVectorAndEndType;

///We define this globally for convenience of users.
typedef HitMapToVectorAndEndType::value_type HitMapToVectorAndEndTypeValType;

class StxHitContainer : public TNamed {
 public:
  StxHitContainer(const Char_t * name, const Char_t * description, Factory<StxHit> *factory) :
    TNamed(name,description), _hitFactory(factory) {}
  virtual ~StxHitContainer() {}
  virtual void Add(StxHit*);
  virtual UInt_t size() const;
  virtual void Reset();
  virtual void Unset(){;}
  virtual void Clear(const Option_t* opt="");
  virtual void Print(Option_t *option="") const;
  //Sort all of the hits in the container.
  virtual void SortHits();
  //Ignore hits marked as used (std::stable_partition)
  void PartitionUsedHits();
  vector<StxHit*> & Hits();
  vector<StxHit*> & Hits(StxHit& ref, Double_t dY, Double_t dZ, Bool_t fetchAll=kFALSE);
  vector<StxHit*> & Hits(const StxDetector *detector, Double_t y, Double_t z, 
			    Double_t dY, Double_t dZ, Bool_t fetchAll=kFALSE) {
    _utilityHit.set(detector,y,z);
    return Hits(_utilityHit,dY,dZ,fetchAll);
  }
  vector<StxHit*> & Hits(StxKalmanTrackNode &node, Bool_t fetchAll=kFALSE) {
    const StxDetector* layer = node.Detector();
    _key.refangle = layer->Key(2);
    _key.position = layer->Key(1);
    _utilityHit.set(layer,node.Y(),node.Z());
    return Hits(_utilityHit,node.WindowY(),node.WindowZ(), fetchAll);
  }
  vector<StxHit*> & Hits(const StxDetector* layer) {
    _key.refangle = layer->Key(2);
    _key.position = layer->Key(1);
    assert(_map.find(_key) != _map.end());
    return _map[_key].theHitVec;
  }
  vector<StxHit*>::iterator HitsBegin(const StxDetector*);
  vector<StxHit*>::iterator HitsEnd(const StxDetector*);
  const HitMapToVectorAndEndType& hits() const { return _map; }
  HitMapToVectorAndEndType& hits()             { return _map; }
  StxHit * NearestHit(StxHit& ref, Double_t dY, Double_t dZ, Bool_t fetchAll=kFALSE);
  StxHit * NearestHit(const StxDetector *detector, Double_t y, Double_t z, 
			 Double_t dY, Double_t dZ, Bool_t fetchAll=kFALSE) {
    _utilityHit.set(detector,y,z);
    return NearestHit(_utilityHit,dY,dZ,fetchAll);
  }
  Factory<StxHit> *HitFactory() {assert(_hitFactory); return _hitFactory;}
  StxHit * Hit() {StxHit * hit = HitFactory()->getInstance(); hit->Reset(); return hit;}
  Bool_t HasKey(Double_t refangle, Double_t position) {
    HitMapToVectorAndEndType::key_type key; 
    key.refangle = refangle;
    key.position = position; 
    return _map.find(key) != _map.end();
  }
  Bool_t HasDetector(const StxDetector* layer) {
    Double_t refangle = layer->Key(2);
    Double_t position = layer->Key(1);
    return  layer ? HasKey(refangle,position) : kFALSE;  
  }
 protected:
  // Utility key used in hit retrieval (avoid constructor call per search)
  HitMapToVectorAndEndType::key_type _key; 
  // Utility hit used as a minimum position in searches
  StxHit _minPoint;
  // Utility hit used as a maximum position in searches
  StxHit _maxPoint;
  // Utility hit used as the reference in searches
  StxHit _utilityHit;
  // Utility iterator to mark the position of a hit vector (avoid constructor call per search)
  vector<StxHit*>::iterator _start;
  // Utility iterator to mark the position of a hit vector (avoid constructor call per search)
  vector<StxHit*>::iterator _stop;
  // Utility hit vector used to return hits  (avoid constructor call per search)
  vector<StxHit*> _selectedHits; //!
  // Actual Hit container used for storage of all hits
  HitMapToVectorAndEndType _map; //!
  Factory<StxHit> *_hitFactory;
  StxHitContainer *_hitContainer;
  Int_t            fIndx[25];
  TString          fPath;
  // Utility ostream operator
  friend ostream& operator<<(ostream&, const StxHitContainer&);

 private:
  StxHitContainer();
  ClassDef(StxHitContainer,0)
};

ostream& operator<<(ostream&, const vector<StxHit*>&);
ostream& operator<<(ostream&, const StxHitContainer&);





#endif
