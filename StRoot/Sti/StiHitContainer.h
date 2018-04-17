//StiHitContainer.h
//M.L. Miller (Yale Software)
//03/01

/*! \class StiHitContainer
  StiHitContainer is exactly that--a container for StiHits!  Because of the
  sheer number of hits in a STAR event, StiHitContainer is designed to
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
  The coordinates of the hits stored are described in StiHit.  It should be
  noted that the ITTF project treats the STAR TPC as if it were 12 sectors,
  each extending to +-200 cm.  That is, we map hits from sectors 13-24 to
  a coordinate system defined by sectors 1-12.  That way we do not need to
  make any distinction between hits that come from different sides of the TPC
  central membrane.  This is motivated by the fact that the east and west
  sectors mark a clear distinction in data taking, but that distinction is
  unimportant in pattern recognition.  
  <p>
  First we describe the storage of the hits.  StiHitContainer treats the hits
  from a common detector plane (e.g., TPC padrow 13, sector 12) as a sorted
  std::vector<StiHit*>.  The hits are sorted via the functor StizHitLessThan.
  This functor has a binary predicate that orders hits in a strict less than
  ordering
  based upon global z value (see above).  Next, StiHitContainer stores these
  hit-vectors in a std::map<HitMapKey, std::vector<StiHit*>, MapKeyLessThan >.
  Class
  HitMapKey is a simple struct that stores two values: refAngle and position,
  and MapKeyLessThan is a simple struct that defines a stric less-than
  ordering for HitMapKey objects.
  These values are described in StiHit.  By specifying a HitMapKey, then, one
  can achieve <b>extremely</b> efficient retrieval of the hit-vector for
  a given detector plane.
  <p>
  Next we will discuss the retrieval of hits from the container.  As stated
  above, one can gain access to a hit-vector for a given detector plane by
  specifying the position and refAngle of a detector (see method
  hits(double,double).  Additionally, one can access the hit-map itself (or
  at least a const reference to it!) via the method hits().  However, as
  stated before, StiHitContainer is capable of efficient retrievl of a
  subset of the hits in an event.  This subset can be defined as a subset
  of the hits from a given detector plane.  Perhaps it is easier to
  elucidate via an example.  Suppose one is interested in the hits
  corresponding to TPC sector 12, padrow 13.  Then, this sector/padrow
  combination can be easily mapped to a position and refAngle.  
  In this detector one can always specify a
  'local' coordinate system where any hit is then fully described by two
  numbers: local y and z (see StiHit for more inforamtion), where y is
  the distance along the plane (padrow) and z is the global z.  Now,
  suppose one is interested in hits that are within some volume centered at
  (y0,z0) and bounded by +-deltaD in y and +-deltaZ in z (deltaD is now
  poorly named--it was meant to represent distance D along a plane).  Then,
  to retrieve the hits from this volume one must call the setDeltaD() and
  setDeltaZ() methods to establish the bounds.  Then one must call
  one of the setRefPoint() methods.  After this, the container has selected
  the hits within the specified volume, and they can be retreived via the
  iterator like interface specified by hasMore() and getHit().
  <p>
  Additionally, StiHitContainer has been modified to provide a similar
  interface to the primary vertices in a STAR event.  Access to the
  verices is via the methods addVertex() and vertices().  Each vertex is
  mapped to an StiHit object and stored in a  hit-vector.
  <p>
  StiHitContainer must be cleared, filled, and sorted for each
  event.  A manual call to sortHits() is necessary to achieve the most
  efficient container implementation.  
  <p>
  
  \author M.L. Miller (Yale Software)

  \note StiHitContainer does not own the hits that it stores.

  \warning  struct MapKeyLessThan is used for ordering the HitMapKey objects.
  For a map, this means that this less-than operation is also used to defined
  equality.  Because this involves operations on doubles that are not
  guarunteed to be <b>exactly</b> identical (the values can come from hits as
  well as detectors), struct MapKeyLessThan has a built in tolerance that
  allows for the situation when two HitMapKey objects are actually equal but
  have very slightly differing values for the doubles they store.  These
  tolerances are currently set in the definition of the struct HitMapKey.

 */

#ifndef StiHitContainer_HH
#define StiHitContainer_HH

#include <cassert>
#include <vector>
#include <map>
#include <time.h>
#include <Stiostream.h>
#include "Stiostream.h"

#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include "Sti/Base/Filter.h"
#include "Sti/Base/Factory.h"
#include "Sti/StiMapUtilities.h"
#include "Sti/StiHit.h"
#include "Sti/StiDetector.h"
#include "Sti/StiKalmanTrackNode.h"
using namespace std;

///We define this globally for convenience of users.
class VectorAndEnd
{	
 private:
    bool fEffectiveEndValid;
    vector<StiHit*>::iterator theEffectiveEnd;
protected:
    friend class StiHitContainer;
    vector<StiHit*>::iterator TheEffectiveEnd() { 
       return fEffectiveEndValid ? theEffectiveEnd
             : theHitVec.end();
    }
 public:
    VectorAndEnd();
    void TestId(int id);
    vector<StiHit*> theHitVec;
    int  fId;
    static int fIdCounter;
    void clear();
    void setEnd(vector<StiHit*>::iterator &endHit) {
      theEffectiveEnd    = endHit;
      fEffectiveEndValid = true;
    }
    void invalidateEnd(){ fEffectiveEndValid = false; }
    vector<StiHit*>  &hits() { return theHitVec; }
    const vector<StiHit*>  &hits() const { return theHitVec; }
    size_t  size() const  { return theHitVec.size(); }
    void push_back(StiHit *hit) { theHitVec.push_back(hit); }
    vector<StiHit*>::iterator begin() { return theHitVec.begin() ; }
 };
///We define this globally for convenience of users.
typedef map<HitMapKey, VectorAndEnd, MapKeyLessThan> HitMapToVectorAndEndType;

///We define this globally for convenience of users.
typedef HitMapToVectorAndEndType::value_type HitMapToVectorAndEndTypeValType;

class StiHitContainer : public Named, public Described
{
public:
    
  StiHitContainer(const string & name, const string & description, Factory<StiHit> *factory);
  virtual ~StiHitContainer();
  virtual void add(StiHit*);
  virtual unsigned int size() const;
  virtual void reset();
  virtual void unset(){;}
  virtual void clear();
  //Sort all of the hits in the container.
  virtual void sortHits();
  void setMaxTimes(int nTimes);
  vector<StiHit*> & getHits();
  vector<StiHit*> & getHits(Filter<StiHit> & filter);
  vector<StiHit*> & getHits(StiHit& ref, double dY, double dZ, bool fetchAll=false);
  vector<StiHit*> & getHits(double position, double refAngle, double y, double z, 
			    double dY, double dZ, bool fetchAll=false);
  vector<StiHit*> & getHits(StiKalmanTrackNode &, bool fetchAll=false);
  vector<StiHit*> & getHits(double refangle, double position);
  vector<StiHit*> & getHits(const StiDetector*);
  vector<StiHit*>::iterator hitsBegin(const StiDetector*);
  vector<StiHit*>::iterator hitsEnd(const StiDetector*);
  const HitMapToVectorAndEndType& hits() const;
  HitMapToVectorAndEndType& hits();
  StiHit * getNearestHit(StiHit& ref, double dY, double dZ, bool fetchAll=false);
  StiHit * getNearestHit(double position, double refAngle, double y, double z, 
			 double dY, double dZ, bool fetchAll=false);
  /// Get the hit factory
  Factory<StiHit> * getHitFactory();
  /// Get a hit instance from the factory
  StiHit * getHit();
  bool hasKey(double refangle, double position);
  bool hasDetector(const StiDetector* layer);
 protected:
  // Utility key used in hit retrieval (avoid constructor call per search)
  HitMapToVectorAndEndType::key_type _key; 
  // Utility hit used as a minimum position in searches
  StiHit _minPoint;
  // Utility hit used as a maximum position in searches
  StiHit _maxPoint;
  // Utility hit used as the reference in searches
  StiHit _utilityHit;
  // Utility iterator to mark the position of a hit vector (avoid constructor call per search)
  vector<StiHit*>::iterator _start;
  // Utility iterator to mark the position of a hit vector (avoid constructor call per search)
  vector<StiHit*>::iterator _stop;
  // Utility hit vector used to return hits  (avoid constructor call per search)
  vector<StiHit*> _selectedHits; //!
  // Actual Hit container used for storage of all hits
  HitMapToVectorAndEndType _map; //!
  Factory<StiHit> * _hitFactory;
  // Utility ostream operator
  friend ostream& operator<<(ostream&, const StiHitContainer&);

 private:
  StiHitContainer();
};

inline const HitMapToVectorAndEndType& StiHitContainer::hits() const 
{
  return _map;
}

inline HitMapToVectorAndEndType& StiHitContainer::hits()
{
  return _map;
}

/// Get hits satisfying the given position and search radius.
/// Position specified with "position,refAngle,y,z"
/// Search radius specified with dY,dZ
inline vector<StiHit*> & StiHitContainer::getHits(double position, double refAngle,double y, double z, 
						  double dY, double dZ, bool fetchAll)
{
  _utilityHit.set(position,refAngle,y,z);
  return getHits(_utilityHit,dY,dZ,fetchAll);
}

/// Get hits satisfying the given position and search radius specified with a Kalman track node
inline vector<StiHit*> & StiHitContainer::getHits(StiKalmanTrackNode & node, bool fetchAll)
{
  _utilityHit.set(node.getRefPosition(),node.getLayerAngle(),node.getY(),node.getZ());
  return getHits(_utilityHit,node.getWindowY(),node.getWindowZ(), fetchAll);
}

/*! Get all hits from the specified detector component
  The values of refangle and position are used to fill an existing
  HitMapToVectorAndEndTypeKey object.  This object is used to key the retrieval of a vector
  of hits associated with the specified position.  For more on the
  definition of refangle and position, please see StiHit documentation.\n
  A call to hits(double,double) corresponds to the retrieval of an object
  from an STL map based on a key.  Such a retrieval is guarunteed to be of
  O(logN) time complexity, where N is the number of keys in the map.  For
  our practices, N is roughly equal to (TPC) 12*45 + (SVT layer 1) 2*4 +
  (SVT layer 2) 2*6 + (SVT layer 3) 2*8 + (SSD) 20.
 */
inline vector<StiHit*>& StiHitContainer::getHits(double refangle, double position)
{
    _key.refangle = refangle;
    _key.position = position; 
    assert(_map.find(_key) != _map.end());
    return _map[_key].theHitVec;
}
inline bool StiHitContainer::hasKey(double refangle, double position)
{
    HitMapToVectorAndEndType::key_type key; 
    key.refangle = refangle;
    key.position = position; 
    return _map.find(key) != _map.end();
} 
inline bool StiHitContainer::hasDetector(const StiDetector* layer)
{  
   double refangle = layer->getPlacement()->getLayerAngle();
   double position = layer->getPlacement()->getLayerRadius();
   return  hasKey(refangle,position);  
}
/// Get all hits from the specified detector component
inline vector<StiHit*>& StiHitContainer::getHits(const StiDetector* layer)
{
    _key.refangle = layer->getPlacement()->getLayerAngle();
    _key.position = layer->getPlacement()->getLayerRadius();
    assert(_map.find(_key) != _map.end());
    return _map[_key].theHitVec;
}


inline StiHit * StiHitContainer::getNearestHit(double position, double refAngle, double y, double z, 
					       double dY, double dZ, bool fetchAll)
{
  _utilityHit.set(position,refAngle,y,z);
  return getNearestHit(_utilityHit,dY,dZ,fetchAll);
}

inline Factory<StiHit> * StiHitContainer::getHitFactory()
{  
  assert(_hitFactory);
  return _hitFactory;
}

inline StiHit * StiHitContainer::getHit()
{
  StiHit * hit = getHitFactory()->getInstance();
  hit->reset();
  return hit;
}

ostream& operator<<(ostream&, const vector<StiHit*>&);
ostream& operator<<(ostream&, const StiHitContainer&);





#endif
