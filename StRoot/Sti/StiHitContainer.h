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

  \note See the documentation of the methods push_back() sortHits() and
  setRefPoint() for performance guaruntees.

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

#include <vector>
#include <map>
#include <time.h>
#include <Stiostream.h>
#include "Stiostream.h"

#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include "Sti/StiMapUtilities.h"
#include "Sti/StiHit.h"

using std::map;
using std::vector;

class StiKalmanTrackNode;
class StiHit;
class StiDetector;
class Messenger;

///We define this globally for convenience of users.
typedef vector<StiHit*> HitVectorType;
struct VectorAndEnd
{	
    VectorAndEnd() {theEffectiveEnd=theHitVec.end();}
    HitVectorType theHitVec;
    HitVectorType::iterator theEffectiveEnd;
};

///We define this globally for convenience of users.
typedef map<HitMapKey, VectorAndEnd, MapKeyLessThan> HitMapToVectorAndEndType;

///We define this globally for convenience of users.
typedef HitMapToVectorAndEndType::value_type HitMapToVectorAndEndTypeValType;

class StiHitContainer : public Named, public Described
{
public:
    
    StiHitContainer(const string & name, const string & description);
    virtual ~StiHitContainer();
    ///Set the half-width of the search window in distance along the pad.
    void setDeltaD(double);
    ///Set the half-width of the search window in distance along global z.
    void setDeltaZ(double);
    ///Return the value of deltaD (cm).
    double deltaD() const;
    ///Return the value of deltaZ (cm).
    double deltaZ() const;
    ///Provide for drawable derived class(es?)
    virtual void update();
    //STL wrappers
    ///Add a hit to the container.
    virtual void push_back(StiHit*);
    ///Return the total number of hits in the container.
    virtual unsigned int size() const;
    ///Declare all hits as unused.
    virtual void reset();
    ///Clear all hits from the container.
    virtual void clear();
    ///Sort all of the hits in the container.
    virtual void sortHits();
    ///Ignore hits marked as used (std::stable_partition)
    void partitionUsedHits();
    ///Return a const reference to a vector of hits.
    const HitVectorType& hits(double refangle, double position);
    ///Return a reference to the vectore of hits, or iterators marking it's bounds
    HitVectorType& hits(const StiDetector*);
    HitVectorType::iterator hitsBegin(const StiDetector*);
    HitVectorType::iterator hitsEnd(const StiDetector*);
    ///Get all hits into a simple vector
    HitVectorType getAllHits();
    ///Return a const reference ot the hit-vector map.
    const HitMapToVectorAndEndType& hits() const;
    HitMapToVectorAndEndType& hits();
    ///Set the reference point to define sub-volume of hits to be accessed.
    void setRefPoint(StiHit* ref, bool fetchAll=false);
    ///Set the reference point to define sub-volume of hits to be accessed.
    void setRefPoint(double position, double refAngle, double y, double z, bool fetchAll=false);
    ///Set reference point to be the location of the given node
    void setRefPoint(StiKalmanTrackNode &, bool fetchAll=false);
    ///Return a boolean that reflects whether there are more hits available in the specified sub-volume.
    bool hasMore() const;
    ///Return a pointer to the StiHit object currently pointed to from the specified sub-volume.
    StiHit* getCurrentHit(); //get current
    ///Return a pointer to the StiHit object currently pointed to from the specified sub-volume.
    StiHit* getHit();  //get current hit and increment
    ///Return the number of hits satisfying the current reference and search domain.
    int getHitCandidateCount() const;  
    ///Add a vertex to the hit-container.
    void addVertex(StiHit*); //push_back
    ///Return the number of vertices stored in the container.
    unsigned int numberOfVertices() const;
    ///Return a const reference to the a vector of vertices.
    const HitVectorType& vertices() const;
protected:
    Messenger& mMessenger;
private:
    //Vertex implementation
    HitVectorType mvertexvec; //! Container for primary vertices
    //static StiHitContainer* sinstance;
    friend ostream& operator<<(ostream&, const StiHitContainer&);
    HitMapToVectorAndEndType::key_type mkey; //store a map key member to avoid constructor call per hit
    double mdeltad; //Search limit in local d-direction
    double mdeltaz; //Search limit in local z-direction
    //Used to search for points that satisfy users query
    StiHit* mminpoint;
    StiHit* mmaxpoint;
    StiHit mUtilityHit;
    HitVectorType::iterator mstart;
    HitVectorType::iterator mstop;
    //Used to return points that satisfy users query
    HitVectorType::const_iterator mcurrent;
    HitVectorType mcandidatevec; //! container of candidate points near to ref
    HitMapToVectorAndEndType mmap; //! the hit container
};

inline const HitMapToVectorAndEndType& StiHitContainer::hits() const 
{
    return mmap;
}

inline HitMapToVectorAndEndType& StiHitContainer::hits()
{
    return mmap;
}

/*! The distance is specified in STAR units (cm).
  deltaD corresponds to the distance along the detector plane, e.g.,
  distance along a TPC padrow.  If one is interested in points that are
  within +- 7.2 cm from a given value of local y (see StiHit), one would
  call setDetlaD(7.2).  
  A call to setDeltaD() sets the value of deltaD until either: \n
  1) another call to setDeltaD() is made or \n
  2) the StiHitContainer instance is deleted.\n
 */
inline void StiHitContainer::setDeltaD(double val) 
{
    mdeltad = val;
}

/*! The distance is specified in STAR units (cm).
  deltaZ corresponds to the distance along z in global STAR coordinations.
  If one is interested in points that are
  withing +- 7.2 cm from a given value of global z (see StiHit), one would
  call setDetlaZ(7.2).  
  A call to setDeltaZ() sets the value of deltaD until either: \n
  1) another call to setDeltaZ() is made or \n
  2) the StiHitContainer instance is deleted.\n
 */
inline void StiHitContainer::setDeltaZ(double val) 
{
    mdeltaz = val;
}

inline double StiHitContainer::deltaD() const 
{
    return mdeltad;
}

inline double StiHitContainer::deltaZ() const 
{
    return mdeltaz;
}

inline int StiHitContainer::getHitCandidateCount() const
{
  return mcandidatevec.size();
}

inline bool StiHitContainer::hasMore() const
{
    return (mcurrent!=mcandidatevec.end()) ? true : false;
}

///Return without incrementing.
inline StiHit* StiHitContainer::getCurrentHit()
{
    return (*mcurrent);
}

///Return and increment.
inline StiHit* StiHitContainer::getHit()
{
    return (*(mcurrent++));
}

/*! Each vertex can be mapped to a StiHit object
*/
inline void StiHitContainer::addVertex(StiHit* val)
{
    mvertexvec.push_back(val);
}

inline unsigned int StiHitContainer::numberOfVertices() const
{
    return mvertexvec.size();
}

/*! The vertices are stored in a single std::vector<StiHit*> object.
 */
inline const HitVectorType& StiHitContainer::vertices() const
{
    return mvertexvec;
}

ostream& operator<<(ostream&, const HitVectorType&);
ostream& operator<<(ostream&, const StiHitContainer&);

#endif
