//StiHitContainer.h
//M.L. Miller (Yale Software)
//03/01

// Container of StHit pointers, version one assums only StTpcHit* will be 
// passed to the container.
// Container is a map (not multimap) keyed by struct, containing vector of
// hits.
// Search region is +-deltaD() in distance along pad, and +-deltaZ() in z
// Version one has two calls to dynamic_cast<StiTpcHit*> which can be
// removed when sector() and padrow() are propogated to StiHit class

#ifndef StiHitContainer_HH
#define StiHitContainer_HH

#include <vector>
#include <map>
#include <time.h>

#include "StTpcHit.h"
#include "StiHitMapUtilities.h"

class ofstream;

typedef vector<StHit*> hitvector;
typedef map<HitMapKey, hitvector, MapKeyLessThan> hitmap;
typedef map<HitMapKey, hitvector, MapKeyLessThan>::value_type hitMapValType;

class StiHitContainer
{
public:
    StiHitContainer();
    virtual ~StiHitContainer();

    void setDeltaD(double val) {mdeltad = val;}
    void setDeltaZ(double val) {mdeltaz = val;}

    double deltaD() const {return mdeltad;}
    double deltaZ() const {return mdeltaz;}
    
    clock_t push_back(StHit*);
    int size() const;
    clock_t clear();
    clock_t clearAndDestroy();
    clock_t sortHits();

    void print() const;
    void print(int sector, int padrow);
    void print(int sector, int padrow, ofstream&);
    
    //User Query Interface
    clock_t setRefPoint(StHit* ref);
    bool hasMore() const;
    const StHit* getHit();

private:
    void findHitsNearRef(StHit* ref);

    HitMapKey mkey; //store a map key member to avoid constructor call per hit
    
    double mdeltad; //Search limit in local d-direction
    double mdeltaz; //Search limit in local z-direction

    //Used to search for points that satisfy users query
    StHit* mminpoint;
    StHit* mmaxpoint;
    hitvector::iterator mstart;
    hitvector::iterator mstop;

    //Used to return points that satisfy users query
    hitvector::const_iterator mcurrent;
    hitvector mcandidatevec; //! container of candidate points near to ref
    
    hitmap mmap; //! the hit container
};
#endif
