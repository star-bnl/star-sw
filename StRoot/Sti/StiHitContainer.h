//StiHitContainer.h
//M.L. Miller (Yale Software)
//03/01

// Container of StiHit pointers.
// Container is a map (not multimap) keyed by struct, containing vector of
// hits.
// Search region is +-deltaD() in distance along pad, and +-deltaZ() in z

#ifndef StiHitContainer_HH
#define StiHitContainer_HH

#include <vector>
#include <map>
#include <time.h>

#include "StiMapUtilities.h"

class StiHit;
class ofstream;

typedef vector<StiHit*> hitvector;
typedef map<HitMapKey, hitvector, MapKeyLessThan> hitmap;
typedef map<HitMapKey, hitvector, MapKeyLessThan>::value_type hitMapValType;

class StiHitContainer
{
public:
    virtual ~StiHitContainer();

    //Singleton access
    static StiHitContainer* instance();
    static void kill();
    
    void setDeltaD(double val) {mdeltad = val;}
    void setDeltaZ(double val) {mdeltaz = val;}

    double deltaD() const {return mdeltad;}
    double deltaZ() const {return mdeltaz;}

    //STL wrappers
    void push_back(StiHit*);
    unsigned int size() const;
    void clear();
    void clearAndDestroy();
    void sortHits();

    //Gets
    const hitvector& hits(double refangle, double position);

    //Debugging Utilities
    void print() const;
    void print(double refangle, double position);
    void print(double refangle, double position, ofstream&);
    
    //User Query Interface
    void setRefPoint(StiHit* ref);
    bool hasMore() const;
    StiHit* getHit();

protected:
    StiHitContainer();

private:
    static StiHitContainer* sinstance;
    
    HitMapKey mkey; //store a map key member to avoid constructor call per hit
    
    double mdeltad; //Search limit in local d-direction
    double mdeltaz; //Search limit in local z-direction

    //Used to search for points that satisfy users query
    StiHit* mminpoint;
    StiHit* mmaxpoint;
    hitvector::iterator mstart;
    hitvector::iterator mstop;

    //Used to return points that satisfy users query
    hitvector::const_iterator mcurrent;
    hitvector mcandidatevec; //! container of candidate points near to ref
    
    hitmap mmap; //! the hit container
};
#endif
