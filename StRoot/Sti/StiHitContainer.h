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
#include <iostream.h>
#include <fstream.h>

#include "StiMapUtilities.h"
#include "StiHit.h"

using std::map;
using std::vector;

class StiHit;

typedef vector<StiHit*> hitvector;
typedef map<HitMapKey, hitvector, MapKeyLessThan> hitmap;
typedef hitmap::value_type hitMapValType;

class StiHitContainer
{
public:
    friend class nobody;
    
    //Singleton access
    static StiHitContainer* instance();
    static void kill();
    
    void setDeltaD(double);
    void setDeltaZ(double);

    double deltaD() const;
    double deltaZ() const;

    //Provide for drawable derived class(es?)
    virtual void update();
    
    //STL wrappers
    virtual void push_back(StiHit*);
    virtual unsigned int size() const;
    virtual void clear();
    virtual void clearAndDestroy();
    virtual void sortHits();

    //Gets
    const hitvector& hits(double refangle, double position);
    const hitmap& hits() const;
    
    //User Query Interface
    void setRefPoint(StiHit* ref);
    //position- postion of detector, not x of hit.
    void setRefPoint(double position, double refAngle, double y, double z);
    bool hasMore() const;
    StiHit* getCurrentHit(); //get current
    StiHit* getHit();  //get current hit and increment

    //Add vertex information
    void addVertex(StiHit*); //push_back
    unsigned int numberOfVertices() const;
    const hitvector& vertices() const;
    
protected:
    StiHitContainer();
    virtual ~StiHitContainer();

private:
    //Vertex implementation
    hitvector mvertexvec; //! Container for primary vertices

private:
    static StiHitContainer* sinstance;
    friend ostream& operator<<(ostream&, const StiHitContainer&);
    
    HitMapKey mkey; //store a map key member to avoid constructor call per hit
    
    double mdeltad; //Search limit in local d-direction
    double mdeltaz; //Search limit in local z-direction

    //Used to search for points that satisfy users query
    StiHit* mminpoint;
    StiHit* mmaxpoint;
    StiHit mUtilityHit;
    hitvector::iterator mstart;
    hitvector::iterator mstop;

    //Used to return points that satisfy users query
    hitvector::const_iterator mcurrent;
    hitvector mcandidatevec; //! container of candidate points near to ref
    
    hitmap mmap; //! the hit container
};

//inlines

inline const hitmap& StiHitContainer::hits() const 
{
    return mmap;
}

inline void StiHitContainer::setDeltaD(double val) 
{
    mdeltad = val;
}

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

inline bool StiHitContainer::hasMore() const
{
    return (mcurrent!=mcandidatevec.end()) ? true : false;
}

//Return without incrementing
inline StiHit* StiHitContainer::getCurrentHit()
{
    return (*mcurrent);
}

//Return and increment
inline StiHit* StiHitContainer::getHit()
{
    return (*(mcurrent++));
}

//vertex inlines

inline void StiHitContainer::addVertex(StiHit* val)
{
    mvertexvec.push_back(val);
}

inline unsigned int StiHitContainer::numberOfVertices() const
{
    return mvertexvec.size();
}

inline const hitvector& StiHitContainer::vertices() const
{
    return mvertexvec;
}

// Non -memebers

ostream& operator<<(ostream&, const hitvector&);

ostream& operator<<(ostream&, const StiHitContainer&);

#endif
