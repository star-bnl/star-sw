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

using std::map;
using std::vector;

class StiHit;

typedef vector<StiHit*> hitvector;
typedef map<HitMapKey, hitvector, MapKeyLessThan> hitmap;
typedef hitmap::value_type hitMapValType;

class StiHitContainer
{
public:
    virtual ~StiHitContainer();

    //Singleton access
    static StiHitContainer* instance();
    static void kill();
    
    inline void setDeltaD(double val) {mdeltad = val;}
    inline void setDeltaZ(double val) {mdeltaz = val;}

    inline double deltaD() const {return mdeltad;}
    inline double deltaZ() const {return mdeltaz;}

    //STL wrappers
    virtual void push_back(StiHit*);
    virtual unsigned int size() const;
    virtual void clear();
    virtual void clearAndDestroy();
    virtual void sortHits();

    //Gets
    const hitvector& hits(double refangle, double position);
    const hitmap& hits() const {return mmap;}
    
    //Debugging Utilities
    void print() const;
    void print(double refangle, double position);
    void print(double refangle, double position, ofstream&);
    
    //User Query Interface
    void setRefPoint(StiHit* ref);
    bool hasMore() const;
    StiHit* getCurrentHit(); //get current
    StiHit* getHit();  //get current hit and increment

    //Add vertex information
    void addVertex(StiHit*); //push_back
    void removeVertex(StiHit*); //look for, remove this vertex if found
    void removeAllVertices(); //clear
    unsigned int numberOfVertices() const;
    
    void resetVertexIterator(); //set iterator to begin()
    
    StiHit* vertex(unsigned int i) const;
    StiHit* firstVertex() const; //first in container
    StiHit* lastVertex() const; //last in container
    StiHit* nextVertex(); //next in container
    StiHit* previousVertex(); //previous

    void printVertices() const;
    
protected:
    StiHitContainer();

private:
    //Vertex implementation
    hitvector mvertexvec; //! Container for primary vertices
    hitvector::const_iterator mvertexiterator;

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
