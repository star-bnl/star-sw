//TpcHitVecUtilites.h
//M.L. Miller, Yale Software, 8/00
#ifdef NEVER
#ifndef TpcHitVecUtilities_HH
#define TpcHitVecUtilities_HH

#include <vector>

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

//Forward Declarationis
class StTrack;
class StTpcHit;
#include "StThreeVectorD.hh"

class TpcHitVecUtilities {
    
public:

    TpcHitVecUtilities();
    virtual ~TpcHitVecUtilities();

    //Access-----------------------------
    void clear();
    void setTrack(StTrack*);
    const vector<StTpcHit*>& tpcHitVec() const; //!

    //Methods--------------------------
    void findHits();     //Get the vector of tpc hits for this track
    void printTpcHitVecPosition(); //Print position to screen
    void sortTpcHitVecZ(); //Sort by z of hit
    int  numberOfHitsInZTrack(double zmin, double zmax);    
protected:
    vector<StTpcHit*> m_tpcHitVec; //!     The vector of tpc hits for this track. 
    StTrack* m_StTrack;
        
};

struct zHitLessThan {
    bool zHitLessThan::operator() (const StTpcHit* hit1, const StTpcHit* hit2) const;
};

#endif
#endif // NEVER
