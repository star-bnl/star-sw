// TpcHitVecUtilities.h
// M.L. Miller
// 6/00

#ifndef TpcHitVecUtilities_HH
#define TpcHitVecUtilities_HH

#include <vector>

using std::vector;

//Forward Declarationis
class StTrack;
class StTpcHit;
class StThreeVectorD;


class TpcHitVecUtilities {
    
public:

    TpcHitVecUtilities();
    TpcHitVecUtilities(StTrack*, int);
    virtual ~TpcHitVecUtilities();

    //Access-----------------------------
    void clear();
    const vector<StTpcHit*>& tpcHitVec() const; //!
    void setTrack(StTrack*);
    void setTrackNumber(int);

    //Methods--------------------------
    void findHits();     //Get the vector of tpc hits for this track
    int findPadrowMaxDifference();     //Find the max diff in padrows in a track

    //STL utilities
    void printTpcHitVec();
    void printTpcHitVecCharge();
    void printTpcHitVecPadrow();
    void sortTpcHitVecCharge();
    void sortTpcHitVecPadRow();
    int padrowDifference (StTpcHit* , StTpcHit* );
    bool findPadrow(unsigned int );
    int nHigherCharges();

protected:
    vector<StTpcHit*> m_tpcHitVec; //!     The vector of tpc hits for this track. 
    int m_TrackNumber;
    StTrack* m_StTrack;
        
};

struct chargeLessThan {
    bool operator() (const StTpcHit* , const StTpcHit* ) const;
};

struct padrowLessThan {
    bool padrowLessThan::operator() (const StTpcHit* hit1, const StTpcHit* hit2) const;
};

struct padrowEqual {
    bool padrowEqual::operator() (const StTpcHit* hit) const;
    unsigned int m_Padrow;
};

#endif
