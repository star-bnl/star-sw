// TpcHitUtilities.h
// M.L. Miller
// Yale Software
// 7/00

#ifndef TpcHitUtilities_H
#define TpcHitUtilities_H

class StTrack;
class StTpcHit;
class StThreeVectorD;

#include <vector>
#include <map>

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class TpcHitUtilities {
public:
    TpcHitUtilities();
    TpcHitUtilities(StTrack*, double MagneticField);
    virtual ~TpcHitUtilities();

    //Access--------------------------------
    void clear();
    void setTrack(StTrack*);
    void setBField(double);

    const vector<StTpcHit*>& tpcHitVec() const;

    //Methods-----------------------------
    void findHits();  //Get the tpc hits from Track
    double dx(StTpcHit*); //Calculate the pathlength of a hit
    bool keepHit(StTpcHit*); //Cut on hit flags
    double crossingAngle(StTpcHit*);  //Calculate the crossingAngle (degrees)
    const StThreeVectorD sectorNormal(int sector); //Return the normal vector to a given sector

    //Utilities-----------------------------
    void printTpcHitVec();

protected:
    vector<StTpcHit*> m_tpcHitVec; //!
    StTrack* m_StTrack;
    double m_BField; //We need this for the crossing angle calculation
};

#endif
