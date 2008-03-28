///////////////////////////////////////////////////////////////////////////
// $Id: TpcHitUtilities.h,v 1.6 2006/05/20 03:17:21 genevb Exp $
//
// Author: M.L. Miller, Yale
//
///////////////////////////////////////////////////////////////////////////
//
// Description: TPC sector gains hit utilities class
//
///////////////////////////////////////////////////////////////////////////
//
// $Log: TpcHitUtilities.h,v $
// Revision 1.6  2006/05/20 03:17:21  genevb
// Changed MapKey to MapQAKey to make it unique for QA
//
// Revision 1.5  2005/07/06 23:21:04  fisyak
// use templated StThreeVectorD
//
// Revision 1.4  2001/04/25 18:16:20  perev
// HPcorrs
//
// Revision 1.3  2000/08/09 18:57:44  lansdell
// improvements in TPC gains code reduces CPU time per event by factor of 2
//
//
///////////////////////////////////////////////////////////////////////////

#ifndef TpcHitUtilities_H
#define TpcHitUtilities_H

class StTrack;
class StTpcHit;
#include "StThreeVectorD.hh"

#include <vector>
#include <map>

#ifndef ST_NO_NAMESPACES
using std::vector;
using std::map;
using std::pair;
#endif

#include "TpcMapUtilities.h"

typedef map<HitMapQAKey, PadrowLocation, MapQAKeyLessThan>::value_type padrowMapValType;

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

protected:
    //Implementation Details----------------
    void buildMaps();        //Build the maps for dx calculation
    bool keepHit(StTpcHit*); //Cut on hit flags
    const StThreeVectorD sectorNormal(int sector); //Return the normal vector to a given sector
    
    map<int, StThreeVectorD> m_SectorNormalMap; //! Map of normal vectors to a sector
    map<HitMapQAKey, PadrowLocation, MapQAKeyLessThan> m_PadrowMap; //! Map of 3 points in each padrow

    //Members-------------------------------
    vector<StTpcHit*> m_tpcHitVec; //!
    StTrack* m_StTrack;
    double m_BField; //We need this for the crossing angle calculation
};

#endif
