/***************************************************************************
 *
 * $Id: StTrackTopologyMap.h,v 2.2 1999/12/13 20:16:39 ullrich Exp $
 *
 * Author: Thomas Ullrich, AUg 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackTopologyMap.h,v $
 * Revision 2.2  1999/12/13 20:16:39  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.2  1999/12/13 20:16:39  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.1  1999/10/13 19:44:17  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTrackTopologyMap_hh
#define StTrackTopologyMap_hh

#include "StObject.h"
#include "StEnumerations.h"

class StTrackTopologyMap : public StObject {
public:
    StTrackTopologyMap();
    StTrackTopologyMap(ULong_t, ULong_t);
    StTrackTopologyMap(const ULong_t*);
    // StTrackTopologyMap(const StTrackTopologyMap&);            use default
    // StTrackTopologyMap& operator=(const StTrackTopologyMap&); use default
    ~StTrackTopologyMap();

    Bool_t   primaryVertexUsed() const;
    UInt_t   numberOfHits(StDetectorId) const;
    Bool_t   hasHitInRow(StDetectorId, UInt_t) const; // first row = 1
    Bool_t   hasHitInSvtLayer(UInt_t) const;          // first layer = 1
    Bool_t   turnAroundFlag() const;
    ULong_t  data(UInt_t) const;
    
protected:
    Bool_t bit(Int_t) const;             // range 0-63
    ULong_t mMap[2];
    // ULong_t mMap[2];
    UInt_t mMap0;
    UInt_t mMap1;
    
    ClassDef(StTrackTopologyMap,1)
};

#endif
