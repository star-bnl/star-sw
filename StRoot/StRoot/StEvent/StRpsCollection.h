/***************************************************************************
 *
 * $Id: StRpsCollection.h,v 2.3 2015/10/02 19:50:50 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 2009
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRpsCollection.h,v $
 * Revision 2.3  2015/10/02 19:50:50  ullrich
 * Added containers for tracks and points.
 *
 * Revision 2.2  2010/02/04 18:16:09  ullrich
 * Added new member mSiliconBunch and referring access methods.
 *
 * Revision 2.1  2009/11/23 22:18:25  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRpsCollection_hh
#define StRpsCollection_hh

#include "StObject.h"
#include "StContainers.h"
#include "StRpsRomanPot.h"
#include "StRpsTrackPoint.h"
#include "StRpsTrack.h"

class StRpsCollection : public StObject {
public:
    StRpsCollection();
    ~StRpsCollection();
    
    unsigned int numberOfRomanPots() const;
    
    const StRpsRomanPot* romanPot(unsigned int) const;
    StRpsRomanPot* romanPot(unsigned int);
    
    StPtrVecRpsCluster clusters() const;
    StPtrVecRpsTrackPoint trackPoints() const;
    StPtrVecRpsTrack tracks() const;
    unsigned char siliconBunch() const;
    
    void setSiliconBunch(unsigned char);
    void addTrackPoint(const StRpsTrackPoint*);
    void addTrack(const StRpsTrack*);
    
    enum {mNumberOfRomanPots = 8};
    
private:
    StRpsRomanPot mRomanPots[mNumberOfRomanPots];
    UChar_t mSiliconBunch;
    
    StSPtrVecRpsTrackPoint mTrackPoints;
    StSPtrVecRpsTrack mTracks;
    
    ClassDef(StRpsCollection, 3)
};


inline void StRpsCollection::addTrackPoint(const StRpsTrackPoint *trackPoint) {
    mTrackPoints.push_back(trackPoint);
}
inline void StRpsCollection::addTrack(const StRpsTrack *track) {
    mTracks.push_back(track);
}

#endif

