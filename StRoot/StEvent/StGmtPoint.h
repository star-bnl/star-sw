/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtHit
 *
 ***************************************************************************
 *
 * Description: data for individual ``point'' on the GMT, i.e. a pair
 * of 1D clusters.  Note, if errors during construction, the key will
 * be set to -999.  Need to check this after constructing.
 *
 ***************************************************************************/

#ifndef _ST_GMT_POINT_H_
#define _ST_GMT_POINT_H_

#include "StHit.h"

class StGmtHit;

class StGmtPoint : public StHit {
public:
    // constructors
    StGmtPoint();
    StGmtPoint( StGmtHit* hit1, StGmtHit* hit2, int key );
    
    // StGmtPoint(const StGmtPoint&);             --> use default
    // StGmtPoint& operator=(const StGmtPoint&);  --> use default
    
    // deconstructor
    ~StGmtPoint();
virtual StDetectorId detector() const           {return kGmtId;}    
    // other accessors
    int getKey();
    int getModule();
    const StGmtHit* getHitLocalX() const;
    const StGmtHit* getHitLocalY() const;
    
    float getPositionLocalX() const;
    float getPositionLocalY() const;
    virtual Int_t volumeID() const {return 0;}
    
protected:
    // data members
    Int_t mKey;                         // unique label
    StGmtHit *mHitLocalX, *mHitLocalY;

private:   
    ClassDef(StGmtPoint,1)
}; 


// inline functions

inline StGmtPoint::StGmtPoint() : StHit(), mHitLocalX(0), mHitLocalY(0) { 
    // nothing else
};

inline int StGmtPoint::getModule() {
    return static_cast< int >(mHardwarePosition/8); //====== FIX ME!!!!!!!!
};

inline int StGmtPoint::getKey() {
    return mKey;
};

inline const StGmtHit* StGmtPoint::getHitLocalX() const {   //////// FIX ME!!!!!!!!!!
    return mHitLocalX;
};

inline const StGmtHit* StGmtPoint::getHitLocalY() const {   //////// FIX ME!!!!!!!!!!
    return mHitLocalY;
};

#endif
