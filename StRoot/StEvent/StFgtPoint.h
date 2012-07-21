/***************************************************************************
 *
 * $Id: StFgtPoint.h,v 2.2 2012/07/21 03:32:34 perev Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: data for individual ``point'' on the FGT, i.e. a pair
 * of 1D clusters.  Note, if errors during construction, the key will
 * be set to -999.  Need to check this after constructing.
 *
 ***************************************************************************
 *
 * $Log: StFgtPoint.h,v $
 * Revision 2.2  2012/07/21 03:32:34  perev
 * BugFix define detector()
 *
 * Revision 2.1  2012/04/16 20:20:49  ullrich
 * Initial Revision
 *
 *
 **************************************************************************/

#ifndef _ST_FGT_POINT_H_
#define _ST_FGT_POINT_H_

#include "StHit.h"

class StFgtHit;

class StFgtPoint : public StHit {
public:
    // constructors
    StFgtPoint();
    StFgtPoint( StFgtHit* hit1, StFgtHit* hit2, int key );
    
    // StFgtPoint(const StFgtPoint&);             --> use default
    // StFgtPoint& operator=(const StFgtPoint&);  --> use default
    
    // deconstructor
    ~StFgtPoint();
virtual StDetectorId detector() const           {return kFgtId;}    
    // other accessors
    int getKey();
    int getDisc();
    int getQuad();
    const StFgtHit* getHitR() const;
    const StFgtHit* getHitPhi() const;
    
    float getPositionR() const;
    float getPositionPhi() const;
    
protected:
    // data members
    Int_t mKey;                         // unique label
    StFgtHit *mHitR, *mHitPhi;
    
private:   
    ClassDef(StFgtPoint,1);
}; 


// inline functions

inline StFgtPoint::StFgtPoint() : StHit(), mHitR(0), mHitPhi(0) { 
    // nothing else
};

inline int StFgtPoint::getDisc() {
    return static_cast< int >(mHardwarePosition/8);
};

inline int StFgtPoint::getQuad() {
    return static_cast< int >((mHardwarePosition/2)%4);
};

inline int StFgtPoint::getKey() {
    return mKey;
};

inline const StFgtHit* StFgtPoint::getHitR() const {
    return mHitR;
};

inline const StFgtHit* StFgtPoint::getHitPhi() const {
    return mHitPhi;
};

#endif
