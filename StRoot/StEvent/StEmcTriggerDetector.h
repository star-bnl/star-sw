/***************************************************************************
 *
 * $Id: StEmcTriggerDetector.h,v 2.1 2002/02/20 03:11:46 ullrich Exp $
 *
 * Author: Alex Suaide, Feb 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcTriggerDetector.h,v $
 * Revision 2.1  2002/02/20 03:11:46  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StEmcTriggerDetector_hh
#define StEmcTriggerDetector_hh
#include "StObject.h"
#include "StEnumerations.h"

class dst_TrgDet_st;

class StEmcTriggerDetector : public StObject {
public:
    StEmcTriggerDetector();
    StEmcTriggerDetector(const dst_TrgDet_st&);
    // StEmcTriggerDetector(const StEmcTriggerDetector&);            use default
    // StEmcTriggerDetector& operator=(const StEmcTriggerDetector&); use default
    ~StEmcTriggerDetector();

    int   numberOfTowers() const;
    
    int   highTower(unsigned int) const;
    int   patch(unsigned int) const;

    void  setHighTower(unsigned int, int);
    void  setPatch(unsigned int, int);
        
protected:
    enum {mMaxTower = 300};
    Char_t  mHighTower[mMaxTower];
    Char_t  mPatch[mMaxTower];
    
    ClassDef(StEmcTriggerDetector,1)
};
#endif
