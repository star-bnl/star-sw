/*!
 * \class StEmcTriggerDetector 
 * \author Alex Suaide, Feb 2002
 */
/***************************************************************************
 *
 * $Id: StEmcTriggerDetector.h,v 2.3 2004/02/11 01:42:09 ullrich Exp $
 *
 * Author: Alex Suaide, Feb 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcTriggerDetector.h,v $
 * Revision 2.3  2004/02/11 01:42:09  ullrich
 * Added new constructor to load data from StTriggerData.
 *
 * Revision 2.2  2002/02/22 22:56:47  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.1  2002/02/20 03:11:46  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StEmcTriggerDetector_hh
#define StEmcTriggerDetector_hh
#include "StObject.h"
#include "StEnumerations.h"

class dst_TrgDet_st;
class StTriggerData;

class StEmcTriggerDetector : public StObject {
public:
    StEmcTriggerDetector();
    StEmcTriggerDetector(const dst_TrgDet_st&);
    StEmcTriggerDetector(const StTriggerData&);
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
