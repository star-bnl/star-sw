/*!
 * \class StEmcTriggerDetector 
 * \author Alex Suaide, Feb 2002
 */
/***************************************************************************
 *
 * $Id: StEmcTriggerDetector.h,v 2.5 2007/07/11 23:06:45 perev Exp $
 *
 * Author: Alex Suaide, Feb 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcTriggerDetector.h,v $
 * Revision 2.5  2007/07/11 23:06:45  perev
 * Cleanup+fix StXXXTriggerDetector
 *
 * Revision 2.4  2004/08/03 17:22:16  ullrich
 * Major update by Akio and Marco.
 *
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
    int   highTowerEndcap(unsigned int) const;
    int   patchEndcap(unsigned int) const;
    unsigned short bemcLayer1(int idx) const;
    unsigned short eemcLayer1(int idx) const;
    unsigned short emcLayer2(int idx) const;

    void  setHighTower(unsigned int, int);
    void  setPatch(unsigned int, int);
    void  setHighTowerEndcap(unsigned int, int);
    void  setPatchEndcap(unsigned int, int);
        
protected:
    enum {mNPatch = 300, mENPatch = 90, mNBemcLayer1 = 48, 
	  mNEemcLayer1 = 16, mNEmcLayer2 = 8};

private:    
    // Layer0 DSM input (from detector)
    char mBeg[1];//!
    Char_t  mHighTower[mNPatch];  // High tower info from trigger patches
    Char_t  mPatch[mNPatch];      // Trigger patch sums
    
    Char_t mEHighTower[mENPatch]; // Endcap high tower per patch
    Char_t mEPatch[mENPatch];     // Endcap trigger patch sums

    // Higher DSM layer inputs (mainly for experts/debugging)
    unsigned short mBemcLayer1[mNBemcLayer1];  // Barrel layer 1 input 
    unsigned short mEemcLayer1[mNEemcLayer1];  // Endcap layer 1 input
    unsigned short mEmcLayer2[mNEmcLayer2];    // Combined EMC layer 2 input
    char mEnd[1];//!;
    
    ClassDef(StEmcTriggerDetector,3)
};
#endif
