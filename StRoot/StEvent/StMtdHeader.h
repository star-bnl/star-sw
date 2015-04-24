/*!
 * \class StMtdHeader 
 */
/***************************************************************************
 *
 * $Id: StMtdHeader.h,v 2.3 2015/04/24 17:51:00 ullrich Exp $
 *
 * Author: Frank Geurts, April 25, 2011
 ***************************************************************************
 *
 * Description:
 *   Barrel MTD header data, contains the MTD data header
 *
 ***************************************************************************
 *
 * $Log: StMtdHeader.h,v $
 * Revision 2.3  2015/04/24 17:51:00  ullrich
 * Added data member mTpcSectorMask and mShouldHaveRejectEvent incl. access fcts.
 *
 * Revision 2.2  2013/04/06 12:18:35  ullrich
 * Increase MAXFIBER from 1 to 2.
 *
 * Revision 2.1  2011/04/25 21:24:02  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StMtdHeader_hh
#define StMtdHeader_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StEnumerations.h"

class StMtdHeader : public StObject {
public:
    enum {MAXFIBER=2};
    
    StMtdHeader();
    ~StMtdHeader();
    
    short          fiberHeader(int fiberId) const;
    unsigned int   fiberTriggerWord(int fiberId) const;
    unsigned int   triggerTime(int fiberId) const;
    int            shouldHaveRejectEvent() const;
    unsigned int   tpcSectorMask() const;
    
    void         setFiberHeader(int fiberId, short val);
    void         setFiberTriggerWord(int fiberId, unsigned int val);
    void         setTriggerTime(unsigned int tdc, int fiberId);
    void         setShouldHaveRejectEvent(int reject);
    void         setTpcSectorMask(unsigned int mask);
    
protected:
    Short_t      mFiberHeader[MAXFIBER];
    UInt_t       mFiberTriggerWord[MAXFIBER];
    UInt_t       mTriggerTime[MAXFIBER];
    Int_t        mShouldHaveRejectEvent; // indication of event status in filtering
                                         // 0 - events not triggered di-muon
                                         // 1 - events should have been rejected 
                                         //     if only triggered by di-muon
                                         // 2 - events pass filtering cuts
    UInt_t       mTpcSectorMask;         // Mask of TPC sectors for tracking in the first iteration
    
    ClassDef(StMtdHeader,2)
};

#endif
