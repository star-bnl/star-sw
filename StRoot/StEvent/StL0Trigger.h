/***************************************************************************
 *
 * $Id: StL0Trigger.h,v 2.0 1999/10/12 18:42:26 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL0Trigger.h,v $
 * Revision 2.0  1999/10/12 18:42:26  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StL0Trigger_hh
#define StL0Trigger_hh
#include "StTrigger.h"

class dst_L0_Trigger_st;

class StL0Trigger : public StTrigger {
public:
    StL0Trigger();
    StL0Trigger(const dst_L0_Trigger_st&);
    // StL0Trigger(const StL0Trigger&);            use default
    // StL0Trigger& operator=(const StL0Trigger&); use default
    ~StL0Trigger();

    UInt_t          coarsePixelArraySize();
    Long_t          coarsePixelArray(UInt_t);
    Long_t          mwcCtbMultiplicity() const;
    Long_t          mwcCtbDipole() const;
    Long_t          mwcCtbTopology() const;
    Long_t          mwcCtbMoment() const;

    void setMwcCtbMultiplicity(Long_t);
    void setMwcCtbDipole(Long_t);
    void setMwcCtbTopology(Long_t);
    void setMwcCtbMoment(Long_t);
    void setCoarsePixelArray(UInt_t, Long_t);
    
protected:
    enum {mMaxPixels = 32};
    Long_t         mCoarsePixelArray[mMaxPixels];
    Long_t         mMwcCtbMultiplicity;
    Long_t         mMwcCtbDipole;
    Long_t         mMwcCtbTopology;
    Long_t         mMwcCtbMoment;
    
    ClassDef(StL0Trigger,1)
};
#endif
