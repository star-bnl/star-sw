/***************************************************************************
 *
 * $Id: StL0Trigger.h,v 2.1 2001/04/05 04:00:38 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL0Trigger.h,v $
 * Revision 2.1  2001/04/05 04:00:38  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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

    unsigned int  coarsePixelArraySize();
    int           coarsePixelArray(unsigned int);
    int           mwcCtbMultiplicity() const;
    int           mwcCtbDipole() const;
    int           mwcCtbTopology() const;
    int           mwcCtbMoment() const;

    void setMwcCtbMultiplicity(int);
    void setMwcCtbDipole(int);
    void setMwcCtbTopology(int);
    void setMwcCtbMoment(int);
    void setCoarsePixelArray(unsigned int, int);
    
protected:
    enum {mMaxPixels = 32};
    Int_t         mCoarsePixelArray[mMaxPixels];
    Int_t         mMwcCtbMultiplicity;
    Int_t         mMwcCtbDipole;
    Int_t         mMwcCtbTopology;
    Int_t         mMwcCtbMoment;
    
    ClassDef(StL0Trigger,1)
};
#endif
