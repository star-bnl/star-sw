/*!
 * \class StL0Trigger 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StL0Trigger.h,v 2.10 2005/10/10 19:24:38 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL0Trigger.h,v $
 * Revision 2.10  2005/10/10 19:24:38  ullrich
 * run number range check, and updated for run5.
 *
 * Revision 2.9  2004/08/03 17:22:16  ullrich
 * Major update by Akio and Marco.
 *
 * Revision 2.8  2003/07/29 19:50:53  ullrich
 * Fix for spin bits added.
 *
 * Revision 2.7  2002/11/26 02:19:11  perev
 * StEventMaker ITTF modif
 *
 * Revision 2.6  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.5  2002/02/15 00:18:13  ullrich
 * Changed signature of bunchCrossingId7bit().
 *
 * Revision 2.4  2002/01/09 15:37:12  ullrich
 * Bunch crossing id and spin bit extraction functions added.
 *
 * Revision 2.3  2001/07/21 00:46:54  ullrich
 * Changed nMaxBcData from 8 to 16 since table changed.
 *
 * Revision 2.2  2001/07/19 00:04:07  ullrich
 * Updated to handle new trigger info.
 *
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
class dst_TrgDet_st;
class StTriggerData;

class StL0Trigger : public StTrigger {
public:
    StL0Trigger();
    void set(const dst_L0_Trigger_st*);
    void set(const dst_TrgDet_st    *);
    void set(const StTriggerData    *);
    // StL0Trigger(const StL0Trigger&);            use default
    // StL0Trigger& operator=(const StL0Trigger&); use default
    ~StL0Trigger();

    unsigned int    coarsePixelArraySize();
    int             coarsePixelArray(unsigned int);
    int             mwcCtbMultiplicity() const;
    int             mwcCtbDipole() const;
    int             mwcCtbTopology() const;
    int             mwcCtbMoment() const;
    unsigned short  dsmInput() const;
    unsigned char   detectorBusy() const; 
    unsigned short  triggerToken() const;
    unsigned short  dsmAddress() const;  
    unsigned char   addBits() const;
    unsigned int    lastDsmArraySize() const;
    unsigned short  lastDsmArray(unsigned int);
    unsigned int    bcDataArraySize() const;
    unsigned short  bcDataArray(unsigned int);

    unsigned int bunchCrossingId7bit(int) const;
    unsigned int bunchCrossingId() const;
    
    int spinBits(int) const;
    int spinBitYellowUp(int) const;
    int spinBitYellowDown(int) const;
    int spinBitBlueUp(int) const;
    int spinBitBlueDown(int) const;

    void setMwcCtbMultiplicity(int);
    void setMwcCtbDipole(int);
    void setMwcCtbTopology(int);
    void setMwcCtbMoment(int);
    void setCoarsePixelArray(unsigned int, int);
    void setDsmInput(unsigned short);
    void setDetectorBusy(unsigned char); 
    void setTriggerToken(unsigned short);
    void setDsmAddress(unsigned short);  
    void setAddBits(unsigned char);   
    void setLastDsmArray(unsigned int, unsigned short);
    void setBcDataArray(unsigned int, unsigned short);
    
protected:
    enum {mMaxPixels = 32, mMaxLastDsm = 8, mMaxBcData = 16};

private:    
    Int_t         mCoarsePixelArray[mMaxPixels];
    Int_t         mMwcCtbMultiplicity;
    Int_t         mMwcCtbDipole;
    Int_t         mMwcCtbTopology;
    Int_t         mMwcCtbMoment;
    UShort_t      mDsmInput;
    UChar_t       mDetectorBusy; 
    UShort_t      mTriggerToken;
    UShort_t      mDsmAddress;  
    UChar_t       mAddBits;   
    UShort_t      mLastDsmArray[mMaxLastDsm];
    UShort_t      mBcDataArray[mMaxBcData];

    ClassDef(StL0Trigger,2)
};
#endif
