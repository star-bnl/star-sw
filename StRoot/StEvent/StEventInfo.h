/*!
 * \class StEventInfo 
 * \author Thomas Ullrich, Jun 2000
 */
/***************************************************************************
 *
 * $Id: StEventInfo.h,v 2.6 2009/11/23 16:34:06 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jun 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventInfo.h,v $
 * Revision 2.6  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.5  2002/02/22 22:56:47  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.4  2001/09/19 04:48:08  ullrich
 * Added event size.
 *
 * Revision 2.3  2001/04/05 04:00:36  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2000/09/06 22:34:24  ullrich
 * Changed mBunchCrossingNumber from scalar to array to hold all 64 bits.
 *
 * Revision 2.1  2000/06/19 01:32:16  perev
 *  Thomas StEvent branches added
 *
 **************************************************************************/
#ifndef StEventInfo_hh
#define StEventInfo_hh

#include "StObject.h"
#include "TString.h"
class event_header_st;

class StEventInfo : public StObject {
public:
    StEventInfo();
    virtual ~StEventInfo();

    const TString& type() const;
    int            id() const;
    int            runId() const;
    int            time() const;
    unsigned int   triggerMask() const;
    unsigned int   bunchCrossingNumber(unsigned int) const;
    unsigned int   eventSize() const;
     
    void setType(const char*);
    void setRunId(int);
    void setId(int);
    void setTime(int);
    void setTriggerMask(unsigned int);
    void setBunchCrossingNumber(unsigned int, unsigned int);
    void setEventSize(unsigned int);
    
protected:
    TString  mType;
    Int_t    mRunId;
    Int_t    mId;
    Int_t    mTime;
    UInt_t   mTriggerMask;
    UInt_t   mBunchCrossingNumber[2];
    UInt_t   mEventSize;

    ClassDef(StEventInfo,2)
};
#endif
