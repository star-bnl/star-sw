/***************************************************************************
 *
 * $Id: StEventInfo.h,v 2.1 2000/06/19 01:32:16 perev Exp $
 *
 * Author: Thomas Ullrich, Jun 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventInfo.h,v $
 * Revision 2.1  2000/06/19 01:32:16  perev
 * Thomas StEvent branches added
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
    StEventInfo(const event_header_st&);
    // StEventInfo(const StEventInfo&);            use default
    // StEventInfo& operator=(const StEventInfo&); use default
    virtual ~StEventInfo();

    const TString& type() const;
    Long_t         id() const;
    Long_t         runId() const;
    Long_t         time() const;
    ULong_t        triggerMask() const;
    ULong_t        bunchCrossingNumber() const;
     
    void setType(const Char_t*);
    void setRunId(Long_t);
    void setId(Long_t);
    void setTime(Long_t);
    void setTriggerMask(ULong_t);
    void setBunchCrossingNumber(ULong_t);
    
protected:
    TString  mType;
    Long_t   mRunId;
    Long_t   mId;
    Long_t   mTime;
    ULong_t  mTriggerMask;
    ULong_t  mBunchCrossingNumber;

    ClassDef(StEventInfo,1)
};
#endif
