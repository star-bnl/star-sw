/***************************************************************************
 *
 * $Id: StCtbTriggerDetector.h,v 2.2 2000/05/09 10:22:21 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbTriggerDetector.h,v $
 * Revision 2.2  2000/05/09 10:22:21  ullrich
 * Updated to cope with modified dst_TrgDet.idl
 *
 * Revision 2.1  1999/10/13 19:42:56  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StCtbTriggerDetector_hh
#define StCtbTriggerDetector_hh
#include "StObject.h"

class dst_TrgDet_st;

class StCtbTriggerDetector : public StObject {
public:
    StCtbTriggerDetector();
    StCtbTriggerDetector(const dst_TrgDet_st&);
    virtual ~StCtbTriggerDetector();
    // StCtbTriggerDetector(const StCtbTriggerDetector&);            use default
    // StCtbTriggerDetector& operator=(const StCtbTriggerDetector&); use default
    
    UInt_t   numberOfTrays() const;
    UInt_t   numberOfSlats() const;
    UInt_t   numberOfPreSamples() const;
    UInt_t   numberOfPostSamples() const;
    UInt_t   numberOfAuxWords() const;
    Float_t  mips(UInt_t tray, UInt_t slot, UInt_t evt = 0) const;
    Char_t   time(UInt_t tray, UInt_t slot, UInt_t evt = 0) const;
    Float_t  aux(UInt_t, UInt_t evt = 0) const;

    void setMips(UInt_t, UInt_t, UInt_t, Float_t);
    void setTime(UInt_t, UInt_t, UInt_t, Char_t);
    void setAux(UInt_t, UInt_t, Float_t);
    void setNumberOfPreSamples(UInt_t);
    void setNumberOfPostSamples(UInt_t);
    
protected:
    enum {mMaxTrays = 120,
	  mMaxSlats = 2,
	  mMaxEventSamples = 11,
	  mMaxAux = 16};
    Float_t  mMips[mMaxTrays][mMaxSlats][mMaxEventSamples];
    Char_t   mTime[mMaxTrays][mMaxSlats][mMaxEventSamples];
    Float_t  mAux[mMaxAux][mMaxEventSamples];
    Int_t    mNumberOfPreSamples;
    Int_t    mNumberOfPostSamples;
    
    ClassDef(StCtbTriggerDetector,1)
};
#endif
