/***************************************************************************
 *
 * $Id: StMwcTriggerDetector.h,v 2.2 2000/05/09 10:22:28 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMwcTriggerDetector.h,v $
 * Revision 2.2  2000/05/09 10:22:28  ullrich
 * Updated to cope with modified dst_TrgDet.idl
 *
 * Revision 2.1  1999/10/13 19:43:29  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StMwcTriggerDetector_hh
#define StMwcTriggerDetector_hh
#include "StObject.h"

class dst_TrgDet_st;

class StMwcTriggerDetector : public StObject {
public:
    StMwcTriggerDetector();
    StMwcTriggerDetector(const dst_TrgDet_st&);
    // StMwcTriggerDetector(const StMwcTriggerDetector&);            use default
    // StMwcTriggerDetector& operator=(const StMwcTriggerDetector&); use default
    virtual ~StMwcTriggerDetector();

    UInt_t   numberOfSectors() const;
    UInt_t   numberOfSubSectors() const;
    UInt_t   numberOfPreSamples() const;
    UInt_t   numberOfPostSamples() const;
    UInt_t   numberOfAuxWords() const;

    Float_t  mips(UInt_t sec, UInt_t subsec, UInt_t evt = 0) const;
    Float_t  aux(UInt_t, UInt_t evt = 0) const;

    void setMips(UInt_t, UInt_t, UInt_t, Float_t);
    void setAux(UInt_t, UInt_t, Float_t);
    void setNumberOfPreSamples(UInt_t);
    void setNumberOfPostSamples(UInt_t);
    
protected:
    enum {mMaxSectors = 24,
	  mMaxSubSectors = 4,
	  mMaxEventSamples = 11,
	  mMaxAux = 32};
    Float_t  mMips[mMaxSectors][mMaxSubSectors][mMaxEventSamples];
    Float_t  mAux[mMaxAux][mMaxEventSamples];
    Int_t    mNumberOfPreSamples;
    Int_t    mNumberOfPostSamples;
    
    ClassDef(StMwcTriggerDetector,1)
};
#endif
