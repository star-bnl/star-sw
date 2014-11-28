/*!
 * \class StMwcTriggerDetector 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StMwcTriggerDetector.h,v 2.5 2007/07/11 23:06:45 perev Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMwcTriggerDetector.h,v $
 * Revision 2.5  2007/07/11 23:06:45  perev
 * Cleanup+fix StXXXTriggerDetector
 *
 * Revision 2.4  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/04/05 04:00:39  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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

    unsigned int   numberOfSectors() const;
    unsigned int   numberOfSubSectors() const;
    unsigned int   numberOfPreSamples() const;
    unsigned int   numberOfPostSamples() const;
    unsigned int   numberOfAuxWords() const;

    float  mips(unsigned int sec, unsigned int subsec, unsigned int evt = 0) const;
    float  aux(unsigned int, unsigned int evt = 0) const;

    void setMips(unsigned int, unsigned int, unsigned int, float);
    void setAux(unsigned int, unsigned int, float);
    void setNumberOfPreSamples(unsigned int);
    void setNumberOfPostSamples(unsigned int);
    
protected:
    enum {mMaxSectors = 24,
          mMaxSubSectors = 4,
          mMaxEventSamples = 11,
          mMaxAux = 32};
    char mBeg[1];//!
    Float_t  mMips[mMaxSectors][mMaxSubSectors][mMaxEventSamples];
    Float_t  mAux[mMaxAux][mMaxEventSamples];
    Int_t    mNumberOfPreSamples;
    Int_t    mNumberOfPostSamples;
    char mEnd[1];//!
    
    ClassDef(StMwcTriggerDetector,2)
};
#endif
