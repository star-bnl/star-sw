/*!
 * \class StCtbTriggerDetector 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StCtbTriggerDetector.h,v 2.7 2007/07/11 23:06:45 perev Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbTriggerDetector.h,v $
 * Revision 2.7  2007/07/11 23:06:45  perev
 * Cleanup+fix StXXXTriggerDetector
 *
 * Revision 2.6  2004/02/11 01:42:09  ullrich
 * Added new constructor to load data from StTriggerData.
 *
 * Revision 2.5  2002/11/19 20:21:00  ullrich
 * Added method to sum all mips.
 *
 * Revision 2.4  2002/02/22 22:56:46  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/04/05 04:00:34  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
class StTriggerData;

class StCtbTriggerDetector : public StObject {
public:
    StCtbTriggerDetector();
    StCtbTriggerDetector(const dst_TrgDet_st&);
    StCtbTriggerDetector(const StTriggerData&);
    virtual ~StCtbTriggerDetector();
    // StCtbTriggerDetector(const StCtbTriggerDetector&);            use default
    // StCtbTriggerDetector& operator=(const StCtbTriggerDetector&); use default
    
    unsigned int   numberOfTrays() const;
    unsigned int   numberOfSlats() const;
    unsigned int   numberOfPreSamples() const;
    unsigned int   numberOfPostSamples() const;
    unsigned int   numberOfAuxWords() const;
    float          mips(unsigned int tray, unsigned int slot, unsigned int evt = 0) const;
    char           time(unsigned int tray, unsigned int slot, unsigned int evt = 0) const;
    float          aux(unsigned int, unsigned int evt = 0) const;

    double         mips(unsigned int evt = 0) const;     // sum over all trays, slot

    void setMips(unsigned int, unsigned int, unsigned int, float);
    void setTime(unsigned int, unsigned int, unsigned int, char);
    void setAux(unsigned int, unsigned int, float);
    void setNumberOfPreSamples(unsigned int);
    void setNumberOfPostSamples(unsigned int);
    
protected:
    enum {mMaxTrays = 120,
          mMaxSlats = 2,
          mMaxEventSamples = 11,
          mMaxAux = 16};
    char mBeg[1];//!
    Float_t  mMips[mMaxTrays][mMaxSlats][mMaxEventSamples];
    Char_t   mTime[mMaxTrays][mMaxSlats][mMaxEventSamples];
    Float_t  mAux[mMaxAux][mMaxEventSamples];
    Int_t    mNumberOfPreSamples;
    Int_t    mNumberOfPostSamples;
    char mEnd[1];//!
    
    ClassDef(StCtbTriggerDetector,2)
};
#endif
