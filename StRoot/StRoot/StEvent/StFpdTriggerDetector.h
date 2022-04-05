/*!
 * \class StFpdTriggerDetector 
 * \author Akio Ogawa, Jul 2004
 */
/***************************************************************************
 *
 * $Id: StFpdTriggerDetector.h,v 2.2 2007/07/11 23:06:45 perev Exp $
 *
 * Author: Akio Ogawa, Jul 2004
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFpdTriggerDetector.h,v $
 * Revision 2.2  2007/07/11 23:06:45  perev
 * Cleanup+fix StXXXTriggerDetector
 *
 * Revision 2.1  2004/08/03 17:20:33  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StFpdTriggerDetector_hh
#define StFpdTriggerDetector_hh
#include "StObject.h"
#include "StEnumerations.h"

class dst_TrgDet_st;
class StTriggerData;

class StFpdTriggerDetector : public StObject {
public:
    StFpdTriggerDetector();
    StFpdTriggerDetector(const dst_TrgDet_st&);
    StFpdTriggerDetector(const StTriggerData&);
    StFpdTriggerDetector(const StFpdTriggerDetector&);           
    StFpdTriggerDetector& operator=(const StFpdTriggerDetector&);
    ~StFpdTriggerDetector();

    void clear();
    void dump() const;
    unsigned int  numberOfTowers(unsigned int nstbps) const;
    unsigned int  numberOfModules() const;
    unsigned int  numberOfLayer1Boards() const;

    unsigned int adc(StBeamDirection eastwest, unsigned int nstbps, unsigned int tower) const;
    unsigned int layer1(StBeamDirection eastwest, unsigned int nstbps, unsigned int board) const;
    unsigned int layer2(StBeamDirection eastwest, unsigned int nstbps) const;

    void setAdc(StBeamDirection eastwest, unsigned int nstbps, unsigned int tower, unsigned char v);
    void setLayer1(StBeamDirection eastwest, unsigned int nstbps, unsigned int board, unsigned short v);
    void setLayer2(StBeamDirection eastwest, unsigned int nstbps, unsigned short v);
        
protected:
    enum {mMaxNS = 49, mMaxTB = 25, mMaxPS =  7, mMaxModule = 6, mMaxBoard = 4};

private: 
    void init();

private:    
    char mBeg[1];//!
    unsigned int mMaxTower[mMaxModule];//!
    UChar_t*     mAdc[2][mMaxModule];  //!

    UChar_t  mEN[mMaxNS];
    UChar_t  mES[mMaxNS];
    UChar_t  mET[mMaxTB];
    UChar_t  mEB[mMaxTB];
    UChar_t  mEPN[mMaxPS];
    UChar_t  mEPS[mMaxPS];
    UChar_t  mWN[mMaxNS];
    UChar_t  mWS[mMaxNS];
    UChar_t  mWT[mMaxTB];
    UChar_t  mWB[mMaxTB];
    UChar_t  mWPN[mMaxPS];
    UChar_t  mWPS[mMaxPS];
    UShort_t mLayer1[2][mMaxModule][mMaxBoard];   
    UShort_t mLayer2[2][mMaxModule];   
    char mEnd[1];//!

    ClassDef(StFpdTriggerDetector,2)
};
#endif
