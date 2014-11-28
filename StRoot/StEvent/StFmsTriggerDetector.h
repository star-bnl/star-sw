/*!
 * \class StFmsTriggerDetector 
 * \author Akio Ogawa, Apr 2007
 */
/***************************************************************************
 *
 * $Id: StFmsTriggerDetector.h,v 2.6 2010/01/13 17:51:55 ullrich Exp $
 *
 * Author: Akio Ogawa, Apr 2007
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFmsTriggerDetector.h,v $
 * Revision 2.6  2010/01/13 17:51:55  ullrich
 * New clearFlag() for mudst reading, Data member mNumHeader gets //!
 *
 * Revision 2.4  2009/02/23 22:29:57  ullrich
 * Fixed problem when running over 2009 data (solution by Pibero)
 *
 * Revision 2.3  2007/12/11 18:11:13  ullrich
 * Fix bugs in QT decoding (Akio).
 *
 * Revision 2.2  2007/07/11 23:06:45  perev
 * Cleanup+fix StXXXTriggerDetector
 *
 * Revision 2.1  2007/07/02 20:21:55  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#ifndef StFmsTriggerDetector_hh
#define StFmsTriggerDetector_hh
#include "StObject.h"

class StTriggerData;

class StFmsTriggerDetector : public StObject {
public: 
    StFmsTriggerDetector();
    StFmsTriggerDetector(const StTriggerData&);
    virtual ~StFmsTriggerDetector();
    // StFmsTriggerDetector(const StFmsTriggerDetector&);            use default
    // StFmsTriggerDetector& operator=(const StFmsTriggerDetector&); use default

    void clearFlag();
 
    unsigned int   nHit() const;
    unsigned int   hit(int line) const;
    unsigned short adc(int crate,  int addr,  int dcard,  int dch);
    unsigned short tdc(int crate,  int addr,  int dcard,  int dch);

    unsigned char  DSM(int ch) const {return mDSM[ch];}
    unsigned char  DSM01(int ch) const {return mDSM01[ch];}
    unsigned char  DSM02(int ch) const {return mDSM02[ch];}
    unsigned short DSM1(int ch) const {return mDSM1[ch];}
    unsigned short DSM2(int ch) const {return mDSM2[ch];}

    void  dump();
        
protected:
    void decode();
    unsigned short getNHT(int) const;
    unsigned short getADR(int) const;
    unsigned short getCRT(int) const;
    unsigned short getADC(int) const;
    unsigned short getTDC(int) const;
    unsigned short getQT8(int) const;
    unsigned short getCHA(int) const; 
    
protected:
    enum {
      mMaxLine     = 1600,
      mMaxDSM      = 256,
      mMaxDSM01    = 112,
      mMaxDSM02    = 16,
      mMaxDSM1     = 16,
      mMaxDSM2     = 8,
      mMaxCrate    = 4,
      mMaxAddr     = 16,
      mMaxDCard    = 4,
      mMaxChan     = 8,
      mOffsetCrate = 11,
      mOffsetAddr  = 16
    }; //!
    
    char mBeg[1];//!
    UInt_t   mNumQTdata;
    UInt_t   mQTdata[mMaxLine];
    Char_t   mDSM[mMaxDSM];
    Char_t   mDSM01[mMaxDSM01];
    Char_t   mDSM02[mMaxDSM02];
    UShort_t mDSM1[mMaxDSM1];
    UShort_t mDSM2[mMaxDSM2];

    int  mNumHeader; //!
    unsigned short mADC[mMaxCrate][mMaxAddr][mMaxDCard][mMaxChan]; //!
    unsigned short mTDC[mMaxCrate][mMaxAddr][mMaxDCard][mMaxChan]; //!
    char mEnd[1];//!

    ClassDef(StFmsTriggerDetector,4)      
};

inline unsigned int   StFmsTriggerDetector::nHit() const {return mNumQTdata;} 
inline unsigned short StFmsTriggerDetector::getNHT(int v) const {return (unsigned short)  (v & 0x000000FF); }
inline unsigned short StFmsTriggerDetector::getADR(int v) const {return (unsigned short) ((v & 0x001F0000) >> 16);}
inline unsigned short StFmsTriggerDetector::getCRT(int v) const {return (unsigned short) ((v & 0xFF000000) >> 24);}
inline unsigned short StFmsTriggerDetector::getADC(int v) const {return (unsigned short)  (v & 0x00000FFF);}
inline unsigned short StFmsTriggerDetector::getTDC(int v) const {return (unsigned short) ((v & 0x001F0000) >> 16);}
inline unsigned short StFmsTriggerDetector::getQT8(int v) const {return (unsigned short) ((v & 0x18000000) >> 27);}
inline unsigned short StFmsTriggerDetector::getCHA(int v) const {return (unsigned short) ((v & 0xE0000000) >> 29);} 

#endif
