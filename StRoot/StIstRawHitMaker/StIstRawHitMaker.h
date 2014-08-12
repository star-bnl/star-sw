/***************************************************************************
*
* $Id: StIstRawHitMaker.h,v 1.6 2014/08/12 23:00:02 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* Reads/un-pack a DAQ or SFS file; Accesses calibration DBs;
* Marks out bad channels/chips; Creates and fills the IST containers.
* Two working modes included: 
* 1) calibration mode: save raw ADC value only 
* 2) non-calibration (physics) mode: access calibration datasets, signal-like
*    raw hit decision, full raw hit info., ...
****************************************************************************
*
* $Log: StIstRawHitMaker.h,v $
* Revision 1.6  2014/08/12 23:00:02  ypwang
* chip occupancy cut added to skip the chip with more than 20% channels fired; change the raw hit decision cut position in the code.
*
* Revision 1.5  2014/07/29 20:13:31  ypwang
* update the IST DB obtain method
*
* Revision 1.4  2014/02/15 19:55:25  ypwang
* remove virtual type declaration from member function
*
* Revision 1.3  2014/02/08 03:34:17  ypwang
* updating scripts
*
*
****************************************************************************
* StIstRawHitMaker.h,v 1.0
* Revision 1.0 2013/11/04 15:55:30 Yaping
* Initial version
****************************************************************************/

#ifndef StIstRawHitMaker_hh
#define StIstRawHitMaker_hh

#include "StRoot/St_base/Stypes.h"
#include "StRoot/StChain/StRTSBaseMaker.h"

#include <vector>
#include <string>
#include <list>
#include <map>
#include <assert.h>

class StIstCollection;
class StIstDb;

class StIstRawHitMaker : public StRTSBaseMaker {
 public: 
   StIstRawHitMaker( const char* name = "ist_raw_hit" );
   ~StIstRawHitMaker();
   Int_t Init();
   Int_t InitRun(Int_t runNumber);
   Int_t Make();
   void Clear( Option_t *opts = "" );

   void setIsCalibrationMode( bool isCaliMode = 0);
   void setHitCut( float hitCut = 5.0);
   void setCmnCorrection(bool doCmnCorrection = 0);
   void setCmnCut(float cmnCut = 3.0);
   void setDataType(int nDataType = 0); // 0 - All data; 1 - non-ZS data; 2 - ZS data; 3 - ZS first data

   // Get CVS
   virtual const char *GetCVS() const;

 protected:
   Bool_t mIsCaliMode, mDoCmnCorrection;
   //control paramters
   Float_t mHitCut, mCmnCut, mChanMinRmsNoiseLevel, mChanMaxRmsNoiseLevel, mApvMaxCmNoiseLevel;
   UChar_t mALLdata, mADCdata, mZSdata, mDefaultTimeBin, mCurrentTimeBinNum;
   UShort_t mMinNumOfRawHits, mMaxNumOfRawHits;

   StIstCollection *mIstCollectionPtr;
   StIstDb *mIstDb;

   // common mode noise
   typedef std::vector< float > CmnVec_t; //APV chip geom. index, CM noise
   CmnVec_t mCmnVec;
   // pedestal
   typedef std::vector< float > PedVec_t; //Channel elec. index, pedestal
   PedVec_t mPedVec;
   // RMS noise
   typedef std::vector< float > RmsVec_t; //Channel elec. index, RMS noise
   RmsVec_t mRmsVec;
   // gain
   typedef std::vector< float > GainVec_t;//Channel elec. index, gain
   GainVec_t mGainVec;
   // mapping
   typedef std::vector< int > MappingVec_t;//Channel elec. index, geometry ID
   MappingVec_t mMappingVec;
   // chip configuration status
   typedef std::vector< unsigned char > ConfigVec_t; //APV chip geom. index, configuration status
   ConfigVec_t mConfigVec;

 private:
   Int_t mDataType; //!  0=all, 1=adc only, 2=zs only

   ClassDef(StIstRawHitMaker,1);
};

// inline functions
inline void StIstRawHitMaker::setIsCalibrationMode( bool isCaliMode )   { mIsCaliMode = isCaliMode;};
inline void StIstRawHitMaker::setHitCut(float hitCut)			{ mHitCut = hitCut;        };
inline void StIstRawHitMaker::setCmnCorrection( bool doCmnCorrection )	{ mDoCmnCorrection = doCmnCorrection; };
inline void StIstRawHitMaker::setCmnCut(float cmnCut)			{ mCmnCut = cmnCut;        };
inline void StIstRawHitMaker::setDataType(int nDataType)		{ mDataType = nDataType;   };

inline const char *StIstRawHitMaker::GetCVS() const {
   static const char cvs[] = "Tag $Name:  $ $Id: StIstRawHitMaker.h,v 1.6 2014/08/12 23:00:02 ypwang Exp $ built "__DATE__" "__TIME__ ;
   return cvs;
};
#endif
