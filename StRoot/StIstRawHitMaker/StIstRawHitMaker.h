/***************************************************************************
*
* $Id: StIstRawHitMaker.h,v 1.1 2014/01/23 20:11:30 ypwang Exp $
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
* Revision 1.1  2014/01/23 20:11:30  ypwang
* adding scripts
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
class StIstDbMaker;
class St_istPedNoise;
class St_istGain;
class St_istMapping;

class StIstRawHitMaker : public StRTSBaseMaker {
 public: 
   StIstRawHitMaker( const char* name = "ist_raw_hit" );
   virtual ~StIstRawHitMaker();
   virtual Int_t Init();
   virtual Int_t InitRun(Int_t runNumber);
   virtual Int_t Make();
   virtual void Clear( Option_t *opts = "" );

   void setIsCalibrationMode( bool isCaliMode = 0);
   void setHitCut( float hitCut = 5.0);
   void setCmnCorrection(bool doCmnCorrection = 0);
   void setCmnCut(float cmnCut = 3.0);
   void setDataType(int nDataType = 0); // 0 - All data; 1 - non-ZS data; 2 - ZS data; 3 - ZS first data

   // Get CVS
   virtual const char *GetCVS() const;

 protected:
   Bool_t mIsCaliMode, mDoCmnCorrection;
   Float_t mHitCut, mCmnCut, mChanMinRmsNoiseLevel;
   UChar_t mALLdata, mADCdata, mZSdata, mDefaultTimeBin;

   StIstCollection *mIstCollectionPtr;
   StIstDbMaker *mIstDbMaker;

   // common mode noise
   typedef std::vector< float > CmnVec_t; //APV chip index, CM noise
   CmnVec_t mCmnVec;
   // pedestal
   typedef std::vector< float > PedVec_t; //Channel index, pedestal
   PedVec_t mPedVec;
   // RMS noise
   typedef std::vector< float > RmsVec_t; //Channel index, RMS noise
   RmsVec_t mRmsVec;
   // gain
   typedef std::vector< float > GainVec_t;//Channel index, gain
   GainVec_t mGainVec;
   // mapping
   typedef std::vector< int > MappingVec_t;//Channel index, geometry ID
   MappingVec_t mMappingVec;

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
   static const char cvs[] = "Tag $Name:  $ $Id: StIstRawHitMaker.h,v 1.1 2014/01/23 20:11:30 ypwang Exp $ built "__DATE__" "__TIME__ ;
   return cvs;
};
#endif
