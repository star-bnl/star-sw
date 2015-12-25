/***************************************************************************
*
* $Id: StIstRawHitMaker.h,v 1.16 2015/12/25 03:51:20 smirnovd Exp $
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
***************************************************************************/

#ifndef StIstRawHitMaker_hh
#define StIstRawHitMaker_hh

#include "StRoot/St_base/Stypes.h"
#include "StRoot/StChain/StRTSBaseMaker.h"
#include "StIstUtil/StIstConsts.h"

#include <vector>
#include <string>
#include <list>
#include <map>
#include <assert.h>

class StIstCollection;

class StIstRawHitMaker : public StRTSBaseMaker
{
public:
   StIstRawHitMaker( const char *name = "ist_raw_hit" );
   ~StIstRawHitMaker();
   Int_t Init();
   Int_t InitRun(Int_t runNumber);
   Int_t Make();
   void Clear( Option_t *opts = "" );

   void setIsCalibrationMode( bool isCaliMode = false )   { mIsCaliMode = isCaliMode;};
   void setHitCut(float hitCut = 5.)			{ mHitCut = hitCut;        };
   void setCmnCorrection( bool doCmnCorrection = false )	{ mDoCmnCorrection = doCmnCorrection; };
   void setCmnCut(float cmnCut = 3.)			{ mCmnCut = cmnCut;        };
   /// 0 - All data; 1 - non-ZS data; 2 - ZS data; 3 - ZS first data
   void setDataType(int nDataType = 0)		{ mDataType = nDataType;   };
   void setDoEmbedding(Bool_t doIt) {mDoEmbedding = doIt;}
   UChar_t getDataType() {return mDataType;} // 0 - All data; 1 - non-ZS data; 2 - ZS data; 3 - ZS first data

   // Get CVS
   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StIstRawHitMaker.h,v 1.16 2015/12/25 03:51:20 smirnovd Exp $ built " __DATE__ " " __TIME__  ;
      return cvs;
   }

protected:
   Bool_t mIsCaliMode;
   Bool_t mDoEmbedding;
   Bool_t mDoCmnCorrection;
   //control paramters
   Float_t mHitCut, mCmnCut, mChanMinRmsNoiseLevel, mChanMaxRmsNoiseLevel, mApvMaxCmNoiseLevel;
   UChar_t mALLdata, mADCdata, mZSdata, mDefaultTimeBin, mCurrentTimeBinNum;
   UShort_t mMinNumOfRawHits, mMaxNumOfRawHits;

   StIstCollection *mIstCollectionPtr;
   StIstCollection *mIstCollectionSimuPtr; //raw ADC container from simu data

   std::vector< float > mCmnVec; ///< APV chip geom. index, common mode (CM) noise
   std::vector< float > mPedVec; ///< Channel elec. index, pedestal
   std::vector< float > mRmsVec; ///< Channel elec. index, RMS noise
   std::vector< float > mGainVec; ///< Channel elec. index, gain
   std::vector< int > mMappingVec; ///< Channel elec. index to geometry ID mapping
   std::vector< unsigned char > mConfigVec; ///< APV chip configuration status indexed by geom. id

private:

   void FillRawHitCollectionFromAPVData(unsigned char dataFlag, int ntimebin, int counterAdcPerEvent[], float sumAdcPerEvent[], int apvElecId,
      int (&signalUnCorrected)[kIstNumApvChannels][kIstNumTimeBins],
      float (&signalCorrected)[kIstNumApvChannels][kIstNumTimeBins]);
   void FillRawHitCollectionFromSimData();

   Int_t mDataType; ///<  0=all, 1=adc only, 2=zs only

   ClassDef(StIstRawHitMaker, 0);
};

#endif


/***************************************************************************
*
* $Log: StIstRawHitMaker.h,v $
* Revision 1.16  2015/12/25 03:51:20  smirnovd
* Squashed commit of the following:
*
* Author: bingchuhuang <bingchuhuang@gmail.com>
* Date:   Wed Dec 9 17:01:39 2015 -0500
*
*     StIstRawHitMaker: Added method to fill hits in pure simulation mode
*
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Tue Dec 8 11:30:39 2015 -0500
*
*     Changes in whitespace only
*
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Tue Dec 8 11:16:14 2015 -0500
*
*     StIstRawHitMaker: Merge simulated ADCs with real data
*
* Author: bingchuhuang <bingchuhuang@gmail.com>
* Date:   Sun Dec 6 23:49:18 2015 -0500
*
*     StIstRawHitMaker: Access collection with simulated ADCs
*
*     The collection of simulated ADCs is normally created by StIstSlowSimMaker who
*     saves it to a globaly accessible dataset ("istRawAdcSimu"). Here we check if
*     such dataset exists and set a pointer to access the collection. The data will be
*     merged with real data ADC values
*
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Mon Dec 7 22:05:06 2015 -0500
*
*     StIstRawHitMaker: Refactor Make(), Define apvId for shorthand
*
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Tue Dec 8 10:49:32 2015 -0500
*
*     StIstRawHitMaker: Refactor Make(), Changed continue to return in the new function
*
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Fri Dec 11 15:07:36 2015 -0500
*
*     StIstRawHitMaker: Refactor Make(), Split long function
*
*     Moved portion of Make() to a separate routine FillRawHitCollectionFromAPVData()
*     This is done in preparation for easier merging of simulated and real ADC data
*     for embedding studies
*
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Fri Dec 4 17:53:14 2015 -0500
*
*     StIstRawHitMaker: Refactor Make(), Zero-initialize arrays with memset
*
* Author: bingchuhuang <bingchuhuang@gmail.com>
* Date:   Fri Dec 4 11:03:33 2015 -0500
*
*     StIstRawHitMaker: Allow public access to mDataType member
*
*     Signed-off-by: Dmitri Smirnov <d.s@plexoos.com>
*
* Author: bingchuhuang <bingchuhuang@gmail.com>
* Date:   Fri Dec 4 10:46:13 2015 -0500
*
*     StIstRawHitMaker: Added member pointer to simulated collection of channel ADC
*
*     Signed-off-by: Dmitri Smirnov <d.s@plexoos.com>
*
* Author: bingchuhuang <bingchuhuang@gmail.com>
* Date:   Fri Dec 4 10:42:21 2015 -0500
*
*     StIstRawHitMaker: Added class member flag for processing embedding
*
*     Signed-off-by: Dmitri Smirnov <d.s@plexoos.com>
*
* Revision 1.15  2015/07/27 18:51:01  huangbc
* Add space before and after "__DATE__" and "__TIME__" for compling under gcc4.8.2
*
* Revision 1.14  2014/10/13 22:33:05  smirnovd
* Minor adjustments to the code and comments
*
* Revision 1.13  2014/09/17 20:33:32  smirnovd
* Squashed commit of the following:
*
* commit 72dc19a6663ea31c719c1a61f6d2b4752dd766aa
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:34:42 2014 -0400
*
*     Minor code refactoring, clean up
*
* commit e083a10a9fb60b7dcce692ef8043b9227c12768b
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:18:16 2014 -0400
*
*     Removed pointless comments
*
* commit 88d51857362c91c954704cec4a31a0b0fa7fccc5
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:17:26 2014 -0400
*
*     Updated description in doxygen comments
*
* commit eb09527489179fc7dab6aa7f23fd132b25185bb1
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Tue Sep 9 15:15:56 2014 -0400
*
*     StIstScanClusterAlgo: Removed unused variable
*
* commit 1a8df63533c71a0e2ba4d8275ebf89f4e3004765
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Fri Aug 22 16:04:47 2014 -0400
*
*     Neatened headers: Removed unused, spelled paths in includes explicitly as it slightly helps in identifying dependencies
*
* commit 972e8ed41403bd680ade5ecc509f8bca004e86ee
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Wed Sep 17 12:34:20 2014 -0400
*
*     Minor stylistic changes
*
* commit 57daf5a1e0b3246fd12f1dd1c2ca089b62930c83
* Author: Dmitri Smirnov <d.s@plexoos.com>
* Date:   Tue Sep 16 16:29:14 2014 -0400
*
*     Improved doxygen comments
*
* Revision 1.12  2014/09/07 07:40:51  ypwang
* the mIstDb was declared as a local variable in InitRun() in stead of as a data member
*
* Revision 1.11  2014/09/07 07:14:48  ypwang
* update definition method for the calibration vectors (remove the multiple typedefs)
*
* Revision 1.10  2014/09/07 06:55:51  ypwang
* remove an unnecessary ierr cut in Make() function, and formatted with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.9  2014/08/22 21:27:20  smirnovd
* Remove inline keyword and move the methods inside the definition. Let the compiler optimize the code as it should not be a problem with these one-liners
*
* Revision 1.8  2014/08/22 15:55:16  smirnovd
* Fixed style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.7  2014/08/21 17:51:08  smirnovd
* Moved CVS history to the end of file
*
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
