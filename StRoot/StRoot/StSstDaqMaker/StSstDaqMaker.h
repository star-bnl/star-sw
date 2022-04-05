/*!
 * \class StSstDaqMaker
 * \author Long Zhou,Nov 2013, according codes from Hao Qiu
 * \Initial version.
 */
/***************************************************************************
 *
 * $Id: StSstDaqMaker.h,v 1.11 2017/04/25 00:32:03 perev Exp $
 *
 * Author: Long Zhou, Nov 2013, according codes from Hao Qiu
 ***************************************************************************
 *
 * Description:
 * Read SST strips info from daq format.
 *
 ***************************************************************************
 *
 * $Log: StSstDaqMaker.h,v $
 * Revision 1.11  2017/04/25 00:32:03  perev
 * Int_t ==> UInt_t to avoid wrong cast
 *
 * Revision 1.10  2016/06/23 20:24:12  bouchet
 * sstBadStrips table decoding and use
 *
 * Revision 1.9  2016/02/03 15:50:48  zhoulong
 * Added some protection to avoid chain crash when there is no available calibration table
 *
 * Revision 1.8  2015/12/14 14:33:42  zhoulong
 * fixed CMN failed chip rejection error
 *
 * Revision 1.7  2015/11/05 21:56:44  zhoulong
 * Add pedestal subtraction and CMN suppression algorithm and did some clearence
 *
 * Revision 1.5  2015/07/06 18:53:10  bouchet
 * blanks removed to make C++ compiler happy (Thanks Yuri)
 *
 * Revision 1.4  2015/07/06 18:38:19  bouchet
 * initialization of variables and pointers (Thanks Yuri)
 *
 * Revision 1.3  2015/06/24 20:58:11  bouchet
 * added codes for using sstChipCorrect and sstMaskChip tables ; replaced StSsdConfig by StSstConfig
 *
 * Revision 1.2  2015/06/24 19:41:54  smirnovd
 * StSstDaqMaker: Removed undefined method Finish()
 *
 * Revision 1.1  2015/06/09 18:32:00  jeromel
 * Clean check-in vrsion of long time ago reviewed SST daq code
 *
 * Revision 1.11  2015/02/04 22:27:33  zhoulong
 * merged DAQ maker for Run14 and Run15.
 *
 * Revision 1.10  2015/01/10 20:18:18  zhoulong
 * 1>remove constant shift. 2>fixed delete pedestal table issue
 *
 * Revision 1.9  2015/01/05 22:07:23  smirnovd
 * StSstDaqMaker: Removed quite pointless overriding methods
 *
 * Revision 1.8  2015/01/05 21:58:56  smirnovd
 * Do not use automatic ROOT I/O as this is a StMaker. Makers are not persistent
 *
 * Revision 1.7  2015/01/05 21:58:48  smirnovd
 * StSstDaqMaker/: Updated style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
 *
 * Revision 1.6  2014/11/10 14:46:37  zhoulong
 * Fixed delete spa_strip table issue
 *
 * Revision 1.5  2014/09/30 18:00:10  zhoulong
 * Fixed alot of issue, and remove histograms, ntuple.
 *
 * Revision 1.4  2014/06/05 14:55:24  zhoulong
 * Added some code to compatible Thorsten's LC FPGA and correct readout channel shift in old LC FPGA
 *
 *
 **************************************************************************/
#ifndef STSSTDAQMAKER_H
#define STSSTDAQMAKER_H

#include "StMaker.h"
#include "StRTSBaseMaker.h"
#include "TH2.h"
#include <vector>
#include <map>
#include "TFile.h"

class StSstConfig;
class St_sstConfiguration;
class St_sstStripCalib;
class St_spa_strip;
class St_sstNoise;
class sstConfiguration_st;
class sstChipCorrect_st;
class sstStripCalib_st;
class sstBadStrips_st;
class sstNoise_st;

class StSstDaqMaker : public StRTSBaseMaker
{
public:
   StSstDaqMaker(const char *name = "sst_raw");
   virtual ~StSstDaqMaker();
   virtual Int_t InitRun(Int_t runumber);
   virtual Int_t Make();
   void Clear(const Option_t * = "");
   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StSstDaqMaker.h,v 1.11 2017/04/25 00:32:03 perev Exp $ built " __DATE__ " " __TIME__;
      return cvs;
   }

private:
   Int_t  GetHitsDataLength(int FiberNumber);
   void   DecodeRdoData();
   void   DecodeHitsData();
   void   DecodeRawWords(UInt_t *val, int vallength, int channel);
   void   DecodeRawWords_r15(UInt_t *val, int vallength, int channel);
   void   DecodeCompressedWords(UInt_t *val, int valength, int channel);
   UInt_t Mid(Int_t start, Int_t end, UInt_t input);
   Int_t  Shift(int runnumber, int &channel);
   void   FindLadderSide(int RDO, int channel, int &ladder, int &side);
   void   FindStripNumber(int &strip);
   void   FindChannelNumber(int &channel);
   void   PrintConfiguration(Int_t runumber, sstConfiguration_st *config);
   void   FillData(vector<vector<int> > vadc, vector<vector<float> > vcmnoise, Int_t id_side, Int_t ladder, Int_t valength);
   void   FillData_FixTH(vector<vector<int> > vadc, vector<vector<float> > vcmnoise, Int_t id_side, Int_t ladder, Int_t valength);

   Float_t CalculateCommonModeNoise(vector<int> vtemp);
   Float_t CalculateCommonModeNoiseSimple(vector<int> vtemp);
   Float_t CalculateCommonModeNoise_FPGA(vector<int> vtemp);
   void   FillReadOutPedTable(sstStripCalib_st *noise); /* Pedestal rms */
   void   FillDefaultReadOutPedTable();
   void   FillNoiseTable(sstNoise_st *noise); /* Intrinsic noise */
   void   FillDefaultNoiseTable();
   void   FillChipNoiseTable(sstChipCorrect_st *chipCorrectTable);
   void   FillDefaultChipNoiseTable();
   void   FillBadStripsTable(sstBadStrips_st* badStripTable);
   Int_t  idWaferToWafer(Int_t idWafer);
   Int_t  idWaferToLadderNumb(Int_t idWafer);

   Char_t              mBeg[1]; //!
   StSstConfig         *mConfig;
   sstConfiguration_st *mConfigTable;//!
   St_spa_strip        *spa_strip;
   St_sstStripCalib    *stripCal;
   /* St_sstStripCalib    *mStripCalib; */
   /* St_sstNoise         *mNoise; */
   /* St_sstChipCorrect   *mChipCorrect; */

   UInt_t *mRdoData;
   Int_t   mRdoDataLength;
   UInt_t *mHeaderData;
   UInt_t *mTrailerData;
   Int_t   mTrailerDataLength;
   UInt_t *mAdc[8];//8 Fiber data pointer
   UInt_t *mAdcHeader[8];
   UInt_t  mAdcLength[8];
   Int_t   mMode;// 0: Physical run , 1: pedestal run
   Int_t   mRdoFlag;
   Int_t   mFiberFlag[8];
   UInt_t  mFlag[8];//daq file flag.
   Int_t   mTrigger;
   Int_t   mSec;//sector number.(in Tonko's pedestal bank)
   Int_t   mFiber;//Fiber number (in Tonko's pedestal bank)
   Int_t   mPed;
   Int_t   mRms;
   Int_t   mRDO;//RDO number
   Int_t   mChannel[8];
   UInt_t  mDataMode[8];// Raw or Compressed data .
   Int_t   mEventnumber;
   Int_t   mEventrunumber;
   UInt_t  mEventTime;// Current event RHIC clock
   UInt_t  mPEventTime;// Previous event RHIC cloc
   Int_t   mCorrectFactor[1920][2]; //chip correction table.
   Int_t   mNoiseCut[1920][2]; //Reject Noise.
   Char_t  mEnd[1]; //!
   Int_t   mRunNum;
   map<Int_t, Int_t> mReadOutPed; //ReadOut Pedestal.
   map<Int_t, Float_t> mIntrinsicRms; //Intrinsic Rms
   map<Int_t, char> mBadStrip; //strips status
   //DAQ File parameters(please look at the SSD data formata document. )
   static const UInt_t  HEADER_LENGTH       = 8;
   static const UInt_t  FIBER_HEADER_LENGTH = 10;
   static const UInt_t  HEADER_TOKEN        = 0xAAAAAAAA;
   static const UInt_t  END_TOKEN           = 0xBBBBBBBB;
   static const UInt_t  TCD_TOKEN           = 0xCCCCCCCC;
   static const UInt_t  FIBER_LINK_TOKEN    = 0xDDDDDDDD;
   static const UInt_t  TCD_END_TOKEN       = 0xEEEEEEEE;
   static const UInt_t  RDO_START           = 24;//RDO Number Start bit
   static const UInt_t  RDO_END             = 28;//RDO Number End bit
   static const UInt_t  TRIG_START          = 0;//TCD trigger word start
   static const UInt_t  TRIG_END            = 20;//TCD trigger word end
   static const UInt_t  FIBER_START         = 28;//Fiber Input Start bit
   static const UInt_t  FIBER_END           = 31;//Fiber Input End bit
   static const UInt_t  HYBRID_ONE_START    = 0;//Hyirid 0 start bit
   static const UInt_t  HYBRID_ONE_END      = 10;//Hyirid 0 end bit
   static const UInt_t  HYBRID_TWO_START    = 10;
   static const UInt_t  HYBRID_TWO_END      = 20;
   static const UInt_t  HYBRID_THREE_START  = 20;
   static const UInt_t  HYBRID_THREE_END    = 30;
   static const UInt_t  COM_ADC_START       = 0;
   static const UInt_t  COM_ADC_END         = 10;
   static const UInt_t  HYBRID_START        = 10;
   static const UInt_t  HYBRID_END          = 14;
   static const UInt_t  STRIP_START         = 14;
   static const UInt_t  STRIP_END           = 24;
   static const UInt_t  ERROR_START         = 28;//CMN Err Code Start Bit
   static const UInt_t  ERROR_END           = 32;//CMN Err Code End Bit
   static const UInt_t  ADC_START           = 4;//adc length start bit
   static const UInt_t  ADC_END             = 20;//adc length end bit
   static const UInt_t  DATAMODE_START      = 0;//adc mode start bit
   static const UInt_t  DATAMODE_END        = 4;//adc mode end bit
   static const UInt_t  RAWMODE             = 0x0;//0x0 Raw data mode
   static const UInt_t  COMPRESSEDMODE      = 0x1;//0x1 ZS mode
   static const UInt_t  CMNSMODE            = 0x3;//0x3 CMN suppressed mode
   static const UInt_t  FLAG_START          = 20;//flag start bit
   static const UInt_t  FLAG_END            = 32;//flag end bit
   static const UInt_t  NORMAL              = 0x0;//0x000  //normal flag
   static const UInt_t  NODATA              = 0x1;//0x001  //no data
   static const UInt_t  OVERFLOWFLAG        = 0x2;//0x002  //over flow
   static const UInt_t  EARLY_ABORT         = 0x3;//0x003  //early abort
   static const UInt_t  WRONG_PIPE_MODE     = 0x4;//0x004  //wrong pipe mode
   static const UInt_t  ADC_LENGTH_LIMIT    = 4106;//adc length limit,include fiber header
   static const UInt_t  CMNERRORCODE        = 0xB;//CMN algorithm failed error code

   //SSD hardware parameters
   static const Int_t   nSstRdo             = 5;
   static const Int_t   nSstSide            = 2;
   static const Int_t   nSstLadder          = 20;
   static const Int_t   nSstFiberPerRdo     = 8;
   static const Int_t   nSstWaferPerLadder  = 16;
   static const Int_t   nSstChipPerWafer    = 6;
   static const Int_t   nSstStripsPerWafer  = 768;
   static const Int_t   nSstStripsPerChip   = 128;
   static const Int_t   nSigmaCut           = 4; /* N*Sigma Cuts */
   static const Int_t   nRmsCut             = 10;
   static const Int_t   RDO2LADDER[5][8];//Ladder cards number in each RDO channel .
   static const Int_t   ReadOutMap[128];
   static const Int_t   Rev_ReadOutMap[128]; // strip number 2 readout channelmap 

   ClassDef(StSstDaqMaker, 0)
};

#endif
