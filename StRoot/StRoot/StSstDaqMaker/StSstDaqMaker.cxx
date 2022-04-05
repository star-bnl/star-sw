/*!
 * \class StSstDaqMaker
 * \author Long Zhou, Nov 2013, according codes from PXL
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StSstDaqMaker.cxx,v 1.16 2017/11/09 16:23:57 smirnovd Exp $
 *
 * Author: Long Zhou, Nov 2013
 ***************************************************************************
 *
 * Description:
 * Read ssd strip info from daq format.
 *
 *
 ***************************************************************************
 *
 * $Log: StSstDaqMaker.cxx,v $
 * Revision 1.16  2017/11/09 16:23:57  smirnovd
 * StSstDaqMaker: Corrected path to included header file
 *
 * Revision 1.15  2017/04/26 14:56:42  perev
 * Remove GetData() commited by accidant
 *
 * Revision 1.13  2016/07/01 18:30:52  bouchet
 * COVERITY : STACK_USE, UNINIT_CTOR fixed
 *
 * Revision 1.12  2016/06/23 20:23:34  bouchet
 * sstBadStrips table decoding and use ; COVERITY : DIVIDE_BY_ZERO, NO_EFFECT fixed
 *
 * Revision 1.11  2016/05/25 15:47:38  smirnovd
 * StSstDaqMaker: Removed commented-out code destined to rot
 *
 * Revision 1.10  2016/05/25 15:47:30  smirnovd
 * StSstDaqMaker: Initializing variable once is enough
 *
 * Revision 1.9  2016/05/25 15:47:23  smirnovd
 * StSstDaqMaker: Got rid of unused local variable
 *
 * Revision 1.8  2016/05/25 15:47:15  smirnovd
 * StSstDaqMaker: Refactored how output file name is formed
 *
 * Memory leak fixed. Note different width for time field for values less < 999
 *
 * Revision 1.7  2016/02/03 15:50:20  zhoulong
 * Added some protection to avoid chain crash when there is no available calibration table
 *
 * Revision 1.6  2015/12/14 14:33:42  zhoulong
 * fixed CMN failed chip rejection error
 *
 * Revision 1.5  2015/11/05 21:56:44  zhoulong
 * Add pedestal subtraction and CMN suppression algorithm and did some clearence
 *
 * Revision 1.4  2015/07/06 18:38:19  bouchet
 * initialization of variables and pointers (Thanks Yuri)
 *
 * Revision 1.3  2015/06/24 20:58:11  bouchet
 * added codes for using sstChipCorrect and sstMaskChip tables ; replaced StSsdConfig by StSstConfig
 *
 * Revision 1.2  2015/06/24 20:37:59  smirnovd
 * Use explicit comparison in order to disable compiler warning
 *
 * Revision 1.1  2015/06/09 18:32:00  jeromel
 * Clean check-in vrsion of long time ago reviewed SST daq code
 *
 * Revision 1.11  2015/02/02 22:58:31  zhoulong
 * Corrected the default ordering issue
 *
 * Revision 1.10  2015/01/10 20:18:18  zhoulong
 * 1>remove constant shift. 2>fixed delete pedestal table issue
 *
 * Revision 1.9  2015/01/05 22:07:31  smirnovd
 * StSstDaqMaker: Use STAR framework return codes
 *
 * In this case we better return an error code
 *
 * Revision 1.8  2015/01/05 22:07:23  smirnovd
 * StSstDaqMaker: Removed quite pointless overriding methods
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
 * Revision 1.4  2014/06/05 14:55:23  zhoulong
 * Added some code to compatible Thorsten's LC FPGA and correct readout channel shift in old LC FPGA
 *
 *
 ***************************************************************************
 * StSstDaqMaker.cxx v 1.1
 * Revision 1.1 2014/1/23 11:18:00 Long Zhou
 * <1>Change Sector to Rdo
 * <2>change the main structure(depend on test DAQ data sample.)
 *
 * StsstDaqMaker.cxx,v 1.0
 * Revision 1.0 2014/1/21 10:46:00 Long Zhou
 * Initial version
 ****************************************************************************/
#include "StSstDaqMaker.h"
#include "St_base/StMessMgr.h"
#include "RTS/src/DAQ_SST/daq_sst.h"
#include "RTS/src/DAQ_READER/daq_dta.h"
#include "RTS/src/DAQ_READER/daqReader.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StRtsTable.h"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StSsdDbMaker/StSstDbMaker.h"
#include "tables/St_spa_strip_Table.h"
#include "tables/St_sstConfiguration_Table.h"
#include "tables/St_sstStripCalib_Table.h"
#include "StSstUtil/StSstConfig.hh"
#include "StIOMaker/StIOMaker.h"
#include "tables/St_sstChipCorrect_Table.h"
#include "tables/St_sstNoise_Table.h"
#include "tables/St_sstBadStrips_Table.h"
#include <map>
#include <vector>
using std::vector;
ClassImp(StSstDaqMaker)

//-----------------------------------------------
//Ladder cards number in each RDO channel.
const Int_t StSstDaqMaker::RDO2LADDER[5][8] = { {1, 2, 3, 4, 5, 6, 7, 8},
   {9, 10, 11, 12, 13, 14, 15, 16},
   {17, 18, 19, 20, 1, 2, 3, 4},
   {5, 6, 7, 8, 9, 10, 11, 12},
   {13, 14, 15, 16, 17, 18, 19, 20}
};

//silicon strip number ordered by ALICE128 readout order
const Int_t StSstDaqMaker::ReadOutMap[128] = {
   97, 96, 98, 95, 99, 94, 100, 93,
   101, 92, 102, 91, 103, 90, 104, 89,
   105, 88, 106, 87, 107, 86, 108, 85,
   109, 84, 110, 83, 111, 82, 112, 81,
   113, 80, 114, 79, 115, 78, 116, 77,
   117, 76, 118, 75, 119, 74, 120, 73,
   121, 72, 122, 71, 123, 70, 124, 69,
   125, 68, 126, 67, 127, 66, 128, 65,
   1, 64, 2, 63, 3, 62, 4, 61,
   5, 60, 6, 59, 7, 58, 8, 57,
   9, 56, 10, 55, 11, 54, 12, 53,
   13, 52, 14, 51, 15, 50, 16, 49,
   17, 48, 18, 47, 19, 46, 20, 45,
   21, 44, 22, 43, 23, 42, 24, 41,
   25, 40, 26, 39, 27, 38, 28, 37,
   29, 36, 30, 35, 31, 34, 32, 33
};
//-------------------------------------------------
//table of ALICE128 readout order ordered by silicon strip number
const Int_t StSstDaqMaker::Rev_ReadOutMap[128] = {
  65,  67,  69,  71,  73,  75,  77,  79,
  81,  83,  85,  87,  89,  91,  93,  95,
  97,  99,  101, 103, 105, 107, 109, 111,
  113, 115, 117, 119, 121, 123, 125, 127,
  128, 126, 124, 122, 120, 118, 116, 114,
  112, 110, 108, 106, 104, 102, 100, 98,
  96,  94,  92,	 90,  88,  86,  84,  82,
  80,  78,  76,	 74,  72,  70,  68,  66,
  64,  62,  60,	 58,  56,  54,  52,  50,
  48,  46,  44,	 42,  40,  38,  36,  34,
  32,  30,  28,	 26,  24,  22,  20,  18,
  16,  14,  12,	 10,  8,   6,   4,   2,
  1,   3,   5,   7,   9,   11,  13,  15,
  17,  19,  21,  23,  25,  27,  29,  31,
  33,  35,  37,  39,  41,  43,  45,  47,
  49,  51,  53,  55,  57,  59,  61,  63
};
//-----------------------------------------------
StSstDaqMaker::StSstDaqMaker(const Char_t *name)
  : StRTSBaseMaker("sst", name),mMode(1)
{
   mConfig=0;
   mConfigTable=0;
   spa_strip=0;
   stripCal=0;
   mRdoData=0;
   mRdoDataLength=0;
   mHeaderData=0;
   mTrailerData=0;
   mTrailerDataLength=0;
   mMode=0;
   mRdoFlag=0;
   mTrigger=0;
   mSec=0;
   mFiber=0;
   mPed=0;
   mRms=0;
   mRDO=0;
   mEventnumber=0;
   mEventrunumber=0;
   mEventTime=0;
   mPEventTime=0;
   mRunNum=0;
   for(int i=0;i<8;i++){
     mAdc[i] = 0;
     mAdcHeader[i] = 0;
     mAdcLength[i] = 0;
     mFiberFlag[i] = 0;
     mFlag[i] = 0;
     mChannel[i] = 0;
     mDataMode[i] = 0;
   }
}
//-----------------------------------------------
StSstDaqMaker::~StSstDaqMaker()
{
}
//-----------------------------------------------
Int_t StSstDaqMaker::InitRun(Int_t runumber)
{
   mEventrunumber = runumber;

   LOG_INFO << "InitRun(Int_t runumber) - Read now Databases" << endm;
   mRunNum = (runumber / 1000000) - 1;
   stripCal = new St_sstStripCalib("sstStripCalib",1);

   if (mRunNum >= 14) {		// For Run14,15,16 Raw Data Mode
     St_sstStripCalib *mStripCalib = (St_sstStripCalib*)GetDataBase("Calibrations/sst/sstStripCalib");
     if (mStripCalib) {
       LOG_INFO << "sst readout pedestal table found ... initialize" << endm;
       FillReadOutPedTable(mStripCalib->GetTable());}
     else {
       LOG_WARN << "InitRun : No access to sstStripCalib table - we will use the default pedestal values( ped = 0)." << endm;
       FillDefaultReadOutPedTable();
     }

     St_sstNoise *mNoise = (St_sstNoise*)GetDataBase("Calibrations/sst/sstNoise");
     if (mNoise) {
       LOG_INFO << "sst noise table found ... initialize" << endm;
       FillNoiseTable(mNoise->GetTable());}
     else {
       LOG_WARN << "InitRun : No access to ssdNoise - will use the default noise(rms in all channel will be 0 adc )" << endm;
       FillDefaultNoiseTable();
     }
   }
   else { // we will not save any data when use default tabls
     LOG_WARN << "InitRun : Unsupported data - we will not save any information )" << endm;
     FillDefaultReadOutPedTable();
     FillDefaultNoiseTable();
   }

   if (mRunNum >= 14 && mRunNum <= 15) { // Only for Run14 , 15 ZS data mode
     St_sstChipCorrect *mChipCorrect = (St_sstChipCorrect*)GetDataBase("Calibrations/sst/sstChipCorrect");
     if (mChipCorrect) {
       LOG_INFO << "sst mask chips table found ... initialize" << endm;
       FillChipNoiseTable(mChipCorrect->GetTable());}
     else {
       LOG_WARN << " no sst masking chips table " << endm;      
       FillDefaultChipNoiseTable();
     }
   }
   else {
     // For Run16, we will not use this table. defaults value will not touch the real data.
     LOG_WARN << "InitRun : We will not use ChipCorrection Table any more )" << endm;
     FillDefaultChipNoiseTable();
   }

   if (mRunNum >= 16) { // Only for Run16
     LOG_DEBUG << " searching for a bad Strip table" << endm;
     St_sstBadStrips *mBadStrip = (St_sstBadStrips*)GetDataBase("Calibrations/sst/sstBadStrips");
     if (mBadStrip) {
       LOG_DEBUG << "sst bad strips table found ... initialize" << endm;
       FillBadStripsTable(mBadStrip->GetTable());
     }
   }

   St_sstConfiguration *configTable = (St_sstConfiguration *) GetInputDB("Geometry/sst/sstConfiguration");
   
   if (!configTable) {
     LOG_ERROR << "InitRun: No relevant entry found in 'Geometry/sst/sstConfiguration' table" << endm;
     return kStFatal;
   }
   
   mConfigTable = (sstConfiguration_st *) configTable->GetTable() ;
   mConfig      = new StSstConfig();

   Int_t totLadderPresent = 0;

   for (Int_t ladder = 1; ladder <= mConfigTable->nMaxLadders; ladder++) {
     if (mConfigTable->ladderIsPresent[ladder - 1] != 0)
       totLadderPresent++;
     
     mConfig->setLadderIsActive(ladder, mConfigTable->ladderIsPresent[ladder - 1]);
   }
   
   PrintConfiguration(mRunNum, mConfigTable);
   mConfig->setNumberOfLadders(totLadderPresent);
   mConfig->setNumberOfWafers(mConfigTable->nMaxWafers / mConfigTable->nMaxLadders);
   mConfig->setNumberOfHybrids(2);
   mConfig->setTotalNumberOfHybrids(nSstSide * nSstWaferPerLadder * totLadderPresent);
   mConfig->setTotalNumberOfLadders(mConfigTable->nMaxLadders);
   mConfig->setNumberOfStrips(nSstStripsPerWafer);
   mConfig->setConfiguration();
   mEventnumber = 0;

   LOG_INFO << "_____________________________" << endm;
   LOG_INFO << "       Via  Datababase......." << endm;
   LOG_INFO << ".......numberOfSectors =     " << mConfigTable->nMaxSectors << endm;
   LOG_INFO << ".......numberOfLadders =     " << totLadderPresent << endm;
   LOG_INFO << " .numberOfWafersPerLadder =  " << mConfigTable->nMaxWafers / mConfigTable->nMaxLadders << endm;
   LOG_INFO << "_____________________________" << endm;
   LOG_INFO << "      InitRun() - Done       " << endm;
   LOG_INFO << "StSstDaqMaker initialization." << endm;
   return kStOK;
}


//-------------------------------------------------
Int_t StSstDaqMaker::Make()
{
   LOG_INFO << "StSstDaqMaker Start Make..." << endm;
   StRtsTable *rts_table = 0;
   mEventnumber++;

   //raw data mode
   while ( (rts_table = GetNextDaqElement("sst/raw")) != 0 ) {
      //each DAQ element record one RDO data
      //one event will loop all the 5 RDOs
      mMode = 0;//0-physic run,1-pedestal run
      mRdoData = (UInt_t *)rts_table->At(0);
      mRdoDataLength = rts_table->GetNRows();
      LOG_INFO << "Found sst Physics Run data at " << hex << mRdoData << dec << " , length in byte: "
               << mRdoDataLength << " in UInt_t: " << mRdoDataLength / sizeof(UInt_t) << endm;
      DecodeRdoData();
      DecodeHitsData();
   }

   //pedestal mode   
   sstStripCalib_st *noise_strip = new sstStripCalib_st();
   memset(noise_strip,0,491520*(sizeof(Short_t) + sizeof(Char_t)));
   while ( (rts_table = GetNextDaqElement("sst/pedrms")) != 0 ) {
      mMode    = 1;
      mRdoData = (UInt_t *)rts_table->At(0);
      mRdoDataLength = rts_table->GetNRows();
      LOG_INFO << "Found sst Pedestal Run data at " << hex << mRdoData
               << dec << " , length in byte: " << mRdoDataLength << endm;
      Int_t flag = 0;
      Int_t id_side, ladder;
      Int_t ladderCountN[20] = {0};
      Int_t ladderCountP[20] = {0};
      Int_t c_correct;
      Int_t channelindex = 0;
      mSec     = rts_table->Sector();
      mRDO     = rts_table->Rdo();
      mFiber   = rts_table->Pad();

      if (mSec != 1) mRDO = mRDO + 3;//sector 2

      if (mRDO < 1 || mRDO > 5)     flag = 1;

      if (mSec < 1 || mSec > 3)     flag = 1;

      if (mFiber < 0 || mFiber > 7) flag = 1;

      if (flag == 1) {
         LOG_WARN << "BAD pedestal data. Sector: " << mSec << " RDO: "
                  << mRDO << " fiber: " << mFiber << endm;
         continue;
      }

      FindLadderSide(mRDO, mFiber, ladder, id_side);
      for (StRtsTable::iterator it = rts_table->begin(); it != rts_table->end(); ++it) {
         daq_sst_pedrms_t *f = (daq_sst_pedrms_t *)*it;

         for (Int_t c = 0; c < nSstStripsPerWafer; c++) {
            //channel
            c_correct = (Int_t)c;

            if (!Shift(mEventrunumber, c_correct)) {
               LOG_INFO << "First readout channel in old LC FPGA is not usable." << endm;
               continue;
            }

            for (Int_t h = 0; h < nSstWaferPerLadder; h++) {
               //wafer
               Int_t s = c_correct;
               FindStripNumber(s);
               mPed  = (short)(f->ped[h][c_correct] - 375); // Pedestal from data have a constant shift (375)
               mRms  = (short)(f->rms[h][c_correct]);       // rms = 16 * raw_rms --> Save as hight precision.
	       channelindex = id_side*nSstLadder*nSstWaferPerLadder*nSstStripsPerWafer
		 + ladder*nSstWaferPerLadder*nSstStripsPerWafer
		 + h*nSstStripsPerWafer
		 + c_correct;		 // + s;
               noise_strip->rms[channelindex]       = mRms;
               noise_strip->pedestals[channelindex] = mPed;
   	       // cout<<"--->Channel is : "<<channelindex<<endl;
               if (id_side == 0) ladderCountP[ladder]++;
               else            ladderCountN[ladder]++;
            }
         }
      }

      if (StMaker::GetDebug()) {
         LOG_DEBUG << "Make()/Counts (p-side): ";

         for (Int_t i = 0; i < nSstLadder; i++) {
            LOG_DEBUG.width(5);
            LOG_DEBUG << ladderCountP[i] << " ";
         }

         LOG_DEBUG << endm;
         LOG_DEBUG << "Make()/Counts (n-side): ";

         for (Int_t i = 0; i < nSstLadder; i++) {
            LOG_DEBUG.width(5);
            LOG_DEBUG << ladderCountN[i] << " ";
         }

         LOG_DEBUG << endm;

      }
   }

   if (stripCal && stripCal->GetNRows() != 0) {
     LOG_DEBUG << "Make()/ Read Pedestal & Noise" << endm;
     LOG_DEBUG << "Make()/stripCal->NRows= " << stripCal->GetNRows() << endm;
   }

   // Directly write sstStripCalib table in strip ordering.
   if(mMode == 1) {
     char name[50];
     sprintf(name, "sstStripCalib.%d.%06d.root", GetDate(), GetTime());

     TFile f1(name,"RECREATE","SSD ped and noise file",9);
     stripCal->AddAt(noise_strip);
     stripCal->Print();
     stripCal->Write();
     f1.Close();
   }

   delete noise_strip;
   return kStOk;
}
//-------------------------------------------------
void StSstDaqMaker::DecodeRdoData()
{
   Int_t index = 0;
   mRDO               = 0;
   mTrigger           = 0;
   mHeaderData        = 0;
   mTrailerData       = 0;
   mTrailerDataLength = 0;
   mRdoFlag           = 1;
   mEventTime         = 0;
   mPEventTime        = 0;

   for (Int_t f = 0; f < 8; f++) mFiberFlag[f] = 1; //flag=0-->bad, flag=1-->good

   if (mRdoDataLength == 0 || !mRdoData) {
      LOG_WARN << "NO RDO OUTPUT DATA!" << endm;
      mRdoFlag = 0;
      return;
   }

   if (mRdoData[0] != HEADER_TOKEN) {
      LOG_WARN << "SST DAQ DATA HEADER_TOKEN is not correct!!" << endm;
      mRdoFlag = 0;
      return;
   }
   else {
      LOG_DEBUG << "SST DAQ DATA HEADER_TOKEN correct: 0x" << hex << mRdoData[0] << dec << endm;
   }

   mEventTime  =  mRdoData[2];
   mPEventTime =  mRdoData[4];

   mHeaderData = mRdoData + index;

   if (StMaker::GetDebug()) {
      for (UInt_t h = 0; h < HEADER_LENGTH; h++) {
         if (h == 0) LOG_DEBUG << "Start Print Event HEADER info... " << endm;

         LOG_DEBUG << "0x" << hex << mHeaderData[h] << dec << endm;
      }
   }

   mTrigger = Mid(TRIG_START, TRIG_END, mRdoData[1]);
   LOG_DEBUG << "Current Event Trigger words is " << mTrigger << endm;
   mRDO = Mid(RDO_START, RDO_END, mRdoData[3]);

   if (mRDO == 6) mRDO = 1; //20140311,luis chenged this serial number

   LOG_DEBUG << "Current RDO number is :" << mRDO << endm;

   if (mRDO < 1 || mRDO > nSstRdo) {
      LOG_WARN << "RDO number is BAD number(<1 || >5), reject this RDO" << endm;
      mRdoFlag = 0;
      return;
   }

   index += HEADER_LENGTH;

   for (Int_t i = 0; i < 8; i++) {
      if (i == 0) {
         mAdcHeader[i] = (mRdoData + index);
         mAdc[i] = mAdcHeader[i] + FIBER_HEADER_LENGTH;
      }
      else {
         mAdc[i] = mAdc[i - 1] + mAdcLength[i - 1];
         mAdcHeader[i] = (mAdcHeader[i - 1] + mAdcLength[i - 1]);
      }

      mAdcLength[i] = Mid(ADC_START, ADC_END, mAdcHeader[i][1]);
      mDataMode[i]  = Mid(DATAMODE_START, DATAMODE_END, mAdcHeader[i][1]);
      mChannel[i]   = Mid(FIBER_START, FIBER_END, mAdcHeader[i][2]);
      mFlag[i]      = Mid(FLAG_START, FLAG_END, mAdcHeader[i][1]);

      if (mAdcHeader[i][0] == FIBER_LINK_TOKEN) {
         LOG_DEBUG << "Fiber [" << mChannel[i] << "]: link Token correct : 0x"
                   << hex << mAdcHeader[i][0] << dec << endm;
      }
      else {
         LOG_WARN << "Fiber [" << mChannel[i] << "]: Link Token wrong :0x"
                  << hex << mAdcHeader[i][0] << dec << endm;
         mFiberFlag[i] = 0;
         continue;
      }

      if (mDataMode[i] == RAWMODE) {
         LOG_DEBUG << "Fiber [" << mChannel[i] << "]:Data mode is RAW data. ADC Length "
                   << mAdcLength[i] << endm;
      }
      else if (mDataMode[i] == COMPRESSEDMODE) {
         LOG_DEBUG << "Fiber [" << mChannel[i] << "]:Data mode is COMPRESSED data. ADC Length "
                   << mAdcLength[i] << endm;
      }
      else if (mDataMode[i] == CMNSMODE) {
	LOG_DEBUG << "Fiber [" << mChannel[i] << "]:Data mode is CMN suppressed data. ADC Length "
		  << mAdcLength[i] << endm;
      }
      else {
         LOG_WARN << "Fiber [" << mChannel[i] << "]: DO not have this data mode; error data mode = "
                  << mDataMode[i] << endm;
         mFiberFlag[i] = 0;
         continue;
      }

      if (mFlag[i] == NODATA) {
         LOG_DEBUG << "Fiber [" << mChannel[i] << "] Flag:NO DATA FLAG,reject this fiber data" << endm;

         if (mAdcLength[i] == FIBER_HEADER_LENGTH)
            LOG_DEBUG << "Fiber [" << mChannel[i] << "] Flag and adc length is consistent! let's look at next fiber.." << endm;

         if (mAdcLength[i] != FIBER_HEADER_LENGTH) {
            LOG_WARN << "Fiber [" << mChannel[i] << "] Flag and adc length is not consistent,Stop !" << endm;
            mFiberFlag[i] = 0;
            continue;
         }
      }

      if (mFlag[i] == OVERFLOWFLAG) {
         LOG_WARN << "Fiber [" << mChannel[i] << "] Flag:Over Flow" << endm;
         mFiberFlag[i] = 0;
         continue;
      }

      if (mFlag[i] == EARLY_ABORT) {
         LOG_WARN << "Fiber [" << mChannel[i] << "] Flag:Abort Early!" << endm;
         mFiberFlag[i] = 0;
         continue;
      }

      if (mFlag[i] == WRONG_PIPE_MODE) {
         LOG_WARN << "FIBER [" << mChannel[i] << "] Flag:Wrong pipe mode!" << endm;
         mFiberFlag[i] = 0;
         continue;
      }

      LOG_DEBUG << "Fiber [" << mChannel[i] << "]: ADC Length = " << mAdcLength[i] << endm;
   }

   //check the end token,TCD end token TCD header
   if (mRdoData[mRdoDataLength / sizeof(UInt_t) - 1] == END_TOKEN) {
      LOG_DEBUG << "End Token Correct: 0x" << hex << mRdoData[mRdoDataLength / sizeof(UInt_t) - 1] << dec << endm;
   }
   else {
      LOG_WARN << "End Token Wrong : 0x" << hex << mRdoData[mRdoDataLength / sizeof(UInt_t) - 1] << dec << endm;
      mRdoFlag = 0;
      return;
   }

   //tcd end token
   if (mRdoData[mRdoDataLength / sizeof(UInt_t) - 3] == TCD_END_TOKEN) {
      LOG_DEBUG << "TCD End Token Correct: 0x" << hex << mRdoData[mRdoDataLength / sizeof(UInt_t) - 3] << dec << endm;
   }
   else {
      LOG_WARN << "TCD End Token Wrong : 0x" << hex << mRdoData[mRdoDataLength / sizeof(UInt_t) - 3] << dec << endm;
      mRdoFlag = 0;
      return;
   }

   //tcd token
   mTrailerData = mAdcHeader[7] + mAdcLength[7];

   if (mTrailerData[0] == TCD_TOKEN) {
      LOG_DEBUG << "TCD Token Correct: 0x" << hex << mTrailerData[0] << dec << endm;
   }
   else {
      LOG_WARN << "TCD Token Wrong : 0x" << hex << mTrailerData[0] << dec << endm;
      mRdoFlag = 0;
      return;
   }

   if (StMaker::GetDebug()) {
      for (Int_t t = 0; t < 10; t++) {
         if (t == 0) LOG_DEBUG << "Start Print Trailer info... " << endm;

         LOG_DEBUG << "0x" << hex << mTrailerData[t] << dec << endm;

         if (mTrailerData[t] == END_TOKEN) break;
      }
   }
}
//-------------------------------------------------
Int_t StSstDaqMaker::GetHitsDataLength(Int_t FiberNumber)
{
   return mAdcLength[FiberNumber];
}
//------------------------------------------------
void StSstDaqMaker::DecodeHitsData()
{
   if (mRdoFlag != 1) return;

   LOG_DEBUG << "START Decoding RDO [" << mRDO << "] data, rdo flag is " << mRdoFlag << endm;

   for (Int_t j = 0; j < 8; j++) {
      if (mFiberFlag[j] != 1) continue;

      if (mChannel[j] < 0 || mChannel[j] > (nSstFiberPerRdo - 1)) {
         LOG_WARN << "Fiber number is BAD (<0 || >7), reject this Fiber" << endm;
         continue;
      }

      Int_t temp = mAdcLength[j] - FIBER_HEADER_LENGTH;

      if (temp == 0 || !mAdc[j]) {
         LOG_WARN << "Fiber [" << mChannel[j] << "]: do not have any data..." << endm;
         continue;
      }

      if (mFlag[j] != NORMAL) {
         LOG_WARN << "Fiber[" << mChannel[j] << "]: data flag is not normal, reject this fiber" << endm;
         continue;
      }

      if (mDataMode[j] == RAWMODE) {
         // DecodeRawWords(mAdc[j], temp, mChannel[j]);
         DecodeRawWords_r15(mAdc[j], temp, mChannel[j]);
      }

      if ((mDataMode[j] == COMPRESSEDMODE) || (mDataMode[j] == CMNSMODE))
	DecodeCompressedWords(mAdc[j], temp, mChannel[j]);
   }
}
//-------------------------------------------------
void StSstDaqMaker::DecodeRawWords(UInt_t *val, Int_t vallength, Int_t channel)
{
   Int_t strip_number[3]    = {0};
   Int_t id_wafer[3]        = {0};
   Int_t ladderCountN[20]   = {0};
   Int_t ladderCountP[20]   = {0};
   Int_t data[3]            = {0};
   Int_t wafer[3]           = {0};
   Int_t hybrid[3]          = {0};
   Int_t strip[3]           = {0};
   Int_t readout[3]         = {0};
   Int_t readout_correct[3] = {0};
   Int_t ladder             = 0;
   Int_t id_side            = 0;
   Int_t count              = 1;

   //initialize St_spa_strip and St_ssdPedStrip table.
   spa_strip = dynamic_cast<St_spa_strip *>( m_DataSet->Find("spa_strip"));

   if (!spa_strip) {
      spa_strip   = new St_spa_strip("spa_strip", vallength);
      m_DataSet->Add(spa_strip);
   }

   spa_strip_st   out_strip;
   LOG_DEBUG << "DECODING RAW MODE data....." << endm;
   //grab ladder and side
   FindLadderSide(mRDO, channel, ladder, id_side);

   //RAW data format
   //Each 32-bit word will record 3 ADC words.
   //Data store structure:
   //
   // 32-bit word   3 ADCs bit 9-0:readout 0; hybrid 0
   //                      bit 19-10:readout 0; hybrid 1
   //                      bit 29-20:readout 0; hybrid 2
   //                      .
   //                      .
   // 32-bit word   3 ADCs bit 9-0:readout 0; hybrid 15
   //                      bit 19-10:readout 1; hybrid 0
   //                      bit 29-20:readout 1; hybrid 1

   for (Int_t i = 0; i < vallength; i++) {
      if (i == 0) hybrid[0] = 0;
      else hybrid[0] = hybrid[2] + 1;

      if (hybrid[0] >= 16) hybrid[0] = hybrid[0] % 16;

      hybrid[1] = hybrid[0] + 1;

      if (hybrid[1] >= 16) hybrid[1] = hybrid[1] % 16;

      hybrid[2] = hybrid[0] + 2;

      if (hybrid[2] >= 16) hybrid[2] = hybrid[2] % 16;

      LOG_DEBUG << "Three hybrid number in current word is :"
                << hybrid[0] << "," << hybrid[1] << "," << hybrid[2] << endm;

      if (i != 0) {
         if (hybrid[0] == 0) readout[0] = readout[0] + 1;

         if (readout[2] > readout[0]) readout[0] = readout[2];

         if (hybrid[1] == 0) readout[1] = readout[1] + 1;

         if (readout[0] > readout[1]) readout[1] = readout[0];

         if (hybrid[2] == 0) readout[2] = readout[2] + 1;

         if (readout[1] > readout[2]) readout[2] = readout[1];
      }

      LOG_DEBUG << "[adc_pointer,readout_0,hybrid_0] = [" << i << ","
                << readout[0] << "," << hybrid[0] << "]" << endm;
      LOG_DEBUG << "[adc_pointer,readout_1,hybrid_1] = [" << i << ","
                << readout[1] << "," << hybrid[1] << "]" << endm;
      LOG_DEBUG << "[adc_pointer,readout_2,hybrid_2] = [" << i << ","
                << readout[2] << "," << hybrid[2] << "]" << endm;

      data[0] = Mid(HYBRID_ONE_START, HYBRID_ONE_END, val[i]);
      data[1] = Mid(HYBRID_TWO_START, HYBRID_TWO_END, val[i]);
      data[2] = Mid(HYBRID_THREE_START, HYBRID_THREE_END, val[i]);

      for (Int_t n = 0; n < 3; n++) {
         readout_correct[n] = readout[n];

         if (!Shift(mEventrunumber, readout_correct[n])) {
            LOG_DEBUG << "First readout channel in old LC FPGA is not usable." << endm;
            continue;
         }

         strip[n] = readout[n];
         wafer[n] = hybrid[n];
         FindStripNumber(strip[n]);

         if (id_side == 0) {
            id_wafer[n] = 7000 + 100 * (nSstWaferPerLadder - wafer[n]) + ladder + 1;
	    strip_number[n] = strip[n] + 1; //strip[n]+1 . in mapping, strip[1-128];
         }
         else {
            id_wafer[n] = 7000 + 100 * ((wafer[n]) + 1) + ladder + 1;
	    strip_number[n] = nSstStripsPerWafer - strip[n];	
         }

	 if( mRunNum >= 14 && mRunNum <=15) {
	   if (gStSstDbMaker->maskChip(id_side, ladder, wafer[n], strip[n] / 128)) continue;
	 }

         out_strip.id          = count;
         out_strip.adc_count   = data[n];
         out_strip.id_strip    = 10000 * (10 * strip_number[n] + id_side) + id_wafer[n]; //id_side:0-->p,1-->N
         out_strip.id_mchit[0] = 0 ;
         out_strip.id_mchit[1] = 0 ;
         out_strip.id_mchit[2] = 0 ;
         out_strip.id_mchit[3] = 0 ;
         out_strip.id_mchit[4] = 0 ;

         spa_strip->AddAt(&out_strip);

         if (id_side == 0) {
            ladderCountP[ladder]++;
         }
         else {
            ladderCountN[ladder]++;
         }

         count = count + 1;
      }
   }

   LOG_DEBUG << "Last readout number: [readout[0] , readout[1] , readout[2]] = [" << readout[0] << ","
             << readout[1] << "," << readout[2] << "]" << endm;

   if (readout[0] > nSstStripsPerWafer || readout[1] > nSstStripsPerWafer || readout[2] > nSstStripsPerWafer) {
      LOG_WARN << "Strip number is larger than 768." << endm;
      return;
   }

   if (StMaker::GetDebug()) {
      LOG_DEBUG << "Make()/Counts (p-side): ";

      for (Int_t i = 0; i < nSstLadder; i++) {
         LOG_DEBUG.width(5);
         LOG_DEBUG << ladderCountP[i] << " ";
      }

      LOG_DEBUG << endm;
      LOG_DEBUG << "Make()/Counts (n-side): ";

      for (Int_t i = 0; i < nSstLadder; i++) {
         LOG_DEBUG.width(5);
         LOG_DEBUG << ladderCountN[i] << " ";
      }

      LOG_DEBUG << endm;

      if (spa_strip->GetNRows() != 0) {
         LOG_DEBUG << "Make()/ Read Signal from Physics Run" << endm;
         LOG_DEBUG << "Make()/  spa_strip->NRows= " << spa_strip->GetNRows() << endm;
      }
   }
}
//-------------------------------------------------
void StSstDaqMaker::DecodeRawWords_r15(UInt_t *val, Int_t vallength, Int_t channel)
{
  vector<vector<int> > vadc(16, vector<int>(768)); //2-D Vector
  vector<vector<float> > vcmnoise(16, vector<float>(6)); //Save CM Noise
  vector<int> vtemp(128);

  for (int i = 0; i < 16; i++) {
    vadc[i].resize(768);
    vcmnoise[i].resize(6);
  }

  Int_t data[3]            = {0};
  Int_t wafer[3]           = {0};
  Int_t hybrid[3]          = {0};
  Int_t strip[3]           = {0};
  Int_t readout[3]         = {0};
  Int_t readout_correct[3] = {0};
  Int_t ladder             = 0;
  Int_t id_side            = 0;
  Int_t readoutindex       = 0;

  LOG_DEBUG << "DECODING RAW MODE data....." << endm;
  //grab ladder and side
  FindLadderSide(mRDO, channel, ladder, id_side);

  for (Int_t i = 0; i < vallength; i++) {
    if (i == 0) hybrid[0] = 0;
    else hybrid[0] = hybrid[2] + 1;

    if (hybrid[0] >= 16) hybrid[0] = hybrid[0] % 16;

    hybrid[1] = hybrid[0] + 1;

    if (hybrid[1] >= 16) hybrid[1] = hybrid[1] % 16;

    hybrid[2] = hybrid[0] + 2;

    if (hybrid[2] >= 16) hybrid[2] = hybrid[2] % 16;

    LOG_DEBUG << "Three hybrid number in current word is :"
	      << hybrid[0] << "," << hybrid[1] << "," << hybrid[2] << endm;

    if (i != 0) {
      if (hybrid[0] == 0) readout[0] = readout[0] + 1;

      if (readout[2] > readout[0]) readout[0] = readout[2];

      if (hybrid[1] == 0) readout[1] = readout[1] + 1;

      if (readout[0] > readout[1]) readout[1] = readout[0];

      if (hybrid[2] == 0) readout[2] = readout[2] + 1;

      if (readout[1] > readout[2]) readout[2] = readout[1];
    }

    LOG_DEBUG << "[adc_pointer,readout_0,hybrid_0] = [" << i << ","
	      << readout[0] << "," << hybrid[0] << "]" << endm;
    LOG_DEBUG << "[adc_pointer,readout_1,hybrid_1] = [" << i << ","
	      << readout[1] << "," << hybrid[1] << "]" << endm;
    LOG_DEBUG << "[adc_pointer,readout_2,hybrid_2] = [" << i << ","
	      << readout[2] << "," << hybrid[2] << "]" << endm;

    data[0] = Mid(HYBRID_ONE_START, HYBRID_ONE_END, val[i]);
    data[1] = Mid(HYBRID_TWO_START, HYBRID_TWO_END, val[i]);
    data[2] = Mid(HYBRID_THREE_START, HYBRID_THREE_END, val[i]);

    for (Int_t n = 0; n < 3; n++) {
      readout_correct[n] = readout[n];

      if (!Shift(mEventrunumber, readout_correct[n])) {
	LOG_DEBUG << "First readout channel in old LC FPGA is not usable." << endm;
	continue;
      }

      strip[n] = readout_correct[n];
      wafer[n] = hybrid[n];
      readoutindex = id_side * nSstLadder * nSstWaferPerLadder * nSstStripsPerWafer 
	+ ladder * nSstWaferPerLadder * nSstStripsPerWafer 
	+ wafer[n] * nSstStripsPerWafer 
	+ strip[n];

      // wrap-around correction 
      data[n] = data[n] + 375;
      if(data[n]>1024) data[n] = data[n] - 1024;


      data[n] = data[n] - 375 - mReadOutPed[readoutindex];
      vadc[wafer[n]][strip[n]] = data[n];
    }
  }

  for (int i = 0; i < 16; i++) {
    for (int j = 0; j < 768; j++) {
      vtemp[j % 128] = vadc[i][j];

      if ((j + 1) % 128 == 0) {
	vcmnoise[i][j / 128] = CalculateCommonModeNoiseSimple(vtemp);
      }
    }
  }

  FillData(vadc, vcmnoise, id_side, ladder, vallength);

  LOG_DEBUG << "Last readout number: [readout[0] , readout[1] , readout[2]] = [" << strip[0] << ","
	    << strip[1] << "," << strip[2] << "]" << endm;

  if (strip[0] > nSstStripsPerWafer || strip[1] > nSstStripsPerWafer || strip[2] > nSstStripsPerWafer) {
    LOG_WARN << "Strip number is larger than 768." << endm;
    return;
  }

  vadc.clear();
  vcmnoise.clear();
  vtemp.clear();
}
//-------------------------------------------------
void StSstDaqMaker::DecodeCompressedWords(UInt_t *val, Int_t vallength, Int_t channel)
{
   Int_t  ladderCountN[20] = {0};
   Int_t  ladderCountP[20] = {0};
   Int_t  strip_number     = 0;
   Int_t  id_side          = 0;
   Int_t  id_wafer         = 0;
   Int_t  count            = 1;
   Int_t  data             = 0;
   Int_t  wafer            = 0;
   Int_t  strip            = 0;
   Int_t  readout          = 0;
   Int_t  ladder           = 0;
   Int_t  chip             = 0;
   Int_t  chipIndex        = 0;
   Int_t  oldchip          = 0;
   Int_t  chipflag         = 0;	// CMN algorithm faild flag.
   UInt_t errorcode        = 0;	// CMN algorithm error code.
   int wafDecodeBadStrip = 0; // used for bad strip table
   int stripDecodeBadStrip = 0;// used for decoding bad strip table 
   int indexDecodeBadStrip = 0; // used for bad strip table
 
   LOG_DEBUG << "Current Event data length : " << vallength << endm;
   spa_strip = dynamic_cast<St_spa_strip *>( m_DataSet->Find("spa_strip"));

   if (!spa_strip) {
      spa_strip   = new St_spa_strip("spa_strip", vallength);
      m_DataSet->Add(spa_strip);
   }

   spa_strip_st  out_strip;
   //grab ladder and side
   FindLadderSide(mRDO, channel, ladder, id_side); //convert channel to real Ladder number and side

   for (Int_t i = 0; i < vallength; i++) {
      wafer     = Mid(HYBRID_START, HYBRID_END, val[i]);
      strip     = Mid(STRIP_START, STRIP_END, val[i]);
      data      = Mid(COM_ADC_START, COM_ADC_END, val[i]);
      errorcode = Mid(ERROR_START,ERROR_END,val[i]);
      chip      = strip/nSstStripsPerChip;
      chipIndex = ladder*nSstWaferPerLadder*nSstChipPerWafer + wafer*nSstChipPerWafer + chip;

      // Mask out CMN failed chips
      if(oldchip!=chipIndex) chipflag = 0;
      if(strip%nSstStripsPerChip==0 && errorcode==CMNERRORCODE) chipflag = 1;
      if(chipflag) continue;
      
      if (!Shift(mEventrunumber, strip)) {
         LOG_INFO << "First readout channel in old LC FPGA is not usable." << endm;
         continue;
      }

      //chipCorrect table
      if(data<mNoiseCut[chipIndex][id_side]) data = 0; //remove noise.
      else data = data - mCorrectFactor[chipIndex][id_side]; //data correction.
      
      readout = strip;

      FindStripNumber(strip);//convert to physic strip number

      if (id_side == 0) {
         id_wafer = 7000 + 100 * (nSstWaferPerLadder - wafer) + ladder + 1;
	 strip_number = strip + 1;
      }
      else {
         id_wafer = 7000 + 100 * ((wafer) + 1) + ladder + 1;
	 strip_number = nSstStripsPerWafer - strip;	
      }
      //chipMask table      
      if( mRunNum >= 14 && mRunNum <=15) {
	if (gStSstDbMaker->maskChip(id_side, ladder, wafer, chip)) continue;
      }

      if(mRunNum >= 16){
	if(id_side == 0){
	  wafDecodeBadStrip = nSstWaferPerLadder - wafer - 1;
	  stripDecodeBadStrip = strip + 1;
	}
	else {
	  wafDecodeBadStrip = wafer + 1 - 1;
	  stripDecodeBadStrip = nSstStripsPerWafer - strip;	
	}
	
	indexDecodeBadStrip = id_side * nSstLadder * nSstWaferPerLadder * nSstStripsPerWafer 
	  + ladder * nSstWaferPerLadder * nSstStripsPerWafer 
	  + wafDecodeBadStrip * nSstStripsPerWafer
	  + stripDecodeBadStrip -1;
	if(mBadStrip[indexDecodeBadStrip]!=0) continue;
      }

      //save only strips with data>0, otherwise it increases the datastrip volume for nothing
      if(data>0){
	out_strip.id          = count;
	out_strip.adc_count   = data;
	out_strip.id_strip    = 10000 * (10 * strip_number + id_side) + id_wafer;
	out_strip.id_mchit[0] = 0 ;
	out_strip.id_mchit[1] = 0 ;
	out_strip.id_mchit[2] = 0 ;
	out_strip.id_mchit[3] = 0 ;
	out_strip.id_mchit[4] = 0 ;
	spa_strip->AddAt(&out_strip);
	
	if (id_side == 0) {
	  ladderCountP[ladder]++;
	}
	else {
	  ladderCountN[ladder]++;
	}
	
	count = count + 1;
      }
      oldchip = chipIndex;
   }
   
   if (readout > nSstStripsPerWafer) {
      LOG_WARN << "Strip number is larger than 768, ERROR" << endm;
      return;
   }

   if (StMaker::GetDebug()) {
      LOG_DEBUG << "Make()/Counts (p-side): ";

      for (Int_t i = 0; i < nSstLadder; i++) {
         LOG_DEBUG.width(5);
         LOG_DEBUG << ladderCountP[i] << " ";
      }

      LOG_DEBUG << endm;
      LOG_DEBUG << "Make()/Counts (n-side): ";

      for (Int_t i = 0; i < nSstLadder; i++) {
         LOG_DEBUG.width(5);
         LOG_DEBUG << ladderCountN[i] << " ";
      }

      LOG_DEBUG << endm;

      if (spa_strip->GetNRows() != 0) {
         LOG_DEBUG << "Make()/ Read Signal from Physics Run" << endm;
         LOG_DEBUG << "Make()/  spa_strip->NRows= " << spa_strip->GetNRows() << endm;
      }
   }
}

//-------------------------------------------------
Int_t StSstDaqMaker::Shift(Int_t runnumber, Int_t &channel)
{
   if (runnumber < 15150058) {
      if (channel == 0) return 0;
      else {
         channel = channel - 1;
         return 1;
      }
   }
   else {
      return 1;
   }
}
//-------------------------------------------------
UInt_t StSstDaqMaker::Mid(Int_t start, Int_t end, UInt_t input)
{
   UInt_t buf;
   buf = input << (32 - end);
   buf = buf >> (32 - end);
   return buf >> start;
}

//--------------------------------------------------
void StSstDaqMaker::FindLadderSide(Int_t RDO, Int_t channel, Int_t &ladder, Int_t &side)
{
   ladder = RDO2LADDER[RDO - 1][channel] - 1; //ladder [0-19];

   if (ladder > 20) LOG_WARN << "Ladder >20. RDO Number is" << RDO << ", channel number :" << channel << endm;

   if (RDO < 3) side = 1;
   else if (RDO > 3) side = 0;
   else if (channel < 4) side = 1;
   else side = 0;
}
//--------------------------------------------------
void StSstDaqMaker::FindStripNumber(Int_t &strip)
{
   Int_t temp = (strip / 128) * 128 + ReadOutMap[strip % 128];
   strip = temp - 1;
}
//--------------------------------------------------
void StSstDaqMaker::FindChannelNumber(Int_t &channel)
{
  Int_t temp = (channel / 128) * 128 + Rev_ReadOutMap[channel % 128];
  channel = temp - 1;
}
//--------------------------------------------------

void StSstDaqMaker::PrintConfiguration(Int_t runumber, sstConfiguration_st *config)
{
   switch (runumber) {
   case 4 : {
      LOG_INFO << "Configuration of ladders for run IV" << endm;
      break;
   }

   case 5 : {
      LOG_INFO << "Configuration of ladders for run V" << endm;
      break;
   }

   case 6 : {
      LOG_INFO << "Configuration of ladders  for run VI" << endm;
      break;
   }

   case 7 : {
      LOG_INFO << "Configuration of ladders  for run VII" << endm;
      break;
   }
   }

   Int_t i = 0;
   Int_t totladderPresent = 0;
   LOG_INFO << "PrintLadderSummary:ladder id                 :";

   for (i = 1; i <= config->nMaxLadders; i++) {
      LOG_INFO.width(3);
      LOG_INFO << i;
   }

   LOG_INFO << endm;
   LOG_INFO << "PrintLadderSummary:Active Ladders on sectors: ";

   for (i = 1; i <= config->nMaxLadders; i++) {
      LOG_INFO.width(3);
      LOG_INFO << mConfig->getLadderIsActive(i);

      if (mConfig->getLadderIsActive(i) > 0)totladderPresent++;

   }

   LOG_INFO << endm;
   LOG_INFO << "totLadderActive = " << totladderPresent << endm;
}
//------------------------------------------------
void StSstDaqMaker::Clear(const Option_t *)
{
   if (spa_strip) {
      delete spa_strip;
      spa_strip = 0;
   }

   if (stripCal) {  
     delete stripCal;
     stripCal = 0;     
   }            
   
   return StMaker::Clear();
}
//------------------------------------------------
void StSstDaqMaker::FillReadOutPedTable(sstStripCalib_st *noise)
{
  LOG_INFO << "InirTun : New Table(sstStripCalib) is used ! " << endm;

  //coding: id = (491520)*side[0-1]/2 + ladder[0-19]*12288 + wafer[0-15]*768 + strip[0-767]
  int totStripSst       = nSstSide*nSstLadder*nSstWaferPerLadder*nSstStripsPerWafer;
  for (Int_t i = 0 ; i < totStripSst ; i++) 
    mReadOutPed[i] = (Int_t)noise[0].pedestals[i];
}
//------------------------------------------------
void StSstDaqMaker::FillDefaultReadOutPedTable()
{
  Int_t size = nSstSide * nSstLadder * nSstWaferPerLadder * nSstStripsPerWafer;
  
  for (Int_t i = 0; i < size; i++) {
    mReadOutPed[i] = 0;	// mean value for all channels
  }
}
//------------------------------------------------
void StSstDaqMaker::FillNoiseTable(sstNoise_st *noise)
{				
  // Table entries are coded by readout ordering.
  LOG_INFO << "InitRun for real data : new Table(ssdNoise) is used" << endm;
  Int_t size = nSstSide * nSstLadder * nSstWaferPerLadder * nSstStripsPerWafer;
  
  for (Int_t i = 0 ; i < size ; i++) 
    mIntrinsicRms[i] = (Int_t)noise[0].rms[i]/10.0; // 10*rms in table
}
//------------------------------------------------
void StSstDaqMaker::FillDefaultNoiseTable()
{
  Int_t size = nSstSide * nSstLadder * nSstWaferPerLadder * nSstStripsPerWafer;
  
  for (Int_t i = 0; i < size; i++) {
    mIntrinsicRms[i] = nRmsCut;	// if not rms table or pedestal table can be found, then SST will not have any output.
  }
}
//------------------------------------------------
void StSstDaqMaker::FillChipNoiseTable(sstChipCorrect_st *chipCorrectTable){
  int side=0,ladder=0,wafer=0,chip=0;
  LOG_DEBUG<<"New ChipNoiseTable was used! "<<endm;
  int totChipSst       = nSstSide*nSstLadder*nSstWaferPerLadder*nSstChipPerWafer;
  int totChipSstSide   = nSstLadder*nSstWaferPerLadder*nSstChipPerWafer;
  int totChipSstLadder = nSstWaferPerLadder*nSstChipPerWafer;
  
  for(Int_t i=0; i<totChipSst;i++){
    side   = i/totChipSstSide;
    ladder = (i - side*totChipSstSide)/totChipSstLadder;
    wafer  = (i - side*totChipSstSide - ladder*totChipSstLadder)/nSstChipPerWafer;
    chip   = (i - side*totChipSstSide - ladder*totChipSstLadder - wafer*nSstChipPerWafer);
    
    LOG_DEBUG <<" i/side/ladder/wafer/chip/correct/CutPos : " 
	      <<i      << " " 
	      <<side   << " " 
	      <<ladder << " " 
	      <<wafer  << " "  
	      <<chip   << " " 	       
	      <<chipCorrectTable[0].nCorrect[i] << " " 	       
	      <<chipCorrectTable[0].nCutPos[i]  << " "  <<endm;	       
    
    mCorrectFactor[ladder*totChipSstLadder+wafer*nSstChipPerWafer+chip][side] = chipCorrectTable[0].nCorrect[i];
    mNoiseCut[ladder*totChipSstLadder+wafer*nSstChipPerWafer+chip][side]      = chipCorrectTable[0].nCutPos[i];
  }
}			       
//------------------------------------------------
void StSstDaqMaker::FillDefaultChipNoiseTable(){
  LOG_DEBUG <<" Default is no any correction ." << endm;
  Int_t size = nSstLadder*nSstWaferPerLadder*nSstChipPerWafer;
  for(Int_t s=0;s<nSstSide;s++) {
    for(Int_t i=0;i<size;i++) {
      mCorrectFactor[i][s] = 0;
      mNoiseCut[i][s]      = 0;
    }
  }
}
//------------------------------------------------
void StSstDaqMaker::FillBadStripsTable(sstBadStrips_st* badStripTable){
  int totChipSst = nSstSide*nSstLadder*nSstWaferPerLadder*nSstStripsPerWafer;  
  for(Int_t i=0; i<totChipSst;i++){    
    mBadStrip[i] = badStripTable[0].status[i];
  }
}			       
//------------------------------------------------
Float_t StSstDaqMaker::CalculateCommonModeNoiseSimple(vector<int> vadc) //Simplify algorithm
{
  Float_t preSum  = 0;
  Float_t preMean = 0;
  Float_t sum     = 0;
  Float_t devcut  = 20; //deviation cut, default is 10 ADC
  Int_t   counter = 0;

  TH1I *hadc = new TH1I("hadc", "adc in each chip", 768, -512, 1024);
  Float_t chipRms  = 0;

  for (int i = 1; i < 128; i++) { //index start from 1 is to avoid the edge effect.
    hadc->Fill(vadc[i]);
  }

  chipRms = hadc->GetRMS();

  if (chipRms > 15) {
    hadc->Delete();
    return 0.; //if Raw signal in one chip have very large RMS, we will mark it to bad
  }

  hadc->Delete();
  //------------------------------
  //upgraded Fix Threshold method.
  for (int i = 1; i < 128; i++) {
    preSum += vadc[i];
  }

  preMean = preSum / 127.;

  Float_t thresholdhig = preMean + devcut;
  Float_t thresholdlow = preMean - devcut;

  for (int i = 1; i < 128; i++) {
    if (vadc[i] < thresholdlow || vadc[i] > thresholdhig) continue;

    sum += vadc[i];
    counter = counter + 1;
  }

  return (counter>0)?(sum/counter):0;
}
//------------------------------------------------
void StSstDaqMaker::FillData(vector<vector<int> > vadc, vector<vector<float> > vcmnoise, Int_t id_side, Int_t ladder, Int_t vallength)
{
  Int_t   id_wafer         = 0;
  Int_t   strip_number     = 0;
  Int_t   count            = 1;
  Int_t   ladderCountN[20] = {0};
  Int_t   ladderCountP[20] = {0};
  Float_t adc              = 0;
  Float_t cmnoise          = 0;
  Float_t intrinsicnoise   = 0;
  Int_t   stripindex       = 0;

  const Float_t Epsinon    = 0.00001;
  spa_strip = (St_spa_strip *) m_DataSet->Find("spa_strip");

  if (!spa_strip) {
    spa_strip   = new St_spa_strip("spa_strip", vallength);
    m_DataSet->Add(spa_strip);
  }

  spa_strip_st out_strip;

  for (int i = 0; i < 16; i++) {
    for (int j = 0; j < 768; j++) {
      Int_t s = j;
      adc = (Float_t)vadc[i][j];
      cmnoise = vcmnoise[i][j / 128];

      //----------------
      //Temporary masking

      if ((cmnoise >= -Epsinon) && (cmnoise <= Epsinon)) adc = 0;
      else {
	adc = adc - cmnoise;
      }

      if (j % 128 == 0) adc = 0;        //first channel is not useable.

      if (id_side == 1) adc = -1 * adc;   //Reverse N-side Signal Charge Sign.

      FindStripNumber(s);

      //---------------
      //hotchip masking
      if( mRunNum >= 14 && mRunNum <=15) {
	if (gStSstDbMaker->maskChip(id_side, ladder, i, j / 128)) continue;
      }

      if (id_side == 0) {
	id_wafer = 7000 + 100 * (nSstWaferPerLadder - i) + ladder + 1;
	strip_number = s + 1;
      }
      else {
	id_wafer = 7000 + 100 * (i + 1) + ladder + 1;
	strip_number = nSstStripsPerWafer - s;
      }

      //-------------------------
      //Offline Zero-Suppression
      stripindex = id_side * nSstLadder * nSstWaferPerLadder * nSstStripsPerWafer 
	+ ladder * nSstWaferPerLadder * nSstStripsPerWafer 
	+ i * nSstStripsPerWafer
	+ j; // readout ordering

      intrinsicnoise = mIntrinsicRms[stripindex]; // readout ordering
      if(intrinsicnoise<std::numeric_limits<float>::epsilon()) intrinsicnoise = std::numeric_limits<unsigned short>::max(); // if the noise is 0, then reject this channel.

      if (adc < nSigmaCut * intrinsicnoise) continue;
      if (adc <=0) continue;
      if (mReadOutPed[stripindex] == 0) continue; // reject channel with bad pedestal.
      //mask bad strips., default is 10 Adc
      if(intrinsicnoise>=nRmsCut) continue;

      out_strip.id          = count;
      out_strip.adc_count   = adc; //Minus the Common mode noise
      out_strip.id_strip    = 10000 * (10 * strip_number + id_side) + id_wafer; //id_side:0-->p,1-->N
      out_strip.id_mchit[0] = 0 ;
      out_strip.id_mchit[1] = 0 ;
      out_strip.id_mchit[2] = 0 ;
      out_strip.id_mchit[3] = 0 ;
      out_strip.id_mchit[4] = 0 ;

      spa_strip->AddAt(&out_strip);

      if (id_side == 0) {
	ladderCountP[ladder]++;
      }
      else {
	ladderCountN[ladder]++;
      }
    }
  }

  if (StMaker::GetDebug()) {
    LOG_DEBUG << "Make()/Counts (p-side): ";

    for (Int_t i = 0; i < nSstLadder; i++) {
      LOG_DEBUG.width(5);
      LOG_DEBUG << ladderCountP[i] << " ";
    }

    LOG_DEBUG << endm;
    LOG_DEBUG << "Make()/Counts (n-side): ";

    for (Int_t i = 0; i < nSstLadder; i++) {
      LOG_DEBUG.width(5);
      LOG_DEBUG << ladderCountN[i] << " ";
    }

    LOG_DEBUG << endm;

    if (spa_strip->GetNRows() != 0) {
      LOG_DEBUG << "Make()/ Read Signal from Physics Run" << endm;
      LOG_DEBUG << "Make()/  spa_strip->NRows= " << spa_strip->GetNRows() << endm;
    }
  }
}
