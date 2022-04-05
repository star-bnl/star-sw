/*!
 * \class StPxlRawHitMaker
 * \author Jan Rusnak, Qiu Hao, Jan 2013, according codes from Xiangming Sun
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlRawHitMaker.cxx,v 1.12 2017/08/28 17:05:16 dongx Exp $
 *
 * Author: Jan Rusnak, Qiu Hao, Jan 2013, according codes from Xiangming Sun
 ***************************************************************************
 *
 * Description:
 * Read pixel raw hits from daq format. One raw hit is one fired pixel.
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlRawHitMaker.cxx,v $
 * Revision 1.12  2017/08/28 17:05:16  dongx
 * Append rawHits to StPxlRawHitCollection only if exists - for simu/embedding
 *
 * Revision 1.11  2016/03/03 07:36:09  qiuh
 * fix bug on row number, should use last row information FOR THE SAME SENSOR
 *
 * Revision 1.10  2015/05/15 05:31:44  qiuh
 * add c in wrong sector/ladder/sensor/row/column warning
 *
 * Revision 1.9  2014/05/29 22:55:28  qiuh
 * print warnings for wrong rows / columns and deserialization errors only when debug > 2
 *
 * Revision 1.8  2014/05/08 15:10:49  smirnovd
 * PXL DB dataset has been renamed to avoid conflict with StPxlDbMaker's name
 *
 * Revision 1.7  2014/04/05 05:20:08  qiuh
 * add Jtag file version print-out and some more warnings for data format errors
 *
 * Revision 1.6  2014/04/01 15:29:24  qiuh
 * add single hot pixel masking
 *
 * Revision 1.5  2014/01/28 19:29:44  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#include "StPxlRawHitMaker.h"
#include "StPxlRawHit.h"
#include "StPxlRawHitCollection.h"
#include "StMessMgr.h"
#include "StRtsTable.h"
#include "StPxlDbMaker/StPxlDb.h"
#include "tables/St_pxlControl_Table.h"
ClassImp(StPxlRawHitMaker)

StPxlRawHitMaker::StPxlRawHitMaker(const Char_t *name) : StRTSBaseMaker("pxl", name)
{
   mPxlRawHitCollection = 0;
   mJtagFileVersion = 0;
}
//_______________________________________________
void StPxlRawHitMaker::Clear(const Option_t *)
{
   if (mPxlRawHitCollection) {
      delete mPxlRawHitCollection;
      mPxlRawHitCollection = 0;
   }
   return StMaker::Clear();
}
//_______________________________________________
Int_t StPxlRawHitMaker::InitRun(Int_t runumber)
{
   TObjectSet *pxlDbDataSet = (TObjectSet *)GetDataSet("pxl_db");
   if (pxlDbDataSet) {
      mPxlDb = (StPxlDb *)pxlDbDataSet->GetObject();
      assert(mPxlDb);
   }
   else {
      LOG_ERROR << "InitRun : no pxlDb" << endm;
      return kStErr;
   }

   const pxlControl_st *pxlControlTable = mPxlDb->pxlControl();
   mHeaderLength = pxlControlTable[0].headerLength;
   mHardwareIdPosition = pxlControlTable[0].hardwareIdPosition;
   mHeaderToken = pxlControlTable[0].headerToken;
   mSeparatorToken = pxlControlTable[0].separatorToken;
   mEndToken = pxlControlTable[0].endToken;
   mChipIdStartBit = pxlControlTable[0].chipIdStartBit;
   mChipIdEndBit = pxlControlTable[0].chipIdEndBit;
   mChipIdPow = pxlControlTable[0].chipIdPow;
   mOverflowBit = pxlControlTable[0].overflowBit;
   mRowOrColumnFlagBit = pxlControlTable[0].rowOrColumnFlagBit;
   mCodingStartBit = pxlControlTable[0].codingStartBit;
   mCodingEndBit = pxlControlTable[0].codingEndBit;
   mDataStartBit = pxlControlTable[0].dataStartBit;
   mDataEndBit = pxlControlTable[0].dataEndBit;
   mSensorGoodStatusMin = pxlControlTable[0].sensorGoodStatusMin;
   mSensorGoodStatusMax = pxlControlTable[0].sensorGoodStatusMax;
   mRowColumnGoodStatus = pxlControlTable[0].rowColumnGoodStatus;
   mDummyState = pxlControlTable[0].dummyState;

   return kStOk;
}
//_______________________________________________
Int_t StPxlRawHitMaker::Make()
{
   LOG_INFO << "StPxlRawHitMaker::Make()" << endm;

   // prepare output data collection
   TObjectSet* pxlRawHitDataSet = (TObjectSet*)GetDataSet("pxlRawHit");
   
   if (!pxlRawHitDataSet)
   {
      LOG_INFO << " pxlRawHitCollection does NOT exist! Create a new one! " << endm;
      mPxlRawHitCollection = new StPxlRawHitCollection();
      ToWhiteBoard("pxlRawHit", mPxlRawHitCollection);
   } else
   {
      LOG_INFO << " pxlRawHitCollection exists! Append raw hits to this collection!" << endm;
      mPxlRawHitCollection = (StPxlRawHitCollection*)pxlRawHitDataSet->GetObject();
      LOG_INFO << " Before RTS unpacking. Number of PxlRawHits = " << mPxlRawHitCollection->numberOfRawHits() << endm;      
   }

   // input data loop (should be 10 loops for 10 sectors normally)
   StRtsTable *rts_table = 0;
   while ( (rts_table = GetNextDaqElement("pxl/raw")) ) {
      mSectorData = (UInt_t *)rts_table->At(0);
      mSectorDataLength = rts_table->GetNRows();

      if (Debug() > 1) {
         LOG_INFO << "Found pxl sector raw data at " << hex << mSectorData << dec << "    length in byte: " << mSectorDataLength << "   in UInt_t: " << mSectorDataLength / sizeof(UInt_t) << endm;
      }

      decodeSectorData();

      decodeHitsData();
   }

   LOG_INFO << " Finishing PxlRawHitMaker. Number of PxlRawHits = " << mPxlRawHitCollection->numberOfRawHits() << endm;
      
   return kStOk;
}
//_______________________________________________                                                                                                                                                  
Int_t StPxlRawHitMaker::Finish()
{
   LOG_INFO << "Jtag file version: "<<mJtagFileVersion<<endm;
   return StMaker::Finish();
}
//_______________________________________________
void StPxlRawHitMaker::decodeSectorData()
{
   int index = 0;
   mSector = 0;
   mHeaderData = 0;
   mHitsData = 0;
   mHitsDataLength = 0;
   mTrailerData = 0;
   mTrailerDataLength = 0;

   if (mSectorDataLength == 0 || !mSectorData) {
      LOG_WARN << "no sector data" << endm;
      return;
   }

   // check header token and get pointer to header data block
   if (mSectorData[0] != mHeaderToken) {
      LOG_WARN << "no pxl sector header token" << endm;
      return;
   }
   else {
      if (Debug() > 1) {
         LOG_INFO << "sector data header token correct: 0x" << hex << mSectorData[0] << dec << endm;
      }
   }

   mHeaderData = mSectorData + index;
   index += mHeaderLength;

   mSector = mHeaderData[mHardwareIdPosition] & 0xf;
   if (mSector > kNumberOfPxlSectors || mSector < 1) {
      LOG_WARN << "wrong sector number: " << mSector << endm;
      return;
   }

   mJtagFileVersion = mHeaderData[mHardwareIdPosition] >> 16;

   for(int i=0; i<32; i++)
      {
         if(mHeaderData[8] >> i && Debug() > 2)
            {
               LOG_WARN << "sector "<<mSector<<"  sensor "<<i+1<<"  deserialization error!" << endm;
            }
      }

   for(int i=0; i<8; i++)
      {
         if(mHeaderData[9] >> i && Debug() > 2)
            {
               LOG_WARN << "sector "<<mSector<<"  sensor "<<i+33<<"  deserialization error!" << endm;
            }
      }

   if(mHeaderData[9] >> 8)
      {
         LOG_WARN << "sector "<<mSector<<"  event memory 1 overflow!" << endm;
      }

   if(mHeaderData[9] >> 9)
      {
         LOG_WARN << "sector "<<mSector<<"  event memory 2 overflow!" << endm;
      }

   for(int i=0; i<32; i++)
      {
         if(mHeaderData[10] >> i)
            {
               LOG_WARN << "sector "<<mSector<<"  sensor "<<i+1<<"  trailer or event length error!" << endm;
            }
      }

   for(int i=0; i<8; i++)
      {
         if(mHeaderData[11] >> i)
            {
               LOG_WARN << "sector "<<mSector<<"  sensor "<<i+33<<"  trailer or event length error!" << endm;
            }
      }

   // get hits data block length
   mHitsDataLength = getHitsDataLength();
   if (Debug() > 1) {
      LOG_INFO << "sector: " << mSector << "     HitsDataLength: " << mHitsDataLength << endm;
   }
   index ++;

   // get pointer to hits data block
   mHitsData = mSectorData + index;

   index += mHitsDataLength;

   // check separater at the end of hits data block
   if (mSectorData[index] == mSeparatorToken) {
      if (Debug() > 1) {
         LOG_INFO << "sector data separator token correct: 0x" << hex << mSectorData[index] << dec << endm;
      }
   }
   else {
      LOG_WARN << "sector data separator token wrong: 0x" << hex << mSectorData[index] << dec << endm;
      mHitsData = 0;
      mHitsDataLength = 0;
      return;
   }
   index++;

   // check end token and get pointer to trailer data block
   if (mSectorData[mSectorDataLength / sizeof(UInt_t) - 1] == mEndToken) {
      if (Debug() > 1) {
         LOG_INFO << "sector data end token corret: 0x" << hex << mSectorData[mSectorDataLength / sizeof(UInt_t) - 1] << dec << endm;
      }
   }
   else {
      LOG_WARN << "sector data end token wrong: 0x" << hex << mSectorData[mSectorDataLength / sizeof(UInt_t) - 1] << dec << endm;
      return;
   }

   mTrailerData = mSectorData + index;
   mTrailerDataLength = mSectorDataLength / sizeof(UInt_t) - 1 - index;

}
//_______________________________________________
Int_t StPxlRawHitMaker::getHitsDataLength()
{
   UInt_t a = 0;
   a = *(mHeaderData + mHeaderLength);
   return (a >> 16) + (a & 0xffff);
}
//_______________________________________________
void StPxlRawHitMaker::decodeHitsData()
{
   if (mHitsDataLength == 0 || !mHitsData) {
      LOG_WARN << "no hits data" << endm;
      return;
   }

   mOverFlowCount = 0;
   // decode hits data block word by word
   for (Int_t i = 0; i < mHitsDataLength; i++) {
      decodeWord(mHitsData[i]);
   }
   if (Debug() > 1) {
      LOG_INFO << "sector: " << mSector << "   overflow count: " << mOverFlowCount << endm;
   }
}
//_______________________________________________
void StPxlRawHitMaker::decodeWord(UInt_t val)
{
   int val1 = val >> 16; // higher 16 bits
   int val0 = val & (0xffff); // lower 16 bits

   // get ladder and sensor from chip id = (ladder-1)*10+sensor-1
   int chip_id = mid(mChipIdStartBit, mChipIdEndBit, val0) + mid(mChipIdStartBit, mChipIdEndBit, val1) * mChipIdPow;
   if (chip_id < 1 || chip_id > kNumberOfPxlLaddersPerSector * kNumberOfPxlSensorsPerLadder) {
      LOG_WARN << "wrong chip id: " << chip_id << endm;
      return;
   }
   mLadder = (chip_id - 1) / 10 + 1;
   mSensor = (chip_id - 1) % 10 + 1;

   int row_flag0 = elementGetBit(val0, mRowOrColumnFlagBit); // determine whether the lower 16 bits is for row or for column
   int row_flag1 = elementGetBit(val1, mRowOrColumnFlagBit); // determine whether the higher 16 bits is for row or for column

   // decode the lower 16 bits
   if (row_flag0 == 1) {
      // decode for row number
      decodeState0(val0);
   }
   else {
      // check sensor status, if good, decode for column number and fill raw hits
      if ( (mPxlDb->sensorStatus(mSector, mLadder, mSensor) >= mSensorGoodStatusMin ) &&
            (mPxlDb->sensorStatus(mSector, mLadder, mSensor) <= mSensorGoodStatusMax ) )
         decodeStateN(val0);
   }

   // decode the higher 16 bits
   if (row_flag1 == 1) {
      // decode for row number
      decodeState0(val1);
   }
   else {
      // check sensor status, if good, decode for column number and fill raw hits
      if ( (mPxlDb->sensorStatus(mSector, mLadder, mSensor) >= mSensorGoodStatusMin ) &&
            (mPxlDb->sensorStatus(mSector, mLadder, mSensor) <= mSensorGoodStatusMax ) )
         decodeStateN(val1);
   }
}
//_______________________________________________
UInt_t StPxlRawHitMaker::mid(Int_t start, Int_t end, UInt_t input)
{
   // decode the bits between "start" and "end" in the "input" word
   UInt_t buf;
   buf = input << (32 - end);
   buf = buf >> (32 - end);
   return buf >> start;
}
//_______________________________________________
Int_t StPxlRawHitMaker::elementGetBit(UInt_t data, Int_t position)
{
   // get the bit at "position" of "data" word
   UInt_t sd = data >> position;
   return sd % 2;
}
//_______________________________________________
Int_t StPxlRawHitMaker::decodeState0(Int_t val)
{
   // get row number
    mRow[(mLadder-1)*kNumberOfPxlSensorsPerLadder+mSensor-1] = mid(mDataStartBit, mDataEndBit, val);
   // check overflow
   if (elementGetBit(val, mOverflowBit)) {
       if (Debug() > 2) LOG_WARN << "pxl overflow at sector: " << mSector << " ladder: " << mLadder << " sensor: " << mSensor << " row: " << mRow[(mLadder-1)*kNumberOfPxlSensorsPerLadder+mSensor-1] << endm;
      mOverFlowCount++;
   }
   // check row number range
   if ((mRow[(mLadder-1)*kNumberOfPxlSensorsPerLadder+mSensor-1] >= kNumberOfPxlRowsOnSensor || mRow[(mLadder-1)*kNumberOfPxlSensorsPerLadder+mSensor-1] < 0) && Debug()>2) {
       LOG_WARN << "wrong row: " << mRow[(mLadder-1)*kNumberOfPxlSensorsPerLadder+mSensor-1] << " at sector: " << mSector << " ladder: " << mLadder << " sensor: " << mSensor << endm;
   }
   return 0;
}
//_______________________________________________
Int_t StPxlRawHitMaker::decodeStateN(Int_t val)
{
   int coding = mid(mCodingStartBit, mCodingEndBit, val); // number of sequential fired columns, 0-3 for 1-4 sequential fired columns
   mColumn = mid(mDataStartBit, mDataEndBit, val); // starting column number
   // loop sequential fired columns and fill raw hits
   for (int c = 0; c < coding + 1; c++) {
      // check sector, ladder, sensor row and column range
       if (mSector > 0 && mSector <= kNumberOfPxlSectors && mLadder > 0 && mLadder <= kNumberOfPxlLaddersPerSector && mSensor > 0 && mSensor <= kNumberOfPxlSensorsPerLadder && mRow[(mLadder-1)*kNumberOfPxlSensorsPerLadder+mSensor-1] >= 0 && mRow[(mLadder-1)*kNumberOfPxlSensorsPerLadder+mSensor-1] < kNumberOfPxlRowsOnSensor && mColumn + c < kNumberOfPxlColumnsOnSensor && mColumn + c >= 0) {
         // check raw and column status and hot pixels
           if (mPxlDb->rowStatus(mSector, mLadder, mSensor, mRow[(mLadder-1)*kNumberOfPxlSensorsPerLadder+mSensor-1]) == mRowColumnGoodStatus
             && mPxlDb->columnStatus(mSector, mLadder, mSensor, mColumn + c) == mRowColumnGoodStatus
               && (!mPxlDb->pixelHot(mSector, mLadder, mSensor, mRow[(mLadder-1)*kNumberOfPxlSensorsPerLadder+mSensor-1], mColumn + c))) {
	   // fill raw hit
	   StPxlRawHit pxlRawHit;
	   pxlRawHit.setSector(mSector);
	   pxlRawHit.setLadder(mLadder);
	   pxlRawHit.setSensor(mSensor);
	   pxlRawHit.setRow(mRow[(mLadder-1)*kNumberOfPxlSensorsPerLadder+mSensor-1]);
	   pxlRawHit.setColumn(mColumn + c);
	   pxlRawHit.setIdTruth(0);
	   if (Debug() > 4) pxlRawHit.print();
	   
	   mPxlRawHitCollection->addRawHit(pxlRawHit);
         }
      }
      else if (mColumn != mDummyState && Debug() > 2) { // 1023: dummy state when the last state from a sensor ends on the lower 16 bits of a 32-bit word
          LOG_WARN << "wrong sector/ladder/sensor/row/column: " << mSector << "/" << mLadder << "/" << mSensor << "/" << mRow[(mLadder-1)*kNumberOfPxlSensorsPerLadder+mSensor-1] << "/" << mColumn <<"+"<<c<< endm;
      }
   }
   return 0;
}
