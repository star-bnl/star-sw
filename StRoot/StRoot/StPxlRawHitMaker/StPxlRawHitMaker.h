/*!
 * \class StPxlRawHitMaker
 * \author Jan Rusnak, Qiu Hao, Jan 2013, according codes from Xiangming Sun
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlRawHitMaker.h,v 1.8 2016/03/03 07:36:10 qiuh Exp $
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
 * $Log: StPxlRawHitMaker.h,v $
 * Revision 1.8  2016/03/03 07:36:10  qiuh
 * fix bug on row number, should use last row information FOR THE SAME SENSOR
 *
 * Revision 1.7  2014/08/06 11:43:35  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.6  2014/04/05 05:20:08  qiuh
 * add Jtag file version print-out and some more warnings for data format errors
 *
 * Revision 1.5  2014/01/28 19:29:44  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#ifndef STAR_StPxlRawHitMaker
#define STAR_StPxlRawHitMaker

#include "StMaker.h"
#include "StRTSBaseMaker.h"

class StPxlRawHitCollection;
class StPxlDb;

class StPxlRawHitMaker : public StRTSBaseMaker
{
public:
   StPxlRawHitMaker(const char *name = "pxl_raw_hit");
   Int_t InitRun(Int_t runumber);
   void Clear(const Option_t * = "");
   Int_t Make();
   Int_t Finish();

   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlRawHitMaker.h,v 1.8 2016/03/03 07:36:10 qiuh Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
   }

protected:
   void decodeSectorData(); ///< decode data of a sector
   Int_t getHitsDataLength(); ///< get length of the hits data block
   void decodeHitsData(); ///< decode the hits data block of a sector
   void decodeWord(UInt_t val); ///< decode a word (32 bits)
   UInt_t mid(Int_t start, Int_t end, UInt_t input); ///< decode the bits between "start" and "end" in the "input" word
   Int_t elementGetBit(UInt_t data, Int_t position); ///< get the bit at "position" of "data" word
   Int_t decodeState0(Int_t val); ///< decoding mainly to get the row number for the following fired columns
   Int_t decodeStateN(Int_t val); ///< decoding mainly to get fired column numbers in the current row

   //! pointers and lengths for data blocks
   UInt_t *mSectorData;
   Int_t mSectorDataLength;
   UInt_t *mHeaderData;
   UInt_t *mHitsData;
   Int_t mHitsDataLength;
   UInt_t *mTrailerData;
   Int_t mTrailerDataLength;

   Int_t mOverFlowCount; ///< count for overflow rows

   //! current sector, ladder, sensor, row, column that is being worked on
   Int_t mSector; ///< sector 1-10
   Int_t mLadder; ///< ladder 1-4
   Int_t mSensor; ///< sensor 1-10
   Int_t mRow[40]; ///< row 0-927, data from 40 sensors on a sector is mixed, need to keep 40 current row numbers
   Int_t mColumn; ///< column 0-959

   Int_t mJtagFileVersion; ///< Jtag configure file version

   StPxlDb *mPxlDb; ///< pxl db structure containing geometry, db information and so on

   StPxlRawHitCollection *mPxlRawHitCollection; ///< generated raw hit collection

   //! decoding control paramters according to firmware
   Short_t mHeaderLength;
   Short_t mHardwareIdPosition; ///< position for hardware id, including sector number
   UInt_t  mHeaderToken;
   UInt_t  mSeparatorToken;
   UInt_t  mEndToken;
   Short_t mChipIdStartBit; ///< start bit for chip id
   Short_t mChipIdEndBit; ///< end bit for chip id
   Short_t mChipIdPow; ///< chipId = mChipIdPow*chipIdFromHigher16Bits+chipIdFromLower16Bits
   Short_t mOverflowBit; ///< bit for row overflow (more fired columns than can be read)
   Short_t mRowOrColumnFlagBit; ///< bit for rowOrColumnFlag, which determine whether "data" is row or column number
   Short_t mCodingStartBit; ///< start bit for "coding", which means how many sequential fired columns
   Short_t mCodingEndBit; ///< end bit for "coding", which means how many sequential fired columns
   Short_t mDataStartBit; ///< start bit for "data", which can be row or column number, depending on rowOrColumnFlag
   Short_t mDataEndBit; ///< end bit for "data", which can be row or column number, depending on rowOrColumnFlag
   Short_t mDummyState; ///< dummy state when the last state from a sensor ends on the lower 16 bits of a 32-bit word

   //! sensor good status range
   Short_t mSensorGoodStatusMin;
   Short_t mSensorGoodStatusMax;

   //! row and column good status
   Short_t mRowColumnGoodStatus;

   ClassDef(StPxlRawHitMaker, 1)  //StAF chain virtual base class for Makers
};

#endif

