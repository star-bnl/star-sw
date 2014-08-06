/*!
 * \class StPxlRawHit
 * \author Qiu Hao, Jan 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlRawHit.h,v 1.4 2014/08/06 11:43:34 jeromel Exp $
 *
 * Author: Qiu Hao, Jan 2013
 ***************************************************************************
 *
 * Description:
 * pixel raw hit before clustering. One raw hit is one fired pixel.
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlRawHit.h,v $
 * Revision 1.4  2014/08/06 11:43:34  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.3  2014/01/28 19:29:44  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#ifndef StPxlRawHit_hh
#define StPxlRawHit_hh

#include "Rtypes.h"
#include "TObject.h"

class StPxlRawHit: public TObject
{
public:
   StPxlRawHit();
   StPxlRawHit(Int_t sector, Int_t ladder, Int_t sensor, Int_t row, Int_t column, Int_t idTruth);

   Int_t  sector() const; ///< sector 1-10
   Int_t  ladder() const; ///< ladder 1-4
   Int_t  sensor() const; ///< sensor 1-10
   Int_t  row() const; ///< row 0-927
   Int_t  column() const; ///< column 0-959
   Int_t  idTruth() const; ///< for embedding, 0 as background

   void setSector(Int_t sector); ///< set sector
   void setLadder(Int_t ladder); ///< set ladder
   void setSensor(Int_t sensor); ///< set sensor
   void setRow(Int_t row); ///< set row
   void setColumn(Int_t column); ///< set column
   void setIdTruth(Int_t idTruth); ///< set idTruth

   void print(); ///< print all information
   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlRawHit.h,v 1.4 2014/08/06 11:43:34 jeromel Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
   }

protected:
   Char_t mSector; ///< sector 1-10
   Char_t mLadder; ///< ladder 1-4
   Char_t mSensor; ///< sensor 1-10
   Short_t mRow; ///< row 0-927
   Short_t mColumn; ///< column 0-959
   UShort_t mIdTruth; ///< for embedding, 0 as background

   ClassDef(StPxlRawHit, 1)
};
#endif
