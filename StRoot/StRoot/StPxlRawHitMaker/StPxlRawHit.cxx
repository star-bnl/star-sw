/*!
 * \class StPxlRawHit
 * \author Qiu Hao, Jan 2012
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlRawHit.cxx,v 1.3 2014/01/28 19:29:44 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2012
 ***************************************************************************
 *
 * Description:
 * pixel raw hit before clustering. One raw hit is one fired pixel.
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlRawHit.cxx,v $
 * Revision 1.3  2014/01/28 19:29:44  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#include "StPxlRawHit.h"

ClassImp(StPxlRawHit)

StPxlRawHit::StPxlRawHit() : TObject()
{
   mSector = -1;
   mLadder = -1;
   mSensor = -1;
   mRow = -1;
   mColumn = -1;
   mIdTruth = 0;
}

StPxlRawHit::StPxlRawHit(Int_t sector, Int_t ladder, Int_t sensor, Int_t row, Int_t column, Int_t idTruth) : TObject()
{
   mSector = sector;
   mLadder = ladder;
   mSensor = sensor;
   mRow = row;
   mColumn = column;
   mIdTruth = idTruth;
}

Int_t StPxlRawHit::sector() const {return mSector;}
Int_t StPxlRawHit::ladder() const {return mLadder;}
Int_t StPxlRawHit::sensor() const {return mSensor;}
Int_t StPxlRawHit::row() const {return mRow;}
Int_t StPxlRawHit::column() const {return mColumn;}
Int_t StPxlRawHit::idTruth() const {return mIdTruth;}

void StPxlRawHit::setSector(Int_t sector)
{
   mSector = sector;
}

void StPxlRawHit::setLadder(Int_t ladder)
{
   mLadder = ladder;
}

void StPxlRawHit::setSensor(Int_t sensor)
{
   mSensor = sensor;
}

void StPxlRawHit::setRow(Int_t row)
{
   mRow = row;
}

void StPxlRawHit::setColumn(Int_t column)
{
   mColumn = column;
}

void StPxlRawHit::setIdTruth(Int_t idTruth)
{
   mIdTruth = idTruth;
}

void StPxlRawHit::print()
{
   printf("StPxlRawHit: sec:%i lad:%i sen:%i row:%i col:%i idT:%i\n", sector(), ladder(), sensor(), row(), column(), idTruth());
}
