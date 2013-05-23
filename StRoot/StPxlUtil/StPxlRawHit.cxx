/*!
 * \class StPxlRawHit 
 * \author Qiu Hao, Jan 2012
 */
/***************************************************************************
 * 
 * $Id: StPxlRawHit.cxx,v 1.2 2013/05/23 21:14:35 qiuh Exp $
 *
 * Author: Qiu Hao, Jan 2012
 ***************************************************************************
 *
 * Description:
 * pixel raw hit before clustering. One raw hit is one fired pixel.
 *
 ***************************************************************************
 *
 * $Log: StPxlRawHit.cxx,v $
 * Revision 1.2  2013/05/23 21:14:35  qiuh
 * *** empty log message ***
 * 
 **************************************************************************/

#include "StPxlRawHit.h"

ClassImp(StPxlRawHit)

StPxlRawHit::~StPxlRawHit() { /* noop */ }

StPxlRawHit::StPxlRawHit()
{
    mSector = -1;
    mLadder = -1;
    mSensor = -1;
    mRow = -1;
    mColumn = -1;
    mIdTruth = 0;
}

StPxlRawHit::StPxlRawHit(int sector, int ladder, int sensor, int row, int column, int idTruth)
{
    mSector = sector;
    mLadder = ladder;
    mSensor = sensor;
    mRow = row;
    mColumn = column;
    mIdTruth = idTruth;
}

int StPxlRawHit::sector() const {return mSector;}
int StPxlRawHit::ladder() const {return mLadder;}
int StPxlRawHit::sensor() const {return mSensor;}
int StPxlRawHit::row() const {return mRow;}
int StPxlRawHit::column() const {return mColumn;}
int StPxlRawHit::idTruth() const {return mIdTruth;}

void StPxlRawHit::setSector(int sector)
{
    mSector = sector;
}

void StPxlRawHit::setLadder(int ladder)
{
    mLadder = ladder;
}

void StPxlRawHit::setSensor(int sensor)
{
    mSensor = sensor;
}

void StPxlRawHit::setRow(int row)
{
    mRow = row;
}

void StPxlRawHit::setColumn(int column)
{
    mColumn = column;
}

void StPxlRawHit::setIdTruth(int idTruth)
{
    mIdTruth = idTruth;
}

void StPxlRawHit::print()
{
    printf("StPxlRawHit: sec:%i lad:%i sen:%i row:%i col:%i idT:%i\n", sector(), ladder(), sensor(), row(), column(), idTruth());
}
