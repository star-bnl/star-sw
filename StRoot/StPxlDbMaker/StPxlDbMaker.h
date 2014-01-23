/*!
 * \class StPxlDbMaker
 * \author J. Bouchet, M. Lomnitz, May 2013
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlDbMaker.h,v 1.5 2014/01/23 01:04:49 qiuh Exp $
 *
 * Author: J. Bouchet, M. Lomnitz, May 2013
 ***************************************************************************
 *
 * Description:
 * Read DB and prepare information on pxl geometry and sensor/row/column status
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 * 
 ***************************************************************************
 *
 * $Log: StPxlDbMaker.h,v $
 * Revision 1.5  2014/01/23 01:04:49  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#ifndef STPXLDBMAKER_H
#define STPXLDBMAKER_H

#include "StMaker.h"
#include "StPxlUtil/StPxlConstants.h"

class St_pxlSensorStatus;
class St_pxlRowColumnStatus;
class pxlSensorStatus_st;
class pxlRowColumnStatus_st;
class TGeoHMatrix;

class StPxlDbMaker : public StMaker {
public: 
    StPxlDbMaker(const char *name="pxlDb");
    ~StPxlDbMaker();
    Int_t  Init();
    Int_t  InitRun(Int_t runNumber);
    Int_t  Make();
    Int_t  Finish();
    void   Clear(const char *opt);

    /// get geoHMatrix for rotation and shift
    TGeoHMatrix *geoHMatrixTpcOnGlobal() const
    {return mGeoHMatrixTpcOnGlobal;}
    TGeoHMatrix *geoHMatrixIdsOnTpc() const
    {return mGeoHMatrixIdsOnTpc;}
    TGeoHMatrix *geoHMatrixPstOnIds() const
    {return mGeoHMatrixPstOnIds;}
    TGeoHMatrix *geoHMatrixPxlOnPst() const
    {return mGeoHMatrixPxlOnPst;}
    TGeoHMatrix *geoHMatrixHalfOnPxl(Int_t half) const   ///< 1: north   2: south
    {return mGeoHMatrixHalfOnPxl[half-1];}
    TGeoHMatrix *geoHMatrixSectorOnHalf(Int_t sector) const
    {return mGeoHMatrixSectorOnHalf[sector-1];}
    TGeoHMatrix *geoHMatrixLadderOnSector(Int_t sector, Int_t ladder) const
    {return mGeoHMatrixLadderOnSector[sector-1][ladder-1];}
    TGeoHMatrix *geoHMatrixSensorOnLadder(Int_t sector, Int_t ladder, Int_t sensor) const
    {return mGeoHMatrixSensorOnLadder[sector-1][ladder-1][sensor-1];}
    TGeoHMatrix *geoHMatrixSensorOnGlobal(Int_t sector, Int_t ladder, Int_t sensor) const
    {return mGeoHMatrixSensorOnGlobal[sector-1][ladder-1][sensor-1];}

    /// get status for sensor/row/column
    Int_t sensorStatus(Int_t sector, Int_t ladder, Int_t sensor) const; ///< 1-9: good or usable status
    Int_t rowStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t row) const; ///< 1: good status
    Int_t columnStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t column) const; ///< 1: good status

    virtual const char *GetCVS() const {
        static const char cvs[]="Tag $Name:  $ $Id: StPxlDbMaker.h,v 1.5 2014/01/23 01:04:49 qiuh Exp $ built "__DATE__" "__TIME__ ;
        return cvs;
    }

private:
    Int_t getPositions(); ///< read and calculate geometry

    TGeoHMatrix *mGeoHMatrixTpcOnGlobal;
    TGeoHMatrix *mGeoHMatrixIdsOnTpc;
    TGeoHMatrix *mGeoHMatrixPstOnIds;
    TGeoHMatrix *mGeoHMatrixPxlOnPst;
    TGeoHMatrix *mGeoHMatrixHalfOnPxl[2];
    TGeoHMatrix *mGeoHMatrixSectorOnHalf[nPxlSectors];
    TGeoHMatrix *mGeoHMatrixLadderOnSector[nPxlSectors][nPxlLaddersPerSector];
    TGeoHMatrix *mGeoHMatrixSensorOnLadder[nPxlSectors][nPxlLaddersPerSector][nPxlSensorsPerLadder];
    TGeoHMatrix *mGeoHMatrixSensorOnGlobal[nPxlSectors][nPxlLaddersPerSector][nPxlSensorsPerLadder];
    pxlSensorStatus_st *mSensorStatusTable;
    pxlRowColumnStatus_st *mRowColumnStatusTable;

  ClassDef(StPxlDbMaker,0)   //StAF chain virtual base class for Makers
};
#endif


