/*!
 * \class StPxlDb
 * \author Qiu Hao, Jan 2014
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlDb.h,v 1.3 2014/03/06 00:50:45 smirnovd Exp $
 *
 * Author: Qiu Hao, Jan 2014
 ***************************************************************************
 *
 * Description:
 * DB information on pxl geometry and sensor/row/column status
 * More information at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 *
 ***************************************************************************
 *
 * $Log: StPxlDb.h,v $
 * Revision 1.3  2014/03/06 00:50:45  smirnovd
 * Nothing changed, it is a test
 *
 * Revision 1.2  2014/01/28 19:29:37  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#ifndef StPxlDb_hh
#define StPxlDb_hh

#include "StPxlUtil/StPxlConstants.h"
#include "StObject.h"
#include "TGeoMatrix.h"

class pxlSensorStatus_st;
class pxlRowColumnStatus_st;
class Survey_st;
class pxlSensorTps_st;
class pxlControl_st;
class StThinPlateSpline;

class StPxlDb : public StObject
{
public:
   StPxlDb();

   //! geoHMatrices describing rotation + shift tranlations between different coordinate systems
   const TGeoHMatrix *geoHMatrixTpcOnGlobal() const
   {return mGeoHMatrixTpcOnGlobal;}
   const TGeoHMatrix *geoHMatrixIdsOnTpc() const
   {return &mGeoHMatrixIdsOnTpc;}
   const TGeoHMatrix *geoHMatrixPstOnIds() const
   {return &mGeoHMatrixPstOnIds;}
   const TGeoHMatrix *geoHMatrixPxlOnPst() const
   {return &mGeoHMatrixPxlOnPst;}
   const TGeoHMatrix *geoHMatrixHalfOnPxl(Int_t half) const   ///< 1: north   2: south
   {return &mGeoHMatrixHalfOnPxl[half - 1];}
   const TGeoHMatrix *geoHMatrixSectorOnHalf(Int_t sector) const
   {return &mGeoHMatrixSectorOnHalf[sector - 1];}
   const TGeoHMatrix *geoHMatrixLadderOnSector(Int_t sector, Int_t ladder) const
   {return &mGeoHMatrixLadderOnSector[sector - 1][ladder - 1];}
   const TGeoHMatrix *geoHMatrixSensorOnLadder(Int_t sector, Int_t ladder, Int_t sensor) const
   {return &mGeoHMatrixSensorOnLadder[sector - 1][ladder - 1][sensor - 1];}
   const TGeoHMatrix *geoHMatrixSensorOnGlobal(Int_t sector, Int_t ladder, Int_t sensor) const
   {return &mGeoHMatrixSensorOnGlobal[sector - 1][ladder - 1][sensor - 1];}

   //! status for sensor/row/column
   Int_t sensorStatus(Int_t sector, Int_t ladder, Int_t sensor) const; ///< 1-9: good or usable status
   Int_t rowStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t row) const; ///< 1: good status
   Int_t columnStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t column) const; ///< 1: good status

   const StThinPlateSpline *thinPlateSpline(Int_t sector, Int_t ladder, Int_t sensor) const ///< thin plate spline function to describe the sensor surface
   {return mThinPlateSpline[sector - 1][ladder - 1][sensor - 1];}

   const pxlControl_st *pxlControl() ///< control parameters for raw data decoding and so on
   {return mPxlControl;}

   void setGeoHMatrices(Survey_st **tables); ///< set geoHMatrix parameters with parameters from Survey_st tables
   void setSensorStatus(pxlSensorStatus_st *sensorStatus) {mSensorStatusTable = sensorStatus;}
   void setRowColumnStatus(pxlRowColumnStatus_st *rowColumnStatus) {mRowColumnStatusTable = rowColumnStatus;}
   void setThinPlateSpline(pxlSensorTps_st *pxlSensorTps); ///< create sensor thin plate spline functions and set their parameters
   void setPxlControl(pxlControl_st *pxlControl) {mPxlControl = pxlControl;}

   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlDb.h,v 1.3 2014/03/06 00:50:45 smirnovd Exp $ built "__DATE__" "__TIME__ ;
      return cvs;
   }

private:
   //! geoHMatrices describing rotation + shift tranlations between different coordinate systems
   TGeoHMatrix *mGeoHMatrixTpcOnGlobal;
   TGeoHMatrix mGeoHMatrixIdsOnTpc;
   TGeoHMatrix mGeoHMatrixPstOnIds;
   TGeoHMatrix mGeoHMatrixPxlOnPst;
   TGeoHMatrix mGeoHMatrixHalfOnPxl[2];
   TGeoHMatrix mGeoHMatrixSectorOnHalf[kNumberOfPxlSectors];
   TGeoHMatrix mGeoHMatrixLadderOnSector[kNumberOfPxlSectors][kNumberOfPxlLaddersPerSector];
   TGeoHMatrix mGeoHMatrixSensorOnLadder[kNumberOfPxlSectors][kNumberOfPxlLaddersPerSector][kNumberOfPxlSensorsPerLadder];
   TGeoHMatrix mGeoHMatrixSensorOnGlobal[kNumberOfPxlSectors][kNumberOfPxlLaddersPerSector][kNumberOfPxlSensorsPerLadder];

   //! status for sensor/row/column
   pxlSensorStatus_st *mSensorStatusTable;
   pxlRowColumnStatus_st *mRowColumnStatusTable;

   pxlControl_st *mPxlControl; ///< control parameters for raw data decoding and so on
   StThinPlateSpline *mThinPlateSpline[kNumberOfPxlSectors][kNumberOfPxlLaddersPerSector][kNumberOfPxlSensorsPerLadder]; ///< thin plate spline function to describe the sensor surface

   ClassDef(StPxlDb, 0)  //StAF chain virtual base class for Makers
};
#endif
