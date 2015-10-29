/*!
 * \class StPxlDb
 * \author Qiu Hao, Jan 2014
 * \Initial Revision.
 */
/***************************************************************************
 *
 * $Id: StPxlDb.h,v 1.9 2014/10/07 19:25:28 smirnovd Exp $
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
 * Revision 1.9  2014/10/07 19:25:28  smirnovd
 * StPxlDbMaker/: Collected all debugging print statements into a single Print() which is called only when Debug2 option is specified
 *
 * Revision 1.8  2014/08/27 16:52:14  qiuh
 * change pxlRowColumnStatus to pxlBadRowColumns to decrease DB szie
 *
 * Revision 1.7  2014/08/06 11:43:34  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.6  2014/07/15 23:28:48  smirnovd
 * Minor style changes
 *
 * Revision 1.5  2014/07/15 23:28:34  smirnovd
 * .msg
 *
 * Revision 1.4  2014/04/01 15:28:18  qiuh
 * add single hot pixel masking
 *
 * Revision 1.3  2014/03/06 00:50:45  smirnovd
 * Nothing changed, it is a test
 *
 * Revision 1.2  2014/01/28 19:29:37  qiuh
 * *** empty log message ***
 *
 *
 **************************************************************************/

#ifndef StPxlDb_h
#define StPxlDb_h
#define __NEW_PXLDB__

#include "StPxlUtil/StPxlConstants.h"
#include "StObject.h"
#include "TGeoMatrix.h"
#ifndef  __NEW_PXLDB__
#include <map>

class pxlSensorStatus_st;
class pxlRowColumnStatus_st;
class pxlBadRowColumns_st;
class pxlHotPixels_st;
class Survey_st;
class pxlSensorTps_st;
class pxlControl_st;
#else /*  __NEW_PXLDB__ */
#include "StDetectorDbMaker/StIstSurveyC.h"
#include "StDetectorDbMaker/StPxlSurveyC.h"
#include "StDetectorDbMaker/St_pxlSensorStatusC.h"
#include "StDetectorDbMaker/St_pxlRowColumnStatusC.h"
#include "StDetectorDbMaker/St_pxlBadRowColumnsC.h"
#include "StDetectorDbMaker/St_pxlHotPixelsC.h"
#include "StDetectorDbMaker/St_pxlControlC.h"
#include "StDetectorDbMaker/St_pxlSensorTpsC.h"
#endif /* ! __NEW_PXLDB__ */
class StThinPlateSpline;


/*!
 * A convenience data container to hold information about the PXL position
 * survey measurements, current channel/pixel status, and other online
 * parameters saved in the STAR database.
 *
 * With the survey data the transformation to the global STAR coordinate system
 * are represented as:
 *
 * <pre>
 * GlobalXyz = TpcOnGlobal * IdsOnTpc * PxlOnIds * HalfOnPxl * SectorOnHalf * LadderOnSector * SensorOnLadder * SensorLocalXyz
 *
 * numbering :
 * Id  = (sector-1)*40 + (ladder-1)*10 + sensor
 * 1<= sector <= 10
 * 1<= ladder <= 4
 * 1<= sensor <= 10
 * </pre>
 *
 * More information about the PXL software packages and organization can be
 * found at
 * https://www.star.bnl.gov/protected/heavy/qiuh/HFT/software/PXL_software.pdf
 */
class StPxlDb : public StObject
{
public:
   StPxlDb();
   virtual ~StPxlDb() {fgInstance = 0; SafeDelete(mRowColumnStatusTable);}
   static StPxlDb* 	instance() {return fgInstance;}

   //! geoHMatrices describing rotation + shift tranlations between different coordinate systems
#ifndef  __NEW_PXLDB__
   const TGeoHMatrix *geoHMatrixTpcOnGlobal() const
     {return mGeoHMatrixTpcOnGlobal;}
#endif /* ! __NEW_PXLDB__ */
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
#ifndef  __NEW_PXLDB__
   Int_t sensorStatus(Int_t sector, Int_t ladder, Int_t sensor) const; ///< 1-9: good or usable status
   Int_t rowStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t row) const; ///< 1: good status
   Int_t columnStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t column) const; ///< 1: good status
   Int_t pixelHot(Int_t sector, Int_t ladder, Int_t sensor, Int_t row, Int_t column) const; ///< 1: hot; 0: good
#else /*  __NEW_PXLDB__ */
   Int_t sensorStatus(Int_t sector, Int_t ladder, Int_t sensor) const { ///< 1-9: good or usable status
     return St_pxlSensorStatusC::instance()->sensorStatus(sector, ladder, sensor);
   }
   Int_t rowStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t row) const { ///< 1: good status
     return St_pxlRowColumnStatusC::instance()->rowStatus(sector, ladder, sensor, row);
   }
   Int_t columnStatus(Int_t sector, Int_t ladder, Int_t sensor, Int_t column) const {; ///< 1: good status
     return St_pxlRowColumnStatusC::instance()->columnStatus(sector, ladder, sensor, column);
   }
   Int_t pixelHot(Int_t sector, Int_t ladder, Int_t sensor, Int_t row, Int_t column) const { ///< 1: hot; 0: good
     return St_pxlHotPixelsC::instance()->pixelHot(sector, ladder, sensor, row, column);
   }
#endif /* ! __NEW_PXLDB__ */
   const StThinPlateSpline *thinPlateSpline(Int_t sector, Int_t ladder, Int_t sensor) const ///< thin plate spline function to describe the sensor surface
   {return mThinPlateSpline[sector - 1][ladder - 1][sensor - 1];}

   /*! Control parameters for raw data decoding and so on */
#ifndef  __NEW_PXLDB__
   const pxlControl_st *pxlControl() {return mPxlControl;}

#else /*  __NEW_PXLDB__ */
   const pxlControl_st *pxlControl() {return St_pxlControlC::instance()->Struct();}
#endif /* ! __NEW_PXLDB__ */
#ifndef  __NEW_PXLDB__
   void setGeoHMatrices(Survey_st **tables); ///< set geoHMatrix parameters with parameters from Survey_st tables
   void setSensorStatus(pxlSensorStatus_st *sensorStatus) {mSensorStatusTable = sensorStatus;}
   void setRowColumnStatus(pxlRowColumnStatus_st *rowColumnStatus) {mRowColumnStatusTable = rowColumnStatus;}
#else /*  __NEW_PXLDB__ */
   void setGeoHMatrices();                   ///< set geoHMatrix parameters with parameters from Survey_st tables
#endif /* ! __NEW_PXLDB__ */
   void setBadRowColumns(pxlBadRowColumns_st *badRowColumns);
#ifndef  __NEW_PXLDB__
   void setHotPixels(pxlHotPixels_st *hotPixelsTable);
#endif /* ! __NEW_PXLDB__ */
   void setThinPlateSpline(pxlSensorTps_st *pxlSensorTps); ///< create sensor thin plate spline functions and set their parameters
#ifndef  __NEW_PXLDB__
   void setPxlControl(pxlControl_st *pxlControl) {mPxlControl = pxlControl;}
#endif /* ! __NEW_PXLDB__ */

   virtual void Print(Option_t *opt = "") const;
   virtual const char *GetCVS() const {
      static const char cvs[] = "Tag $Name:  $ $Id: StPxlDb.h,v 1.9 2014/10/07 19:25:28 smirnovd Exp $ built " __DATE__ " " __TIME__ ;
      return cvs;
   }

private:
   //! geoHMatrices describing rotation + shift translations between different coordinate systems
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
#ifndef __NEW_PXLDB__
   pxlSensorStatus_st *mSensorStatusTable;
#endif /* !__NEW_PXLDB__ */
   pxlRowColumnStatus_st *mRowColumnStatusTable;
#ifndef __NEW_PXLDB__
   map<unsigned int,short> mMapHotPixels; //! 
   pxlControl_st *mPxlControl; ///< control parameters for raw data decoding and so on
#endif /* !__NEW_PXLDB__ */
   StThinPlateSpline *mThinPlateSpline[kNumberOfPxlSectors][kNumberOfPxlLaddersPerSector][kNumberOfPxlSensorsPerLadder]; ///< thin plate spline function to describe the sensor surface
   static StPxlDb       *fgInstance;

   ClassDef(StPxlDb, 0)
};
#endif /* StPxlDb_h */
