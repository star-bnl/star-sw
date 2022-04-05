// $Id: StFtpcDbReader.hh,v 1.30 2010/04/08 16:46:16 jcs Exp $
//
// $Log: StFtpcDbReader.hh,v $
// Revision 1.30  2010/04/08 16:46:16  jcs
// swap data for RDO6,RDO7 FTPC East when Calibrations_ftpc/ftpcElectronics->swapRDO6RDO7East=1
//
// Revision 1.29  2009/08/04 08:37:28  jcs
// When the flaser option is included in the bfc, the 'perfect' gain table and
// adjustAverageWest = adjustAverageEast = 0.0, will be used for cluster finding
//
// Revision 1.28  2008/07/30 14:47:32  jcs
// if microsecondsPerTimebin calculated from RHIC clock, write the new value for mMicrosecondsPerTimebin back into
// Calibrations_ftpc/ftpcElectronics table
//
// Revision 1.27  2008/05/13 19:12:00  jcs
// added laserTZero to Calibrations_ftpc/ftpcElectronics
//
// Revision 1.26  2006/10/17 19:11:43  fisyak
// Add definition of mPadPitch
//
// Revision 1.25  2006/03/13 19:26:59  jcs
// add constructor StFtpcCalibMaker
//
// Revision 1.24  2005/12/12 13:40:12  jcs
// simplify StFtpcDbReader
//
// Revision 1.23  2005/10/26 14:03:43  jcs
// Change setMicrosecondsPerTimebin
//
// Revision 1.22  2005/10/24 13:43:01  jcs
// If microsecondsPerTimebin calculated from RHIC clock frequency, store in
// Calibrations_ftpc/ftpcElectronics database table. Otherwise get default
// value from Calibrations_ftpc/ftpcElectronics database table.
//
// Revision 1.21  2005/10/14 07:29:01  jcs
// Calculate microsecondsPerTimebin from RHIC ClockFrequency
// If RHIC ClockFrequency = 0, use default value from database
//
// Revision 1.20  2004/07/18 14:10:04  jcs
// get adjustAverageWest/East from Calibrations_ftpc/ftpcGas
//
// Revision 1.19  2003/07/03 13:21:54  fsimon
// Added cathode offset information to constructor for SlowSimulator
//
// Revision 1.18  2003/06/13 12:11:12  jcs
// change constructor comment to specify that it is also used for StFtpcMixerMaker
//
// Revision 1.17  2003/06/12 10:01:26  jcs
// renamed ftpcClusterGeometry database table to ftpcClusterGeom
// (name was too long)
//
// Revision 1.16  2003/06/11 12:06:03  jcs
// get inner cathode and cluster geometry parameters from database
//
// Revision 1.15  2003/06/10 13:13:51  jcs
// get mix,max gas temperature and pressure limits from database
//
// Revision 1.14  2003/05/06 20:19:40  mora
// Add a new constructor only with FTPC dimensions and geometry for ITTF
//
// Revision 1.13  2003/02/19 14:50:39  jcs
// get default temperatures from database
//
// Revision 1.12  2003/01/14 12:58:01  jcs
// use Geometry_ftpc/ftpcAsicMap to control corrections for error in Y2001-2002
// FTPC asic mapping
//
// Revision 1.11  2002/10/15 09:46:46  fsimon
// Constructor used by SlowSimulator changed to include Db access to
// ftpcAmpSlope, ftpcAmpOffset and ftpcTimeOffset
//
// Revision 1.10  2002/01/21 22:12:53  jcs
// add gas temperature difference between west and east FTPC to database
//
// Revision 1.9  2001/11/21 12:36:27  jcs
// make ftpcGas database table available to FTPC cluster maker
//
// Revision 1.8  2001/10/29 12:54:43  jcs
// add new constructor for StFtpcDriftMapMaker
//
// Revision 1.7  2001/10/19 09:40:11  jcs
// tZero now in data base in ftpcElectronics
//
// Revision 1.6  2001/08/16 18:27:54  jcs
// add inline get function for driftCathodeVoltage
//
// Revision 1.5  2001/07/11 21:18:01  jcs
// changes for new FTPC database structures
//
// Revision 1.4  2001/04/04 17:08:42  jcs
// remove references to StFtpcParamReader from StFtpcDbReader
//
// Revision 1.3  2001/04/02 12:10:24  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters
// from StarDb/ftpc
//
// Revision 1.2  2001/03/19 15:52:47  jcs
// use ftpcDimensions from database
//
// Revision 1.1  2001/03/06 23:34:06  jcs
// use database instead of params
//
//

#ifndef STAR_StFtpcDbReader
#define STAR_StFtpcDbReader

#include "StMaker.h"

#include "TObject.h"

#include "tables/St_ftpcDimensions_Table.h"
#include "tables/St_ftpcPadrowZ_Table.h"
#include "tables/St_ftpcAsicMap_Table.h"
#include "tables/St_ftpcEField_Table.h"
#include "tables/St_ftpcVDrift_Table.h"
#include "tables/St_ftpcDeflection_Table.h"
#include "tables/St_ftpcdVDriftdP_Table.h"
#include "tables/St_ftpcdDeflectiondP_Table.h"
#include "tables/St_ftpcAmpSlope_Table.h"
#include "tables/St_ftpcAmpOffset_Table.h"
#include "tables/St_ftpcTimeOffset_Table.h"
#include "tables/St_ftpcGas_Table.h"
#include "tables/St_ftpcDriftField_Table.h"
#include "tables/St_ftpcElectronics_Table.h"
#include "tables/St_ftpcInnerCathode_Table.h"
#include "tables/St_ftpcClusterGeom_Table.h"


class StFtpcDbReader : public TObject 
{
  
protected:
  //ClusterFinder parameters (also used by other classes)
  Int_t mNumberOfPadrows;
  Int_t mNumberOfPadrowsPerSide;
  Int_t mFirstPadrowToSearch;
  Int_t mLastPadrowToSearch;
  Int_t mNumberOfSectors;
  Int_t mFirstSectorToSearch;
  Int_t mLastSectorToSearch;
  Int_t mNumberOfPads;
  Int_t mNumberOfTimebins;
  Int_t mMinTimeBin;
  Int_t mMinTimeBinMed;
  Int_t mMinTimeBinOut;
  Int_t mMaxTimeLength;
  Int_t mMaxTimeLengthMed;
  Int_t mMaxTimeLengthOut;
  Int_t mMaxPadLength;
  Int_t mMaxPadLengthMed;
  Int_t mMaxPadLengthOut;
  Int_t mDeltaTime;
  Int_t mDeltaPad;

  Bool_t mEastIsInverted;
  Bool_t mAsic2EastNotInverted;

  Float_t mPhiOrigin;
  Float_t mPhiPerSector;
  Float_t mRadiansPerPad;
  Float_t mRadiansPerBoundary;
  Float_t mSensitiveVolumeInnerRadius;
  Float_t mSensitiveVolumeOuterRadius;

  Float_t *mPadrowZPosition;

  Int_t   mNumberOfMagboltzBins;
  Int_t   mMaximumNumberOfMagboltzBins;
  Float_t *mMagboltzEField;
  Float_t *mMagboltzVDrift;
  Float_t *mMagboltzDeflection;
  Float_t *mMagboltzdVDriftdP;
  Float_t *mMagboltzdDeflectiondP;

  Float_t mPercentAr;
  Float_t mPercentCO2;
  Float_t mPercentNe;
  Float_t mPercentHe;
  Float_t mGasGain;
  Float_t mGasAttenuation;
  Float_t mGasIonizationPotential;
  Float_t mBaseTemperature;
  Float_t mBasePressure;
  Float_t mPressureOffset;
  Float_t mTemperatureDifference;
  Float_t mDefaultTemperatureWest;
  Float_t mDefaultTemperatureEast;
  Float_t mMinPressure;
  Float_t mMaxPressure;
  Float_t mMinGasTemperature;
  Float_t mMaxGasTemperature;
  Float_t mAdjustAverageWest;
  Float_t mAdjustAverageEast;

  Float_t mTZero;
  Float_t mLaserTZero;
  Float_t mMicrosecondsPerTimebin;
  Bool_t mSwapRDO6RDO7East;

  Float_t mOffsetCathodeWest;
  Float_t mOffsetCathodeEast;
  Float_t mAngleOffsetWest;
  Float_t mAngleOffsetEast;

  Float_t mDriftCathodeVoltage;
  Float_t mMinimumDriftField;
  Float_t mStepSizeDriftField;
  Float_t mRadiusTimesField;

  Float_t mMinChargeWindow;

  ftpcAmpSlope_st   *ampslopeTable;
  ftpcAmpOffset_st  *ampoffsetTable;
  ftpcTimeOffset_st *timeoffsetTable;
  ftpcGas_st* gasTable;
  ftpcElectronics_st *electronicsTable;
  //SlowSimulator parameters
  Float_t mPadLength;
  Float_t mPadPitch;

private:

Bool_t mLaserRun;                         // indicator to know if this is a laser run

Int_t FtpcAmpOffset(St_ftpcAmpOffset *ampoffset);
Int_t FtpcAmpSlope(St_ftpcAmpSlope *ampslope);
Int_t FtpcAsicMap(St_ftpcAsicMap *asicmap);
Int_t FtpcClusterGeom(St_ftpcClusterGeom *clustergeo);
Int_t FtpcdDeflectiondP(St_ftpcdDeflectiondP *ddeflectiondp);
Int_t FtpcDeflection(St_ftpcDeflection *deflection);
Int_t FtpcDimensions(St_ftpcDimensions *dimensions);
Int_t FtpcDriftField(St_ftpcDriftField *driftfield);
Int_t FtpcdVDriftdP(St_ftpcdVDriftdP *dvdriftdp);
Int_t FtpcEField(St_ftpcEField *efield);
Int_t FtpcElectronics(St_ftpcElectronics *electronics);
Int_t FtpcGas(St_ftpcGas *gas);
Int_t FtpcInnerCathode(St_ftpcInnerCathode *cathode);
Int_t FtpcPadrowZ(St_ftpcPadrowZ *zrow);
Int_t FtpcTimeOffset(St_ftpcTimeOffset *timeoffset);
Int_t FtpcVDrift(St_ftpcVDrift *vdrift);
  
public:
  // constructor used by StFtpcClusterMaker:
  StFtpcDbReader(St_ftpcDimensions    *dimensions,
                 St_ftpcPadrowZ       *zrow,
		 St_ftpcAsicMap       *asicmap,
                 St_ftpcEField        *efield,
                 St_ftpcVDrift        *vdrift,
                 St_ftpcDeflection    *deflection,
                 St_ftpcdVDriftdP     *dvdriftdp,
                 St_ftpcdDeflectiondP *ddeflectiondp,
                 St_ftpcAmpSlope      *ampslope,
                 St_ftpcAmpOffset     *ampoffset,
                 St_ftpcTimeOffset    *timeoffset,
                 St_ftpcDriftField    *driftfield,
                 St_ftpcGas           *gas,
                 St_ftpcElectronics   *electronics,
		 St_ftpcInnerCathode  *cathode,
		 St_ftpcClusterGeom *clustergeo);
  // constructor used by StFtpcSlowSimMaker
  StFtpcDbReader(St_ftpcDimensions    *dimensions,
		 St_ftpcAsicMap       *asicmap,
                 St_ftpcEField        *efield,
                 St_ftpcVDrift        *vdrift,
                 St_ftpcDeflection    *deflection,
                 St_ftpcdVDriftdP     *dvdriftdp,
                 St_ftpcdDeflectiondP *ddeflectiondp,
                 St_ftpcGas           *gas,
                 St_ftpcDriftField    *driftfield,
                 St_ftpcElectronics   *electronics,
		 St_ftpcAmpSlope      *ampslope,
                 St_ftpcAmpOffset     *ampoffset,
                 St_ftpcTimeOffset    *timeoffset,
		 St_ftpcInnerCathode  *cathode);
  // constructor used by StFtpcDriftMapMaker:
  StFtpcDbReader(St_ftpcDimensions    *dimensions,
                 St_ftpcPadrowZ       *zrow,
                 St_ftpcEField        *efield,
                 St_ftpcVDrift        *vdrift,
                 St_ftpcDeflection    *deflection,
                 St_ftpcdVDriftdP     *dvdriftdp,
                 St_ftpcdDeflectiondP *ddeflectiondp,
                 St_ftpcGas           *gas,
                 St_ftpcDriftField    *driftfield);
  // constructor used by StFtpcCalibMaker:
  StFtpcDbReader(St_ftpcDimensions    *dimensions,
                 St_ftpcPadrowZ       *zrow,
                 St_ftpcEField        *efield,
                 St_ftpcVDrift        *vdrift,
                 St_ftpcDeflection    *deflection,
                 St_ftpcdVDriftdP     *dvdriftdp,
                 St_ftpcdDeflectiondP *ddeflectiondp,
                 St_ftpcElectronics   *electronics,
                 St_ftpcGas           *gas,
                 St_ftpcDriftField    *driftfield);
  // constructor used by Sti/StFtpcDetectorBuilder and StFtpcMixerMaker:
  StFtpcDbReader(St_ftpcDimensions    *dimensions,
                 St_ftpcPadrowZ       *zrow        );


  ~StFtpcDbReader();

  Int_t returnCode;

  Float_t padrowZPosition(Int_t i); 
  Float_t magboltzEField(Int_t i);
  Float_t magboltzVDrift(Int_t i,Int_t padrow);
  Float_t magboltzDeflection(Int_t i, Int_t padrow);
  Float_t magboltzdVDriftdP(Int_t i, Int_t padrow);
  Float_t magboltzdDeflectiondP(Int_t i, Int_t padrow);
  Float_t amplitudeSlope(Int_t i, Int_t padrow);
  Float_t amplitudeOffset(Int_t i, Int_t padrow);
  Float_t timeOffset(Int_t i, Int_t padrow);

  // parameter set functions
  Int_t setMagboltzEField(Int_t i, Float_t newvalue);
  Int_t setMagboltzVDrift(Int_t i, Int_t padrow, Float_t  newvalue);
  Int_t setMagboltzDeflection(Int_t i, Int_t padrow, Float_t  newvalue);
  Int_t setMagboltzdVDriftdP(Int_t i, Int_t padrow, Float_t  newvalue);
  Int_t setMagboltzdDeflectiondP(Int_t i, Int_t padrow, Float_t  newvalue);
  Int_t setMicrosecondsPerTimebin(Float_t newvalue);
  Bool_t setLaserRun(Bool_t laserRun);

  // inline get functions
  Int_t numberOfPadrows() {return mNumberOfPadrows;}
  Int_t numberOfPadrowsPerSide() {return mNumberOfPadrowsPerSide;}
  Int_t firstPadrowToSearch() {return mFirstPadrowToSearch;}
  Int_t lastPadrowToSearch() {return mLastPadrowToSearch;}
  Int_t numberOfSectors() {return mNumberOfSectors;}
  Int_t firstSectorToSearch() {return mFirstSectorToSearch;}
  Int_t lastSectorToSearch() {return mLastSectorToSearch;}
  Int_t numberOfPads() {return mNumberOfPads;}
  Int_t numberOfTimebins() {return mNumberOfTimebins;}
   
  Bool_t EastIsInverted() {return mEastIsInverted;}
  Bool_t Asic2EastNotInverted() {return mAsic2EastNotInverted;}
  Bool_t  SwapRDO6RDO7East() {return mSwapRDO6RDO7East;}

  Int_t numberOfMagboltzBins() {return mNumberOfMagboltzBins;}
  Int_t maximumNumberOfMagboltzBins() {return mMaximumNumberOfMagboltzBins;}

  Float_t phiOrigin()    {return mPhiOrigin;}
  Float_t phiPerSector() {return mPhiPerSector;}
  Float_t phiEnd()       {return mPhiOrigin+mPhiPerSector;}
  Float_t padLength()    {return mPadLength;}
  Float_t padPitch()     {return mPadPitch;}
  Float_t radiansPerPad() {return mRadiansPerPad;}
  Float_t radiansPerBoundary() {return mRadiansPerBoundary;}
  Float_t sensitiveVolumeInnerRadius() {return mSensitiveVolumeInnerRadius;}
  Float_t sensitiveVolumeOuterRadius() {return mSensitiveVolumeOuterRadius;}

  Float_t percentAr() {return mPercentAr;}
  Float_t percentCO2() {return mPercentCO2;}
  Float_t percentNe() {return mPercentNe;}
  Float_t percentHe() {return mPercentHe;}
  Float_t gasGain() {return mGasGain;}
  Float_t gasAttenuation() {return mGasAttenuation;}
  Float_t gasIonizationPotential() {return mGasIonizationPotential;}
  Float_t baseTemperature() {return mBaseTemperature;}
  Float_t basePressure() {return mBasePressure;}
  Float_t pressureOffset() {return mPressureOffset;}
  Float_t temperatureDifference(){return mTemperatureDifference;}
  Float_t defaultTemperatureWest(){return mDefaultTemperatureWest;}
  Float_t defaultTemperatureEast(){return mDefaultTemperatureEast;}
  Float_t minPressure() {return mMinPressure;}
  Float_t maxPressure() {return mMaxPressure;}
  Float_t minGasTemperature() {return mMinGasTemperature;}
  Float_t maxGasTemperature() {return mMaxGasTemperature;}
  Float_t adjustAverageWest() {return mAdjustAverageWest;}
  Float_t adjustAverageEast() {return mAdjustAverageEast;}

  Float_t tZero() {return mTZero;}
  Float_t laserTZero() {return mLaserTZero;}
  Float_t microsecondsPerTimebin() {return mMicrosecondsPerTimebin;}

  Float_t driftCathodeVoltage() {return mDriftCathodeVoltage;}
  Float_t minimumDriftField() {return mMinimumDriftField;}
  Float_t stepSizeDriftField() {return mStepSizeDriftField;}
  Float_t radiusTimesField() {return mRadiusTimesField;}

  Float_t offsetCathodeWest() {return mOffsetCathodeWest;}
  Float_t offsetCathodeEast() {return mOffsetCathodeEast;}
  Float_t angleOffsetWest() {return mAngleOffsetWest;}
  Float_t angleOffsetEast() {return mAngleOffsetEast;}
   
  Int_t minTimeBin() {return mMinTimeBin;} 
  Int_t minTimeBinMed() {return mMinTimeBinMed;}
  Int_t minTimeBinOut() {return mMinTimeBinOut;}
  Int_t maxTimeLength() {return mMaxTimeLength;}
  Int_t maxPadLength() {return mMaxPadLength;}
  Int_t maxTimeLengthMed() {return mMaxTimeLengthMed;}
  Int_t maxPadLengthMed() {return mMaxPadLengthMed;}
  Int_t maxTimeLengthOut() {return mMaxTimeLengthOut;}
  Int_t maxPadLengthOut() {return mMaxPadLengthOut;}
  Int_t deltaTime() {return mDeltaTime;}
  Int_t deltaPad() {return mDeltaPad;}
  Float_t minChargeWindow() {return mMinChargeWindow;}

};

#endif
