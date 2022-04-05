/***************************************************************************
 *
 * $Id: StRunInfo.cxx,v 2.7 2004/10/20 16:06:53 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRunInfo.cxx,v $
 * Revision 2.7  2004/10/20 16:06:53  ullrich
 * Add variables to report on space charge and the correction mode.
 *
 * Revision 2.6  2004/07/06 23:05:21  ullrich
 * Added SVT drift velocity scaler.
 *
 * Revision 2.5  2004/01/22 23:14:07  ullrich
 * Added Rhic scaler methods (BBC).
 *
 * Revision 2.4  2002/02/25 19:32:47  ullrich
 * Added more RHIC related info.
 *
 * Revision 2.3  2002/01/31 23:42:36  ullrich
 * Added member to hold BBC coincidence rate.
 *
 * Revision 2.2  2001/12/02 19:27:12  ullrich
 * Added new member and methods.
 *
 * Revision 2.1  2001/09/18 00:14:17  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StRunInfo.h"

static const char rcsid[] = "$Id: StRunInfo.cxx,v 2.7 2004/10/20 16:06:53 ullrich Exp $";

ClassImp(StRunInfo)

StRunInfo::StRunInfo()
{
    mRunId = 0;
    mProductionTime = 0;
    mCenterOfMassEnergy = 0;
    mMagneticFieldZ = 0;

    mZdcEastRate = 0;
    mZdcWestRate = 0;
    mZdcCoincidenceRate = 0;
    mBbcCoincidenceRate = 0;
    mBackgroundRate = 0;
    mL0RateToRich = 0;
    mSvtDriftVelocityScaler = 0;

    for (int i=0; i<2; i++) {
	mBeamMassNumber[i] = 0;
	mTpcDriftVelocity[i] = 0;
	mBeamEnergy[i] = 0;
	mInitialBeamIntensity[i] = 0;
	mBeamLifeTime[i] = 0;
	mBeamFillNumber[i] = 0;
    }

    mBbcEastRate = 0;		 
    mBbcWestRate = 0;		 
    mBbcBlueBackgroundRate = 0;	 
    mBbcYellowBackgroundRate = 0;

    mSpaceChargeCorrectionMode = 0;
    mSpaceCharge = 0;
}

StRunInfo::~StRunInfo() {/* noop */}

int
StRunInfo::runId() const
{return mRunId;}

time_t
StRunInfo::productionTime() const
{return mProductionTime;}

TString
StRunInfo::productionVersion() const
{return mProductionVersion;}

double
StRunInfo::centerOfMassEnergy() const
{return mCenterOfMassEnergy;}

int
StRunInfo::beamMassNumber(StBeamDirection dir) const
{return mBeamMassNumber[dir];}

float
StRunInfo::beamEnergy(StBeamDirection dir) const
{return mBeamEnergy[dir];}

float
StRunInfo::initialBeamIntensity(StBeamDirection dir) const
{return mInitialBeamIntensity[dir];}

float
StRunInfo::beamLifeTime(StBeamDirection dir) const
{return mBeamLifeTime[dir];}

float
StRunInfo::beamFillNumber(StBeamDirection dir) const
{return mBeamFillNumber[dir];}

double
StRunInfo::magneticField() const
{return mMagneticFieldZ;}

double
StRunInfo::tpcDriftVelocity(StBeamDirection dir) const
{return mTpcDriftVelocity[dir];}

double
StRunInfo::svtDriftVelocityScaler() const
{return mSvtDriftVelocityScaler;}

double
StRunInfo::zdcWestRate() const
{return mZdcWestRate;}

double
StRunInfo::zdcEastRate() const
{return mZdcEastRate;}

double
StRunInfo::zdcCoincidenceRate() const
{return mZdcCoincidenceRate;}

double
StRunInfo::bbcCoincidenceRate() const
{return mBbcCoincidenceRate;}

double
StRunInfo::backgroundRate() const
{return mBackgroundRate;}

double
StRunInfo::l0RateToRich() const
{return mL0RateToRich;}

double
StRunInfo::bbcEastRate() const
{return mBbcEastRate;}

double
StRunInfo::bbcWestRate() const
{return mBbcWestRate;}

double
StRunInfo::bbcBlueBackgroundRate() const
{return mBbcBlueBackgroundRate;}

double
StRunInfo::bbcYellowBackgroundRate() const
{return mBbcYellowBackgroundRate;}

int
StRunInfo::spaceChargeCorrectionMode() const
{return mSpaceChargeCorrectionMode;}

float
StRunInfo::spaceCharge() const
{return mSpaceCharge;}

void
StRunInfo::setRunId(int val) {mRunId = val;}

void
StRunInfo::setProductionTime(time_t val)
{mProductionTime = val;}               

void
StRunInfo::setProductionVersion(const char* val)
{mProductionVersion = TString(val);}            

void
StRunInfo::setCenterOfMassEnergy(double val)
{mCenterOfMassEnergy = val;}            

void
StRunInfo::setBeamMassNumber(StBeamDirection dir, int val)
{mBeamMassNumber[dir] = val;}

void
StRunInfo::setBeamEnergy(StBeamDirection dir, float val)
{mBeamEnergy[dir] = val;}    

void
StRunInfo::setInitialBeamIntensity(StBeamDirection dir, float val)
{mInitialBeamIntensity[dir] = val;}    

void
StRunInfo::setBeamLifeTime(StBeamDirection dir, float val)
{mBeamLifeTime[dir] = val;}    

void
StRunInfo::setBeamFillNumber(StBeamDirection dir, float val)
{mBeamFillNumber[dir] = val;}    

void
StRunInfo::setMagneticField(double val)
{mMagneticFieldZ = val;}                

void
StRunInfo::setTpcDriftVelocity(StBeamDirection dir, double val)
{mTpcDriftVelocity[dir] = val;}  

void
StRunInfo::setSvtDriftVelocityScaler(float val)
{mSvtDriftVelocityScaler = val;}  

void
StRunInfo::setZdcWestRate(double val)
{mZdcWestRate = val;}

void
StRunInfo::setZdcEastRate(double val)
{mZdcEastRate = val;}

void
StRunInfo::setZdcCoincidenceRate(double val)
{mZdcCoincidenceRate = val;}

void
StRunInfo::setBbcCoincidenceRate(double val)
{mBbcCoincidenceRate = val;}

void
StRunInfo::setBackgroundRate(double val)
{mBackgroundRate = val;}

void
StRunInfo::setL0RateToRich(double val)
{mL0RateToRich = val;}

void
StRunInfo::setBbcEastRate(double val)
{mBbcEastRate = val;}

void
StRunInfo::setBbcWestRate(double val)
{mBbcWestRate = val;}

void
StRunInfo::setBbcBlueBackgroundRate(double val)
{mBbcBlueBackgroundRate = val;}

void
StRunInfo::setBbcYellowBackgroundRate(double val)
{mBbcYellowBackgroundRate = val;}

void
StRunInfo::setSpaceChargeCorrectionMode(int val)
{mSpaceChargeCorrectionMode = val;}

void
StRunInfo::setSpaceCharge(float val)
{mSpaceCharge = val;}
