/***************************************************************************
 *
 * $Id: StRunInfo.cxx,v 2.4 2002/02/25 19:32:47 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRunInfo.cxx,v $
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

static const char rcsid[] = "$Id: StRunInfo.cxx,v 2.4 2002/02/25 19:32:47 ullrich Exp $";

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

    for (int i=0; i<2; i++) {
	mBeamMassNumber[i] = 0;
	mTpcDriftVelocity[i] = 0;
	mBeamEnergy[i] = 0;
	mInitialBeamIntensity[i] = 0;
	mBeamLifeTime[i] = 0;
	mBeamFillNumber[i] = 0;
    }
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
