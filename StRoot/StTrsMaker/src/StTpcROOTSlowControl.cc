/***************************************************************************
 *
 * $Id: StTpcROOTSlowControl.cc,v 1.1 1999/03/23 03:39:23 lasiuk Exp $
 *
 * Author: brian, against his will 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTpcROOTSlowControl.cc,v $
 * Revision 1.1  1999/03/23 03:39:23  lasiuk
 * Initial Revsion
 *
 **************************************************************************/
#ifdef __ROOT__
#include "StTpcROOTSlowControl.hh"

StTpcSlowControl* StTpcROOTSlowControl::mInstance = 0;

StTpcROOTSlowControl::StTpcROOTSlowControl() { /* nopt */}

StTpcROOTSlowControl::StTpcROOTSlowControl(slowcontrolDataSet* dS)
{
    mDriftVelocity       = dS->driftVelocity;
    mDriftVoltage        = dS->driftVoltage;
    mISAnodeVoltage      = dS->innerSectorAnodeVoltage;
    mISGatingGridVoltage = dS->innerSectorGatingGridV;
    mOSAnodeVoltage      = dS->outerSectorAnodeVoltage;
    mOSGatingGridVoltage = dS->outerSectorGatingGridV;
    mHallPressure        = dS->hallPressure;
    mHallTemperature     = dS->hallTemperature;
		     
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif

    // Make sure of unit integrity:
    mDriftVelocity       *= (centimeter/(second*1.e-6));
    mDriftVoltage        *= volt;
    mISAnodeVoltage      *= volt;
    mISGatingGridVoltage *= volt;
    mOSAnodeVoltage      *= volt;
    mOSGatingGridVoltage *= volt;

    //mHallTemperature /= kelvin;
    mHallPressure        *= atmosphere;
    
}

StTpcSlowControl* StTpcROOTSlowControl::instance()
{
    if (mInstance == 0) {
	cerr << "StTpcROOTSlowControl::getInstance(): " << endl;	
	cerr << "\tWARNING" << endl;
	cerr << "\tNo arguments for instantiation of" << endl;
	cerr << "\tsingleton class. All values default to zero." << endl;
        mInstance = new StTpcROOTSlowControl;
    }
    return mInstance;	
}

StTpcROOTSlowControl::~StTpcROOTSlowControl() {/* noop */}

StTpcSlowControl* StTpcROOTSlowControl::instance(slowcontrolDataSet* dS)
{
    if (mInstance == 0)
	mInstance = new StTpcROOTSlowControl(dS);
    else {
	cerr << "StTpcROOTSlowControl::getInstance(): " << endl;	
	cerr << "\tWARNING" << endl;
	cerr << "\tSingleton class is already instantiated." << endl;
	cerr << "\tArgument (const char*) is ignored." << endl;
    }
    return mInstance;
}

void StTpcROOTSlowControl::print(ostream& os) const
{
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif
    os << "Slow Control Parameters:" << endl;
    os << "========================" << endl;
    os << "Drift Velocity:      " << driftVelocity()/(centimeter/(1.e-6*second))   << " cm/us" << endl;
    os << "Drift Voltage:       " << driftVoltage()/volt                 << " V" << endl;
    os << endl;
    os << "Hall Temperature:    " << hallTemperature()                   << " C" << endl;
    os << "Hall Pressure:       " << hallPressure()/atmosphere           << " atm" << endl;
    os << endl;
    os << "InnerSector:" << endl;
    os << "Anode Voltage:       " << innerSectorAnodeVoltage()/volt      << " V" << endl;
    os << "Gating Grid Voltage: " << innerSectorGatingGridVoltage()/volt << " V" << endl;
    os << "OuterSector:" << endl;
    os << "Anode Voltage:       " << outerSectorAnodeVoltage()/volt      << " V" << endl;
    os << "Gating Grid Voltage: " << outerSectorGatingGridVoltage()/volt << " V" << endl;
    os << endl;
}
#endif
