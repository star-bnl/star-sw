/***************************************************************************
 *
 * $Id: StTpcROOTSlowControl.cc,v 1.3 2000/03/15 17:39:49 calderon Exp $
 *
 * Author: brian, against his will 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTpcROOTSlowControl.cc,v $
 * Revision 1.3  2000/03/15 17:39:49  calderon
 * Remove beeps
 *
 * Revision 1.2  1999/04/07 00:45:43  lasiuk
 * addition of gas gain
 *
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
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif

    mDriftVelocity       = dS->driftVelocity *= (centimeter/(second*1.e-6));
    mDriftVoltage        = dS->driftVoltage  *= volt;
    mISAnodeVoltage      = dS->innerSectorAnodeVoltage *= volt;
    mISGatingGridVoltage = dS->innerSectorGatingGridV  *= volt;
    mOSAnodeVoltage      = dS->outerSectorAnodeVoltage *= volt;
    mOSGatingGridVoltage = dS->outerSectorGatingGridV  *= volt;

    mISGasGain           = dS->innerSectorGasGain;
    mISGasGainVzero      = dS->innerSectorGasGainVzero *= volt;
    mISGasGainb          = dS->innerSectorGasGainb     /= volt;
    
    mOSGasGain           = dS->outerSectorGasGain;
    mOSGasGainVzero      = dS->outerSectorGasGainVzero *= volt;
    mOSGasGainb          = dS->outerSectorGasGainb     /= volt;
    
    mHallPressure        = dS->hallPressure            *= atmosphere;
    mHallTemperature     = dS->hallTemperature;


}

StTpcSlowControl* StTpcROOTSlowControl::instance()
{
    if (mInstance == 0) {
	cerr << "StTpcROOTSlowControl::instance() " << endl;	
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
	cerr << "StTpcROOTSlowControl::instance() " << endl;	
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
    os << "Gas Gain:           "    << innerSectorGasGain()               <<  endl;
    os << "Gas Gain Vzero:       "  << innerSectorGasGainVzero()/volt      << " V"  << endl;
    os << "Gas Gainb:              "    << innerSectorGasGainb()*volt           << " /V"  << endl;
    os << "OuterSector:" << endl;
    os << "Anode Voltage:       " << outerSectorAnodeVoltage()/volt      << " V" << endl;
    os << "Gating Grid Voltage: " << outerSectorGatingGridVoltage()/volt << " V" << endl;
    os << "Gas Gain:            "    << outerSectorGasGain()               <<  endl;
    os << "Gas Gain Vzero:       "  << outerSectorGasGainVzero()/volt      << " V"  << endl;
    os << "Gas Gainb:             "    << outerSectorGasGainb()*volt           << " /V"  << endl;
    os << endl;
}
#endif
