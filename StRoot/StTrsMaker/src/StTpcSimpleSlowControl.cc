/***************************************************************************
 *
 * $Id: StTpcSimpleSlowControl.cc,v 1.2 1999/04/07 00:45:42 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTpcSimpleSlowControl.cc,v $
 * Revision 1.2  1999/04/07 00:45:42  lasiuk
 * addition of gas gain
 *
 * Revision 1.1  1998/11/10 17:12:22  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.5  1998/11/05 19:03:53  lasiuk
 * fix unit integrity to make sure DB call returns in base unit
 *
 * Revision 1.4  1998/11/05 18:19:41  lasiuk
 * drift voltage
 *
 * Revision 1.3  1998/11/04 21:25:05  lasiuk
 * add voltages/ add print/ incorporate units
 *
 * Revision 1.2  1998/05/20 19:01:26  ullrich
 * Renamed getInstance() to instance().
 *
 * Revision 1.1  1998/05/20 14:59:28  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTpcSimpleSlowControl.hh"
#include "StGetConfigValue.hh"

StTpcSlowControl* StTpcSimpleSlowControl::mInstance = 0;

StTpcSimpleSlowControl::StTpcSimpleSlowControl() { /* nopt */}

StTpcSimpleSlowControl::StTpcSimpleSlowControl(const char* fname)
{
    StGetConfigValue(fname,"driftVelocity",mDriftVelocity);
    StGetConfigValue(fname,"driftVoltage",mDriftVoltage);
    StGetConfigValue(fname,"innerSectorAnodeVoltage",mISAnodeVoltage);
    StGetConfigValue(fname,"innerSectorGatingGridV",mISGatingGridVoltage);
    StGetConfigValue(fname,"outerSectorAnodeVoltage",mOSAnodeVoltage);
    StGetConfigValue(fname,"outerSectorGatingGridV",mOSGatingGridVoltage);
    StGetConfigValue(fname,"hallPressure",mHallPressure);
    StGetConfigValue(fname,"hallTemperature",mHallTemperature);

    StGetConfigValue(fname,"innerSectorGasGain",mISGasGain);
    StGetConfigValue(fname,"innerSectorGasGainVzero",mISGasGainVzero);
    StGetConfigValue(fname,"innerSectorGasGainb",mISGasGainb);
    StGetConfigValue(fname,"outerSectorGasGain",mOSGasGain);
    StGetConfigValue(fname,"outerSectorGasGainVzero",mOSGasGainVzero);
    StGetConfigValue(fname,"outerSectorGasGainb",mOSGasGainb);
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

    mISGasGainVzero      *= volt;
    mISGasGainb          /= volt;
    mOSGasGainVzero      *= volt;
    mOSGasGainb          /= volt;
    
    //mHallTemperature /= kelvin;
    mHallPressure        *= atmosphere;
    
}

StTpcSlowControl* StTpcSimpleSlowControl::instance()
{
    if (mInstance == 0) {
	cerr << "StTpcSimpleSlowControl::getInstance(): " << endl;	
	cerr << "\tWARNING" << endl;
	cerr << "\tNo arguments for instantiation of" << endl;
	cerr << "\tsingleton class. All values default to zero." << endl;
        mInstance = new StTpcSimpleSlowControl;
    }
    return mInstance;	
}

StTpcSimpleSlowControl::~StTpcSimpleSlowControl() {/* noop */}

StTpcSlowControl* StTpcSimpleSlowControl::instance(const char* file)
{
    if (mInstance == 0)
	mInstance = new StTpcSimpleSlowControl(file);
    else {
	cerr << "StTpcSimpleSlowControl::getInstance(): " << endl;	
	cerr << "\tWARNING" << endl;
	cerr << "\tSingleton class is already instantiated." << endl;
	cerr << "\tArgument (const char*) is ignored." << endl;
    }
    return mInstance;
}

void StTpcSimpleSlowControl::print(ostream& os) const
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
    os << "Gas Gain:            "    << innerSectorGasGain()               << endl;
    os << "Gas Gain Vzero:       "  << innerSectorGasGainVzero()/volt      << " V"  << endl;
    os << "Gas Gainb:              "    << innerSectorGasGainb()*volt           << " /V"  << endl;
    
    os << "OuterSector:" << endl;
    os << "Anode Voltage:       " << outerSectorAnodeVoltage()/volt      << " V" << endl;
    os << "Gating Grid Voltage: " << outerSectorGatingGridVoltage()/volt << " V" << endl;
    os << "Gas Gain:            "    << outerSectorGasGain()                      << endl;
    os << "Gas Gain Vzero:       "  << outerSectorGasGainVzero()/volt      << " V"  << endl;
    os << "Gas Gainb:              "    << outerSectorGasGainb()*volt           << " /V"  << endl;

    os << endl;
}
