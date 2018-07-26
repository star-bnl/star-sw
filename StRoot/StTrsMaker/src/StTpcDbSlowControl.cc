/***************************************************************************
 *
 * $Id: StTpcDbSlowControl.cc,v 1.5 2009/11/03 14:34:19 fisyak Exp $
 *
 * Authors: Manuel Calderon de la Barca Sanchez
 *          Brian Lasiuk
 *          Sept 13, 1999
 ***************************************************************************
 *
 * Description: Class to hold Slow Control parameters for TRS taken
 *                from the STAR TPC DB. Class implemented as Singleton
 *
 ***************************************************************************
 *
 * $Log: StTpcDbSlowControl.cc,v $
 * Revision 1.5  2009/11/03 14:34:19  fisyak
 * Remove default in zFromTB
 *
 * Revision 1.4  2000/03/15 17:39:48  calderon
 * Remove beeps
 *
 * Revision 1.3  2000/02/10 01:21:49  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.2  2000/01/10 23:14:29  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.1  1999/10/11 23:55:21  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 *
 **************************************************************************/
#include "StTpcDbSlowControl.hh"

#ifndef ST_NO_EXCEPTIONS
#   include <stdexcept>
#   if !defined(ST_NO_NAMESPACES)
        using std::invalid_argument;
#   endif
#endif

//#include "StUtilities/StMessMgr.h"


StTpcSlowControl* StTpcDbSlowControl::mInstance = 0;

StTpcDbSlowControl::StTpcDbSlowControl() { /* nopt */}

StTpcDbSlowControl::StTpcDbSlowControl(StTpcDb* globalDbPointer)
{
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif
    gTpcDbPtr            = globalDbPointer;

//     mDriftVelocity       = gTpcDbPtr->SlowControlSim()->driftVelocity();
//     mDriftVoltage        = gTpcDbPtr->SlowControlSim()->driftVoltage();
//     mISAnodeVoltage      = gTpcDbPtr->SlowControlSim()->innerSectorAnodeVoltage();
//     mISGatingGridVoltage = gTpcDbPtr->SlowControlSim()->innerSectorGatingGridV();
//     mOSAnodeVoltage      = gTpcDbPtr->SlowControlSim()->outerSectorAnodeVoltage();
//     mOSGatingGridVoltage = gTpcDbPtr->SlowControlSim()->outerSectorGatingGridV();

//     mISGasGain           = gTpcDbPtr->SlowControlSim()->innerSectorGasGain();
//     mISGasGainVzero      = gTpcDbPtr->SlowControlSim()->innerSectorGasGainVzero();
//     mISGasGainb          = gTpcDbPtr->SlowControlSim()->innerSectorGasGainb();
    
//     mOSGasGain           = gTpcDbPtr->SlowControlSim()->outerSectorGasGain();
//     mOSGasGainVzero      = gTpcDbPtr->SlowControlSim()->outerSectorGasGainVzero();
//     mOSGasGainb          = gTpcDbPtr->SlowControlSim()->outerSectorGasGainb();
    
//     mHallPressure        = gTpcDbPtr->SlowControlSim()->hallPressure();
//     mHallTemperature     = gTpcDbPtr->SlowControlSim()->hallTemperature();

//     mDriftVelocity       *= (centimeter/(microsecond));
//     mDriftVoltage        *= volt;
//     mISAnodeVoltage      *= volt;
//     mISGatingGridVoltage *= volt;
//     mOSAnodeVoltage      *= volt;
//     mOSGatingGridVoltage *= volt;


//     mISGasGainVzero      *= volt;
//     mISGasGainb          /= volt;
    

//     mOSGasGainVzero      *= volt;
//     mOSGasGainb          /= volt;
    
//     mHallPressure       *= atmosphere;

}

StTpcSlowControl* StTpcDbSlowControl::instance()
{
    if (!mInstance) {
#ifndef ST_NO_EXCEPTIONS
	throw invalid_argument("StTpcDbSlowControl::getInstance(): Argument Missing!");
#else	
	cerr << "StTpcDbSlowControl::instance() " << endl;	
	cerr << "\tWARNING" << endl;
	cerr << "\tNo arguments for instantiation of" << endl;
	cerr << "Exiting..." << endl;
#endif
    }
    return mInstance;	
}

StTpcDbSlowControl::~StTpcDbSlowControl() {/* noop */}

StTpcSlowControl* StTpcDbSlowControl::instance(StTpcDb* globalDbPointer)
{
    if (!mInstance)
	mInstance = new StTpcDbSlowControl(globalDbPointer);
    else {
	cerr << "StTpcDbSlowControl::instance() " << endl;	
	cerr << "\tWARNING" << endl;
	cerr << "\tSingleton class is already instantiated." << endl;
	cerr << "\tArgument (const char*) is ignored." << endl;
    }
    return mInstance;
}

void StTpcDbSlowControl::print(ostream& os) const
{
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif
    os << "Slow Control Parameters:" << endl;
    os << "========================" << endl;    
    os << "Drift Velocity: East   " << driftVelocity(13)/(centimeter/(1.e-6*second))   
       << " West   " << driftVelocity( 1)/(centimeter/(1.e-6*second))   
       << " cm/us" << endl;
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
