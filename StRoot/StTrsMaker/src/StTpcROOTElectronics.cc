/*****************************************************************
 *
 * $Id: StTpcROOTElectronics.cc,v 1.3 2000/01/10 23:14:29 lasiuk Exp $
 *
 * Author: brian March 22, 1999
 *
 *****************************************************************
 * Description: Simple ASCII Database for Electronics parameters for
 *              the STAR Main TPC              
 *
 *****************************************************************
 *
 * $Log: StTpcROOTElectronics.cc,v $
 * Revision 1.3  2000/01/10 23:14:29  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.2  1999/12/08 02:10:41  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.1  1999/03/23 03:39:22  lasiuk
 * Initial Revsion
 *
 ******************************************************************/
#ifdef __ROOT__
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif
#include "StTpcROOTElectronics.hh"

#ifndef ST_NO_EXCEPTIONS
#   include <stdexcept>
#   if !defined(ST_NO_NAMESPACES)
        using std::invalid_argument;
#   endif
#endif
StTpcElectronics* StTpcROOTElectronics::mInstance = 0; // static data member

StTpcROOTElectronics::StTpcROOTElectronics() { /* nopt */ }

StTpcROOTElectronics::StTpcROOTElectronics(electronicsDataSet* dS)
{
    mNominalGain         = dS->nominalGain;
    mSamplingFrequency   = dS->samplingFrequency;
    mTZero               = dS->tZero;
    mAdcConversion       = dS->adcConversion;    
    mAdcConversionCharge = dS->adcCharge;    
    mNumberOfTimeBins    = dS->numberOfTimeBins;    
    mAveragePedestal     = static_cast<int>(dS->averagePedestal);
    mShapingTime         = dS->shapingTime;
    mTau                 = dS->tau;

#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif

    //Units Integrity
    mNominalGain         *= (millivolt)/(coulomb*1.e-15);  // mV/fC
    mSamplingFrequency   *= MHz;
    mTZero               *= microsecond;
    mAdcConversion       *= (millivolt);
    mAdcConversionCharge *= (coulomb*1.e-15);
    mShapingTime         *= nanosecond;
    mTau                 *= nanosecond;
}

StTpcElectronics*
StTpcROOTElectronics::instance()
{
    if (!mInstance) {
#ifndef ST_NO_EXCEPTIONS
	throw invalid_argument("StTpcROOTElectronics::getInstance(): Argument Missing!");
#else
	cerr << "StTpcROOTElectronics::getInstance(): Argument Missing!" << endl;
	cerr << "No arguments for instantiantion" << endl;
	cerr << "Exiting..." << endl;
#endif
    }
    return mInstance;
}

StTpcElectronics*
StTpcROOTElectronics::instance(electronicsDataSet* dS)
{
    if (!mInstance) {
	mInstance = new StTpcROOTElectronics(dS);
    }
    else {
	cerr << "StTpcROOTElectronics::getInstance()\a\a\a"  << endl;
	cerr << "\tWARNING:" << endl;
	cerr << "\tSingleton class is already instantiated" << endl;
	cerr << "\tArgument  ignored!!" << endl;
	cerr << "\tContinuing..." << endl;
    }
    return mInstance;
}

double StTpcROOTElectronics::channelGain(int sector, int row, int pad) const
{
    // This should be where channel by channel gains are looked up
    return nominalGain();
}

double StTpcROOTElectronics::channelGain(StTpcPadCoordinate& coord) const
{
    return channelGain(coord.sector(), coord.row(), coord.pad());
}

int StTpcROOTElectronics::pedestal(int sector, int row, int pad, int timeB) const
{
    return averagePedestal();
}

int StTpcROOTElectronics::pedestal(StTpcPadCoordinate& coord) const
{
    return pedestal(coord.sector(), coord.row(), coord.pad(), coord.timeBucket());
}

void StTpcROOTElectronics::print(ostream& os) const
{
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif
    os << "ROOT Electronics Data Base Parameters"                         << endl;
    os << "======================================="                         << endl;
    os << "Analog:"                                                         << endl;
    os << "nominalGain:         " << mNominalGain/((volt*.001)/(coulomb*1.e-15))  << " mV/fC"       << endl;
    os << "samplingFrequency:   " << mSamplingFrequency/MHz   << " MHz"         << endl;
    os << "tZero:               " << mTZero/microsecond       << " us"          << endl;
    os << "shapingTime:         " << mShapingTime/nanosecond  << " ns"          << endl;
    os << "shapingTime2:        " << mTau/nanosecond          << " ns"          << endl;

    os << "\nDigital:"                                                      << endl;
    os << "adcConversion:       " << mAdcConversion/(volt*.001) << " mV/channel" << endl;
    os << "adcConversionCharge: " << mAdcConversionCharge/(coulomb*1.e-15)  << " mV/fC"      << endl;    
    os << "numberOfTimeBins:    " << mNumberOfTimeBins                      << endl;    
    os << "averagePedestal:     " << mAveragePedestal      << " channels"   << endl;
    os << endl;
}
#endif
