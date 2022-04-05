/*****************************************************************
 *
 * $Id: StTpcSimpleElectronics.cc,v 1.9 2012/06/11 15:04:56 fisyak Exp $
 *
 * Author: brian Nov 3, 1998
 *
 *****************************************************************
 * Description: Simple ASCII Database for Electronics parameters for
 *              the STAR Main TPC              
 *
 *****************************************************************
 *
 * $Log: StTpcSimpleElectronics.cc,v $
 * Revision 1.9  2012/06/11 15:04:56  fisyak
 * std namespace
 *
 * Revision 1.8  2000/06/07 02:03:11  lasiuk
 * exit/abort ultimatum
 *
 * Revision 1.7  2000/03/15 17:39:49  calderon
 * Remove beeps
 *
 * Revision 1.6  2000/01/10 23:14:30  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.5  1999/02/28 20:17:40  lasiuk
 * add numberOfTimeBins
 *
 * Revision 1.4  1999/02/24 19:33:29  lasiuk
 * add tzero offset parameter
 *
 * Revision 1.3  1999/01/18 17:21:29  lasiuk
 * tau units
 *
 * Revision 1.2  1999/01/18 10:22:08  lasiuk
 * add tau
 *
 * Revision 1.1  1998/11/10 17:12:22  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.3  1998/11/05 19:03:50  lasiuk
 * fix unit integrity to make sure DB call returns in base unit
 *
 * Revision 1.2  1998/11/04 21:26:11  lasiuk
 * incorporate units
 *
 * Revision 1.1  1998/11/04 18:52:33  lasiuk
 * Initial Revision
 *
 ******************************************************************/

#include "SystemOfUnits.h"
#include "StTpcSimpleElectronics.hh"
#include "StGetConfigValue.hh"
#ifndef ST_NO_EXCEPTIONS
#   include <stdexcept>
#   if !defined(ST_NO_NAMESPACES)
        using std::invalid_argument;
#   endif
#endif

StTpcElectronics* StTpcSimpleElectronics::mInstance = 0; // static data member

StTpcSimpleElectronics::StTpcSimpleElectronics() { /* nopt */ }

StTpcSimpleElectronics::StTpcSimpleElectronics(const char* file)
{
    StGetConfigValue(file,"nominalGain",mNominalGain);
    StGetConfigValue(file,"samplingFrequency",mSamplingFrequency);
    StGetConfigValue(file,"tZero",mTZero);
    StGetConfigValue(file,"adcConversion",mAdcConversion);    
    StGetConfigValue(file,"adcCharge",mAdcConversionCharge);    
    StGetConfigValue(file,"numberOfTimeBins",mNumberOfTimeBins);    
    StGetConfigValue(file,"averagePedestal",mAveragePedestal);
    StGetConfigValue(file,"shapingTime",mShapingTime);
    StGetConfigValue(file,"tau",mTau);

#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif

    //Units Integrity
    mNominalGain         *= ((volt*.001)/(coulomb*1.e-15));  // mV/fC
    mSamplingFrequency   *= MHz;
    mTZero               *= microsecond;
    mAdcConversion       *= (volt*.001);
    mAdcConversionCharge *= (coulomb*1.e-15);
    mShapingTime         *= nanosecond;
    mTau                 *= nanosecond;
}

StTpcElectronics*
StTpcSimpleElectronics::instance()
{
    if (!mInstance) {
#ifndef ST_NO_EXCEPTIONS
	throw invalid_argument("StTpcSimpleElectronics::getInstance(): Argument Missing!");
#else
	std::cerr << "StTpcSimpleElectronics::getInstance(): Argument Missing!" << endl;
	std::cerr << "No arguments for instantiantion" << endl;
	std::cerr << "Aborting..." << endl;
	abort();
#endif
    }
    return mInstance;
}

StTpcElectronics*
StTpcSimpleElectronics::instance(const char* file)
{
    if (!mInstance) {
	mInstance = new StTpcSimpleElectronics(file);
    }
    else {
	std::cerr << "StTpcSimpleElectronics::instance()"  << endl;
	std::cerr << "\tWARNING:" << endl;
	std::cerr << "\tSingleton class is already instantiated" << endl;
	std::cerr << "\tArgument \"" << file << "\" ignored!!" << endl;
	std::cerr << "\tContinuing..." << endl;
    }
    return mInstance;
}

double StTpcSimpleElectronics::channelGain(int sector, int row, int pad) const
{
    // This should be where channel by channel gains are looked up
    return nominalGain();
}

double StTpcSimpleElectronics::channelGain(StTpcPadCoordinate& coord) const
{
    return channelGain(coord.sector(), coord.row(), coord.pad());
}

int StTpcSimpleElectronics::pedestal(int sector, int row, int pad, int timeB) const
{
    return averagePedestal();
}

int StTpcSimpleElectronics::pedestal(StTpcPadCoordinate& coord) const
{
    return pedestal(coord.sector(), coord.row(), coord.pad(), coord.timeBucket());
}

void StTpcSimpleElectronics::print(ostream& os) const
{
#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif
    os << "Simple Electronics Data Base Parameters"                         << endl;
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
