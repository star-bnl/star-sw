/*****************************************************************
 *
 * $Id: StTpcSimpleElectronics.cc,v 1.2 1999/01/18 10:22:08 lasiuk Exp $
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
 * Revision 1.2  1999/01/18 10:22:08  lasiuk
 * add tau
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

StTpcElectronics* StTpcSimpleElectronics::mInstance = 0; // static data member

StTpcSimpleElectronics::StTpcSimpleElectronics() { /* nopt */ }

StTpcSimpleElectronics::StTpcSimpleElectronics(const char* file)
    StGetConfigValue(file,"nominalGain",mNominalGain);
    StGetConfigValue(file,"samplingFrequency",mSamplingFrequency);
    StGetConfigValue(file,"tZero",mTZero);
    StGetConfigValue(file,"adcConversion",mAdcConversion);    
    StGetConfigValue(file,"adcCharge",mAdcConversionCharge);    
    StGetConfigValue(file,"averagePedestal",mAveragePedestal);
    StGetConfigValue(file,"shapingTime",mShapingTime);
    StGetConfigValue(file,"tau",mTau);

#ifndef ST_NO_NAMESPACES
    using namespace units;
#endif

    mTau                 += nanosecond;
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
	cerr << "StTpcSimpleElectronics::getInstance(): Argument Missing!" << endl;
	cerr << "No arguments for instantiantion" << endl;
	cerr << "Exiting..." << endl;
	exit(1);
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
	cerr << "StTpcSimpleElectronics::getInstance()\a\a\a"  << endl;
	cerr << "\tWARNING:" << endl;
	cerr << "\tSingleton class is already instantiated" << endl;
	cerr << "\tArgument \"" << file << "\" ignored!!" << endl;
	cerr << "\tContinuing..." << endl;
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
    os << "nominalGain:         " << mNominalGain/((volt*.001)/(coulomb*1.e-15))  << " mV/fC"       << endl;
    os << "samplingFrequency:   " << mSamplingFrequency/MHz   << " MHz"         << endl;
    os << "tZero:               " << mTZero/microsecond       << " us"          << endl;
    os << "shapingTime:         " << mShapingTime/nanosecond  << " ns"          << endl;
    os << "shapingTime2:        " << mTau/nanosecond          << " ns"          << endl;

    os << "\nDigital:"                                                      << endl;
    os << "adcConversion:       " << mAdcConversion/(volt*.001) << " mV/channel" << endl;
    os << "adcConversionCharge: " << mAdcConversionCharge/(coulomb*1.e-15)  << " mV/fC"      << endl;    
    os << "averagePedestal:     " << mAveragePedestal      << " channels"   << endl;
    os << endl;
}
