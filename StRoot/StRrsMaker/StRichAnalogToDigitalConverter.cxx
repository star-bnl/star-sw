/********************************************************
 * $Id: StRichAnalogToDigitalConverter.cxx,v 1.3 2000/02/08 16:22:19 lasiuk Exp $
 *
 * Description:
 *  StRichAnalogToDigitalConverter takes an analog signal
 *  and converts it to an ADC count, depending on the
 *  given adc factor, pedestal and adc range. 
 *
 *  In this implementation, 
 *    all the computations are done in femtocoulombs. 
 *
 ******************************************************
 * $Log: StRichAnalogToDigitalConverter.cxx,v $
 * Revision 1.3  2000/02/08 16:22:19  lasiuk
 * use dbs
 * systemOfUnits now used
 *
 * Revision 1.2  2000/01/25 22:02:19  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:00  lasiuk
 * Initial Revision
 *
 ******************************************************/

#include <algorithm>

#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using std::min;
using std::max;
using namespace units;
#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif  

#include "StRichPhysicsDb.h"
#include "StRichGeometryDb.h"
#include "StRichAnalogToDigitalConverter.h"

StRichAnalogToDigitalConverter::StRichAnalogToDigitalConverter()
    : mAddPedestal(0)
{
    mPhysicsDb      = StRichPhysicsDb::getDb();
    //mMaxADC     = static_cast<int>(pow(2.,mPhysicsDb->channel_width));
    mMaxADC         = static_cast<int>(pow(2.,mPhysicsDb->adcChannelWidth()));
    mPedestal       = mPhysicsDb->averagePedestal();
    mAdcConversion  = mPhysicsDb->adcConversion();// fC/ADC_channels

    //mElectronCharge = e_SI*1.0e15;// #electrons -> fC conversion
}

StRichAnalogToDigitalConverter::~StRichAnalogToDigitalConverter() { /* nopt */ }
    
int StRichAnalogToDigitalConverter::operator()(double signal) const
{
    //    
    // signal is in [electrons], e_charge in [fC] and adc_factor in [fC/channel]

    int Q = mRound( signal  /  mAdcConversion );
    if(mAddPedestal)
	Q += mPedestal;
    
    // check underflow or overflow ( saturation )
	
    Q=min(max(0,Q),mMaxADC);
    
    return Q;
}

#ifndef ST_NO_NAMESPACES
//}
#endif
