/********************************************************
 * $Id: StRichAnalogToDigitalConverter.cxx,v 1.2 2000/01/25 22:02:19 lasiuk Exp $
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
 * Revision 1.2  2000/01/25 22:02:19  lasiuk
 * Second Revision
 *
 * Revision 1.2  2000/01/25 22:02:19  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:00  lasiuk
 * Initial Revision
 *
 ******************************************************/

#include <algorithm>

#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::min;
using std::max;
#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif  

#include "StRichPhysicsDb.h"
#include "StRichGeometryDb.h"
#include "StRichAnalogToDigitalConverter.h"

StRichAnalogToDigitalConverter::StRichAnalogToDigitalConverter()
    : mAddPedestal(0) { /* nopt */ }

StRichAnalogToDigitalConverter::~StRichAnalogToDigitalConverter() { /* nopt */ }
    
int StRichAnalogToDigitalConverter::operator()(double signal) const
{
    static StRichPhysicsDb*  mPhysicsDb  = StRichPhysicsDb::getDb();
    static MyRound          mRound;
    static int adc_max       = static_cast<int>(pow(2.,mPhysicsDb->channel_width));
    static int pedestal      = mPhysicsDb->pedestal;
    static double adc_factor = mPhysicsDb->adc_factor;           // fC/ADC_channels
    static double e_charge   = mPhysicsDb->e_charge * 1.0e15;    // C->fC conversion
    
    
    // signal is in [electrons], e_charge in [fC] and adc_factor in [fC/channel]
    
    int Q = mRound( signal * e_charge  /  adc_factor );
    PR(signal);
    PR(e_charge);
    PR(adc_factor);
    PR(Q);
    if(mAddPedestal)
	Q += pedestal;
    
    // check underflow or overflow ( saturation )
	
    Q=min(max(0,Q),adc_max);
    
    return Q;
}

#ifndef ST_NO_NAMESPACES
//}
#endif
