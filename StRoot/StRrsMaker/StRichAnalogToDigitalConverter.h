/*************************************************************
 * $Id: StRichAnalogToDigitalConverter.h,v 1.1 2000/01/18 21:32:00 lasiuk Exp $
 *
 * Description:
 *   StRichAnalogToDigitalConverter is the function object containing
 *   the algorithm that transforms the simulated analog signal 
 *   to a digital output. 
 *    
 *   StRichAnalogToDigitalConverter is used like a normal function,
 *   i.e. StRichAnalogToDigitalConverter(double);  
 * 
 *   StRichAnalogToDigitalConverter takes an analog signal
 *   and converts it to an ADC count, depending on the
 *   given adc factor, pedestal and adc range.
 *
 ****************************************************************
 * $Log: StRichAnalogToDigitalConverter.h,v $
 * Revision 1.1  2000/01/18 21:32:00  lasiuk
 * Initial Revision
 *
 * Revision 1.1  2000/01/18 21:32:00  lasiuk
 * Initial Revision
 *
 *   revision history:
 *     - 7/22/1999 created the class, Alexandre Nevski.
 *     - 8/24/1999 initial implementation
 *     - 8/31/1999 final ADC counts implementation
 *
 *************************************************************/
#ifndef ST_RICH_ANALOG_TO_DIGITAL_CONVERTER_H
#define ST_RICH_ANALOG_TO_DIGITAL_CONVERTER_H

#include <functional>
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::unary_function;
#endif

#endif
#include "StRichOtherAlgorithms.h"
};
    struct StRichAnalogToDigitalConverter : public unary_function<double,double> {
	
	int operator()(double) const;
	
    };
inline
void StRichAnalogToDigitalConverter::setAddPedestal(int v) {mAddPedestal = v;}

#ifndef ST_NO_NAMESPACES
//} 
#endif

#endif
