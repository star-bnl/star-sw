/**************************************************NoiseSimulator.h**\
 * $Id: StRichNoiseSimulator.h,v 1.1 2000/01/18 21:32:03 lasiuk Exp $
 *
 * Description:
 *   StRichNoiseSimulator is the function object containing
 *   the algorithm that add noise to a certain signal.
 * 
 *   StRichNoiseSimulator is used like a normal function,
 *   i.e. StRichNoiseSimulator();  
 * 
 *   StRichNoiseSimulator generates an electric noise
 *   depending on an experimental factor from a 
 *   database.
 *
 *********************************************************************
 * $Log: StRichNoiseSimulator.h,v $
 * Revision 1.1  2000/01/18 21:32:03  lasiuk
 * Initial Revision
 *
 * Revision 1.1  2000/01/18 21:32:03  lasiuk
 * Initial Revision
 *
 *   revision history:
 *     - 8/24/1999 created the class, Alexandre Nevski.
 *     - 8/24/1999 initial implementation, C & A.
 *
 ********************************************************************/

#ifndef ST_RICH_NOISE_SIMULATOR_H
#define ST_RICH_NOISE_SIMULATOR_H

#include <functional>
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::unary_function;
#endif

//namespace StRichRawData {
#endif
#include "StRichRrsMacros.h"
    struct StRichNoiseSimulator : public unary_function<double,double> {
	double operator()(void) const;
    };

#ifndef ST_NO_NAMESPACES
//} 
#endif

#endif
