/****************************************************************
 * $Id: StRichInduceSignal.h,v 1.3 2000/02/08 16:26:52 lasiuk Exp $
 *
 * Description:
 *   InduceSigna1 is the module containing the sequence of
 *   algorithms to follow to produce analog signals on pads  from
 *   low energy electrons. 
 *
 *****************************************************************
 * $Log: StRichInduceSignal.h,v $
 * Revision 1.3  2000/02/08 16:26:52  lasiuk
 * rm viewer dependence
 *
 * Revision 1.3  2000/02/08 16:26:52  lasiuk
 * rm viewer dependence
 *
 * Revision 1.2  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
 * Initial Revision
 *
 *   revision history:
 *     - 7/31/1999 created the class,               Alexandre Nevski.
 *     - 8/18/1999 first error handling impl.       Alexandre Nevski.
 *     - 8/24/1999 added new call algorithm to ADC and Noise
 *                                                  C & A
 *     - 9/2/1999 ADC and Noise transferred to Maker. 
 *                                                  C & A
 ********************************************************************/
#ifndef ST_RICH_INDUCE_SIGNAL_H
#define ST_RICH_INDUCE_SIGNAL_H

#include <functional>
#ifndef ST_NO_NAMESPACES
using std::unary_function;
#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
#include "StRichRrsMacros.h"
#include "StRichGHit.h"

struct StRichInduceSignal : public unary_function<StRichGHit,void> {
    void operator()(StRichGHit& );
};
    
#ifndef ST_NO_NAMESPACES    
//}
#endif

#endif // INDUCESIGNAL_H
