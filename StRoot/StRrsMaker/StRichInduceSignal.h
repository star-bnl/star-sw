/****************************************************************
 * $Id: StRichInduceSignal.h,v 1.1 2000/01/18 21:32:02 lasiuk Exp $
 *
 * Description:
 *   InduceSigna1 is the module containing the sequence of
 *   algorithms to follow to produce analog signals on pads  from
 *   low energy electrons. 
 *
 *****************************************************************
 * $Log: StRichInduceSignal.h,v $
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
 * Initial Revision
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
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500

#include <functional>
#ifndef ST_NO_NAMESPACES

#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
    struct StRichInduceSignal : public unary_function<StRichGHit,void> {
	void operator()(StRichGHit& );
    };
struct StRichInduceSignal : public unary_function<StRichGHit,void> {
    void operator()(StRichGHit& );
};
    
#ifndef ST_NO_NAMESPACES    
//}
#endif

#endif // INDUCESIGNAL_H
