/*********************************************************************
 * $Id: StRichInduceSignal.cxx,v 1.3 2000/02/08 16:26:51 lasiuk Exp $
 *
 * Description:
 *  StRichInduceSignal calculates an analog signal on pads.
 *  In this implementation, it first selects the
 *  closest wire to which the electron will drift,
 *  computes an amplification factor from GasGain,
 *  and finally uses a AnalogSignalGenerator to
 *  distribute the total charge to surrounding
 *  pads. 
 *
 *
 *********************************************************************
 * $Log: StRichInduceSignal.cxx,v $
 * Revision 1.3  2000/02/08 16:26:51  lasiuk
 * rm viewer dependence
 *
 * Revision 1.3  2000/02/08 16:26:51  lasiuk
 * rm viewer dependence
 *
 * Revision 1.2  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
 * Initial Revision
 *********************************************************************/

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

#include "StRichInduceSignal.h"
#include "StRichSelectWire.h"
#include "StRichGasGain.h"
#include "StRichAnalogSignalGenerator.h"

void StRichInduceSignal::operator()(StRichGHit& hit)
{
    StRichSelectWire selectWire;
    StRichGasGain amplify;
    StRichAnalogSignalGenerator ASG;
	
    ASG( hit, amplify ( hit, selectWire( hit ) ) );    
	
}

#ifndef ST_NO_NAMESPACES
//}
#endif
