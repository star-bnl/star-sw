/*******************************************************
 * $Id: StRichInduceSignal.cxx,v 1.1 2000/01/18 21:32:02 lasiuk Exp $
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
 *******************************************************
 * $Log: StRichInduceSignal.cxx,v $
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
 * Initial Revision
 *
 * Revision 1.2  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
 *******************************************************/
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
 * Initial Revision
#ifdef RICH_WITH_VIEWER
#include "StRichViewer.h"
#endif
 *********************************************************************/

#ifndef ST_NO_NAMESPACES
#endif
#ifdef RICH_WITH_VIEWER
#include "StRichViewer.h"
#endif

#include "StRichSelectWire.h"
    void StRichInduceSignal::operator()(StRichGHit& hit)
    {
	StRichSelectWire selectWire;
	StRichGasGain amplify;
	StRichAnalogSignalGenerator ASG;
    StRichSelectWire selectWire;
#ifdef RICH_WITH_VIEWER
	if (StRichViewer::histograms ) {
	    StRichViewer::getView()->mWhichQuadrant->Fill( hit.quad );
	    StRichViewer::getView()->mParticleId->Fill( hit.id );
	}
#endif
	ASG( hit, amplify ( hit, selectWire( hit ) ) );    
    StRichAnalogSignalGenerator ASG;
    }
    ASG( hit, amplify ( hit, selectWire( hit ) ) );    
	
}

#ifndef ST_NO_NAMESPACES
//}
#endif
