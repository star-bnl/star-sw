/************************************************************************
 * $Id: StRichFilter.cxx,v 1.3 2000/02/08 16:22:56 lasiuk Exp $
 *
 * Description:
 *  StRichFilter decides which algorithms to apply depending on the kind
 *  of received particle.
 *  In this implementation, StRichFilter examines which particle will 
 *  induce a signal, such as the charged particle in CH4 (RGAP) or 
 *  the Cerenkov photon in cesium iodide (RCSI). The former produces
 *  a serie of ionizations, the latter kicks out a low energy 
 *  electron by photoeffect. The charged particle in RCSI is rejected.
 *  Note: Cerenkov photons have negative energy loss by 
 *  convention (GEANT), while charged particles do not.
 *
 *
 *  ChangeCoord transforms  RGAP coordinates (4 quadrants) in  
 *  RCSI coordinates (per each quadrant)
 *
 *
 *  whatQuadrant assigns a quadrant number to any particle
 *  in RGAP local coordinates system. This is useful
 *  to allow generic algorithms to be reused for
 *  both photons and charged particles. 
 *
 *
 *****************************************************************************
 * $Log: StRichFilter.cxx,v $
 * Revision 1.3  2000/02/08 16:22:56  lasiuk
 * move selection into maker.  Remove from package next revision
 *
 * Revision 1.3  2000/02/08 16:22:56  lasiuk
 * move selection into maker.  Remove from package next revision
 *
 * Revision 1.2  2000/01/25 22:02:19  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:00  lasiuk
 * Initial Revision
 *
 *****************************************************************************/
#ifdef NEVER
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
#include "StRichFilter.h"
#include "StRichIonization.h"
#include "StRichInduceSignal.h"

#ifdef RICH_WITH_VIEWER
#include "StRichViewer.h"
#endif

void StRichFilter::operator()(StRichGHit& hit)
{
    StRichIonization   ionize;
    StRichInduceSignal induceSignal;

    //
    // If in the GAP, ionize the gas
    PR(hit.volumeID());
    if ( hit.volumeID() != "RCSI" ) { 
	ionize( hit );		 
    }
    else {
	//
	// check if it is photon
	if ( hit.dE() > 0 ) 
	    induceSignal ( hit );
    }
    
#ifdef RICH_WITH_VIEWER
    if (StRichViewer::histograms ) 
	StRichViewer::getView()->mPadPlane->Fill( hit.position().z(), hit.position().x(), 1);
#endif
}

#ifndef ST_NO_NAMESPACES
//}
#endif
#endif
