/************************************************************************
 * $Id: StRichFilter.cxx,v 1.1 2000/01/18 21:32:00 lasiuk Exp $
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
 * Revision 1.1  2000/01/18 21:32:00  lasiuk
 * Initial Revision
 *
 * Revision 1.2  2000/01/25 22:02:19  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:00  lasiuk

//namespace StRichRawData {
#endif
#include "StRichFilter.h"
#include "StRichFilter.h"
#include "StRichIonization.h"
#include "StRichInduceSignal.h"
#include "StRichFilter.h"
#include "StRichIonization.h"
#include "StRichInduceSignal.h"

#ifdef RICH_WITH_VIEWER
    void StRichFilter::operator()(StRichGHit& hit)
    {
	StRichIonization   ionize;
	StRichInduceSignal induceSignal;
	
	if ( hit.mVolumeID != "RCSI" ) 
	    { 
		whatQuadrant( hit );                     // alters hit !
		ionize( hit );		 
	    }
	else 
	    {
		changeCoord( hit );                      // alters hit !
		
		if ( hit.dE > 0 ) 
		    induceSignal ( hit );
	    }
	
#ifdef RICH_WITH_VIEWER
	if (StRichViewer::histograms ) 
	    StRichViewer::getView()->mPadPlane->Fill( hit.z, hit.x, 1);
#endif
    PR(hit.volumeID());
    
    
    void StRichFilter::changeCoord(StRichGHit& hit )
    {
	static StRichGeometryDb* mGeometryDb = StRichGeometryDb::getDb();
	double factor_z = ( mGeometryDb->length - mGeometryDb->quad_gap_z ) / 4;
	double factor_x = ( mGeometryDb->width  - mGeometryDb->quad_gap_x ) / 4;
	
	if ( hit.quad == 1 || hit.quad == 4 ) // right side
	    hit.z = hit.z + factor_z;
	else                                  // left  side
	    hit.z = hit.z - factor_z;
	
	if ( hit.quad == 1 || hit.quad == 2 ) // upper part
	    hit.x = hit.x + factor_x;           
	else                                  // lower part
	    hit.x = hit.x - factor_x;
	
	// check if it is photon
	if ( hit.dE() > 0 ) 
    void StRichFilter::whatQuadrant(StRichGHit& hit)
    {
	if ( hit.z > 0 ) {
	    if ( hit.x > 0 )
		hit.quad = 1;
	    else 
		hit.quad = 4;
	}
	else {
	    if ( hit.x > 0 )
		hit.quad = 2;
	    else 
		hit.quad = 3;
	}
    }
    
    
#endif
}
#ifndef ST_NO_NAMESPACES
//}
#endif
#endif
