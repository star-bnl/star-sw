/**********************************************************
 * $Id: StRichSelectWire.cxx,v 1.2 2000/01/25 22:02:22 lasiuk Exp $
 *
 * Description:
 *
 *  SelectWire computes the position of the
 *  closest wire to a given hit. SelectWire depends
 *  only on the detector geometry. 
 *
 *  In this implementation, 
 *    - a wire number is calculated, between 1 and 
 *    the total number of wires
 *    - then the Rich local position is found and returned
 * 
 *
 ************************************************************
 * $Log: StRichSelectWire.cxx,v $
 * Revision 1.2  2000/01/25 22:02:22  lasiuk
 * Second Revision
 *
 *
 * Revision 1.3  2000/02/08 16:31:54  lasiuk
 * use dbs
 *
 * Revision 1.2  2000/01/25 22:02:22  lasiuk
 *
 *************************************************************/
 *
 * Revision 1.1  2000/01/18 21:32:04  lasiuk
 * Initial Revision
 *********************************************************************/

//namespace StRichRawData {
#endif

#include "StRichSelectWire.h"
#include "StRichSelectWire.h"
#include "StRichGHit.h"
#include "StRichOtherAlgorithms.h"
#include "StRichGeometryDb.h"
    wirePosition StRichSelectWire::operator()( const StRichGHit& hit ) const 
    {
	static StRichGeometryDb* geoDB     = StRichGeometryDb::getDb();  // locals
	static double first_wire_pos = geoDB->wire_x0;
	static double wire_spacing   = geoDB->wire_spacing;
	static MyRound round;
	
	int wire = round(  (first_wire_pos-hit.x) / wire_spacing  ) + 1;
	
	
	// now taking possible errors in account 
	// condition given by fiducial cut
	// +5 is added here because of the possible wires
	// between quadrants. For now we suppose there are 
	// 5 additional wires in the gap, but this line should
	// be rewritten as soon as more information on geometry
	// is availaible.
	// if (wire < 1 || wire > geoDB->number_of_wires+5) do smth;
	
	wirePosition =
	if (StRichViewer::histograms )
	    StRichViewer::getView()->mWhichWire->Fill(wire);
    
	double wire_position =  first_wire_pos - (wire-1) * wire_spacing;
#ifdef RICH_WITH_VIEWER
	return (  wire_position ); 
	
    }
#endif
	
	return (wirePosition); 
}
#ifndef ST_NO_NAMESPACES
//}
#endif
