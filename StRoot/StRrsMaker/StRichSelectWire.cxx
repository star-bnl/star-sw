/*********************************************************************
 * $Id: StRichSelectWire.cxx,v 1.3 2000/02/08 16:31:54 lasiuk Exp $
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
 *********************************************************************
 * $Log: StRichSelectWire.cxx,v $
 * Revision 1.3  2000/02/08 16:31:54  lasiuk
 * use dbs
 *
 *
 * Revision 1.3  2000/02/08 16:31:54  lasiuk
 * use dbs
 *
 * Revision 1.2  2000/01/25 22:02:22  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:04  lasiuk
 * Initial Revision
 *********************************************************************/

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

#include "StRichSelectWire.h"
#include "StRichGHit.h"
#include "StRichOtherAlgorithms.h"
#include "StRichGeometryDb.h"
#ifdef RICH_WITH_VIEWER
#include "StRichViewer.h"
#endif

    static double first_wire_pos = geoDB->firstWirePositionInX();
{
    static StRichGeometryDb* geoDB     = StRichGeometryDb::getDb();  // locals
    static MyRound round;
	
    int wire = round(  (first_wire_pos-hit.position().x()) / wire_spacing  ) + 1;
    //PR(wire);
	
    // now taking possible errors in account 
    // condition given by fiducial cut
    // +5 is added here because of the possible wires
    // between quadrants. For now we suppose there are 
    // 5 additional wires in the gap, but this line should
    // be rewritten as soon as more information on geometry
    // is availaible.
    // if (wire < 1 || wire > geoDB->number_of_wires+5) do smth;
	    wireNumber = geoDB->numberOfWires()/2;
	wirePosition =
	    geoDB->firstWirePositionInY(-1)-(wireNumber-geoDB->numberOfWires()/2)*geoDB->wirePitch();
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
