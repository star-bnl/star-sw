/*********************************************************************
 * $Id: StRichSelectWire.cxx,v 1.5 2000/03/12 23:56:33 lasiuk Exp $
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
 * Revision 1.5  2000/03/12 23:56:33  lasiuk
 * new coordinate system
 * exchange MyRound with inline templated funtion
 *
 * Revision 1.5  2000/03/12 23:56:33  lasiuk
 * new coordinate system
 * exchange MyRound with inline templated funtion
 *
 * Revision 1.4  2000/02/12 21:55:13  lasiuk
 * take into account gap between top and bottom of
 * chamber
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

wirePosition StRichSelectWire::operator()( const StRichGHit& hit ) const 
{
    static StRichGeometryDb* geoDB     = StRichGeometryDb::getDb();  // locals
    //static double first_wire_pos = geoDB->wire_x0;
    //static double first_wire_pos = geoDB->firstWirePositionInX();
    //static double wire_spacing   = geoDB->wire_spacing;
    static double wire_spacing   = geoDB->wirePitch();

    
    int wireNumber;  // wire Number starts at zero
    double wirePosition;
    
    if(hit.position().y() >= 0) {
	wireNumber =
	    nearestInteger((geoDB->firstWirePositionInY(1)-hit.position().y())/geoDB->wirePitch());
	if(wireNumber<0)
	    wireNumber = 0;
	else if (wireNumber>(geoDB->numberOfWires()/2-1))
	    wireNumber = geoDB->numberOfWires()/2-1;
	wirePosition =
	    geoDB->firstWirePositionInY(1)-(wireNumber)*geoDB->wirePitch();
    }
    else {
	wireNumber =
	    nearestInteger((geoDB->firstWirePositionInY(-1)-hit.position().y())/geoDB->wirePitch())+geoDB->numberOfWires()/2.;
	if(wireNumber>(geoDB->numberOfWires()-1))
	    wireNumber = geoDB->numberOfWires()-1;
	else if(wireNumber<geoDB->numberOfWires()/2)
	    wireNumber = geoDB->numberOfWires()/2;
	wirePosition =
	    geoDB->firstWirePositionInY(-1)-(wireNumber-geoDB->numberOfWires()/2)*geoDB->wirePitch();
    }
    
#ifdef RICH_WITH_VIEWER
    if (StRichViewer::histograms )
	StRichViewer::getView()->mWhichWire->Fill(wireNumber);
#endif
	
	return (wirePosition); 
}
#ifndef ST_NO_NAMESPACES
//}
#endif
