/*********************************************************************
 * $Id: StRichSelectWire.cxx,v 1.6 2000/03/17 14:55:06 lasiuk Exp $
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
 * Revision 1.6  2000/03/17 14:55:06  lasiuk
 * Large scale revisions after ROOT dependent memory leak
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

#include "StRichSelectWire.h"
#include "StRichOtherAlgorithms.h"
#include "StRichGeometryDb.h"

#ifdef RICH_WITH_VIEWER
#include "StRichViewer.h"
#endif

StRichSelectWire::StRichSelectWire()
{
    StRichGeometryDb* tmpGeometryDb = StRichGeometryDb::getDb();

    mWirePitch = tmpGeometryDb->wirePitch();
    mFirstWirePositionInYTop = tmpGeometryDb->firstWirePositionInY(1);
    mFirstWirePositionInYBottom = tmpGeometryDb->firstWirePositionInY(-1);
    mNumberOfWires = tmpGeometryDb->numberOfWires();
}
StRichSelectWire::~StRichSelectWire() {/* notp */}

double StRichSelectWire::whichWire(const StRichMiniHit* hit) const 
{
    int wireNumber;  // wire Number starts at zero
    double wirePosition;
    
    if(hit->position().y() >= 0) {
	wireNumber =
	    nearestInteger((mFirstWirePositionInYTop - hit->position().y())/mWirePitch);
	if(wireNumber<0)
	    wireNumber = 0;
	else if (wireNumber>(mNumberOfWires/2-1))
	    wireNumber = mNumberOfWires/2-1;
	wirePosition =
	    mFirstWirePositionInYTop-(wireNumber)*mWirePitch;
    }
    else {
	wireNumber =
	    nearestInteger(( mFirstWirePositionInYBottom - hit->position().y())/mWirePitch)+mNumberOfWires/2.;
	if(wireNumber>(mNumberOfWires-1))
	    wireNumber = mNumberOfWires-1;
	else if(wireNumber<mNumberOfWires/2)
	    wireNumber = mNumberOfWires/2;
	wirePosition =
	     mFirstWirePositionInYBottom - (wireNumber-mNumberOfWires/2)*mWirePitch;
    }
    
#ifdef RICH_WITH_VIEWER
    if (StRichViewer::histograms )
	StRichViewer::getView()->mWhichWire->Fill(wireNumber);
#endif
	
    return (wirePosition); 
}
