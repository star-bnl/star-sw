/*******************************************************************
 * $Id: StRichAnalogSignalGenerator.cxx,v 1.7 2000/03/17 14:54:12 lasiuk Exp $
 *
 * Description:
 *  StRichAnalogSignalGenerator generates signals on pads
 *  by calculating the nearest pad (row and col) and
 *  distributing a given charge on the area hit.
 *
 *  In this implementation, first the nearest pad is computed,
 *  as well as 2 size factors (functions of the current 
 *  quadrant). Once the pad's position is checked to be within
 *  limits (cf. horizon effect, main documentation), 
 *  the charge can be distributed on the neighboring pads, in
 *  this case, a 3x3 area. The charge can now be induced with the
 *  InduceTension function, that takes the distance(x,z) of the pad 
 *  from the charge as its arguments. It is, in fact, computed as if 
 *  the center of the charge was on the bottom left corner of the pad,
 *  spanning to infinity, but only the rectangle on top of
 *  the pad is counted, hence the addition and substraction of
 *  4 "quadrants", cf. quads.gif. Eventually, the analog signal is
 *  recorded to the Writer, that stores signals. The variable sum 
 *  records the total distributed signal and exists mainly for
 *  histogramming purposes. 
 *
 *
 *  InduceTension computes the charge by calculating 
 *  an integral representing the charge on a limited 
 *  plane. The integration, approximated by a serie 
 *  is based on the 'Impact reconstruction in Pad Chambers,
 *  I. Simulation studies' by M.Benayoun, Ph.Leruste, J.L.Narjoux, 
 *  M.Sene, R.Sene, A.Volte, College de France, 1993.   
 * 
 *************************************************************************
 * $Log: StRichAnalogSignalGenerator.cxx,v $
 * Revision 1.7  2000/03/17 14:54:12  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.7  2000/03/17 14:54:12  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.6  2000/03/12 23:56:33  lasiuk
 * new coordinate system
 * exchange MyRound with inline templated funtion
 *
 * Revision 1.5  2000/02/14 01:09:46  lasiuk
 * add track_p to GHit c'tor
 *
 * Revision 1.4  2000/02/08 23:51:13  lasiuk
 * removal of rrs macro---CC4.2 cannot handle it!
 *
 * Revision 1.3  2000/02/08 16:21:41  lasiuk
 * use coordinate transformation routines for pad limits
 * incorporation of dbs
 *
 * Revision 1.2  2000/01/25 22:02:19  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:31:59  lasiuk
 * Initial Revision
 *
 **************************************************************************/

#include <math.h>                         
#include <algorithm>

#ifndef ST_NO_NAMESPACES
using std::min;
using std::max;
#endif

#include "StRichAnalogSignalGenerator.h"
#include "StRichGeometryDb.h"
#include "StRichCoordinateTransform.h"
#include "StRichWriter.h"
#include "StRichOtherAlgorithms.h"

#ifdef RICH_WITH_VIEWER
#include "StRichViewer.h"
#endif

StRichAnalogSignalGenerator* StRichAnalogSignalGenerator::mInstance = 0;

StRichAnalogSignalGenerator*
StRichAnalogSignalGenerator::getInstance(StRichWriter* aWriter)
{
    if(!mInstance)
	mInstance = new StRichAnalogSignalGenerator(aWriter);
    return mInstance;
}

StRichAnalogSignalGenerator::StRichAnalogSignalGenerator() {/* never called*/}

StRichAnalogSignalGenerator::StRichAnalogSignalGenerator(StRichWriter* theWriter)
{
    mGeomDb    = StRichGeometryDb::getDb();
    mTransform = StRichCoordinateTransform::getTransform(mGeomDb);
    mOutput    = theWriter;
    
    mNumberOfPadsInRowQ    = mGeomDb->numberOfPadsInAQuadrantRow(); //n_pad_x
    mNumberOfRowsInColumnQ = mGeomDb->numberOfRowsInAQuadrantColumn(); //n_pad_y;
    mPadLength             = mGeomDb->padLength(); //pad_side_y;
    mPadWidth              = mGeomDb->padWidth(); //pad_side_x;
    mAnodePadPlaneSpacing  = mGeomDb->anodeToPadSpacing(); //height;
}

StRichAnalogSignalGenerator::~StRichAnalogSignalGenerator()
{
    delete mInstance;
}

void StRichAnalogSignalGenerator::induceSignal(const StRichMiniHit* hit, double q )
{
    if(RRS_DEBUG)
	cout << "StRichAnalogSignalGenerator::induceSignal() --> q= " << q << endl;
    double sum = 0;
    double x, y, q00, q10, q01, q11,s;

    //
    // find which pad ( row and col )

    StRichRawCoordinate raw;
    StRichLocalCoordinate local(hit->position());

    (*mTransform)(local, raw);

    //
    // horizon effect - limits on rows and cols
    //
    pair<int, int> rowLimits = calculateRowLimits(raw);
    pair<int, int> padLimits = calculatePadLimits(raw);

    if(RRS_DEBUG) {
	cout << "Calculate Limits for (local): " << local << endl;
	cout << "Calculate Limits for (raw): " << raw << endl;
	cout << "rowLimits: ("
	     << rowLimits.first << ", " << rowLimits.second << ')' << endl;
	cout << "padLimits: ("
	     << padLimits.first << ", " << padLimits.second << ')' << endl;
    }
    int i, j;
    StRichRawCoordinate tmpRaw;
    StRichLocalCoordinate tmpLoc;
    for (i=rowLimits.first; i<=rowLimits.second; i++) {
	tmpRaw.setRow(i);
	for (j=padLimits.first; j<=padLimits.second; j++) {
	    tmpRaw.setPad(j);

	    (*mTransform)(tmpRaw,tmpLoc);
	    x = tmpLoc.position().x();
	    y = tmpLoc.position().y();
	    
	    q00 = induceTension (( (hit->position().y()-y) - mPadLength/2)/mAnodePadPlaneSpacing,   
				 ( (hit->position().x()-x) - mPadWidth/2 )/mAnodePadPlaneSpacing);    
	    q10 = induceTension (( (hit->position().y()-y) + mPadLength/2)/mAnodePadPlaneSpacing,   
				 ( (hit->position().x()-x) - mPadWidth/2 )/mAnodePadPlaneSpacing);    
	    q01 = induceTension (( (hit->position().y()-y) - mPadLength/2)/mAnodePadPlaneSpacing,     
				 ( (hit->position().x()-x) + mPadWidth/2 )/mAnodePadPlaneSpacing);     
	    q11 = induceTension (( (hit->position().y()-y) + mPadLength/2)/mAnodePadPlaneSpacing,    
				 ( (hit->position().x()-x) + mPadWidth/2 )/mAnodePadPlaneSpacing);   

	    s =  q * (q00-q10-q01+q11);
	    sum += s;
	    if(RRS_DEBUG)
		cout << "s/sum " << s << '/' << sum << endl;
	    
	    // save signal on pad
	    mOutput->putSignal(i,j,s,hit->id(),hit->trackp());
#ifdef RICH_WITH_VIEWER
	    if (StRichViewer::histograms ) 
		StRichViewer::getView()->mAnalogSignals->Fill(x,y,s);
	    cout << "fill ASG" << endl;
	    // histograms
#endif
	} 
    }
#ifdef RICH_WITH_VIEWER
    if (StRichViewer::histograms )
	StRichViewer::getView()->mTotalCharge->Fill(sum/q);
#endif
}

double StRichAnalogSignalGenerator::induceTension(double ratioy, double ratiox) const 
{
    static const double g = 0.9159655942;   // Catalan constant
    double qsum, c1, c2, arctan;
    
    c1 = ratioy*ratiox;
    c2 = ratioy*ratioy + ratiox*ratiox;
    
    int s = 1;
    qsum = 0;
    
    for (int i=1; i<=29 ; i+=2) {
	arctan = atan(c1/(i*sqrt(i*i + c2)));
	qsum += s*(arctan-c1/(i*i));
	s = -s;
    }
    return ((g*c1 + qsum)/(2*M_PI));
}

pair<int, int>
StRichAnalogSignalGenerator::calculatePadLimits(const StRichRawCoordinate& raw) const
{
    int deltaPad = 1;
    int tmpPad = nearestInteger(raw.pad());

    pair<int,int> limits;
    if(tmpPad<=deltaPad) {//0-deltaPad
	limits.first  = 0;
	limits.second = tmpPad+deltaPad;
    }
    else if (tmpPad >= ( (mNumberOfPadsInRowQ-1)-(deltaPad)) &&
	     tmpPad <= ( (mNumberOfPadsInRowQ-1) )         ) {//79-deltaPad
	limits.first = tmpPad-deltaPad;
	limits.second = mNumberOfPadsInRowQ-1;
    }
    else if (tmpPad >= ( (mNumberOfPadsInRowQ         )) &&
	     tmpPad <= ( (mNumberOfPadsInRowQ+deltaPad)) ) {//80+deltaPad
	limits.first = mNumberOfPadsInRowQ;
	limits.second = tmpPad+deltaPad;
    }
    else if (tmpPad >= (2*mNumberOfPadsInRowQ - 1)) {//159
	limits.first  = (2*mNumberOfPadsInRowQ - 1) - deltaPad;
	limits.second = (2*mNumberOfPadsInRowQ - 1);
    }
    else {
	limits.first  = tmpPad - deltaPad;
	limits.second = tmpPad + deltaPad;
    }
    
    return limits;
}

pair<int, int>
StRichAnalogSignalGenerator::calculateRowLimits(const StRichRawCoordinate& raw) const
{ 
    int deltaRow = 1;
    int tmpRow = nearestInteger(raw.row());

    pair<int,int> limits;
    if(tmpRow <= deltaRow) {//0-deltaRow
	limits.first  = 0;
	limits.second = tmpRow+deltaRow;
    }
    else if (tmpRow >= ( (mNumberOfRowsInColumnQ-1)-(deltaRow)) &&
	     tmpRow <= ( ((mNumberOfRowsInColumnQ-1) )        )) {//47-deltaRow
	limits.first = tmpRow-deltaRow;
	limits.second = (mNumberOfRowsInColumnQ)-1;
    }
    else if (tmpRow >= ( (mNumberOfRowsInColumnQ         ) ) &&
	     tmpRow <= ( (mNumberOfRowsInColumnQ+deltaRow))) {//48+deltaRow
	limits.first  = mNumberOfRowsInColumnQ;
	limits.second = tmpRow+deltaRow;
    }
    else if (tmpRow >= (2*mNumberOfRowsInColumnQ - 1) ) {//95
	limits.first  = (2*mNumberOfRowsInColumnQ - 1) - deltaRow;
	limits.second = (2*mNumberOfRowsInColumnQ - 1);
    }
    else {
	limits.first  = tmpRow - deltaRow;
	limits.second = tmpRow + deltaRow;
    }
    
    return limits;
}

#ifndef ST_NO_NAMESPACES
//}
#endif
