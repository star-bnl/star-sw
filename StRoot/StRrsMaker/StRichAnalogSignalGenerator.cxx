/*******************************************************************
 * $Id: StRichAnalogSignalGenerator.cxx,v 1.3 2000/02/08 16:21:41 lasiuk Exp $
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
 * Revision 1.3  2000/02/08 16:21:41  lasiuk
 * use coordinate transformation routines for pad limits
 * incorporation of dbs
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
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData { 
#endif
#include "StRichCoordinateTransform.h"
#include "StRichCoordinateTransform.h"
#include "StRichWriter.h"
#include "StRichOtherAlgorithms.h"

    mGeomDb  = StRichGeometryDb::getDb(); 
    mOutput  = StRichWriter::getInstance();

    mNumberOfPadsInRowQ    = mGeomDb->numberOfPadsInAQuadrantRow(); //n_pad_z
    mNumberOfRowsInColumnQ = mGeomDb->numberOfRowsInAQuadrantColumn(); //n_pad_x;
    mPadLength             = mGeomDb->padLength(); //pad_side_x;
    mPadWidth              = mGeomDb->padWidth(); //pad_side_z;
    mTransform = StRichCoordinateTransform::getTransform(mGeomDb);
    mOutput    = theWriter;
    
    mNumberOfPadsInRowQ    = mGeomDb->numberOfPadsInAQuadrantRow(); //n_pad_x
    mNumberOfRowsInColumnQ = mGeomDb->numberOfRowsInAQuadrantColumn(); //n_pad_y;
    mPadLength             = mGeomDb->padLength(); //pad_side_y;
    mPadWidth              = mGeomDb->padWidth(); //pad_side_x;
    rrs << "StRichAnalogSignalGenerator::operator() --> q= " << q << endl;
void StRichAnalogSignalGenerator::operator()( const StRichGHit& hit, double q ) const
    double x, z, q00, q10, q01, q11,s;

	cout << "StRichAnalogSignalGenerator::operator() --> q= " << q << endl;
{
    if(RRS_DEBUG)
    StRichCoordinateTransform transform(mGeomDb);
    
	cout << "StRichAnalogSignalGenerator::induceSignal() --> q= " << q << endl;
    double sum = 0;
    double x, y, q00, q10, q01, q11,s;
    transform(local, raw);
    //
    StRichLocalCoordinate local(hit.position());

    StRichRawCoordinate raw;
    StRichLocalCoordinate local(hit->position());

    (*mTransform)(local, raw);
    rrs << "rowLimits: (" << rowLimits.first << ", " << rowLimits.second << ')' << endl;
    rrs << "padLimits: (" << padLimits.first << ", " << padLimits.second << ')' << endl;

	cout << "Calculate Limits for (local): " << local << endl;
	cout << "Calculate Limits for (raw): " << raw << endl;
	cout << "rowLimits: ("
	     << rowLimits.first << ", " << rowLimits.second << ')' << endl;
	cout << "padLimits: ("
	     << padLimits.first << ", " << padLimits.second << ')' << endl;
    }
    int i, j;
	    transform(tmpRaw,tmpLoc);
    StRichLocalCoordinate tmpLoc;
	    z = tmpLoc.position().z();
	
	    q00 = induceTension (( (hit.position().x()-x) - mPadLength/2)/mAnodePadPlaneSpacing,   
				 ( (hit.position().z()-z) - mPadWidth/2 )/mAnodePadPlaneSpacing);    
	    q10 = induceTension (( (hit.position().x()-x) + mPadLength/2)/mAnodePadPlaneSpacing,   
				 ( (hit.position().z()-z) - mPadWidth/2 )/mAnodePadPlaneSpacing);    
	    q01 = induceTension (( (hit.position().x()-x) - mPadLength/2)/mAnodePadPlaneSpacing,     
				 ( (hit.position().z()-z) + mPadWidth/2 )/mAnodePadPlaneSpacing);     
	    q11 = induceTension (( (hit.position().x()-x) + mPadLength/2)/mAnodePadPlaneSpacing,    
				 ( (hit.position().z()-z) + mPadWidth/2 )/mAnodePadPlaneSpacing);   
	    q01 = induceTension (( (hit.position().y()-y) - mPadLength/2)/mAnodePadPlaneSpacing,     
				 ( (hit.position().x()-x) + mPadWidth/2 )/mAnodePadPlaneSpacing);     
	    q11 = induceTension (( (hit.position().y()-y) + mPadLength/2)/mAnodePadPlaneSpacing,    
	    rrs << "s/sum " << s << '/' << sum << endl;
	    q11 = induceTension (( (hit->position().y()-y) + mPadLength/2)/mAnodePadPlaneSpacing,    
				 ( (hit->position().x()-x) + mPadWidth/2 )/mAnodePadPlaneSpacing);   
	    mOutput->putSignal(i,j,s,hit.id());
	    s =  q * (q00-q10-q01+q11);
	    sum += s;
		StRichViewer::getView()->mAnalogSignals->Fill(z,x,s);        // histograms
	    // save signal on pad
	    mOutput->putSignal(i,j,s,hit->id(),hit->trackp());
#ifdef RICH_WITH_VIEWER
	    if (StRichViewer::histograms ) 
		StRichViewer::getView()->mAnalogSignals->Fill(x,y,s);
	    cout << "fill ASG" << endl;
	    // histograms
#endif
	} 
double StRichAnalogSignalGenerator::induceTension(double ratiox, double ratioz) const 
#ifdef RICH_WITH_VIEWER
    if (StRichViewer::histograms )
	StRichViewer::getView()->mTotalCharge->Fill(sum/q);
#endif
    c1 = ratiox*ratioz;
    c2 = ratiox*ratiox + ratioz*ratioz;
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

    if(raw.pad()<=deltaPad) {//0-deltaPad
StRichAnalogSignalGenerator::calculatePadLimits(const StRichRawCoordinate& raw) const
	limits.second = raw.pad()+deltaPad;
    int deltaPad = 1;
    else if (raw.pad() >= ( (mNumberOfPadsInRowQ-1)-(deltaPad)) &&
	     raw.pad() <= ( (mNumberOfPadsInRowQ-1) )         ) {//79-deltaPad
	limits.first = raw.pad()-deltaPad;
    if(tmpPad<=deltaPad) {//0-deltaPad
	limits.first  = 0;
    else if (raw.pad() >= ( (mNumberOfPadsInRowQ         )) &&
	     raw.pad() <= ( (mNumberOfPadsInRowQ+deltaPad)) ) {//80+deltaPad
    else if (tmpPad >= ( (mNumberOfPadsInRowQ-1)-(deltaPad)) &&
	limits.second = raw.pad()+deltaPad;
	limits.first = tmpPad-deltaPad;
    else if (raw.pad() >= (2.*mNumberOfPadsInRowQ - 1)) {//159
	limits.first  = (2.*mNumberOfPadsInRowQ - 1) - deltaPad;
	limits.second = (2.*mNumberOfPadsInRowQ - 1);
	     tmpPad <= ( (mNumberOfPadsInRowQ+deltaPad)) ) {//80+deltaPad
	limits.first = mNumberOfPadsInRowQ;
	limits.first  = raw.pad() - deltaPad;
	limits.second = raw.pad() + deltaPad;
    else if (tmpPad >= (2*mNumberOfPadsInRowQ - 1)) {//159
	limits.first  = (2*mNumberOfPadsInRowQ - 1) - deltaPad;
	limits.second = (2*mNumberOfPadsInRowQ - 1);
    }
    else {
	limits.first  = tmpPad - deltaPad;
	limits.second = tmpPad + deltaPad;
    }
    

    if(raw.row() <= deltaRow) {//0-deltaRow
StRichAnalogSignalGenerator::calculateRowLimits(const StRichRawCoordinate& raw) const
	limits.second = raw.row()+deltaRow;
    int deltaRow = 1;
    else if (raw.row() >= ( (mNumberOfRowsInColumnQ-1)-(deltaRow)) &&
	     raw.row() <= ( ((mNumberOfRowsInColumnQ-1) )        )) {//47-deltaRow
	limits.first = raw.row()-deltaRow;
    if(tmpRow <= deltaRow) {//0-deltaRow
	limits.first  = 0;
    else if (raw.row() >= ( (mNumberOfRowsInColumnQ         ) ) &&
	     raw.row() <= ( (mNumberOfRowsInColumnQ+deltaRow))) {//48+deltaRow
    else if (tmpRow >= ( (mNumberOfRowsInColumnQ-1)-(deltaRow)) &&
	limits.second = raw.row()+deltaRow;
	limits.first = tmpRow-deltaRow;
    else if (raw.row() >= (2.*mNumberOfRowsInColumnQ - 1) ) {//95
	limits.first  = (2.*mNumberOfRowsInColumnQ - 1) - deltaRow;
	limits.second = (2.*mNumberOfRowsInColumnQ - 1);
	     tmpRow <= ( (mNumberOfRowsInColumnQ+deltaRow))) {//48+deltaRow
	limits.first  = mNumberOfRowsInColumnQ;
	limits.first  = raw.row() - deltaRow;
	limits.second = raw.row() + deltaRow;
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
