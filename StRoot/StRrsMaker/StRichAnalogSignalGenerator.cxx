/*******************************************************************
 * $Id: StRichAnalogSignalGenerator.cxx,v 1.1 2000/01/18 21:31:59 lasiuk Exp $
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
 * Revision 1.1  2000/01/18 21:31:59  lasiuk
 * Initial Revision
 *
 * Revision 1.3  2000/02/08 16:21:41  lasiuk
 * use coordinate transformation routines for pad limits
 * incorporation of dbs
 *
 * Revision 1.2  2000/01/25 22:02:19  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:31:59  lasiuk
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
 *
 **************************************************************************/

#include <math.h>                         
#include <algorithm>

#ifndef ST_NO_NAMESPACES
using std::min;
#include "StRichGeometryDb.h"
#include "StRichOtherAlgorithms.h"
//namespace StRichRawData { 
#endif
#include "StRichCoordinateTransform.h"
#include "StRichWriter.h"
#include "StRichAnalogSignalGenerator.h"
#include "StRichWriter.h"

    void StRichAnalogSignalGenerator::operator()( const StRichGHit& hit, double q ) const
    {
	static StRichGeometryDb* geoDb  = StRichGeometryDb::getDb(); 
	static StRichWriter* output     = StRichWriter::getInstance();
	static MyRound round;
	static double pad_spacing = geoDb->pad_spacing;
	static int n_pad_z        = geoDb->n_pad_z;
	static int n_pad_x        = geoDb->n_pad_x;
	static double pad_side_x  = geoDb->pad_side_x;
	static double pad_side_z  = geoDb->pad_side_z;
	static double height      = geoDb->height;
	static int area = 1;                                            // 3x3 pad area
	double sum = 0;
	double x, z, q00, q10, q01, q11,s;
    if(RRS_DEBUG)
    
	// find which pad ( row and col )
	
	int r_factor = n_pad_x * ((hit.quad == 3 || hit.quad == 4) ? 1 : 0); 
	int c_factor = n_pad_z * ((hit.quad == 1 || hit.quad == 4) ? 1 : 0); 
	int row =
	    round( (geoDb->quads[hit.quad].x0 - hit.x)  /  (pad_side_x + pad_spacing) ) +
	    r_factor;
	int col =
	    round( (hit.z - geoDb->quads[hit.quad].z0)  /  (pad_side_z + pad_spacing) ) +
	    c_factor;   
    double x, y, q00, q10, q01, q11,s;
    //


	// x lower bound 
	if (row < r_factor)
	    row = r_factor;
	
	// x upper bound
	if (row > n_pad_x-1+r_factor)
	    row = n_pad_x-1+r_factor;          

	// z lower bound
	if (col < c_factor)
	    col = c_factor;

	// z upper bound
	if (col > n_pad_z-1+c_factor)
	    col = n_pad_z-1+c_factor;

	// looping through neighboring pads

	int i, j;
	for (i=max(r_factor,row-area); i<=min(row+area, r_factor+n_pad_x-1); i++) 
	    for (j=max(c_factor,col-area); j<=min(col+area, c_factor+n_pad_z-1); j++) 
		{    
		    x = geoDb->quads[hit.quad].x0 - (i-r_factor)*(pad_side_x + pad_spacing);
		    z = geoDb->quads[hit.quad].z0 + (j-c_factor)*(pad_side_z + pad_spacing);
	
		    q00 = induceTension (( (hit.x-x) - pad_side_x/2)  /  height,   
					 ( (hit.z-z) - pad_side_z/2)  /  height );    
		    q10 = induceTension (( (hit.x-x) + pad_side_x/2)  /  height,   
					 ( (hit.z-z) - pad_side_z/2)  /  height );    
		    q01 = induceTension (( (hit.x-x) - pad_side_x/2)  /  height,     
					 ( (hit.z-z) + pad_side_z/2)  /  height );     
		    q11 = induceTension (( (hit.x-x) + pad_side_x/2)  /  height,    
					 ( (hit.z-z) + pad_side_z/2)  /  height );   

		    s =  q * (q00-q10-q01+q11);
		    sum += s;

		    // save signal on pad
			StRichViewer::getView()->mStRichAnalogSignals->Fill(z,x,s);        // histograms
	    s =  q * (q00-q10-q01+q11);
		    if (StRichViewer::histograms ) 
			StRichViewer::getView()->mAnalogSignals->Fill(z,x,s);        // histograms
	    // save signal on pad
		} 

	    if (StRichViewer::histograms ) 
		StRichViewer::getView()->mAnalogSignals->Fill(x,y,s);
	    cout << "fill ASG" << endl;
	    // histograms
    int s = 1;
    for (int i=1; i<=29 ; i+=2) {
    else {

    double StRichAnalogSignalGenerator::induceTension(double ratiox, double ratioz) const 
    {
	static const double g = 0.9159655942;   // Catalan constant
	double qsum, c1, c2, arctan;
    
	c1 = ratiox*ratioz;
	c2 = ratiox*ratiox + ratioz*ratioz;
    
	int s = 1;
	qsum = 0;
    
	for (int i=1; i<=29 ; i+=2)
	    {
		arctan = atan(c1/(i*sqrt(i*i + c2)));
		qsum += s*(arctan-c1/(i*i));
		s = -s;
	    }
	return ((g*c1 + qsum)/(2*M_PI));
    else if (tmpRow >= (2*mNumberOfRowsInColumnQ - 1) ) {//95

    else {
	limits.first  = tmpRow - deltaRow;
	limits.second = tmpRow + deltaRow;
    }
    
    return limits;
}

#ifndef ST_NO_NAMESPACES
//}
#endif
