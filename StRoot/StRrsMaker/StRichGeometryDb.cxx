/*******************************************************************
 * $Id: StRichGeometryDb.cxx,v 1.4 2000/02/12 21:55:44 lasiuk Exp $
 *
 * Description:
 *
 *******************************************************************
 * $Log: StRichGeometryDb.cxx,v $
 * Revision 1.4  2000/02/12 21:55:44  lasiuk
 * Wire position adjustment
 *
 * add normal vector value
 *
 * Revision 1.5  2000/02/29 18:25:46  lasiuk
 * change radial placement of detector
 *
 * Revision 1.4  2000/02/12 21:55:44  lasiuk
 * Wire position adjustment
 *
 * Revision 1.3  2000/02/08 16:26:01  lasiuk
 * rmove vector and StGlobals from Interface.
 * allocate space for survey parameters
 * calculate sector origins and pad corner positions
 *
 * Revision 1.2  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
//SCL
 * Initial Revision
 *
 *******************************************************************/

#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif

//RRS
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#include "StRichGeometryDb.h"

StRichGeometryDb* StRichGeometryDb::p2Db = 0;

StRichGeometryDb::StRichGeometryDb()
    : quads(5), quadsOrigin(5)
{
    //StRichGeometryDb::p2Db = this;   // access to current instance
    
StRichGeometryDb::~StRichGeometryDb()
    quad_gap_z        =  30.0 * millimeter;          
    delete p2Db;
}
void StRichGeometryDb::my_fill()
    
    pad_side_x        = 7.9   * millimeter;          // verified
    pad_side_z        = 7.5   * millimeter;          // verified
    quad_gap_x        =  30.0 * millimeter;          // verified          
    quad_gap_y        =  30.0 * millimeter;          
	
    wire_spacing      = 4.2   * millimeter;          // verified
    number_of_wires   = 192;                         // verified
    n_pad_x           = 48;                          // verified
    n_pad_z           = 80;                          // verified
    pad_pitch         = 8.0   * millimeter;
    row_pitch         = 8.4   * millimeter;
    pad_spacing       = 0.5   * millimeter;          // verified
    row_spacing       = 0.5   * millimeter;

    n_pad_x           = 80;                          // verified
    n_pad_y           = 48;                          // verified

    mNumberOfPadsInaColumn = 96;
    mNumberOfPadsInaRow    = 160;
    
    number_of_pads    = 15360;                       // verified

    mRadialDistanceToRich   = 242.6655311 * centimeter;
    length            = 1310  * millimeter;          // verified
    width             = 836   * millimeter;          // verified
    height            = 2     * millimeter;          // verified
    mRadialDistanceToRich   = 242.925 * centimeter;
	
    //mRadialDistanceToRich   = 242.925 * centimeter;
    // change from gjk (May 15,2000)
    mRadialDistanceToRich   =	243.130 *centimeter;

    //
    // Calculate the positions of the uppermost left had
    quads[1].z0 = quadrantGapInZ()/2. + padPitch()*0.5;
    quads[4].z0 = quads[1].z0;

    //
    quads[2].z0 = -quadrantGapInZ()/2. - padPitch()*(numberOfPadsInAQuadrantRow()-0.5);
    quads[3].z0 = quads[2].z0;
    
    //1.9 * centimeter;          // verified
    quads[1].x0 =
	quadrantGapInX()/2 + rowPitch()*(numberOfRowsInAQuadrantColumn()-0.5);
    quads[2].x0 = quads[1].x0;
    // -65.1 * centimeter;          // verified
    quads[2].x0 = -quadrantGapInX()/2. - padPitch()*(numberOfPadsInAQuadrantRow()-0.5);
    quads[3].x0 = -quadrantGapInX()/2. - rowPitch()*0.5;
    quads[4].x0 = quads[3].x0;   
    // 41.4 * centimeter;          // verified
    quads[1].y0 =
	quadrantGapInY()/2 + rowPitch()*(numberOfRowsInAQuadrantColumn()-0.5);
    quads[2].y0 = quads[1].y0;
    quadsOrigin[1].z0 =
	quadrantGapInZ()/2. + padPitch()*(numberOfPadsInAQuadrantRow()/2.);
    quadsOrigin[4].z0 = quadsOrigin[1].z0;
    quads[4].y0 = quads[3].y0;   

    quadsOrigin[2].z0 =
	-quadrantGapInZ()/2. - padPitch()*(numberOfPadsInAQuadrantRow()/2.);
    quadsOrigin[3].z0 = quadsOrigin[2].z0;
    quadsOrigin[1].x0 =
	quadrantGapInX()/2. + padPitch()*(numberOfPadsInAQuadrantRow()/2.);
    quadsOrigin[1].x0 =
	quadrantGapInX()/2 + rowPitch()*(numberOfRowsInAQuadrantColumn()/2.);
    quadsOrigin[2].x0 = quadsOrigin[1].x0;
    quadsOrigin[2].x0 =
	-quadrantGapInX()/2. - padPitch()*(numberOfPadsInAQuadrantRow()/2.);
    quadsOrigin[3].x0 =
	-quadrantGapInX()/2. - rowPitch()*(numberOfRowsInAQuadrantColumn()/2.);
    quadsOrigin[4].x0 = quadsOrigin[3].x0;   
    quadsOrigin[1].y0 =
	quadrantGapInY()/2 + rowPitch()*(numberOfRowsInAQuadrantColumn()/2.);
    quadsOrigin[2].y0 = quadsOrigin[1].y0;

    wire_x0[0] = quad_gap_x/2. + (n_pad_x - 0.5)*row_pitch + wire_spacing/2.;
    quadsOrigin[3].y0 =
    wire_x0[1] = -quad_gap_x/2. - (0.5)*row_pitch + wire_spacing/2.;
    //-1.71 * centimeter;          // verified
    wire_y0[1] = -quad_gap_y/2. - (0.5)*row_pitch + wire_spacing/2.;

    mRadiatorDimension = StThreeVector<double>(131./2.,83.6/2.,1.)*centimeter;
    mQuartzDimension = StThreeVector<double>(131./2.,83.6/2.,.5)*centimeter;
    mPadPlaneDimension = StThreeVector<double>(131./2.,83.6/2.,0.)*centimeter;
    mProximityGap = 8.*centimeter; 
    mNormalVectorToPadPlane = StThreeVector<double>(-.49718, 0.86765, -0.00038);
}

StRichGeometryDb* StRichGeometryDb::getDb()
{
    if(!p2Db)
	p2Db = new StRichGeometryDb();
    
    return p2Db;
}

    os << " (Zo, Xo) cm  (Corner Pad)" << endl;
    os << "  (" << (quadrantZ0(2)/centimeter) << "," << (quadrantX0(2)/centimeter) << ")      ";
    os << " (" << (quadrantZ0(1)/centimeter) << "," << (quadrantX0(1)/centimeter) << ")" << endl;
    os << "  (" << (quadrantZ0(3)/centimeter) << "," << (quadrantX0(3)/centimeter) << ")      ";
    os << "(" << (quadrantZ0(4)/centimeter) << "," << (quadrantX0(4)/centimeter) << ")" << endl;
    os << " Detector Length=   " << (detectorLength()/centimeter) << " cm" << endl;
    os << " Detector Width=    " << (detectorWidth()/centimeter)  << " cm\n" << endl;
    os << "  (" << (quadrantZOrigin(2)/centimeter) << "," << (quadrantXOrigin(2)/centimeter) << ")    ";
    os << " (" << (quadrantZOrigin(1)/centimeter) << "," << (quadrantXOrigin(1)/centimeter) << ")" << endl;
    os << "  (" << (quadrantZOrigin(3)/centimeter) << "," << (quadrantXOrigin(3)/centimeter) << ")    ";
    os << "(" << (quadrantZOrigin(4)/centimeter) << "," << (quadrantXOrigin(4)/centimeter) << ")\n" << endl;
    os << "  (" << (quadrantX0(3)/centimeter) << "," << (quadrantY0(3)/centimeter) << ")      ";
    os << "(" << (quadrantX0(4)/centimeter) << "," << (quadrantY0(4)/centimeter) << ")" << endl;
    os << "Quad Gap (z)=      " << (quadrantGapInZ()/millimeter) << " mm" << endl;
    os << "\nQuadrant Origins" << endl;
    os << "  (" << (quadrantXOrigin(2)/centimeter) << "," << (quadrantYOrigin(2)/centimeter) << ")    ";

    os << endl;
    os << "radiator Dimension:  " << (radiatorDimension()/centimeter) << " cm" << endl;
    os << "quartz Dimension:    " << (quartzDimension()/centimeter)   << " cm" << endl;
    os << "pad Plane Dimension: " << (padPlaneDimension()/centimeter) << " cm" << endl;
    os << "Proximity Gap:       " << (proximityGap()/centimeter)      << " cm" << endl;
    
    os << endl;
    os << "Total # of Pads:   " << (numberOfPads())                       << endl;
    os << " Complete Row:     " << (numberOfPadsInARow())                 << endl;
    os << " Complete Column:  " << (numberOfRowsInAColumn())              << endl;
    os << " Quadrant Row:     " << (numberOfPadsInAQuadrantRow())         << endl;
    os << " Quadrant Column:  " << (numberOfRowsInAQuadrantColumn())      << endl;

    os << endl;
    os << "Pad Width=         " << (padWidth()/millimeter)        << " mm" << endl;
    os << "Pad Length=        " << (padLength()/millimeter)       << " mm" << endl;
    os << "Wire OffSet (+x)=   " << (firstWirePositionInX(1)/centimeter) << " cm" << endl;
    os << "Wire OffSet (-x)=   " << (firstWirePositionInX(-1)/centimeter) << " cm" << endl;
    os << "Pad Spacing=       " << (padSpacing()/millimeter)      << " mm" << endl;
    os << "Row Spacing=       " << (rowSpacing()/millimeter)      << " mm" << endl;

    os << endl;
    os << "Number of Wires:   " << (numberOfWires())                       << endl;
    os << "Wire Pitch=        " << (wirePitch()/millimeter)       << " mm" << endl;
    os << "Wire OffSet (+y)=   " << (firstWirePositionInY(1)/centimeter) << " cm" << endl;

    os << endl;
    os << "AnodeToPadSpacing= " << (anodeToPadSpacing()/centimeter) << " cm" << endl;

    os << "\nSurvey Geometry:" << endl;
    os << "radial Distance=   " << (radialDistanceToRich()/centimeter) << " cm"      << endl;
    os << "inclination Angle= " << (inclinationAngle()/degree)         << " degrees" << endl;
#ifndef ST_NO_NAMESPACES
//}
#endif
    os << "normal Vector:     " << normalVectorToPadPlane()            << endl;

    os << "\n*************** End of Geometry Parameters **********************\n" << endl;
}
