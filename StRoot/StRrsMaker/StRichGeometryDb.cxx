/*******************************************************************
 * $Id: StRichGeometryDb.cxx,v 2.7 2009/08/26 13:09:39 fine Exp $
 *
 * Description:
 *
 *******************************************************************
 * $Log: StRichGeometryDb.cxx,v $
 * Revision 2.7  2009/08/26 13:09:39  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 2.6  2002/02/22 18:37:18  dunlop
 * adjusted production version as short-term fix
 *
 * Revision 2.5  2001/10/04 22:16:48  dunlop
 * Fixing small mistake in new euler angles
 *
 * Revision 2.4  2001/10/04 19:37:43  dunlop
 * Modification to fill in year 2001 information.
 * Has TPC in global coordinates + small shift + mod to gap in x and y to match
 * machine drawings
 *
 * Revision 2.3  2001/08/22 16:42:59  lasiuk
 * data base functionality to check library environment variable
 *
 * Revision 2.2  2000/11/21 21:40:45  dunlop
 * Tweak for ExB effects
 *
 * Revision 2.1  2000/10/20 23:25:16  dunlop
 * Fine tune for P00hi
 *
 * Revision 2.0  2000/08/09 16:17:00  gans
 * Readded Files That were not added in last CVS. Cosmetic Changes, naming convention
 * for StRichDrawableT(foo)
 *
 * Revision 1.9  2000/05/19 15:15:08  lasiuk
 * modification of parameters for ideal geometry for simulation
 * normal vector and proximity gap
 *
 * Revision 1.8  2000/05/15 18:03:45  lasiuk
 * change survey parameter distance to pad plane.
 * Shift to .13 for t'formation routines.  (gjk)
 *
 * Revision 1.7  2000/03/17 14:54:39  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.6  2000/03/12 22:18:45  lasiuk
 * add from materials Db
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
 * Initial Revision
 *
 *******************************************************************/
#include <stdlib.h>
#include <cstring>
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#include "StRichGeometryDb.h"

StRichGeometryDb* StRichGeometryDb::p2Db = 0;

StRichGeometryDb::StRichGeometryDb()
    : quads(5), quadsOrigin(5)
{
    //StRichGeometryDb::p2Db = this;   // access to current instance
    
    //star_fill();
    my_fill();
}

StRichGeometryDb::~StRichGeometryDb()
{
    delete p2Db;
}
void StRichGeometryDb::my_fill()
{
    mVersion = 1.0;
    
// JCD Changed 10/4/2001 to match machine drawings
    quad_gap_x        =  30.0 * millimeter;          // verified          
    quad_gap_y        =  29.8 * millimeter;          
	
    wire_spacing      = 4.2   * millimeter;          // verified
    number_of_wires   = 192;                         // verified

    pad_side_x        = 7.5   * millimeter;          // verified
    pad_side_y        = 7.9   * millimeter;          // verified
    pad_pitch         = 8.0   * millimeter;
    row_pitch         = 8.4   * millimeter;
    pad_spacing       = 0.5   * millimeter;          // verified
    row_spacing       = 0.5   * millimeter;

    n_pad_x           = 80;                          // verified
    n_pad_y           = 48;                          // verified

    mNumberOfPadsInaColumn = 96;
    mNumberOfPadsInaRow    = 160;
    
    number_of_pads    = 15360;                       // verified

    // Detector Parameters
    length            = 1310  * millimeter;          // verified
    width             = 836   * millimeter;          // verified
    height            = 2     * millimeter;          // verified

    //
    // Survey Geometry
    //    mInclinationAngle       = 30.  * degree;
    //mRadialDistanceToRich   = 242.925 * centimeter;
    // change from gjk (May 15,2000)
    //mRadialDistanceToRich   =	243.130 *centimeter;
    // Survey Info from jcd (July,10 2000)
    // mRadialDistanceToRich   =	240.1246 *centimeter;
    //

    // Calculate the positions of the uppermost left had
    // corner pads in the pad plane quadrant.  Located in
    // StRichGeometryDbInterface.cxx!

    //
    // Do not use units here, they are already used!
    //
    
    //1.9 * centimeter;          // verified
    quads[1].x0 = quadrantGapInX()/2. + padPitch()*0.5;
    quads[4].x0 = quads[1].x0;
    
    // -65.1 * centimeter;          // verified
    quads[2].x0 = -quadrantGapInX()/2. - padPitch()*(numberOfPadsInAQuadrantRow()-0.5);
    quads[3].x0 = quads[2].x0;

    // 41.4 * centimeter;          // verified
    quads[1].y0 =
	quadrantGapInY()/2 + rowPitch()*(numberOfRowsInAQuadrantColumn()-0.5);
    quads[2].y0 = quads[1].y0;

    // -1.92 * centimeter;
    quads[3].y0 = -quadrantGapInY()/2. - rowPitch()*0.5;
    quads[4].y0 = quads[3].y0;   

    //
    // Origin of Quadrant Centers
    //(+/-33.5, +/- 21.65)
    quadsOrigin[1].x0 =
	quadrantGapInX()/2. + padPitch()*(numberOfPadsInAQuadrantRow()/2.);
    quadsOrigin[4].x0 = quadsOrigin[1].x0;
    
    // -65.1 * centimeter;          // verified
    quadsOrigin[2].x0 =
	-quadrantGapInX()/2. - padPitch()*(numberOfPadsInAQuadrantRow()/2.);
    quadsOrigin[3].x0 = quadsOrigin[2].x0;

    // 41.4 * centimeter;          // verified
    quadsOrigin[1].y0 =
	quadrantGapInY()/2 + rowPitch()*(numberOfRowsInAQuadrantColumn()/2.);
    quadsOrigin[2].y0 = quadsOrigin[1].y0;

    // -1.92 * centimeter;
    quadsOrigin[3].y0 =
	-quadrantGapInY()/2. - rowPitch()*(numberOfRowsInAQuadrantColumn()/2.);
    quadsOrigin[4].y0 = quadsOrigin[3].y0;   

    //
    // First wire position in x
    //41.61 * centimeter;          // verified
    wire_y0[0] = quad_gap_y/2. + (n_pad_y - 0.5)*row_pitch + wire_spacing/2.;
    //-1.71 * centimeter;          // verified
    wire_y0[1] = -quad_gap_y/2. - (0.5)*row_pitch + wire_spacing/2.;

    mRadiatorDimension = StThreeVector<double>(131./2.,83.6/2.,1.)*centimeter;
    mQuartzDimension = StThreeVector<double>(131./2.,83.6/2.,.5)*centimeter;
    mPadPlaneDimension = StThreeVector<double>(131./2.,83.6/2.,0.)*centimeter;

//Change on July 14, 2000 to make switchable, use survey flexibility

    mProximityGap = 8.*centimeter;

#ifdef RICH_SIM_GEOMETRY

    //
    // This is ideal geometry for historical reasons only
    //
    // Cylindrical coordinates: 5 o'clock = -60 degrees
    //
    cout << "StRichGeometryDB::my_fill() use ideal" << endl;
    mLocalOriginAngle       = -60.  * degree;
    mLocalOriginR           = 243.13 * centimeter;    
    mLocalOriginZ           = 0. * centimeter;
    
    mLocalAngleZ            = 30. * degree;
    mLocalAngleY            = 0. * degree;
    mLocalAngleX           = 0. * degree;

#else

     //
     // this would be so much easier with
     // strings, but...
     //
     const char* starVersion = getenv("STAR_VERSION");
     cout << "starVersion= " << starVersion << endl;
     
     if(strstr(starVersion, "00")) {
	cout << "Year 2000 Geometry: " << endl;
	this->fill2000();
     }
     else if(strstr(starVersion, "01he") || strstr(starVersion, "01e")) {
	cout << "Year 2000 Geometry 2nd production: " << endl;
	this->fill2001he();	 
     }
     else if(strstr(starVersion, "01")||strstr(starVersion, "DEV")||strstr(starVersion,"02")) {
	cout << "Year 2001 Geometry: " << endl;
	this->fill2001();	 
     }
     else {
	 cout << "Warning:\n";
	 cout << "\tStRichGeometry::my_fill()\n";
	 cout << "\tUnknown starVersion: " << starVersion << endl;
	 cout << "\tUse Year 2001 Geometry" << endl;
	 this->fill2001();
     }

     mNormalVectorToPadPlane = 
	 StThreeVector<double>(
			       (cos(mLocalAngleZ)*sin(mLocalAngleY)*sin(mLocalAngleX)
				- sin(mLocalAngleZ) * cos(mLocalAngleX)),
			       (sin(mLocalAngleZ)*sin(mLocalAngleY)*sin(mLocalAngleX)
				+ cos(mLocalAngleZ)*cos(mLocalAngleX)),
			       (cos(mLocalAngleY)*sin(mLocalAngleX))
			       );
     
#endif

}


void StRichGeometryDb::fill2000() {

    //
    // This is the standard 2000 geometry that
    // was used for the 2000 november-december
    // production
    //
    // Euler-like angles
    //
    
    mLocalAngleX = -0.0200353082209942 * degree ;
    mLocalAngleY = 0.037884454328129 * degree ;
    mLocalAngleZ = 29.8171493344754 * degree ;

    //
    // Cylindrical coordinates: 5 o'clock = -60 degrees
    //    mLocalOriginAngle = -59.9911296661538 * degree ; 
    //    mLocalOriginR = 240.126958857466 * centimeter ;
    //    mLocalOriginZ = -0.558878974855719 * centimeter ; 
    // local shift of (0.25,0.17,-0.1)
    //
    
    mLocalOriginAngle = -59.9506239157014*degree;
    mLocalOriginR = 240.22755151829*centimeter;
    mLocalOriginZ = -0.3089772 * centimeter;
}

void StRichGeometryDb::fill2001he() {

    cout << "*******************WARNING ************************\a\a" << endl;
    cout << "\tStRichGeometryDb::fill2001he()\n";
    cout << "\tNo Geometry defined for year 2001he just yet\n";
    cout << "\tThis is the Year 2000 data produced in the spring/summer 2001\n";
    cout << "\tUsing Year 2000" << endl;
    this->fill2000();
    
    //
    // 2001 Survey Geometry
    // Euler-like rotations
    //
    
    //mLocalAngleX = 
    //mLocalAngleY = 
    //mLocalAngleZ = 

    //mLocalOriginAngle =
    //mLocalOriginR     =
    //mLocalOriginZ     =

}
void StRichGeometryDb::fill2001() {

    cout << "\tStRichGeometryDb::fill2001()\n";
    // production
    //
    // Euler-like angles
    //
    // Use exact from survey
    mLocalAngleX = -0.0200353082209942 * degree ;
    mLocalAngleY = 0.037884454328129 * degree ;
    mLocalAngleZ = 29.8171493344754 * degree ;
    //
    // P00hm + tpc local->global + (0.03,-0.025,0) local
    mLocalOriginAngle = -60.0233755889891 * degree ; 
    mLocalOriginR = 240.160406174684 * centimeter ;
    mLocalOriginZ = -0.470960678181733 * centimeter ; 
    cout << "mLocalOriginAngle = " << mLocalOriginAngle/degree << endl;
    cout << "mLocalOriginR = " << mLocalOriginR/centimeter << endl;
    cout << "mLocalOriginZ = " << mLocalOriginZ/centimeter << endl;
	

}


StRichGeometryDb* StRichGeometryDb::getDb()
{
    if(!p2Db)
	p2Db = new StRichGeometryDb();
    
    return p2Db;
}

void StRichGeometryDb::print(ostream& os) const
{
    os << "**************** StRichGeometryDb::print() ****************" << endl;
    os << "Detector:" << endl;
    
    os << " Detector Length=   " << (detectorLength()/centimeter) << " cm" << endl;
    os << " Detector Width=    " << (detectorWidth()/centimeter)  << " cm\n" << endl;

    os << " (Xo, Yo) cm  (Corner Pad)" << endl;
    os << "  (" << (quadrantX0(2)/centimeter) << "," << (quadrantY0(2)/centimeter) << ")      ";
    os << " (" << (quadrantX0(1)/centimeter) << "," << (quadrantY0(1)/centimeter) << ")" << endl;
    os << "  (" << (quadrantX0(3)/centimeter) << "," << (quadrantY0(3)/centimeter) << ")      ";
    os << "(" << (quadrantX0(4)/centimeter) << "," << (quadrantY0(4)/centimeter) << ")" << endl;

    os << "\nQuadrant Origins" << endl;
    os << "  (" << (quadrantXOrigin(2)/centimeter) << "," << (quadrantYOrigin(2)/centimeter) << ")    ";
    os << " (" << (quadrantXOrigin(1)/centimeter) << "," << (quadrantYOrigin(1)/centimeter) << ")" << endl;
    os << "  (" << (quadrantXOrigin(3)/centimeter) << "," << (quadrantYOrigin(3)/centimeter) << ")    ";
    os << "(" << (quadrantXOrigin(4)/centimeter) << "," << (quadrantYOrigin(4)/centimeter) << ")\n" << endl;

    os << "Quad Gap (x)=      " << (quadrantGapInX()/millimeter) << " mm" << endl;
    os << "Quad Gap (y)=      " << (quadrantGapInY()/millimeter) << " mm" << endl;

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
    os << "Pad Pitch=         " << (padPitch()/millimeter)        << " mm" << endl;
    os << "Row Pitch=         " << (rowPitch()/millimeter)        << " mm" << endl;
    os << "Pad Spacing=       " << (padSpacing()/millimeter)      << " mm" << endl;
    os << "Row Spacing=       " << (rowSpacing()/millimeter)      << " mm" << endl;

    os << endl;
    os << "Number of Wires:   " << (numberOfWires())                       << endl;
    os << "Wire Pitch=        " << (wirePitch()/millimeter)       << " mm" << endl;
    os << "Wire OffSet (+y)=   " << (firstWirePositionInY(1)/centimeter) << " cm" << endl;
    os << "Wire OffSet (-y)=   " << (firstWirePositionInY(-1)/centimeter) << " cm" << endl;

    os << endl;
    os << "AnodeToPadSpacing= " << (anodeToPadSpacing()/centimeter) << " cm" << endl;


    os << "\nGeometry for Local <-> Global:" << endl;
    os << "Origin in cylindrical coordinates:" << endl;
    os << "Radial distance =   " << (localOriginR()/centimeter) << " cm"      << endl;
    os << "Angle = " << (localOriginAngle()/degree)         << " degrees" << endl;
    os << "Z position =   " << (localOriginZ()/centimeter) << " cm"      << endl;

    os << "Euler-like angles: local in global = Rz * Ry * Rx * P(xyz->zxy)" << endl;
    os << "AngleX = " << (localAngleX()/degree)         << " degrees" << endl;
    os << "AngleY = " << (localAngleY()/degree)         << " degrees" << endl;
    os << "AngleZ = " << (localAngleZ()/degree)         << " degrees" << endl;

    os << "normal Vector:     " << normalVectorToPadPlane()            << endl;

    os << "\n*************** End of Geometry Parameters **********************\n" << endl;
}
