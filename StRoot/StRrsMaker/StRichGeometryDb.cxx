/******************************************************
 * $Id: StRichGeometryDb.cxx,v 1.2 2000/01/25 22:02:20 lasiuk Exp $
 *
 * Description:
 *
 ******************************************************
 * $Log: StRichGeometryDb.cxx,v $
 * Revision 1.2  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
 * Revision 1.4  2000/02/12 21:55:44  lasiuk
 * Wire position adjustment
 *
 * Revision 1.3  2000/02/08 16:26:01  lasiuk
 * rmove vector and StGlobals from Interface.
 * allocate space for survey parameters
 ******************************************************/

//#include "TInterpreter.h"              // ROOT
//#include "TSystem.h"
//#include "St_Table.h"
//#include "St_DataSet.h"
//#include "rich_rich.h"

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
    : quads(5) 
#endif
    StRichGeometryDb::p2Db = this;   // access to current instance
#include "StRichGeometryDb.h"

StRichGeometryDb* StRichGeometryDb::p2Db = 0;

StRichGeometryDb::StRichGeometryDb()
    : quads(5), quadsOrigin(5)
{
    //StRichGeometryDb::p2Db = this;   // access to current instance
    
    quads[1].z0       =   1.9 * centimeter;          // verified
    quads[2].z0       = -65.1 * centimeter;          // verified
    quads[3].z0       = -65.1 * centimeter;          // verified
    quads[4].z0       =   1.9 * centimeter;          // verified
    quads[1].x0       =  41.4 * centimeter;          // verified
    quads[2].x0       =  41.4 * centimeter;          // verified
    quads[3].x0       = -1.92 * centimeter;          // verified
    quads[4].x0       = -1.92 * centimeter;          // verified
StRichGeometryDb::~StRichGeometryDb()
    quad_gap_z        =  30.0 * millimeter;          
    delete p2Db;
}
    wire_x0           = 41.61 * centimeter;          // verified
void StRichGeometryDb::my_fill()
    
    pad_side_x        = 7.9   * millimeter;          // verified
    pad_side_z        = 7.5   * millimeter;          // verified
	
    n_pad_x           = 48;                          // verified
    n_pad_z           = 80;                          // verified
    row_spacing       = 0.5   * millimeter;
    n_pad_y           = 48;                          // verified

    mNumberOfPadsInaColumn = 96;
    ampl_factor       = 1.0e4;                       // verified      
}
    mNumberOfPadsInaRow    = 160;
    quadsOrigin[1].y0 =
/*
 *  A fill from a central DB works the following way. 
 *  A STAR_Table that can hold the values is created. It
 *  should be found in the central repository of such tables.
 *  A macro, located in the central repository of DB macros
 *  fills the table with numbers. Then I copy those numbers
 *  to my DBs.
 */

// void StRichGeometryDb::star_fill()
// {    
//     gInterpreter->ProcessLine(".L rich_rich.C");
//     St_DataSet * r = (St_DataSet *) gInterpreter->Calc("CreateTable()");
//     rich_rich_st * s=(rich_rich_st*)((St_Table*)r)->GetArray();
//     if ( !s ) cerr << "Error reading from STAR_TABLE!!!\n";
    
//     version           = s->version;
    
//     quads[1].z0       = s->quad1_z0;
//     quads[2].z0       = s->quad2_z0;
//     quads[3].z0       = s->quad3_z0;
//     quads[4].z0       = s->quad4_z0;
//     quads[1].x0       = s->quad1_x0;
//     quads[2].x0       = s->quad2_x0;
//     quads[3].x0       = s->quad3_x0;
//     quads[4].x0       = s->quad4_x0;
//     quad_gap_x        = s->quad_gap_x;
//     quad_gap_z        = s->quad_gap_z; 
    
//     wire_spacing      = s->wire_spacing; 
//     wire_x0           = s->wire_x0;  
//     number_of_wires   = s->number_of_wires;  
    
//     pad_side_x        = s->pad_side_x;    
//     pad_side_z        = s->pad_side_z;   
//     pad_spacing       = s->pad_spacing;   
//     n_pad_x           = s->n_pad_x;           
//     n_pad_z           = s->n_pad_z;          
//     number_of_pads    = s->number_of_pads;  
//     length            = s->length;           
//     width             = s->width;            
//     height            = s->height;           
//     ampl_factor       = s->ampl_factor;  
    
// }


/*
 *  Function handles the access to the
 *  only instance by using a static
 *  pointer. 
 */
    wire_y0[1] = -quad_gap_y/2. - (0.5)*row_pitch + wire_spacing/2.;

    mRadiatorDimension = StThreeVector<double>(131./2.,83.6/2.,1.)*centimeter;
    mQuartzDimension = StThreeVector<double>(131./2.,83.6/2.,.5)*centimeter;
    mPadPlaneDimension = StThreeVector<double>(131./2.,83.6/2.,0.)*centimeter;
    mProximityGap = 8.*centimeter; 
    mNormalVectorToPadPlane = StThreeVector<double>(-.49718, 0.86765, -0.00038);
}

StRichGeometryDb* StRichGeometryDb::getDb()
{
    os << "** StRichGeometryDb::print() **" << endl;
	p2Db = new StRichGeometryDb();
    
    os << "Detector Length=   " << (detectorLength()/centimeter) << " cm" << endl;
    os << "Detector Width=    " << (detectorWidth()/centimeter)  << " cm" << endl;

    os << "Xo (1)=            " << (quadrantX0(1)/centimeter)    << " cm" << endl;
    os << "Xo (2)=            " << (quadrantX0(2)/centimeter)    << " cm" << endl;
    os << "Xo (3)=            " << (quadrantX0(3)/centimeter)    << " cm" << endl;
    os << "Xo (4)=            " << (quadrantX0(4)/centimeter)    << " cm" << endl;

    os << "Zo (1)=            " << (quadrantZ0(1)/centimeter)    << " cm" << endl;
    os << "Zo (2)=            " << (quadrantZ0(2)/centimeter)    << " cm" << endl;
    os << "Zo (3)=            " << (quadrantZ0(3)/centimeter)    << " cm" << endl;
    os << "Zo (4)=            " << (quadrantZ0(4)/centimeter)    << " cm" << endl;

    os << "Quad Gap (x)=      " << (quadrantGapsInX()/centimeter) << " cm" << endl;
    os << "Quad Gap (z)=      " << (quadrantGapsInZ()/centimeter) << " cm" << endl;
    // Extra here

    os << "No of Pads:        " <<  (numberOfPads())                       << endl;
    os << " Row:              " << (numberOfPadsInRow())                   << endl;
    os << " Column:           " << (numberOfPadsInColumn())                << endl;

    os << "Pad Pitch=         " << (padPitch()/centimeter)        << " cm" << endl;
    os << "Pad Width=         " << (padWidth()/centimeter)        << " cm" << endl;
    os << "Pad Length=        " << (padLength()/centimeter)       << " cm" << endl;

    os << "Wire Pitch=        " << (wirePitch()/centimeter)       << " cm" << endl;
    os << "Wire OffSet (x)=   " << (firstWirePositionInX()/centimeter) << " cm" << endl;
    os << "Number of Wires:   " << (numberOfWires())                       << endl;
    os << "Pad Spacing=       " << (padSpacing()/millimeter)      << " mm" << endl;

    os << "Gas Gain=          " << (gasGainAmplificationFactor())            << endl;
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
