/******************************************************
 * $Id: StRichGeometryDb.cxx,v 1.1 2000/01/18 21:32:02 lasiuk Exp $
 *
 * Description:
 *
 ******************************************************
 * $Log: StRichGeometryDb.cxx,v $
 * Revision 1.1  2000/01/18 21:32:02  lasiuk
 * Initial Revision
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
    //RRS

#include "SystemOfUnits.h"
    StRichGeometryDb* StRichGeometryDb::p2Db = 0;
    
    StRichGeometryDb::StRichGeometryDb()
	: quads(5) 
    {
#ifndef ST_NO_NAMESPACES
    : quads(5) 
#endif
    StRichGeometryDb::p2Db = this;   // access to current instance
    }
    : quads(5), quadsOrigin(5)
    void StRichGeometryDb::my_fill()
    {
	version = 1.0;
    quads[4].x0       = -1.92 * centimeter;          // verified
	quads[1].z0       =   1.9 * centimeter;          // verified
	quads[2].z0       = -65.1 * centimeter;          // verified
	quads[3].z0       = -65.1 * centimeter;          // verified
	quads[4].z0       =   1.9 * centimeter;          // verified
	quads[1].x0       =  41.4 * centimeter;          // verified
	quads[2].x0       =  41.4 * centimeter;          // verified
	quads[3].x0       = -1.92 * centimeter;          // verified
	quads[4].x0       = -1.92 * centimeter;          // verified
	quad_gap_x        =  30.0 * millimeter;          // verified          
	quad_gap_z        =  30.0 * millimeter;          
	
	wire_spacing      = 4.2   * millimeter;          // verified
	wire_x0           = 41.61 * centimeter;          // verified
	number_of_wires   = 192;                         // verified
	
	pad_side_x        = 7.9   * millimeter;          // verified
	pad_side_z        = 7.5   * millimeter;          // verified
	pad_spacing       = 0.5   * millimeter;          // verified
	n_pad_x           = 48;                          // verified
	n_pad_z           = 80;                          // verified
	number_of_pads    = 15360;                       // verified
	length            = 1310  * millimeter;          // verified
	width             = 836   * millimeter;          // verified
	height            = 2     * millimeter;          // verified
	ampl_factor       = 1.0e4;                       // verified      
    }
    
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
{

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
