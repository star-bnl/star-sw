#include <stdio.h>
#include <iostream.h>
#include <strings.h>
#include <sys/types.h>
#include <stdlib.h>
#include <Rtypes.h> /* use ROOT variables: ..._t */
#include "St_l3_Coordinate_Transformer.h"
#include "St_l3_Coordinates.h"
#include "StDbUtilities/StCoordinates.hh"
#include "StTpcDb/StTpcDb.h"

ClassImp(St_l3_Coordinate_Transformer);


//______________________________
St_l3_Coordinate_Transformer::St_l3_Coordinate_Transformer()
{
    // allocate memory for varialbes
    xyz_local = new Double_t[3];
    xyz_global = new Double_t[3];

    // initialize transformations
    this->Use_transformation_provided_by_db() ;
    //this->Print_parameters() ;    
}
//______________________________
St_l3_Coordinate_Transformer::~St_l3_Coordinate_Transformer()
{
    delete [] xyz_local;
    delete [] xyz_global;
}
//______________________________
void St_l3_Coordinate_Transformer::raw_to_local()
{
  Int_t row    =  (Int_t) pa_ti_ro_se[2];
  Double_t  pitch = (pa_ti_ro_se[2]<14) ? innerSectorPadPitch : outerSectorPadPitch;
  
  Double_t pads2move = pa_ti_ro_se[0] - numberOfPadsAtRow[(Int_t)pa_ti_ro_se[2]-1]/2;
  xyz_local[0] = pitch * (pads2move-.5);
  xyz_local[1] = radialDistanceAtRow[Int_t(pa_ti_ro_se[2])-1];
  if (row <=13 )
      {
	  Double_t innerdriftLength = 200.668 ; 
  	  Double_t lengthpertimebucket = 0.574731 ;
	  xyz_local[2] = innerdriftLength - pa_ti_ro_se[1] *lengthpertimebucket;
      }
  else
      {
	  Double_t outerdriftLength = 201.138 ; 
  	  Double_t lengthpertimebucket = 0.574731 ;
	  xyz_local[2] = outerdriftLength - pa_ti_ro_se[1] *lengthpertimebucket;
      }  
}
//______________________________
inline void St_l3_Coordinate_Transformer::local_to_global()
{
    Int_t sector = (Int_t) pa_ti_ro_se[3];
    xyz_global[0] = SectorCos[sector-1] * xyz_local[0] + SectorSin[sector-1] * xyz_local[1] ;
    Int_t eastsector = (sector>12) ? sector-12 : sector;
    xyz_global[1] = -1.*SectorSin[eastsector-1] * xyz_local[0] + SectorCos[eastsector-1] * xyz_local[1] ;
    xyz_global[2] =  (sector<13) ? xyz_local[2] : -xyz_local[2] ;

 }
//______________________________
Double_t* St_l3_Coordinate_Transformer::raw_to_global(Double_t* Pa_Ti_Ro_Se)
{
    pa_ti_ro_se = Pa_Ti_Ro_Se;
    raw_to_local();
    local_to_global();
    return xyz_global;
}
//______________________________
void St_l3_Coordinate_Transformer::raw_to_global(const St_l3_ptrs_Coordinate &raw ,St_l3_xyz_Coordinate &global )
{
    St_l3_xyz_Coordinate local(0,0,0) ;
    raw_to_local( raw ,local ) ;
    local_to_global( raw ,local ,global ) ;
}
//______________________________
void St_l3_Coordinate_Transformer::raw_to_local(const St_l3_ptrs_Coordinate &raw ,St_l3_xyz_Coordinate &local )
{
    Double_t  pitch = (raw.Getr()<14) ?    innerSectorPadPitch : outerSectorPadPitch;
    Double_t  pads2move =  raw.Getp() - numberOfPadsAtRow[(Int_t)raw.Getr()-1]/2;  
    local.Setx( pitch * (pads2move - 0.5) ) ;
    Double_t a = raw.Getp() ;
    Double_t b = raw.Gett() ;
    //raw.Setp(99) ;
    a= raw.Getp() ;
    local.Sety( radialDistanceAtRow[(Int_t)(raw.Getr())-1] ) ;
		
    // Timebucket to z different for inner and outer sector and east and west half of tpc
    if( raw.Gets()<=12 )
	{
	    if( raw.Getr() <= 13 )
		{
		    local.Setz(drift_length_inner_east - raw.Gett() * lengthPerTb) ;
		}
	    else 
		{
		    local.Setz(drift_length_outer_east - raw.Gett() * lengthPerTb) ;
		}
	}
    else if ( raw.Gets()>12 )
	{
	   if( raw.Getr() <= 13 )
		{
		    local.Setz(drift_length_inner_west - raw.Gett() * lengthPerTb) ;
		}
	    else 
		{
		    local.Setz(drift_length_outer_west - raw.Gett() * lengthPerTb) ;
		}
	} 
}
//______________________________
void St_l3_Coordinate_Transformer::local_to_global(const St_l3_ptrs_Coordinate &raw , const St_l3_xyz_Coordinate &local ,St_l3_xyz_Coordinate &global )
{
    Int_t sector = (Int_t) raw.Gets();
    global.Setx( SectorCos[sector-1] * local.Getx() + SectorSin[sector-1] * local.Gety() ) ;
    
    if ( sector <= 12 )
	{
	    global.Sety( -1.*SectorSin[sector-1] * local.Getx() + SectorCos[sector-1] * local.Gety()) ;
	    global.Setz( local.Getz() ) ;
	}
    else 
	{
	    global.Sety( -1.*SectorSin[(sector-12)-1] * local.Getx() + SectorCos[(sector-12)-1] * local.Gety()) ; 
	    global.Setz( -1 * local.Getz() ) ;
	}
}
//______________________________
void St_l3_Coordinate_Transformer::Set_parameters_by_hand()
{
    // Set the parameters straight forward
    lengthPerTb = 0.5738;
    drift_length_inner_east = 200.448;
    drift_length_outer_east = 200.918;
    drift_length_inner_west = 200.094;
    drift_length_outer_west = 200.564;

    cout << "Constants for transformation set by hand." << endl;
}
//______________________________
void St_l3_Coordinate_Transformer::Use_transformation_provided_by_db()
{
  // perform official transform and recalulate parameters
  // the official transform doesn't take doubles for pad and time
  // that's why we must extract the parameters ...
  StTpcPadCoordinate* padco = new StTpcPadCoordinate();
  StGlobalCoordinate* glo   = new StGlobalCoordinate();
  StTpcCoordinateTransform tra(gStTpcDb);

  // inner row sector 1 (east)
  padco->setSector(1);
  padco->setRow(5);
  padco->setPad(1);
  padco->setTimeBucket(100);
   
  tra(*padco,*glo);
  Double_t z_100 = glo->position().z();
  padco->setTimeBucket(0);
  tra(*padco,*glo);
  Double_t z_0 = glo->position().z();
  Double_t lengthPerTb_inner_east = fabs((z_0-z_100)/100) ;
  drift_length_inner_east = fabs(glo->position().z()+0.5*lengthPerTb_inner_east) ;
    
  //cout << "sector : " << padco->sector() << "\t"; 
  //cout << "length per tb inner = " << lengthPerTb  << "\t";
  //cout << "innerdrift : " << innerdriftLength << endl;
    
  // outer row sector 1 (east)
  padco->setSector(1);
  padco->setRow(30);
  padco->setPad(1);
  padco->setTimeBucket(100);
  tra(*padco,*glo);
  z_100 = glo->position().z();
  padco->setTimeBucket(0);
  tra(*padco,*glo);
  z_0 = glo->position().z();
  Double_t lengthPerTb_outer_east = fabs((z_0-z_100)/100) ;
  drift_length_outer_east = fabs(glo->position().z()+0.5*lengthPerTb_outer_east) ;

  //cout << "sector : " << padco->sector() << "\t"; 
  //cout << "length per tb outer = " << lengthPerTb  << "\t";
  //cout << "outerdrift : " << outerdriftLength << endl;

  // inner row sector 15 (west)
  padco->setSector(15);
  padco->setRow(5);
  padco->setPad(1);
  padco->setTimeBucket(100);
   
  tra(*padco,*glo);
  z_100 = glo->position().z();
  padco->setTimeBucket(0);
  tra(*padco,*glo);
  z_0 = glo->position().z();
  Double_t lengthPerTb_inner_west = fabs((z_0-z_100)/100) ;
  drift_length_inner_west = fabs(glo->position().z()-0.5*lengthPerTb_inner_west) ;

  //cout << "sector : " << padco->sector() << "\t"; 
  //cout << "length per tb inner = " << lengthPerTb_w  << "\t";
  //cout << "innerdrift : " << innerdriftLength_w << endl;
    
    // outer row sector 15 (west)
  padco->setSector(15);
  padco->setRow(30);
  padco->setPad(1);
  padco->setTimeBucket(100);
  tra(*padco,*glo);
  z_100 = glo->position().z();
  padco->setTimeBucket(0);
  tra(*padco,*glo);
  z_0 = glo->position().z();
  Double_t lengthPerTb_outer_west = fabs((z_0-z_100)/100) ;
  drift_length_outer_west = fabs(glo->position().z()-0.5*lengthPerTb_outer_west) ;

  //cout << "sector : " << padco->sector() << "\t"; 
  //cout << "length per tb outer = " << lengthPerTb_w  << "\t";
  //cout << "outerdrift : " << outerdriftLength_w << endl;   
  
  if (lengthPerTb_outer_west != lengthPerTb_inner_west != lengthPerTb_outer_east != lengthPerTb_inner_east)
    {
      lengthPerTb = lengthPerTb_outer_west ;
    }
  else
    {
      cerr << "Different driftvelocities in TPC observed -> Set to 0. " << endl;
      lengthPerTb = 0;
    }
  
  delete glo ;
  delete padco ;
   
  //  cout << "Constants set by using official transformation." << endl;
}
//______________________________
void St_l3_Coordinate_Transformer::Get_parameters_from_db()
{
  // connect to database
  Double_t driftvelocity = gStTpcDb->DriftVelocity();
  Double_t inner_effective_driftlength = gStTpcDb->Dimensions()->innerEffectiveDriftDistance();
  Double_t outer_effective_driftlength = gStTpcDb->Dimensions()->outerEffectiveDriftDistance();
  Double_t frequency =  gStTpcDb->Electronics()->samplingFrequency();
  Double_t tzero = gStTpcDb->Electronics()->tZero();
    
  cout << "The driftvelocity is set to : " << driftvelocity << endl;
  cout << "The innerdrift is set to    : " << inner_effective_driftlength << endl;
  cout << "The outerdrift is set to    : " << outer_effective_driftlength << endl;
  cout << "The tzero is set to         : " << tzero << endl;
  cout << "Frequency ist set to        : " << frequency << endl;


  lengthPerTb = 1 / frequency / (1E6) * driftvelocity ; // cm per timebucket
  drift_length_inner_west = inner_effective_driftlength + 3 * tzero * (1E-9) * driftvelocity  ;  // cm
  drift_length_outer_west = outer_effective_driftlength + 3 * tzero * (1E-9) * driftvelocity  ;  // cm 
 
  cout << "No distinction between east and west implemented yet !" << endl;

}
//______________________________
void St_l3_Coordinate_Transformer::Print_parameters()
{
    cout << "Used parameters : " << endl ;
    cout << "Length per tb  : " << lengthPerTb << endl ;
    cout << "drift_length_inner_east: " << drift_length_inner_east << endl ;
    cout << "drift_length_outer_east: " << drift_length_outer_east << endl ;
    cout << "drift_length_inner_west: " << drift_length_inner_west << endl ;
    cout << "drift_length_outer_west: " << drift_length_outer_west << endl ;
    cout << endl ;
}

/////////
// init our statics
/////////
Double_t St_l3_Coordinate_Transformer::outerSectorPadPitch = 0.67;     // cm
Double_t St_l3_Coordinate_Transformer::innerSectorPadPitch = 0.335;    // cm

// number of pads in padrow
Int_t St_l3_Coordinate_Transformer::numberOfPadsAtRow[45] = {
    88,96,104,112,118,126,134,142,150,158,166,174,182,
    98,100,102,104,106,106,108,110,112,112,114,116,
    118,120,122,122,124,126,128,128,130,132,134,136,
    138,138,140,142,144,144,144,144 
};
// radial distance (center pad) from detector center in cm
Double_t St_l3_Coordinate_Transformer::radialDistanceAtRow[45] = {
    60.0, 64.8, 69.6, 74.4, 79.2, 84.0, 88.8, 93.60,     //   7 * 4.80 cm spacing
    98.8, 104., 109.20, 114.4, 119.6,                    //   5 * 5.20 cm spacing
    127.195, 129.195, 131.195, 133.195, 135.195,         //  32 * 2.00 cm spacing
    137.195, 139.195, 141.195, 143.195, 145.195,
    147.195, 149.195, 151.195, 153.195, 155.195,
    157.195, 159.195, 161.195, 163.195, 165.195,
    167.195, 169.195, 171.195, 173.195, 175.195,
    177.195, 179.195, 181.195, 183.195, 185.195,
    187.195, 189.195
};
// sector-rotation factors: 30 degree steps
Double_t St_l3_Coordinate_Transformer::SectorSin[24] = {
     0.5,  0.866025404,
     1.0,  0.866025404,
     0.5,  0.,
    -0.5, -0.866025404,
    -1.0, -0.866025404,
    -0.5,  0.,
    -0.5, -0.866025404,
    -1.0, -0.866025404,
    -0.5,  0.,
     0.5,  0.866025404,
     1.0,  0.866025404,
     0.5,  0.,
};
// sector-rotation factors: 30 degree steps
Double_t St_l3_Coordinate_Transformer::SectorCos[24] = {
    0.866025404,  0.5,
     0.,          -0.5,
    -0.866025404, -1.0,
    -0.866025404, -0.5,
     0.,           0.5,
     0.866025404,  1.0,
    -0.866025404, -0.5,
     0.,           0.5,
     0.866025404,  1.0,
     0.866025404,  0.5,
     0.,          -0.5,
    -0.866025404, -1.0,
};
