#include <stdio.h>
#include <iostream.h>
#include <strings.h>
#include <sys/types.h>
#include <stdlib.h>
#include <Rtypes.h> /* use ROOT variables: ..._t */
#include "St_l3_Coord_Trans.h"

ClassImp(St_l3_Coord_Trans);


//______________________________
St_l3_Coord_Trans::St_l3_Coord_Trans()
{
    // allocate memory for varialbes
    xyz_local = new Double_t[3];
    xyz_global = new Double_t[3];
}
//______________________________
St_l3_Coord_Trans::~St_l3_Coord_Trans()
{
    delete [] xyz_local;
    delete [] xyz_global;
}
//______________________________
inline void St_l3_Coord_Trans::raw_to_local()
{
  /*Double_t pad =  pa_ti_ro_se[0];
  Double_t tb  =  pa_ti_ro_se[1];
  Int_t row    =  (Int_t) pa_ti_ro_se[2];

  Double_t  pitch = (row<14) ?
      innerSectorPadPitch : outerSectorPadPitch;

  Double_t pads2move = pad - numberOfPadsAtRow[row-1]/2;
  xyz_local[0] = pitch * (pads2move-.5);
  xyz_local[1] = radialDistanceAtRow[row-1];
  xyz_local[2] = driftLength - tb*lengthPerTb;*/

    Int_t row    =  (Int_t) pa_ti_ro_se[2];
    Double_t  pitch = (pa_ti_ro_se[2]<14) ?

    innerSectorPadPitch : outerSectorPadPitch;
  
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
inline void St_l3_Coord_Trans::local_to_global()
{
    Int_t sector = (Int_t) pa_ti_ro_se[3];
    xyz_global[0] = SectorCos[sector-1] * xyz_local[0] + SectorSin[sector-1] * xyz_local[1] ;
    Int_t eastsector = (sector>12) ? sector-12 : sector;
    xyz_global[1] = -1.*SectorSin[eastsector-1] * xyz_local[0] + SectorCos[eastsector-1] * xyz_local[1] ;
    xyz_global[2] =  (sector<13) ? xyz_local[2] : -xyz_local[2] ;

 }
//______________________________
Double_t* St_l3_Coord_Trans::raw_to_global(Double_t* Pa_Ti_Ro_Se)
{
    pa_ti_ro_se = Pa_Ti_Ro_Se;
    raw_to_local();
    local_to_global();
    return xyz_global;
}

/////////
// init our statics
/////////
Double_t St_l3_Coord_Trans::lengthPerTb =  0.5977;          // = 208/348
Double_t St_l3_Coord_Trans::driftLength =  208;             // cm
Double_t St_l3_Coord_Trans::outerSectorPadPitch = 0.67;     // cm
Double_t St_l3_Coord_Trans::innerSectorPadPitch = 0.335;    // cm

// number of pads in padrow
Int_t St_l3_Coord_Trans::numberOfPadsAtRow[45] = {
    88,96,104,112,118,126,134,142,150,158,166,174,182,
    98,100,102,104,106,106,108,110,112,112,114,116,
    118,120,122,122,124,126,128,128,130,132,134,136,
    138,138,140,142,144,144,144,144 
};
// radial distance (center pad) from detector center in cm
Double_t St_l3_Coord_Trans::radialDistanceAtRow[45] = {
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
Double_t St_l3_Coord_Trans::SectorSin[24] = {
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
Double_t St_l3_Coord_Trans::SectorCos[24] = {
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
