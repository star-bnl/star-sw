//  sl3CoordinateTransform.h
//
//  Christof Struck, struck@star.physics.yale.edu
//
//  Okt. 08 1999
//
//  fast coordinate transformation: rawToLocal, localToGlobal
//  and rawToGlobal
//

// Numbering convetion (see DAQ Data Format Document)
//   all sequences start with number '1', e.g. sector 1-24,
//   row 1-45

#include "l3GeneralHeaders.h"
#ifndef SL3COORDINATETRANSFORM
#define SL3COORDINATETRANSFORM


#define innerSectorPadPitch     0.335    // cm
#define outerSectorPadPitch     0.67     // cm
#define driftLength             208.     // cm
#define lengthPerTb             0.5977   // = 208/348


// number of pads in padrow
static short numberOfPadsAtRow[45] = {
    88,96,104,112,118,126,134,142,150,158,166,174,182,
    98,100,102,104,106,106,108,110,112,112,114,116,
    118,120,122,122,124,126,128,128,130,132,134,136,
    138,138,140,142,144,144,144,144 
};

// radial distance (center pad) from detector center in cm
static double radialDistanceAtRow[45] = {
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
static double SectorSin[NSECTORS] = {
     0.5,  0.866025404, // 1-2
     1.0,  0.866025404, // 3-4
     0.5,  0.,          // 5-6
    -0.5, -0.866025404, // 7-8 
    -1.0, -0.866025404, // 9-10
    -0.5,  0.,  
    -0.5, -0.866025404,
    -1.0, -0.866025404,
    -0.5,  0.,
     0.5,  0.866025404,
     1.0,  0.866025404,
     0.5,  0.,
};

static double SectorCos[NSECTORS] = {
     0.866025404,  0.5, // 1-2
     0.,          -0.5, // 3-4
    -0.866025404, -1.0, // 5-6
    -0.866025404, -0.5, // 7-8
     0.,           0.5, // 9-10
     0.866025404,  1.0, // 11-12
     0.866025404,  0.5, // 13-14
     0.,          -0.5, // 15-16
    -0.866025404, -1.0, // 17-18
    -0.866025404, -0.5, // 19-20
     0.,           0.5, // 21-22
     0.866025404,  1.0, // 23-24
};
/*
    -0.866025404, -0.5, ! 13-14
     0.,           0.5, ! 15-16
     0.866025404,  1.0, ! 17-18
     0.866025404,  0.5, ! 19-20
     0.,          -0.5, ! 21-22
    -0.866025404, -1.0, ! 23-24
*/

inline int rawToLocal ( int row, double pad, double tb,
			double *xLocal, double *yLocal, double *zLocal) {

  double pitch = (row<14) ?
          innerSectorPadPitch : outerSectorPadPitch;

  double pads2move = pad - numberOfPadsAtRow[row-1]/2;
  *xLocal = pitch * (pads2move-.5);

  *yLocal = radialDistanceAtRow[row-1];

  //printf("local x,y coordinates: %f,  %f\n", *xLocal, *yLocal);

  // z coordinate
  // needs access to db probably
  //     Brian's version (see TRS lib):
  //     double z = frischGrid - driftVelocity * (tZero + tb*timebinWidth);
  *zLocal = driftLength - tb*lengthPerTb;

  //printf("z coordinate: %f\n", *zLocal);
  return 0;
}

inline int localToRaw ( int row, 
			double xLocal, double yLocal, double zLocal,  
                        double& pad, double& tb ) {

  double pitch = (row<14) ?
          innerSectorPadPitch : outerSectorPadPitch;

  double pad2move = xLocal / pitch + .5 ;
  pad             = pad2move + numberOfPadsAtRow[row-1]/2; 
  tb              = (driftLength - zLocal) / lengthPerTb ;
  return 0;
}

inline int localToGlobal ( int sector, double xLocal, double yLocal, double zLocal,
			   double *x, double *y, double *z ) {

  // rotate local x,y coordinates
  // 2x2 rotation matrix:
  //   ( cos b    sin b )
  //   (-sin b    cos b )
  *x = SectorCos[sector-1] * xLocal + SectorSin[sector-1] * yLocal;
  // caution: sector>12 needs x->-x and y->y (east side!)
  // ==> set sector to sector-12
  int eastsector = (sector>12) ? 
          sector-12 : sector;
//*y = -1.*SectorSin[eastsector-1] *xLocal + SectorCos[eastsector-1] * yLocal;
  *y = -1.*SectorSin[sector-1] *xLocal + SectorCos[sector-1] * yLocal;
  *z = (sector<13) ? zLocal : -zLocal ;
  return 0;
}

inline int globalToLocal ( int sector, int row,  
                           double xGlobal, double yGlobal, double zGlobal,
			   double &xLocal, double &yLocal, double &zLocal ) {

  xLocal = SectorCos[sector-1] * xGlobal - SectorSin[sector-1] * yGlobal ;
  yLocal = radialDistanceAtRow[row-1];
  zLocal = fabs(zGlobal) ;
  return 0;
}

inline int rawToGlobal ( int sector, int row, double pad, double tb,
			 double *x, double *y, double *z) {

  double xLocal, yLocal, zLocal ;
  rawToLocal ( row, pad, tb, &xLocal, &yLocal, &zLocal ) ;
  localToGlobal ( sector, xLocal, yLocal, zLocal, x, y, z ) ;   
  return 0;
}

#endif
