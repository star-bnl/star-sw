/*
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
*/

#define innerSectorPadPitch     0.335    /* cm */ 
#define outerSectorPadPitch     0.67     /* cm */
#define driftLength             208.     /* cm */
#define lengthPerTb             0.5977   /* = 208/348 */


/* number of pads in padrow */
static short numberOfPadsAtRow[45] = {
    88,96,104,112,118,126,134,142,150,158,166,174,182,
    98,100,102,104,106,106,108,110,112,112,114,116,
    118,120,122,122,124,126,128,128,130,132,134,136,
    138,138,140,142,144,144,144,144 
};

/* radial distance (center pad) from detector center in cm */
static double radialDistanceAtRow[45] = {
    60.0, 64.8, 69.6, 74.4, 79.2, 84.0, 88.8, 93.60,     /*   7 * 4.80 cm spacing  */
    98.8, 104., 109.20, 114.4, 119.6,                    /*   5 * 5.20 cm spacing */
    127.195, 129.195, 131.195, 133.195, 135.195,         /*  32 * 2.00 cm spacing */
    137.195, 139.195, 141.195, 143.195, 145.195,
    147.195, 149.195, 151.195, 153.195, 155.195,
    157.195, 159.195, 161.195, 163.195, 165.195,
    167.195, 169.195, 171.195, 173.195, 175.195,
    177.195, 179.195, 181.195, 183.195, 185.195,
    187.195, 189.195
};

/* sector-rotation factors: 30 degree steps  */
static double SectorSin[24] = {
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

static double SectorCos[24] = {
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

int rawToLocal ( int row, double pad, double tb,
			double *xLo, double *yLo, double *zLo) {

  double *xLocal, *yLocal, *zLocal;
  double pitch;
  double pads2move;
  xLocal = xLo;
  yLocal = yLo;
  zLocal = zLo;

  pitch = (row<14) ? innerSectorPadPitch : outerSectorPadPitch;

  pads2move = pad - numberOfPadsAtRow[row-1]/2;
  *xLocal = pitch * (pads2move-.5);
  *yLocal = radialDistanceAtRow[row-1];

  /*
    printf("local x,y coordinates: %f,  %f\n", *xLocal, *yLocal);

  // z coordinate
  // needs access to db probably
  //     Brian's version (see TRS lib):
  //     double z = frischGrid - driftVelocity * (tZero + tb*timebinWidth); 
  */
  *zLocal = driftLength - tb*lengthPerTb;

  /*printf("z coordinate: %f\n", *zLocal); */
  return 0;
}

int localToGlobal ( int sector, double xLocal, double yLocal, double zLocal,
			   double *xc, double *yc, double *zc ) {

    int eastsector;
    double* x; double* y; double* z;
    x = xc ; y = yc ; z = zc;
    /* rotate local x,y coordinates */
    /* 2x2 rotation matrix: */
    /*   ( cos b    sin b ) */
    /*   (-sin b    cos b ) */
    *x = SectorCos[sector-1] * xLocal + SectorSin[sector-1] * yLocal;
    /* caution: sector>12 needs x->-x and y->y (east side!) */
    /* ==> set sector to sector-12 */
  eastsector = (sector>12) ? sector-12 : sector;
  *y = -1.*SectorSin[eastsector-1] *xLocal + SectorCos[eastsector-1] * yLocal;
  *z = (sector<13) ? zLocal : -zLocal ;
  return 0;
}



int rawToGlobal ( int sector, int row, double pad, double tb,
			 double *xc, double *yc, double *zc) {

  double xLocal, yLocal, zLocal ;
  double* x; double* y; double* z;
  x = xc ; y = yc ; z = zc;
  rawToLocal ( row, pad, tb, &xLocal, &yLocal, &zLocal ) ;
  localToGlobal ( sector, xLocal, yLocal, zLocal, x, y, z ) ;   
  return 0;
}
