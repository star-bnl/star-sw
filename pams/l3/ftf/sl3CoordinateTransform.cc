// Numbering convetion (see DAQ Data Format Document)
//   all sequences start with number '1', e.g. sector 1-24,
//   row 1-45
#include "sl3GeoConstants.h"

int rawToLocal ( int row, double pad, double tb,
	         double& x, double& y, double& z) {
  // transform to local x,y coordinates first

  double pitch = padSpacing[row-1];
  double pads2move = pad - nPadsInRow[row-1]/2;

  x = pitch * (pads2move-.5);
  y = padrowOffset[row-1];

  // z coordinate
  // needs access to db probably
  //     Brian's version (see TRS lib):
  //     double z = frischGrid - driftVelocity * (tZero + tb*timebinWidth);
  z = driftLength - tb*lengthPerTb ;

  return 0;
}

int localToGlobal ( int sector,
                    double& xLocal, double& yLocal, double& zLocal,
		    double& x,      double& y,      double& z) {

  //printf("local x,y coordinates: %f,  %f\n", xlocal, ylocal);

  // z coordinate
  // needs access to db probably
  //     Brian's version (see TRS lib):
  //     double z = frischGrid - driftVelocity * (tZero + tb*timebinWidth);
  z = (sector<13) ? zLocal : -zLocal ;


  // rotate local x,y coordinates
  // 2x2 rotation matrix:
  //   ( cos b    sin b )
  //   (-sin b    cos b )
  x = SectorCos[sector-1] * xLocal + SectorSin[sector-1] * yLocal;
  // caution: sector>12 needs x->-x and y->y (east side!)
  // ==> set sector to sector-12
  int eastsector = (sector>12) ? 
          sector-12 : sector;
  y = -1.*SectorSin[eastsector-1] *xLocal + SectorCos[eastsector-1] * yLocal;

  return 0;
}
int rawToGlobal ( int sector, int row, double pad, double tb,
		  double& x, double& y, double& z) {
  double xLocal, yLocal, zLocal ;
  rawToLocal ( row, pad, tb, xLocal, yLocal, zLocal ) ;
  localToGlobal ( sector, xLocal, yLocal, zLocal, x, y, z ) ;   
  return 0;
}
