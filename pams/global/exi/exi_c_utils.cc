/***************************************************************************
 *
 * $Id: exi_c_utils.cc,v 1.1 2000/11/28 22:01:07 genevb Exp $
 *
 * Author: Gene Van Buren, BNL, Nov 2000
 ***************************************************************************
 *
 * Description:  C/C++ based utilities for EXI
 *
 ***************************************************************************
 *
 * $Log: exi_c_utils.cc,v $
 * Revision 1.1  2000/11/28 22:01:07  genevb
 * Use 3D DCA for bach & Xi, check for unphysical geom
 *
 *
 **************************************************************************/
#include "math_constants.h"
#include "phys_constants.h"
#include "StThreeVectorD.hh"
#include "StHelixD.hh"

#ifndef __CINT__
#include "StarCallf77.h"
#define setPrimVert_ F77_NAME(setprimvert,SETPRIMVERT)
#define setBfield_ F77_NAME(setbfield,SETBFIELD)
#define helixDCA_ F77_NAME(helixdca,HELIXDCA)
extern "C" {
void type_of_call setPrimVert_(float* x);
void type_of_call setBfield_(float* b);
void type_of_call helixDCA_(float* charge, float* x, float* p, float* dca);
}
#endif

StThreeVectorD primVertx;
StThreeVectorD origin;
float bfield;

void type_of_call setPrimVert_(float* x) {
  primVertx.setX(x[0]);
  primVertx.setY(x[1]);
  primVertx.setZ(x[2]);
}

void type_of_call setBfield_(float* b) {
  bfield = *b;
}

void type_of_call helixDCA_(float* charge, float* x, float* p, float* dca) {
  double pt        = sqrt(p[0]*p[0] + p[1]*p[1]);
  double bcharge   = (*charge)*bfield;
  double curvature = bcharge*C_D_CURVATURE/pt;
  double dip       = atan(p[2]/pt);
  int    h         = ((bcharge > 0) ? -1 : 1);
  double phase     = atan2(p[1],p[0]) - (h*C_PI_2);
  origin.setX(x[0]);
  origin.setY(x[1]);
  origin.setZ(x[2]);
  StHelixD globHelix(curvature, dip, phase, origin, h);
  (*dca) = (float) globHelix.distance(primVertx);
}                                                                         
