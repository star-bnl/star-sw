/***************************************************************************
 *
 * $Id: exi_c_utils.cc,v 1.2 2003/01/24 21:30:21 genevb Exp $
 *
 * Author: Gene Van Buren, BNL, Nov 2000
 ***************************************************************************
 *
 * Description:  C/C++ based utilities for EXI
 *
 ***************************************************************************
 *
 * $Log: exi_c_utils.cc,v $
 * Revision 1.2  2003/01/24 21:30:21  genevb
 * Fix Xi helix DCA
 *
 * Revision 1.1  2000/11/28 22:01:07  genevb
 * Use 3D DCA for bach & Xi, check for unphysical geom
 *
 *
 **************************************************************************/
#include "math_constants.h"
#include "phys_constants.h"
#include "StThreeVectorD.hh"
#include "StHelixD.hh"
#include "TMath.h"

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
  double pt        = TMath::Sqrt(p[0]*p[0] + p[1]*p[1]);
  double bcharge   = (*charge)*bfield;
  double curvature = TMath::Abs(bcharge)*C_D_CURVATURE/pt;
  double dip       = TMath::ATan(p[2]/pt);
  int    h         = ((bcharge > 0) ? -1 : 1);
  double phase     = TMath::ATan2(p[1],p[0]) - (h*C_PI_2);
  origin.setX(x[0]);
  origin.setY(x[1]);
  origin.setZ(x[2]);
  StHelixD globHelix(curvature, dip, phase, origin, h);
  (*dca) = (float) globHelix.distance(primVertx);
  StThreeVectorD p1 = globHelix.at(globHelix.pathLength(primVertx));
  StThreeVectorD p2(p1.x()-globHelix.xcenter(),p1.y()-globHelix.ycenter(),0);
  StThreeVectorD p3(primVertx.x()-globHelix.xcenter(),primVertx.y()-globHelix.ycenter(),0);
  if (p3.mag2() > p2.mag2()) (*dca) = -(*dca);
}                                                                         
