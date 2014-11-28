#ifndef STHBT_GAMOVCORRECT
#define STHBT_GAMOVCORRECT


#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtPair.hh"
#include "PhysicalConstants.h"
//#include <cstdio>


double gamovCorrect(const StHbtPair* pair,
		    const double& charge) {
  static double px1,py1,pz1,px2,py2,pz2;
  static double px1new,py1new,pz1new;
  static double px2new,py2new,pz2new;
  static double vx1cms,vy1cms,vz1cms;
  static double vx2cms,vy2cms,vz2cms;
  static double VcmsX,VcmsY,VcmsZ;
  static double dv = 0.0;
  static double e1,e2,e1new,e2new;
  static double psi,theta;
  static double beta,gamma;
  static double VcmsXnew;

  // G = 2*pi*(eta)/(Exp(2*pi*(eta))-1)
  // eta = Z1*Z2*e^2/((h_bar)*c* Vel(rel) )
  px1 = pair->track1()->FourMomentum().px();
  py1 = pair->track1()->FourMomentum().py();
  pz1 = pair->track1()->FourMomentum().pz();
  e1 = pair->track1()->FourMomentum().e();
  px2 = pair->track2()->FourMomentum().px();
  py2 = pair->track2()->FourMomentum().py();
  pz2 = pair->track2()->FourMomentum().pz();
  e2 = pair->track2()->FourMomentum().e();
  
  VcmsX = ( px1+px2 )/( e1+e2 );
  VcmsY = ( py1+py2 )/( e1+e2 );
  VcmsZ = ( pz1+pz2 )/( e1+e2 );
  // Rotate Vcms to x-direction
  psi = atan(VcmsY/VcmsX);
  VcmsXnew = VcmsX*cos(psi)+VcmsY*sin(psi);
  VcmsX = VcmsXnew;
  theta = atan(VcmsZ/VcmsX);
  VcmsXnew = VcmsX*cos(theta)+VcmsZ*sin(theta);
  VcmsX = VcmsXnew;
  // Gamma and Beta
  beta = VcmsX;
  gamma = 1.0/::sqrt( 1.0-beta*beta );

  // Rotate p1 and p2 to new frame
  px1new = px1*cos(psi)+py1*sin(psi);
  py1new = -px1*sin(psi)+py1*cos(psi);
  px1 = px1new;
  px1new = px1*cos(theta)+pz1*sin(theta);
  pz1new = -px1*sin(theta)+pz1*cos(theta);
  px1 = px1new;
  py1 = py1new;
  pz1 = pz1new;

  px2new = px2*cos(psi)+py2*sin(psi);
  py2new = -px2*sin(psi)+py2*cos(psi);
  px2 = px2new;
  px2new = px2*cos(theta)+pz2*sin(theta);
  pz2new = -px2*sin(theta)+pz2*cos(theta);
  px2 = px2new;
  py2 = py2new;
  pz2 = pz2new;

  // Lorentz transform the x component and energy
  e1new = gamma*e1 - gamma*beta*px1;
  px1new = -gamma*beta*e1 + gamma*px1;
  e2new = gamma*e2 - gamma*beta*px2;
  px2new = -gamma*beta*e2 + gamma*px2;
  px1 = px1new;
  px2 = px2new;

  // New velocities
  vx1cms = px1/e1new;
  vy1cms = py1/e1new;
  vz1cms = pz1/e1new;
  vx2cms = px2/e2new;
  vy2cms = py2/e2new;
  vz2cms = pz2/e2new;

  // Velocity difference in CMS frame
  dv = ::sqrt( (vx1cms-vx2cms)*(vx1cms-vx2cms) +
             (vy1cms-vy2cms)*(vy1cms-vy2cms) +
             (vz1cms-vz2cms)*(vz1cms-vz2cms) );
  
  // Assumes same charge particles
  double eta = charge*fine_structure_const/( dv );
  double gamov = twopi*eta/(exp(twopi*eta)-1);
  return (gamov);
}

#endif
