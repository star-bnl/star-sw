//
// First pass - simply exercise all the vector methods
//
#include <iostream>

#include "HepMC/SimpleVector.h"

int main() 
{
  // ThreeVector
  HepMC::ThreeVector vector3;
  HepMC::ThreeVector v3(1.1,2.2,3.3);
  HepMC::ThreeVector vx(1.34);
  
  HepMC::ThreeVector v3copy( v3 );
   
  double eps = 1.e-15; // allowed differnce between doubles
  int numbad = 0;
 
  double x = v3.x();
  double y = v3.y();
  double z = v3.z();
  double p2 = v3.perp2();
  double pt = v3.perp();
  double r = v3.r();
  double th = v3.theta();
  double ph = v3.phi();
  double mag = std::sqrt(x*x + y*y + z*z);
  double pperp = std::sqrt(x*x + y*y);

  vx.set(1., 2., 3.);
  vx.setX(1.1);
  vx.setY(2.3);
  vx.setZ(4.4);
  vx.setPhi(0.12);
  vx.setTheta(0.54);
  
  vector3 = v3;

  if( fabs( mag - r ) > eps ) { 
     std::cout << "different ThreeVector magnitude: " << mag << " " << r << std::endl;
     std::cout << "difference is : " << ( mag - r ) << std::endl;
     ++numbad;
  }
  if( fabs( pperp - pt ) > eps ) { 
     std::cout << "different ThreeVector Pt: " << pperp << " " << pt << std::endl;
     std::cout << "difference is : " << ( pperp - pt ) << std::endl;
     ++numbad;
  }

  if( v3 == vector3 ) {
  } else {
     ++numbad;
     std::cout << "vectors v3 and vector3 are different" << std::endl;
  }
  if( v3 != v3copy ) {
     ++numbad;
     std::cout << "vectors v3 and v3copy are different" << std::endl;
  }
 
  // FourVector
  HepMC::FourVector vector;
  HepMC::FourVector v4(1.1,2.2,3.3,4.4);
  HepMC::FourVector vt(1.34);
  
  HepMC::FourVector vectorcopy( v4 );
  vector = v4;
  
  double px = v4.px();
  double py = v4.py();
  double pz = v4.pz();
  double e  = v4.e();
   x = vectorcopy.x();
   y = vectorcopy.y();
   z = vectorcopy.z();
  double t = vectorcopy.t();
  
   p2 = v4.perp2();
   pt = v4.perp();
   th = v4.theta();
   ph = v4.phi();
   r = v4.rho();
  double masssq1 = v4.m2();
  double mass1 = v4.m();
  double pr1 = v4.pseudoRapidity();
  double eta1 = v4.eta();
  double masssq2 = vector.m2();
  double mass2 = vector.m();
  double pr2 = vector.pseudoRapidity();
  double eta2 = vector.eta();

  vt.set(1., 2., 3., 5.5);
  vt.setX(1.1);
  vt.setY(2.3);
  vt.setZ(4.4);
  vt.setT(6.5);
  vt.setPx(3.1);
  vt.setPy(2.2);
  vt.setPz(-1.1);
  vt.setE(5.4);

  mag = std::sqrt(x*x + y*y + z*z);
  pperp = std::sqrt(x*x + y*y);
  if( fabs( mag - r ) > eps ) { 
     std::cout << "different FourVector magnitude: " << mag << " " << r << std::endl;
     std::cout << "difference is : " << ( mag - r ) << std::endl;
     ++numbad;
  }
  if( fabs( pperp - pt ) > eps ) { 
     std::cout << "different FourVector Pt: " << pperp << " " << pt << std::endl;
     std::cout << "difference is : " << ( pperp - pt ) << std::endl;
     ++numbad;
  }

  if( px != x ) { 
     std::cout << "different X values: " << px << " " << x << std::endl;
     ++numbad;
  }
  if( py != y ) { 
     std::cout << "different Y values: " << py << " " << y << std::endl;
     ++numbad;
  }
  if( pz != z ) { 
     std::cout << "different Z values: " << pz << " " << z << std::endl;
     ++numbad;
  }
  if( e != t ) { 
     std::cout << "different E values: " << e << " " << t << std::endl;
     ++numbad;
  }
  if( fabs( masssq1 - masssq2 ) > eps ) { 
     std::cout << "different mass sq values: " << masssq1 << " " << masssq2 << std::endl;
     std::cout << "difference is : " << ( masssq1 - masssq2 ) << std::endl;
     ++numbad;
  }
  if( fabs( mass1 - mass2 ) > eps ) { 
     std::cout << "different mass values: " << mass1 << " " << mass2 << std::endl;
     std::cout << "difference is : " << ( mass1 - mass2 ) << std::endl;
     ++numbad;
  }
  if( fabs( pr1 - pr2 ) > eps ) { 
     std::cout << "different pseudorapidity values: " << pr1 << " " << pr2 << std::endl;
     std::cout << "difference is : " << ( pr1 - pr2 ) << std::endl;
     ++numbad;
  }
  if( fabs( eta1 - eta2 ) > eps ) { 
     std::cout << "different eta values: " << eta1 << " " << eta2 << std::endl;
     std::cout << "difference is : " << ( eta1 - eta2 ) << std::endl;
     ++numbad;
  }
  if( v4 == vector ) {
  } else {
     std::cout << "vectors v and vector are different" << std::endl;
     ++numbad;
  }
  if( v4 != vectorcopy ) {
     std::cout << "vectors v and vectorcopy are different" << std::endl;
     ++numbad;
  }

  return numbad;
}
