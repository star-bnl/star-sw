// Global functions and objects required by Heed

#ifndef G_TRACK_HEED_GLOBALS
#define TRACK_HEED_GLOBALS

#include "wcpplib/geometry/gparticle.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"

#include "heed++/code/HeedCluster.h"

#include "Sensor.hh"

namespace Garfield {

namespace HeedInterface {

  Sensor* sensor;
  Medium* medium;
  
  bool useEfield;
  bool useBfield;

}

}

BlkArr<HeedCluster> cluster_bank;
AbsList<ActivePtr<gparticle> > particle_bank;

void 
field_map(const point& pt, vec& efield, vec& bfield, vfloat& mrange) {

  const double x = pt.v.x;
  const double y = pt.v.y;
  const double z = pt.v.z;

  // Initialise the electric and magnetic field.  
  efield = vec(0., 0., 0.);
  bfield = vec(0., 0., 0.);
  mrange = DBL_MAX;
  
  if (Garfield::HeedInterface::sensor == 0) {
    std::cerr << "TrackHeedGlobals::field_map:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }

  // TODO: check correct dimensions of E and B fields  
  if (Garfield::HeedInterface::useEfield) {
    double ex = 0., ey = 0., ez = 0.;
    int status = 0;
    Garfield::HeedInterface::sensor->ElectricField(
        x, y, z, ex, ey, ez, 
        Garfield::HeedInterface::medium, status);
    efield.x = ex * 1.e-5; 
    efield.y = ey * 1.e-5; 
    efield.z = ez * 1.e-5;
  }
  
  if (Garfield::HeedInterface::useBfield) {
    double bx = 0., by = 0., bz = 0.;
    int status = 0;
    Garfield::HeedInterface::sensor->MagneticField(x, y, z, 
                                                   bx, by, bz, 
                                                   status);
    bfield.x = bx; bfield.y = by; bfield.z = bz;
  }

}

void 
check_point(gparticle* gp) { }

long last_particle_number;

extern trajestep_limit gtrajlim;
trajestep_limit gtrajlim(100.*cm, 1000.*cm, 0.1*rad, 0.2* rad);

double mparticle::speed_of_light=c_light;

#endif
