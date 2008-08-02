// -*- mode: c++;-*-
// $Id: StjEtaToDetectorEta.h,v 1.1 2008/08/02 04:04:26 tai Exp $
#ifndef ETATODETECTORETA_H
#define ETATODETECTORETA_H

#include <cmath>

namespace StSpinJet {

class EtaToDetectorEta {

public:
  EtaToDetectorEta(double BEMCy = 222.625, double EEMCz = 270.0)
    : _BEMCy(BEMCy), _EEMCz(EEMCz) { }
  virtual ~EtaToDetectorEta() { }

  double operator()(double eta, double z) 
  {
    double thetaD;
    if(fabs(eta) < 1e-5) {
      if(fabs(z) < 1e-5) return 0.0;
      thetaD = atan2(_BEMCy, z);
    } else {
      double tanTh =tan(2*atan(exp(-eta)));
      if(tanTh > 0 && tanTh*(_EEMCz - z) < _BEMCy) { // ENDCAP
	thetaD = atan2(tanTh*(_EEMCz - z), _EEMCz);
      } else { // BARREL
	thetaD = atan2(_BEMCy, (z + _BEMCy/tanTh));
      }
    }
    double deta = -log(fabs(tan(thetaD/2)));
    return (thetaD > 0) ? deta : -deta;
  }

private:

  double _BEMCy;
  double _EEMCz;

};

}


#endif // ETATODETECTORETA_H

