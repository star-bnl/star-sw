// -*- mode: c++;-*-
// $Id: StjEtaToDetectorEta.h,v 1.2 2008/08/03 00:28:58 tai Exp $
#ifndef STJETATODETECTORETA_H
#define STJETATODETECTORETA_H

#include <cmath>

class StjEtaToDetectorEta {

public:
  StjEtaToDetectorEta(double BEMCy = 222.625, double EEMCz = 270.0)
    : _BEMCy(BEMCy), _EEMCz(EEMCz) { }
  virtual ~StjEtaToDetectorEta() { }

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

#endif // STJETATODETECTORETA_H

