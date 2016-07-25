/// Bethe-Bloch constant in GeVg^-1cm^2
const double _k   = 0.307e-3;

/// Electron mass in GeV/c^2
const double _mec = 0.510998e-3;
double elossCalculator(double z2=1, double zOverA=0.5, double m=0.13956995, double beta2=16./17., double ionization2=8.0883e-14 ) const
{
  if (beta2>=1. || beta2<0) return 0;
  //    throw runtime_error("StiElossCalculator::calculate() - ERROR - beta2==1");
  double gamma2 = 1./(1-beta2);
  double gamma  = ::sqrt(gamma2);
  double massRatio = _mec/m;
  double tMax = 2.*_mec*beta2*gamma2/(1.+2*gamma*massRatio+massRatio*massRatio);

  return _k*z2*zOverA*(0.5*::log(_mec*beta2*gamma2*tMax/ionization2)-beta2)/beta2;
}
