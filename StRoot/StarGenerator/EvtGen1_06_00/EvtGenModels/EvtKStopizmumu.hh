#ifndef EVTKSTOPIZMUMU_HH
#define EVTKSTOPIZMUMU_HH

#include <string>

#include "EvtGenBase/EvtDecayAmp.hh"

class EvtParticle;

class EvtKStopizmumu : public EvtDecayAmp
{
 public:
  virtual std::string getName()
  { return "KS_PI0MUMU"; }

  virtual EvtDecayBase* clone()
  { return new EvtKStopizmumu; }

  virtual void init();

  virtual void initProbMax()
  { setProbMax(1.0e-10); }

  virtual void decay( EvtParticle *p );
  
  double F_z( const double& z, 
              const double& rvsq );
  EvtComplex G_z( const double& z );
  double Wpol_z( const double& z, 
                 const double& as, 
                 const double& bs );
  EvtComplex chi_z( const double& z, 
                    const double& rpisq );
  EvtComplex Wpipi_z( const double& z, 
                      const double& alpha_s, 
                      const double& beta_s, 
                      const double& rvsq, 
                      const double& rpisq, 
                      const double& z0 );

};

#endif //EVTKTOPIZMUMU_HH
