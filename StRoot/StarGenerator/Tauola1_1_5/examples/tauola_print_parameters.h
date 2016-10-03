#include "Tauola/Tauola.h"
#include "Tauola/f_Variables.h"
#include "Tauola/f_Init.h"
#include "Tauola/f_Decay.h"
#include "Tauola/f_FilHep.h"
#include <iostream>
using std::cout;
using std::endl;
using namespace Tauolapp;

// Prints TAUOLA  parameters (residing inside its library): e.g. to test user interface
void tauola_print_parameters()
{
  int prec = cout.precision(12);
  cout<<"#########################################################################################"<<endl;
  cout<<"Tauola++ parameters set at initialization"<<endl;
  cout<<"(this check-method can be used at any moment of program execution)."<<endl;
  cout<<"Parameters meaning, defaults, and allowed ranges are explained in program documentation "<<endl;
  cout<<"http://www.ph.unimelb.edu.au/~ndavidson/tauola/Tauola_interface_design.pdf "<<endl;
  cout<<endl;
  cout<<"-----------------------------------------------------------"<<endl;
  cout<<endl;
  cout<<"variables used mainly  in C++ interface:  "<<endl;
  cout<<endl;
  cout<<"Tau mass:                                     "<<Tauola::getTauMass()<<endl;
  cout<<"Tau lifetime:                                 "<<Tauola::tau_lifetime<<endl;
  cout<<"Tau PDG id:                                   "<<Tauola::getDecayingParticle()<<endl;
  cout<<endl;
  cout<<"Decay of eta, K0s, pi on/off:                 "<<Tauola::ion[2]<<","<<Tauola::ion[1]<<","<<Tauola::ion[0]<<endl;
  cout<<endl;
  cout<<"Length units:                                 "<< ((Tauola::lengthUnit==Tauola::CM)    ? "CM"  : "MM")  <<endl;
  cout<<"Momentum units:                               "<< ((Tauola::momentumUnit==Tauola::MEV) ? "MEV" : "GEV") <<endl;
  cout<<"Momentum conservation threshold (for checks): "<<Tauola::momentum_conservation_threshold<<endl;
  cout<<endl;
  cout<<"Higgs (scalar-pseudoscalar) PDG id:           "<<Tauola::getHiggsScalarPseudoscalarPDG()<<endl;
  cout<<"Higgs (scalar-pseudoscalar) mixing angle:     "<<Tauola::getHiggsScalarPseudoscalarMixingAngle()<<endl;
  cout<<endl;
  cout<<endl;
  cout<<"Switches used in spin correlation methods on/off for:"<<endl
      <<"GAMMA,Z0,HIGGS,HIGGS_H,HIGGS_A,HIGGS_PLUS,HIGGS_MINUS,W_PLUS,W_MINUS: "
      <<Tauola::spin_correlation.GAMMA<<","<<Tauola::spin_correlation.Z0<<","<<Tauola::spin_correlation.HIGGS<<","<<Tauola::spin_correlation.HIGGS_H<<","
      <<Tauola::spin_correlation.HIGGS_A<<","<<Tauola::spin_correlation.HIGGS_PLUS<<","<<Tauola::spin_correlation.HIGGS_MINUS<<","
      <<Tauola::spin_correlation.W_PLUS<<","<<Tauola::spin_correlation.W_MINUS<<endl;
  cout<<endl;
  cout<<"-----------------------------------------------------------"<<endl;
  cout<<endl;
  cout<<"variables used in tau decays : "<<endl;
  cout<<endl;
  cout<<"-----------------------------------------------------------"<<endl;
  cout<<endl;

  cout<<"tau+ decay mode:                                               "<<jaki_.jak1<<endl;
  cout<<"tau- decay mode:                                               "<<jaki_.jak2<<endl;
  cout<<endl;
  cout<<"a1     relative branching ratio pi- pi- pi+ / pi- pi pi0:      "<<taukle_.bra1<<endl;
  cout<<"K0     relative attribute K0_L / K0_S:                         "<<taukle_.brk0<<endl;
  cout<<"K0_bar relative attribute K0_L / K0_S:                         "<<taukle_.brk0b<<endl;
  cout<<"Kstar  relative branching ratio K0 pi- /K- pi0 :               "<<taukle_.brks<<endl;
  cout<<endl;
  cout<<"QED radiation in leptonic decays on/off:                       "<<taurad_.itdkrc<<endl;
  cout<<"QED radiation in leptonic decays minimal photon energy cutoff: "<<taurad_.xk0dec<<endl;
  cout<<endl;
  cout<<"#########################################################################################"<<endl;

  cout.precision(prec);
}
