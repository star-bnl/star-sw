#include "StEfficiency.h"
#include <fstream.h>
#include "StSpectraCutNhit.h"
#include "StSpectraCutDCA.h"
#include "StGetConfigValue.hh"

StEfficiency::StEfficiency(){
  //
  // for better design should call working constructor with default arguments
  //
}

StEfficiency::StEfficiency(efficiencyType efficType, char* efficFile) {
  //
  // create a container of StSpectraCut
  // each element is one of several derived cuts
  // read these cuts from file
  //

  int *nhitRange = new int[2];
  StGetConfigValue(efficFile, "nhit", nhitRange, 2);
  cout << "nhit "<< nhitRange[0] << " " << nhitRange[1] << endl;
  mSpectraCutContainer.
    push_back(new StSpectraCutNhit(nhitRange[0],nhitRange[1]));

  double * dcaRange = new double[2];  
  StGetConfigValue(efficFile, "dca", dcaRange, 2);
  cout << "dca " << dcaRange[0] << " " << dcaRange[1] << endl;
  mSpectraCutContainer.
    push_back(new StSpectraCutDCA(dcaRange[0], dcaRange[1]));

  if (efficType == FUNCTION) {
     mMomentumTerm = 0.04;
     mScale = 0.96;

  } else if (efficType == HISTOGRAM) {
    //
    // read histogram from file and copy it into mEfficHist
    // not implemented
  } else {
    cout << "not a known type of efficieincy " << endl;
    // throw an exception?
  }  

}
StEfficiency::~StEfficiency() {
}

void StEfficiency::setParticle(string particle) {
  mParticle = StParticleTable::instance()->findParticle(particle) ;
}

double StEfficiency::efficiency(StTrack* track) {

 StThreeVectorD mom = track->geometry()->momentum();
 double p = abs(mom);
 double mass = mParticle->mass();
 double e = sqrt(p*p + mass*mass);
 double beta = p/e ;
 double effic = mScale*(1-mMomentumTerm/pow(beta*p,2));
 // cout << mScale << " " << mMomentumTerm << " " << effic << endl;
return effic;
}
