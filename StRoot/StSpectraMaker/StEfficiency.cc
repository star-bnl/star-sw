#include "StEfficiency.h"
#include <fstream.h>
#include "StSpectraCutNhit.h"
#include "StSpectraCutDCA.h"

StEfficiency::StEfficiency(){

}

StEfficiency::StEfficiency(efficiencyType efficType, char* efficFile) {
  //
  // create a container of StSpectraCut
  // each element is one of several derived cuts
  // read these cuts from file
  //
  ifstream efrom(efficFile);
  if (efrom.bad()) {
    cout << "problem opening file " << efficFile << endl;
    // thow an excpetion ?
  } 
  //
  // read cuts from file and fill container of cuts
  // to do, enumerate types of cuts for error checking
  //
  string cut;
  do {
    efrom >> cut ;
    if (cut == "nhit") {
      int nHitLow, nHitHigh;
      efrom >> nHitLow >> nHitHigh ;
      mSpectraCutContainer.push_back(new StSpectraCutNhit(nHitLow,nHitHigh));
    } else if (cut == "dca") {
      double dcaLow, dcaHigh;
      efrom >> dcaLow >> dcaHigh ;
      mSpectraCutContainer.push_back(new StSpectraCutDCA(dcaLow, dcaHigh));
    }
  } while ( cut!= "end");

  if (efficType == function){
     double scale;
     double momentumTerm;
     efrom >> scale >> momentumTerm;
     mMomentumTerm = momentumTerm;
     mScale = scale;

  } else if (efficType == histogram) {
    //
    // read histogram from file and copy it into mEfficHist
    // not implemented
  } else {
    cout << "not a known type of efficieincy " << endl;
    // throw an exception?
  }  
  efrom.close();
}

StEfficiency::~StEfficiency() {
}
istream& operator>>(istream& is, efficiencyType& efficType) {
  string word;
  is >> word;
  if (word == "function") {
    efficType = function;
  } else if (word == "histogram") {
    efficType = histogram;
  } else {
    cout << "error in type of efficiency correction" << endl;
    // throw an exception?
  }
  return is;
}
    

istream& operator>>(istream& is, StEfficiency& effic) {
  //
  // in file have "type of track cut", lower, upper range
  // e.g. nhit  10 1000
  // use this to instantiate a StSpectraCutNhit(10,1000)
  // and then push this back into container of track cuts
  // crash in some way if there is not a match between nhit and
  // a derived StSpectraCut
  //
  double scale;
  double momentumTerm;
  is >> scale >> momentumTerm;
  effic.mMomentumTerm = momentumTerm;
  effic.mScale = scale;
  return is;
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
return effic;
}
