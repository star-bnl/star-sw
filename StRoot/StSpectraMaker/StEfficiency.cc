#include "StEfficiency.h"
StEfficiency::StEfficiency() {
}

StEfficiency::~StEfficiency() {
}

void StEfficiency::setNhitCut(int nhit) {
 mNhitCut = nhit;
}
int StEfficiency::nhitCut() {
 return mNhitCut ;
}
 
void StEfficiency::setDcaCut(double dca) {
 mDcaCut = dca;
}
double StEfficiency::dcaCut() {
 return mDcaCut ;
}

void StEfficiency::init(StParticleDefinition* particle) {

  // placeholder for efficiency parameterisation
  // derived  from embedding 
  // since this will change for specific analyses, this init
  // will most probably read numbers from file
  // in meantime, effic =  mScale*(1-mMomentumTerm/(beta*p)^2);

  mParticle = particle;
  mScale = 0.96;
  mMomentumTerm = 0.0004;
}
 
double StEfficiency::efficiency(StTrack* track) {

 const double  bField = 0.5*tesla;
 StThreeVectorD mom = track->helix().momentum(bField);
 double p = abs(mom);
 double mass = mParticle->mass();
 double e = sqrt(p*p + mass*mass);
 double beta = p/e ;
 double effic = mScale*(1-mMomentumTerm/pow(beta*p,2));
return effic;
}
