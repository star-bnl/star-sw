#include "StSpectraAnalysis.h"


StSpectraAnalysis::~StSpectraAnalysis() {
}

void StSpectraAnalysis::setParticle(string particle) {

 mParticle = StParticleTable::instance()->findParticle(particle) ;
 if (mParticle) {
   cout << "found particle, mass " << mParticle->mass() << endl;
 }
 else {
   cout << "particle not found: " << particle.c_str() << endl;
 }
}

StParticleDefinition* StSpectraAnalysis::getParticle() {
 return mParticle;
}

void StSpectraAnalysis::setTitle(string title) {
  mTitle = title;
}

void StSpectraAnalysis::setEfficiency(StEfficiency effic) {
  mEffic = effic;
}
StEfficiency* StSpectraAnalysis::getEfficiency() {
  return &(mEffic);
}






