#include "StSpectraAnalysis.h"


StSpectraAnalysis::StSpectraAnalysis() {

  // mTitle    = new char[20]; 

}

StSpectraAnalysis::~StSpectraAnalysis() {
  //delete mTitle;
}

void StSpectraAnalysis::setParticle(string particle) {

 mParticle = StParticleTable::instance()->findParticle(particle) ;
 if (mParticle) {
   cout << "found particle, mass " << mParticle->mass() << endl;
 }
 else {
   cout << "particle not found: " << particle << endl;
 }
}

StParticleDefinition* StSpectraAnalysis::getParticle() {
 return mParticle;
}

void StSpectraAnalysis::setTitle(string title) {
  // strcpy(mTitle,title)
  mTitle = title;
}

void StSpectraAnalysis::setEfficiencyParam(StEfficiency effic) {
  mEffic = effic;
}
StEfficiency* StSpectraAnalysis::getEfficiencyParam() {
  return &(mEffic);
}






