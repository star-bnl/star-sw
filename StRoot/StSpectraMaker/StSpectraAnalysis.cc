#include "StSpectraAnalysis.h"


StSpectraAnalysis::StSpectraAnalysis() {

  mTitle    = new char[20]; 
  mYBinSize = new double;
  mMtBinSize = new double;
  mEffic = new StEfficiency;
  mNumEvent = new int;

}

StSpectraAnalysis::~StSpectraAnalysis() {
}

void StSpectraAnalysis::setParticle(char particle[20]) {

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

void StSpectraAnalysis::setTitle(char title[20]) {
 strcpy(mTitle,title);
}

void StSpectraAnalysis::setYBinSize(double ybin) {
  *mYBinSize = ybin;
}
double StSpectraAnalysis::getYBinSize() {
  return *mYBinSize;
}
void StSpectraAnalysis::setMtBinSize(double mtbin) {
  *mMtBinSize = mtbin;
}
double StSpectraAnalysis::getMtBinSize() {
  return *mMtBinSize;
}
void StSpectraAnalysis::setEfficiencyParam(StEfficiency* effic) {
  mEffic = effic;
}
StEfficiency* StSpectraAnalysis::getEfficiencyParam() {
  return mEffic;
}






