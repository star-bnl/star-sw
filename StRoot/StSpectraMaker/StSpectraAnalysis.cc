#include "StSpectraAnalysis.h"
#include "StParticleDefinition.hh"
#include "StParticleTable.hh"
#include "StEventTypes.h"

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
string StSpectraAnalysis::getTitle() {
  return mTitle;
}

void StSpectraAnalysis::setEfficiency(StEfficiency& effic) {
  mEffic = effic;
}
StEfficiency* StSpectraAnalysis::getEfficiency() {
  return &(mEffic);
}

void StSpectraAnalysis::setAbscissa(StSpectraAbscissa abscissa,
				    float lbin, float ubin, int nbin) {
  mAbscissa     = abscissa;
  mlbinAbscissa = lbin;
  mubinAbscissa = ubin;
  mnbinAbscissa = nbin;
}

void StSpectraAnalysis::setOrdinate(StSpectraOrdinate ordinate,
				    float lbin, float ubin, int nbin) {
  mOrdinate     = ordinate;
  mlbinOrdinate = lbin;
  mubinOrdinate = ubin;
  mnbinOrdinate = nbin;
}

StSpectraAnalysis::StSpectraAnalysis(){
  // no-op
}
StSpectraAnalysis::~StSpectraAnalysis(){
}














