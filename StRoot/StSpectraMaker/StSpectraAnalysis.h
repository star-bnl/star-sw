#ifndef StSpectraAnalysis_hh
#define StSpectraAnalysis_hh


#include "StEfficiency.h"

class StEvent;
class StParticleDefinition;

//
// a base class
// for all derived StSpectraAnalysis classes
//

class StSpectraAnalysis {

 public:

  virtual ~StSpectraAnalysis();  
  void setTitle(string title);
  string getTitle();

  void setParticle(string particle);
  StParticleDefinition* getParticle();

  void setEfficiency(StEfficiency& effic);
  StEfficiency* getEfficiency();

 // should perhaps be one function to setAxes
  void setAbscissa(StSpectraAbscissa abscissa, float lbin, float ubin, int nbin); 
  void setOrdinate(StSpectraOrdinate ordinate, float lbin, float ubin, int nbin);

  virtual void bookHistograms()=0;
  virtual void fillHistograms(StEvent& event)=0;
  virtual void projectHistograms()=0;
  virtual void writeHistograms()=0;

 private:
 
 protected:

  StSpectraAnalysis();

  string mTitle;
  StParticleDefinition* mParticle;
  StEfficiency mEffic;
  int mNumEvent;

  // perhaps define a StSpectrAxis class?
  // with these data members?

  StSpectraOrdinate mOrdinate;
  float mlbinOrdinate;
  float mubinOrdinate;
  int mnbinOrdinate;

  StSpectraAbscissa mAbscissa;
  float mlbinAbscissa;
  float mubinAbscissa;
  int mnbinAbscissa;

};

#endif

