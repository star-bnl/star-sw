#include "StEfficiency.h"
#include <fstream.h>
#include "StSpectraCutNhit.h"
#include "StSpectraCutDCA.h"
#include "StGetConfigValue.hh"
#include "TFile.h"
#include "StParticleTable.hh"

StEfficiency::StEfficiency() {}

StEfficiency::StEfficiency(char* efficFile) {

  // load container of cuts
  // each element is one of several derived cuts
  // read these cuts from file.
  // This can certainly be improved upon to cope with varying number 
  // of track cuts, protection against cuts not being in file etc.
  //

  int *nhitRange = new int[2];
  StGetConfigValue(efficFile, "nhit", nhitRange, 2);
  cout << "nhit "<< nhitRange[0] << " " << nhitRange[1] << endl;
  mSpectraCutContainer.
    push_back(new StSpectraCutNhit(nhitRange[0],nhitRange[1]));

  double *dcaRange = new double[2];  
  StGetConfigValue(efficFile, "dca", dcaRange, 2);
  cout << "dca " << dcaRange[0] << " " << dcaRange[1] << endl;
  mSpectraCutContainer.
    push_back(new StSpectraCutDCA(dcaRange[0], dcaRange[1]));
  //
  
  char* histRootFileName = new char[100];
  StGetConfigValue(efficFile,"histogram", histRootFileName, 100);
  //
  // read histogram from file and copy it into mEfficHist
  //
  cout << histRootFileName << endl;
  TFile histRootFile(histRootFileName);
  //
  // how do I check that this was successful use isOpen()
  //
  // check which histograms are there
  //
  bool efficHistogramFound = false ;

  TObject* fromFile = histRootFile.Get("efficYPt");
  if (fromFile != 0 ){
    mEfficHistogram = *((TH2D*)fromFile);
    efficHistogramFound = true; 
    mAbscissa = kRapidity;
    mOrdinate = kPperp;
  } 

  if (efficHistogramFound == false) {
    fromFile = histRootFile.Get("efficEtaPt");
    if (fromFile != 0 ){
      mEfficHistogram = *((TH2D*)fromFile);
      efficHistogramFound = true; 
      mAbscissa = kPseudoRapidity;
      mOrdinate = kPperp;
    } 
  }
  if (efficHistogramFound == false) {
      cout << "efficiency histto not found" << endl; 
    // set failure mode of constructor
  }
 //
  histRootFile.Close();
}

StEfficiency::~StEfficiency() {
}

void StEfficiency::setParticle(string particle) {
  mParticle = StParticleTable::instance()->findParticle(particle) ;
}

double StEfficiency::efficiency(StTrack* track) {

 StThreeVectorD mom = track->geometry()->momentum();
 double p = abs(mom);
 double pz = mom.z();
 double pperp = mom.perp();
 double mass = mParticle->mass();
 double E = sqrt(p*p + mass*mass);
 double y = 0.5*log((E+pz)/(E-pz)); 
 
 double effic;
 if ((y < (*this).getLowEdge('x')) ||
     (y > (*this).getUpEdge('x')) ||
     (pperp < (*this).getLowEdge('y')) ||
     (pperp > (*this).getUpEdge('y')) ) {
   //
   // outside acceptance
   //
   effic = 0.0 ;
 } else {
   int globalBin = mEfficHistogram.FindBin(float(y),float(pperp),0.);
   effic = mEfficHistogram.GetBinContent(globalBin);
 }

return effic;
}

StSpectraOrdinate StEfficiency::getOrdinate(){
      return mOrdinate;
}

StSpectraAbscissa StEfficiency::getAbscissa(){
  return mAbscissa;
}

double StEfficiency::getLowEdge(char axis) {
  double lowEdge =0.; 
  if (axis == 'x' || axis == 'X') {
      lowEdge = (mEfficHistogram.GetXaxis())->GetXmin();
  } else if (axis == 'y' || axis == 'Y') {
      lowEdge = (mEfficHistogram.GetYaxis())->GetXmin();
  }
  return lowEdge;
}

double StEfficiency::getUpEdge(char axis) {
  double upEdge = 0.; 
  if (axis == 'x' || axis == 'X') {
      upEdge = mEfficHistogram.GetXaxis()->GetXmax();
  } else if (axis == 'y' || axis == 'Y') {
      upEdge = mEfficHistogram.GetYaxis()->GetXmax();
  }
  return upEdge;
}
 
int StEfficiency::getNbin(char axis) {
  int nbin = 0; 
  if (axis == 'x' || axis == 'X') {
      nbin = mEfficHistogram.GetNbinsX();
  } else if (axis == 'y' || axis == 'Y') {
      nbin = mEfficHistogram.GetNbinsY();
  }
  return nbin;
}




