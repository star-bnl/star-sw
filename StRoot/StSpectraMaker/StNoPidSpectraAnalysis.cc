#include "StNoPidSpectraAnalysis.h"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include <string.h>
#include <TFile.h>
#include "StEventTypes.h"

StNoPidSpectraAnalysis::StNoPidSpectraAnalysis() {}

StNoPidSpectraAnalysis::~StNoPidSpectraAnalysis() {}

void StNoPidSpectraAnalysis::bookHistograms() {

  mNumEvent = 0;
 
  // use abscissa and ordinate types

  string hlab2DSpectra = "";

  if (mAbscissa == kPseudoRapidity && mOrdinate == kPperp) {
    hlab2DSpectra= "EtaPt";
  } else {
    // failure !!!
  } 

  hlab2DSpectra = hlab2DSpectra + mTitle;
  const char* h2DSpectra = hlab2DSpectra.c_str(); 
  m2DSpectra = new TH2D(h2DSpectra,"2-D spectra",
			 mnbinAbscissa, mlbinAbscissa,mubinAbscissa,
			 mnbinOrdinate,mlbinOrdinate,mubinOrdinate);
  m2DSpectra->Sumw2();

}

void StNoPidSpectraAnalysis::fillHistograms(StEvent& event) {

  StVertex* primvtx = event.primaryVertex();
  if (primvtx==0) return;
  mNumEvent++ ; 

  cout << mParticle->charge() << endl;

  const StSPtrVecPrimaryTrack& tracks = event.primaryVertex()->daughters();
  StPrimaryTrackIterator iter;
  StPrimaryTrack *track;

  for (iter = tracks.begin();
       iter != tracks.end(); iter++) {
    track = *iter;
    if (track==0) continue;
          // 
	  // check to see if track satisfies the quality cuts set up
	  // for this analysis
	  //
       vector<StSpectraCut*>::const_iterator cutIter;
       bool satisfiesAllCuts = true ;
       for (cutIter = mEffic.mSpectraCutContainer.begin();
	    cutIter != mEffic.mSpectraCutContainer.end();
	    cutIter++) {
	    if (!((*cutIter)->satisfiesCut(track,&event)) && satisfiesAllCuts){
	       satisfiesAllCuts = false;
	    }
       }

       if (satisfiesAllCuts && 
	      fabs(mParticle->charge() - 
		   track->geometry()->charge())<0.01) {

	    StThreeVectorD mom = track->geometry()->momentum();
	    double p = abs(mom);

	    double effic = mEffic.efficiency(track);
	    //  cout << effic << endl;
	    if (effic > 0. && effic <= 1.) {
		float weight = 1./effic;
		double pperp = mom.perp();
                double pz = mom.z();
		double pseudoy = 0.5*log((p+pz)/(p-pz));
                double xvalue =-1000.;
                double yvalue = 0.;
		if (mAbscissa == kPseudoRapidity) {
		  xvalue = pseudoy;
		} 
		if (mOrdinate == kPperp) {
		  yvalue = pperp;
		}
		m2DSpectra->Fill(xvalue,yvalue,weight);
	    }
       }       
  }   
}

void StNoPidSpectraAnalysis::projectHistograms() {

  if (mNumEvent==0) return;
  float xnorm = 1./float(mNumEvent);
  m2DSpectra->Scale(xnorm);
 
  Stat_t stats[8];
  m2DSpectra->GetStats(stats);
  cout << "sum of weights " << stats[0] << endl;

}

void StNoPidSpectraAnalysis::writeHistograms(){

  const char* outputName = ((*this).getTitle()+".root").c_str();
  TFile* analysisOutputFile = new TFile(outputName,"RECREATE");
  m2DSpectra->Write();
  delete analysisOutputFile;
}






