#include "StTpcDeviantSpectraAnalysis.h"
#include <string.h>

StTpcDeviantSpectraAnalysis::StTpcDeviantSpectraAnalysis() {
}

StTpcDeviantSpectraAnalysis::~StTpcDeviantSpectraAnalysis() {
}

void StTpcDeviantSpectraAnalysis::setYAxis(float lYbin, float uYbin, int nYbin) {
  mlYbin = lYbin;
  muYbin = uYbin;
  mnYbin = nYbin;
}

void StTpcDeviantSpectraAnalysis::setPtAxis(float lPtbin, float uPtbin, int nPtbin) {
  mlPtbin = lPtbin;
  muPtbin = uPtbin;
  mnPtbin = nPtbin;
}

void StTpcDeviantSpectraAnalysis::bookHistograms() {

  mNumEvent = 0;
 
  float ldevbin = -5.;
  float udevbin = 5. ;
  int Ndevbins = 50;

  string hlabYPt = "YPt";
  hlabYPt = hlabYPt + mTitle;
  const char* hYPt = hlabYPt.c_str(); 
  mYPt = new TH2D(hYPt,"y,pt",
			 mnYbin, mlYbin,muYbin,
			 mnPtbin,mlPtbin,muPtbin);
  mYPt->Sumw2();

  string hlabDedx = "dedxvsP";
  hlabDedx = hlabDedx + mTitle;
  const char* hDedx = hlabDedx.c_str(); 
  mDedxvsP = new TH2D(hDedx,"dedx vs p",50,0.,1.,50, 0., 1.e-05);
  mDedxvsP->Sumw2();

  string hlabYPtDev = "YPtDeviant";
  hlabYPtDev = hlabYPtDev + mTitle;
  const char* hYPtDev = hlabYPtDev.c_str(); 
  mYPtDeviant = new TH3D(hYPtDev,"number sigma from PID band, y,pt",
			 mnYbin, mlYbin,muYbin,
			 mnPtbin,mlPtbin,muPtbin,
			 Ndevbins,ldevbin,udevbin);
  mYPtDeviant->Sumw2();  


  string hlabPID = "PIDDeviant";
  hlabPID = hlabPID + mTitle;
  const char* hPID = hlabPID.c_str(); 
  mPIDDeviant = new TH1D(hPID,"number sigma from pid band",
		Ndevbins,ldevbin,udevbin);
  mPIDDeviant->Sumw2();
}

void StTpcDeviantSpectraAnalysis::fillHistograms(StEvent& event) {

  StVertex* primvtx = event.primaryVertex();
  if (primvtx==0) return;
  mNumEvent++ ; 
 
  double mMassPid = mParticle->mass();
  cout << mParticle->charge() << endl;

  const StSPtrVecPrimaryTrack& tracks = event.primaryVertex()->daughters();
  StPrimaryTrackIterator iter;
  StPrimaryTrack *track;

  for (iter = tracks.begin();
       iter != tracks.end(); iter++) {
    track = *iter;
    if (track==0) continue;
       StTpcDedxPidAlgorithm tpcDedxAlgorithm;
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

       // now apply pid algorithm tpcDedxAlgorithm, which finds the tpc dedx
       // object in the collection of pidTraits, and initializes
       // the data members of the algorithm
       // which can then be accessed through member functions
       // of the algorithm e.g. numberOfSigma
       // guess is currently not used
       //
            const StParticleDefinition *guess = track->pidTraits(tpcDedxAlgorithm);
	    double deviant = tpcDedxAlgorithm.numberOfSigma(mParticle);

	    double effic = mEffic.efficiency(track);
	    //  cout << effic << endl;
	    if (effic > 0. && effic <= 1.) {
		float weight = 1./effic;
		double pperp = mom.perp();
		double mt = sqrt(pperp*pperp + mMassPid*mMassPid);
		double E = sqrt(p*p+mMassPid*mMassPid);
		double pz = mom.z();
		double y = 0.5*log((E+pz)/(E-pz)); 
		mYPtDeviant->Fill(y,pperp,deviant,weight);
	    }
       }       
  }   
}

void StTpcDeviantSpectraAnalysis::projectHistograms() {

  if (mNumEvent==0) return;
  float xnorm = 1./float(mNumEvent);
  mYPtDeviant->Scale(xnorm);

  // check for histograms
  cout << mYPtDeviant->GetDimension() << "-D histogram" << endl;
  int NYbins  = mYPtDeviant->GetNbinsX();
  int NPtbins = mYPtDeviant->GetNbinsY();

  cout << "has "<< NYbins*NPtbins << " bins" << endl ;
 
  Stat_t stats[8];
  mYPtDeviant->GetStats(stats);
  cout << "sum of weights " << stats[0] << endl;

}








