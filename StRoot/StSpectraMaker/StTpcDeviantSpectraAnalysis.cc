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

void StTpcDeviantSpectraAnalysis::setMtAxis(float lMtbin, float uMtbin, int nMtbin) {
  mlMtbin = lMtbin;
  muMtbin = uMtbin;
  mnMtbin = nMtbin;
}

void StTpcDeviantSpectraAnalysis::bookHistograms() {

  cout << mlYbin << " " << muYbin << " " << mnYbin << endl;
  cout << mlMtbin << " " << muMtbin << " " << mnMtbin << endl;
  mNumEvent = 0;
 
  float ldevbin = -5.;
  float udevbin = 5. ;
  int Ndevbins = 50;

  string hlabYMt = "YMt";
  hlabYMt = hlabYMt + mTitle;
  const char* hYMt = hlabYMt.c_str(); 
  mYMt = new TH2D(hYMt,"y,mt",
			 mnYbin, mlYbin,muYbin,
			 mnMtbin,mlMtbin,muMtbin);
  mYMt->Sumw2();

  string hlabDedx = "dedxvsP";
  hlabDedx = hlabDedx + mTitle;
  const char* hDedx = hlabDedx.c_str(); 
  mDedxvsP = new TH2D(hDedx,"dedx vs p",50,0.,1.,50, 0., 1.e-05);
  mDedxvsP->Sumw2();

  string hlabYMtDev = "YMtDeviant";
  hlabYMtDev = hlabYMtDev + mTitle;
  const char* hYMtDev = hlabYMtDev.c_str(); 
  mYMtDeviant = new TH3D(hYMtDev,"number sigma from PID band, y,mt",
			 mnYbin, mlYbin,muYbin,
			 mnMtbin,mlMtbin,muMtbin,
			 Ndevbins,ldevbin,udevbin);
  mYMtDeviant->Sumw2();  


  string hlabPID = "PIDDeviant";
  hlabPID = hlabPID + mTitle;
  const char* hPID = hlabPID.c_str(); 
  mPIDDeviant = new TH1D(hPID,"number sigma from pid band",
		Ndevbins,ldevbin,udevbin);
  mPIDDeviant->Sumw2();


}

void StTpcDeviantSpectraAnalysis::fillHistograms(StEvent& event) {

  mNumEvent++ ;  
  double mMassPid = mParticle->mass();
  cout << mParticle->charge() << endl;
  int pidStatus = 1;
  StTrackCollection *tracks = event.trackCollection();
  StTrackIterator iter;
  StGlobalTrack *track;

  StVertex* primvtx = event.primaryVertex();

  if (primvtx==0) return;

  for (iter = tracks->begin();
       iter != tracks->end(); iter++) {
    track = *iter;
    if (track==0) continue;
    pidStatus = track->pidTraits().tpcDedxPid()->meetsStandardPid();
    if (pidStatus) {
	  const StDedx *tpc = track->tpcDedx();
          if (tpc==0) continue;

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

	  const double  bField = 0.5*tesla;
	  // remove this hardwired and get from new StEvent

	  if (satisfiesAllCuts && 
	      fabs(mParticle->charge() - track->helix().charge(bField))<0.01) {
	    StThreeVectorD mom = track->helix().momentum(bField);
	    double p = abs(mom);
            double dedx = tpc->mean();
	    double deviant = 
	      track->pidTraits().tpcDedxPid()->numberOfSigma(mMassPid);

	    double effic = mEffic.efficiency(track);
	    //  cout << effic << endl;
            if (effic > 0. && effic < 1.) {
	      float weight = 1./effic;
              double pperp = mom.perp();
              double mt = sqrt(pperp*pperp + mMassPid*mMassPid);
              double E = sqrt(p*p+mMassPid*mMassPid);
	      double pz = mom.z();
	      double y = 0.5*log((E+pz)/(E-pz)); 
              mYMtDeviant->Fill(y,mt,deviant,weight);

	    // diagnostic
	      if (p < 1.0*GeV) {
		//  cout << weight<<" " << deviant << endl;
               mDedxvsP->Fill(float(p), float(dedx));
               mPIDDeviant->Fill(deviant,weight);
	      }
	    }
	  }            
    }
  }   

}
void StTpcDeviantSpectraAnalysis::projectHistograms() {

  if (mNumEvent==0) return;
  float xnorm = 1./float(mNumEvent);
  mYMtDeviant->Scale(xnorm);

  // check for histograms
  cout << mYMtDeviant->GetDimension() << "-D histogram" << endl;
  int NYbins  = mYMtDeviant->GetNbinsX();
  int NMtbins = mYMtDeviant->GetNbinsY();

  cout << "has "<< NYbins*NMtbins << " bins" << endl ;
 
  Stat_t stats[8];
  mYMtDeviant->GetStats(stats);
  cout << "sum of weights " << stats[0] << endl;
  mPIDDeviant->GetStats(stats);
  cout << "sum of weights " << stats[0] << endl;

  TH1D* deviant = new TH1D[NYbins*NMtbins];

  int ihist = 0;
  char hlab[100];
 
  for (Int_t iYSlice =1 ; iYSlice < NYbins+1; iYSlice++){ 
    int iYFirst = iYSlice ;
    int iYLast  = iYSlice + 1 ;
    for (Int_t iMtSlice =1 ; iMtSlice < NMtbins+1; iMtSlice++){ 
      int iMtFirst = iMtSlice ;
      int iMtLast  = iMtSlice + 1 ;
      sprintf(hlab,"deviant%iY%i",iYSlice,iMtSlice);
      string hlabel=hlab;
      hlabel=hlabel+mTitle;
      // strcpy(hlab,hlabel); 
      // strcat(hlab,mTitle);
      const char* h = hlabel.data();    
      // project here
      deviant[ihist] = *(mYMtDeviant->ProjectionZ(h, iYFirst, iYLast,
					   iMtFirst, iMtLast,"E"));       
      ihist++;
    }
  }
}








