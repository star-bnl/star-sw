#include "StTpcDeviantSpectraAnalysis.h"
#include <string.h>

StTpcDeviantSpectraAnalysis::StTpcDeviantSpectraAnalysis() {
}

StTpcDeviantSpectraAnalysis::~StTpcDeviantSpectraAnalysis() {
}

void StTpcDeviantSpectraAnalysis::bookHistograms() {
 
  *mNumEvent = 0;

  float massParticle = mParticle->mass();
 
  float ldevbin = -10.;
  float udevbin = 10. ;
  int Ndevbins = 50;
  float lybin =-2. ;
  float uybin = 2.;
  int NYbins = int(0.5+(uybin - lybin)/(*mYBinSize));
  float lmtbin = massParticle ;
  float umtbin = massParticle + 1.;
  int NMtbins = int(0.5+(umtbin - lmtbin)/(*mMtBinSize));

  char* hlab = new char[100];

  strcpy(hlab,"YMtDeviant"); 
  strcat(hlab,mTitle);
  mYMtDeviant = new TH3D(hlab,"number sigma from PID band, y,mt",
			 NYbins, lybin,uybin,
			 NMtbins,lmtbin,umtbin,
			 Ndevbins,ldevbin,udevbin);
  mYMtDeviant->Sumw2();
  
  strcpy(hlab,"YMt"); 
  strcat(hlab,mTitle);
  mYMt = new TH2D(hlab,"y,mt",
			 NYbins, lybin,uybin,
			 NMtbins,lmtbin,umtbin);
  mYMt->Sumw2();
  
  strcpy(hlab,"PIDDeviant"); 
  strcat(hlab,mTitle);
  mPIDDeviant = new TH1D(hlab,"number sigma from pid band",
		Ndevbins,ldevbin,udevbin);
  mPIDDeviant->Sumw2();

  strcpy(hlab,"dedxvsP"); 
  strcat(hlab,mTitle);
  mDedxvsP = new TH2D(hlab,"dedx vs p",50,0.,1.,50, 0., 1.e-05);
  mDedxvsP->Sumw2();

}

void StTpcDeviantSpectraAnalysis::fillHistograms(StEvent& event) {

  (*mNumEvent)++ ;  
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
	  int nhit = tpc->numberOfPointsUsed();
	  float dca = track->helix().distance(primvtx->position());
         
	  const double  bField = 0.5*tesla;
	  StThreeVectorD mom = track->helix().momentum(bField);
	  double p = abs(mom);
	  // 
	  // check to see if track satisfies the quality cuts set up
	  // for this analysis
	  //
	  if (nhit > mEffic->nhitCut() && 
	      dca < mEffic->dcaCut() && 
	      fabs(mParticle->charge() - track->helix().charge(bField)) < 0.01) {

            double dedx = tpc->mean();
	    double deviant = 
	      track->pidTraits().tpcDedxPid()->numberOfSigma(mMassPid);

	    double effic = mEffic->efficiency(track);
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

  if (*mNumEvent==0) return;
  float xnorm = 1./float(*mNumEvent);
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
  char hlabel[100];
  char hlab[100]; 
  for (Int_t iYSlice =1 ; iYSlice < NYbins+1; iYSlice++){ 
    int iYFirst = iYSlice ;
    int iYLast  = iYSlice + 1 ;
    for (Int_t iMtSlice =1 ; iMtSlice < NMtbins+1; iMtSlice++){ 
      int iMtFirst = iMtSlice ;
      int iMtLast  = iMtSlice + 1 ;
      sprintf(hlabel,"deviant%iY%i",iYSlice,iMtSlice);
      strcpy(hlab,hlabel); 
      strcat(hlab,mTitle);   
      // project here
      deviant[ihist] = *(mYMtDeviant->ProjectionZ(hlab, iYFirst, iYLast,
					   iMtFirst, iMtLast,"E"));       
      ihist++;
    }
  }
}

