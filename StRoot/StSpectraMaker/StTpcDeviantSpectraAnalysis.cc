#include "StTpcDeviantSpectraAnalysis.h"
#include "StTpcDedxPidAlgorithm.h"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include <string.h>
#include <TFile.h>
#include "StEventTypes.h"

StTpcDeviantSpectraAnalysis::StTpcDeviantSpectraAnalysis() {}

StTpcDeviantSpectraAnalysis::~StTpcDeviantSpectraAnalysis() {}

void StTpcDeviantSpectraAnalysis::bookHistograms() {

  mNumEvent = 0;
 
  float ldevbin = -5.;
  float udevbin = 5. ;
  int Ndevbins = 50;

  // use abscissa and ordinate types

  string hlab2DSpectra = "";
  if (mAbscissa == kRapidity && mOrdinate == kPperp) {
    hlab2DSpectra= "YPt";
  } else if (mAbscissa == kRapidity && mOrdinate == kTransverseMass) {
    hlab2DSpectra= "YMt";
  } else if (mAbscissa == kPseudoRapidity && mOrdinate == kPperp) {
    hlab2DSpectra= "EtaPt";
  } 
  //
  // replace all this cut and paste with a a helper function
  //

  string hlab2DSpectraProjected = hlab2DSpectra;
  hlab2DSpectraProjected = hlab2DSpectraProjected + mTitle;
  const char* h2DSpectraProjected = hlab2DSpectraProjected.c_str(); 
  m2DSpectra = new TH2D(h2DSpectraProjected,
			       "2D phase space inv. spectra",
			 mnbinAbscissa, mlbinAbscissa,mubinAbscissa,
			 mnbinOrdinate,mlbinOrdinate,mubinOrdinate);
  m2DSpectra->Sumw2();  

  string hlab2DJacobian = hlab2DSpectra+"Jacobian";
  hlab2DJacobian = hlab2DJacobian + mTitle;
  const char* h2DJacobian = hlab2DJacobian.c_str(); 
  m2DJacobian = new TH2D(h2DJacobian,
			       "2D phase space Jacobian",
			 mnbinAbscissa, mlbinAbscissa,mubinAbscissa,
			 mnbinOrdinate,mlbinOrdinate,mubinOrdinate);
  m2DJacobian->Sumw2();  

  string hlab2DCounts = hlab2DSpectra+"Counts";
  hlab2DCounts = hlab2DCounts + mTitle;
  const char* h2DCounts = hlab2DCounts.c_str(); 
  m2DCounts = new TH2D(h2DCounts,
			       "2D phase space Counts",
			 mnbinAbscissa, mlbinAbscissa,mubinAbscissa,
			 mnbinOrdinate,mlbinOrdinate,mubinOrdinate);
  m2DCounts->Sumw2(); 

  string hlab2DSpectraDev = hlab2DSpectra+"DeviantWeighted";
  hlab2DSpectraDev = hlab2DSpectraDev + mTitle;
  const char* h2DSpectraDev = hlab2DSpectraDev.c_str(); 
  m2DSpectraDeviantWeighted = new TH3D(h2DSpectraDev,
			       "number sigma from PID band in 2D phase space bins",
			 mnbinAbscissa, mlbinAbscissa,mubinAbscissa,
			 mnbinOrdinate,mlbinOrdinate,mubinOrdinate,
			 Ndevbins,ldevbin,udevbin);
  m2DSpectraDeviantWeighted->Sumw2();  
  

  string hlab2DSpectraDevC = hlab2DSpectra+"DeviantCounts";
  hlab2DSpectraDevC = hlab2DSpectraDevC + mTitle;
  const char* h2DSpectraDevC = hlab2DSpectraDevC.c_str(); 
  m2DSpectraDeviantCounts = new TH3D(h2DSpectraDevC,
			       "number sigma from PID band in 2D phase space bins",
			 mnbinAbscissa, mlbinAbscissa,mubinAbscissa,
			 mnbinOrdinate,mlbinOrdinate,mubinOrdinate,
			 Ndevbins,ldevbin,udevbin);
  m2DSpectraDeviantCounts->Sumw2(); 

  string hlabDedx = "dedxvsP";
  hlabDedx = hlabDedx + mTitle;
  const char* hDedx = hlabDedx.c_str(); 
  mDedxvsP = new TH2D(hDedx,"dedx vs p",50,0.,1.,50, 0., 1.e-05);
  mDedxvsP->Sumw2();

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
            track->pidTraits(tpcDedxAlgorithm);
	    double deviant = tpcDedxAlgorithm.numberOfSigma(mParticle);

	    double pperp = mom.perp();
	    double mt = sqrt(pperp*pperp + mMassPid*mMassPid);
	    double E = sqrt(p*p+mMassPid*mMassPid);
	    double pz = mom.z();
	    double y = 0.5*log((E+pz)/(E-pz));
	    double pseudoy = 0.5*log((p+pz)/(p-pz));
            double xvalue=-1000.;
	    double yvalue=0.;
	    if (mAbscissa == kRapidity) {
		  xvalue = y;
	    } else if (mAbscissa == kPseudoRapidity) {
		  xvalue = pseudoy;
	    } 
	    if (mOrdinate == kPperp) {
		  yvalue = pperp;
	    } else if (mOrdinate == kTransverseMass) {
		  yvalue = mt;
	    }
            double effic = mEffic.efficiency(track);
	    //  cout << effic << endl;
	    if (effic > 0. && effic <= 1. && yvalue > 0.) {
		float weight = 1./effic; 
		m2DSpectraDeviantWeighted->Fill(xvalue,yvalue,deviant,weight);
                m2DCounts->Fill(xvalue,yvalue,1);
                m2DJacobian->Fill(xvalue,yvalue,1./yvalue);
	        m2DSpectraDeviantCounts->Fill(xvalue,yvalue,deviant,1.);
	    }
	    mPIDDeviant->Fill(deviant);
       }       
  }
}

void StTpcDeviantSpectraAnalysis::projectHistograms() {

  if (mNumEvent==0) return;
  float xnorm = 1./float(mNumEvent);
  m2DSpectraDeviantWeighted->Scale(xnorm);
 
  Stat_t stats[8];
  m2DSpectraDeviantWeighted->GetStats(stats);
  cout << "sum of weights " << stats[0] << endl;

}

void StTpcDeviantSpectraAnalysis::writeHistograms(){

  const char* outputName = ((*this).getTitle()+".root").c_str();
  TFile* analysisOutputFile = new TFile(outputName,"RECREATE");
  m2DSpectraDeviantWeighted->Write();
  m2DSpectraDeviantCounts->Write();
  m2DJacobian->Write();
  m2DCounts->Write();
  m2DSpectra->Write();
  mPIDDeviant->Write();
  delete analysisOutputFile;
}






