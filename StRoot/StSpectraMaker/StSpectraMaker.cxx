// $Id: StSpectraMaker.cxx,v 1.1 1999/11/03 21:22:41 ogilvie Exp $
// $Log: StSpectraMaker.cxx,v $
// Revision 1.1  1999/11/03 21:22:41  ogilvie
// initial version
//
//
///////////////////////////////////////////////////////////////////////////////
//
///////////////////////////////////////////////////////////////////////////////
#include "StSpectraMaker.h"
#include "StChain.h"
#include "StRun.h"
#include "StEvent.h"
#include "StMessMgr.h"
#include <vector>

static const char rcsid[] = "$Id: StSpectraMaker.cxx,v 1.1 1999/11/03 21:22:41 ogilvie Exp $";

StSpectraMaker::StSpectraMaker(const Char_t *name) : StMaker(name) {
}

StSpectraMaker::~StSpectraMaker() {
}

Int_t StSpectraMaker::Init() {

  mOutput = new TFile("deviant.root","RECREATE");
  //
  // create two analyses, piplus and pminus via tpc dedx deviants
  // do this via a file script?
  //

  StTpcDeviantSpectraAnalysis* piplus = new StTpcDeviantSpectraAnalysis;
  piplus->setParticle("pi+");
  piplus->setTitle("piplus");
  mSpectraAnalysisContainer.push_back(piplus);
 
  StTpcDeviantSpectraAnalysis* piminus = new StTpcDeviantSpectraAnalysis;
  piminus->setParticle("pi-");
  piminus->setTitle("piminus");
  mSpectraAnalysisContainer.push_back(piminus);

  //
  // set common characteristics of spectra, e.g. size of bins, efficiencies
  // dca cuts etc., do this via a file?
  // loop through the analyses and load into the private data members the
  // spectra characteristics
  // 

  StTpcDeviantSpectraAnalysis* analysis = new StTpcDeviantSpectraAnalysis;
  vector<StTpcDeviantSpectraAnalysis*>::const_iterator analysisIter;

  for (analysisIter = mSpectraAnalysisContainer.begin();
	 analysisIter != mSpectraAnalysisContainer.end();
	 analysisIter++) {
    analysis = *analysisIter;
    double ybin = 0.1;
    double mtbin = 0.1;
    analysis->setYBinSize(ybin); 
    analysis->setMtBinSize(mtbin);

    StEfficiency* effic = new StEfficiency;
    effic->setNhitCut(10);
    effic->setDcaCut(2.*centimeter);
    effic->init(analysis->getParticle());
    analysis->setEfficiencyParam(effic);

    analysis->bookHistograms();
  }
 return StMaker::Init();
}

Int_t StSpectraMaker::Make() {
  StEvent* mEvent;
  mEvent = (StEvent *) GetInputDS("StEvent");
  if (! mEvent) return kStOK; // If no event, we're done
  StEvent& ev = *mEvent;
  //
  // for each analysis, call fill histograms
  //
  StTpcDeviantSpectraAnalysis* analysis = new StTpcDeviantSpectraAnalysis;
  vector<StTpcDeviantSpectraAnalysis*>::const_iterator analysisIter;

  for (analysisIter = mSpectraAnalysisContainer.begin();
	 analysisIter != mSpectraAnalysisContainer.end();
	 analysisIter++) {
    analysis = *analysisIter;
    analysis->fillHistograms(ev);
  }  
  return kStOK;
}

void StSpectraMaker::Clear(Option_t *opt) {

  StMaker::Clear();
}

Int_t StSpectraMaker::Finish() {
  
  StTpcDeviantSpectraAnalysis* analysis = new StTpcDeviantSpectraAnalysis;
  vector<StTpcDeviantSpectraAnalysis*>::const_iterator analysisIter;

  for (analysisIter = mSpectraAnalysisContainer.begin();
	 analysisIter != mSpectraAnalysisContainer.end();
	 analysisIter++) {
    analysis = *analysisIter;
    analysis->projectHistograms();
  }

  // write out histograms
  cout << "writing out histograms" << endl;

  mOutput->Write("MyKey",kSingleKey);
  mOutput->Close();
  return kStOK;
}

ClassImp(StSpectraMaker)


