// $Id: StSpectraMaker.cxx,v 1.7 2000/03/03 03:30:32 ogilvie Exp $
// $Log: StSpectraMaker.cxx,v $
// Revision 1.7  2000/03/03 03:30:32  ogilvie
// major infra. change to read in/use efficiency histos
//
// Revision 1.6  2000/01/11 19:09:12  ogilvie
// compiles on sun CC5, linux, but not sun cc4
//
// Revision 1.5  1999/11/28 20:22:06  ogilvie
// updated to work with new StEvent
//
// Revision 1.4  1999/11/22 01:54:58  ogilvie
// generalised analysis containers to beany object that inherits from StSpectraAnalysis
//
// Revision 1.3  1999/11/08 17:07:32  ogilvie
// *** empty log message ***
//
// Revision 1.2  1999/11/05 18:58:49  ogilvie
// general tidy up following Mike Lisa's review. List of analyses conntrolled via
// analysis.dat, rather than hardcoded into StSpectraMaker.cxx
//
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
#include "StEventTypes.h"
#include "StTpcDeviantSpectraAnalysis.h"
#include "StMessMgr.h"
#include "StEfficiency.h"


#include <vector>
#include <fstream.h>

string readString(ifstream& ifs) {
  string line;
  #if defined(__SUNPRO_CC)
        char c; line.erase();
        while ((c = ifs.get()) && c != '\n' && !ifs.eof()) line += c;
  #else
        getline(ifs,line,'\n');
  #endif
  cout << line.c_str() << endl;
  return line;
}

static const char rcsid[] = "$Id: StSpectraMaker.cxx,v 1.7 2000/03/03 03:30:32 ogilvie Exp $";

StSpectraMaker::StSpectraMaker(const Char_t *name) : StMaker(name) {
}

StSpectraMaker::~StSpectraMaker() {
}

Int_t StSpectraMaker::Init() {

  mOutput = new TFile("deviant.root","RECREATE");
  //
  // create the analyses that are stored in the file analysis.dat, 
  // 

  ifstream from("StRoot/StSpectraMaker/analysis.dat");
  while (!from.eof()) {

    cout << "particle name " ;
    string particleName = readString(from);
    string analysisTitle = readString(from); 
    char* efficiencyFile = new char[100];
    from >> efficiencyFile; 
    cout << efficiencyFile << endl;
    StEfficiency effic(efficiencyFile);
    delete efficiencyFile; 

    effic.setParticle(particleName);
    
    float lYbin = effic.getLowEdge('x');
    float uYbin = effic.getUpEdge('x');
    int nYbin = effic.getNbin('x');
    float lPtbin = effic.getLowEdge('y');
    float uPtbin = effic.getUpEdge('y');
    int nPtbin = effic.getNbin('y');

    StTpcDeviantSpectraAnalysis* anal = new StTpcDeviantSpectraAnalysis;
    anal->setParticle(particleName);
    anal->setTitle(analysisTitle);
    anal->setEfficiency(effic);   
    anal->setYAxis(lYbin,uYbin,nYbin); 
    anal->setPtAxis(lPtbin,uPtbin,nPtbin);
 
    mSpectraAnalysisContainer.push_back(anal);

    string comment = readString(from); 
  }
  from.close();
 
  //
  // loop through the analyses and book histograms
  // 

  vector<StSpectraAnalysis*>::const_iterator analysisIter;

  for (analysisIter = mSpectraAnalysisContainer.begin();
	 analysisIter != mSpectraAnalysisContainer.end();
	 analysisIter++) {
    (*analysisIter)->bookHistograms();
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
  vector<StSpectraAnalysis*>::const_iterator analysisIter;

  for (analysisIter = mSpectraAnalysisContainer.begin();
	 analysisIter != mSpectraAnalysisContainer.end();
	 analysisIter++) {
    (*analysisIter)->fillHistograms(ev);
  }  
  return kStOK;
}

void StSpectraMaker::Clear(Option_t *opt) {

  StMaker::Clear();
}

Int_t StSpectraMaker::Finish() {

  vector<StSpectraAnalysis*>::const_iterator analysisIter;

  for (analysisIter = mSpectraAnalysisContainer.begin();
	 analysisIter != mSpectraAnalysisContainer.end();
	 analysisIter++) {
    (*analysisIter)->projectHistograms();
  }

  // write out histograms
  cout << "writing out histograms" << endl;

  mOutput->Write("MyKey",kSingleKey);
  cout << "written"<< endl;
  mOutput->Close();

  // delete analyses in container

  for (analysisIter = mSpectraAnalysisContainer.begin();
	 analysisIter != mSpectraAnalysisContainer.end();
	 analysisIter++) {
    delete (*analysisIter);
  }

  return kStOK;
}


ClassImp(StSpectraMaker)



