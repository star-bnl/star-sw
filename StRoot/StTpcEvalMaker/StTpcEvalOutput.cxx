//-------------------------------------------------
// For StTpcEvalMaker
//-------------------------------------------------
// author: milton toy
// additions: manuel cbs
//-------------------------------------------------
// class definitions for StTpcEvalOutput
//-------------------------------------------------
#include <iostream.h>
#include <stdlib.h>
#include <string>

#include "TFile.h"

#include "StTpcEvalOutput.h"

ClassImp(StTpcEvalOutput)

StTpcEvalOutput::StTpcEvalOutput() {
}

void StTpcEvalOutput::Open() {
  cout << " StTpcEvalMaker:  Opening output file..." <<endl;
  // to do: pass on string to replace StTpcEval.hist.root
  mOutputFile = new TFile("StTpcEval.hist.root","RECREATE","StTpcEvalMaker Results");
}

void StTpcEvalOutput::Write() {
  cout << " StTpcEvalMaker:  Writing out histograms..." <<endl;
  mOutputFile->Write();
}

void StTpcEvalOutput::Close() {
  cout << " StTpcEvalMaker:  Closing output file..." <<endl;
  mOutputFile->Close();
}
