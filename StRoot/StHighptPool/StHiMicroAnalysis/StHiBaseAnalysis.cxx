/***************************************************************************
 *
 * $Id: StHiBaseAnalysis.cxx,v 1.2 2002/04/03 00:23:27 jklay Exp $                                    
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  Class to perform basic I/O and functions for doing   
 *               highpt Analysis on highpt uDSTs
 *
 *
 ***************************************************************************
 * 
 * $Log: StHiBaseAnalysis.cxx,v $
 * Revision 1.2  2002/04/03 00:23:27  jklay
 * Fixed private member access bugs in analysis code
 *
 * Revision 1.1  2002/04/02 20:05:18  jklay
 * Bums analysis tools for highpt uDSTs
 *
 *
 **************************************************************************/
#include "StHiBaseAnalysis.h"

//__________________

StHiBaseAnalysis::StHiBaseAnalysis(const char* inputDir,
			   const char* outRootName)
  :  mInputDir(inputDir),mOutRootName(outRootName),
     mNEvent(0), mNFile(0),
     mDebug(0), mNEventAccepted(0),
     mNHiPtTrack(0)
     
{


}

//__________________

StHiBaseAnalysis::~StHiBaseAnalysis()
{
}

//__________________
//
// reads in all the files in the directory
// by default, looks for files ending in minimc.root 
//

Int_t
StHiBaseAnalysis::Init()
{
  mBenchmark = new TBenchmark();
  mBenchmark->Start("timer");

  Int_t stat=0;

  cout << "n event : " << mNEvent << endl;

  Cut::ShowCuts();

  cout << "init more" << endl;
  stat += initMore();

  //initHalf();
  if(Cut::Half()) cout << "HALF IS ON! " << Cut::Half() << endl;


  cout << "debug     : " << mDebug << endl << endl;
  
  initChain();

  // create the output file
  //
  cout << "\nOutput file = " << mOutRootName << endl;
  mOutRootFile = new TFile(mOutRootName.Data(),"RECREATE");
  if(!mOutRootFile) {
    cout << "Cannot open output root file " << mOutRootName << endl;
    stat++;
  }

  // init the histograms after the Tfile
  //
  initHistograms();

  return stat;
}

/*
void
StHiBaseAnalysis::initHalf()
{
  if(!Cut::Half()) return;

  cout << "StHiBaseAnalysis::initHalf()" << endl;

  TString side;

  switch(Cut::Half()){
  case 'e' : side = "east_"; break;
  case 'w' : side = "west_"; break;
  default  :  cerr << "same side but vertex cut is weird" << endl; exit(1);
  }

  Ssiz_t last = mOutRootName.Last('/');
  TString filename;
  TString dir;

  if(last) { // directory in name
    filename = mOutRootName;
    dir  = mOutRootName;
    filename.Remove(0,last+1);
    dir.Remove(last+1,mOutRootName.Length());
    filename.Prepend(side);
    mOutRootName = dir + filename;
  }
  else{
    mOutRootName.Prepend(side);
  }

}
*/

//_________________

Int_t
StHiBaseAnalysis::initMore()
{
  return 0;

}

void 
StHiBaseAnalysis::initChain()
{
  cout << "StHiBaseAnalysis::initChain()" << endl;
  //
  // create the chain and event
  //
  // add the files to the chain 
  //
  mHiMicroChain = new TChain("StHiMicroTree");
  mHiMicroEvent = new StHiMicroEvent;

  //
  // set the address where to read the event object
  //
  mHiMicroChain->SetBranchAddress("StHiMicroEvent",&mHiMicroEvent);

  IO io(mInputDir.Data(),"himicro.root");
  io.setNFile(mNFile);
  io.chain(mHiMicroChain);

}
//______________________

void
StHiBaseAnalysis::Run()
{
  if(mDebug)
    cout << "StHiBaseAnalysis::Run()" << endl;

  //
  // loop over all events
  //
  Int_t nEvent = (Int_t) mHiMicroChain->GetEntries();

  if(mNEvent && mNEvent<nEvent) nEvent = mNEvent;

  mNEvent = nEvent;

  cout << "Total # of events " << nEvent << endl;

  // turn of debug automatically for large number of events
  //
  if(nEvent>10000) mDebug = 0;

  Int_t display = 10000;

  for(Int_t iEvent=0; iEvent<nEvent; iEvent++){
    
    //
    // read the event into memory
    //
    mHiMicroChain->GetEvent(iEvent);

    if(mHiMicroEvent){
      if(iEvent%display==0){
	cout << "--------------------- event " << iEvent 
	     << "--------------------- " << endl;
	cout << "\tprimary vertex z : " << mHiMicroEvent->VertexZ() << endl;
	cout << "\tflow centrality  : " << mHiMicroEvent->Centrality() << endl;
      }

      fillEventHistograms();

      // 

      if(acceptEvent(mHiMicroEvent)){

	mNEventAccepted++;

	if(iEvent%display==0) cout << "\tAccepted event" << endl;

	trackLoop();
      }
      else{
	if(iEvent%display==0) cout << "\tNot Accepted " << endl;
      }
    }
    else{ // couldnt read the event??
      cout << "##Cannot process event." << endl;
    }
    mHiMicroEvent->Clear();
  }
}

//_____________________

void 
StHiBaseAnalysis::trackLoop()
{
  if(mDebug)
    cout << "StHiBaseAnalysis::trackLoop()" << endl;

  Int_t nTrack = mHiMicroEvent->NTrack();
  StHiMicroTrack* track;
  
  for(Int_t i=0; i<nTrack; i++){
    track =(StHiMicroTrack*) mHiMicroEvent->tracks()->At(i);

  } // tracks

  if(mDebug)
    cout << "\ttracks : " << nTrack << endl;

}
//_____________________

void
StHiBaseAnalysis::fillEventHistograms()
{

}

//_______________________

Bool_t 
StHiBaseAnalysis::trackOk(StHiMicroTrack* track)
{
  if(!track) {
    cout << "No track pointer?" << endl;
    return kFALSE;
  }
  return kTRUE;
}

//_______________________

void
StHiBaseAnalysis::Finish()
{
  cout << "###StHiBaseAnalysis::Finish" << endl;

  cout << "\tall     : " << mNEvent << endl;
  cout << "\taccepted: " << mNEventAccepted << endl;

  finishHistograms();

  cout << "\tWriting " << mOutRootName << endl;
  mOutRootFile->Write();
  mOutRootFile->Close();
  cout << "\tDone" << endl;

  mBenchmark->Show("timer");

}
//______________________

void
StHiBaseAnalysis::finishHistograms()
{
}

//_______________________

Bool_t
StHiBaseAnalysis::acceptEvent(StHiMicroEvent* event)
{
  return CutRc::Accept(event);

}

//_____________________________________

ClassImp(StHiBaseAnalysis)
