// any problems, send an email to chajecki@mps.ohio-state.edu
#include "AliStHbtEventReader.h"
#include <stdlib.h>
#include "StChain.h"
#include "TChain.h"
#include "TFile.h"
#include "TTree.h"

#include "StPhysicalHelixD.hh"

#include "SystemOfUnits.h"

#include "StIOMaker/StIOMaker.h"

#include "TVector3.h"
#include "TString.h"

#include <math.h>
#include <string>
#include <typeinfo>

#include "StHbtMaker/Infrastructure/StExceptions.hh"
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"

#include "StHbtMaker/Infrastructure/StHbtVector.hh"

#include "StHbtMaker/Reader/AliStHbtEvent.h"
#include "StHbtMaker/Reader/AliStHbtTrack.h"

#include "StarClassLibrary/StMemoryInfo.hh"

ClassImp(AliStHbtEventReader)

#if !(ST_NO_NAMESPACES)
  using namespace units;
#endif


//__________________
AliStHbtEventReader::AliStHbtEventReader(StHbtIOMode mode, StIOMaker* io, 
				   const char* dirName, const char* fileName, 
				   const char* filter, int maxFiles)
  : mIOMaker(io), mIOMode(mode), mMaxFiles(maxFiles), mDebug(0), mCurrentFile(0), 
  mTTree(0) {
  if (mDebug) cout << "AliStHbtEventReader::AliStHbtEventReader(...)"<< endl;

  mDir = string(dirName);
  mFile = string(fileName);
  mFilter = string(filter);
  mReaderStatus = 0;  // "good"
  mEvent = new AliStHbtEvent;
  if (mDebug) cout << "AliStHbtEventReader::AliStHbtEventReader(...) - leaving"<< endl;
}

//__________________
AliStHbtEventReader::~AliStHbtEventReader(){
  if (mCurrentFile) { mCurrentFile->Close(); delete mCurrentFile; mCurrentFile = 0;}
}

//__________________
StHbtString AliStHbtEventReader::Report(){
  StHbtString temp = "\n This is the AliStHbtEventReader\n";
  return temp;
}

//__________________
StHbtEvent* AliStHbtEventReader::ReturnHbtEvent(){
  if (mDebug) cout << "AliStHbtEventReader::ReturnHbtEvent()" << endl;

  StHbtEvent* hbtEvent = 0;

  try {
      hbtEvent = read();
  }
  catch(StExceptionEOF e) {
      e.print();
      mReaderStatus = 2;
      return 0;
  }
  catch(StException e) {
      e.print();
      mReaderStatus = 1;
      return 0;
  }
  
  if (!hbtEvent) cout << "AliStHbtEventReader::ReturnHbtEvent() - no hbtEvent" << endl;
  
  return hbtEvent;
}

//__________________
StHbtEvent* AliStHbtEventReader::read(){
  if (!mTChain) {
    try {
      cout << initRead(mDir,mFile,mFilter,mMaxFiles) << " files to analyse " << endl;
    }
    catch(StException e) {
      e.print();
      return 0;
    }
  }
  float sumofpid;
 
  unsigned int nEvents = (unsigned int)mTChain->GetEntries();
  if (!nEvents) throw StException("AliStHbtEventReader::read() - no events to read ");

  mEvent->Clear("");
  int iBytes= mTChain->GetEntry(mEventIndex++);

  if (nEvents<mEventIndex) throw StExceptionEOF("AliStHbtEventReader::read()");
  if (!iBytes) throw StException("AliStHbtEventReader::read() - no event ");

	StHbtEvent *hbtEvent = 0;

	if(mEvent) 
	{
	    hbtEvent = new StHbtEvent;
	    
	    hbtEvent->SetEventNumber(mEvent->GetEventNumber());
	    hbtEvent->SetRunNumber(mEvent->GetRunNumber());
	    hbtEvent->SetCtbMult(0);
	    hbtEvent->SetZdcAdcEast(0);
	    hbtEvent->SetZdcAdcWest(0);
	    hbtEvent->SetNumberOfTpcHits(0);
	    hbtEvent->SetNumberOfTracks(mEvent->GetMultiplicity());
	    hbtEvent->SetNumberOfGoodTracks(0);
	    hbtEvent->SetUncorrectedNumberOfPositivePrimaries(0);
	    hbtEvent->SetUncorrectedNumberOfNegativePrimaries(0); 
	    hbtEvent->SetUncorrectedNumberOfPrimaries(0);
	    hbtEvent->SetReactionPlane(0,0);
	    hbtEvent->SetReactionPlaneError(0, 0);
	    hbtEvent->SetReactionPlaneSubEventDifference(0, 0);
	    hbtEvent->SetTriggerWord(mEvent->GetTrigger());
	    hbtEvent->SetTriggerActionWord(0);
	    hbtEvent->SetL3TriggerAlgorithm(0, 0);
	    hbtEvent->SetUncorrectedNumberOfPrimaries(mEvent->GetMultiplicity());
	    StThreeVectorF vertex(mEvent->GetVertexX(),mEvent->GetVertexY(),mEvent->GetVertexZ());
	    hbtEvent->SetPrimVertPos(vertex);
	    
	    StHbtTrackCollection *mTrackCollection = hbtEvent->TrackCollection();

	    TClonesArray *tracks = 0;
	    
	    tracks = mEvent->Tracks();

	    if (tracks) 
	    {
		int nTracks = tracks->GetEntries();
		for ( int i=0; i<nTracks; i++) 
		{
		    sumofpid=0;
		    AliStHbtTrack *track = (AliStHbtTrack*) tracks->UncheckedAt(i);
		    StHbtTrack* trackCopy = new StHbtTrack;
		    trackCopy->SetHiddenInfo(0);
		    
		    trackCopy->SetCharge(track->GetCharge());
		    trackCopy->SetNHits(track->GetNTpcHits());
		    
		    //---- Set dummy values ----//
		    
		    trackCopy->SetNHitsDedx(0);
		    trackCopy->SetNSigmaElectron(0.);
		    trackCopy->SetNSigmaPion(0.);
		    trackCopy->SetNSigmaKaon(0.);
		    trackCopy->SetNSigmaProton(0.);

		    sumofpid = track->GetPidProbElectron() + track->GetPidProbPion() + track->GetPidProbKaon() + track->GetPidProbProton();
		    
		    trackCopy->SetPidProbElectron(float(track->GetPidProbElectron()/sumofpid));
		    trackCopy->SetPidProbPion(float(track->GetPidProbPion()/sumofpid));
		    trackCopy->SetPidProbKaon(float(track->GetPidProbKaon()/sumofpid));
		    trackCopy->SetPidProbProton(float(track->GetPidProbProton()/sumofpid));
		    trackCopy->SetdEdx(track->GetdEdx());
		    trackCopy->SetDCAxy(track->GetImpactParameterXY());
		    trackCopy->SetDCAz(track->GetImpactParameterZ());
		    trackCopy->SetDCAxyGlobal(0.);
		    trackCopy->SetDCAzGlobal(0.);
		    trackCopy->SetChiSquaredXY(0.);
		    trackCopy->SetChiSquaredZ(0.);
		    
		    //---- Set momentum ----//
	    
		    float px = track->GetPx();
		    float py = track->GetPy();
		    float pz = track->GetPz();
		    
		    StHbtThreeVector v(px,py,pz);
		    
		    trackCopy->SetP(v);
		    trackCopy->SetPt(sqrt(px*px+py*py));
		    
	    // NEED TO SET HELIX!!
		    
		    const StThreeVectorD p((double)px,(double)py,(double)pz);
		    const StThreeVectorD origin((double)mEvent->GetVertexX(),(double)mEvent->GetVertexY(),(double)mEvent->GetVertexZ());
		    
		    StPhysicalHelixD helix(p,origin,(double)(mEvent->GetMagField())*kilogauss,(double)(track->GetCharge()));
		    
		    trackCopy->SetHelix(helix);
	    
		    trackCopy->SetTopologyMap(0,track->GetTopologyMap(0));
		    trackCopy->SetTopologyMap(1,track->GetTopologyMap(1));
		    trackCopy->SetTopologyMap(2,track->GetTopologyMap(2));
		    trackCopy->SetTopologyMap(3,track->GetTopologyMap(3));
		    trackCopy->SetTopologyMap(4,track->GetTopologyMap(4));
		    trackCopy->SetTopologyMap(5,track->GetTopologyMap(4));
		    
		    mTrackCollection->push_back(trackCopy);
		}
	    }
	    
	}
	
	return hbtEvent; 
}

int AliStHbtEventReader::initRead(string dir, string file, string filter, int mMaxFiles){
  mEventIndex =0;
  mTChain = new TChain("AliStHbtTree","AliStHbtTree");

  int nFiles =0;
  if (file!="") { // if a filename was given
    if( strstr(file.c_str(),".lis") || strstr(file.c_str(),".list") ) { // if a file list is specified
      try {
	nFiles = fillChain(mTChain, (dir+file).c_str(), mMaxFiles);
      }
      catch(StException e) {
	throw e;
      }
    }
    else { // a single file was specified
      mTChain->Add((dir+file).c_str());
      nFiles++;
    }
  }
  else {
    try {
      nFiles = fillChain(mTChain,dir.c_str(), filter.c_str(), mMaxFiles);
    }
    catch(StException e) {
      throw e;
    }
  }  
	
	mTChain->SetBranchAddress("AliStHbtEvent",&mEvent); 
  return nFiles;
}


int AliStHbtEventReader::uninitRead(){
  if (mEvent) delete mEvent;
  if (mTChain) delete mTChain;
  mEvent = 0;
  mTChain = 0;
  return 0;
}

int AliStHbtEventReader::fillChain(TChain* chain, const char* fileList, const int maxFiles) {
  ifstream* inputStream = new ifstream;
  inputStream->open(fileList);
  if (!(inputStream)) throw StException("AliStHbtEventReader::fillChain(string dir) - can not open directory");
  char* temp;
  int count=0;
  if (mDebug>1) cout << " AliStHbtEventReader::fillChain(...)- inputStream->good() : " << inputStream->good() << endl;
  for (;inputStream->good();) {
    temp = new char[200];
    inputStream->getline(temp,200);

		TString fileName(temp);
		if(fileName.Contains("root")) {
       chain->Add(temp);
       ++count;
		}
 	  delete temp;
    if (count>maxFiles) break;
  }   
  delete inputStream;
  if (mDebug) cout << "AliStHbtEventReader::(string dir)(string dir) - Added " << count << " files to the chain" << endl;
  return count;
}

int AliStHbtEventReader::fillChain(TChain* chain, const char* dir, const char* filter, const int maxFiles) {
  // read directory
  void *pDir = gSystem->OpenDirectory(dir);
  if(!pDir) throw StException("AliStHbtEventReader::fillChain(string dir) - can not open directory");
  // now find the files that end in the specified searchString
  const char* fileName(0);
  int count(0);
  while((fileName = gSystem->GetDirEntry(pDir))){
    if(strcmp(fileName,".")==0 || strcmp(fileName,"..")==0) continue;
    if(strstr(fileName,filter) ) { // found a match
      char* fullFile = gSystem->ConcatFileName(dir,fileName);
      // add it to the chain
      cout << "AliStHbtEventReader::fillChain(string dir) - Adding " << fullFile << " to the chain" << endl;
      chain->Add(fullFile);
      delete fullFile;
      ++count;
      if (count>maxFiles) break;
    }   
  }
  cout << "AliStHbtEventReader::(string dir)(string dir) - Added " << count << " files to the chain" << endl;
  return count;
}








