//
// StPicoDstReader allows to read picoDst file or a list of files
//

// C++ headers
#include <string>
#include <sstream>
#include <iostream>
#include <fstream>
#include <assert.h>

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoDstReader.h"
#include "StPicoEvent.h"
#include "StPicoTrack.h"
#include "StPicoEmcTrigger.h"
#include "StPicoMtdTrigger.h"
#include "StPicoBbcHit.h"
#include "StPicoEpdHit.h"
#include "StPicoBTowHit.h"
#include "StPicoBTofHit.h"
#include "StPicoMtdHit.h"
#include "StPicoFmsHit.h"
#include "StPicoBEmcPidTraits.h"
#include "StPicoBTofPidTraits.h"
#include "StPicoMtdPidTraits.h"
#include "StPicoTrackCovMatrix.h"
#include "StPicoBEmcSmdEHit.h"
#include "StPicoBEmcSmdPHit.h"
#include "StPicoETofHit.h"
#include "StPicoETofPidTraits.h"
#include "StPicoArrays.h"
#include "StPicoDst.h"

// ROOT headers
#include "TRegexp.h"

ClassImp(StPicoDstReader)

//_________________
StPicoDstReader::StPicoDstReader(const Char_t* inFileName) :
  mPicoDst(new StPicoDst()), mChain(NULL), mTree(NULL),
  mEventCounter(0), mPicoArrays{}, mStatusArrays{} {

  streamerOff();
  createArrays();
  std::fill_n(mStatusArrays, sizeof(mStatusArrays) / sizeof(mStatusArrays[0]), 1);
  mInputFileName = inFileName;
}

//_________________
StPicoDstReader::~StPicoDstReader() {
  if(mChain) {
    delete mChain;
  }
  if(mPicoDst) {
    delete mPicoDst;
  }
}

//_________________
void StPicoDstReader::clearArrays() {
  for(Int_t iArr=0; iArr<StPicoArrays::NAllPicoArrays; iArr++) {
    mPicoArrays[iArr]->Clear();
  }
}

//_________________
void StPicoDstReader::SetStatus(const Char_t *branchNameRegex, Int_t enable) {
  if(strncmp(branchNameRegex, "St", 2) == 0) {
    // Ignore first "St"
    branchNameRegex += 2;
  }

  TRegexp re(branchNameRegex, 1);
  for(Int_t iArr=0; iArr<StPicoArrays::NAllPicoArrays; iArr++) {
    Ssiz_t len;
    if(re.Index(StPicoArrays::picoArrayNames[iArr], &len) < 0) continue;
    LOG_INFO << "StPicoDstMaker::SetStatus " << enable
	     << " to " << StPicoArrays::picoArrayNames[iArr] << endm;
    mStatusArrays[iArr] = enable;
  }

  setBranchAddresses(mChain);
}

//_________________
void StPicoDstReader::setBranchAddresses(TChain *chain) {
  if (!chain) return;
  chain->SetBranchStatus("*", 0);
  TString ts;
  for (Int_t i = 0; i < StPicoArrays::NAllPicoArrays; ++i) {
    if (mStatusArrays[i] == 0) continue;
    char const* bname = StPicoArrays::picoArrayNames[i];
    TBranch* tb = chain->GetBranch(bname);
    if (!tb) {
      LOG_WARN << "setBranchAddress: Branch name " << bname << " does not exist!" << endm;
      continue;
    }
    ts = bname;
    ts += "*";
    chain->SetBranchStatus(ts, 1);
    chain->SetBranchAddress(bname, mPicoArrays + i);
    assert(tb->GetAddress() == (char*)(mPicoArrays + i));
  }
  mTree = mChain->GetTree();
}

//_________________
void StPicoDstReader::streamerOff() {
  // This is to to save space on the file. No need for TObject bits for this structure.
  // see: https://root.cern.ch/doc/master/classTClass.html#a606b0442d6fec4b1cd52f43bca73aa51
  StPicoEvent::Class()->IgnoreTObjectStreamer();
  StPicoTrack::Class()->IgnoreTObjectStreamer();
  StPicoBTofHit::Class()->IgnoreTObjectStreamer();
  StPicoBTowHit::Class()->IgnoreTObjectStreamer();
  StPicoMtdHit::Class()->IgnoreTObjectStreamer();
  StPicoBbcHit::Class()->IgnoreTObjectStreamer();
  StPicoEpdHit::Class()->IgnoreTObjectStreamer();
  StPicoFmsHit::Class()->IgnoreTObjectStreamer();
  StPicoEmcTrigger::Class()->IgnoreTObjectStreamer();
  StPicoMtdTrigger::Class()->IgnoreTObjectStreamer();
  StPicoBTofPidTraits::Class()->IgnoreTObjectStreamer();
  StPicoBEmcPidTraits::Class()->IgnoreTObjectStreamer();
  StPicoMtdPidTraits::Class()->IgnoreTObjectStreamer();
  StPicoTrackCovMatrix::Class()->IgnoreTObjectStreamer();
  StPicoBEmcSmdEHit::Class()->IgnoreTObjectStreamer();
  StPicoBEmcSmdPHit::Class()->IgnoreTObjectStreamer();
  StPicoETofHit::Class()->IgnoreTObjectStreamer();
  StPicoETofPidTraits::Class()->IgnoreTObjectStreamer();
}

//_________________
void StPicoDstReader::createArrays() {

  for(Int_t iArr=0; iArr<StPicoArrays::NAllPicoArrays; iArr++) {
    mPicoArrays[iArr] = new TClonesArray(StPicoArrays::picoArrayTypes[iArr],
					 StPicoArrays::picoArraySizes[iArr]);
  }
  mPicoDst->set(mPicoArrays);
}

//_________________
void StPicoDstReader::Finish() {
  if(mChain) {
    delete mChain;
  }
  mChain = NULL;
}

//_________________
void StPicoDstReader::Init() {

  if(!mChain) {
    mChain = new TChain("PicoDst");
  }

  std::string const dirFile = mInputFileName.Data();

  if( dirFile.find(".list") != std::string::npos ||
      dirFile.find(".lis") != std::string::npos ) {

    std::ifstream inputStream( dirFile.c_str() );

    if(!inputStream) {
      LOG_ERROR << "ERROR: Cannot open list file " << dirFile << endm;
    }

    Int_t nFile = 0;
    std::string file;
    size_t pos;
    while(getline(inputStream, file)) {
      // NOTE: our external formatters may pass "file NumEvents"
      //       Take only the first part
      //cout << "DEBUG found " <<  file << endl;
      pos = file.find_first_of(" ");
      if (pos != std::string::npos ) file.erase(pos,file.length()-pos);
      //cout << "DEBUG found [" <<  file << "]" << endl;

      if(file.find(".picoDst.root") != std::string::npos) {
        TFile* ftmp = TFile::Open(file.c_str());
        if(ftmp && !ftmp->IsZombie() && ftmp->GetNkeys()) {
          LOG_INFO << " Read in picoDst file " << file << endm;
          mChain->Add(file.c_str());
          ++nFile;
        } //if(ftmp && !ftmp->IsZombie() && ftmp->GetNkeys())

        if (ftmp) {
	        ftmp->Close();
        } //if (ftmp)
      } //if(file.find(".picoDst.root") != std::string::npos)
    } //while (getline(inputStream, file))

    LOG_INFO << " Total " << nFile << " files have been read in. " << endm;
  } //if(dirFile.find(".list") != std::string::npos || dirFile.find(".lis" != string::npos))
  else if(dirFile.find(".picoDst.root") != std::string::npos) {
    mChain->Add(dirFile.c_str());
  }
  else {
    LOG_WARN << " No good input file to read ... " << endm;
  }

  if(mChain) {
    setBranchAddresses(mChain);
    mChain->SetCacheSize(50e6);
    mChain->AddBranchToCache("*");
    mPicoDst->set(mPicoArrays);
  }
}

//_________________
Bool_t StPicoDstReader::readPicoEvent(Long64_t iEvent __attribute__((unused)) ) {

  Int_t mStatusRead = true; // true - okay, false - nothing to read

  if (!mChain) {
    LOG_WARN << " No input files ... ! EXIT" << endm;
    mStatusRead = false;
    return mStatusRead;
  }

  Int_t bytes = mChain->GetEntry(mEventCounter++);
  Int_t nCycles = 0;
  while( bytes <= 0) {
    if( mEventCounter >= mChain->GetEntriesFast() ) {
    }

    LOG_WARN << "Encountered invalid entry or I/O error while reading event "
	           << mEventCounter << " from \"" << mChain->GetName() << "\" input tree\n";
    bytes = mChain->GetEntry(mEventCounter++);
    nCycles++;
    LOG_WARN << "Not input has been found for: " << nCycles << " times" << endm;
    if(nCycles >= 10) {
      LOG_ERROR << "Terminating StPicoDstReader::ReadProcess(Long64_t) after "
		            << nCycles << " times!" << endm;
      mStatusRead = false;
      break;
    }
  }
  return mStatusRead;
}
