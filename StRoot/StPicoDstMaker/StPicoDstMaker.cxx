//
// StPicoDstMaker performs the conversion from MuDst to PicoDst
//

// C++ headers
#include <algorithm>
#include <unordered_map>
#include <string>
#include <vector>

// ROOT headers
#include "TRegexp.h"
#include "TChain.h"
#include "TClonesArray.h"
#include "TTree.h"
#include "TBranch.h"
#include "TObjectSet.h"

// STAR headers
#include "StChain/StChain.h"
#include "StChain/StChainOpt.h"
#include "St_base/StMessMgr.h"
#include "StarRoot/TAttr.h"
#include "StarRoot/THelixTrack.h"
#include "StarClassLibrary/StThreeVector.hh"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StHelix.hh"
#include "StarClassLibrary/StPhysicalHelix.hh"
#include "StarClassLibrary/PhysicalConstants.h"

#include "StEvent/StBTofHeader.h"
#include "StEvent/StDcaGeometry.h"
#include "StEvent/StEmcCollection.h"
#include "StEvent/StEmcCluster.h"
#include "StEvent/StEmcDetector.h"
#include "StEvent/StEmcModule.h"
#include "StEvent/StEmcRawHit.h"
#include "StEvent/StTriggerData.h"
#include "StEvent/StEnumerations.h"
#include "StEvent/StL0Trigger.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMtdHit.h"
#include "StMuDSTMaker/COMMON/StMuMtdPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuMtdHeader.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcPoint.h"
#include "StMuDSTMaker/COMMON/StMuFmsUtil.h"
#include "StMuDSTMaker/COMMON/StMuEpdHit.h"
#include "StMuDSTMaker/COMMON/StMuETofHit.h"

#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcTables.h"

#include "StFmsDbMaker/StFmsDbMaker.h"

#include "tables/St_mtdModuleToQTmap_Table.h"
#include "tables/St_mtdQTSlewingCorr_Table.h"
#include "tables/St_mtdQTSlewingCorrPart2_Table.h"

// PicoDst headers
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#include "StPicoEvent/StPicoEmcTrigger.h"
#include "StPicoEvent/StPicoMtdTrigger.h"
#include "StPicoEvent/StPicoBbcHit.h"
#include "StPicoEvent/StPicoEpdHit.h"
#include "StPicoEvent/StPicoBTowHit.h"
#include "StPicoEvent/StPicoBTofHit.h"
#include "StPicoEvent/StPicoMtdHit.h"
#include "StPicoEvent/StPicoFmsHit.h"
#include "StPicoEvent/StPicoBEmcPidTraits.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"
#include "StPicoEvent/StPicoMtdPidTraits.h"
#include "StPicoEvent/StPicoTrackCovMatrix.h"
#include "StPicoEvent/StPicoBEmcSmdEHit.h"
#include "StPicoEvent/StPicoBEmcSmdPHit.h"
#include "StPicoEvent/StPicoETofHit.h"
#include "StPicoEvent/StPicoETofPidTraits.h"
#include "StPicoEvent/StPicoArrays.h"
#include "StPicoEvent/StPicoDst.h"
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoDstMaker/StPicoUtilities.h"

//_________________
StPicoDstMaker::StPicoDstMaker(char const* name) :
  StMaker(name),
  mTpcVpdVzDiffCut(6.),
  mMuDst(nullptr), mPicoDst(new StPicoDst()),
  mEmcCollection(nullptr), mEmcPosition(nullptr),
  mEmcGeom{}, mEmcIndex{},
  mBField(0),
  mVtxMode(PicoVtxMode::NotSet), // This should always be ::NotSet, do not change it, see ::Init()
  mCovMtxMode(PicoCovMtxMode::Skip),
  mBEmcSmdMode(PicoBEmcSmdMode::SmdSkip),
  mInputFileName(), mOutputFileName(), mOutputFile(nullptr),
  mChain(nullptr), mTTree(nullptr), mEventCounter(0), mSplit(99), mCompression(9), mBufferSize(65536 * 4),
  mModuleToQT{}, mModuleToQTPos{}, mQTtoModule{}, mQTSlewBinEdge{}, mQTSlewCorr{},
  mPicoArrays{}, mStatusArrays{},
  mFmsFiller(*mPicoDst) {

  streamerOff();
  createArrays();
  std::fill_n(mStatusArrays, sizeof(mStatusArrays) / sizeof(mStatusArrays[0]), 1);
}

//_________________
StPicoDstMaker::StPicoDstMaker(PicoIoMode ioMode, char const* fileName,
			       char const* name) : StPicoDstMaker(name) {
  // Constructor that pics IO mode, filename and extension
  StMaker::m_Mode = ioMode;
  mInputFileName = fileName;
}

//_________________
StPicoDstMaker::~StPicoDstMaker() {
  delete mChain;
  delete mPicoDst;
}

//_________________
void StPicoDstMaker::clearArrays() {
  for(Int_t i=0; i<StPicoArrays::NAllPicoArrays; i++) {
    mPicoArrays[i]->Clear();
  }
}

/**
 * Allows one to disable/enable branches matching a simple regex pattern
 * `branchNameRegex` when reading picoDst.root files. For example,
 *
 * ~~~ {.cpp}
 * StPicoDstMaker::SetStatus("*", 0);      // Disables all branches
 * StPicoDstMaker::SetStatus("Emc*", 1);   // Enables branches starting with "Emc"
 * ~~~
 *
 * Note that if the first two characters in `branchNameRegex` are "St" they will
 * be ignored, i.e. "StBTof*" is the same as "BTof*".
 */
//_________________
void StPicoDstMaker::SetStatus(char const* branchNameRegex, int enable) {

  if(strncmp(branchNameRegex, "St", 2) == 0) {
    branchNameRegex += 2; //Ignore first "St"
  }

  TRegexp re(branchNameRegex, 1);
  for(Int_t i=0; i<StPicoArrays::NAllPicoArrays; i++) {
    Ssiz_t len;
    if(re.Index(StPicoArrays::picoArrayNames[i], &len) < 0) {
      continue;
    }
    LOG_INFO << "StPicoDstMaker::SetStatus " << enable << " to "
	     << StPicoArrays::picoArrayNames[i] << endm;
    mStatusArrays[i] = enable;
  }

  if(StMaker::m_Mode == PicoIoMode::IoRead) {
    setBranchAddresses(mChain);
  }
}

//_________________
void StPicoDstMaker::setBranchAddresses(TChain* chain) {
  if (!chain) return;
  chain->SetBranchStatus("*", 0);
  TString ts;
  for (int i = 0; i < StPicoArrays::NAllPicoArrays; ++i) {
    if (mStatusArrays[i] == 0) continue;
    char const* bname = StPicoArrays::picoArrayNames[i];
    TBranch* tb = chain->GetBranch(bname);
    if (!tb) {
      LOG_WARN << "setBranchAddress: Branch name " << bname
	       << " does not exist!" << endm;
      continue;
    }
    ts = bname;
    ts += "*";
    chain->SetBranchStatus(ts, 1);
    chain->SetBranchAddress(bname, mPicoArrays + i);
    assert(tb->GetAddress() == (char*)(mPicoArrays + i));
  }
  mTTree = mChain->GetTree();
}

//_________________
void StPicoDstMaker::streamerOff() {
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
  StPicoBEmcSmdEHit::Class()->IgnoreTObjectStreamer();
  StPicoBEmcSmdPHit::Class()->IgnoreTObjectStreamer();
  StPicoEmcTrigger::Class()->IgnoreTObjectStreamer();
  StPicoMtdTrigger::Class()->IgnoreTObjectStreamer();
  StPicoBTofPidTraits::Class()->IgnoreTObjectStreamer();
  StPicoBEmcPidTraits::Class()->IgnoreTObjectStreamer();
  StPicoMtdPidTraits::Class()->IgnoreTObjectStreamer();
  StPicoTrackCovMatrix::Class()->IgnoreTObjectStreamer();
  StPicoETofHit::Class()->IgnoreTObjectStreamer();
  StPicoETofPidTraits::Class()->IgnoreTObjectStreamer();
}

//_________________
void StPicoDstMaker::createArrays() {

  for (Int_t i = 0; i < (StPicoArrays::NAllPicoArrays); ++i) {
    mPicoArrays[i] = new TClonesArray(StPicoArrays::picoArrayTypes[i],
				      StPicoArrays::picoArraySizes[i]);
  }
  mPicoDst->set(mPicoArrays);
}

//_________________
Int_t StPicoDstMaker::Init() {

  switch (StMaker::m_Mode) {
  case PicoIoMode::IoWrite:
    if (mVtxMode == PicoVtxMode::NotSet) {
      if (setVtxModeAttr() != kStOK) {
	LOG_ERROR << "Pico Vertex Mode is not set ... " << endm;
	return kStErr;
      }
    } //if (mVtxMode == PicoVtxMode::NotSet)

    // To write or not to write covariance matrices into the branch
    if (setCovMtxModeAttr() != kStOK) {
      LOG_ERROR << "Pico covariance matrix I/O mode is not set ..." << endm;
      return kStErr;
    }

    // To write or not to write BEmc Smd hits into the branch
    if (setBEmcSmdModeAttr() != kStOk) {
      LOG_ERROR << "Pico BEmc Smd I/O mode is not set ..." << endm;
      return kStErr;
    }

    if (mInputFileName.Length() == 0) {
      // No input file
      mOutputFileName = GetChainOpt()->GetFileOut();
      mOutputFileName.ReplaceAll(".root", ".picoDst.root");
    }
    else {
      mInputFileName = mInputFileName(mInputFileName.Index("st_"), mInputFileName.Length());
      mOutputFileName = mInputFileName;
      mOutputFileName.ReplaceAll("MuDst.root", "picoDst.root");

      if (mOutputFileName == mInputFileName) {
	LOG_ERROR << "Input file is not a MuDst ... " << endm;
	return kStErr;
      }
    }

    openWrite();
    initEmc();
    break;

  case PicoIoMode::IoRead:
    openRead();
    break;

  default:
    LOG_ERROR << "Pico IO mode is not set ... " << endm;
    return kStErr;
  }

  return kStOK;
}

//_________________
Int_t StPicoDstMaker::setVtxModeAttr(){

  //Read the Tpc-Vpd cut from the input
  Float_t cut = DAttr("TpcVpdVzDiffCut");
  if ( cut != 0.0)  mTpcVpdVzDiffCut = cut;
  LOG_INFO << " mTpcVpdVzDiffCut = " << mTpcVpdVzDiffCut << endm;

  if (strcasecmp(SAttr("PicoVtxMode"), "PicoVtxDefault") == 0) {
    setVtxMode(PicoVtxMode::Default);
    LOG_INFO << " PicoVtxDefault is being used " << endm;
    return kStOK;
  }
  else if (strcasecmp(SAttr("PicoVtxMode"), "PicoVtxVpd") == 0) {
    setVtxMode(PicoVtxMode::Vpd);
    LOG_INFO << " PicoVtxVpd is being used " << endm;
    return kStOK;
  }
  else if (strcasecmp(SAttr("PicoVtxMode"), "PicoVtxVpdOrDefault") == 0) {
    setVtxMode(PicoVtxMode::VpdOrDefault);
    LOG_INFO << " PicoVtxVpdOrDefault is being used " << endm;
    return kStOK;
  }

  return kStErr;
}

//_________________
Int_t StPicoDstMaker::setCovMtxModeAttr() {

  // Choose the writing method: skip - do not write; write - write

  if (strcasecmp(SAttr("PicoCovMtxMode"), "PicoCovMtxWrite") == 0) {
    setCovMtxMode(PicoCovMtxMode::Write);
    LOG_INFO << " PicoCovMtxWrite is being used " << endm;
    return kStOK;
  }
  else if ( (strcasecmp(SAttr("PicoCovMtxMode"), "") == 0) ||
	    (strcasecmp(SAttr("PicoCovMtxMode"), "PicoCovMtxSkip") == 0) ){
    setCovMtxMode(PicoCovMtxMode::Skip);
    LOG_INFO << " PicoCovMtxSkip is being used " << endm;
    return kStOK;
  }
  /*
  if (strcasecmp(SAttr("PicoCovMtxMode"), "PicoCovMtxSkip") == 0) {
    setCovMtxMode(PicoCovMtxMode::Skip);
    LOG_INFO << " PicoCovMtxSkip is being used " << endm;
    return kStOK;
  }
  else if (strcasecmp(SAttr("PicoCovMtxMode"), "PicoCovMtxWrite") == 0) {
    setCovMtxMode(PicoCovMtxMode::Write);
    LOG_INFO << " PicoCovMtxWrite is being used " << endm;
    return kStOK;
  }
  */

  // Only if something went wrong
  return kStErr;
}

//_________________
Int_t StPicoDstMaker::setBEmcSmdModeAttr() {

  // Choose the writing method: skip - do not write; write - write
  if (strcasecmp(SAttr("PicoBEmcSmdMode"), "PicoBEmcSmdWrite") == 0) {
    setBEmcSmdMode(PicoBEmcSmdMode::SmdWrite);
    LOG_INFO << " PicoBEmcSmdWrite is being used " << endm;
    return kStOK;
  }
  else if ( (strcasecmp(SAttr("PicoBEmcSmdMode"), "") == 0) ||
	    (strcasecmp(SAttr("PicoBEmcSmdMode"), "PicoBEmcSmdSkip") == 0) ){
    setBEmcSmdMode(PicoBEmcSmdMode::SmdSkip);
    LOG_INFO << " PicoBEmcSmdSkip is being used " << endm;
    return kStOK;
  }
  return kStErr;
}

//_________________
Int_t StPicoDstMaker::InitRun(Int_t const runnumber) {

  if (StMaker::m_Mode == PicoIoMode::IoWrite) {
    if (!initMtd(runnumber)) {
      LOG_ERROR << " MTD initialization error!!! " << endm;
      return kStErr;
    }
  }
  return kStOK;
}

//_________________
Bool_t StPicoDstMaker::initMtd(Int_t const runnumber) {

  // Oct. 1st (approx. 273rd day) is the start of a new running year
  int year = runnumber / 1e6 + 1999;
  if ((runnumber % 1000000) / 1000 >= 273) year += 1;
  LOG_INFO << "Run = " << runnumber << " year = " << year << endm;

  // Obtain maps from DB
  for (Int_t i = 0; i < 30; ++i) {
    for (Int_t j = 0; j < 5; ++j) {
      mModuleToQT[i][j]    = -1;
      mModuleToQTPos[i][j] = -1;
    }
  }
  for (Int_t i = 0; i < 8; ++i) {
    for (Int_t j = 0; j < 8; ++j) {
      mQTtoModule[i][j]    = -1;
    }
  }

  LOG_INFO << "Retrieving mtdModuleToQTmap table from database ..." << endm;
  TDataSet* dataset = GetDataBase("Geometry/mtd/mtdModuleToQTmap");
  if(!dataset) {
    LOG_ERROR << "No database (mtdModuleToQTmap) was found" << endm;
    return kStErr;
  }
  St_mtdModuleToQTmap* mtdModuleToQTmap = static_cast<St_mtdModuleToQTmap*>(dataset->Find("mtdModuleToQTmap"));
  if (!mtdModuleToQTmap) {
    LOG_ERROR << "No mtdModuleToQTmap table was found in database" << endm;
    return kStErr;
  }
  mtdModuleToQTmap_st* mtdModuleToQTtable = static_cast<mtdModuleToQTmap_st*>(mtdModuleToQTmap->GetTable());
  if(!mtdModuleToQTtable) {
    LOG_ERROR << "No mtdModuleToQTtable was found in mtdModuleToQTmap" << endm;
    return kStErr;
  }

  for (Int_t i = 0; i < 30; ++i) {
    for (Int_t j = 0; j < 5; ++j) {
      Int_t index = i * 5 + j;
      Int_t qt = mtdModuleToQTtable->qtBoardId[index];
      Int_t channel = mtdModuleToQTtable->qtChannelId[index];
      mModuleToQT[i][j]    = qt;
      if (channel < 0) {
        mModuleToQTPos[i][j] = channel;
      }
      else {
        if (channel % 8 == 1) {
	  mModuleToQTPos[i][j] = 1 + channel / 8 * 2;
	}
        else {
	  mModuleToQTPos[i][j] = 2 + channel / 8 * 2;
	}
      }
      if (mModuleToQT[i][j] > 0 && mModuleToQTPos[i][j] > 0) {
        mQTtoModule[mModuleToQT[i][j] - 1][mModuleToQTPos[i][j] - 1] = j + 1;
      }
    } //for (Int_t j = 0; j < 5; ++j)
  } //for (Int_t i = 0; i < 30; ++i)

  // online slewing correction for QT board
  for (int j = 0; j < 8; ++j) {
    for (int i = 0; i < 16; ++i) {
      for (Int_t k = 0; k < 8; ++k) {
        mQTSlewBinEdge[j][i][k] = -1;
        mQTSlewCorr[j][i][k]    = -1;
      } //for (Int_t k = 0; k < 8; ++k)
    } //for (int i = 0; i < 16; ++i)
  } //for (int j = 0; j < 8; ++j)

  LOG_INFO << "Retrieving mtdQTSlewingCorr table from database ..." << endm;
  dataset = GetDataBase("Calibrations/mtd/mtdQTSlewingCorr");
  St_mtdQTSlewingCorr* mtdQTSlewingCorr = dataset ? static_cast<St_mtdQTSlewingCorr*>(dataset->Find("mtdQTSlewingCorr")) : nullptr;
  if (!mtdQTSlewingCorr) {
    LOG_ERROR << "No mtdQTSlewingCorr table found in database" << endm;
    return kStErr;
  }
  mtdQTSlewingCorr_st* mtdQTSlewingCorrtable = static_cast<mtdQTSlewingCorr_st*>(mtdQTSlewingCorr->GetTable());
  for (int j = 0; j < 4; ++j) {
    for (int i = 0; i < 16; ++i) {
      for (Int_t k = 0; k < 8; ++k) {
        Int_t index = j * 16 * 8 + i * 8 + k;
        mQTSlewBinEdge[j][i][k] = (int) mtdQTSlewingCorrtable->slewingBinEdge[index];
        mQTSlewCorr[j][i][k] = (int) mtdQTSlewingCorrtable->slewingCorr[index];
      } //for (Int_t k = 0; k < 8; ++k)
    } //for (int i = 0; i < 16; ++i)
  } //for (int j = 0; j < 4; ++j)
  if (year == 2016) {
    dataset = GetDataBase("Calibrations/mtd/mtdQTSlewingCorrPart2");
    St_mtdQTSlewingCorrPart2* mtdQTSlewingCorr2 = dataset ? static_cast<St_mtdQTSlewingCorrPart2*>(dataset->Find("mtdQTSlewingCorrPart2")) : nullptr;
    if (!mtdQTSlewingCorr2) {
      LOG_ERROR << "No mtdQTSlewingCorr2 table found in database for year " << year << endm;
      return kStErr;
    }
    mtdQTSlewingCorrPart2_st* mtdQTSlewingCorrtable2 = static_cast<mtdQTSlewingCorrPart2_st*>(mtdQTSlewingCorr2->GetTable());
    for (int j = 0; j < 4; ++j) {
      for (int i = 0; i < 16; ++i) {
	for (Int_t k = 0; k < 8; ++k) {
	  Int_t index = j * 16 * 8 + i * 8 + k;
	  mQTSlewBinEdge[j + 4][i][k] = (int) mtdQTSlewingCorrtable2->slewingBinEdge[index];
	  mQTSlewCorr[j + 4][i][k] = (int) mtdQTSlewingCorrtable2->slewingCorr[index];
	} //for (Int_t k = 0; k < 8; ++k)
      } //for (int i = 0; i < 16; ++i)
    } //for (int j = 0; j < 4; ++j)
  } // if (year == 2016)

  return kTRUE;
}

//_________________
Int_t StPicoDstMaker::Finish() {

  if (StMaker::m_Mode == PicoIoMode::IoRead) {
    closeRead();
  }
  else if (StMaker::m_Mode == PicoIoMode::IoWrite) {
    closeWrite();
    finishEmc();
  }
  return kStOK;
}

//_________________
Int_t StPicoDstMaker::openRead() {

  if (!mChain) mChain = new TChain("PicoDst");

  string const dirFile = mInputFileName.Data();
  if (dirFile.find(".list") != string::npos) {
    ifstream inputStream(dirFile.c_str());

    if (!inputStream) {
      LOG_ERROR << "ERROR: Cannot open list file " << dirFile << endm;
      return kStErr;
    }

    int nFile = 0;
    string file;
    size_t pos;
    while ( getline(inputStream, file) ) {
      // NOTE: our external formatters may pass "file NumEvents"
      //       Take only the first part
      //cout << "DEBUG found " <<  file << endl;
      pos = file.find_first_of(" ");
      if (pos!=string::npos ) file.erase(pos,file.length()-pos);
      //cout << "DEBUG found [" <<  file << "]" << endl;

      if (file.find(".picoDst.root") != string::npos) {
        TFile* ftmp = TFile::Open(file.c_str());
        if (ftmp && !ftmp->IsZombie() && ftmp->GetNkeys()) {
          LOG_INFO << " Read in picoDst file " << file << endm;
          mChain->Add(file.c_str());
          ++nFile;
        }

        if (ftmp) ftmp->Close();
      }
    } // while (getline(inputStream, file))

    LOG_INFO << " Total " << nFile << " files have been read in. " << endm;

  } else if (dirFile.find(".picoDst.root") != string::npos) {
    mChain->Add(dirFile.c_str());

  } else {
    LOG_WARN << " No good input file to read ... " << endm;
  }

  // Set branch addresses and pointers
  if (mChain) {
    setBranchAddresses(mChain);
    mChain->SetCacheSize(50e6);
    mChain->AddBranchToCache("*");
    mPicoDst->set(mPicoArrays);
  }

  return kStOK;
}

//_________________
void StPicoDstMaker::openWrite() {

  mOutputFile = new TFile(mOutputFileName.Data(), "RECREATE");
  LOG_INFO << " Output file: " << mOutputFileName.Data() << " created." << endm;
  mOutputFile->SetCompressionLevel(mCompression);
  int bufsize = mBufferSize;
  if (mSplit) bufsize /= 4;
  mTTree = new TTree("PicoDst", "StPicoDst", mSplit);
  mTTree->SetAutoSave(1000000);
  for (int i = 0; i < StPicoArrays::NAllPicoArrays; ++i) {
    if (mStatusArrays[i] == 0) {
      LOG_INFO << " Branch " << StPicoArrays::picoArrayNames[i] << " status is OFF! " << endm;
      continue;
    }

    mTTree->Branch(StPicoArrays::picoArrayNames[i], &mPicoArrays[i], bufsize, mSplit);
  }
}

//_________________
void StPicoDstMaker::initEmc() {

  // Create an instance of the StEmcPosition
  mEmcPosition = new StEmcPosition();
  // Fill the instance
  for (int i = 0; i < 4; ++i) {
    mEmcGeom[i] = StEmcGeom::getEmcGeom(detname[i].Data());
  }
}

//_________________
void StPicoDstMaker::buildEmcIndex() {

  // Retrieve a pointer to BEMC
  StEmcDetector* mEmcDet = mMuDst->emcCollection()->detector(kBarrelEmcTowerId);
  // Clean EmcIndex
  std::fill_n(mEmcIndex, sizeof(mEmcIndex) / sizeof(mEmcIndex[0]), nullptr);

  if (!mEmcDet) return;

  // Loop over 120 modules
  for (size_t iMod = 1; iMod <= mEmcDet->numberOfModules(); ++iMod) {

    StSPtrVecEmcRawHit& modHits = mEmcDet->module(iMod)->hits();

    // Each module has 40 towers (that should work)
    for (size_t iHit = 0; iHit < modHits.size(); ++iHit) {

      StEmcRawHit* rawHit = modHits[iHit];

      // If no tower information is available
      // then we skip the current tower
      if(!rawHit) continue;

      UInt_t softId = rawHit->softId(1);
      if (mEmcGeom[0]->checkId(softId) == 0) { // OK

        // Here we store one to one correspondence
        // between tower id (detector, module, eta, sub)
        // and its softId (numerical order of the tower)
        mEmcIndex[softId - 1] = rawHit;
      }
    } //for (size_t iHit = 0; iHit < modHits.size(); ++iHit)
  } //for (size_t iMod = 1; iMod <= mEmcDet->numberOfModules(); ++iMod)
}

//_________________
void StPicoDstMaker::finishEmc() {
  delete mEmcPosition;
  mEmcPosition = nullptr;

  std::fill_n(mEmcGeom, 4, nullptr);
}

//_________________
void StPicoDstMaker::Clear(char const*) {
  if (StMaker::m_Mode == PicoIoMode::IoRead)
    return;
  clearArrays();
}

//_________________
void StPicoDstMaker::closeRead() {
  delete mChain;
  mChain = nullptr;
}

//_________________
void StPicoDstMaker::closeWrite() {
  if (StMaker::m_Mode == PicoIoMode::IoWrite) {
    if (mOutputFile) {
      mOutputFile->Write();
      mOutputFile->Close();
    }
  }
}

//_________________
int StPicoDstMaker::Make() {

  int returnStarCode = kStOK;

  if (StMaker::m_Mode == PicoIoMode::IoWrite) {
    returnStarCode = MakeWrite();
  }
  else if (StMaker::m_Mode == PicoIoMode::IoRead) {
    returnStarCode = MakeRead();
  }

  return returnStarCode;
}

//_________________
Int_t StPicoDstMaker::MakeRead() {

  if (!mChain) {
    LOG_WARN << " No input files ... ! EXIT" << endm;
    return kStWarn;
  }

  int bytes = mChain->GetEntry(mEventCounter++);
  while( bytes <= 0) {
    if( mEventCounter >= mChain->GetEntriesFast() ) {
      return kStEOF;
    }

    LOG_WARN << "Encountered invalid entry or I/O error while reading event "
	     << mEventCounter << " from \"" << mChain->GetName() << "\" input tree\n";
    bytes = mChain->GetEntry(mEventCounter++);
  }

  fillEventHeader();

  return kStOK;
}

//_________________
Int_t StPicoDstMaker::MakeWrite() {

  StMaker::WhiteBoard("muDst", &mMuDst);

  if (!mMuDst) {
    LOG_ERROR << "No \"StMuDst\" object found in this event. It is usually created by StMuDstMaker" << endm;
    return kStErr;
  }

  // Retrieve pointer to StMuEvent
  StMuEvent* muEvent = mMuDst->event();

  if (!muEvent) {
    LOG_WARN << " No MuEvent " << endm;
    return kStWarn;
  }

  int const originalVertexId = mMuDst->currentVertexIndex();
  if (!selectVertex()) {
    LOG_INFO << "Vertex is not valid" << endm;
    mMuDst->setVertexIndex(originalVertexId);
    return kStOK;
  }

  mBField = muEvent->magneticField();

  // Get Emc collection
  mEmcCollection = mMuDst->emcCollection();

  if (mEmcCollection) {
    // Build EmcIndex before ::fillTracks()
    buildEmcIndex();
    // Fill BTOW hits only if ::buildEmcIndex() has been called for this event
    fillBTowHits();
  }

  // Fill StPicoEvent members
  fillTracks();
  fillEvent();
  fillEmcTrigger();
  fillMtdTrigger();
  fillBTofHits();
  fillMtdHits();
  fillEpdHits();
  fillBbcHits();
  fillETofHits();

  // Could be a good idea to move this call to Init() or InitRun()
  StFmsDbMaker* fmsDbMaker = static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));

  if (fmsDbMaker) {
    StMuFmsUtil::recoverMuFmsCollection(*mMuDst, fmsDbMaker);
  }

  mFmsFiller.fill(*mMuDst);

  if (Debug()) mPicoDst->printTracks();

  mTTree->Fill();

  mMuDst->setVertexIndex(originalVertexId);

  return kStOK;
}

//_________________
void StPicoDstMaker::fillEventHeader() const {
  StPicoEvent* event=StPicoDst::event();
  if(!event) {
    return;
  }

  /// Get or create
  StEvtHddr* header=GetEvtHddr();
  header->SetRunNumber(event->runId());
  header->SetEventNumber(event->eventId());
  header->SetGMTime( (UInt_t) (event->time()) );
}

//_________________
void StPicoDstMaker::fillTracks() {
  // We save primary tracks associated with the selected primary vertex only
  // don't use StMuTrack::primary(), it returns primary tracks associated with
  // all vertices
  std::unordered_map<unsigned int, unsigned int> index2Primary;

  // Retrieve number of primary tracks
  Int_t nPrimarys = mMuDst->numberOfPrimaryTracks();

  // Loop over primary trakcs
  for (int i = 0; i < nPrimarys; ++i) {
    StMuTrack* pTrk = (StMuTrack*)mMuDst->primaryTracks(i);
    if(!pTrk) continue;

    index2Primary[pTrk->id()] = i;
  } // for (int i = 0; i < nPrimarys; ++i)

  // Retrieve number of global tracks
  Int_t nGlobals = mMuDst->numberOfGlobalTracks();

  // Loop over global tracks
  for (int i = 0; i < nGlobals; ++i) {

    // Retrieve i-th global track
    StMuTrack* gTrk = (StMuTrack*)mMuDst->globalTracks(i);

    // Check the existence and track type
    if(!gTrk) continue;
    if(gTrk->type()!=global) continue;

    // Obtain primary track that corresponds to gTrk
    StMuTrack const* const pTrk = index2Primary.find(gTrk->id()) != index2Primary.end() ?
      (StMuTrack*)mMuDst->primaryTracks(index2Primary[gTrk->id()]) : nullptr;

    // Check that if primary track exists, it should have a primary flag
    if(pTrk) {
      // Check track type and global2primary correspondence
      if (pTrk->type() != primary) continue;
      if (pTrk->id() != gTrk->id()) continue;
    }

    // Global track should correspondence to some covariance information
    if(gTrk->index2Cov() < 0) continue;

    // Obtain DCA information (covariance matrix)
    StDcaGeometry* dcaG = mMuDst->covGlobTracks(gTrk->index2Cov());
    if(!dcaG) {
      LOG_WARN << "No dca Geometry for this track !!! " << i << endm;
      continue;
    }

    // Obtain size of the picoTrack array
    int counter = mPicoArrays[StPicoArrays::Track]->GetEntries();

    // Create new empty StPicoTrack and add it into collection
    new((*(mPicoArrays[StPicoArrays::Track]))[counter]) StPicoTrack();

    // Return pointer to the picoTrack
    StPicoTrack* picoTrk = (StPicoTrack*)mPicoArrays[StPicoArrays::Track]->At(counter);

    // Fill basic track information here
    picoTrk->setId( gTrk->id() );
    picoTrk->setChi2( gTrk->chi2() );
    // Store dE/dx in KeV/cm and its error. Next lines are needed
    // in order to store the values obrained with the same method:
    // either truncated mean of fit. Starting SL14 the default was
    // changed from thuncated mean to fit.
    static TString Production(mMuDst->event()->runInfo().productionVersion());
    static TString prodYear(Production.Data()+2,2);
    static Int_t defY = prodYear.Atoi();
    static StDedxMethod defaultdEdxMethod = (defY > 0 && defY < 14) ? kTruncatedMeanId : kLikelihoodFitId;
    if (defaultdEdxMethod == kTruncatedMeanId) {
      picoTrk->setDedx( gTrk->probPidTraits().dEdxTruncated() );
      picoTrk->setDedxError( gTrk->probPidTraits().dEdxErrorTruncated() );
    }
    else {
      picoTrk->setDedx( gTrk->probPidTraits().dEdxFit() );
      picoTrk->setDedxError( gTrk->probPidTraits().dEdxErrorFit() );
    }

    // Fill track's hit information
    picoTrk->setNHitsDedx( gTrk->nHitsDedx() );

    Int_t flag = gTrk->flag();
    if (flag / 100 < 7) { // TPC tracks
      picoTrk->setNHitsFit( gTrk->nHitsFit(kTpcId) * gTrk->charge() );
      picoTrk->setNHitsMax( gTrk->nHitsPoss(kTpcId) );
    }
    else { // FTPC tracks
      if (gTrk->helix().momentum(mBField * kilogauss).pseudoRapidity() > 0.) {
	//West FTPC
	picoTrk->setNHitsFit( gTrk->nHitsFit(kFtpcWestId) * gTrk->charge() );
	picoTrk->setNHitsMax( gTrk->nHitsPoss(kFtpcWestId) );
      }
      else {
	// East FTPC
	picoTrk->setNHitsFit( gTrk->nHitsFit(kFtpcEastId) * gTrk->charge() );
	picoTrk->setNHitsMax( gTrk->nHitsPoss(kFtpcEastId) );
      }
    }

    // Fill nSigmaPID information
    picoTrk->setNSigmaElectron( gTrk->nSigmaElectron() );
    picoTrk->setNSigmaPion( gTrk->nSigmaPion() );
    picoTrk->setNSigmaKaon( gTrk->nSigmaKaon() );
    picoTrk->setNSigmaProton( gTrk->nSigmaProton() );

    // Fill topology map (2 words)
    for(UInt_t iMap=0; iMap<2; iMap++) {
      picoTrk->setTopologyMap( iMap, gTrk->topologyMap().data(iMap) );
    }

    // Calculate global momentum and position at point of DCA to the pVtx
    // at it is done in MuDst
    if( dcaG ) {
      const StThreeVectorF &pvert =  mMuDst->primaryVertex()->position();
      Double_t vtx[3] = {pvert[0],pvert[1],pvert[2]};
      THelixTrack thelix =  dcaG->thelix();
      thelix.Move(thelix.Path(vtx));
      const Double_t *pos = thelix.Pos();
      const Double_t *mom = thelix.Dir();
      StThreeVectorF momAtDca(mom[0], mom[1], mom[2]);
      momAtDca *= dcaG->momentum().mag();
      picoTrk->setGlobalMomentum( momAtDca[0], momAtDca[1], momAtDca[2] );
      picoTrk->setOrigin( pos[0], pos[1], pos[2] );
    }
    else {
      StPhysicalHelixD gHelix = dcaG->helix();
      gHelix.moveOrigin( gHelix.pathLength( mMuDst->primaryVertex()->position() ) );
      StThreeVectorF globMom =  gHelix.momentum( mBField * kilogauss );
      StThreeVectorF globOrigin = gHelix.origin();
      picoTrk->setGlobalMomentum( globMom.x(),
				  globMom.y(),
				  globMom.z() );
      picoTrk->setOrigin( globOrigin.x(),
			  globOrigin.y(),
			  globOrigin.z() );
    }

    /*
    // Old picoDst style
    StPhysicalHelixD gHelix = dcaG->helix();
    gHelix.moveOrigin( gHelix.pathLength( mMuDst->primaryVertex()->position() ) );
    StThreeVectorF globMom =  gHelix.momentum( mBField * kilogauss );
    StThreeVectorF globOrigin = gHelix.origin();
    picoTrk->setGlobalMomentum( globMom.x(),
				globMom.y(),
				globMom.z() );
    picoTrk->setOrigin( globOrigin.x(),
			globOrigin.y(),
			globOrigin.z() );
    */

    // Save primary track momentum
    picoTrk->setPrimaryMomentum( 0., 0., 0. ); //Should be initialized in the constructor
    if (pTrk) {
      picoTrk->setPrimaryMomentum( pTrk->momentum().x(),
				   pTrk->momentum().y(),
				   pTrk->momentum().z() );
    } //if(pTrk)


    // Fill covariance matrix. Fill it only if the flag was set up
    if( mCovMtxMode == PicoCovMtxMode::Write ) {

      // Retrieve index in the PicoTrackCovMatrix pico array
      Int_t cov_index = mPicoArrays[StPicoArrays::TrackCovMatrix]->GetEntries();
      // Create new empty instance. It must be initialized with 0 (default constructor)
      new((*(mPicoArrays[StPicoArrays::TrackCovMatrix]))[cov_index]) StPicoTrackCovMatrix();
      // Make a pointer to the new element
      StPicoTrackCovMatrix *covMatrix = (StPicoTrackCovMatrix*)mPicoArrays[StPicoArrays::TrackCovMatrix]->At(cov_index);

      if( dcaG ) {
	// Retrieve pointers to the covariance matrix values and errors
	const Float_t* parDCA = dcaG->params();
	const Float_t* errorMatrix = dcaG->errMatrix();

	// Set covariance matrix values
	covMatrix->setImp( parDCA[0] );
	covMatrix->setZ( parDCA[1] );
	covMatrix->setPsi( parDCA[2] );
	covMatrix->setPti( parDCA[3] );
	covMatrix->setTan( parDCA[4] );
	covMatrix->setCurv( parDCA[5] );

	Float_t SigmaArr[5];
	Float_t CorrArr[10];
	Int_t ii = 0;
	for(Int_t iVal=0; iVal<5; iVal++) {
	  SigmaArr[iVal] = TMath::Sqrt( errorMatrix[ii] );
	  for(Int_t j=0; j<iVal; j++) {
	    Int_t ij = (ii - iVal - 1) + (j + 1);
	    Int_t ij1 = ij - iVal;
	    CorrArr[ij1] = errorMatrix[ij] / ( SigmaArr[iVal] * SigmaArr[j] );
	  }
	  ii += iVal+2;
	} //for(Int_t iVal = 0; iVal < 5; iVal++)

	// Set sigma and correlation arrays
	covMatrix->setSigmas( SigmaArr );
	covMatrix->setCorrelations( CorrArr );
      } //if(dcaG)
    } //if( mCovMtxMode == PicoCovMtxMode::Write )

    // Fill pid traits

    // BEMC information
    if (mEmcCollection) {
      Int_t id = -1;
      Int_t adc0;
      Float_t e[5];
      Float_t dist[4];
      Int_t nhit[2];
      Int_t ntow[3];

      getBEMC(gTrk, &id, &adc0, e, dist, nhit, ntow);

      if (id >= 0) {
        Int_t bemc_index = mPicoArrays[StPicoArrays::BEmcPidTraits]->GetEntries();
        new((*(mPicoArrays[StPicoArrays::BEmcPidTraits]))[bemc_index]) StPicoBEmcPidTraits(counter, id, adc0, e, dist, nhit, ntow);
        picoTrk->setBEmcPidTraitsIndex(bemc_index);
      }
    } //if (mEmcCollection)

    // TOF information
    if (gTrk->tofHit()) {

      // Retrieve index in the PicoBTofPidTraits pico array
      Int_t btof_index = mPicoArrays[StPicoArrays::BTofPidTraits]->GetEntries();
      // Create new empty instance
      new((*(mPicoArrays[StPicoArrays::BTofPidTraits]))[btof_index]) StPicoBTofPidTraits();
      //new((*(mPicoArrays[StPicoArrays::BTofPidTraits]))[btof_index]) StPicoBTofPidTraits(gTrk, pTrk, counter);

      // Return pointer to the current trait
      StPicoBTofPidTraits *btofPidTraits =
	(StPicoBTofPidTraits*)mPicoArrays[StPicoArrays::BTofPidTraits]->At(btof_index);

      // Fill traits information

      // Fill index of the corresponding pico track
      btofPidTraits->setTrackIndex(counter);

      // Retrive and store btof hit information:
      // cellId keeps: (tray-1)*192+(module-1)*6+(cell-1): -1 - no match
      StMuBTofHit *btofHit = (StMuBTofHit*)gTrk->tofHit();
      Int_t tray         = btofHit->tray();
      Int_t module       = btofHit->module();
      Int_t cell         = btofHit->cell();
      btofPidTraits->setBTofCellId(tray, module, cell);
      btofPidTraits->setBTofMatchFlag( (UChar_t)(gTrk->btofPidTraits().matchFlag()) );

      // Time-of-flight and beta
      btofPidTraits->setTOF( gTrk->btofPidTraits().timeOfFlight() );
      btofPidTraits->setBeta( gTrk->btofPidTraits().beta() );
      // This was the original way (commented line below).
      // I think it was wrong and changed it to the line above
      // btofPidTraits->setBeta( ( (pTrk) ? pTrk->btofPidTraits().beta() : -999. ) );

      // Retrieve and store hit position
      StThreeVectorF pos = gTrk->btofPidTraits().position();
      btofPidTraits->setHitPositionXYZ( pos.x(), pos.y(), pos.z() );

      // Retrieve and store local Y and Z coordinates
      btofPidTraits->setYLocal( gTrk->btofPidTraits().yLocal() );
      btofPidTraits->setZLocal( gTrk->btofPidTraits().zLocal() );

      // Set index of the corresponding btofPidTrait to the pico track
      picoTrk->setBTofPidTraitsIndex(btof_index);
    } //if (gTrk->tofHit())

        // ETOF information
    if (gTrk->etofHit()) {

      // Retrieve index in the PicoETofPidTraits pico array
      Int_t etof_index = mPicoArrays[StPicoArrays::ETofPidTraits]->GetEntries();
      // Create new empty instance
      new((*(mPicoArrays[StPicoArrays::ETofPidTraits]))[etof_index]) StPicoETofPidTraits();

      // Return pointer to the current trait
      StPicoETofPidTraits *etofPidTraits =
	(StPicoETofPidTraits*)mPicoArrays[StPicoArrays::ETofPidTraits]->At( etof_index );

      // Fill traits information:

      // Fill index of the corresponding pico track
      etofPidTraits->setTrackIndex( counter );

      // Hit index is properly set later in the fillETofHits() function
      // --> for now temporarily save the StMuTrack id that will
      //     be used to map to the index of StPicoETofHits in the StPicoArray --
      etofPidTraits->setHitIndex( gTrk->id() );

      /*
      StMuETofHit* hit = (StMuETofHit*) gTrk->etofHit();
      LOG_INFO << "corresponding muHit: " << hit->sector()
               << " " << hit->zPlane() << " " << hit->counter()
               << " " << hit->localX() << " " << hit->localY()
               << " " << hit->time()   << " " << hit->totalTot()
               << endm;
      */

      // Retrive and store etof pid traits information:
      // Matching information
      etofPidTraits->setMatchFlag( (UChar_t)(gTrk->etofPidTraits().matchFlag()) );

      // Time-of-flight and beta
      etofPidTraits->setTof(  gTrk->etofPidTraits().timeOfFlight() );
      etofPidTraits->setBeta( gTrk->etofPidTraits().beta() );

      // Retrieve and store position of track crossing with etof volume
      StThreeVectorF pos = gTrk->etofPidTraits().position();
      etofPidTraits->setCrossingPos( pos.x(), pos.y(), pos.z() );

      // Retrieve and store local Y and Z coordinates
      etofPidTraits->setDeltaX( gTrk->etofPidTraits().deltaX() );
      etofPidTraits->setDeltaY( gTrk->etofPidTraits().deltaY() );

      // Set index of the corresponding etofPidTrait to the pico track
      picoTrk->setETofPidTraitsIndex( etof_index );
    } //if (gTrk->etofHit())

    // MTD information
    if (gTrk->mtdHit()) {

      // Retrieve the index in the MtdPidTraits array
      Int_t mtd_index = mPicoArrays[StPicoArrays::MtdPidTraits]->GetEntries();
      new((*(mPicoArrays[StPicoArrays::MtdPidTraits]))[mtd_index]) StPicoMtdPidTraits();
      //new((*(mPicoArrays[StPicoArrays::MtdPidTraits]))[mtd_index]) StPicoMtdPidTraits(gTrk->mtdHit(), &(gTrk->mtdPidTraits()), counter);

      StPicoMtdPidTraits *mtdPidTraits =
	(StPicoMtdPidTraits*)mPicoArrays[StPicoArrays::MtdPidTraits]->At(mtd_index);

      // Store index of the corresponding pico track
      mtdPidTraits->setTrackIndex(counter);
      // Store mtdPidTraits related information
      mtdPidTraits->setMatchFlag( (Char_t)gTrk->mtdPidTraits().matchFlag() );
      mtdPidTraits->setDeltaY( gTrk->mtdPidTraits().deltaY() );
      mtdPidTraits->setDeltaZ( gTrk->mtdPidTraits().deltaZ() );
      mtdPidTraits->setDeltaTimeOfFlight( gTrk->mtdPidTraits().timeOfFlight() -
					  gTrk->mtdPidTraits().expTimeOfFlight() );
      mtdPidTraits->setBeta( gTrk->mtdPidTraits().pathLength() /
			     gTrk->mtdPidTraits().expTimeOfFlight() * 1e9 / c_light );
      // Store mtdPidTraits hit related information as:
      // (backleg-1) * 60 + (module-1) * 12 + cell
      mtdPidTraits->setHitChannel( gTrk->mtdHit()->backleg(),
				   gTrk->mtdHit()->module(),
				   gTrk->mtdHit()->cell() );

      // Set index of the mtdPidTrait to the pico track
      picoTrk->setMtdPidTraitsIndex(mtd_index);
    } //if (gTrk->mtdHit())

  } //for (int i = 0; i < nGlobals; ++i)
}

//_________________
bool StPicoDstMaker::getBEMC(const StMuTrack* t, int* id, int* adc, float* ene, float* d, int* nep, int* towid) {

  *id = -1;
  *adc = 0;

  std::fill(ene, ene+5, 0.);
  std::fill(d, d+4, 1.e9);
  std::fill(nep, nep+2, 0);
  std::fill(towid, towid+3, -1);

  if (!mEmcCollection) {
    LOG_WARN << " No Emc Collection for this event " << endm;
    return kFALSE;
  }

  StThreeVectorD position, momentum;
  StThreeVectorD positionBSMDE, momentumBSMDE;
  StThreeVectorD positionBSMDP, momentumBSMDP;

  double magneticField = mBField * kilogauss / tesla; // in Tesla

  bool ok       = false;
  bool okBSMDE  = false;
  bool okBSMDP  = false;

  if (mEmcPosition) {
    ok      = mEmcPosition->projTrack(&position,      &momentum,      t, magneticField, mEmcGeom[0]->Radius());
    okBSMDE = mEmcPosition->projTrack(&positionBSMDE, &momentumBSMDE, t, magneticField, mEmcGeom[2]->Radius());
    okBSMDP = mEmcPosition->projTrack(&positionBSMDP, &momentumBSMDP, t, magneticField, mEmcGeom[3]->Radius());
  }

  if (!ok) {
    LOG_WARN << " Projection failed for this track ... " << endm;
    return kFALSE;
  }

  if (ok && okBSMDE && okBSMDP) {

    Int_t mod = 0, eta = 0, sub = 0;
    StSPtrVecEmcPoint& bEmcPoints = mEmcCollection->barrelPoints();
    int index = 0;
    float mindist = 1.e9;
    mEmcGeom[0]->getBin(positionBSMDP.phi(), positionBSMDE.pseudoRapidity(), mod, eta, sub); //project on SMD plan

    // Loop over all BEMC measurements, aka "points"
    for (StSPtrVecEmcPointIterator it = bEmcPoints.begin(); it != bEmcPoints.end(); ++it, ++index) {

      bool associated = false;
      // Consider only BEMC clusters
      StPtrVecEmcCluster& bEmcClusters = (*it)->cluster(kBarrelEmcTowerId);
      if (bEmcClusters.size() == 0) continue;
      if (bEmcClusters[0] == NULL) continue;

      // Loop over all BEMC clusters
      for (StPtrVecEmcClusterIterator cIter = bEmcClusters.begin(); cIter != bEmcClusters.end(); ++cIter) {

        StPtrVecEmcRawHit& bEmcHits = (*cIter)->hit();

        // Loop over all hits/towers in the BEMC cluster
        for (StPtrVecEmcRawHitIterator hIter = bEmcHits.begin(); hIter != bEmcHits.end(); ++hIter) {

          // Find BEMC hit matching the track projection to BEMC
          if (mod == (Int_t)(*hIter)->module() && eta == (Int_t)(*hIter)->eta() && sub == (Int_t)(*hIter)->sub()) {
            associated = true;
            break;
          }
        }

        if (associated) {

          // Loop over all hits/towers in the BEMC cluster again
          for (StPtrVecEmcRawHitIterator hitit = bEmcHits.begin(); hitit != bEmcHits.end(); ++hitit) {
            // Save the highest energy among the towers in the BEMC cluster to ene[0]
            if ((*hitit)->energy() > ene[0]) ene[0] = (*hitit)->energy();
            // Save the highest ADC among the towers in the BEMC cluster to adc
            if ((int)(*hitit)->adc() > (*adc)) *adc = (*hitit)->adc();
          }
        } //if (associated)
      } //for (StPtrVecEmcClusterIterator cIter = bEmcClusters.begin(); cIter != bEmcClusters.end(); ++cIter)

      StPtrVecEmcCluster& smdeClusters = (*it)->cluster(kBarrelSmdEtaStripId);
      StPtrVecEmcCluster& smdpClusters = (*it)->cluster(kBarrelSmdPhiStripId);

      if (associated) {
        *id = index;
        ene[1] = ene[1] + (*it)->energy(); //use point's energy, not tower cluster's energy

        float deltaphi = (*it)->position().phi() - positionBSMDP.phi();
        if (deltaphi >= TMath::Pi()) deltaphi = deltaphi - TMath::TwoPi();
        if (deltaphi < -TMath::Pi()) deltaphi = deltaphi + TMath::TwoPi();

        float rsmdp = mEmcGeom[3]->Radius();
        float pointz = (*it)->position().z();
        float deltaz = pointz - positionBSMDE.z();
        if (sqrt(deltaphi * deltaphi * rsmdp * rsmdp + deltaz * deltaz) < mindist) {
          d[1] = deltaphi;
          d[0] = deltaz;
          if (smdeClusters.size() >= 1) nep[0] = smdeClusters[0]->nHits();
          if (smdpClusters.size() >= 1) nep[1] = smdpClusters[0]->nHits();
          mindist = sqrt(deltaphi * deltaphi * rsmdp * rsmdp + deltaz * deltaz);
        }
      }//associated
    } //for (StSPtrVecEmcPointIterator it = bEmcPoints.begin(); it != bEmcPoints.end(); ++it, ++index)

  } // end if (ok && okBSMDE && okBSMDP)

  // Get BEMC tower energy from matched tower + 2 nearest towers
  int towerId = 0;
  int localTowerId = -1;
  int localId1 = -1;
  int localId2 = -1;
  double energy1 = 0, energy2 = 0;
  double energyTemp = 0;
  double dist1 = 1000, dist2 = 1000;
  double distTemp = 0;
  Float_t etaTemp = 0, phiTemp = 0;

  if (mEmcGeom[0]->getId(position.phi(), position.pseudoRapidity(), towerId) == 1) return kTRUE;

  for (int ieta = -1; ieta < 2; ++ieta) {
    for (int iphi = -1; iphi < 2; ++iphi) {
      localTowerId++;//loops from 0 to 8
      int nextTowerId = mEmcPosition->getNextTowerId(towerId, ieta, iphi);
      if (nextTowerId < 1 || nextTowerId > 4800) continue;
      StEmcRawHit* emcHit = mEmcIndex[nextTowerId - 1];
      if (emcHit == 0) continue;
      if (emcHit->energy() < 0.2) continue; // don't include any noise tower

      if (ieta == 0 && iphi == 0) {
        mEmcGeom[0]->getEta(nextTowerId, etaTemp);
        mEmcGeom[0]->getPhi(nextTowerId, phiTemp);
        ene[2] = emcHit->energy();
        d[2] = position.pseudoRapidity() - etaTemp;
        d[3] = position.phi() - phiTemp;
      }
      else {
        energyTemp = emcHit->energy();
        mEmcGeom[0]->getEta(nextTowerId, etaTemp);
        mEmcGeom[0]->getPhi(nextTowerId, phiTemp);
        distTemp = sqrt((etaTemp - position.pseudoRapidity()) * (etaTemp - position.pseudoRapidity()) + (phiTemp - position.phi()) * (phiTemp - position.phi()));

	// In case the new tower is closer to the matched tower
	// than the other closest one we swap them.
	// i.e. previously closest tower will become the second
	// closest tower
        if (distTemp < dist1) {
          dist2 = dist1;
          dist1 = distTemp;
          energy2 = energy1;
          energy1 = energyTemp;
	  localId2 = localId1;
          localId1 = localTowerId;
        }
        else if (distTemp < dist2) {
          dist2 = distTemp;
          energy2 = energyTemp;
          localId2 = localTowerId;
        }
      } //else
    } //for (int iphi = -1; iphi < 2; ++iphi)
  } //for (int ieta = -1; ieta < 2; ++ieta)
  towid[0] = towerId;
  ene[3] = energy1;     //closest tower
  towid[1] = localId1;
  ene[4] = energy2;     //2nd closest tower
  towid[2] = localId2;

  LOG_DEBUG << " ====== BEMC results ====== " << "\n"
            << " Energy = " << ene[0] << " " << ene[1] << " " << ene[2] << " " << ene[3] << " " << ene[4] << "\n"
            << " BSMD = " << nep[0] << " " << nep[1] << "\n"
            << " TowerId = " << towid[0] << " " << towid[1] << " " << towid[2] << endm;

  return kTRUE;
}

//_________________
void StPicoDstMaker::fillEvent() {

  // Obtain event iterator
  int counter = mPicoArrays[StPicoArrays::Event]->GetEntries();
  // Create empty pico event
  new((*(mPicoArrays[StPicoArrays::Event]))[counter]) StPicoEvent();

  // Return pointer to the latest (just created) pico event
  StPicoEvent *picoEvent = (StPicoEvent*)mPicoArrays[StPicoArrays::Event]->At(counter);

  // Retrieve muEvent
  StMuEvent* ev = mMuDst->event();

  // Set event parameters
  picoEvent->setRunId( ev->runNumber() );
  picoEvent->setEventId( ev->eventNumber() );
  picoEvent->setFillId( ev->runInfo().beamFillNumber( blue ) );
  picoEvent->setBField( ev->magneticField() );
  picoEvent->setTime( ev->eventInfo().time() );

  // Set primary vertex information
  picoEvent->setPrimaryVertexPosition( ev->primaryVertexPosition().x(),
				       ev->primaryVertexPosition().y(),
				       ev->primaryVertexPosition().z() );
  picoEvent->setPrimaryVertexPositionError( ev->primaryVertexErrors().x(),
					    ev->primaryVertexErrors().y(),
					    ev->primaryVertexErrors().z() );

  if( StMuPrimaryVertex *pv = mMuDst->primaryVertex() ) {
    picoEvent->setPrimaryVertexRanking( pv->ranking() );
    picoEvent->setNumberOfBEMCMatch( pv->nBEMCMatch() );
    picoEvent->setNumberOfBTOFMatch( pv->nBTOFMatch() );
  }

  // Copy trigger IDs of the current event
  picoEvent->setTriggerIds( ev->triggerIdCollection().nominal().triggerIds() );

  // Save various multiplicities
  picoEvent->setRefMultFtpcEast( ev->refMultFtpcEast() );
  picoEvent->setRefMultFtpcWest( ev->refMultFtpcWest() );
  picoEvent->setRefMultPos( ev->refMultPos() );
  picoEvent->setRefMultNeg( ev->refMultNeg() );

  {
    using namespace StPicoUtilities;
    auto custom_refMult = StPicoUtilities::calculateRefMult(*mMuDst);
    picoEvent->setRefMult2NegEast( custom_refMult[RefMult2NegEast] );
    picoEvent->setRefMult2PosEast( custom_refMult[RefMult2PosEast] );
    picoEvent->setRefMult2NegWest( custom_refMult[RefMult2NegWest] );
    picoEvent->setRefMult2PosWest( custom_refMult[RefMult2PosWest] );
    picoEvent->setRefMult3NegEast( custom_refMult[RefMult3NegEast] );
    picoEvent->setRefMult3PosEast( custom_refMult[RefMult3PosEast] );
    picoEvent->setRefMult3NegWest( custom_refMult[RefMult3NegWest] );;
    picoEvent->setRefMult3PosWest( custom_refMult[RefMult3PosWest] );
    picoEvent->setRefMult4NegEast( custom_refMult[RefMult4NegEast] );
    picoEvent->setRefMult4PosEast( custom_refMult[RefMult4PosEast] );
    picoEvent->setRefMult4NegWest( custom_refMult[RefMult4NegWest] );
    picoEvent->setRefMult4PosWest( custom_refMult[RefMult4PosWest] );
    picoEvent->setRefMultHalfNegEast( custom_refMult[RefMultHalfNegEast] );
    picoEvent->setRefMultHalfPosEast( custom_refMult[RefMultHalfPosEast] );
    picoEvent->setRefMultHalfNegWest( custom_refMult[RefMultHalfNegWest] );
    picoEvent->setRefMultHalfPosWest( custom_refMult[RefMultHalfPosWest] );
  }

  picoEvent->setGRefMult( ev->grefmult() );
  picoEvent->setNumberOfGlobalTracks( mMuDst->numberOfGlobalTracks() );
  picoEvent->setbTofTrayMultiplicity( ev->btofTrayMultiplicity() );
  picoEvent->setETofHitMultiplicity( ev->etofHitMultiplicity() );
  picoEvent->setETofDigiMultiplicity( ev->etofDigiMultiplicity() );

  // Store number of hits in HFT
  picoEvent->setNHitsHFT( 0, ev->numberOfPxlInnerHits() );
  picoEvent->setNHitsHFT( 1, ev->numberOfPxlOuterHits() );
  picoEvent->setNHitsHFT( 2, ev->numberOfIstHits() );
  picoEvent->setNHitsHFT( 3, ev->numberOfSsdHits() );

  // If TOF information is available
  if( StBTofHeader *header = mMuDst->btofHeader() ) {
    picoEvent->setNVpdHitsEast( header->numberOfVpdHits(east) );
    picoEvent->setNVpdHitsWest( header->numberOfVpdHits(west) );
    picoEvent->setNTofT0( header->nTzero() );
    picoEvent->setVzVpd( header->vpdVz() );
  }

  // ZDC and BBC background rates
  picoEvent->setZDCx( ev->runInfo().zdcCoincidenceRate() );
  picoEvent->setBBCx( ev->runInfo().bbcCoincidenceRate() );
  picoEvent->setBackgroundRate( ev->runInfo().backgroundRate() );
  picoEvent->setBbcBlueBackgroundRate( ev->runInfo().bbcBlueBackgroundRate() );
  picoEvent->setBbcYellowBackgroundRate( ev->runInfo().bbcYellowBackgroundRate() );
  picoEvent->setBbcEastRate( ev->runInfo().bbcEastRate() );
  picoEvent->setBbcWestRate( ev->runInfo().bbcWestRate() );
  picoEvent->setZdcEastRate( ev->runInfo().zdcEastRate() );
  picoEvent->setZdcWestRate( ev->runInfo().zdcWestRate() );

  // ZDC detailed information
  StZdcTriggerDetector &ZDC = ev->zdcTriggerDetector();
  picoEvent->setZdcSumAdcEast( ZDC.adcSum(east) );
  picoEvent->setZdcSumAdcWest( ZDC.adcSum(west) );

  // Loop over all ZDC strips
  for(int iStrip=1; iStrip<9; ++iStrip) {
    if( ZDC.zdcSmd(east, 1, iStrip) ) {
      picoEvent->setZdcSmdEastHorizontal( (iStrip-1), ZDC.zdcSmd(east, 1, iStrip) );
    }
    if( ZDC.zdcSmd(east, 0, iStrip) ) {
      picoEvent->setZdcSmdEastVertical( (iStrip-1), ZDC.zdcSmd(east, 0, iStrip) );
    }
    if( ZDC.zdcSmd(west, 1, iStrip) ) {
      picoEvent->setZdcSmdWestHorizontal( (iStrip-1), ZDC.zdcSmd(west, 1, iStrip) );
    }
    if( ZDC.zdcSmd(west, 0, iStrip) ) {
      picoEvent->setZdcSmdWestVertical( (iStrip-1), ZDC.zdcSmd(west, 0, iStrip) );
    }
  } //for(int iStrip=1; iStrip<9; ++iStrip)

  // BBC detailed information
  StBbcTriggerDetector bbc = ev->bbcTriggerDetector();

  // Loop over all PMTs
  for(UInt_t iPMT=0; iPMT<bbc.numberOfPMTs(); ++iPMT) {

    UInt_t const eastWest = (iPMT<24) ? 0 : 1;  // East: 0-23, West: 24-47
    UInt_t const pmtId = iPMT % 24;             // pmtId: 0-23

    if( eastWest==0 ) { //East
      picoEvent->setBbcAdcEast( pmtId, bbc.adc(iPMT) );
    }
    else { //West
      picoEvent->setBbcAdcWest( pmtId, bbc.adc(iPMT) );
    }
  } //for(UInt_t iPMT=0; iPMT<bbc.numberOfPMTs(); ++iPMT)

  // Set bunch crossing ID
  picoEvent->setBunchId( ev->l0Trigger().bunchCrossingId() );
}

//_________________
void StPicoDstMaker::fillEmcTrigger() {

  // Test for EMC trigger
  StTriggerSimuMaker* trigSimu = (StTriggerSimuMaker*)GetMaker("StarTrigSimu");
  if (!trigSimu) return;

  // BEMC High Tower trigger
  int bht0 = trigSimu->bemc->barrelHighTowerTh(0);
  int bht1 = trigSimu->bemc->barrelHighTowerTh(1);
  int bht2 = trigSimu->bemc->barrelHighTowerTh(2);
  int bht3 = trigSimu->bemc->barrelHighTowerTh(3);
  LOG_DEBUG << " bht thresholds " << bht0 << " " << bht1 << " " << bht2 << " " << bht3 << endm;
  for (int i = 0; i < 4; ++i) {
    mPicoDst->event()->setHighTowerThreshold(i, trigSimu->bemc->barrelHighTowerTh(i));
  }

  // Loop over all towers
  for(Int_t towerId=1; towerId<=4800; towerId++) {
    Int_t status;
    trigSimu->bemc->getTables()->getStatus(BTOW, towerId, status);
    Int_t adc = trigSimu->bemc->barrelHighTowerAdc(towerId);
    UChar_t flag = 0;
    vector<unsigned short> smdEHits;
    vector<unsigned short> smdPHits;

    if (adc > bht1) {
      LOG_DEBUG << " id = " << towerId << " adc = " << adc << endm;
      flag |= 1 << 1;
    }

    if (adc > bht2) {
      LOG_DEBUG << " id = " << towerId << " adc = " << adc << endm;
      flag |= 1 << 2;

      // Fill BEmc Smd hits. Fill only if the flag was set up
      if( mBEmcSmdMode == PicoBEmcSmdMode::SmdWrite ) {
	StEmcDetector* smde = mEmcCollection->detector(kBarrelSmdEtaStripId);
	StEmcDetector* smdp = mEmcCollection->detector(kBarrelSmdPhiStripId);

	if (smde && smdp) {
	  Float_t trigEta, trigPhi;
	  mEmcGeom[0]->getEtaPhi(towerId, trigEta, trigPhi);

	  // Length of tower side in eta/phi space
	  float towerSideLength = 0.05;
	  // Length of tower diagonal in eta/phi space
	  float towerDiagonal = towerSideLength*TMath::Sqrt(2);
	  // Maximum distance from trigger tower to include SMD hits
	  float radius = 1.5 * towerDiagonal;

	  // Loop over all SMD hits, but only save ones that are within some radius
	  // of the trigger tower

	  // Loop over eta strips
	  for (unsigned int i=1; i<=smde->numberOfModules(); i++) {

	    StEmcModule* module = smde->module(i);
	    StSPtrVecEmcRawHit& aHit = module->hits();

	    if (aHit.size() < 1) continue;

	    for (unsigned int j=0; j<aHit.size(); j++) {
	      Int_t id = -1;
	      Float_t smdPhi = 0.;
	      Float_t smdEta = 0.;
	      mEmcGeom[2]->getId(i, aHit[j]->eta(), aHit[j]->sub(), id);
	      mEmcGeom[2]->getEtaPhi(id, smdEta, smdPhi);
	      // Distance from trig in eta/phi space
	      float dr = TMath::Sqrt( (smdEta - trigEta) * (smdEta - trigEta) +
				      (smdPhi - trigPhi) * (smdPhi - trigPhi) );
	      if (radius >= dr) {
      		Int_t adc  = aHit[j]->adc();
      		Int_t counterSmde = mPicoArrays[StPicoArrays::BEmcSmdEHit]->GetEntries();
      		smdEHits.push_back( (unsigned char)counterSmde );
      		Float_t smdEnergy = aHit[j]->energy();
      		new( ( *(mPicoArrays[StPicoArrays::BEmcSmdEHit]) )[counterSmde]) StPicoBEmcSmdEHit(id, adc, smdEnergy);
	      } //if (radius >= dr)
	    } //for (int j = 0; j < aHit.size(); j++)
	  } //for (int i = 1; i <= smde->numberOfModules(); i++)

	  // Loop over phi strips
	  for (unsigned int i=1; i<=smdp->numberOfModules(); i++) {

	    StEmcModule* module = smdp->module(i);
	    StSPtrVecEmcRawHit& aHit = module->hits();

	    if (aHit.size() < 1) continue;

	    for (unsigned int j=0; j<aHit.size(); j++) {
	      Int_t id = -1;
	      Float_t smdPhi = 0.;
	      Float_t smdEta = 0.;
	      mEmcGeom[3]->getId(i, aHit[j]->eta(), aHit[j]->sub(), id);
	      mEmcGeom[3]->getEtaPhi(id, smdEta, smdPhi);
	      // Distance from trig in eta/phi space
	      Float_t dr = TMath::Sqrt( (smdEta - trigEta) * (smdEta - trigEta) +
					(smdPhi - trigPhi) * (smdPhi - trigPhi) );
	      if (radius >= dr) {
		int adc  = aHit[j]->adc();
		Int_t counterSmdp = mPicoArrays[StPicoArrays::BEmcSmdPHit]->GetEntries();
		smdPHits.push_back( (unsigned char)counterSmdp );
		Float_t smdEnergy = aHit[j]->energy();
		new( ( *(mPicoArrays[StPicoArrays::BEmcSmdPHit]) ) [counterSmdp]) StPicoBEmcSmdPHit(id, adc, smdEnergy);
	      }
	    } //for (int j = 0; j < aHit.size(); j++)
	  } //for (int i = 1; i <= smdp->numberOfModules(); i++)
	} //if (smde && smdp)
      } //if( mBEmcSmdHitMode == PicoBEmcSmdHitMode::SmdWrite )

    } //if (adc >bht2)

    if (adc > bht3) {
      LOG_DEBUG << " id = " << towerId << " adc = " << adc << endm;
      flag |= 1 << 3;
    }

    if (flag & 0xf) {
      Int_t counter = mPicoArrays[StPicoArrays::EmcTrigger]->GetEntries();
      new((*(mPicoArrays[StPicoArrays::EmcTrigger]))[counter]) StPicoEmcTrigger(flag, towerId, adc, smdEHits, smdPHits);
    }
  } //for(Int_t towerId=1; towerId<=4800; towerId++)

  // BEMC Jet Patch trigger threshold
  int const bjpth0 = trigSimu->bemc->barrelJetPatchTh(0);
  int const bjpth1 = trigSimu->bemc->barrelJetPatchTh(1);
  int const bjpth2 = trigSimu->bemc->barrelJetPatchTh(2);

  for (int i = 0; i < 3; ++i) {
    mPicoDst->event()->setJetPatchThreshold(i, trigSimu->bemc->barrelJetPatchTh(i));
  }

  // Loop over triggers
  for (int jp = 0; jp < 18; ++jp) {
    // BEMC: 12 Jet Patch + 6 overlap Jet Patches. As no EEMC information
    // is recorded in Pico tree, not EEMC trigger information also.
    int const jpAdc = trigSimu->bemc->barrelJetPatchAdc(jp);

    unsigned char flag = 0;
    if (jpAdc > bjpth0) {
      flag |= 1 << 4;
    }

    if (jpAdc > bjpth1) {
      flag |= 1 << 5;
    }

    if (jpAdc > bjpth2) {
      flag |= 1 << 6;
    }

    if (flag & 0x70) {
      int counter = mPicoArrays[StPicoArrays::EmcTrigger]->GetEntries();
      new((*(mPicoArrays[StPicoArrays::EmcTrigger]))[counter]) StPicoEmcTrigger(flag, jp, jpAdc);
    }
  } //for (int jp = 0; jp < 18; ++jp)
}

//_________________
void StPicoDstMaker::fillMtdTrigger() {

  // Retrieve index of the last element of the MTD trigger array
  int counter = mPicoArrays[StPicoArrays::MtdTrigger]->GetEntries();

  // Create empty MTDTrigger object and add it to pico array
  new((*(mPicoArrays[StPicoArrays::MtdTrigger]))[counter]) StPicoMtdTrigger();

  // Retrieve the last (new) MTD trigger object
  StPicoMtdTrigger *mtdTrigger = (StPicoMtdTrigger*)mPicoArrays[StPicoArrays::MtdTrigger]->At(counter);

  // Fill MTD trigger information

  StMuMtdHeader *muMtdHeader = mMuDst->mtdHeader();
  if(muMtdHeader) {
    mtdTrigger->setTHUBtime(0, 25 * ( muMtdHeader->triggerTime(1) & 0xfff ) );
    mtdTrigger->setTHUBtime(1, 25 * ( muMtdHeader->triggerTime(2) & 0xfff ) );
    mtdTrigger->setShouldHaveRejectEvent( muMtdHeader->shouldHaveRejectEvent() );
  }

  StTriggerData* trigger = const_cast<StTriggerData*>(mMuDst->event()->triggerData());
  if(!trigger) {
    LOG_WARN << "No trigger data bank available!" << endm;
    return;
  }

  // VPD TacSum
  mtdTrigger->setVpdTacSum(trigger->vpdEarliestTDCHighThr(east) + trigger->vpdEarliestTDCHighThr(west) );

  // run year
  const int runnumber = mMuDst->event()->runNumber();
  int year = runnumber / 1e6 + 1999;
  // Oct. 1st (approx. 273rd day) is the start of a new running year
  if ((runnumber % 1000000) / 1000 >= 273) year += 1;

  // Get QT information
  const int kNQTboard = 8;
  UShort_t mtdQTadc[kNQTboard][16], mtdQTtac[kNQTboard][16];
  memset(mtdQTadc, 0, sizeof(mtdQTadc));
  memset(mtdQTtac, 0, sizeof(mtdQTtac));

  for (Int_t i = 0; i < 32; i++){
    Int_t type = (i / 4) % 2;
    if(year<=2015){
      if (type == 1){
	mtdQTtac[0][i - i / 4 * 2 - 2] = trigger->mtdAtAddress(i, 0);
	mtdQTtac[1][i - i / 4 * 2 - 2] = trigger->mtdgemAtAddress(i, 0);
	mtdQTtac[2][i - i / 4 * 2 - 2] = trigger->mtd3AtAddress(i, 0);
	mtdQTtac[3][i - i / 4 * 2 - 2] = trigger->mtd4AtAddress(i, 0);
      }
      else{
	mtdQTadc[0][i - i / 4 * 2] = trigger->mtdAtAddress(i, 0);
	mtdQTadc[1][i - i / 4 * 2] = trigger->mtdgemAtAddress(i, 0);
	mtdQTadc[2][i - i / 4 * 2] = trigger->mtd3AtAddress(i, 0);
	mtdQTadc[3][i - i / 4 * 2] = trigger->mtd4AtAddress(i, 0);
      }//else
    }//if(year<=2015)
    else{
      for (int im = 0; im < kNQTboard; im++){
	if (year != 2016 && im>=4) continue;
	if (type == 0) mtdQTadc[im][i - i / 4 * 2] = trigger->mtdQtAtCh(im + 1, i, 0);
	else           mtdQTtac[im][i - i / 4 * 2 - 2] = trigger->mtdQtAtCh(im + 1, i, 0);
      }//for (int im = 0; im < kNQTboard; im++)
    }//else
  }//for (Int_t i = 0; i < 32; i++)

  mtdTrigger->setQTtacSum(runnumber, mtdQTadc, mtdQTtac, mQTtoModule,
			  mQTSlewBinEdge, mQTSlewCorr);

  // MT101 information
  UShort_t mt101Tac[kNQTboard][2],  mt101Id[kNQTboard][2];
  for (Int_t i = 0; i < kNQTboard; i++) {
    int idx = 0;
    if(year==2016) {
      idx = i / 2 * 3 + i % 2 * 16;
    }
    else {
      idx = i * 3;
    }
    mt101Tac[i][0] = (trigger->mtdDsmAtCh(idx, 0)) + ((trigger->mtdDsmAtCh(idx + 1, 0) & 0x3) << 8);
    mt101Id[i][0]  = (trigger->mtdDsmAtCh(idx + 1, 0) & 0xc) >> 2;
    mt101Tac[i][1] = (trigger->mtdDsmAtCh(idx + 1, 0) >> 4) + ((trigger->mtdDsmAtCh(idx + 2, 0) & 0x3f) << 4);
    mt101Id[i][1]  = (trigger->mtdDsmAtCh(idx + 2, 0) & 0xc0) >> 6;
  } //for (Int_t i = 0; i < kNQTboard; i++)
  mtdTrigger->setMT101(mt101Tac, mt101Id);

  // TF201 information
  UInt_t dsmBit1 = trigger->dsmTF201Ch(0);
  UInt_t dsmBit2 = 0;
  if (year == 2016) {
    dsmBit2 = trigger->dsmTF201Ch(6);
  }
  mtdTrigger->setTF201TriggerBit(year, dsmBit1, dsmBit2);
}

//_________________
void StPicoDstMaker::fillBTowHits() {

  //Loop over 120 modules and each of them consist from 40 towers
  for (Int_t iTower = 0; iTower < 4800; iTower++) {

    StEmcRawHit* aHit = mEmcIndex[iTower];

    // Do not skip torwers for which information does not exist.
    // Set up default values for all towers and if some do not exist
    // then will them with aka default (see StPicoBTowHit::isBad() ) info

    Int_t adc = 0;
    Float_t energy = -2.;
    if (aHit) {
      //int softId = aHit->softId(1);
      adc = aHit->adc();
      energy = aHit->energy();
    }

    Int_t counter = mPicoArrays[StPicoArrays::BTowHit]->GetEntries();
    new((*(mPicoArrays[StPicoArrays::BTowHit]))[counter]) StPicoBTowHit(adc, energy);
  } //for (int i = 0; i < 4800; ++i)
}

//_________________
void StPicoDstMaker::fillBTofHits() {

  // Loop over BTOF hits
  for (unsigned int i = 0; i < mMuDst->numberOfBTofHit(); ++i) {

    // Retrieve i-th hit
    StMuBTofHit* aHit = (StMuBTofHit*)mMuDst->btofHit(i);
    if (!aHit) continue;

    if (aHit->tray() > 120) continue;
    int cellId = (aHit->tray() - 1) * 192 + (aHit->module() - 1) * 6 + (aHit->cell() - 1);

    int counter = mPicoArrays[StPicoArrays::BTofHit]->GetEntries();
    new((*(mPicoArrays[StPicoArrays::BTofHit]))[counter]) StPicoBTofHit(cellId);
  }
}

//_________________
void StPicoDstMaker::fillETofHits() {

  // Retrieve and fill both ETOF hit and pidTraits


  std::map< Int_t, Int_t > muTrackId2picoHitIndex;

  // Loop over ETOF hits
  for ( size_t i = 0; i < mMuDst->numberOfETofHit(); ++i) {

    // Retrieve i-th hit
    StMuETofHit* aHit = (StMuETofHit*)mMuDst->etofHit(i);
    if (!aHit) continue;

    unsigned int geomId = (aHit->sector() - 13) * 9 + (aHit->zPlane() - 1) * 3 + aHit->counter();

    // Create an instance of ETOF hit
    StPicoETofHit picoHit = StPicoETofHit();
    // Fill the current hit information
    picoHit.setGeomId(geomId);
    picoHit.setLocalX(aHit->localX());
    picoHit.setLocalY(aHit->localY());
    picoHit.setClusterSize(aHit->clusterSize());
    picoHit.setTime(aHit->time());
    picoHit.setTot(aHit->totalTot());
    /*
      LOG_INFO << "muHit sector  = "     << aHit->sector()
      << " zPlane  = "     << aHit->zPlane()
      << " counter = "     << aHit->counter()
      << " localX = "      << aHit->localX()
      << " localY = "      << aHit->localY()
      << " clusterSize = " << aHit->clusterSize()
      << " time = "        << aHit->time()
      << " tot  = "        << aHit->totalTot()
      << endm;
	   picoHit.Print();
    */
    int counter = mPicoArrays[StPicoArrays::ETofHit]->GetEntries();
    new((*(mPicoArrays[StPicoArrays::ETofHit]))[counter]) StPicoETofHit(picoHit);

    if( aHit->associatedTrackId() > -1 ) muTrackId2picoHitIndex[ aHit->associatedTrackId() ] = counter;
  }

  //LOG_INFO << "size of muTrackId2picoHitIndex map: " << muTrackId2picoHitIndex.size() << endm;

  // Set the eTOF hit index of the eTOF PID traits to their proper values
  for( size_t i = 0; i < (size_t)mPicoArrays[StPicoArrays::ETofPidTraits]->GetEntriesFast(); ++i ) {
    StPicoETofPidTraits* etofPidTraits = (StPicoETofPidTraits*)mPicoArrays[StPicoArrays::ETofPidTraits]->At( i );

    int trackId = etofPidTraits->hitIndex();

    if( muTrackId2picoHitIndex.count( trackId ) ) {
      etofPidTraits->setHitIndex( muTrackId2picoHitIndex.at( trackId ) );
    }
    else {
      etofPidTraits->setHitIndex( -1 );
    }
    /*
    LOG_INFO << "muPidTrait (" << i << "): trackId = " << trackId
    << "  hit index: " << etofPidTraits->hitIndex() << endm;
    if(etofPidTraits->hitIndex() > -1 ) {
    StPicoETofHit* picoHit = (StPicoETofHit*)mPicoArrays[StPicoArrays::ETofHit]->At( etofPidTraits->hitIndex() );

    if( picoHit ) {
      picoHit->Print();
    }
    }
    */
  }
}

//_________________
void StPicoDstMaker::fillBbcHits() {
  // Retrieve pointer to StMuEvent
  StMuEvent* muEvent = mMuDst->event();
  // Retrieve trigger data
  const StTriggerData* trg = muEvent->triggerData();
  // Trigger data must exist
  if( trg ) {
    // Loop over two directions (east, west)
    for (int ew=-1; ew<2; ew+=2) { // note loop -1,+1
      StBeamDirection dir = (ew<0) ? east : west;
      // Loop over PMT boxes
      for (int pmt=1; pmt<17; pmt++) {
	int adc = trg->bbcADC(dir,pmt);
	if (adc>0) {
	  int tdc = trg->bbcTDC5bit(dir,pmt);
	  int tac = trg->bbcTDC(dir,pmt);
	  bool trueval = kTRUE;
	  int counter = mPicoArrays[StPicoArrays::BbcHit]->GetEntries();
	  new((*(mPicoArrays[StPicoArrays::BbcHit]))[counter]) StPicoBbcHit(pmt,ew,adc,tac,tdc,trueval,trueval);
	} //if (adc>0)
      } //for (int pmt=1; pmt<17; pmt++)
    } //for (int ew=-1; ew<2; ew+=2)
  }
}
//_________________
void StPicoDstMaker::fillEpdHits() {
  // Loop over EPD hits
  for (unsigned int i=0; i < mMuDst->numberOfEpdHit(); i++) {
    StMuEpdHit* aHit = mMuDst->epdHit(i);
    if (!aHit) continue;
    int counter = mPicoArrays[StPicoArrays::EpdHit]->GetEntries();
    new((*(mPicoArrays[StPicoArrays::EpdHit]))[counter]) StPicoEpdHit(aHit->id(), aHit->qtData(), aHit->nMIP());
  } //for (unsigned int i=0; i < mMuDst->numberOfEpdHit(); i++)
}

//_________________
void StPicoDstMaker::fillMtdHits() {

  // Retrieve number of MTD hits from muDst
  Int_t nMtdHits = mMuDst->numberOfMTDHit();

  // Loop over MTD hits
  for (Int_t iHit = 0; iHit < nMtdHits; ++iHit) {
    // Retrieve i-th hit
    StMuMtdHit* hit = (StMuMtdHit*)mMuDst->mtdHit(iHit);
    if (!hit) continue;

    // Retrieve the index int the MtdHit pico array
    Int_t counter = mPicoArrays[StPicoArrays::MtdHit]->GetEntries();
    // Create new MTD hit
    new((*(mPicoArrays[StPicoArrays::MtdHit]))[counter]) StPicoMtdHit();

    // Return pointer to a new MTD hit
    StPicoMtdHit *mtdHit = (StPicoMtdHit*)mPicoArrays[StPicoArrays::MtdHit]->At(counter);

    // Fill MTD hit as: (backleg-1) * 60 + (module-1) * 12 + cell
    mtdHit->setHitChannel( hit->backleg(),
			   hit->module(),
			   hit->cell() );

    // Fill leading edge time
    mtdHit->setLeadingEdgeTime( hit->leadingEdgeTime() );

    // Fill trailing edge time
    mtdHit->setTrailingEdgeTime( hit->trailingEdgeTime() );

  } //for (Int_t iHit = 0; iHit < nMtdHits; ++iHit)

  // Retrieve number of MTD hits from picoDst
  unsigned int nHits = mPicoDst->numberOfMtdHits();
  // Retrieve number of MTD hits from picoDst associated with PidTraits
  unsigned int nMtdPidTraits = mPicoDst->numberOfMtdPidTraits();

  // Loop over MtdPidTrait and MtdHit picoArrays in order
  // to find MtdHit that corresponds to MtdPidTrait
  for (unsigned int iPidHit = 0; iPidHit < nMtdPidTraits; ++iPidHit) {

    // Return pointer ot i-th MtdPidTrait
    StPicoMtdPidTraits* pidTrait = mPicoDst->mtdPidTraits(iPidHit);

    // Loop over MtdHit
    for (unsigned int iHit = 0; iHit < nHits; ++iHit) {

      // Return pointer to i-th MtdHit
      StPicoMtdHit* hit = mPicoDst->mtdHit(iHit);

      // Correspondence check
      if (pidTrait->gChannel() == hit->gChannel()) {
        pidTrait->setMtdHitIndex(iHit);
        break;
      }
    } //for (unsigned int iHit = 0; iHit < nHits; ++iHit)
  } //for (unsigned int iPidHit = 0; iPidHit < nMtdPidTraits; ++iPidHit)

  // Check the firing hits
  if (mPicoDst->numberOfMtdTriggers() != 1) {
    LOG_ERROR << "There are " << mPicoDst->numberOfMtdTriggers() << " MTD trigger. Check it!" << endm;
    return;
  }

  // MTD trigger part
  StPicoMtdTrigger* trigger = mPicoDst->mtdTrigger(0);
  Int_t triggerQT[8][2];
  Bool_t triggerBit[8][8];
  Int_t pos1 = 0, pos2 = 0;

  for (Int_t i = 0; i < 8; ++i)  {
    for (Int_t j = 0; j < 2; ++j) {
      triggerQT[i][j] = 0;
    } //for (Int_t j = 0; j < 2; ++j)
    for (Int_t j = 0; j < 8; ++j) {
      triggerBit[i][j] = kFALSE;
    } //for (Int_t j = 0; j < 8; ++j)

    trigger->getMaximumQTtac(i + 1, pos1, pos2);
    triggerQT[i][0] = pos1;
    triggerQT[i][1] = pos2;
    for (Int_t j = 0; j < 2; ++j) {
      if (triggerQT[i][j] > 0 && ((trigger->getTF201TriggerBit() >> (i * 2 + j)) & 0x1)) {
        triggerBit[i][triggerQT[i][j] - 1] = kTRUE;
      }
    } //for (Int_t j = 0; j < 2; ++j)
  } //for (Int_t i = 0; i < 8; ++i)

  vector<Int_t> triggerPos;
  vector<Int_t> hitIndex;

  // Loop over MTD hits from pico array
  for (unsigned int i = 0; i < nHits; ++i) {
    StPicoMtdHit* hit = mPicoDst->mtdHit(i);
    Int_t backleg = hit->backleg();
    Int_t module  = hit->module();
    Int_t qt = mModuleToQT[backleg - 1][module - 1];
    Int_t pos = mModuleToQTPos[backleg - 1][module - 1];
    if (qt >= 1 && qt <= 8 && pos > 0 && triggerBit[qt - 1][pos - 1]) {
      triggerPos.push_back(qt * 10 + pos);
      hitIndex.push_back(i);
    }
    else {
      hit->setTriggerFlag(0);
    }
  } //for (unsigned int i = 0; i < nHits; ++i)

  vector<Int_t> hits;

  while (triggerPos.size() > 0) {
    hits.clear();
    hits.push_back(0);
    for (Int_t j = 1; j < (Int_t)triggerPos.size(); ++j) {
      if (triggerPos[j] == triggerPos[0]) {
        hits.push_back(j);
      }
    } //for (Int_t j = 1; j < (Int_t)triggerPos.size(); ++j)

    for (Int_t k = (Int_t)hits.size() - 1; k > -1; k--) {
      StPicoMtdHit* hit = mPicoDst->mtdHit( hitIndex[hits[k]] );
      hit->setTriggerFlag( (Int_t)hits.size() );
      triggerPos.erase(triggerPos.begin() + hits[k]);
      hitIndex.erase(hitIndex.begin() + hits[k]);
    } //for (Int_t k = (Int_t)hits.size() - 1; k > -1; k--)
  } //while (triggerPos.size() > 0)
}

/**
 * Selects a primary vertex from `muDst` vertex collection according to the
 * vertex selection mode `mVtxMode` specified by the user. The mode must be
 * set with StMaker::SetAttr("PicoVtxMode", "your_desired_vtx_mode") as by
 * default the selection mode is `PicoVtxMode::NotSet`.
 *
 * Returns `true` if the user has specified a valid vertex selection mode and
 * a valid vertex satisfying the corresponding predefined conditions is found in
 * the muDst vertex collection.
 *
 * Returns `false` otherwise.
 */
//_________________
bool StPicoDstMaker::selectVertex() {

  // Create a NULL pointer to the MuPrimaryVertex
  StMuPrimaryVertex* selectedVertex = nullptr;

  // Switch between modes: Default and Vpd (VpdOrDefault)
  // Default takes the first primary vertex, meanwhile
  // Vpd
  if (mVtxMode == PicoVtxMode::Default) {

    // Choose the default vertex, i.e. the first vertex
    mMuDst->setVertexIndex(0);
    selectedVertex = mMuDst->primaryVertex();
  }
  else if (mVtxMode == PicoVtxMode::Vpd || mVtxMode == PicoVtxMode::VpdOrDefault) {

    // For VpdOrDefault option one will take the first primary
    // vertex if no TOF or VPD information is available
    if(mVtxMode == PicoVtxMode::VpdOrDefault) {
      mMuDst->setVertexIndex(0);
      selectedVertex = mMuDst->primaryVertex();
    }

    // Retrieve pointer to TOF and VPD information
    StBTofHeader const* mBTofHeader = mMuDst->btofHeader();

    if (mBTofHeader && fabs(mBTofHeader->vpdVz()) < 200) {

      // Retrieve z-position of pVtx estimated from VPD information
      float vzVpd = mBTofHeader->vpdVz();

      // Loop over primary vertices
      for (unsigned int iVtx = 0; iVtx < mMuDst->numberOfPrimaryVertices(); ++iVtx) {

	// Return pointer to i-th primary vertex
        StMuPrimaryVertex* vtx = mMuDst->primaryVertex(iVtx);
        if (!vtx) continue;

	// Check TpcVz and VpdVz difference
        if (fabs(vzVpd - vtx->position().z()) < mTpcVpdVzDiffCut) {
          mMuDst->setVertexIndex(iVtx);
          selectedVertex = mMuDst->primaryVertex();
          break;
        } //if (fabs(vzVpd - vtx->position().z()) < mTpcVpdVzDiffCut)
      } //for (unsigned int iVtx = 0; iVtx < mMuDst->numberOfPrimaryVertices(); ++iVtx)
    } //if (mBTofHeader && fabs(mBTofHeader->vpdVz()) < 200)
  } //else if (mVtxMode == PicoVtxMode::Vpd || mVtxMode == PicoVtxMode::VpdOrDefault)
  else { // default case
    LOG_ERROR << "Pico Vtx Mode not set!" << endm;
  }

  // Retrun false if selected vertex is not valid
  return selectedVertex ? true : false;
}
