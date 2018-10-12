/*!
 * \class  StPrepEmbedMaker
 * \brief  Prepare GEANT Maker with input from embedding settings and DAQ event
 * \author A.Rose, Y.Fisyak
 * \date   May 2007
 *
 * This maker prepares the GEANT Maker (St_geant_Maker) for running
 * in the embedding chain. There are several values which need to be
 * set so that the data and simulation have similar environments,
 * including magnetic field, detector geometry and primary vertex. It
 * does so by re-setting the default values for the GEANT maker,
 * St_geant_Maker, event by event.
 *
 * The Make method for this maker must be called in each event before
 * the Make method of the St_geant_Maker, or the simulated and real
 * event will not be appropriately matched.
 *
 * $Id: StPrepEmbedMaker.cxx,v 1.18 2018/10/12 13:44:49 zhux Exp $
 *
 */

#include "TFile.h"
#include "StIOMaker/StIOMaker.h"
#include "StMessMgr.h"
#include "StPrepEmbedMaker.h"
#include "StEvtHddr.h"
#include "TTree.h"
#include "StGenericVertexMaker/StGenericVertexMaker.h"
#include "StGenericVertexMaker/StFixedVertexFinder.h"

#include "tables/St_vertexSeed_Table.h"
#include "TString.h"
#include "TSystem.h"

#include <unistd.h>

ClassImp(StPrepEmbedMaker)
struct embedSettings{
  Double_t mult;
  Int_t pid;
  Double_t ptlow;
  Double_t pthigh;
  Double_t etalow;
  Double_t etahigh;
  Double_t philow;
  Double_t phihigh;
  Double_t temperature;
  Int_t rnd1;
  Int_t rnd2;
  Double_t vzlow;
  Double_t vzhigh;
  Double_t vr;
  Double_t vpdvz;
  Double_t pvrank;
  Int_t NReqTrg;
  static const Int_t nTriggerId = 32 ;
  Int_t ReqTrgId[nTriggerId];
  TString mode;
};

static embedSettings  *mSettings = 0;
const Double_t StPrepEmbedMaker::mRapidityMaximumCut = 10.0 ; // Maximum rapidity range for 'spectrum' option

//____________________________________________________________________________________________________
StPrepEmbedMaker::StPrepEmbedMaker(const Char_t *name) : StMaker(name) 
{
  mGeant3=0;
  mTagFile = "" ;
  mMoreTagsFile = "" ;
  mFzFile = "temp.fz";
  mEventCounter = 0;

  if( !mSettings ){
    mSettings = new embedSettings();

    /// Default z-vertex selection (only apply if mSkipMode = kTRUE)
    mSettings->vzlow  = -200.0 ;
    mSettings->vzhigh =  200.0 ;

    /// Default multiplicity is 0
    mSettings->mult=0.;

    /// Default temperature is 300 MeV
    mSettings->temperature=0.3; // GeV

    /// Default is no trigger selection
    mSettings->NReqTrg = 0 ;
    for(Int_t itrg=0; itrg<mSettings->nTriggerId; itrg++){
      mSettings->ReqTrgId[itrg] = 0 ;
    }

    /// Default mode is flatpt
    mSettings->mode = "flatpt";
  }
  mFile = 0;
  mMoreFile = 0 ;
  mTree = 0;
  mMoreTree = 0;
  mSkipMode = kFALSE; /// Do not skip the false vertex
  mSpreadMode = kFALSE; /// Do not smear z-vertex
  mOpenFzFile = kFALSE; /// Do not write .fz file
  mPrimeMode = kFALSE; /// Do not prime the first event
  mSavePid = 0;
  mPrimed = kFALSE;
  mVpdVzCutMode = kFALSE; /// Do not cut on VpdVz
  mPVRankCutMode = kFALSE; /// Do not cut on PVRank
  mRapidityMode = kTRUE;  /// flat in rapidity 
}
//____________________________________________________________________________________________________
StPrepEmbedMaker::~StPrepEmbedMaker() { 
  SafeDelete(mFile);
}

//____________________________________________________________________________________________________
Int_t StPrepEmbedMaker::Init() 
{
  srand((unsigned)time(0));
  mSettings->rnd1 = abs(int(rand()*10000)+getpid());
  mSettings->rnd2 = abs(int(rand()*10000)+getpid());

  if (mSettings->mode.CompareTo("strange", TString::kIgnoreCase) == 0)
  {
    mSpreadMode= kTRUE;
    LOG_INFO <<"StPrepEmbedMaker::Init  Setting spreader mode for embedding mode "<<mSettings->mode <<endm;
  }

  return StMaker::Init();
}

//____________________________________________________________________________________________________
Int_t StPrepEmbedMaker::InitRun(const int runnum)
{
  //Call geant maker, set defaults
  if (! mGeant3) {
    mGeant3 = TGiant3::Geant3();
    if( ! mGeant3)
      {
	LOG_ERROR << "StPrepEmbedMaker::InitRun  Geant3 pointer not found. exiting."<<endm;
	return kStErr;
      }
  }

  // Skip initialization if tags file has not been defined
  if (mTagFile.IsWhitespace()){
    LOG_ERROR << "StPrepEmbedMaker::InitRun  TagFile has not been defined" << endm;
    return kStErr;
  }

  // Open Tags file
  mFile = TFile::Open(mTagFile);
  if (! mFile ) {
    LOG_ERROR << "StPrepEmbedMaker::Init  TagFile : " << mTagFile << " cannot be opened" << endm;
    return kStErr;
  }

  // Get Tag tree
  mTree = (TTree *) mFile->Get("Tag");
  if (! mTree ) {
    LOG_ERROR << "StPrepEmbedMaker::Init  In TagFile : " << mTagFile << " cannot find TTree \"Tag\"" << endm;
    return kStErr;
  }

  // Check mode type, select settings
  if (mSpreadMode){
    LOG_INFO << "StPrepEmbedMaker::Init  Spreader mode set. Looking for MoreTags file ..."<<endm;
    mMoreTagsFile = mTagFile;
    const int indx1 = mMoreTagsFile.Index(".tags",0);
    const int indx2 = mMoreTagsFile.Last('.');
    if (indx1!=indx2) mMoreTagsFile.Remove(indx1+1,(indx2-indx1));
    mMoreTagsFile.Insert(indx1+1,"moretags.");
    mMoreFile = TFile::Open(mMoreTagsFile);

    if (mMoreFile ) { 
      mMoreTree = (TTree *) mMoreFile->Get("MoreTags");
      if (! mMoreTree ) {
        LOG_ERROR << "StPrepEmbedMaker::Init  In MoreTagsFile : " << mMoreTagsFile << " cannot find TTree \"MoreTags\"" << endm;
        return kStErr;
      }//end if more tree 
    }
    else {
      LOG_INFO << "StPrepEmbedMaker::Init  File moretags.root not found. If this embedding is for years 2007 through 2009, this will most likely cause the embedding to quit prematurely."<<endm;
      return kStErr ;
    }//end if mMoreFile
  }//end spreadMode setup


  // Obsolete methods
#if 0
  if(mSettings->mode.CompareTo("Spectrum", TString::kIgnoreCase) == 0)
  { 
    // Call the old gentx binary for this request. 
    // We need to port this code into the StPrepEmbedMaker,
    // but the underlying functions don't exist in St_geant_Maker 
    // at this time (11-25-09).
    // magic numbers: 1000 events, temp.fz need to be resolved
    TString cmd;
    cmd = Form("root4star -q \'~starofl/embedding/getVerticiesFromTags.C\(1000,\"./\",\"%s\")\'", 
     	  mTagFile.Data());
    cmd = Form("~starofl/embedding/GENTX/gentx %s %s %i %i %f %f %f %i %f %f %f %i",
     	  mTagFile.Data(),"temp.fz",
     	  1000,mSettings->mult,mSettings->etalow, mSettings->etahigh, 
     	  0.0,0,mSettings->ptlow, mSettings->pthigh,mSettings->temperature,
     	  mSettings->rnd1);
    gSystem->Exec(cmd.Data());

    Do("gfile p temp.fz"); 
  }//end if Spectrum
#endif

  if( mOpenFzFile ) {
    // Open .fz file
    // Name fz file with same basename as daq file
    // If there are no daq files, use default name "temp.fz"
    if (StIOMaker* ioMaker = (StIOMaker*)GetMakerInheritsFrom("StIOMaker")) {
      if (const Char_t* daqfile = ioMaker->GetFile()) {
        LOG_DEBUG << "StPrepEmbedMaker::InitRun  daq file: " << daqfile << endm;
        mFzFile = gSystem->BaseName(daqfile);
        mFzFile.ReplaceAll(".daq",".fz");
      }
    }
    LOG_INFO << "StPrepEmbedMaker::InitRun  Open FZ file: " << mFzFile << endm;
    Do("user/output o " + mFzFile);
  }
#if 0 /* Please don't do this. starsim make in root4star has distroied table on virtual functions. */
  // Common geant settings
  Do("make gstar"); // Make user-defined particles available
#else
  gSystem->Load("libgstar");
  Do("call gstar");
#endif   
  Do("detp  hadr_on");
  TString cmd("rndm ");
  cmd+=mSettings->rnd1; cmd+=" "; cmd+=mSettings->rnd2;
  Do(cmd.Data());

  return 0;
}

//____________________________________________________________________________________________________
Int_t StPrepEmbedMaker::Make() 
{
  mEventCounter++;  // increase counter
  StEvtHddr* EvtHddr = (StEvtHddr*) GetDataSet("EvtHddr");
  if (! EvtHddr) {
    LOG_ERROR << "StPrepEmbedMaker::Make EvtHddr has not been found" << endm;
    return kStErr;
  }
  Int_t nFound = mTree->Draw("uncorrectedNumberOfPrimaries:primaryVertexFlag",
			     Form("mRunNumber==%i&&mEventNumber==%i",EvtHddr->GetRunNumber(),EvtHddr->GetEventNumber()),
			     "goff");
  if (nFound != 1) {
    LOG_ERROR << "StPrepEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber() << "/" << EvtHddr->GetEventNumber() 
	      << " has been found in tag file" << nFound << " times" <<  endm;
    return kStErr;
  }
  LOG_INFO << "StPrepEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber()
	   << "/" << EvtHddr->GetEventNumber() 
	   << " has been found with uncorrectedNumberOfPrimaries = " <<  mTree->GetV1()[0] 
	   << " and primaryVertexFlag = " << mTree->GetV2()[0]  <<  endm; 

  if (mTree->GetV1()[0] <= 0 || mTree->GetV2()[0] )
  {
    LOG_ERROR << "StPrepEmbedMaker::Make reject this event" << endm;
    return kStErr;
  }

  if( mMoreTree )
  {
     nFound = mMoreTree->Draw("nRefMult",
	     Form("RunId==%i&&EvtId==%i",EvtHddr->GetRunNumber(),EvtHddr->GetEventNumber()),
	     "goff");
     if (nFound != 1) {
	  LOG_ERROR << "StPrepEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber() << "/" << EvtHddr->GetEventNumber()
	     << " has been found in moretag file" << nFound << " times" <<  endm;
	  return kStErr;
     }
     LOG_INFO << "StPrepEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber()
	  << "/" << EvtHddr->GetEventNumber()
	  << " has been found (in moretags) with uncorrectedNumberOfPrimaries = " <<  mMoreTree->GetV1()[0] << endm;

     if (mMoreTree->GetV1()[0] <= 0 )
     {
	  LOG_ERROR << "StPrepEmbedMaker::Make reject this event" << endm;
	  return kStErr;
     }
     if ( fabs(mMoreTree->GetV1()[0]- mTree->GetV1()[0]) > 0.1 )
     {
	  LOG_INFO << "StPrepEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber()
	     << "/" << EvtHddr->GetEventNumber()
	     << " has different uncorrectedNumberOfPrimaries from tags and moretags" << endm;
     }
  }

  // Extract info for mult for this event
  const Int_t numberOfPrimaryTracks = mMoreTree ? mMoreTree->GetV1()[0] : mTree->GetV1()[0];
  const Int_t npart = getMultiplicity( *EvtHddr, numberOfPrimaryTracks ) ;

  nFound = (Int_t) mTree->Draw("primaryVertexX:primaryVertexY:primaryVertexZ:TriggerId",
			       Form("mRunNumber==%i&&mEventNumber==%i",
				    EvtHddr->GetRunNumber(),
				    EvtHddr->GetEventNumber()),
			       "goff");

  if ( mMoreTree )
  {
     nFound = (Int_t) mMoreTree->Draw("VX:VY:VZ",
	     Form("RunId==%i&&EvtId==%i",
		  EvtHddr->GetRunNumber(),
		  EvtHddr->GetEventNumber()),
	     "goff");
  }

  Double_t xyztmp[3];
  if ( mMoreTree )
  {
     xyztmp[0] = mMoreTree->GetV1()[0];
     xyztmp[1] = mMoreTree->GetV2()[0];
     xyztmp[2] = mMoreTree->GetV3()[0];
  }
  else
  {
     xyztmp[0] = mTree->GetV1()[0];
     xyztmp[1] = mTree->GetV2()[0];
     xyztmp[2] = mTree->GetV3()[0];
  }

  const Double_t xyz[3] = {xyztmp[0],xyztmp[1],xyztmp[2]};

  // Skip event if no primary vertex - effectively if tags say it is 0,0,0
  if (fabs(xyz[0])<1e-7 && fabs(xyz[1])<1e-7 && fabs(xyz[2])<1e-7 ){
    LOG_INFO << "StPrepEmbedMaker::Make  Event " << EvtHddr->GetEventNumber()
             << " has tags with vertex approx at (0,0,0) - probably no PV, skipping."
             << endm;
    return kStSKIP;
  }

  // Skip event 
  // 1. if vertexZ is not in the required range
  // 2. if vr = sqrt{vx^2 + vy^2} is not in the required range
  if (mSkipMode == kTRUE){
    const Double_t vr = sqrt(xyz[0]*xyz[0] + xyz[1]*xyz[1]);
    if (xyz[2]<mSettings->vzlow || xyz[2]>mSettings->vzhigh || vr>=mSettings->vr ){
      LOG_INFO << "StPrepEmbedMaker::Make  Event " << EvtHddr->GetEventNumber()
        << " has tags with vertex at (" << xyz[0] << "," << xyz[1] << "," << xyz[2]
        << "), vr = " << vr
        << " - out of Vz or Vr range, skipping." << endm;
      return kStSKIP;
    }

    LOG_INFO << "StPrepEmbedMaker::Make  Event " << EvtHddr->GetEventNumber()
      << " has tags with vertex at (" << xyz[0] << "," << xyz[1] << "," << xyz[2]
      << "), vr = " << vr
      << " - within requested Vz and Vr range !" << endm;
  }          
  
  // more skipping. cut on trigger id.
  if (mSkipMode == kTRUE){
    LOG_INFO << "StPrepEmbedMaker::Event " << EvtHddr->GetEventNumber()
      << " has Triggers: " << endm;
    for (Int_t iTrg=0 ; iTrg<mSettings->nTriggerId ; iTrg++){
      LOG_INFO << mTree->GetV4()[iTrg] << " ";
    }
    LOG_INFO << endm;

    Bool_t fired = kFALSE;
    for (Int_t iTrg=0 ; iTrg<mSettings->nTriggerId ; iTrg++){
      for (Int_t iReqTrg=0; iReqTrg<mSettings->NReqTrg ; iReqTrg++) {
        if (mTree->GetV4()[iTrg] == mSettings->ReqTrgId[iReqTrg]){
          LOG_INFO << "StPrepEmbedMaker::Requested trigger " << mSettings->ReqTrgId[iReqTrg] << " is fired!" << endm;
          fired = kTRUE;
        }
      }
    }

    if (!fired && mSettings->NReqTrg>0) {
      LOG_INFO << "StPrepEmbedMaker::No requested triggers are fired in this event, skipping." << endm;
      return kStSKIP;
    }
  }

  // more skipping. cut on VpdVz in btofheader
  Float_t vpdvz;
  if(mSkipMode == kTRUE && mVpdVzCutMode == kTRUE)
  {
      // (Run 10, 11 and 12) need an external file (moretags.root).
      nFound=0;
      nFound = (Int_t) mTree->Draw("VpdVz",
				   Form("mRunNumber==%i&&mEventNumber==%i",
					EvtHddr->GetRunNumber(),
					EvtHddr->GetEventNumber()),"goff");

      //get primary vertex errors from moretags.root
      if(nFound == -1 && mMoreTree) {
        nFound = (Int_t) mMoreTree->Draw("VpdVz",
             Form("RunId==%i&&EvtId==%i",
                  EvtHddr->GetRunNumber(),
                  EvtHddr->GetEventNumber()),
             "goff");

        LOG_INFO << "StPrepEmbedMaker::Make Use moretags file to extract VpdVz, nFound =" << nFound << endm ;
      }

      if (nFound != 1) {
        LOG_ERROR << "StPrepEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber() << "/" << EvtHddr->GetEventNumber()
             << " has been found in moretags file " << nFound << " times" <<  endm;
        return kStErr;
     }
     vpdvz = mMoreTree->GetV1()[0];
     LOG_INFO << vpdvz << endm;

     //cut on events
     if( fabs(vpdvz) < 1e-7 ) {
	  LOG_INFO << "StPrepEmbedMaker::Make  Event " << EvtHddr->GetEventNumber()
	     << " has tags with vertex at (" << xyz[0] << "," << xyz[1] << "," << xyz[2]
	     << "), VpdVz = " << vpdvz
	     << " - VpdVz is too small (i.e. no BTOF in this run), skipping." << endm;
	  return kStSKIP;
     }
     if( fabs(vpdvz) >= 100. ) {
	  LOG_INFO << "StPrepEmbedMaker::Make  Event " << EvtHddr->GetEventNumber()
	     << " has tags with vertex at (" << xyz[0] << "," << xyz[1] << "," << xyz[2]
	     << "), VpdVz = " << vpdvz
	     << " - VpdVz is too large, skipping." << endm;
        return kStSKIP;
     }
     if( fabs(xyz[2]-vpdvz) > mSettings->vpdvz ) {
	  LOG_INFO << "StPrepEmbedMaker::Make  Event " << EvtHddr->GetEventNumber()
	     << " has tags with vertex at (" << xyz[0] << "," << xyz[1] << "," << xyz[2]
	     << "), VpdVz = " << vpdvz
	     << " - out of |Vz-VpdVz| range, skipping." << endm;
	  return kStSKIP;
     }
  }


  // more skipping. cut on PVRank
  Float_t pvrank;
  if(mSkipMode == kTRUE && mPVRankCutMode == kTRUE)
  {
      // (Run 12, and before) need an external file (moretags.root).
      nFound=0;
      nFound = (Int_t) mTree->Draw("PVRank",
				   Form("mRunNumber==%i&&mEventNumber==%i",
					EvtHddr->GetRunNumber(),
					EvtHddr->GetEventNumber()),"goff");

      //get primary vertex errors from moretags.root
      if(nFound == -1 && mMoreTree) {
        nFound = (Int_t) mMoreTree->Draw("PVRank",
             Form("RunId==%i&&EvtId==%i",
                  EvtHddr->GetRunNumber(),
                  EvtHddr->GetEventNumber()),
             "goff");

        LOG_INFO << "StPrepEmbedMaker::Make Use moretags file to extract PVRank, nFound =" << nFound << endm ;
      }

      if (nFound != 1) {
        LOG_ERROR << "StPrepEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber() << "/" << EvtHddr->GetEventNumber()
             << " has been found in moretags file " << nFound << " times" <<  endm;
        return kStErr;
     }
     pvrank = mMoreTree->GetV1()[0];
     LOG_INFO << pvrank << endm;

     //cut on events
     if( pvrank <= mSettings->pvrank ) {
	  LOG_INFO << "StPrepEmbedMaker::Make  Event " << EvtHddr->GetEventNumber()
	     << " has tags with vertex at (" << xyz[0] << "," << xyz[1] << "," << xyz[2]
	     << "), PVRank = " << pvrank
	     << " - PVRank is < = " << mSettings->pvrank << ", skipping." << endm;
	  return kStSKIP;
     }
  }
  //Done set up for event.

  //Setup embedded particle
  // gkine      npart ID        PTLOW,   PTHIGH,   YLOW,   YHIGH,   PHILOW,   PHIHIGH,   ZLOW,   ZHIGH
  //make sure zlow!=zhigh in particle definition - not sure of result. 
  //Z vertex will be forced in vxyz statement.

  Double_t xyzerr[3] = {0.,0.,0.};
//  Double_t vzlow = xyz[2];
//  Double_t vzhigh = xyz[2];
   
  if(mSettings->mode.CompareTo("strange", TString::kIgnoreCase) == 0)
  {
      // For this embedding type, we smear the start position of the particle
      // with the vertex errors. Old embedding (2007 through 2009) needs an
      // external file (moretags.root).
      nFound=0;
      nFound = (Int_t) mTree->Draw("sigmaPVX:sigmaPVY:sigmaPVZ",
				   Form("mRunNumber==%i&&mEventNumber==%i",
					EvtHddr->GetRunNumber(),
					EvtHddr->GetEventNumber()),"goff");

      //get primary vertex errors from moretags.root
      if( mMoreTree ) {
        nFound = (Int_t) mMoreTree->Draw("VXsigma:VYsigma:VZsigma",
             Form("RunId==%i&&EvtId==%i",
                  EvtHddr->GetRunNumber(),
                  EvtHddr->GetEventNumber()),
             "goff");

        LOG_INFO << "StPrepEmbedMaker::Make Use moretags file to extract vertex errors, nFound =" << nFound << endm ;
      }

      if ( mMoreTree ) {
	   xyzerr[0] = mMoreTree->GetV1()[0];
	   xyzerr[1] = mMoreTree->GetV2()[0];
	   xyzerr[2] = mMoreTree->GetV3()[0];
	}
	else {
	   xyzerr[0] = mTree->GetV1()[0];
	   xyzerr[1] = mTree->GetV2()[0];
	   xyzerr[2] = mTree->GetV3()[0];
	}

     LOG_INFO << xyzerr[0] << " " << xyzerr[1] << " " << xyzerr[2] << endm;
//     vzlow = -100.0;
//     vzhigh = 100.0;

     //Set the vertex for StEvent with StGenericVertexMaker
     StGenericVertexMaker * vmaker = (StGenericVertexMaker*) GetMaker("GenericVertex");
     StFixedVertexFinder * vfinder = (StFixedVertexFinder *) vmaker->GetGenericFinder();
     vfinder->SetVertexPosition(xyz[0],xyz[1],xyz[2]);
  }

  if( mPrimeMode && !mPrimed ) {
     mSavePid = mSettings->pid;
     mSettings->pid = 45;
  }

  // gkine is needed to set the z-vertex
  gkine(npart, xyz[2], xyz[2]);

  // Flat (pt, y)
  if( mRapidityMode ) phasespace(npart);
  
  Do(Form("gvertex %f %f %f",xyz[0],xyz[1],xyz[2]));
  if( mSettings->mode.CompareTo("strange", TString::kIgnoreCase) == 0 )
    {
	Do(Form("gspread %f %f %f", xyzerr[0],xyzerr[1],xyzerr[2]));
    }
  else
    { 
 	Do("vsig 0 0;");
    }

  // Sloped momentum distribution
  if( mSettings->mode.CompareTo("Spectrum", TString::kIgnoreCase) == 0 ){
    // Make sure temperature > 0
    if ( mSettings->temperature > 0.0 ){
      // np:    Number of particle type
      // code:  Particle geant id
      // mult:  Numbe of particles per event (negative multiplicity means exact input number of particles per event)
      // slope: Inverse slope parameter (c/GeV)
      // dy:    Rapidity width, from etahigh (negative sign means flat rapidity)
      LOG_INFO << "StPrepEmbedMaker::Make  Generate sloped momentum distribution with T=" 
        << mSettings->temperature << " GeV !"
        << endm;

      Do("subevent 0");
      Do(Form("detp mick miky.np=%d code=%d mult=%d slope=%f dy=%f", 
            1, mSettings->pid, -npart, 1.0/mSettings->temperature, -mSettings->etahigh));
 
      Do(Form("user/input please my.micky"));
//      Do(Form("gfile u my.micky"));
    }
    else{
      LOG_ERROR << "StPrepEmbedMaker::Make  Input temperature <= 0, T=" << mSettings->temperature
        << ",    skip to generate sloped momentum distribution"
        << endm ;
    }
  }

  Do("trig 1");

  if( mPrimeMode && !mPrimed ){
     mSettings->pid = mSavePid;
     mPrimed = kTRUE;
  }   

  return kStOK;
}

//____________________________________________________________________________________________________
Int_t StPrepEmbedMaker::Finish()
{
  if( mOpenFzFile ) {
    /// Write and close .fz file
    LOG_INFO << "StPrepEmbedMaker::Finish  Write and close fz file: " << mFzFile << endm;
    Do("user/output c " + mFzFile);
  }
  return 0;
}

//____________________________________________________________________________________________________
void StPrepEmbedMaker::Do(const Char_t *job)
{  
  Int_t l=strlen(job);
  if (l) {
    LOG_INFO << "StPrepEmbedMaker::Do(" << job << ");" << endm;
    mGeant3->Kuexel(job);
  }
}

//____________________________________________________________________________________________________
void StPrepEmbedMaker::SetPartOpt(const Int_t pid, const Double_t mult)  
{ 
  mSettings->mult=mult; mSettings->pid=pid; 
  LOG_INFO << "StPrepEmbedMaker::SetPartOpt mult = " << mSettings->mult
	   << " pid = " << mSettings->pid << endm;
}

//____________________________________________________________________________________________________
void StPrepEmbedMaker::SetOpt(const Double_t ptlow, const Double_t pthigh,
			      const Double_t etalow, const Double_t etahigh, const Double_t philow,
			      const Double_t phihigh, const TString type) 
{
  mSettings->ptlow=ptlow;   mSettings->pthigh=pthigh; 
  mSettings->etalow=etalow; mSettings->etahigh=etahigh;
  mSettings->philow=philow;  mSettings->phihigh=phihigh;
  mSettings->mode=type;
  LOG_INFO << "StPrepEmbedMaker::SetOpt ptlow = " << mSettings->ptlow << " pthigh = " << mSettings->pthigh
	   << " etalow = " << mSettings->etalow << " etahigh = " << mSettings->etahigh
	   << " philow = " << mSettings->philow << " phihigh = " << mSettings->phihigh
	   <<" Mode: "<< type.Data() << endm;
}

//____________________________________________________________________________________________________
void StPrepEmbedMaker::SetTemp(const double t)
{
  mSettings->temperature=t;
  LOG_INFO << "StPrepEmbedMaker::SetTemp  set temperature= " << mSettings->temperature << endm;
}

//____________________________________________________________________________________________________
void StPrepEmbedMaker::SetTagFile(const Char_t *file)
{
  mTagFile = file;
  LOG_INFO << "StPrepEmbedMaker::SetTagFile  set tags file= " << mTagFile << endm;
}

//____________________________________________________________________________________________________
void StPrepEmbedMaker::SetSkipMode(const Bool_t flag)
{
  LOG_INFO << "StPrepEmbedMaker::SetSkipMode  set skip mode= ";
  mSkipMode = flag;

  if( mSkipMode ){
    LOG_INFO << " ON" << endm ;
  }
  else{
    LOG_INFO << " OFF" << endm ;
  }
}

//____________________________________________________________________________________________________
void StPrepEmbedMaker::SetSpreadMode(const Bool_t flag)
{
  mSpreadMode=flag;

  LOG_INFO << "StPrepEmbedMaker::SetSpreadMode  set spread mode= ";

  if( mSpreadMode ){
    LOG_INFO << " ON" << endm ;
  }
  else{
    LOG_INFO << " OFF" << endm ;
  }
}

//____________________________________________________________________________________________________
void StPrepEmbedMaker::SetPrimeMode(const Bool_t flag)
{
  mPrimeMode=flag;

  LOG_INFO << "StPrepEmbedMaker::SetPrimeMode  set prime mode= ";

  if( mPrimeMode ){
    LOG_INFO << " ON" << endm ;
  }
  else{
    LOG_INFO << " OFF" << endm ;
  }
}

//____________________________________________________________________________________________________
void StPrepEmbedMaker::SetRapidityMode(const Bool_t flag)
{
  mRapidityMode=flag;

  LOG_INFO << "StPrepEmbedMaker::SetRapidityMode  set rapidity mode= ";

  if( mRapidityMode ){
    LOG_INFO << " Rapidity" << endm ;
  }
  else{
    LOG_INFO << " Pseudo-rapidity" << endm ;
  }
}

//____________________________________________________________________________________________________
void StPrepEmbedMaker::SetVpdVzCutMode(const Bool_t flag)
{

  mVpdVzCutMode=flag;

  LOG_INFO << "StPrepEmbedMaker::SetVpdVzCutMode  set VpdVz cut mode= ";

  if( mVpdVzCutMode ){
    LOG_INFO << " ON" << endm ;
  }
  else{
    LOG_INFO << " OFF" << endm ;
  }
 
  if(flag){
     //now moretags.root needed for VpdVz  
     SetSpreadMode(flag);
  }
}

//____________________________________________________________________________________________________
void StPrepEmbedMaker::SetPVRankCutMode(const Bool_t flag)
{

  mPVRankCutMode=flag;

  LOG_INFO << "StPrepEmbedMaker::SetPVRankCutMode  set PVRank cut mode= ";

  if( mPVRankCutMode ){
    LOG_INFO << " ON" << endm ;
  }
  else{
    LOG_INFO << " OFF" << endm ;
  }
 
  if(flag){
     //now moretags.root needed for PVRank  
     SetSpreadMode(flag);
  }
}

//____________________________________________________________________________________________________
Int_t StPrepEmbedMaker::getMultiplicity(const StEvtHddr& EvtHddr, const Int_t nprimarytracks) const
{
  /// Get multiplicity generated in the embedding

  Int_t npart = 0;
  if(mSettings->mult < 1.) 
    {
      npart=int(mSettings->mult * nprimarytracks);
      if (npart < 5)
      {
	LOG_INFO << "StPrepEmbedMaker::Event " << EvtHddr.GetEventNumber() 
	      << " has too small numberOfPrimaryTracks " << nprimarytracks << " for the mult fraction requested. Forcing npart to 5." << endm; 
	npart=5;
      }
  
    }
  else
    {
      npart = int (mSettings->mult);
    }

  return npart ;
}

//________________________________________________________________________________
void StPrepEmbedMaker::SetTrgOpt(const Int_t TrgId) 
{
  // Skip TrgId = 0
  if(TrgId == 0){
    LOG_ERROR << "StPrepEmbedMaker::SetTrgOpt Input trigger id = 0. Skip" << endm;
    return;
  }

  if(mSettings->NReqTrg >= mSettings->nTriggerId) {
    LOG_ERROR << "StPrepEmbedMaker::SetTrgOpt too many triggers are requested!" <<endm;
    return;
  }

  mSettings->ReqTrgId[mSettings->NReqTrg] = TrgId ;
  mSettings->NReqTrg ++ ;
  LOG_INFO << "StPrepEmbedMaker::SetTrgOpt trigger " << mSettings->ReqTrgId[mSettings->NReqTrg-1] << " requested" << endm;
}

//________________________________________________________________________________
void StPrepEmbedMaker::SetZVertexCut(const Double_t vzlow, const Double_t vzhigh)
{
  // Make sure vzlow < vzhigh
  if( (vzlow > vzhigh) || (vzlow == 0.0 && vzhigh == 0.0) ){
    LOG_ERROR << "StPrepEmbedMaker::SetZVertexCut  input vzlow > vzhigh or vzlow = vzhigh = 0" << endm;
    return;
  }

  mSettings->vzlow  = vzlow ;
  mSettings->vzhigh = vzhigh ;
  LOG_INFO << "StPrepEmbedMaker::SetZVertexCut  Cut z-vertex in " << mSettings->vzlow 
    << " < vz < "
    << mSettings->vzhigh
    << " (cm)" << endm;
}

//________________________________________________________________________________
void StPrepEmbedMaker::SetVrCut(const Double_t vr)
{
  // Make sure vr != 0
  if( vr == 0.0 ){
    LOG_ERROR << "StPrepEmbedMaker::SetVrCut  input vr = 0" << endm;
    return;
  }

  mSettings->vr = vr ;
  LOG_INFO << "StPrepEmbedMaker::SetVrCut  Cut vr in " << mSettings->vr
    << " (cm)" << endm;
}

//________________________________________________________________________________
void StPrepEmbedMaker::SetVpdVzCut(const Double_t vpdvz)
{
  // Make sure vpdvz > 0
  if( vpdvz <= 0.0 ){
    LOG_ERROR << "StPrepEmbedMaker::SetVpdVzCut  input |vpdvz-vz| <= 0" << endm;
    return;
  }

  mSettings->vpdvz = vpdvz ;
  LOG_INFO << "StPrepEmbedMaker::SetVpdVzCut  Cut |vpdvz-vz| in " << mSettings->vpdvz
    << " (cm)" << endm;
}

//________________________________________________________________________________
void StPrepEmbedMaker::SetPVRankCut(const Double_t pvrank)
{
  mSettings->pvrank = pvrank ;
  LOG_INFO << "StPrepEmbedMaker::SetPVRankCut  Cut P.V. ranking larger than " << mSettings->pvrank
    << endm;
}

//________________________________________________________________________________
void StPrepEmbedMaker::OpenFzFile()
{
  // Swtich to enable writing .fz file (default is off, i.e. do not write .fz file) 
  mOpenFzFile = kTRUE ;
  LOG_INFO << "StPrepEmbedMaker::OpenFzFile  Write .fz file. File basename will be taken "
           << "from daq file basename" << endm;
}


//________________________________________________________________________________
void StPrepEmbedMaker::phasespace(const Int_t mult)
{
  Double_t rapidityMin = mSettings->etalow ;
  Double_t rapidityMax = mSettings->etahigh ;

  /// Set wider rapidity range in order NOT to cut out the eta 
  if( mSettings->mode.CompareTo("Spectrum", TString::kIgnoreCase) == 0 ){
    rapidityMin = -mRapidityMaximumCut ;
    rapidityMax =  mRapidityMaximumCut ;
  }

  Do( Form("phasespace %i %i %f %f %f %f;",
      mult, mSettings->pid,
      mSettings->ptlow, mSettings->pthigh,
      rapidityMin, rapidityMax)
  );
}

//________________________________________________________________________________
void StPrepEmbedMaker::gkine(const Int_t mult, const Double_t vzmin, const Double_t vzmax)
{
  Double_t rapidityMin = mSettings->etalow ;
  Double_t rapidityMax = mSettings->etahigh ;

  /// Set wider rapidity range in order NOT to cut out the eta 
  if( mSettings->mode.CompareTo("Spectrum", TString::kIgnoreCase) == 0 ){
    rapidityMin = -mRapidityMaximumCut ;
    rapidityMax =  mRapidityMaximumCut ;
  }

  Do( Form("gkine %i %i %f %f %f %f %f %f %f %f;",
      mult, mSettings->pid,
      mSettings->ptlow, mSettings->pthigh,
      rapidityMin, rapidityMax,
      mSettings->philow, mSettings->phihigh, 
      vzmin, vzmax)
  );
}


/* -------------------------------------------------------------------------
 * $Log: StPrepEmbedMaker.cxx,v $
 * Revision 1.18  2018/10/12 13:44:49  zhux
 * updated vertex errors from moretags
 *
 * Revision 1.17  2018/09/29 13:22:35  zhux
 * updated primary vertex sigma names in moretree
 *
 * Revision 1.16  2018/04/04 18:48:35  smirnovd
 * After a long and fruitful discussion with DK it was decided to remove the static_cast. The world is in balance again and veprbl can sleep well at night
 *
 * Revision 1.15  2018/03/30 23:36:32  smirnovd
 * Fix improper cast revealed in 64-bit build
 *
 * Revision 1.14  2016/10/14 07:12:26  zhux
 * "refmult" and "vx,vy,vz" will be read from moretags.root file if it exists.
 * The minimum number of embedded particles are now set to 5 instead of 1, when the number of embeded particles is set to be proportional to refmult.
 *
 * Revision 1.13  2016/06/21 16:05:18  jwebb
 * Init all members.
 *
 * Revision 1.12  2013/03/26 13:38:18  fisyak
 * restore back modififcations as not related to drop in no. of reconstructed tracks
 *
 * Revision 1.10  2013/02/06 22:04:24  fisyak
 * add missing call to gstar
 *
 * Revision 1.9  2013/01/17 15:09:19  fisyak
 * Remove starsim make, which destroys virtual function tables, bug #2487
 *
 * Revision 1.8  2012/06/03 06:34:45  zhux
 * Added a switch to cut on the ranking of primary vertex
 *
 * Revision 1.7  2012/05/13 06:37:04  zhux
 * Added switch to choose between the two kinematic variables: rapidty or pseudo-rapdity
 *
 * Revision 1.6  2012/04/23 23:53:23  zhux
 * Added a switch to cut on |VpdVz-Vz|
 *
 * Revision 1.5  2011/12/05 15:50:49  zhux
 * Add switch to prime the first event with deuterons (for dbar, tbar and hypertritons embedding).
 * see ticket# 2097 for details.
 *
 * Revision 1.4  2010/11/30 23:32:22  hmasui
 * Add fz file and a switch to enable writing fz file
 *
 * Revision 1.3  2010/11/07 23:28:36  hmasui
 * Added transverse vertex cut
 *
 * Revision 1.2  2010/05/26 03:23:09  hmasui
 * Implement spectrum option by gstar_micky
 *
 * Revision 1.1  2010/04/05 20:18:55  jeromel
 * Moved from one level up
 *
 * Revision 1.14  2010/04/02 20:14:50  didenko
 * StPrepEmbedMaker for Hiroshi
 *
 * Revision 1.13  2010/02/09 01:07:32  andrewar
 * Changed defualt setting of mSpreadMode to kFALSE. Modified logic when looking up
 * vertex errors; first looks at tags.root, then (if failure) attempts moretags.root.
 * Added backward compatibility for embedding mode (default is now FlatPt).
 *
 * Revision 1.12  2010/02/05 23:01:04  andrewar
 * Update with spectra embedding mode.
 *
 * Revision 1.11  2009/07/01 23:20:34  andrewar
 * Updated with codes for Strangeness embedding (taken from Xianglei's code,
 * Feb 09)
 *
 * Revision 1.9  2008/09/04 00:07:27  fisyak
 * Change default from gkine to phasespace
 *
 * Revision 1.8  2008/08/19 23:11:27  andrewar
 * Added initialization for RNDM seeds. Seeding now from the clock and the UNIX
 * process ID (as suggested by Marco).
 *
 * Revision 1.7  2008/08/15 15:09:37  lbarnby
 * Skip embedding events without primary vertex + flag for this behaviour (default is to skip)
 *
 * Revision 1.6  2008/07/30 14:11:55  lbarnby
 * Changed tags used to get multiplicity for calculating how many particle to embed from numberOfPrimaryTracks to uncorrectedNumberOfPrimaries because former is created by StPCollTagMaker which was not used in P08ic Au+Au 9 GeV production whereas latter is from StTagsMaker
 *
 * Revision 1.5  2008/06/17 16:08:56  fisyak
 * Move access to TGiant into InitRun
 *
 * Revision 1.4  2008/01/21 01:23:37  perev
 * WarnOff
 *
 * Revision 1.3  2007/09/18 21:53:45  fisyak
 * Don't use field set from GEANT
 *
 * Revision 1.2  2007/08/29 22:59:33  andrewar
 * Added some calls for GEANT simulation of embedded particles.
 *
 * Revision 1.1  2007/07/12 20:34:35  fisyak
 * Add StPrepEmbedMaker
 *
 *
 * -------------------------------------------------------------------------
 */

