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
 * $Id: StPrepEmbedMaker.cxx,v 1.1 2007/07/12 20:34:35 fisyak Exp $
 *
 */

#include "TFile.h"
#include "StMessMgr.h"
#include "StPrepEmbedMaker.h"
#include "StEvtHddr.h"
#include "TTree.h"

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
};
static embedSettings  *mSettings = 0;
//________________________________________________________________________________
StPrepEmbedMaker::StPrepEmbedMaker(const Char_t *name) : StMaker(name) {
  mEventCounter = 0;
  mGeant3=0;
  mSettings = new embedSettings();
  mSettings->mult=0.;
  mFile = 0;
  mTree = 0;
}
//________________________________________________________________________________
StPrepEmbedMaker::~StPrepEmbedMaker() { 
  SafeDelete(mFile);
}
//________________________________________________________________________________
Int_t StPrepEmbedMaker::Init() {
  //Input settings from setup file
  
  //Call geant maker, set defaults
  mGeant3 = TGiant3::Geant3();
  if( ! mGeant3)
    {
      LOG_ERROR << "Geant3 pointer not found. exiting."<<endm;
      return kStErr;
    }
  if (mTagFile == "") {
    LOG_ERROR << "TagFile has not been defined" << endm;
    return kStErr;
  }
  mFile = new TFile(mTagFile);
  if (! mFile ) {
    LOG_ERROR << "TagFile : " << mTagFile << " cannot be opened" << endm;
    return kStErr;
  }
  mTree = (TTree *) mFile->Get("Tag");
    if (! mTree ) {
      LOG_ERROR << "In TagFile : " << mTagFile << " cannot find TTree \"Tag\"" << endm;
      return kStErr;
    }
    //    Do("detp  hadr_on");
    return StMaker::Init();
}
//________________________________________________________________________________
Int_t StPrepEmbedMaker::Make() {
  mEventCounter++;  // increase counter
  StEvtHddr* EvtHddr = (StEvtHddr*) GetDataSet("EvtHddr");
  if (! EvtHddr) {
    LOG_ERROR << "StPrepEmbedMaker::Make EvtHddr has not been found" << endm;
    return kStErr;
  }
  Int_t nFound = mTree->Draw("numberOfPrimaryTracks",
			     Form("mRunNumber==%i&&mEventNumber==%i",EvtHddr->GetRunNumber(),EvtHddr->GetEventNumber()),
			     "goff");
  if (nFound != 1) {
    LOG_ERROR << "StPrepEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber() << "/" << EvtHddr->GetEventNumber() 
	      << " has been found in tag file" << nFound << " times" <<  endm;
    return kStErr;
  }
  Int_t numberOfPrimaryTracks = (Int_t) mTree->GetV1()[0];
  // Extract info for mult for this event
  Int_t npart=int(mSettings->mult * numberOfPrimaryTracks);
  if (! npart) {
    LOG_ERROR << "StPrepEmbedMaker::Make EvtHddr skip event" << EvtHddr->GetEventNumber() 
	      << " because numberOfPrimaryTracks " << numberOfPrimaryTracks << " is too low" << endm; 
    return kStErr;
  }
  nFound = (Int_t) mTree->Draw("primaryVertexX:primaryVertexY:primaryVertexZ",
			       Form("mRunNumber==%i&&mEventNumber==%i",EvtHddr->GetRunNumber(),EvtHddr->GetEventNumber()),
			       "goff");
  Double_t xyz[3] = {mTree->GetV1()[0],mTree->GetV2()[0],mTree->GetV3()[0]};
  // gkine              PTLOW,   PTHIGH,   YLOW,   YHIGH,   PHILOW,   PHIHIGH,   ZLOW,   ZHIGH
  TString cmd(Form("gkine %i %i %f %f %f %f %f %f %f %f;",
		   npart, mSettings->pid,
		   mSettings->ptlow, mSettings->pthigh,
		   mSettings->etalow, mSettings->etahigh,
		   mSettings->philow, mSettings->phihigh, xyz[2], xyz[2]));
  Do(cmd.Data());
  Do(Form("vxyz %f %f %f",xyz[0],xyz[1],xyz[2]));
  Do("vsig 0 0;");
  return kStOK;
}
//_____________________________________________________________________________
void StPrepEmbedMaker::Do(const Char_t *job)
{  
  Int_t l=strlen(job);
  if (l) {
    LOG_INFO << "StPrepEmbedMaker::Do(" << job << ");" << endm;
    mGeant3->Kuexel(job);
  }
}
//________________________________________________________________________________
void StPrepEmbedMaker::SetPartOpt(Int_t pid, Double_t mult)  { 
  mSettings->mult=mult; mSettings->pid=pid; 
  LOG_INFO << "StPrepEmbedMaker::SetPartOpt mult = " << mSettings->mult
	   << " pid = " << mSettings->pid << endm;
}
//________________________________________________________________________________
void StPrepEmbedMaker::SetOpt(Double_t ptlow, Double_t pthigh,
			      Double_t etalow, Double_t etahigh, Double_t philow,
			      Double_t phihigh) {
  mSettings->ptlow=ptlow;   mSettings->pthigh=pthigh; 
  mSettings->etalow=etalow; mSettings->etahigh=etahigh;
  mSettings->philow=philow;  mSettings->phihigh=phihigh;
  LOG_INFO << "StPrepEmbedMaker::SetOpt ptlow = " << mSettings->ptlow << " pthigh = " << mSettings->pthigh
	   << " etalow = " << mSettings->etalow << " etahigh = " << mSettings->etahigh
	   << " philow = " << mSettings->philow << " phihigh = " << mSettings->phihigh << endm;
}
/* -------------------------------------------------------------------------
 * $Log: StPrepEmbedMaker.cxx,v $
 * Revision 1.1  2007/07/12 20:34:35  fisyak
 * Add StPrepEmbedMaker
 *
 *
 * -------------------------------------------------------------------------
 */

