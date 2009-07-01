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
 * $Id: StPrepEmbedMaker.cxx,v 1.11 2009/07/01 23:20:34 andrewar Exp $
 *
 */

#include "TFile.h"
#include "StMessMgr.h"
#include "StPrepEmbedMaker.h"
#include "StEvtHddr.h"
#include "TTree.h"
#include "StGenericVertexMaker/StGenericVertexMaker.h"
#include "StGenericVertexMaker/StFixedVertexFinder.h"

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
  Int_t rnd1;
  Int_t rnd2;
  Double_t vzlow;
  Double_t vzhigh;
  Int_t NReqTrg;
  Int_t ReqTrgId[32];
  TString mode;
};

static embedSettings  *mSettings = 0;
//________________________________________________________________________________
StPrepEmbedMaker::StPrepEmbedMaker(const Char_t *name) : StMaker(name) {
  mEventCounter = 0;
  mGeant3=0;

  if( !mSettings ){
    mSettings = new embedSettings();
    mSettings->mult=0.;
  }
  mFile = 0;
  mTree = 0;
  mSkipMode = kTRUE;
}
//________________________________________________________________________________
StPrepEmbedMaker::~StPrepEmbedMaker() { 
  SafeDelete(mFile);
}
//________________________________________________________________________________
Int_t StPrepEmbedMaker::Init() {

	srand((unsigned)time(0));
        mSettings->rnd1 = abs(int(rand()*10000)+getpid());
        mSettings->rnd2 = abs(int(rand()*10000)+getpid());


    return StMaker::Init();
}

//----
Int_t StPrepEmbedMaker::InitRun(int runnum)
{
  //Field can change from event to event (malformed event headers?) - set once per run
  //  Do("field = 5.");
  //Input settings from setup file
  
  //Call geant maker, set defaults
  if (! mGeant3) {
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
    
   if (mSpreadMode){
         mMoreTagsFile = mTagFile;
         int indx1 = mMoreTagsFile.Index(".tags",0);
         int indx2 = mMoreTagsFile.Last('.');
         if (indx1!=indx2) mMoreTagsFile.Remove(indx1+1,(indx2-indx1));
         mMoreTagsFile.Insert(indx1+1,"moretags.");
         mMoreFile = new TFile(mMoreTagsFile);
         if (! mMoreFile ) {
            LOG_ERROR << "MoreTagsFile : " << mMoreTagsFile << " cannot be opened" << endm;
            return kStErr;
         }
         mMoreTree = (TTree *) mMoreFile->Get("MoreTags");
         if (! mMoreTree ) {
            LOG_ERROR << "In MoreTagsFile : " << mMoreTagsFile << " cannot find TTree \"MoreTags\"" << endm;
            return kStErr;
         }
    }
    
    
//    Do("detp  hadr_on");
 //   Do("make gstar");
    cout <<"Setting up Jpsi particle"<<endl;
    Do("vec/cr JBUF(1)");
    Do("vec/cr BR(6) R 100. 0. 0. 0. 0. 0.");
    Do("vec/cr MODE(6) I 302 0 0 0 0 0");
    Do("spart 160 'JPSI' 3 3.09688 0 8.E-21 JBUF 0 BR MODE");	

    Do("vec/cr UBUF1(1)");
    Do("vec/cr BRATIO1(6) R 100. 0. 0. 0. 0. 0.");
    Do("vec/cr MODE1(6) I 809 0 0 0 0 0");
    Do("spart 16 'KAON 0 SHORT' 3 0.497671 0 8.922E-11 UBUF1 0 BRATIO1 MODE1");
  
    Do("vec/cr UBUF2(1)");
    Do("vec/cr BRATIO2(6) R 100. 0. 0. 0. 0. 0.");
    Do("vec/cr MODE2(6) I 1112 0 0 0 0 0");
    Do("spart 151 'Phi-KK' 3 1.0194 0 1.482e-22 UBUF2 0 BRATIO2 MODE2");

    Do("vec/cr UBUF2(1)");
    Do("vec/cr BRATIO3(6) R 50. 50. 0. 0. 0. 0.");
    Do("vec/cr MODE3(6) I 0760 0 0 0 0 0");
    Do("spart 845 'Special DALITZ' 4 1.5318 +1 8.4e-17 UBUF2 0 BRATIO2 MODE3");
    Do("vec/cr BRATIO4(6) R 50. 50. 0. 0. 0. 0.");
    Do("vec/cr MODE4(6) I 010203 0 0 0 0 0");
    Do("spart 60 'Special DALITZ' 4 .1349766 0 8.4e-17 UBUF2 0 BRATIO4 MODE4");

    Do("vec/cr UBUF2(1)");
    Do("vec/cr BRATIO2(6) R 100. 0. 0. 0. 0. 0.");
    Do("vec/cr MODE2(6) I 203 0 0 0 0 0");
    Do("spart 161 'Upsilon1S' 4 9.460  0 1.254e-20 UBUF2 0 BR(6) MODE2");
    Do("spart 162 'Upsilon2S' 4 10.023 0 1.545e-20 UBUF2 0 BR(6) MODE2");
    Do("spart 163 'Upsilon3S' 4 10.355 0 2.556e-20 UBUF2 0 BR(6) MODE2");

       
               
 
    Do("detp  hadr_on");
    TString cmd("rndm ");
    cmd+=mSettings->rnd1; cmd+=" "; cmd+=mSettings->rnd2;
    Do(cmd.Data());
    
    Do("user/output o temp.fz");


  }
  return 0;
}


//________________________________________________________________________________
Int_t StPrepEmbedMaker::Make() {
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
  LOG_INFO << "StPrepEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber() << "/" << EvtHddr->GetEventNumber() 
	   << " has been found with uncorrectedNumberOfPrimaries = " <<  mTree->GetV1()[0] 
	   << " and primaryVertexFlag = " << mTree->GetV2()[0]  <<  endm;
   if (mTree->GetV1()[0] <= 0 || mTree->GetV2()[0] ) {
     LOG_ERROR << "StPrepEmbedMaker::Make reject this event" << endm;
     return kStErr;
  }
 
  Int_t numberOfPrimaryTracks = (Int_t) mTree->GetV1()[0];
  // Extract info for mult for this event
  Int_t npart;
  if(mSettings->mult < 1.) 
    {
      npart=int(mSettings->mult * numberOfPrimaryTracks);
      if (! npart)
      {
	LOG_INFO << "StPrepEmbedMaker::Event " << EvtHddr->GetEventNumber() 
	      << " has too small numberOfPrimaryTracks " << numberOfPrimaryTracks << " for the mult fraction requested. Forcing npart to 1." << endm; 
	npart=1;
      }
  
    }
  else
    {
      npart = int (mSettings->mult);
    }


  nFound = (Int_t) mTree->Draw("primaryVertexX:primaryVertexY:primaryVertexZ",
			       Form("mRunNumber==%i&&mEventNumber==%i",
				    EvtHddr->GetRunNumber(),
				    EvtHddr->GetEventNumber()),
			       "goff");
  Double_t xyz[3] = {mTree->GetV1()[0],mTree->GetV2()[0],mTree->GetV3()[0]};
  // Skip event if no primary vertex - effectively if tags say it is 0,0,0
  if (mSkipMode == kTRUE){ 
    if (fabs(xyz[0])<1e-7 && fabs(xyz[1])<1e-7 && fabs(xyz[2])<1e-7 ){
      LOG_INFO << "StPrepEmbedMaker::Event " << EvtHddr->GetEventNumber()
	       << " has tags with vertex approx at (0,0,0) - probably no PV, skipping." << endm;
      return kStSKIP;
    }
  }
  //Done set up for event.

  //Setup embedded particle
  // gkine      npart ID        PTLOW,   PTHIGH,   YLOW,   YHIGH,   PHILOW,   PHIHIGH,   ZLOW,   ZHIGH
  //make sure zlow!=zhigh in particle definition - not sure of result. 
  //Z vertex will be forced in vxyz statement.

       Double_t xyzerr[3] = {0.,0.,0.};
          Double_t vzlow = xyz[2];
          Double_t vzhigh = xyz[2];
   
  if(mSettings->mode.Contains("strange"))
    {
     //get primary vertex errors from moretags.root
     nFound = (Int_t) mMoreTree->Draw("VXERR:VYERR:VZERR",
             Form("RunId==%i&&EvtId==%i",
                  EvtHddr->GetRunNumber(),
                  EvtHddr->GetEventNumber()),
             "goff");
     if (nFound != 1) {
          LOG_ERROR << "StPrepEmbedMaker::Make Run/Event = " << EvtHddr->GetRunNumber() << "/" << EvtHddr->GetEventNumber()
             << " has been found in moretags file" << nFound << " times" <<  endm;
          return kStErr;
     }
     //xyzerr[0] = mMoreTree->GetV1()[0];
     //xyzerr[1] = mMoreTree->GetV2()[0];

     xyzerr[0] = 0;
     xyzerr[1] = 0;
     xyzerr[2] = mMoreTree->GetV3()[0];
     LOG_INFO << xyzerr[0] << " " << xyzerr[1] << " " << xyzerr[2] << endm;
     vzlow = -100.0;
     vzhigh = 100.0;

     //Set the vertex for StEvent with StGenericVertexMaker
     StGenericVertexMaker * vmaker = (StGenericVertexMaker*) GetMaker("GenericVertex");
     StFixedVertexFinder * vfinder = (StFixedVertexFinder *) vmaker->GetGenericFinder();
     vfinder->SetVertexPosition(xyz[0],xyz[1],xyz[2]);
  }



  TString cmd;
#if 1 
   cmd = Form("gkine %i %i %f %f %f %f %f %f %f %f;",
            npart, mSettings->pid,
            mSettings->ptlow, mSettings->pthigh,
            mSettings->etalow, mSettings->etahigh,
            mSettings->philow, mSettings->phihigh, xyz[2], xyz[2]);
            //mSettings->philow, mSettings->phihigh, vzlow, vzhigh);
   Do(cmd.Data());
#endif
  cmd = Form("phasespace %i %i %f %f %f %f;",
	     npart, mSettings->pid,
	     mSettings->ptlow, mSettings->pthigh,
	     mSettings->etalow, mSettings->etahigh);
  
  Do(cmd.Data());
  Do(Form("gvertex %f %f %f",xyz[0],xyz[1],xyz[2]));
  if( mSettings->mode.Contains("strange") )
    {
	Do(Form("gspread %f %f %f", xyzerr[0],xyzerr[1],xyzerr[2]));
    }
  else
    { 
 	Do("vsig 0 0;");
    }

  Do("trig 1");

  return kStOK;
}

Int_t StPrepEmbedMaker::Finish()
{
  TString cmd("user/output c temp.fz");
  Do(cmd.Data());
  return 0;
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
void StPrepEmbedMaker::SetPartOpt(Int_t pid, Double_t mult)  
{ 
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

