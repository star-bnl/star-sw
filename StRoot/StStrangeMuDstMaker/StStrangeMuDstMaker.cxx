// $Id: StStrangeMuDstMaker.cxx,v 2.0 2000/06/02 22:11:54 genevb Exp $
// $Log: StStrangeMuDstMaker.cxx,v $
// Revision 2.0  2000/06/02 22:11:54  genevb
// New version of Strangeness micro DST package
//
// Revision 1.8  2000/04/27 14:17:34  genevb
// Safer deletes
//
// Revision 1.7  2000/04/27 14:13:04  genevb
// Added old tree deletion
//
// Revision 1.6  2000/04/18 02:30:04  genevb
// Added multi-file capabilities
//
// Revision 1.5  2000/04/07 18:18:30  genevb
// Additional crash protection
//
// Revision 1.4  2000/04/06 14:51:11  genevb
// Fixed bug with storing event info when making subDST
//
// Revision 1.3  2000/04/05 20:23:53  genevb
// Introduce creating sub-Micro DSTs, dynamic expansion of clones arrays as needed, SetNoKeep() function
//
// Revision 1.2  2000/03/29 20:52:13  genevb
// Added StKinkMuDst, replaced arrays
//
// Revision 1.1  2000/03/29 03:10:07  genevb
// Introduction of Strangeness Micro DST package
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StStrangeMuDstMaker strangeness micro DST maker                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "StStrangeMuDstMaker.h"
#include "TFile.h"
#include "TTree.h"
#include "StTree.h"
#include "StEventMaker/StEventMaker.h"
#include "StEvent/StEvent.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StEvent.h"
#include "StStrangeEvMuDst.hh"
#include "StStrangeCuts.hh"
#include "StMcEventTypes.hh"
#include "StParticleDefinition.hh"
#include "StMessMgr.h"
#include "StStrangeControllerInclude.h"

#define EachController(proc) \
  if (doV0) v0->proc; \
  if (doXi) xi->proc; \
  if (doKink) kink->proc;

ClassImp(StStrangeMuDstMaker)
//_____________________________________________________________________________
StStrangeMuDstMaker::StStrangeMuDstMaker(const char *name) : StMaker(name) {

  doV0 = kFALSE;
  doXi = kFALSE;
  doKink = kFALSE;
  doMc = kFALSE;
  rw = StrangeNoFile;

  muDst = 0;
  tree = 0;
  evClonesArray = 0;
  dstMaker = 0;
  cuts = new StStrangeCuts();
  SetNumber(-2);

  evFile = "evMuDst.root";
  v0File = "v0MuDst.root";
  xiFile = "xiMuDst.root";
  kinkFile = "kinkMuDst.root";
  evFiles = 0;
  v0Files = 0;
  xiFiles = 0;
  kinkFiles = 0;
}
//_____________________________________________________________________________
StStrangeMuDstMaker::~StStrangeMuDstMaker() {

  if (!dstMaker) {           // Don't delete other maker's array!
    delete evClonesArray; evClonesArray = 0;
  }
  delete cuts; cuts = 0;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::Init() {

  firstEvent = kTRUE;
  if (Debug()) gMessMgr->Debug() << "In StStrangeMuDstMaker::Init() ... "
                               << GetName() << endm; 
  
  if (OpenFile() == kStErr) return kStErr;
  if (!dstMaker) {
    evClonesArray = new TClonesArray("StStrangeEvMuDst",1);
  }
  StStrangeControllerBase::currentMaker = this;
  if (doV0) v0 = new StV0Controller();
  if (doXi) xi = new StXiController();
  if (doKink) kink = new StKinkController();

  if (rw == StrangeRead) {            // READING  the Micro Dst
    InitReadDst();
    if (dstMaker) {
      gMessMgr->Error() << "StStrangeMuDstMaker: cannot read a dst and create "
                        << "a dst with one maker.\n"
			<< "  Will read only." << endm;
      dstMaker = 0;
    }
  } else {                            // CREATING the Micro Dst
    InitCreateDst();
  }
  
  return StMaker::Init();
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::InitReadDst() {
  
  tree = (TTree*) muDst->Get("StrangeMuDst");
  if (!tree) {
    gMessMgr->Error() << "StStrangeMuDstMaker: no StrangeMuDst tree"
                      << " in file " << evFile << endm;
    return;
  }
  tree->SetBranchAddress("Event",&evClonesArray);
  EachController(InitReadDst());
  cuts->Append((TOrdCollection*) muDst->Get("StrangeCuts"));
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::InitCreateDst() {
  
  tree = new TTree("StrangeMuDst","Strangeness Micro-DST");
  tree->SetDirectory(muDst);

  if (doV0) v0->InitCreateDst(v0File);
  if (doXi) xi->InitCreateDst(xiFile);
  if (doKink) kink->InitCreateDst(kinkFile);

  if (!dstMaker) {
    Int_t split=2;
    Int_t bsize=64000;
    TBranch* branch = tree->Branch("Event",&evClonesArray,bsize,split);
    branch->SetFile(evFile);
    cuts->Assure();
  }
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::InitCreateSubDst() {

  Int_t split=1;
  Int_t bsize=64000;
  evClonesArray = dstMaker->GetEvClonesArray();
  TBranch* branch = tree->Branch("Event",&evClonesArray,bsize,split);
  branch->SetFile(evFile);
  EachController(InitCreateSubDst());
  cuts->Append(dstMaker->Cuts().GetCollection());
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::Make() {

  if (Debug()) gMessMgr->Debug() << "In StStrangeMuDstMaker::Make() ... "
                               << GetName() << endm; 

  if (rw == StrangeRead) {            // READING  the Micro Dst
    return MakeReadDst();
  } else if (!dstMaker) {             // CREATING a new Micro Dst
    return MakeCreateDst();
    
  // Else creating a sub-Micro Dst, done in the Clear() stage.
  // However, since the old Micro Dst's Init() is called AFTER the new
  // one's, special initialization for the subDST must be done on the
  // first time through Make().
  } else if (!evClonesArray) {
    InitCreateSubDst();
  }
  return kStOK;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::MakeReadDst() {

  if (!tree) return kStErr;
  Int_t event_number = GetNumber();
  if (event_number == -2) {        // If event numbers aren't supplied,
    event_number = 0;              // start at 0 and increment ourselves.
    SetNumber(-1);                 // Use m_Number = -1 to indicate this.
  } else if (event_number == -1) {
    event_number = tree->GetReadEvent() + 1;
  } else if (evFiles) {
    event_number -= evNumber;
  }
  Int_t tree_size = (Int_t) tree->GetEntries();
  if (event_number >= tree_size) {
    if (evFiles) {                                     // If reading from
      SetStFiles();                                    // multiple files, then
      if (!evFile) return kStErr;                      // get the next file
      CloseFile();                                     // names, close the old
      if (OpenFile() == kStErr) return kStErr;         // files, open the new,
      InitReadDst();                                   // and subtract total
      evNumber += tree_size;                           // events from old file
      event_number -= tree_size;                       // from current event #.
    } else {
      return kStErr;
    }
  }
  if (! tree->GetEvent(event_number)) return kStErr;   // Read the event
  EachController(MakeReadDst());
  
  return kStOK;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::MakeCreateDst() {

  // Get the cut parameters and fill StrangeCuts on the first event
  if (firstEvent) {
    Int_t iSize = cuts->GetCollection()->GetSize();
    cuts->Fill("V0", GetDataSet("ev0par2"));
    cuts->Fill("Xi", GetDataSet("exipar"));
    cuts->Fill("Kink", GetDataSet("tkf_tkfpar"));
    Int_t fSize = cuts->GetCollection()->GetSize();
    if (!(fSize-iSize))
      gMessMgr->Warning("StStrangeMuDstMaker: no cut parameters found.");
    firstEvent = kFALSE;
  }  

  // Get pointers to event and primary vertex
  StEventMaker* evMaker = (StEventMaker *) GetMaker("events");
  if( ! evMaker->event() ) return kStOK; 
  StEvent& event = *(evMaker->event());
  StPrimaryVertex *primaryVertex = event.primaryVertex();
  if( !primaryVertex ) {
    gMessMgr->Error("StStrangeMuDstMaker: no primary vertex");
    return kStErr;
  }
  new((*evClonesArray)[0]) StStrangeEvMuDst(primaryVertex);

  EachController(MakeCreateDst(event));
  if (doMc) MakeCreateMcDst();
  tree->Fill();

  return kStOK;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::MakeCreateMcDst() {

  gMessMgr->Info("StStrangeMuDstMaker::MakeCreateMcDst(): running...");
  
  StMcEvent* mcEvent = 0;
  StMcEventMaker* mcEventMaker = (StMcEventMaker*) GetMaker("StMcEvent");
  if (mcEventMaker) mcEvent = mcEventMaker->currentMcEvent();
  if (!mcEvent)   {
    gMessMgr->Error() << "StStrangeMuDstMaker: no StMcEvent! \n" 
	 << " mc branch and assoc branch  will not be filled. " << endm;       
    return 0;
  }
  if (!GetMaker("StAssociationMaker")) {
    gMessMgr->Warning() << "StStrangeMuDstMaker: no associated info! \n" 
		      << " assoc branch will not be filled. " << endm;
  }

  //Loop over all MC vertices
  StSPtrVecMcVertex& mcVertices = mcEvent->vertices();
  for (StMcVertexIterator mcVertexIt = mcVertices.begin();
                          mcVertexIt != mcVertices.end(); mcVertexIt++) {
    const StMcTrack* parent = (*mcVertexIt)->parent();
    if (parent) {
      if (doV0) // looking for V0s
        if ((parent->geantId()==16)||(parent->geantId()==18)||
            (parent->geantId()==26)) 
	  v0->MakeCreateMcDst(*mcVertexIt);
      if (doXi) // looking for Xis
        if ((parent->geantId()==23)||(parent->geantId()==24)||
            (parent->geantId()==31)||(parent->geantId()==32))
	  xi->MakeCreateMcDst(*mcVertexIt);
      if (doKink) // looking for Kinks
	if ((parent->geantId()==11)||(parent->geantId()==12))
	  kink->MakeCreateMcDst(*mcVertexIt);
    }
  }//end of loop over MC  vertices
  EachController(PrintNumMc());

  return kStOK;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::MakeCreateSubDst() {

  EachController(MakeCreateSubDst());
  tree->Fill();

  return kStOK;
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::Clear(Option_t *option) {

  if (Debug()) gMessMgr->Debug() << "In StStrangeMuDstMaker::Clear() ... "
                               << GetName() << endm; 

  if (tree) {
    if (dstMaker) {                                   // Making a subDST
      MakeCreateSubDst();
    } else if (evClonesArray) evClonesArray->Clear(); // Not if making a subDST

    EachController(Clear());
    if (rw == StrangeNoKeep) tree->Reset();
  }

  StMaker::Clear(option);
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::Finish() {

  if (Debug()) gMessMgr->Debug() << "In StStrangeMuDstMaker::Finish() ... "
                               << GetName() << endm; 
  CloseFile();
  EachController(Finish());

  return kStOK;
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SetWrite(char* eFile, char* vFile,
                                   char* xFile, char* kFile) {
  rw = StrangeWrite;
  SetFiles(eFile,vFile,xFile,kFile);
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SetRead (char* eFile, char* vFile,
                                   char* xFile, char* kFile) {
  rw = StrangeRead;
  SetFiles(eFile,vFile,xFile,kFile);
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SetRead (StFile* eFiles, StFile* vFiles,
                                   StFile* xFiles, StFile* kFiles) {
  rw = StrangeRead;
  evFiles = eFiles;
  v0Files = vFiles;
  xiFiles = xFiles;
  kinkFiles = kFiles;
  evNumber = 0;
  SetStFiles();
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SetNoKeep() {
  rw = StrangeNoKeep;
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SetFiles (char* eFile, char* vFile,
                                    char* xFile, char* kFile) {
  if (eFile) evFile = eFile;
  if (vFile) v0File = vFile;
  if (xFile) xiFile = xFile;
  if (kFile) kinkFile = kFile;
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SetStFiles () {
  if (evFiles) {
    evFiles->GetNextBundle();
    evFile = const_cast<char*> (evFiles->GetFileName(0));
  }
  if (v0Files) {
    v0Files->GetNextBundle();
    v0File = const_cast<char*> (v0Files->GetFileName(0));
  }
  if (xiFiles) {
    xiFiles->GetNextBundle();
    xiFile = const_cast<char*> (xiFiles->GetFileName(0));
  }
  if (kinkFiles) {
    kinkFiles->GetNextBundle();
    kinkFile = const_cast<char*> (kinkFiles->GetFileName(0));
  }
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::OpenFile() {
  char* option=0;
  if (rw == StrangeRead)
    option = "READ";
  else if (rw == StrangeWrite)
    option = "RECREATE";
  else
    return kStOk;
    
  if( ! (muDst = new TFile(evFile,option)) ) {
    gMessMgr->Error() << "StStrangeMuDstMaker: Error opening event file:\n  "
                      << evFile << endm;
    return kStErr;
  }
  gMessMgr->Info() << "StStrangeMuDstMaker: Opened event file:\n  "
                   << evFile << endm;     
  return kStOk;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::CloseFile() {
  if (muDst) {
    if (rw == StrangeWrite) {
      muDst->Write();
      muDst->cd();
      cuts->Store();
    }
    muDst->Close();
    tree = 0;
  }
  return kStOk;
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SelectEvent() {
  EachController(Select(-1));
}
