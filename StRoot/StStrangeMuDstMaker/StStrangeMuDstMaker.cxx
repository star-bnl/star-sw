// $Id: StStrangeMuDstMaker.cxx,v 3.3 2000/08/10 01:14:17 genevb Exp $
// $Log: StStrangeMuDstMaker.cxx,v $
// Revision 3.3  2000/08/10 01:14:17  genevb
// Upped basket size of event branch; Set old file format
//
// Revision 3.2  2000/07/17 20:28:40  genevb
// File size limitation workaround, some under the hood improvements
//
// Revision 3.1  2000/07/14 21:28:34  genevb
// Added V0Mc index for XiMc, fixed bug with entries for XiMc, cleaned up controllers
//
// Revision 3.0  2000/07/14 12:56:49  genevb
// Revision 3 has event multiplicities and dedx information for vertex tracks
//
// Revision 2.1  2000/06/09 22:17:10  genevb
// Allow MC data to be copied between DSTs, other small improvements
//
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

// Set maximum file size to 1.9 GB (Root has a 2GB limit)
#define MAXFILESIZE 1900000000
#define EachDoT(proc) \
  for (Int_t i=1; i<strDstT; i++) { \
    if (doT[i]) proc; }
#define EachController(proc) EachDoT(cont[i]->proc);

ClassImp(StStrangeMuDstMaker)
//_____________________________________________________________________________
StStrangeMuDstMaker::StStrangeMuDstMaker(const char *name) : StMaker(name) {

  muDst = 0;
  tree = 0;
  evClonesArray = 0;
  dstMaker = 0;
  cuts = new StStrangeCuts();
  SetNumber(-2);
  outFileNum = 1;

  TString suffix = "MuDst.root";
  for (Int_t i=0; i<strDstT; i++) {
    doT[i] = kFALSE;
    cont[i] = 0;
    files[i]=0;
    
    // Defaults file names: evMuDst.root, v0MuDst.root, etc.
    TString prefix = strTypeNames[i];
    prefix.ToLower();
    prefix.Append(suffix);
    size_t len = prefix.Length();
    file[i] = new char[len + 1];
    strncpy(file[i],prefix.Data(),len);
    (file[i])[len] = 0;
  }

  doMc = kFALSE;
  rw = StrangeNoFile;

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
  EachDoT(cont[i] = StStrangeControllerBase::Instantiate(i));

  v0 = cont[v0T];
  xi = cont[xiT];
  kink = cont[kinkT];

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
                      << " in file " << file[evT] << endm;
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
  EachController(InitCreateDst());

  if (!dstMaker) {
    Int_t split=2;
    Int_t bsize=1024000;
    TBranch* branch = tree->Branch("Event",&evClonesArray,bsize,split);
    branch->SetFile(file[evT]);
    cuts->Assure();
  }
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::InitCreateSubDst() {

  evClonesArray = dstMaker->GetEvClonesArray();
  Int_t split=2;
  Int_t bsize=64000;
  TBranch* branch = tree->Branch("Event",&evClonesArray,bsize,split);
  branch->SetFile(file[evT]);
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
  } else if (files[evT]) {
    event_number -= evNumber;
  }
  Int_t tree_size = (Int_t) tree->GetEntries();
  if (event_number >= tree_size) {
    if (files[evT]) {                                     // If reading from
      SetStFiles();                                    // multiple files, then
      if (!file[evT]) return kStErr;                   // get the next file
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
    cuts->Fill(strTypeNames[v0T], GetDataSet("ev0par2"));
    cuts->Fill(strTypeNames[xiT], GetDataSet("exipar"));
    cuts->Fill(strTypeNames[kinkT], GetDataSet("tkf_tkfpar"));
    Int_t fSize = cuts->GetCollection()->GetSize();
    if (!(fSize-iSize))
      gMessMgr->Warning("StStrangeMuDstMaker: no cut parameters found.");
    firstEvent = kFALSE;
  }  

  // Get event
  StEventMaker* evMaker = (StEventMaker *) GetMaker("events");
  StEvent* event = evMaker->event();
  if (!event) return kStOK; 
  if (!(event->primaryVertex())) {
    gMessMgr->Warning("StStrangeMuDstMaker: no primary vertex; skipping event.");
    return kStWarn;
  }

  new((*evClonesArray)[0]) StStrangeEvMuDst(*event);
  EachController(MakeCreateDst(*event));
  if (doMc) MakeCreateMcDst();

  CheckFile();
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
      if (doT[xiT]) { // looking for Xis
        if ((parent->geantId()==23)||(parent->geantId()==24)||
            (parent->geantId()==31)||(parent->geantId()==32))
	  cont[xiT]->MakeCreateMcDst(*mcVertexIt);
      } else if (doT[v0T]) { // looking for V0s
        if ((parent->geantId()==16)||(parent->geantId()==18)||
            (parent->geantId()==26)) 
	  cont[v0T]->MakeCreateMcDst(*mcVertexIt);
      }
      if (doT[kinkT]) { // looking for Kinks
	if ((parent->geantId()==11)||(parent->geantId()==12))
	  cont[kinkT]->MakeCreateMcDst(*mcVertexIt);
      }
    }
  }//end of loop over MC  vertices

  // Do all non-Xi V0's at the end...
  if (doT[xiT] && doT[v0T]) {
    for (StMcVertexIterator mcVertexIt = mcVertices.begin();
                          mcVertexIt != mcVertices.end(); mcVertexIt++) {
      const StMcTrack* parent = (*mcVertexIt)->parent();
      if ((parent) && ((parent->geantId()==16)||(parent->geantId()==18)||
            (parent->geantId()==26))) {
        const StMcTrack* parent2 = parent->parent();
        // Check for cascade parentage
        if (!((parent2) && ((parent2->geantId()==23)||(parent2->geantId()==24)||
            (parent2->geantId()==31)||(parent2->geantId()==32))))
	  cont[v0T]->MakeCreateMcDst(*mcVertexIt);
      }
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
  files[evT] = eFiles;
  files[v0T] = vFiles;
  files[xiT] = xFiles;
  files[kinkT] = kFiles;
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
  if (eFile) file[evT] = eFile;
  if (vFile) file[v0T] = vFile;
  if (xFile) file[xiT] = xFile;
  if (kFile) file[kinkT] = kFile;
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SetStFiles () {
  for (Int_t i=0; i<strDstT; i++) {
    if (files[i]) {
      files[i]->GetNextBundle();
      file[i] = const_cast<char*> (files[i]->GetFileName(0));
    }
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
    
  if( ! (muDst = new TFile(file[evT],option)) ) {
    gMessMgr->Error() << "StStrangeMuDstMaker: Error opening event file:\n  "
                      << file[evT] << endm;
    return kStErr;
  }
  if (rw == StrangeWrite) muDst->SetFormat(1);   // Necessary to read MuDst in plain root
  gMessMgr->Info() << "StStrangeMuDstMaker: Opened event file:\n  "
                   << file[evT] << endm;     
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
void StStrangeMuDstMaker::CheckFile() {
  if (muDst->GetBytesWritten() > MAXFILESIZE) {
    gMessMgr->Warning() << "StStrangeMuDstMaker: File size limit "
                        << MAXFILESIZE << " exceeded!\n"
			<< "           Closing file " << file[evT] << endm;
    CloseFile();
    char buf_[10];
    sprintf(buf_,"_%d",(++outFileNum));
    for (Int_t i=0; i<strDstT; i++) {
      TString fixer = file[i];
      size_t len = fixer.Length();
      if (outFileNum>2) {
        TString suffix = strrchr(file[i],'.');
        size_t last_ = fixer.Last('_');
	size_t len_ = len - last_;
	fixer.Remove(last_,len_).Append(buf_).Append(suffix);
        len = fixer.Length();
      } else {
        size_t lastdot = fixer.Last('.');
        fixer.Insert(lastdot,buf_);
        len = fixer.Length();
        file[i] = new char[len + 5];
      }
      strncpy(file[i],fixer.Data(),len);
      (file[i])[len] = 0;
    }
    OpenFile();
    InitCreateDst();
  }
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SelectEvent() {
  EachController(Select(-1));
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::UnselectEvent() {
  EachController(Unselect(-1));
}
