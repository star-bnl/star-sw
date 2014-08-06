// $Id: StStrangeMuDstMaker.cxx,v 3.17 2002/05/20 21:37:12 genevb Exp $
// $Log: StStrangeMuDstMaker.cxx,v $
// Revision 3.17  2002/05/20 21:37:12  genevb
// Fixed problem with file names for branches
//
// Revision 3.16  2002/05/10 20:59:31  genevb
// Fixed bug with branch status and changed cuts split level
//
// Revision 3.15  2002/04/30 16:02:48  genevb
// Common muDst, improved MC code, better kinks, StrangeCuts now a branch
//
// Revision 3.14  2001/11/05 23:41:06  genevb
// Add more dEdx, B field info, careful of changes to TTree unrolling
//
// Revision 3.13  2001/09/14 21:39:02  genevb
// Adjustments to not depend on order in which maker Clear() is called
//
// Revision 3.12  2001/08/23 13:20:55  genevb
// Many bug workarounds...
//
// Revision 3.11  2001/05/04 20:15:14  genevb
// Common interfaces and reorganization of components, add MC event info
//
// Revision 3.10  2001/04/25 18:20:53  perev
// HPcorrs
//
// Revision 3.9  2001/01/30 04:06:54  genevb
// Better handling of file switches
//
// Revision 3.8  2001/01/04 01:03:23  genevb
// Add CheckFile() for creating sub-dsts
//
// Revision 3.7  2000/12/18 21:35:18  genevb
// Introduced variable buffer-sizing
//
// Revision 3.6  2000/10/27 21:55:31  genevb
// Allow use of event.root files
//
// Revision 3.5  2000/09/28 20:16:05  jones
// Added doT0JitterAbort() optio; added fix to CheckFile in case of no file
//
// Revision 3.4  2000/09/07 02:22:09  genevb
// Added AbortEvent() functionality
//
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
#include "StEvent/StEvent.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StEvent.h"
#include "StStrangeEvMuDst.hh"
#include "StStrangeCuts.hh"
#include "StMcEventTypes.hh"
#include "StParticleDefinition.hh"
#include "StMessMgr.h"
#include "StuJitterBug.hh"

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
  evMcArray = 0;
  cutsArray = 0;
  dstMaker = 0;
  cuts = new StStrangeCuts();
  SetNumber(-2);
  outFileNum = 1;
  doT0JitterAbort = kFALSE;

  TString suffix = "MuDst.root";
  for (Int_t i=0; i<strDstT; i++) {
    doT[i] = kFALSE;
    cont[i] = 0;
    files[i] = 0;
    bsize[i] = 0;
    
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
  abortEvent = kFALSE;

  bsize[evT] = 64000;

}
//_____________________________________________________________________________
StStrangeMuDstMaker::~StStrangeMuDstMaker() {

  if (!dstMaker) {           // Don't delete other maker's array!
    delete evClonesArray; evClonesArray = 0;
    if (evMcArray) { delete evMcArray; evMcArray = 0; }
  }
  delete cutsArray; cutsArray = 0;
  delete cuts; cuts = 0;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::Init() {

  abortEvent = kFALSE;
  firstEvent = kTRUE;
  if (Debug()) gMessMgr->Debug() << "In StStrangeMuDstMaker::Init() ... "
                               << GetName() << endm; 
  
  if (OpenFile() == kStErr) return kStErr;
  if (!dstMaker) {
    evClonesArray = new TClonesArray("StStrangeEvMuDst",1);
    if (doMc) evMcArray = new TClonesArray("StStrangeEvMuDst",1);
  }
  cutsArray = new TClonesArray("TCut",0);
  StStrangeControllerBase::currentMaker = this;
  {EachDoT(cont[i] = StStrangeControllerBase::Instantiate(i));}

  v0 = cont[v0T];
  xi = cont[xiT];
  kink = cont[kinkT];

  {EachDoT( if (bsize[i]) cont[i]->SetBufferSize(bsize[i]) );}

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
  if (!tree) tree = (TTree*) muDst->Get("MuDst");
  if (!tree) {
    gMessMgr->Error() << "StStrangeMuDstMaker: no StrangeMuDst or MuDst tree"
                      << " in file " << file[evT] << endm;
    return;
  }

  // 5/20/02 - GVB
  // Correct for a problem with setting branch file names in old MuDSTs.
  // Setting of branch file names has now been removed (never worked anyhow).
  if (strcmp(tree->GetBranch("Event")->GetFileName(),"")) {
    TObjArray* listOfBranches = tree->GetListOfBranches();
    for (int i=0; i<listOfBranches->GetEntriesFast(); i++)
      ((TBranch*) listOfBranches->At(i))->SetFile(muDst);
  }

  tree->SetBranchStatus("*",0);
  tree->SetBranchStatus("Event.*",1);
  tree->SetBranchAddress("Event",&evClonesArray);
  if (doMc) {
    tree->SetBranchStatus("McEvent.*",1);
    tree->SetBranchAddress("McEvent",&evMcArray);
  }
  EachController(InitReadDst());

  if (tree->GetBranch("StrangeCuts")) {
    tree->SetBranchStatus("StrangeCuts.*",1);
    tree->SetBranchAddress("StrangeCuts",&cutsArray);
  } else {
    TOrdCollection* ordCuts = (TOrdCollection*) muDst->Get("StrangeCuts");
    if (ordCuts) {
      cuts->Reset(ordCuts);
    } else {
      gMessMgr->Warning("StStrangeMuDstMaker: no StrangeCuts");
      cuts->UnknownCuts();
    }
  }
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::InitCreateDst() {
  
  tree = new TTree("StrangeMuDst","Strangeness Micro-DST");
  tree->SetDirectory(muDst);
  EachController(InitCreateDst());

  Int_t split = 99;
  TBranch* branch = tree->Branch("StrangeCuts",&cutsArray,bsize[evT],split);
  if (!dstMaker) {
    branch = tree->Branch("Event",&evClonesArray,bsize[evT],split);
    if (doMc) {
      branch = tree->Branch("McEvent",&evMcArray,bsize[evT],split);
    }
  }
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::InitCreateSubDst() {

  Int_t split=2;
  evClonesArray = dstMaker->GetEvClonesArray();
  TBranch* branch = tree->Branch("Event",&evClonesArray,bsize[evT],split);
  if (doMc) {
    evMcArray = dstMaker->GetEvMcArray();
    branch = tree->Branch("McEvent",&evMcArray,bsize[evT],split);
  }
  EachController(InitCreateSubDst());
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
  while (event_number >= tree_size) {
    if (files[evT]) {                                  // If reading from
      SetStFiles();                                    // multiple files, then
      if (!file[evT]) return kStErr;                   // get the next file
      CloseFile();                                     // names, close the old
      if (OpenFile() == kStErr) return kStErr;         // files, open the new,
      InitReadDst();                                   // and subtract total
      evNumber += tree_size;                           // events from old file
      event_number -= tree_size;                       // from current event #.
      tree_size = (Int_t) tree->GetEntries();
    } else {
      return kStErr;
    }
  }
  if ((tree->GetEvent(event_number)) <= 0) return kStErr;   // Read the event
  EachController(MakeReadDst());
 
  if (cutsArray->GetEntriesFast()) cuts->Reset(cutsArray);

  return kStOK;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::MakeCreateDst() {

  if (abortEvent) return kStOK;

  // Get the cut parameters and fill StrangeCuts on the first event
  if (firstEvent) {
    Int_t iSize = cuts->GetSize();
    cuts->Fill(strTypeNames[v0T], GetDataSet("ev0par2"));
    cuts->Fill(strTypeNames[xiT], GetDataSet("exipar"));
    cuts->Fill(strTypeNames[kinkT], GetDataSet("tkf_tkfpar"));
    Int_t fSize = cuts->GetSize();
    if (!(fSize-iSize)) {
      gMessMgr->Warning("StStrangeMuDstMaker: no cut parameters found.");
      cuts->UnknownCuts();
    }
    firstEvent = kFALSE;
  }  

  // Get event
  StEvent* event = (StEvent*) GetInputDS("StEvent");
  if (!event) return kStOK; 
  if (doT0JitterAbort && t0JitterAbort(event)) {
    gMessMgr->Warning("StStrangeMuDstMaker: T0 jitter; skipping event.");
    return kStWarn;
  }
  if (!(event->primaryVertex())) {
    gMessMgr->Warning("StStrangeMuDstMaker: no primary vertex; skipping event.");
    return kStWarn;
  }

  new((*evClonesArray)[0]) StStrangeEvMuDst(*event);
  EachController(MakeCreateDst(*event));
  if (doMc) MakeCreateMcDst();

  CheckFile();
  cuts->UpdateArray(cutsArray);
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
  new((*evMcArray)[0]) StStrangeEvMuDst(*mcEvent);
  if (!GetMaker("StAssociationMaker")) {
    gMessMgr->Warning() << "StStrangeMuDstMaker: no associated info! \n" 
		      << " assoc branch will not be filled. " << endm;
  }

  //Loop over all MC vertices
  StSPtrVecMcVertex& mcVertices = mcEvent->vertices();
  for (StMcVertexIterator mcVertexIt = mcVertices.begin();
                          mcVertexIt != mcVertices.end(); mcVertexIt++) {
    const StMcTrack* parent = (*mcVertexIt)->parent();
    if (parent) switch (parent->geantId()) {

      // Xi vertex candidates
      case (23) : // Xi-
      case (31) : // AntiXi+
      case (24) : // Omega-
      case (32) : // AntiOmega+
        if (doT[xiT]) cont[xiT]->MakeCreateMcDst(*mcVertexIt);
        break;

      // V0 vertex candidates
      case (10) : // Kaon0Long
      case (16) : // Kaon0Short
      case (18) : // Lambda
      case (26) : // AntiLambda
        // Do all V0's here only if not doing Xi's
        if (doT[v0T] && !(doT[xiT])) cont[v0T]->MakeCreateMcDst(*mcVertexIt);
        break;

      // Kink vertex candidates
      case ( 5) : // Muon+
      case ( 6) : // Muon-
      case ( 8) : // Pion+
      case ( 9) : // Pion-
      case (11) : // Kaon+
      case (12) : // Kaon-
        if (doT[kinkT]) {
          // No need to keep decays not within the TPC
          // (must change if looking for kinks inside inner TPC radius)
          float rad = (*mcVertexIt)->position().perp();
          if ((rad < 195.) && (rad > 65.))
            cont[kinkT]->MakeCreateMcDst(*mcVertexIt);
        }
        break;

      default   : {}
    }
  }//end of loop over MC  vertices

  // If doing Xi's, all Xi V0 daughters are done concurrently with their Xi.
  // Do all non-Xi V0's at the end...
  if (doT[xiT] && doT[v0T]) {
    for (StMcVertexIterator mcVertexIt = mcVertices.begin();
                          mcVertexIt != mcVertices.end(); mcVertexIt++) {
      Bool_t notFromXi = kTRUE;
      const StMcTrack* parent = (*mcVertexIt)->parent();
      if (parent) switch (parent->geantId()) {
        case (18) : // Lambda
        case (26) : // AntiLambda
          { // Check for cascade parentage
            const StMcTrack* parent2 = parent->parent();
            if (parent2) switch (parent2->geantId()) {
              case (23) : // Xi-
              case (31) : // AntiXi+
              case (24) : // Omega-
              case (32) : // AntiOmega+
                notFromXi = kFALSE;
              default   : {}
            }
          }
        case (10) : // Kaon0Long
        case (16) : // Kaon0Short
	  if (notFromXi) cont[v0T]->MakeCreateMcDst(*mcVertexIt);
        default   : {}
      }
    }
  }//end of loop over MC  vertices
  EachController(PrintNumMc());

  return kStOK;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::MakeCreateSubDst() {

  if (abortEvent) return kStOK;

  EachController(MakeCreateSubDst());
  CheckFile();
  cuts->Reset(dstMaker->Cuts());
  cuts->UpdateArray(cutsArray);
  tree->Fill();

  return kStOK;
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::Clear(Option_t *option) {

  if (Debug()) gMessMgr->Debug() << "In StStrangeMuDstMaker::Clear() ... "
                               << GetName() << endm; 

  for (Int_t i=0; i<subMakers.GetEntries(); i++) {
    StStrangeMuDstMaker* subMaker = (StStrangeMuDstMaker*) (subMakers[i]);
    subMaker->ClearForReal(option);
  }
  if (!dstMaker) {
    ClearForReal(option);
  }
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::ClearForReal(Option_t *option) {

  if (Debug()) gMessMgr->Debug() << "In StStrangeMuDstMaker::ClearForReal() ... "
                               << GetName() << endm; 

  if (tree) {
    if (dstMaker) {                                   // Making a subDST
      MakeCreateSubDst();
    } else {
      if (evClonesArray) evClonesArray->Clear();      // Not if making a subDST
      if (evMcArray) evMcArray->Clear();              // Not if making a subDST
    }
    cutsArray->Clear();

    EachController(Clear());
    if (rw == StrangeNoKeep) tree->Reset();
  }

  abortEvent = kFALSE;
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
  char option[16];
  char inout[16];
  if (rw == StrangeRead) {
    sprintf(option,"READ");
    sprintf(inout,"reading");
  } else if (rw == StrangeWrite) {
    sprintf(option,"RECREATE");
    sprintf(inout,"writing");
  } else
    return kStOk;
    
  if( ! (muDst = new TFile(file[evT],option)) ) {
    gMessMgr->Error() << "StStrangeMuDstMaker: Error opening event file:\n  "
                      << file[evT] << endm;
    return kStErr;
  }
  // 4/26/01 - GVB
  // The following line became outdated with switching to Root I/O instead
  // of STAR I/O as of Root version 3.
  //if (rw == StrangeWrite) muDst->SetFormat(1);   // Necessary to read MuDst in plain root
  gMessMgr->Info() << "StStrangeMuDstMaker: Opened event file for " << inout
                   << ":\n  " << file[evT] << endm;     
  cuts->ForceUpdateArray();
  return kStOk;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::CloseFile() {
  if (muDst) {
    if (rw == StrangeWrite) {
      muDst->Write();
      muDst->cd();
      // No longer necessary to store cuts as they are now a branch
      // cuts->Store();
    }
    muDst->Close();
    tree = 0;
  }
  return kStOk;
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::CheckFile() {
  if (muDst) {
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
      if (dstMaker) InitCreateSubDst();
    }
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
//_____________________________________________________________________________
void StStrangeMuDstMaker::SubDst(StStrangeMuDstMaker* maker) {
  dstMaker = maker;
  if (!(maker->subMakers.FindObject((TObject*) this)))
    maker->subMakers.Add(this);
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SetCorrectionFile(char* fname) {
  StStrangeEvMuDst::SetCorrectionFile(fname);
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SetFractionFile(char* fname) {
  StStrangeEvMuDst::SetFractionFile(fname);
}

