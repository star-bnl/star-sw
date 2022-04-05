#include "StChain.h"
#include "StIOMaker/StIOMaker.h"
#include "StEventMaker/StEventMaker.h"
#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"
#include "StStrangeMuDstMaker/StV0MuDst.hh"
#include "StStrangeMuDstMaker/StXiMuDst.hh"
#include "StStrangeMuDstMaker/StKinkMuDst.hh"
#include "StStrangeMuDstMaker/StStrangeCuts.hh"
#include "StStrangeMuDstMaker/StStrangeEvMuDst.hh"
#include "StStrangeMuDstPlayer.h"
#include "StMessMgr.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StAssociationMaker/StAssociationMaker.h"
// #include "StHbtMaker/StRandyTopMapMaker.h" // RWitt: Commented 7/24/15 to remove dependency on StHbtMaker

ClassImp(StrangeMuDstPlayer)

char empty = '\0';
char slash = '/';
void ParseFileName(Char_t *output, Char_t **file, Char_t **dir) {
  if ((*file = strrchr(output,slash))) {
    (*file)++;
    *dir = new char[strlen(output)+5];
    strncpy(*dir,output,(strlen(output)-strlen(*file)));
  } else {
    *file = output;
    *dir = &empty;
  }
}

StrangeMuDstPlayer::StrangeMuDstPlayer() {
  doFileSplit = kTRUE;
  doMC = kFALSE;
  doT0Abort = kTRUE;
  doTopMapFix = kFALSE;
  doReadDST = kFALSE;
  doV0 = kTRUE;
  doXi = kTRUE;
  doKink = kTRUE;

  evPrimTracks = 0;
  evPrimVertexZ = 100.;
  v0DecayLength = 2.0;
  v0DcaDaughtersToPrimVertex = 0.7;
  v0DcaToPrimVertex = 0.8;
  v0DcaDaughters = 0.75;
  v0NumTpcHits = 15;
  xiDecayLength = 0.;
  xiDcaDaughters = 0.7;
  xiDcaV0Daughters = 0.7;
  xiDcaToPrimVertex = 0.7;
  xiDcaV0ToPrimVertex = 0.;
  xiDcaBachelorToPrimVertex = 0.;
}

void StrangeMuDstPlayer::Make(Int_t NEvents, StFile* input, Char_t* output) {

  StStrangeMuDstMaker *muDstMakers[3];
  StStrangeMuDstMaker *v0MuDstMaker, *xiMuDstMaker, *kinkMuDstMaker;
  StIOMaker *IOMaker;
  StEventMaker *eventMaker;
  StMcEventMaker *mcEventReader;
  StAssociationMaker *associator;
//   StRandyTopMapMaker *topoMapFixer;  // RWitt: Commented 7/24/15 to remove dependency on StHbtMaker
  Char_t *file, *dir, *outfile[3], line[80];
  TString prefix[3];
  Int_t mNDstMakers = 0;

  // Create a chain
  StChain chain("myChain");

  // Create Makers
  IOMaker = new StIOMaker("IO","r",input,"bfcTree");
  // Indicate input branches
  IOMaker->SetBranch("*",0,"0");             //deactivate all branches
  IOMaker->SetBranch("runcoBranch",0,"r");   //activate runco Branch
  if( doReadDST ) {
    IOMaker->SetBranch("dstBranch",0,"r");   //activate dst Branch
    eventMaker = new StEventMaker("events","title");
  } else
    IOMaker->SetBranch("eventBranch",0,"r"); //activate event Branch
  // The following are needed for using Monte Carlo info
  if( doMC ) {
    IOMaker->SetBranch("geantBranch",0,"r"); //activate geant Branch
    mcEventReader = new StMcEventMaker; 
    associator    = new StAssociationMaker;
  }
  // RWitt: Commented 7/24/15 to remove dependency on StHbtMaker
//   // The following is needed for MDC3 files with incorrect topology maps
//   if( doTopMapFix ) 
//     topoMapFixer = new StRandyTopMapMaker();
  
  if( doFileSplit ) {
    if( doV0 ) {
      v0MuDstMaker = new StStrangeMuDstMaker("v0MuDstMaker");
      v0MuDstMaker->DoV0();      // Selects V0 vertices for new micro-DST
      muDstMakers[mNDstMakers] = v0MuDstMaker;
      prefix[mNDstMakers] = "v0_";
      mNDstMakers++;
    }
    if( doXi ) {
      xiMuDstMaker = new StStrangeMuDstMaker("xiMuDstMaker");
      xiMuDstMaker->DoXi();      // Selects Xi vertices for new micro-DST
      muDstMakers[mNDstMakers] = xiMuDstMaker;
      prefix[mNDstMakers] = "xi_";
      mNDstMakers++;
    }
    if( doKink ) {
      kinkMuDstMaker = new StStrangeMuDstMaker("kinkMuDstMaker");
      kinkMuDstMaker->DoKink();  // Selects Kink vertices for new micro-DST
      muDstMakers[mNDstMakers] = kinkMuDstMaker;
      prefix[mNDstMakers] = "kink_";
      mNDstMakers++;
    }
    ParseFileName(output, &file, &dir);
    for(Int_t i=0; i<mNDstMakers; i++) {
      //      outfile[i] = strdup(dir);    // doesn't work - too short ?
      outfile[i] = new char[strlen(output)+5];
      strcpy(outfile[i],dir);
      strcat(outfile[i],prefix[i].Data());
      strcat(outfile[i],file);
      muDstMakers[i]->SetWrite(outfile[i]);
      if( doT0Abort ) 
	muDstMakers[i]->DoT0JitterAbort();
      if( doMC )
	muDstMakers[i]->DoMc();   // Keep MC info if it is available
    }
  } else {
    mNDstMakers = 1;
    muDstMakers[0] = new StStrangeMuDstMaker("muDstMaker");
    if( doV0 ) muDstMakers[0]->DoV0();    // Selects V0sfor new micro-DST
    if( doXi ) muDstMakers[0]->DoXi();    // Selects Xis for new micro-DST
    if( doKink ) muDstMakers[0]->DoKink();  // Selects Kinks for new micro-DST
    muDstMakers[0]->SetWrite(output);
    if( doT0Abort ) 
      muDstMakers[0]->DoT0JitterAbort();
    if( doMC ) 
      muDstMakers[0]->DoMc();  // Keep MC info if it is available
  }
  
  // Do init
  Int_t istatus = chain.Init();
  if( istatus ) { chain.FatalErr(istatus,"on init"); return; }

  // Loop over events
  for( Int_t i=0; i<NEvents; i++ ) {
    switch (istatus = chain.Make()) {
      case 0: break;
      case 2: { gMessMgr->Info("Last event from input."); break; }
      case 3: { gMessMgr->Error() << "Event " << i << " had error " <<
        istatus << ". Continuing."; gMessMgr->Print(); break; }
      default: { gMessMgr->Warning() << "Event " << i << " returned status " <<
        istatus << ". Continuing."; gMessMgr->Print(); }
    }

    if( istatus == 2 ) break;
    
    if( i != NEvents) chain.Clear();
    sprintf(line,"*** Finished processing event %d",i);
    gMessMgr->Info(line);
  }
  
  // Finish
  if( NEvents >= 1 ) {
    chain.Finish();
  }
}

void StrangeMuDstPlayer::Filter(Int_t NEvents, StFile* input, Char_t* output) {

  StStrangeMuDstMaker *oldMuDstMaker=0;
  StStrangeMuDstMaker *newMuDstMakers[3];
  StStrangeMuDstMaker *v0MuDstMaker=0;
  StStrangeMuDstMaker *xiMuDstMaker=0;
  StStrangeMuDstMaker *kinkMuDstMaker=0;
  Char_t *file, *dir, *outfile[3], line[80];
  TString prefix[3];
  Int_t mNDstMakers = 0;

  // Create a chain
  StChain chain("myChain");

  // Create Makers

  // The maker for the new micro DST must be constructed _before_ the 
  // maker to read the old micro DST. This is because the copying is
  // done during chain.Clear(), and the new maker's Clear() must be
  // called to do the copying before the old maker's Clear() is called,
  // erasing the event.
  
  if( doFileSplit ) {
    if( doV0 ) {
      v0MuDstMaker = new StStrangeMuDstMaker("v0MuDstMaker");
      v0MuDstMaker->DoV0();      // Selects V0 vertices for new micro-DST
      newMuDstMakers[mNDstMakers] = v0MuDstMaker;
      prefix[mNDstMakers] = "v0_";
      mNDstMakers++;
    }
    if( doXi ) {
      xiMuDstMaker = new StStrangeMuDstMaker("xiMuDstMaker");
      xiMuDstMaker->DoXi();      // Selects Xi vertices for new micro-DST
      newMuDstMakers[mNDstMakers] = xiMuDstMaker;
      prefix[mNDstMakers] = "xi_";
      mNDstMakers++;
    }
    if( doKink ) {
      kinkMuDstMaker = new StStrangeMuDstMaker("kinkMuDstMaker");
      kinkMuDstMaker->DoKink();  // Selects Kink vertices for new micro-DST
      newMuDstMakers[mNDstMakers] = kinkMuDstMaker;
      prefix[mNDstMakers] = "kink_";
      mNDstMakers++;
    }
    ParseFileName(output, &file, &dir);
    for(Int_t i=0; i<mNDstMakers; i++) {
      //      outfile[i] = strdup(dir);    // doesn't work - too short ?
      outfile[i] = new char[strlen(output)+5];
      strcpy(outfile[i],dir);
      strcat(outfile[i],prefix[i].Data());
      strcat(outfile[i],file);
      newMuDstMakers[i]->SetWrite(outfile[i]);
      if( doT0Abort ) 
	newMuDstMakers[i]->DoT0JitterAbort();
      if( doMC )
	newMuDstMakers[i]->DoMc();   // Keep MC info if it is available
    }
  } else {
    mNDstMakers = 1;
    newMuDstMakers[0] = new StStrangeMuDstMaker("newMuDstMaker");
    if( doV0 ) newMuDstMakers[0]->DoV0();    // Selects V0s for new micro-DST
    if( doXi ) newMuDstMakers[0]->DoXi();    // Selects Xis for new micro-DST
    if( doKink ) newMuDstMakers[0]->DoKink();// Selects Kinks for new micro-DST
    newMuDstMakers[0]->SetWrite(output);
    if( doT0Abort ) 
      newMuDstMakers[0]->DoT0JitterAbort();
    if( doMC ) 
      newMuDstMakers[0]->DoMc();  // Keep MC info if it is available
    // Duplicate pointer to maker: access by name or generically
    v0MuDstMaker = newMuDstMakers[0];
    xiMuDstMaker = newMuDstMakers[0];
    kinkMuDstMaker = newMuDstMakers[0];
  }
  
  oldMuDstMaker = new StStrangeMuDstMaker("oldMuDstMaker");
  oldMuDstMaker->SetRead(input); // Selects READ mode
  // DoV0() and DoMc() are automatically called for the old maker by the new.
  {for( Int_t i=0; i<mNDstMakers; i++ )
    newMuDstMakers[i]->SubDst(oldMuDstMaker);}
  // Next, any additional cuts that are being made should be added to
  // the cuts information in the new DST.
  char buff[80];
  // Event-wise cuts go here
  {for( Int_t i=0; i<mNDstMakers; i++ ) {
    sprintf(buff,"< +/- %f",evPrimVertexZ);
    newMuDstMakers[i]->Cuts().Add("evPrimVertexZ",buff);
    sprintf(buff,"> %d",evPrimTracks);
    newMuDstMakers[i]->Cuts().Add("evPrimTracks",buff);
  }}
  // Specific cuts follow
  if( doV0 ) {
    sprintf(buff,"> %f",v0DecayLength);
    v0MuDstMaker->Cuts().Add("v0DecayLength",buff);
    sprintf(buff,"> %f",v0DcaDaughtersToPrimVertex);
    v0MuDstMaker->Cuts().Add("dcaPosToPrimVertex || dcaNegToPrimVertex",buff);
    sprintf(buff,"< %f",v0DcaToPrimVertex);
    v0MuDstMaker->Cuts().Add("v0DcaToPrimVertex",buff);
    sprintf(buff,"< %f",v0DcaDaughters);
    v0MuDstMaker->Cuts().Add("v0DcaDaughters",buff);
    sprintf(buff,"> %f",v0NumTpcHits);
    v0MuDstMaker->Cuts().Add("v0NumTpcHits",buff);
  }
  if( doXi ) {
    sprintf(buff,"> %f",xiDecayLength);
    xiMuDstMaker->Cuts().Add("xiDecayLength",buff);
    sprintf(buff,"< %f",xiDcaDaughters);
    xiMuDstMaker->Cuts().Add("xiDcaDaughters",buff);
    sprintf(buff,"< %f",xiDcaV0Daughters);
    xiMuDstMaker->Cuts().Add("xiDcaV0Daughters",buff);
    sprintf(buff,"< %f",xiDcaToPrimVertex);
    xiMuDstMaker->Cuts().Add("xiDcaToPrimVertex",buff);
    sprintf(buff,"> %f",xiDcaV0ToPrimVertex);
    xiMuDstMaker->Cuts().Add("xiDcaV0ToPrimVertex",buff);
    sprintf(buff,"> %f",xiDcaBachelorToPrimVertex);
    xiMuDstMaker->Cuts().Add("xiDcaBachelorToPrimVertex",buff);
  }  

  gMessMgr->Info() << "evPrimVertexZ < " << evPrimVertexZ << endm;
  gMessMgr->Info() << "evPrimTracks > " << evPrimTracks << endm;
  gMessMgr->Info() << "v0DecayLength > " << v0DecayLength << endm;
  gMessMgr->Info() << "v0DcaToPrimVertex < " << v0DcaToPrimVertex << endm;
  gMessMgr->Info() << "v0DcaDaughtersToPrimVertex > " << 
    v0DcaDaughtersToPrimVertex << endm;
  gMessMgr->Info() << "v0DcaDaughters < " << v0DcaDaughters << endm;
  gMessMgr->Info() << "v0NumTpcHits > " << v0NumTpcHits << endm;
  gMessMgr->Info() << "xiDecayLength > " << xiDecayLength << endm;
  gMessMgr->Info() << "xiDcaDaughters < " << xiDcaDaughters << endm;
  gMessMgr->Info() << "xiDcaV0Daughters < " << xiDcaV0Daughters << endm;
  gMessMgr->Info() << "xiDcaToPrimVertex < " << xiDcaToPrimVertex << endm;
  gMessMgr->Info() << "xiDcaV0ToPrimVertex > " << xiDcaV0ToPrimVertex << endm;
  gMessMgr->Info() << "xiDcaBachelorToPrimVertex > " << 
    xiDcaBachelorToPrimVertex << endm;

  // Do init
  Int_t istatus = chain.Init();
  if( istatus ) { chain.FatalErr(istatus,"on init"); return; }

  // Loop over events
  for( Int_t i=0; i<NEvents; i++ ) {
    switch (istatus = chain.Make(i)) {
      case 0: break;
      case 2: { gMessMgr->Info("Last event from input."); break; }
      case 3: { gMessMgr->Error() << "Event " << i << " had error " <<
        istatus << ". Ending."; gMessMgr->Print(); break; }
      default: { gMessMgr->Warning() << "Event " << i << " returned status " <<
        istatus << ". Ending."; gMessMgr->Print(); }
    }

    if( istatus ) break;

    Float_t primZ = oldMuDstMaker->GetEvent()->primaryVertexZ();
    unsigned int primaryTracks = oldMuDstMaker->GetEvent()->primaryTracks();
    // Apply cut on the primary vertex Z position
    if (TMath::Abs(primZ)<evPrimVertexZ && primaryTracks>evPrimTracks) {
      if( doV0 )
	// Apply selection criteria to the V0s
	for( Int_t j=0; j<oldMuDstMaker->GetNV0(); j++ ) {
	  StV0MuDst *v0j = oldMuDstMaker->GetV0(j);
	  if( v0j->decayLengthV0() > v0DecayLength && 
	      v0j->dcaV0ToPrimVertex() < v0DcaToPrimVertex &&
	      ((v0j->dcaPosToPrimVertex() > v0DcaDaughtersToPrimVertex) ||
	       (v0j->dcaNegToPrimVertex() > v0DcaDaughtersToPrimVertex)) &&
	      v0j->dcaV0Daughters() < v0DcaDaughters &&
	      v0j->topologyMapPos().numberOfHits(kTpcId) > v0NumTpcHits &&
	      v0j->topologyMapNeg().numberOfHits(kTpcId) > v0NumTpcHits )
	    v0MuDstMaker->SelectV0(j);
	}
      if( doXi ) 
	// Apply selection criteria to the Xis
	for( Int_t j=0; j<oldMuDstMaker->GetNXi(); j++ ) {
	  StXiMuDst *xij = oldMuDstMaker->GetXi(j);
	  if( xij->decayLengthXi() > xiDecayLength &&
	      xij->dcaXiDaughters() < xiDcaDaughters &&
	      xij->dcaV0Daughters() < xiDcaV0Daughters &&
	      xij->dcaXiToPrimVertex() < xiDcaToPrimVertex &&
	      xij->dcaV0ToPrimVertex() > xiDcaV0ToPrimVertex &&
	      xij->dcaBachelorToPrimVertex() > xiDcaBachelorToPrimVertex )
	    xiMuDstMaker->SelectXi(j);
	}
      if( doKink ) 
	// Just copy the kinks for now
	for( Int_t j=0; j<oldMuDstMaker->GetNKink(); j++ ) {
	  kinkMuDstMaker->SelectKink(j);
	}
    } else {
      {for( Int_t j=0; j<mNDstMakers; j++ )
	newMuDstMakers[j]->AbortEvent();}
    } // prim vertex Z and prim Tracks cut
  
    if( i != NEvents) chain.Clear();
    sprintf(line,"*** Finished processing event %d",i);
    gMessMgr->Info(line);
  }
  
  // Finish
  if( NEvents >= 1 ) {
    chain.Finish();
  }
}

void StrangeMuDstPlayer::Play(Int_t NEvents, StFile* input, Char_t* output) {

  StStrangeMuDstMaker *oldMuDstMaker=0;
  StStrangeMuDstMaker *newMuDstMakers[3];
  StStrangeMuDstMaker *v0MuDstMaker=0;
  StStrangeMuDstMaker *xiMuDstMaker=0;
  StStrangeMuDstMaker *kinkMuDstMaker=0;
  StIOMaker *IOMaker;
  StEventMaker *eventMaker;
  StMcEventMaker *mcEventReader;
  StAssociationMaker *associator;
  //   StRandyTopMapMaker *topoMapFixer; // RWitt: Commented 7/24/15 to remove dependency on StHbtMaker
  Char_t *file, *dir, *outfile[3], line[80];
  TString prefix[3];
  Int_t mNDstMakers = 0;

  // Create a chain
  StChain chain("myChain");

  // Create Makers
  IOMaker = new StIOMaker("IO","r",input,"bfcTree");
  // Indicate input branches
  IOMaker->SetBranch("*",0,"0");             //deactivate all branches
  IOMaker->SetBranch("runcoBranch",0,"r");   //activate runco Branch
  if( doReadDST ) {
    IOMaker->SetBranch("dstBranch",0,"r");   //activate dst Branch
    eventMaker = new StEventMaker("events","title");
  } else
    IOMaker->SetBranch("eventBranch",0,"r"); //activate event Branch
  // The following are needed for using Monte Carlo info
  if( doMC ) {
    IOMaker->SetBranch("geantBranch",0,"r"); //activate geant Branch
    mcEventReader = new StMcEventMaker; 
    associator    = new StAssociationMaker;
  }
  // RWitt: Commented 7/24/15 to remove dependency on StHbtMaker
  //   // The following is needed for MDC3 files with incorrect topology maps
  //   if( doTopMapFix ) 
  //     topoMapFixer = new StRandyTopMapMaker();

  // The maker for the new micro DST must be constructed _before_ the 
  // maker to read the old micro DST. This is because the copying is
  // done during chain.Clear(), and the new maker's Clear() must be
  // called to do the copying before the old maker's Clear() is called,
  // erasing the event.
  
  if( doFileSplit ) {
    if( doV0 ) {
      v0MuDstMaker = new StStrangeMuDstMaker("v0MuDstMaker");
      v0MuDstMaker->DoV0();      // Selects V0 vertices for new micro-DST
      newMuDstMakers[mNDstMakers] = v0MuDstMaker;
      prefix[mNDstMakers] = "v0_";
      mNDstMakers++;
    }
    if( doXi ) {
      xiMuDstMaker = new StStrangeMuDstMaker("xiMuDstMaker");
      xiMuDstMaker->DoXi();      // Selects Xi vertices for new micro-DST
      newMuDstMakers[mNDstMakers] = xiMuDstMaker;
      prefix[mNDstMakers] = "xi_";
      mNDstMakers++;
    }
    if( doKink ) {
      kinkMuDstMaker = new StStrangeMuDstMaker("kinkMuDstMaker");
      kinkMuDstMaker->DoKink();  // Selects Kink vertices for new micro-DST
      newMuDstMakers[mNDstMakers] = kinkMuDstMaker;
      prefix[mNDstMakers] = "kink_";
      mNDstMakers++;
    }
    ParseFileName(output, &file, &dir);
    for(Int_t i=0; i<mNDstMakers; i++) {
      //      outfile[i] = strdup(dir);    // doesn't work - too short ?
      outfile[i] = new char[strlen(output)+5];
      strcpy(outfile[i],dir);
      strcat(outfile[i],prefix[i].Data());
      strcat(outfile[i],file);
      newMuDstMakers[i]->SetWrite(outfile[i]);
      if( doT0Abort ) 
	newMuDstMakers[i]->DoT0JitterAbort();
      if( doMC )
	newMuDstMakers[i]->DoMc();   // Keep MC info if it is available
    }
  } else {
    mNDstMakers = 1;
    newMuDstMakers[0] = new StStrangeMuDstMaker("newMuDstMaker");
    if( doV0 ) newMuDstMakers[0]->DoV0();    // Selects V0s for new micro-DST
    if( doXi ) newMuDstMakers[0]->DoXi();    // Selects Xis for new micro-DST
    if( doKink ) newMuDstMakers[0]->DoKink();// Selects Kinks for new micro-DST
    newMuDstMakers[0]->SetWrite(output);
    if( doT0Abort ) 
      newMuDstMakers[0]->DoT0JitterAbort();
    if( doMC ) 
      newMuDstMakers[0]->DoMc();  // Keep MC info if it is available
    // Duplicate pointer to maker: access by name or generically
    v0MuDstMaker = newMuDstMakers[0];
    xiMuDstMaker = newMuDstMakers[0];
    kinkMuDstMaker = newMuDstMakers[0];
  }
  
  oldMuDstMaker = new StStrangeMuDstMaker("oldMuDstMaker");
  oldMuDstMaker->SetNoKeep();  // Reset tree after every event
  if( doT0Abort ) 
    oldMuDstMaker->DoT0JitterAbort();
  // DoV0() and DoMc() are automatically called for the old maker by the new.
  {for( Int_t i=0; i<mNDstMakers; i++ )
    newMuDstMakers[i]->SubDst(oldMuDstMaker);}
  // Next, any additional cuts that are being made should be added to
  // the cuts information in the new DST.
  char buff[80];
  // Event-wise cuts go here
  {for( Int_t i=0; i<mNDstMakers; i++ ) {
    sprintf(buff,"< +/- %f",evPrimVertexZ);
    newMuDstMakers[i]->Cuts().Add("evPrimVertexZ",buff);
    sprintf(buff,"> %d",evPrimTracks);
    newMuDstMakers[i]->Cuts().Add("evPrimTracks",buff);
  }}
  // Specific cuts follow
  if( doV0 ) {
    sprintf(buff,"> %f",v0DecayLength);
    v0MuDstMaker->Cuts().Add("v0DecayLength",buff);
    sprintf(buff,"> %f",v0DcaDaughtersToPrimVertex);
    v0MuDstMaker->Cuts().Add("dcaPosToPrimVertex || dcaNegToPrimVertex",buff);
    sprintf(buff,"< %f",v0DcaToPrimVertex);
    v0MuDstMaker->Cuts().Add("v0DcaToPrimVertex",buff);
    sprintf(buff,"< %f",v0DcaDaughters);
    v0MuDstMaker->Cuts().Add("v0DcaDaughters",buff);
    sprintf(buff,"> %f",v0NumTpcHits);
    v0MuDstMaker->Cuts().Add("v0NumTpcHits",buff);
  }
  if( doXi ) {
    sprintf(buff,"> %f",xiDecayLength);
    xiMuDstMaker->Cuts().Add("xiDecayLength",buff);
    sprintf(buff,"< %f",xiDcaDaughters);
    xiMuDstMaker->Cuts().Add("xiDcaDaughters",buff);
    sprintf(buff,"< %f",xiDcaV0Daughters);
    xiMuDstMaker->Cuts().Add("xiDcaV0Daughters",buff);
    sprintf(buff,"< %f",xiDcaToPrimVertex);
    xiMuDstMaker->Cuts().Add("xiDcaToPrimVertex",buff);
    sprintf(buff,"> %f",xiDcaV0ToPrimVertex);
    xiMuDstMaker->Cuts().Add("xiDcaV0ToPrimVertex",buff);
    sprintf(buff,"> %f",xiDcaBachelorToPrimVertex);
    xiMuDstMaker->Cuts().Add("xiDcaBachelorToPrimVertex",buff);
  }  

  gMessMgr->Info() << "evPrimVertexZ < " << evPrimVertexZ << endm;
  gMessMgr->Info() << "evPrimTracks > " << evPrimTracks << endm;
  gMessMgr->Info() << "v0DecayLength > " << v0DecayLength << endm;
  gMessMgr->Info() << "v0DcaToPrimVertex < " << v0DcaToPrimVertex << endm;
  gMessMgr->Info() << "v0DcaDaughtersToPrimVertex > " << 
    v0DcaDaughtersToPrimVertex << endm;
  gMessMgr->Info() << "v0DcaDaughters < " << v0DcaDaughters << endm;
  gMessMgr->Info() << "v0NumTpcHits > " << v0NumTpcHits << endm;
  gMessMgr->Info() << "xiDecayLength > " << xiDecayLength << endm;
  gMessMgr->Info() << "xiDcaDaughters < " << xiDcaDaughters << endm;
  gMessMgr->Info() << "xiDcaV0Daughters < " << xiDcaV0Daughters << endm;
  gMessMgr->Info() << "xiDcaToPrimVertex < " << xiDcaToPrimVertex << endm;
  gMessMgr->Info() << "xiDcaV0ToPrimVertex > " << xiDcaV0ToPrimVertex << endm;
  gMessMgr->Info() << "xiDcaBachelorToPrimVertex > " << 
    xiDcaBachelorToPrimVertex << endm;

  // Do init
  Int_t istatus = chain.Init();
  if( istatus ) { chain.FatalErr(istatus,"on init"); return; }

  // Loop over events
  for( Int_t i=0; i<NEvents; i++ ) {
    switch (istatus = chain.Make()) {
      case 0: break;
      case 2: { gMessMgr->Info("Last event from input."); break; }
      case 3: { gMessMgr->Error() << "Event " << i << " had error " <<
        istatus << ". Continuing."; gMessMgr->Print(); break; }
      default: { gMessMgr->Warning() << "Event " << i << " returned status " <<
        istatus << ". Continuing."; gMessMgr->Print(); }
    }

    if( istatus == 2 ) break;

    Float_t primZ = oldMuDstMaker->GetEvent()->primaryVertexZ();
    unsigned int primaryTracks = oldMuDstMaker->GetEvent()->primaryTracks();
    // Apply cut on the primary vertex Z position
    if (TMath::Abs(primZ)<evPrimVertexZ && primaryTracks>evPrimTracks) {
      if( doV0 )
	// Apply selection criteria to the V0s
	for( Int_t j=0; j<oldMuDstMaker->GetNV0(); j++ ) {
	  StV0MuDst *v0j = oldMuDstMaker->GetV0(j);
	  if( v0j->decayLengthV0() > v0DecayLength && 
	      v0j->dcaV0ToPrimVertex() < v0DcaToPrimVertex &&
	      ((v0j->dcaPosToPrimVertex() > v0DcaDaughtersToPrimVertex) ||
	       (v0j->dcaNegToPrimVertex() > v0DcaDaughtersToPrimVertex)) &&
	      v0j->dcaV0Daughters() < v0DcaDaughters &&
	      v0j->topologyMapPos().numberOfHits(kTpcId) > v0NumTpcHits &&
	      v0j->topologyMapNeg().numberOfHits(kTpcId) > v0NumTpcHits )
	    v0MuDstMaker->SelectV0(j);
	}
      if( doXi )
	// Apply selection criteria to the Xis
	for( Int_t j=0; j<oldMuDstMaker->GetNXi(); j++ ) {
	  StXiMuDst *xij = oldMuDstMaker->GetXi(j);
	  if( xij->decayLengthXi() > xiDecayLength &&
	      xij->dcaXiDaughters() < xiDcaDaughters &&
	      xij->dcaV0Daughters() < xiDcaV0Daughters &&
	      xij->dcaXiToPrimVertex() < xiDcaToPrimVertex &&
	      xij->dcaV0ToPrimVertex() > xiDcaV0ToPrimVertex &&
	      xij->dcaBachelorToPrimVertex() > xiDcaBachelorToPrimVertex )
	    xiMuDstMaker->SelectXi(j);
	}
      if( doKink ) 
	// Just copy the kinks for now
	for( Int_t j=0; j<oldMuDstMaker->GetNKink(); j++ ) {
	  kinkMuDstMaker->SelectKink(j);
	}
    } else {
      {for( Int_t j=0; j<mNDstMakers; j++ )
	newMuDstMakers[j]->AbortEvent();}
    } // prim vertex Z and prim Tracks cut
  
    if( i != NEvents) chain.Clear();
    sprintf(line,"*** Finished processing event %d",i);
    gMessMgr->Info(line);
  }
  
  // Finish
  if( NEvents >= 1 ) {
    chain.Finish();
  }
}

void StrangeMuDstPlayer::Copy(Int_t NEvents, StFile* input, Char_t* output) {

  StStrangeMuDstMaker *oldMuDstMaker=0;
  StStrangeMuDstMaker *newMuDstMakers[3];
  StStrangeMuDstMaker *v0MuDstMaker=0;
  StStrangeMuDstMaker *xiMuDstMaker=0;
  StStrangeMuDstMaker *kinkMuDstMaker=0;
  Char_t *file, *dir, *outfile[3], line[80];
  TString prefix[3];
  Int_t mNDstMakers = 0;

  // Create a chain
  StChain chain("myChain");

  // Create Makers

  // The maker for the new micro DST must be constructed _before_ the 
  // maker to read the old micro DST. This is because the copying is
  // done during chain.Clear(), and the new maker's Clear() must be
  // called to do the copying before the old maker's Clear() is called,
  // erasing the event.
  
  if( doFileSplit ) {
    if( doV0 ) {
      v0MuDstMaker = new StStrangeMuDstMaker("v0MuDstMaker");
      v0MuDstMaker->DoV0();      // Selects V0 vertices for new micro-DST
      newMuDstMakers[mNDstMakers] = v0MuDstMaker;
      prefix[mNDstMakers] = "v0_";
      mNDstMakers++;
    }
    if( doXi ) {
      xiMuDstMaker = new StStrangeMuDstMaker("xiMuDstMaker");
      xiMuDstMaker->DoXi();      // Selects Xi vertices for new micro-DST
      newMuDstMakers[mNDstMakers] = xiMuDstMaker;
      prefix[mNDstMakers] = "xi_";
      mNDstMakers++;
    }
    if( doKink ) {
      kinkMuDstMaker = new StStrangeMuDstMaker("kinkMuDstMaker");
      kinkMuDstMaker->DoKink();  // Selects Kink vertices for new micro-DST
      newMuDstMakers[mNDstMakers] = kinkMuDstMaker;
      prefix[mNDstMakers] = "kink_";
      mNDstMakers++;
    }
    ParseFileName(output, &file, &dir);
    for(Int_t i=0; i<mNDstMakers; i++) {
      //      outfile[i] = strdup(dir);    // doesn't work - too short ?
      outfile[i] = new char[strlen(output)+5];
      strcpy(outfile[i],dir);
      strcat(outfile[i],prefix[i].Data());
      strcat(outfile[i],file);
      newMuDstMakers[i]->SetWrite(outfile[i]);
      if( doT0Abort ) 
	newMuDstMakers[i]->DoT0JitterAbort();
      if( doMC )
	newMuDstMakers[i]->DoMc();   // Keep MC info if it is available
    }
  } else {
    mNDstMakers = 1;
    newMuDstMakers[0] = new StStrangeMuDstMaker("newMuDstMaker");
    if( doV0 ) newMuDstMakers[0]->DoV0();    // Selects V0s for new micro-DST
    if( doXi ) newMuDstMakers[0]->DoXi();    // Selects Xis for new micro-DST
    if( doKink ) newMuDstMakers[0]->DoKink();// Selects Kinks for new micro-DST
    newMuDstMakers[0]->SetWrite(output);
    if( doT0Abort ) 
      newMuDstMakers[0]->DoT0JitterAbort();
    if( doMC ) 
      newMuDstMakers[0]->DoMc();  // Keep MC info if it is available
    // Duplicate pointer to maker: access by name or generically
    v0MuDstMaker = newMuDstMakers[0];
    xiMuDstMaker = newMuDstMakers[0];
    kinkMuDstMaker = newMuDstMakers[0];
  }
  
  oldMuDstMaker = new StStrangeMuDstMaker("oldMuDstMaker");
  oldMuDstMaker->SetRead(input); // Selects READ mode
  // DoV0() and DoMc() are automatically called for the old maker by the new.
  {for( Int_t i=0; i<mNDstMakers; i++ )
    newMuDstMakers[i]->SubDst(oldMuDstMaker);}

  // Do init
  Int_t istatus = chain.Init();
  if( istatus ) { chain.FatalErr(istatus,"on init"); return; }

  // Loop over events
  for( Int_t i=0; i<NEvents; i++ ) {
    switch (istatus = chain.Make(i)) {
      case 0: break;
      case 2: { gMessMgr->Info("Last event from input."); break; }
      case 3: { gMessMgr->Error() << "Event " << i << " had error " <<
        istatus << ". Ending."; gMessMgr->Print(); break; }
      default: { gMessMgr->Warning() << "Event " << i << " returned status " <<
        istatus << ". Ending."; gMessMgr->Print(); }
    }

    if( istatus ) break;

    if( doV0 )
      // Copy the V0s
      for( Int_t j=0; j<oldMuDstMaker->GetNV0(); j++ )
	v0MuDstMaker->SelectV0(j);
    if( doXi ) 
      // Copy the Xis
      for( Int_t j=0; j<oldMuDstMaker->GetNXi(); j++ )
	xiMuDstMaker->SelectXi(j);
    if( doKink ) 
      // Copy the kinks
      for( Int_t j=0; j<oldMuDstMaker->GetNKink(); j++ )
	kinkMuDstMaker->SelectKink(j);
    
    if( i != NEvents) chain.Clear();
    sprintf(line,"*** Finished processing event %d",i);
    gMessMgr->Info(line);
  }
  
  // Finish
  if( NEvents >= 1 ) {
    chain.Finish();
  }
}

