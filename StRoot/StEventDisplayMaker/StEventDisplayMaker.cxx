//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
// $Id: StEventDisplayMaker.cxx,v 1.40 1999/11/24 14:57:52 fine Exp $

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventDisplayMaker class for Makers                                 //
//  To diplay the StEvent/StGlobalTrack object with the GEANT geometry  //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html   //
//                                                                      //
//             How to create the custom filter                          //
//                                                                      //
// 1. Choose name for your filter                                       //
// 2. Copy $STAR/StRoot/StEvent/St_TLA_EventFiler.cxx                   //
//         $STAR/StRoot/StEvent/St_TLA_EventFiler.h                     //
// 3. Replace all "_TLA_" with the name of the filter you've chosen     //
// 4. Edit St<your_name>EventFiler.cxx to introduce your own version of //
//    St<your_name>EventFiler::Filter methods                           //
// 5. Create the "regular" STAR share library for yor filter class      //
// 6. Create/Edit "chain" macro  (see drawEvent.C )                     //
//    6.1. Load in there your filter share library                      //
//    6.2. Create in there your filter object('s)                       //
//    6.2. Link your filter with the element of StEvent you want        //
//         your filter affect:                                          //
//                                                                      //
//         disp      = new StEventDisplayMaker;                         //
//         St_TLA_EventFilter *trackFilter = new St_TLA_EventFilter();  //
//         disp->SetFilter(trackFilter,StEventDisplayMaker::kTrack);    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TROOT.h"
#include "TCanvas.h"
#include "TColor.h"
#include "TStyle.h"
#include "TTUBS.h"

#include "StEventDisplayMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_Node.h"
#include "St_NodeView.h"
#include "St_NodePosition.h"
#include "St_PolyLine3D.h"
#include "St_Points3D.h"
#include "StHits3DPoints.h"
#include "StVertices3DPoints.h"
#include "StHelix3DPoints.h"
#include "St_Table3Points.h"
#include "StVirtualEventFilter.h"
#include "St_Table.h"
#include "St_TableSorter.h"
#include "StArray.h"
#include "tables/St_tpt_track_Table.h"

#include "StThreeVector.hh"
#include "StHelixD.hh"

// #include "StEvent.h"
#include <TCanvas.h>
#include <TGeometry.h>
#include <TWebFile.h>
#include <TBRIK.h>
#include <TH2.h>
#include <TMath.h>
#include <TObjString.h>

// StVirtualEventFilter hitsOffFilter(0);
// StVirtualEventFilter hitsOnFilter(1);
StVirtualEventFilter StEventDisplayMaker::m_DefaultFilters[StEventDisplayMaker::kEndOfEventList];

//_____________________________________________________________________________
//
//                         StEventDisplayMaker
//_____________________________________________________________________________

ClassImp(StEventDisplayMaker)

//_____________________________________________________________________________
StEventDisplayMaker::StEventDisplayMaker(const char *name):StMaker(name)
{
  m_Hall          =  0;  
  m_FullView      =  0;  
  m_ShortView     =  0; 
  m_Sensible      =  0; 
  m_EventsNode    =  0;

  m_HitCollector  = new TList;
  m_TrackCollector= new TList;
  m_TableCollector= new TList;

  m_PadBrowserCanvas = 0;

  m_ListDataSetNames = 0;

  m_FilterArray   = new TObjArray(kEndOfEventList);
  Int_t i; 
  for (i =0;i<kEndOfEventList;i++) {
   m_FilterArray->AddAt(&m_DefaultFilters[i],i);
//    m_FilterArray->AddAt(&hitsOffFilter,i);
  }
  ((StVirtualEventFilter *)m_FilterArray->At(kVertices))->TurnOn();
  ((StVirtualEventFilter *)m_FilterArray->At(kGlobalTracks))->TurnOn();
  ((StVirtualEventFilter *)m_FilterArray->At(kTrack))->TurnOn();

  gROOT->GetListOfBrowsables()->Add(this,GetName());
}
//_____________________________________________________________________________
StEventDisplayMaker::~StEventDisplayMaker(){
  gROOT->GetListOfBrowsables()->RecursiveRemove(this);
  ClearEvents();
  delete m_FilterArray;
  delete m_EventsNode;
  delete m_HitCollector;
  delete m_TrackCollector;
  delete m_TableCollector;
  if (m_ListDataSetNames) {
     m_ListDataSetNames->Delete();
     delete m_ListDataSetNames;
     m_ListDataSetNames = 0;
  }
}
//
//_____________________________________________________________________________
Int_t StEventDisplayMaker::Init(){
   // Create geometry 
   BuildGeometry();
   // Create a special node to keep "tracks" and "hits"
   CreateTrackNodes();
   // define the custom palette (may affect other pictures)
#ifdef PALETTE
   palette();    
#endif
   // Call the "standard" Init()
   return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StEventDisplayMaker::BuildGeometry()
{
  // Create STAR sub-detector definition

  m_Hall = (St_Node *)GetDataSet("HALL");
  if (!m_Hall) return kStErr;
  //
  // Create an iterator to navigate STAR geometry
  St_DataSetIter volume(m_Hall,0);
// ---  Create "standard" TPC and SVT views ----
  St_Node *sector = 0;
  const Char_t *volueNames[] = {"TPSS","SLDI","SFDM"};
  const Int_t lvolueNames = sizeof(volueNames)/sizeof(Char_t *);
  while ( (sector = ( St_Node *)volume()) ){
    Bool_t found = kFALSE;
    Int_t i;
    for (i =0; i < lvolueNames; i++) 
    if (strcmp(sector->GetName(),volueNames[i]) == 0 ) {found = kTRUE; break; }
    if (found) {
      sector->SetVisibility(St_Node::kSonUnvisible);
      sector->Mark();
      if (!i) {  // special case for TPSS sectors
        TTUBE *tubs = (TTUBE *)sector->GetShape();
        tubs->SetNumberOfDivisions(1);
      }
    }
  }

  // Select sensors
  m_FullView = new St_NodeView(*m_Hall); 
  // Create the "open" sub-structure from the full one
  m_Sensible = new St_NodeView(m_FullView);
  delete m_FullView; m_FullView = 0;
  printf(" drawing the STAR geometry sensible volumes and hits \n");

//  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/HitsDrawFullView.gif"> </P> End_Html // 

  //_______________________________________
  //
  //   Make simple view
  //   Select node to be left
  //_______________________________________
  printf(" Marking the current structure to create another simplified one\n");
  m_Sensible->Mark();                                                       // Select HALL
  
  // Select all sectors
  St_DataSetIter nextSector(m_Sensible,0);
  St_DataSet *tpssNode = 0;
  while (( tpssNode = nextSector() ) ) {
//    if (strcmp(tpssNode->GetName(),"TPGV") && strcmp(tpssNode->GetName(),"TPSS")) continue;
    Int_t i;
    for (i =0; i < lvolueNames; i++) 
      if (strcmp(tpssNode->GetName(),volueNames[i]) == 0 ) {tpssNode->Mark(); break;}    
  }

  //_______________________________________
  //
  //    Select all hits
  //_______________________________________
  St_NodeView *trackNode = (St_NodeView *)m_Sensible->FindByName(".track");    // Select ".track"
  if (trackNode) trackNode->Mark();     
  trackNode = (St_NodeView *)m_Sensible->FindByName("CAVE");                   // Select CAVE
  if (trackNode) {
     trackNode->Mark();     
     St_DataSetIter nextHits(trackNode);
     while ( ( m_ShortView = (St_NodeView *) nextHits()) ) {
       if (strcmp(m_ShortView->GetName(),"ZCAL")==0) continue;       // skip ZCAL detector element
       m_ShortView->Mark();
    }
  }
   
  //_______________________________________
  //
  //   Create new short view                                        // New "short" dataset
  //_______________________________________
  printf(" Creating a new structure simplified structure\n");
  m_ShortView = new St_NodeView(m_Sensible);
  delete m_Sensible; m_Sensible = 0;

  //_______________________________________
  //
  //   Replace the "HALL" node with an articial ".event"  node
  //_______________________________________
  return 0;
}

//______________________________________________________________________________
void StEventDisplayMaker::AddName(const Char_t *name)
{
  //  "name" - StEvent
  //  "g2t_tpc_hit(track_id,x[0]:x[1]:x[2])"
  //   Attention:     NO EXPRESSION, yet !!!

  if (!m_ListDataSetNames) m_ListDataSetNames = new TList;
  m_ListDataSetNames->Add(new TObjString(name));
}
//______________________________________________________________________________
void StEventDisplayMaker::ClearEvents()
{
  if (m_Mode == 2) return;
  // Clear picture
  if (m_EventsNode) {
    if (m_PadBrowserCanvas) 
        m_PadBrowserCanvas->Clear();
    delete m_EventsView;
    m_EventsView = 0;
    St_Node *node = 0;
    St_DataSetIter nextNode(m_EventsNode);
    while ( (node = (St_Node *)nextNode() )) {
      TShape *shape = node->GetShape();
      if (shape) delete shape;   
      m_EventsNode->Delete();
      m_HitCollector->Delete();
      m_TrackCollector->Delete();
   }
  }
}
//_____________________________________________________________________________
void StEventDisplayMaker::ClearCanvas()
{
  // Clear canvas from the TBrowser Context Menu
  if (m_PadBrowserCanvas) {
    Clear();
    m_PadBrowserCanvas->Modified();
    m_PadBrowserCanvas->Update();
  }
}

//_____________________________________________________________________________
Int_t StEventDisplayMaker::CreateTrackNodes()
{
 //  Create St_Node dataset
  if (m_EventsNode) delete m_EventsNode;
  m_EventsNode  = new St_Node(".track",".track",(TShape *)0);
  m_EventsNode->Mark();
  m_EventsNode->SetVisibility();
  AddConst(m_EventsNode);
  return 0;
}
//______________________________________________________________________________
void StEventDisplayMaker::Clear(Option_t *)
{
  ClearEvents();
//  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StEventDisplayMaker::CreateCanvas()
{
  if (!m_PadBrowserCanvas) {
   // Attention !!! The name of TCanvas MUST unique across all ROOT
   // objects otherwise those will be destroyed by TCanvas ctor !!!
     m_PadBrowserCanvas = new TCanvas("STARMonitor","Event Display",10,600,400,400);
  }
  m_ShortView->Draw();
  m_PadBrowserCanvas->Modified();
  m_PadBrowserCanvas->Update();
  return 0;
}

//_____________________________________________________________________________
Int_t StEventDisplayMaker::MakeGlobalTracks()
{
  Int_t trackCounter  = 0;
  Int_t hitCounter    = 0;
#ifdef STEVENT
  const Int_t maxTrackCounter = 9999999;
  Width_t size;
  Style_t style;
  StTrackCollection *tracks = m_Event->trackCollection();
  if (tracks) {
     StGlobalTrackIterator next(tracks);
     StGlobalTrack *globTrack = 0;
     while ( ( globTrack = (StGlobalTrack *)next() ) && trackCounter < maxTrackCounter) {
        StVirtualEventFilter *filter = (StVirtualEventFilter *)m_FilterArray->At(kGlobalTracks);
        if (filter && (filter->IsOff() || ( filter->Filter(globTrack,size,style)==0 ) ) ) continue;
        // ------------------------   Tracks   ------------------------- //
        filter = (StVirtualEventFilter *)m_FilterArray->At(kTrack);
        if (!filter || filter->IsOn() ) 
              trackCounter +=  MakeTracks(globTrack,filter);

        // -------------------------   Hits  --------------------------- //
        filter = (StVirtualEventFilter *)m_FilterArray->At(kTrackTpcHits);
        if (!filter || filter->IsOn() ) {
           const StVecPtrTpcHit &hits   = globTrack->tpcHits();
           hitCounter += MakeHits(&hits,filter);
        }

        filter = (StVirtualEventFilter *)m_FilterArray->At(kTrackSvtHits);
        if (!filter || filter->IsOn() ) {
           const StVecPtrSvtHit &hits   = globTrack->svtHits();
           hitCounter += MakeHits(&hits,filter);
        }

        filter = (StVirtualEventFilter *)m_FilterArray->At(kTrackFtpcHits);
        if (!filter || filter->IsOn() ) {
           const StVecPtrFtpcHit &hits   = globTrack->ftpcHits();
           hitCounter += MakeHits(&hits,filter);
        }
     }
     printf(" %d tracks %d hits have been found\n",trackCounter, hitCounter);
   }   
#endif
   return trackCounter+hitCounter;
}

//_____________________________________________________________________________
Int_t StEventDisplayMaker::Make()
{
//  const Int_t maxTrackCounter = 9;
  if (!m_EventsNode) return kStErr;
  if (m_Mode == 1)   return  kStOK;
  CreateCanvas();
  if (Debug()) PrintInfo();
  //_______________________________________
  //
  // Reset all filters
  //_______________________________________
 
  TIter nextFilter(m_FilterArray);
  StVirtualEventFilter *f = 0;
  while ( (f = (StVirtualEventFilter *)nextFilter()) ) f->Reset(); 

 //---  Temporary ---
 //   ClearCanvas();
 //_______________________________________
 //
 //   Creating tracks and hits shapes
 //_______________________________________
  Int_t totalCounter = 0;
  if (m_ShortView){
    if (!m_ListDataSetNames || m_ListDataSetNames->GetSize() == 0) 
    {
    // Add default names
      m_Event  = (StEvent *) GetDataSet("StEvent");       
      if (m_Event)  AddName("StEvent");
      else {
        St_DataSet *dshits = GetDataSet("tphit");
        if (dshits)   {
           AddName("tphit(id_globtrk,x:y:z)");
           printf(" tphit found !!!\n");  
          ((St_Table *)dshits)->Print(0,10);   
        }
         St_DataSet *dstracks = GetDataSet("tptrack");
         if (dstracks) {
            AddName(dstracks->GetName());
            printf(" tptrack found !!!\n"); 
           ((St_Table *)dstracks)->Print(0,1);
         }
      }
    }
    TIter nextNames(m_ListDataSetNames);
    TObjString *eventName = 0;
     while ( (eventName = (TObjString *)nextNames()) ) 
    {
      m_Event = 0;
      m_Table = 0;
  
      Char_t *nextObjectName = StrDup(eventName->String().Data());
      Char_t *positions[] = {0,0,0,0,0,0};
      Int_t type = ParseName(nextObjectName, positions);
      if (Debug()) { printf(" The type of the current parameter is %d \n", type); }
      if (!type) { delete [] nextObjectName; continue; }
      const Char_t *foundName = positions[0];
      St_DataSet *event = GetDataSet(foundName);
      if (!event) {
         if (Debug()) Warning("Make","No object \"%s\" found",foundName);
         continue;
      }
      if (event->InheritsFrom("St_Table") && 
          (( type == 5) || (type == 1) || (type == 2)) ) 
      {
        //  ----- Draw "table" events -------------------------- //
          m_Table = (St_Table *)event;                           //
          totalCounter += MakeTable((const Char_t **)positions); //
        //  ---------------------------------------------------- //
      }
      else if (event->InheritsFrom("StEvent") && type == 1) {
         //  ---- Draw "StEvent" events ---- //
           m_Event = (StEvent *)event;       //
           totalCounter += MakeEvent();      //
         //  ------------------------------- //
      }
      else if (Debug()) Warning("Make","Can not draw the object \"%s\"",nextObjectName); 
      delete [] nextObjectName;
     }
     if (totalCounter) {
       m_EventsView = new St_NodeView(*m_EventsNode);
       m_ShortView->Add(m_EventsView);
     }
     printf(" updating view of %d objects\n",totalCounter);
     m_PadBrowserCanvas->Update();
   }
   return kStOK;
}

//_____________________________________________________________________________
Int_t StEventDisplayMaker::ParseName(Char_t *inName, Char_t *positions[])
{ 
  // returns  the number of the tokens found:
  //
  //              "1" - assuming StEvent name defined by position[0]
  //              "5" - assuming St_Table columns definitions
  //              "0" - syntax error
  //  
  //  "name" - StEvent
  //  "g2t_tpc_hit(track_id,x[0]:x[1]:x[2])"
  //   Attention:     NO EXPRESSION, yet !!!
  //

  Int_t nParsed = 0;
  if (inName && inName[0]) {
    Char_t *pos = 0;
    const Char_t *errorMessages[] = {  "the open bracket missed"
                                     , "first comma missed"
                                     , "first collon missed"
                                     , "second collon missed"
                                     , "the closed bracket missed"
                                    };
    const Int_t lenExpr = sizeof(errorMessages)/sizeof(Char_t *);
    const Char_t *openBracket  = "(";
    const Char_t *closeBracket = ")";
    const Char_t *comma        = ",)";
    const Char_t *collon       = ":";
    const Char_t *delimiters[] = {openBracket,comma,collon,collon,closeBracket };
    pos = positions[nParsed] = inName;
    UInt_t idx = 0;
    nParsed = 1;
    Bool_t hasClosed = kFALSE;
    for (nParsed = 1; nParsed <= lenExpr; nParsed ++ ) 
    {
      if  ( (idx = strcspn(pos+1,delimiters[nParsed-1])) >=  strlen(pos+1) ) break;
      pos = pos+idx+1;
      positions[nParsed] = pos+1;
      // Is it close bracket
      if (*pos == *closeBracket) { *pos = 0 ; hasClosed = kTRUE; break; }
      *pos = 0;
    }
    if (nParsed > 1) {
      for (Int_t i=1;i<nParsed-1;i++) 
         if (!positions[i]) cerr << "StEventDisplayMaker::ParseName" << errorMessages[i-1] << endl;
         if ( (nParsed <= lenExpr) && !hasClosed ) nParsed = 0;
    }
  }
  return nParsed;
}

//_____________________________________________________________________________
Int_t StEventDisplayMaker::MakeEvent()
{
  if (!m_Event) return 0;

  Int_t total      = 0;
  Int_t hitCounter = 0;
  //----------------------------//
    total = MakeGlobalTracks(); //
  //----------------------------//
#ifdef STEVENT
  StVirtualEventFilter *filter = 0;
  filter = (StVirtualEventFilter *)m_FilterArray->At(kTpcHit);
  if (!filter || filter->IsOn() ) {
  StTpcHitCollection *hits   = m_Event->tpcHitCollection();
     hitCounter = MakeHits(hits,filter);
     if (Debug()) printf(" TpcHitCollection: %d hits\n", hitCounter);
     total += hitCounter;
  }

  filter = (StVirtualEventFilter *)m_FilterArray->At(kFtpcHit);
  if (!filter || filter->IsOn() ) {
     StFtpcHitCollection *hits   = m_Event->ftpcHitCollection();
     hitCounter = MakeHits(hits,filter);
     if (Debug()) printf(" FtpcHitCollection: %d hits\n", hitCounter);
     total += hitCounter;
  }

  filter = (StVirtualEventFilter *)m_FilterArray->At(kSvtHit);
  if (!filter || filter->IsOn() ) {
     StSvtHitCollection *hits   = m_Event->svtHitCollection();
     hitCounter = MakeHits(hits,filter);
     if (Debug()) printf(" SvtHitCollection: %d hits\n", hitCounter);
     total += hitCounter;
  }

  filter = (StVirtualEventFilter *)m_FilterArray->At(kEmcTowerHit);
  if (!filter || filter->IsOn() ) {
    StEmcTowerHitCollection *hits   = m_Event->emcTowerHitCollection();
    hitCounter = MakeHits(hits,filter);
    if (Debug()) printf(" EmcTowerHitCollection: %d hits\n", hitCounter);
    total += hitCounter;
  }

  filter = (StVirtualEventFilter *)m_FilterArray->At(kEmcPreShowerHit);
  if (!filter || filter->IsOn() ) {
    StEmcPreShowerHitCollection *hits   = m_Event->emcPreShowerHitCollection();
    hitCounter = MakeHits(hits,filter);
    if (Debug()) printf(" EmcPreShowerHitCollection: %d hits\n", hitCounter);
    total += hitCounter;
  } 

  filter = (StVirtualEventFilter *)m_FilterArray->At(kSmdPhiHit);
  if (!filter || filter->IsOn() ) {
    StSmdPhiHitCollection *hits   = m_Event->smdPhiHitCollection();
    hitCounter = MakeHits(hits,filter);
    if (Debug()) printf(" SmdPhiHitCollection: %d hits\n", hitCounter);
    total += hitCounter;
  }

  filter = (StVirtualEventFilter *)m_FilterArray->At(kSmdEtaHit);
  if (!filter || filter->IsOn() ) {
    StSmdEtaHitCollection *hits   = m_Event->smdEtaHitCollection();
    hitCounter = MakeHits(hits,filter);
    if (Debug()) printf(" SmdEtaHitCollection: %d hits\n", hitCounter);
    total += hitCounter;
  }

  filter = (StVirtualEventFilter *)m_FilterArray->At(kVertices);
  if (!filter || filter->IsOn() ) {
    StVertexCollection *vertices   = m_Event->vertexCollection();
    hitCounter =  MakeVertices(vertices,filter);
    if (Debug()) printf(" VertexCollection: %d vertices\n", hitCounter);
    total += hitCounter;
  }

  filter = (St
