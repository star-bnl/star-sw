//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
//  
// 

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventDisplayMaker class for Makers                                 //
//  To diplay the StEvent/StGlobalTrack object with the GEANT geometry  //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html   //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TROOT.h"
#include "TCanvas.h"
#include "TColor.h"
#include "TStyle.h"
#include "StEventDisplayMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_Node.h"
#include "St_NodeView.h"
#include "St_NodePosition.h"
#include "St_PolyLine3D.h"
#include "StHits3DPoints.h"
#include "StHelix3DPoints.h"
#include "StVirtualEventFilter.h"

#include "StEvent.h"
#include <TCanvas.h>
#include <TGeometry.h>
#include <TWebFile.h>
#include <TBRIK.h>
#include <TH2.h>
#include <TMath.h>

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

  m_PadBrowserCanvas = 0;

  m_FilterArray   = new TObjArray(kEndOfEventList);
  Int_t i; 
  for (i =0;i<kEndOfEventList;i++) {
   m_FilterArray->AddAt(&m_DefaultFilters[i],i);
//    m_FilterArray->AddAt(&hitsOffFilter,i);
  }
  ((StVirtualEventFilter *)m_FilterArray->At(kGlobalTracks))->TurnOn();
  ((StVirtualEventFilter *)m_FilterArray->At(kTrack))->TurnOn();

  gROOT->GetListOfBrowsables()->Add(this,GetName());
}
//_____________________________________________________________________________
StEventDisplayMaker::~StEventDisplayMaker(){
  ClearEvents();
  delete m_FilterArray;
  delete m_EventsNode;
  delete m_HitCollector;
  delete m_TrackCollector;
}
//_____________________________________________________________________________
static void palette()
{  
  const float  saturation = 1;
  const float  lightness = 0.5;
  const float  MaxHue = 280;
  const float  MinHue = 0;
  const int    MaxColors = 50;   
  int          palette[MaxColors];
  int          index;
  float        hue, r, g, b, rv, gv, bv;
  TColor       *color;
  unsigned int failures = 0;
  
  for (int i=0 ; i<MaxColors ; i++) {
      index = palette[i] = MaxColors+1+i;     
      color = new TColor(index, 0, 0, 0);
      hue = MaxHue-(i+1)*((MaxHue-MinHue)/MaxColors);
      color->HLStoRGB(hue, lightness, saturation, r, g, b);
      color->SetRGB(r, g, b);
      gGXW->GetRGB(index, rv, gv, bv);
      if (r != rv || g != gv || b != bv) {
          failures++;
          palette[i] =  i ? palette[i-1] : 1;
      }
  }
  if (failures)
      printf("palette(): couldn't allocate %d of %d colors\n", failures, MaxColors);
  gStyle->SetPalette(MaxColors, palette);
}
//_____________________________________________________________________________
Int_t StEventDisplayMaker::Init(){
   // Create geometry 
   BuildGeometry();
   // Create a special node to keep "tracks" and "hits"
   CreateTrackNodes();
   // define the custom palette (may affect other pictures)
   palette();
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
  St_DataSetIter volume(m_Hall);

  // Select sensors
  m_FullView = new St_NodeView(*m_Hall); 
  // Create the "open" sub-structure from the full one
  m_Sensible = new St_NodeView(m_FullView);
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
    if (strcmp(tpssNode->GetName(),"TPGV")) continue;
    tpssNode->Mark();
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

  //_______________________________________
  //
  //   Replace the "HALL" node with an articial ".event"  node
  //_______________________________________
  return 0;
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
  const Int_t maxTrackCounter = 9999999;
  Int_t trackCounter  = 0;
  Int_t hitCounter    = 0;
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

        // ------------------------   Hits   ------------------------- //
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
 //---  Temporary ---
//   ClearCanvas();
 //_______________________________________
 //
 //   Creating tracks and hits shapes
 //_______________________________________
  m_Event  = (StEvent *) GetDataSet("StEvent");
  if (!m_Event) return kStErr;

  Int_t total      = 0;
  Int_t hitCounter = 0;
  if (m_ShortView){

     total = MakeGlobalTracks();
     StVirtualEventFilter *filter = 0;
     filter = (StVirtualEventFilter *)m_FilterArray->At(kTpcHit);
     if (!filter || filter->IsOn() ) {
        StTpcHitCollection *hits   = m_Event->tpcHitCollection();
        hitCounter += MakeHits(hits,filter);
        printf(" TpcHitCollection: %d \n", hitCounter);
     }

     filter = (StVirtualEventFilter *)m_FilterArray->At(kFtpcHit);
     if (!filter || filter->IsOn() ) {
        StFtpcHitCollection *hits   = m_Event->ftpcHitCollection();
        hitCounter += MakeHits(hits,filter);
        printf(" FtpcHitCollection: %d \n", hitCounter);
     }

     filter = (StVirtualEventFilter *)m_FilterArray->At(kSvtHit);
     if (!filter || filter->IsOn() ) {
        StSvtHitCollection *hits   = m_Event->svtHitCollection();
        hitCounter += MakeHits(hits,filter);
        printf(" SvtHitCollection: %d \n", hitCounter);
     }

     filter = (StVirtualEventFilter *)m_FilterArray->At(kEmcTowerHit);
     if (!filter || filter->IsOn() ) {
        StEmcTowerHitCollection *hits   = m_Event->emcTowerHitCollection();
        hitCounter += MakeHits(hits,filter);
        printf(" EmcTowerHitCollection: %d \n", hitCounter);
     }

     filter = (StVirtualEventFilter *)m_FilterArray->At(kEmcPreShowerHit);
     if (!filter || filter->IsOn() ) {
        StEmcPreShowerHitCollection *hits   = m_Event->emcPreShowerHitCollection();
        hitCounter += MakeHits(hits,filter);
        printf(" EmcPreShowerHitCollection: %d \n", hitCounter);
     } 

     filter = (StVirtualEventFilter *)m_FilterArray->At(kSmdPhiHit);
     if (!filter || filter->IsOn() ) {
        StSmdPhiHitCollection *hits   = m_Event->smdPhiHitCollection();
        hitCounter += MakeHits(hits,filter);
        printf(" SmdPhiHitCollection: %d \n", hitCounter);
     }

     filter = (StVirtualEventFilter *)m_FilterArray->At(kSmdEtaHit);
     if (!filter || filter->IsOn() ) {
        StSmdEtaHitCollection *hits   = m_Event->smdEtaHitCollection();
        hitCounter += MakeHits(hits,filter);
        printf(" SmdEtaHitCollection: %d \n", hitCounter);
     }

     if (total+hitCounter ) {
        m_EventsView = new St_NodeView(*m_EventsNode);
        m_ShortView->Add(m_EventsView);
     }
   }
  printf(" updating view of %d global tracks and %d hits\n",total, hitCounter);
  m_PadBrowserCanvas->Update();
  return kStOK;
}
//_____________________________________________________________________________
Color_t StEventDisplayMaker::GetColorAttribute(Int_t adc)
{
  // Convert the inpput signal amplitude into color index
//  return Color_t(10-(adc?TMath::Log2(adc):10));
//  return Color_t(adc?TMath::Log2(adc)+50:10);
  return Color_t(50 + (adc/256));
}


//_____________________________________________________________________________
Int_t StEventDisplayMaker::MakeHits(const StObjArray *eventCollection,StVirtualEventFilter *filter)
{
  if (eventCollection && eventCollection->GetLast() ) {
    Int_t   hitCounter = 0;
    Color_t hitColor = kYellow;
    Style_t hitStyle = 1;
    Width_t hitSize  = 2;

    // --------------------- hits filter ------------------------- //
    if (filter) hitColor =  filter->Filter(eventCollection,hitSize,hitStyle);
    // ----------------------------------------------------------- //
    if (hitColor > 0) {
       StHits3DPoints   *hitsPoints  = new StHits3DPoints((StObjArray *)eventCollection);
       m_HitCollector->Add(hitsPoints);    // Collect to remove  
       St_PolyLineShape *hitsShape   = new St_PolyLineShape(hitsPoints);
         hitsShape->SetVisibility(1);        hitsShape->SetLineColor(hitColor);
         hitsShape->SetLineStyle(hitStyle);  hitsShape->SetLineWidth(hitSize);
       // Create a node to hold it
       St_Node *thisHit = new St_Node("hits",eventCollection->GetName(),hitsShape);
         thisHit->Mark();
         thisHit->SetVisibility();
       St_NodePosition *pp = m_EventsNode->Add(thisHit); 
       if (!pp && hitCounter) {
          printf(" no track position %d\n",hitCounter);
       }
       return hitColor;
    }
  }
  return 0;
}

//_____________________________________________________________________________
Int_t StEventDisplayMaker::MakeTracks( StGlobalTrack *globTrack,StVirtualEventFilter *filter)
{
  if (globTrack) {
    Int_t   trackCounter = 0;
    Color_t trackColor = kRed;
    Style_t trackStyle = 1;
    Width_t trackSize  = 2;

    // --------------------- tracks filter ------------------------- //
    if (filter) trackColor =  filter->Filter(globTrack,trackSize,trackStyle);
    // ----------------------------------------------------------- //
    if (trackColor > 0) {
       StHelix3DPoints *tracksPoints  = new StHelix3DPoints(globTrack,globTrack->length(),30);
       m_TrackCollector->Add(tracksPoints);    // Collect to remove  
       St_PolyLineShape *tracksShape   = new St_PolyLineShape(tracksPoints,"L");
         tracksShape->SetVisibility(1);         tracksShape->SetColorAttribute(trackColor);
         tracksShape->SetLineStyle(trackStyle); tracksShape->SetSizeAttribute(trackSize);
       // Create a node to hold it
       St_Node *thisTrack = new St_Node("tracks",globTrack->GetName(),tracksShape);
         thisTrack->Mark();   thisTrack->SetVisibility();
         trackCounter++;
       St_NodePosition *pp = m_EventsNode->Add(thisTrack); 
       if (!pp && trackCounter) {
          printf(" no track position %d\n",trackCounter);
       }
       return trackColor;
    }
  }
  return 0;
}

//________________________________
//
//       -- Filters  --
//________________________________


//_____________________________________________________________________________
Int_t StEventDisplayMaker::SetFlag(Int_t flag, EDisplayEvents filterIndex)
{
 // Set the new filter flag and return the previous one
  StVirtualEventFilter *f = (StVirtualEventFilter *)m_FilterArray->At(filterIndex);
  Int_t res = f ? f->Turn(flag): 0;
  if (Debug()) PrintFilterStatus();
  return res;
}
//_____________________________________________________________________________
StVirtualEventFilter *StEventDisplayMaker::SetFilter(StVirtualEventFilter *filter, EDisplayEvents filterIndex)
{
 // Set the new filter and return the previous one
  StVirtualEventFilter *f = 0;
  if (filter) {
   f = (StVirtualEventFilter *)m_FilterArray->At(filterIndex);
   if (f) 
     filter->Turn(f->GetFlag());
   m_FilterArray->AddAt(filter,filterIndex);
  }
  if (Debug()) PrintFilterStatus();
  return f;
}
 
//_____________________________________________________________________________
void StEventDisplayMaker::PrintFilterStatus()
{
  const Char_t *filterNames[] = {
                                  "Primary Vertex"
                                 , "Tpc Hit"
                                 , "Svt Hit"
                                 , "Ftpc Hit"
                                 , "EmcTower Hit"
                                 , "EmcPreShower Hit"
                                 , "SmdPhi Hit"
                                 , "SmdEta Hit"
                                 , "Global Tracks"
                                 ,  "Track"
                                 ,  "Track Tpc Hits"
                                 ,  "Track Svt Hits"
                                 ,  "Track Ftpc Hits" 
                                } ;
   
  Int_t i;
  for (i =0;i<kEndOfEventList;i++) {
      StVirtualEventFilter *filter = (StVirtualEventFilter *)m_FilterArray->At(i);
      Int_t isOn = filter->IsOn();
      printf(" Filter for %16s%",filterNames[i]);
      if (filter) {
          if (filter->IsOn()) printf(" is ON\n");
          else printf(" is       OFF\n");
      }
      else printf(" doesn't exist\n");
  }
}

#define DISPLAY_FILTER_DEFINITION(filterName)                                  \
Int_t  StEventDisplayMaker::_NAME3_(Set,filterName,Flag)(Int_t flag)         \
{ return SetFlag(flag,_NAME2_(k,filterName)); }                              \
StVirtualEventFilter *StEventDisplayMaker::_NAME2_(Set,filterName)(StVirtualEventFilter *filter) \
{ return SetFilter(filter,_NAME2_(k,filterName)); }

DISPLAY_FILTER_DEFINITION(PrimaryVertex)

// -- Hits collections filters --

DISPLAY_FILTER_DEFINITION(TpcHit)
DISPLAY_FILTER_DEFINITION(SvtHit)
DISPLAY_FILTER_DEFINITION(FtpcHit)
DISPLAY_FILTER_DEFINITION(EmcTowerHit)
DISPLAY_FILTER_DEFINITION(EmcPreShowerHit)
DISPLAY_FILTER_DEFINITION(SmdPhiHit)
DISPLAY_FILTER_DEFINITION(SmdEtaHit)

// -- StGlobalTrack filters --

DISPLAY_FILTER_DEFINITION(GlobalTracks)
DISPLAY_FILTER_DEFINITION(Track)
DISPLAY_FILTER_DEFINITION(TrackTpcHits)
DISPLAY_FILTER_DEFINITION(TrackSvtHits)
DISPLAY_FILTER_DEFINITION(TrackFtpcHits)

// --  end of filter list --

