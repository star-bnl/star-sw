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

#include "StEvent.h"
#include <TCanvas.h>
#include <TGeometry.h>
#include <TWebFile.h>
#include <TBRIK.h>
#include <TH2.h>
#include <TMath.h>


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

  SetTrackFilterFlag();
  SetHitFilterFlag();
  gROOT->GetListOfBrowsables()->Add(this,GetName());
}
//_____________________________________________________________________________
StEventDisplayMaker::~StEventDisplayMaker(){
  ClearEvents();
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
void StEventDisplayMaker::Clear(Option_t *option)
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
Int_t StEventDisplayMaker::HitsFilter(const StVecPtrTpcHit &)
{
 // indicate whether the hit information of the global track is filtered
  return GetHitFilterFlag();
}
//_____________________________________________________________________________
Int_t StEventDisplayMaker::GlobalTrackFilter(StGlobalTrack *)  {
  // indicate whether the entire track information is filtered
  return 1;
}
//_____________________________________________________________________________
Int_t StEventDisplayMaker::TrackFilter(StGlobalTrack *)  {
 // indicate whether the helix information of the global track is filtered
  return GetTrackFilterFlag();
}
//_____________________________________________________________________________
Int_t StEventDisplayMaker::Make()
{
  const Int_t maxTrackCounter = 9999999;
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
  Int_t trackCounter  = 0;
  Int_t hitCounter    = 0;
  Int_t tpcHitCounter = 0;

  m_Event  = (StEvent *) GetDataSet("StEvent");
  if (!m_Event) return kStErr;
  StTrackCollection *tracks = m_Event->trackCollection();
  if (tracks) {
     StGlobalTrackIterator next(tracks);
     StGlobalTrack *globTrack = 0;
     while ( ( globTrack = (StGlobalTrack *)next() ) && trackCounter < maxTrackCounter) {
        St_Node *thisTrack = 0;
        // ------------------- Global filter ------------------------- //
        if (!GlobalTrackFilter(globTrack))                 continue;
        // ------------------------ Tracks --------------------------- //
        Color_t trackColor = kRed;
        Style_t trackStyle = 1;
        Width_t trackSize  = 2;
        // ------------------- Tracks filter ------------------------- //
        if (TrackFilter(globTrack) ) {
        // ----------------------------------------------------------- //
          StHelix3DPoints *helixPoints = new StHelix3DPoints(globTrack,globTrack->length(),30);
          m_TrackCollector->Add(helixPoints);  // Collect to remove
          trackColor = trackCounter%7;
          St_PolyLineShape *helixShape  = new St_PolyLineShape(helixPoints,"L");
            helixShape->SetVisibility(1); helixShape->SetColorAttribute(trackColor+kGreen);
            helixShape->SetLineStyle(trackStyle);  helixShape->SetSizeAttribute(trackSize);
            thisTrack = new St_Node("tracks","tracks",helixShape);
            thisTrack->Mark(); thisTrack->SetVisibility();
            trackCounter++;
            St_NodePosition *pp = m_EventsNode->Add(thisTrack); 
            if (!pp && trackCounter) {
               printf(" no track position %d\n",trackCounter);
            }
         }
         // ------------------------   Hits   ------------------------- //
         Color_t hitColor = kYellow;
         Style_t hitStyle = 1;
         Width_t hitWidth = 1;
         const StVecPtrTpcHit &hits   = globTrack->tpcHits();
         // ---------------------- Hit filter ------------------------- //
         if ( HitsFilter(hits) )  {
         // ----------------------------------------------------------- //
           StHits3DPoints *hitPoints    = new StHits3DPoints((StVecPtrTpcHit *)&hits);
           St_PolyLineShape *trackShape = 0;
           if (hitPoints->Size()>1) {
             trackShape  = new St_PolyLineShape(hitPoints);
             trackShape->SetVisibility(1); trackShape->SetColorAttribute(hitColor);
             trackShape->SetLineStyle(hitStyle);  trackShape->SetSizeAttribute(hitWidth);
             m_HitCollector->Add(hitPoints);  // Collect to remove  
             thisTrack = new St_Node("hits","hits",trackShape);
             thisTrack->Mark(); thisTrack->SetVisibility();
             hitCounter++;
             St_NodePosition *pp = m_EventsNode->Add(thisTrack); 
             if (!pp && hitCounter) {
                printf(" no hit position %d\n",hitCounter);
             }
          }
          else 
            delete hitPoints;         
        }
     }
     printf(" %d tracks %d hits have been found\n",trackCounter, hitCounter);
   }   
   
#if 0
   // ------------------- TPC hits filter ------------------------- //
     if (!tpcHitsFilter(globTrack))                 continue;
   // ------------------------ Tracks --------------------------- //
   StTpcHitCollection *t          = m_Event->tpcHitCollection();
   StHits3DPoints *tpcHitsPoints  = new StHits3DPoints(t);
   St_PolyLineShape *tpcHitsShape = new St_PolyLineShape(tpcHitsPoints);
   trackShape->SetVisibility(1);
   trackShape->SetLineColor(kYellow);
   trackShape->SetLineStyle(1);
   trackShape->SetLineWidth(2);
   // Create a node to hold it
    if (!thisTrack) {
      thisTrack = new St_Node("hits","hits",trackShape);
      thisTrack->Mark();
      thisTrack->SetVisibility();
      St_NodePosition *pp = hall->Add(thisTrack);
      if (!pp) printf(" no position %d\n",ntrack);
      tpcHitCounter++;
   }
#endif
   if ( (trackCounter  || hitCounter || tpcHitCounter ) && m_ShortView){
     // Create new one
       m_EventsView = new St_NodeView(*m_EventsNode);
       m_ShortView->Add(m_EventsView);
   }
  printf(" updating view \n");
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

