//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
// $Id: StEventDisplayMaker.cxx,v 1.129 2007/11/14 22:42:16 fine Exp $

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEventDisplayMaker class for Makers                                 //
//                                                                      //
//  To mix and display the "detector" object defined by  GEANT geometry //
//  and "event" object defined by TTable objects                        //
//                                                                      //
//  The "regular" ROOT viewers, namely TPad, X3D and OpenGL can be used //
//                                                                      //
//  Example provided by default filter (StVirtualEventFilter class):    //
//  begin_html <P ALIGN=CENTER> <IMG SRC="gif/EventDisplay.gif" width=100%> </P> end_html   //
//                                                                      //
// - "doEvents" - defines the "bfc" should make NO reconstruction       //
//                and get the information from the begin_html <a href="http://www.rhic.bnl.gov/STAR/html/comp_l/ofl/dst_table_model.html">DST</a> end_html file directly    //
// - "y1h geant" - define the source of the detector geometry           //
// - "noevent"   - defines no StEvent output is required                //
//                                                                      //
// - green dots represent the hits                                      //
//   from begin_html <a href="dst_point_st.html">dst/point</a> end_html  table with no track associated                      //
//                                                                      //
// - "colored" dots represent the hits associated with begin_html <a href="dst_track_st.html">dst/globtrk</a> end_html //
//   (The color is used to distinguish the hits of one track from others)//
//                                                                      //
// - "colored" lines represent the begin_html <a href="dst_track_st.html">dst/primtrk</a> end_html          //
//                                                                      //
// - "detector geometry" is defined by STAR geant definition (see: St_geant_Maker) //
//                                                                      //
// - the default 3D viewer is ROOT TPad object. That can be switched to either //
//   X3D or OpenGL view TPad "view" menu (see picture attached)         //
//  begin_html <P ALIGN=CENTER> <IMG SRC="gif/DisplayGL.gif" width=100%> </P> end_html   //
//                                                                      //
// - The custom view can be provided by user code as well. The user should provide 
//   his/her own selection with the selection class-filter.             //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html   //
//                                                                      //
//                                                                      //
//             How to create the custom filter                          //
//                                                                      //
// 1. Choose name for your filter                                       //
// 2. Pick the pattern:                                                 //
//    cvs co StRoot/macros/graphics/StSectorHitFilter                   //
// 3. Copy it to your filter directory:                                 //
//    cp StRoot/macros/graphics/StSectorHitFilter/* StRoot/MyFilter     //
//    where "MyFilter" is the name of the filter you choose             //
// 3. Replace all "SectorHit" with the name of the filter you've chosen //
// 4. Edit St<your_name>Filer.cxx to introduce your own version of      //
//    St<your_name>EventFiler::Channel methods                          //
// 5. Create the "regular" STAR share library for yor filter class      //
// 6. Call TurnDisplay.C("MyFiler") to include StEventDisplayMaker with //
//    your custom filter in the "regular" chain: "bfc.C" or "doEvents"  //
// 7. Call .x PadControlPanel.C                                         //
//                                                                      //
// 8. Push either "ReDraw" to redraw the current event                  //
//                "NextEvent" to see next event                         //                                                                    //
//////////////////////////////////////////////////////////////////////////

#include "TROOT.h"
#include "Stiostream.h"
#include "TError.h"
#include "TCanvas.h"
#include "TColor.h"
#include "TStyle.h"
#include "TTUBS.h"
#include "TPaveLabel.h"
#include "TGeometry.h"
#include "TBRIK.h"
#include "TMath.h"
#include "TObjString.h"
#include "TSystem.h"
#include "TPolyLine3D.h"
#include "TPolyMarker3D.h"

#include "StEventDisplayMaker.h"
#include "TDataSetIter.h"
#include "TVolume.h"
#include "TVolumeView.h"
#include "TVolumePosition.h"
#include "St_PolyLine3D.h"
#include "TPoints3D.h"
#include "StHits3DPoints.h"
#include "StVertices3DPoints.h"
#include "StHelix3DPoints.h"
#include "tables/St_dst_point_Table.h"
#include "tables/St_dst_track_Table.h"
#include "TTable3Points.h"
#include "St_Table3PackedPoints.h"
#include "StVirtualEventFilter.h"
#include "TTable.h"
#include "TTableSorter.h"
#include "tables/St_tpt_track_Table.h"
#include "tables/St_dst_event_summary_Table.h"
#include "TCoinEmcTower.h"
#include "TDataProvider.h"

#include "StEventControlPanel.h"
#include "StPadControlPanel.h"

#include "StTrackChair.h"

#include "StDefaultFilter.h"
#include "StGlobalFilterABC.h"
#include "StEventHelper.h"
#include "StEventDisplayInfo.h"
#include "StObject.h"

#ifdef R__QT
#  include <qfont.h>
#  include <qapplication.h>
#  include <qpixmap.h>
#  include <qbuttongroup.h>
#  include <qtooltip.h>
#  include "TQtRootViewer3D.h"
#  include "TGQt.h"
    StEventControlPanel *fEventControlPanel=0;  //!
// #define CAN_RENDER_PAD_DIRECTLY
#  include "TQGLViewerImp.h"
#endif 

// StVirtualEventFilter hitsOffFilter(0);
// StVirtualEventFilter hitsOnFilter(1);
// StVirtualEventFilter StEventDisplayMaker::m_DefaultFilters[StEventDisplayMaker::kEndOfEventList];
StDefaultFilter StEventDisplayMaker::m_DefaultFilters[StEventDisplayMaker::kEndOfEventList];

Int_t               StEventDisplayMaker::fgEventLoop = 0;
StEventDisplayInfo *StEventDisplayMaker::fgInfo      = 0;

//_____________________________________________________________________________
StEventDisplayInfo::StEventDisplayInfo(StEventDisplayInfo **kaddr, const char* title, UInt_t w, UInt_t h)
#ifdef R__QT
  :QTextEdit(title)
  {  
    setCaption("Event Info");
    setWFlags( getWFlags() | Qt::WDestructiveClose);
    resize(w,h);
#else
  {
#endif
    fKAddr=kaddr;*fKAddr=this;
  }
//_____________________________________________________________________________
void StEventDisplayInfo::AddText(const char *info){ 
#ifdef R__QT
  append(info);
#endif
}
//_____________________________________________________________________________
void StEventDisplayInfo::SetText(const char *info){ 
#ifdef R__QT
  setText(info);
#endif
}
//_____________________________________________________________________________
inline void StEventDisplayInfo::Popup(){ 
#ifdef R__QT
  show();
  raise();
#endif
}

//_____________________________________________________________________________
//
//                         StEventDisplayMaker
//_____________________________________________________________________________

ClassImp(StEventDisplayMaker)
//_____________________________________________________________________________
StEventDisplayMaker::StEventDisplayMaker(const char *name):StMaker(name)
{
   // check the Qt env
  gROOT->ProcessLine("StCheckQtEnv::SetQtEnv();") ;
  mRedraw    =  0;
  mEventHelper    =  0;
  m_Hall          =  0;  
  m_FullView      =  0;  
  m_ShortView     =  0; 
  m_Sensible      =  0; 
  m_EventsNode    =  0;
  m_EventsView    =  0;
  
  m_HitCollector  = new TList;
  m_TrackCollector= new TList;
  m_TableCollector= new TList;

  m_PadBrowserCanvas = 0;
  mRunNumberLabel    = 0;
  mEventNumberLabel  = 0;
  mDateTimeLabel     = 0;

  m_ListDataSetNames = 0;
  m_VolumeList       = 0;
  mFilterList        = 0;
  memset(fColCash,0,sizeof(fColCash));
  f3DViewer          = 0;
  fCoin3DReady       = kFALSE;
  SetGeomType();
  fEmcTowers         = 0;
  fColorProvider     = 0; 
  fSizeProvider      = 0;
  
  fEventIdToRender   = 0; // render all events
  
  fEventNeedRescan   = kFALSE;
  fNeedsClear3DView  = kFALSE;
  m_FilterArray   = new TObjArray(kEndOfEventList);
  Int_t i; 
  for (i =0;i<kEndOfEventList;i++) {
   m_FilterArray->AddAt(&m_DefaultFilters[0],i); // We want one and the same filter everywhere
//   m_FilterArray->AddAt(&m_DefaultFilters[i],i);
//   m_FilterArray->AddAt(&hitsOffFilter,i);
  }
  ((StVirtualEventFilter *)m_FilterArray->At(kTptTrack))->TurnOn();
  ((StVirtualEventFilter *)m_FilterArray->At(kTable))->TurnOn();

  gROOT->GetListOfBrowsables()->Add(this,GetName());
 
  // Create the default geometry model

  const char *volumeNames[] = {"TPSS","STSI"}; // STLI"};  // tpc + svt
// emc   const char *volueNames[] = {"TPSS","STLI","ECAL","CALB"}; // STSI"};
//       const char *volueNames[] = {"TPSS","STLI","ECAL","CALB","BTOF"};
  const Int_t lvolumeNames = sizeof(volumeNames)/sizeof(char *);
  for (i=0;i<lvolumeNames;i++) AddVolume(volumeNames[i]);
#ifdef R__QT
  // redefine the default application font
  QFont  myFont = QApplication::font();
  // int myFontSize = myFont. pointSize ();
  myFont.setPointSize(8);
  QApplication::setFont(myFont);
  QToolTip::setGloballyEnabled(kTRUE);
#include "starIcon.xpm"
  QPixmap qStarIcon(starIcon);
  fEventControlPanel  = new StEventControlPanel();
  fEventControlPanel->Bar()->setIcon(qStarIcon);
  fEventControlPanel->Show();
  gSystem->DispatchOneEvent(1);
#else
  gROOT->LoadMacro("EventControlPanel.C");
  gSystem->DispatchOneEvent(1);
  gROOT->LoadMacro("PadControlPanel.C"  );
  fprintf(stderr," /n/n/nPlease install Qt package to run Event Display. \n \t Thank you!\n/n/n/n");
#endif

}
//_____________________________________________________________________________
Int_t StEventDisplayMaker::Redraw()
{
   mRedraw=1;
   int ans = Make();
   mRedraw=0;
   return ans;
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
  if (m_VolumeList) {
     m_VolumeList->Delete();
     delete m_VolumeList;
     m_VolumeList = 0;
  }
}

//_____________________________________________________________________________
Int_t StEventDisplayMaker::Init(){
   // Create geometry 
   BuildGeometry();
     // Create a special node to keep "tracks" and "hits"
   CreateTrackNodes();
     // define the custom palette (may affect other pictures)
   gStyle->SetPalette(1);   
   // Call the "standard" Init()
   return StMaker::Init();
}
//_____________________________________________________________________________
static bool FindStiVolume(const char *name,TList *list) {
   // Find STI volume by the prefix
   // There are 3 (by now) Sti volume prefices:
   // 1 - Tpc_
   // 2 - Ssd_
   // 3 - Svt_
   // Extract it if any 
   bool found = false;
   if (list && name && name[0]) {
      char *pos = 0;
      if ((pos = strchr(name,'_'))) {
         TString pre;
         pre.Insert(0,name,pos-name);
         // Look up
         TIter next(list);
         TObject *vol = 0;
         while ( !found &&  (vol = next())) 
         { 
            /*printf("%s %s\n",vol->GetName(),pre.Data()); */ 
            const char *g3name = vol->GetName();
            if (!strcmp(g3name,"TPSS"))      g3name="TPC";
            else if (!strcmp(g3name,"STSI")) g3name="SVT";
            else if (!strcmp(g3name,"SFSM")) g3name="SSD";
            found = pre.BeginsWith(g3name,TString::kIgnoreCase);
         }
      }
   }
   return found;   
}
//_____________________________________________________________________________
Int_t StEventDisplayMaker::BuildGeometry()
{
  // Create STAR sub-detector definition
  m_Hall = 0;
  Bool_t gotSti = kFALSE;
  Int_t dipLevel=2;
  if (GeomType()) 
  {
     m_Hall = (TVolume *)GetDataSet("STIGEOM");
     if (m_Hall) {
        gotSti = kTRUE;
        m_Hall->MarkAll(); // m_Hall->ls(0);
        TDataSetIter volume(m_Hall,0);
// ---  Create "standard" TPC and SVT views ----
        TVolume *sector = 0;
        Int_t countMarked = 0;
        while ( (sector = ( TVolume *)volume()) ){
          Bool_t found = kFALSE;
          if (found = FindStiVolume(sector->GetName(),m_VolumeList)) {
             sector->SetVisibility(TVolume::kBothVisible);
             sector->Mark(); countMarked++;
             if (sector->GetLineColor()==1 || sector->GetLineColor()==7) 
                  sector->SetLineColor(14);
          } else { 
            sector->UnMark();
            sector->SetVisibility(TVolume::kThisUnvisible);
          }
       }
//     if (gotSti && countMarked)  m_Hall->Add(geantHall);
       m_Hall->SetVisibility(TVolume::kThisUnvisible);
       if (Debug()) 
          m_Hall->ls(3);
        
//        dipLevel = 10;
     } else {
        Warning("BuildGeometry","No STI geometry was found. GEANT3 will be used insteed");
     }
  }

  TVolume *geantHall = 0;
  if (!m_Hall) {
    geantHall = (TVolume *)GetDataSet("HALL");
    if (geantHall ) m_Hall = geantHall;
  }
  if (!m_Hall) return kStErr;
  
  // Create an iterator to navigate STAR geometry
  if (geantHall) {
     TDataSetIter volume(geantHall,0);
// ---  Create "standard" TPC and SVT views ----
     TVolume *sector = 0;
     Int_t countMarked = 0;
     while ( (sector = ( TVolume *)volume()) ){
       Bool_t found = kFALSE;
       found = (m_VolumeList && m_VolumeList->FindObject(sector->GetName()));
       if (found) {
          sector->SetVisibility(TVolume::kBothVisible);
          sector->Mark(); countMarked++;
          if (sector->GetLineColor()==1 || sector->GetLineColor()==7) 
                  sector->SetLineColor(14);
          TShape *myShape = sector->GetShape();
          if (myShape->InheritsFrom(TTUBE::Class()) &&
            strcmp(sector->GetName(),"TPSS")==0 ) {	 
            ((TTUBE *)myShape)->SetNumberOfDivisions(1);
          }
        } else { 
          sector->UnMark();
          sector->SetVisibility(TVolume::kThisUnvisible);
        }
     }
//     if (gotSti && countMarked)  m_Hall->Add(geantHall);
     m_Hall->SetVisibility(TVolume::kThisUnvisible);
  }
  m_ShortView = new TVolumeView(*m_Hall,dipLevel); 
  if (Debug()) 
     m_ShortView->ls(0);
//  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/HitsDrawFullView.gif"> </P> End_Html // 
//    if (strcmp(tpssNode->GetName(),"TPGV") && strcmp(tpssNode->GetName(),"TPSS")) continue;
  MakeEmcTowers();
  return 0;
}
//______________________________________________________________________________
void StEventDisplayMaker::ClearGeometry()
{
   // Delete the geometry structure
   delete m_ShortView; m_ShortView = 0; m_EventsView = 0;
}
//______________________________________________________________________________
void StEventDisplayMaker::AddName(const char *name,Bool_t refresh)
{
  //  "name" - StEvent
  //  "g2t_tpc_hit(track_id,x[0]:x[1]:x[2])"
  //   Attention:     NO EXPRESSION, yet !!!

  if (!m_ListDataSetNames) m_ListDataSetNames = new TList;
  if (!m_ListDataSetNames->FindObject(name)) 
        m_ListDataSetNames->Add(new TObjString(name));
  if (strncmp(name,"StEvent",7)==0) {
#ifdef R__QT
     if (refresh && fEventControlPanel)
           fEventControlPanel->Refresh();
#else
     gSystem->DispatchOneEvent(1);
#endif 
  }
}
//______________________________________________________________________________
void StEventDisplayMaker::AddFilter(StFilterABC* filt)
{
    if (!mFilterList) mFilterList = new TList;
    if (mFilterList->FindObject(filt->GetName())) return;
    mFilterList->Add(filt);
#ifdef R__QT
    if (fEventControlPanel) fEventControlPanel->AddFilter((TObject*)filt); 
#endif
}
//______________________________________________________________________________
void StEventDisplayMaker::RemoveName(const char *name)
{
   TObject *o = 0;
   if (m_ListDataSetNames) {
     o = m_ListDataSetNames->FindObject(name);
     if (o) delete m_ListDataSetNames->Remove(o);
  }
}

//______________________________________________________________________________
void StEventDisplayMaker::PrintNames()
{
 if(m_ListDataSetNames) {
   TIter next(m_ListDataSetNames);
   TObjString *str;
   while ( (str = (TObjString *)next()) ) {
     printf(" table: \"%s\"\n",str->String().Data());
   }
 }
}

//______________________________________________________________________________
void StEventDisplayMaker::AddVolume(const char *name)
{
  // Add "GEANT" volume name to the detector model
  if (!m_VolumeList) m_VolumeList = new TList;
  if (!m_VolumeList->FindObject(name)) 
        m_VolumeList->Add(new TObjString(name));
}
//______________________________________________________________________________
void StEventDisplayMaker::RemoveVolume(const char *name)
{
  // Remove "GEANT" volume name to the detector model
   TObject *o = 0;
   if (m_VolumeList) {
     o = m_VolumeList->FindObject(name);
     if (o) delete m_VolumeList->Remove(o);
  }
}

//______________________________________________________________________________
void StEventDisplayMaker::PrintVolumes()
{
  // Print the "GEANT" volume names for the detector model
 if(m_VolumeList) {
   TIter next(m_VolumeList);
   TObjString *str;
   while ( (str = (TObjString *)next()) ) {
     printf(" GEANT volume: \"%s\"\n",str->String().Data());
   }
 }
}

//______________________________________________________________________________
void StEventDisplayMaker::ClearEvents()
{
  if (m_Mode == 2) return;
  // Clear picture
  if (f3DViewer) fNeedsClear3DView  = kTRUE;
  if (m_EventsNode) {
    if (!GetEventPad()) CreateCanvas();
    else if (f3DViewer) 
       f3DViewer->BeginScene(0); // to clear the primitivies
	else 
       m_PadBrowserCanvas->Clear();

    // vf TEmcTowers *emchits = dynamic_cast<TEmcTowers *>(GetDataSet("emchits"));
    // vf if (emchits) emchits->ResetProviders();

    delete m_EventsView;
    m_EventsView = 0;
    TVolume *node = 0;
    TDataSetIter nextNode(m_EventsNode);
    while ( (node = (TVolume *)nextNode() )) {
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
  if (f3DViewer)
    Clear();
  else if (m_PadBrowserCanvas) {
    Clear();
    m_PadBrowserCanvas->Modified();
    m_PadBrowserCanvas->Update();
  }
}

//_____________________________________________________________________________
Int_t StEventDisplayMaker::CreateTrackNodes()
{
 //  Create TVolume dataset
  if (m_EventsNode) delete m_EventsNode;
  m_EventsNode  = new TVolume(".track",".track",(TShape *)0);
  m_EventsNode->Mark();
  m_EventsNode->SetVisibility();
  AddConst(m_EventsNode);
  return 0;
}
//______________________________________________________________________________
void StEventDisplayMaker::Clear(Option_t *)
{
  mRedraw=0;
  ClearEvents();
//  StMaker::Clear();
}

//_____________________________________________________________________________
TVirtualPad *StEventDisplayMaker::CreateCanvas()
{
   if (!GetEventPad()) {
      // Attention !!! The name of TCanvas MUST unique across all ROOT
      // objects otherwise those will be destroyed by TCanvas ctor !!!
      m_PadBrowserCanvas = new TCanvas("STARMonitor","Event Display",10,600,400,400);
      m_PadBrowserCanvas->ResetView3D(0);
      m_PadBrowserCanvas->SetFillColor(kBlack);
      // Add three TPad's for GetRunNumber/GetEventNumber()/GetDateTime/
#ifdef R__QT      
#ifdef CAN_RENDER_PAD_DIRECTLY    
      if (f3DViewer) 
         ((TQtRootViewer3D*)f3DViewer)->DisconnectPad();    
      f3DViewer = (TQtRootViewer3D*)TVirtualViewer3D::Viewer3D(0,"oiv");
      LOG_INFO << f3DViewer->ClassName() << endm;
      if (f3DViewer) {
         // Create Open GL viewer
         TGQt::SetCoinFlag(1);
         fCoin3DReady = kTRUE;
//  vf          f3DViewer->BeginScene(m_PadBrowserCanvas);
         f3DViewer->BeginScene(0);
// vf 16.03.2007          f3DViewer->EndScene();
#if 0         
         TGLViewerImp *viewerImp = f3DViewer->GetViewerImp();
         if (viewerImp) 
         {
         
            QObject::connect(&viewerImp->Signals(),SIGNAL( ObjectSelected(TObject *, const QPoint&))
                 , this, SLOT(ObjectSelected(TObject *, const QPoint &)));
            QObject::connect(&viewerImp->Signals(),SIGNAL(destroyed()), this, SLOT(Disconnect3DViewer()));
         }
#endif         
      }
#endif         
#endif      
   }

   char buffer[100];
   Int_t date  = GetDate();
   Int_t year  = date/10000;
   Int_t day   = (date - year*10000);
   Int_t month = day/100;
   day         = day  - month*100;

   Int_t time  = GetTime();
   Int_t hours = time/10000;
   Int_t sec   = (time - hours*10000);
   Int_t min   =  sec/100;
   sec         =  sec  - min*100;

   sprintf(buffer,"Event Display: Run=%d; Event=%d; Date=%d.%02d.%02d/%02d:%02d:%02d",
      GetRunNumber(),GetEventNumber(),year,month,day,hours,min,sec);

   m_PadBrowserCanvas->SetTitle(buffer);

   if (!m_ShortView) BuildGeometry();
   if (m_ShortView) { DrawObject(m_ShortView); }
   
   // Draw the Emc tower if present
   if (fSizeProvider) {
#ifdef STAR_COIN   
   if (f3DViewer && !fEmcTowers && fCoin3DReady) {
           fEmcTowers = new TCoinEmcTowers();
           fEmcTowers->SetColorProvider(fColorProvider);
           fEmcTowers->SetSizeProvider(fSizeProvider);
           f3DViewer ->AddRawObject(ULong_t(fEmcTowers->GetBarrel()),(unsigned int)TGLViewerImp::kRaw);

       }
       if (f3DViewer && fEmcTowers)  fEmcTowers->UpdateShape("");
#endif
   }
   if (!f3DViewer) {
      m_PadBrowserCanvas->Modified();
      m_PadBrowserCanvas->Update();
   }
   return m_PadBrowserCanvas;
}
//_____________________________________________________________________________
void StEventDisplayMaker::DrawObject(TObject *object,Option_t *option,Bool_t first)
{
  // Draw object directly to 3D viewer if present
  if (object) {
    if ( !f3DViewer) 
    {
        if (first && m_PadBrowserCanvas)
          m_PadBrowserCanvas->GetListOfPrimitives()->AddFirst( object );
        else {
           object->Draw(option);
        }
     }
    else 
    {
        if (fNeedsClear3DView) {
#ifdef CAN_RENDER_PAD_DIRECTLY           
           f3DViewer->BeginScene(0);
#endif           
           fNeedsClear3DView=kFALSE;
        }
        if (object->InheritsFrom(TCollection::Class())){
           TIter next((TCollection*)object);
           TObject *addMe  = 0;
           while ( (addMe = next())  ) DrawObject(addMe,option,first);
        } else {
#ifdef CAN_RENDER_PAD_DIRECTLY           
           if (first)
               f3DViewer ->AddObjectFirst(object, option);
           else
               f3DViewer ->AddObject(object, option);
           printf("object %s viewer %s \n", object->GetName(),f3DViewer->ClassName() );
#endif
        }
     }
  }
}
//_____________________________________________________________________________
Int_t StEventDisplayMaker::Make()
{

// AGAIN:
   fgEventLoop = -1;

   // Skip all events but one if needed
   if (     EventIdToRender() 
         && (EventIdToRender() != GetEventNumber())) 
   {
      return kStOk;
   }
//  const Int_t maxTrackCounter = 9;
  if (!m_EventsNode) return kStErr;
  if (m_Mode == 1)   return  kStOK;
  
  if (f3DViewer) f3DViewer->SetUpdatesEnabled(kFALSE);
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

#if 0  //VP
    if (!m_ListDataSetNames || m_ListDataSetNames->GetSize() == 0) 
    {
    // Add default names
      {
#if TPC
        TDataSet *dshits = GetDataSet("tphit");
        if (dshits)   {
           AddName("tphit(id_globtrk,x:y:z)");
           printf(" tphit found !!!\n");  
          ((TTable *)dshits)->Print(0,10);   
        }
         TDataSet *dstracks = GetDataSet("tptrack");
         if (dstracks) {
            AddName(dstracks->GetName());
            printf(" tptrack found !!!\n"); 
           ((TTable *)dstracks)->Print(0,1);
         }
#else
       printf(" Note: To get tracks colored with de/dx the dst_dedx table should be provided\n");
       printf("       as follows:\n\n");
       printf("       ds->AddName(\"dst/dst_dedx(id_track)\");\n\n");
       printf(" where: <ds> is a pointer to StEventDisplayMaker\n");
       printf(" The method above can be invboke via TBrowser context-menu\n");


       
#ifdef dedx_default
        const char *dedx2Refs = "dst/dst_dedx";
        TDataSet *dedx  = GetDataSet(dedx2Refs);
        {
           AddName("dst/dst_dedx(id_track)");
           if (Debug()) {
              printf(" dst_dedx found !!!\n");  
             ((TTable *)dedx)->Print(0,5);   
           }
        }
#endif
        TDataSet *dshits = GetDataSet("dst/point");
        // if (dshits)   
        {
           AddName("dst/point(id_track,position[0]:position[1]:charge)");
           if (Debug()) {
              printf(" tphit found !!!\n");  
             ((TTable *)dshits)->Print(0,10);   
           }
        }
        const char *track2Draw = "dst/primtrk";
        TDataSet *dstracks = GetDataSet(track2Draw);
        // if (dstracks) 
        {
           AddName(track2Draw);
           printf(" %s found !!!\n",track2Draw); 
           if (Debug()) {
             printf(" %s found !!!\n",track2Draw); 
            ((TTable *)dstracks)->Print(0,1);
           }
        }
        const char *vertex2Draw = "dst/vertex(vtx_id,x:y:z)";
        {
           AddName(vertex2Draw);
        }
        // printf(" no %s found !!!\n",track2Draw); 
#endif
      }
    }
#endif  //VP
    TIter nextNames(m_ListDataSetNames);
    TObjString *eventName = 0;
     while ( (eventName = (TObjString *)nextNames()) ) 
    {
      m_Table = 0;
  
      char *nextObjectName = StrDup(eventName->String().Data());
      char *positions[] = {0,0,0,0,0,0};
      Int_t type = ParseName(nextObjectName, positions);
      if (Debug()) { printf(" The type of the current parameter is %d \n", type); }
      if (!type) { delete [] nextObjectName; continue; }
      const char *foundName = positions[0];
      TDataSet *event = GetDataSet(foundName);
      if (!event) {
         if (Debug()) Warning("Make","No object \"%s\" found",foundName);
         continue;
      }
      if (event->InheritsFrom("TTable") && 
          (( type == 5) || (type == 1) || (type == 2)) ) 
      {
        //  ----- Draw "table" events -------------------------- //
          m_Table = (TTable *)event;                             //
          totalCounter += MakeTable((const char **)positions); //
        //  ---------------------------------------------------- //
      }
      else if (event->InheritsFrom("StEvent")) {
          totalCounter += MakeEvent(event,(const char **)positions); 
      }
      else if (Debug()) Warning("Make","Can not draw the object \"%s\"",nextObjectName); 
      delete [] nextObjectName;
     }
     if (totalCounter) {
       m_EventsView = new TVolumeView(*m_EventsNode);
       m_ShortView->Add(m_EventsView);
     }
     printf(" updating view of %d objects\n",totalCounter);
     if (!totalCounter) fEventNeedRescan=kFALSE;

     if (!f3DViewer) {
        m_PadBrowserCanvas->Modified();
        m_PadBrowserCanvas->Update();
     }
   }
#if 0
   printf("StEventDisplayMaker::EventLoop Started\n");
   int resLoop = MakeLoop(0);
   printf("StEventDisplayMaker::EventLoop FINISHED\n");
   switch (resLoop) {
     case 1: ClearCanvas(); goto AGAIN;     
     case 2: break;
     case 3: fgEventLoop = 0; return kStEOF;
   } 
#endif 
   fgEventLoop = 0;
   if (f3DViewer) {  f3DViewer->EndScene(); f3DViewer->SetUpdatesEnabled(); /*f3DViewer->Update();*/ }
   gSystem->DispatchOneEvent(1);
   if (  EventIdToRender() ) {
      return kStEOF;
   } else {
      return kStOK;
   }
}
//_____________________________________________________________________________
void  StEventDisplayMaker::MakeEmcTowers()
{
// add the fake EMC providers to test
//_____________________________________________________________________________
class TEmcSizeProvider : public TDataProvider {
protected :
     UInt_t   fScale;     // The normalization factor;
     Int_t    fIndex;     // current index;
     UShort_t fThreshold; // Min reported value  
     
     inline Int_t ReportValue(UShort_t val) 
     {
        return val;
//        return (val - fThreshold > 0 ? 0: Int_t((val - fThreshold)/fScale));
     }
public:
    //________________________________________________________________
    TEmcSizeProvider(UShort_t *src=0,unsigned char *available=0,UShort_t *len=0) : TDataProvider(src,available, len)
     , fScale(4095), fThreshold(200) { 
        fScale /= 100;
     }
    //________________________________________________________________
    virtual ~TEmcSizeProvider() {}
    //________________________________________________________________
    virtual Int_t Attribute(Int_t nSegments,Int_t nSectors) {
       if (fDataSource) {
          return  ReportValue (fDataSource[nSegments*120 + nSectors]);
      } else 
        return 0;
    }
    //________________________________________________________________
    virtual void ComputerScale() 
    {
      // Find the maximum
      fScale = 1; // 4095/10; /* Web page value */
    }
    //________________________________________________________________
    virtual Int_t NextAttribute() { 
//       Int_t  daqId = 0;
//       Int_t  tdc = -1;
       Int_t report = 0;
       if (fIndex > 4800) fIndex = 0;
       int towerId = fIndex+1;fIndex++;
       // printf(" towerid size attr = %d\n", towerId);
       if ( towerId > 61*40 && towerId <=  62*40)  return 2; // STAR has no East-end emc tower yet !!!
       if ( towerId > 72*40 && towerId <=  73*40)  return 31; // STAR has no East-end emc tower yet !!!
       if ( towerId > 103*40 && towerId <= 104*40) return 44; // STAR has no East-end emc tower yet !!!
       if ( towerId > 116*40 && towerId <= 117*40) return 49; // STAR has no East-end emc tower yet !!!

       if ( towerId > 1*40 && towerId <=   2*40)  return 2; // STAR has no East-end emc tower yet !!!
       if ( towerId > 28*40 && towerId <=  29*40) return 31; // STAR has no East-end emc tower yet !!!
       if ( towerId > 43*40 && towerId <=  44*40) return 44; // STAR has no East-end emc tower yet !!!
       if ( towerId > 58*40 && towerId <=  59*40) return 49; // STAR has no East-end emc tower yet !!!

       return  towerId/10;
       if (  towerId <= 1200)                        return  100; // 0  
       if ( (towerId > 2400) && ( towerId < 3600) )  return  100; // 0
       if ( (towerId >= 1200) && (towerId < 2400) )  return 20;
       else return 80;
       return report;
    }
    //________________________________________________________________
    virtual void ResetCounter()
    { 
      fIndex = 0;       
    }
    //________________________________________________________________
    inline  void SetScale( UInt_t  scale)  { fScale=scale;    }
    //________________________________________________________________
    inline  void SetThreshold( UInt_t  cut){ fThreshold =cut; }
    //________________________________________________________________
    inline  UInt_t GetScale()              { return fScale;   }
};


//_____________________________________________________________________________
class TEmcColorProvider  : public TEmcSizeProvider {
public:
    //________________________________________________________________
    TEmcColorProvider(UShort_t *src=0,unsigned char *available=0,UShort_t *len=0) 
    : TEmcSizeProvider (src,available,len)
    { }
    //________________________________________________________________
    virtual Int_t Attribute(Int_t nSegments,Int_t nSectors) {
       if (fDataSource) {
          return ReportValue( fDataSource[nSegments*120 + nSectors]);
      } else 
        return 0;
    }
    //________________________________________________________________
    virtual void ComputerScale() 
    {
      // Find the maximum
      fScale = 4095/1000;
    }
    //________________________________________________________________
    virtual Int_t NextAttribute() { 
//       Int_t  daqId = 0;
//       Int_t colorResponce = 0;
       if (fIndex > 4800) fIndex = 0;
       int towerId = fIndex+1;fIndex++;
       return  towerId/100;
       // printf(" towerid = %d\n", towerId);
       // UInt_t colorCode = ReportValue((fDataSource[daqId])) ;
       if ( towerId >  1*40 && towerId <=   2*40) return kBlue;    // STAR has no East-end emc tower yet !!!
       if ( towerId > 28*40 && towerId <=  29*40) return kGreen;   // STAR has no East-end emc tower yet !!!
       if ( towerId > 43*40 && towerId <=  44*40) return kYellow;  // STAR has no East-end emc tower yet !!!
       if ( towerId > 58*40 && towerId <=  59*40) return kRed;     // STAR has no East-end emc tower yet !!!

       if ( towerId >  61*40 && towerId <=  62*40)  return kBlue;  // STAR has no East-end emc tower yet !!!
       if ( towerId >  72*40 && towerId <=  73*40)  return kGreen; // STAR has no East-end emc tower yet !!!
       if ( towerId > 103*40 && towerId <= 104*40) return kYellow; // STAR has no East-end emc tower yet !!!
       if ( towerId > 116*40 && towerId <= 117*40) return kRed;    // STAR has no East-end emc tower yet !!!
       return kCyan; //0
    }
};
// -------------------
         fSizeProvider = new TEmcSizeProvider();
         fColorProvider = new TEmcColorProvider (); 
}

//_____________________________________________________________________________
Int_t StEventDisplayMaker::ParseName(char *inName, char *positions[])
{ 
  // returns  the number of the tokens found:
  //
  //              "1" - assuming StEvent name defined by position[0]
  //              "5" - assuming TTable columns definitions
  //              "0" - syntax error
  //  
  //  "name" - StEvent
  //  "g2t_tpc_hit(track_id,x[0]:x[1]:x[2])"
  //   Attention:     NO EXPRESSION, yet !!!
  //

  Int_t nParsed = 0;
  if (inName && inName[0]) {
    char *pos = 0;
    const char *errorMessages[] = {  "the open bracket missed"
                                     , "first comma missed"
                                     , "first collon missed"
                                     , "second collon missed"
                                     , "the closed bracket missed"
                                    };
    const Int_t lenExpr = sizeof(errorMessages)/sizeof(char *);
    const char *openBracket  = "(";
    const char *closeBracket = ")";
    const char *comma        = ",)";
    const char *collon       = ":";
    const char *delimiters[] = {openBracket,comma,collon,collon,closeBracket };
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
Color_t StEventDisplayMaker::GetColorAttribute(Int_t adc)
{
  // Convert the inpput signal amplitude into color index
//  return Color_t(10-(adc?TMath::Log2(adc):10));
//  return Color_t(adc?TMath::Log2(adc)+50:10);
  return Color_t(50 + (adc/256));
}

//_____________________________________________________________________________
Int_t StEventDisplayMaker::MakeTable(const char **positions)
{
  int nskip = 500;
  StVirtualEventFilter *filter = 0;
  Int_t tableCounter = 0;
//  StTrackChair &tracks = *StTrackChair::Instance(m_Table);
//  if (!(&tracks)) {
  if (StTrackChair::IsTrack(m_Table)==-1) {

     St_dst_point *dstp = (St_dst_point *)m_Table;
     int nrow = dstp->GetNRows();
     dst_point_st *dst = dstp->GetTable();
     int jrow = 0;
     for (int irow=0;irow<nrow;irow++) {
       if(!dst[irow].id_track)		continue;
       if(dst[irow].id_track%nskip) 	continue;
       if(irow!=jrow) dst[jrow]=dst[irow];
       jrow++;
     }
     dstp->SetNRows(jrow);

    filter = (StVirtualEventFilter *)m_FilterArray->At(kTable);
    if (!filter || filter->IsOn() ) {
       tableCounter += MakeTableHits(m_Table,filter,positions[1],&positions[2]);
       if (Debug()) printf(" TTable: %d \n", tableCounter);
    }
  }
  else {
     St_dst_track *dstt = (St_dst_track *)m_Table;
     int nrow = dstt->GetNRows();
     dst_track_st *dst = dstt->GetTable();
     int jrow = 0;
     for (int irow=0;irow<nrow;irow++) {
       if(!dst[irow].id) 		continue;
       if(dst[irow].id%nskip) 		continue;
       if(irow!=jrow) dst[jrow]=dst[irow];
       jrow++;
     }
     dstt->SetNRows(jrow);

    filter = (StVirtualEventFilter *)m_FilterArray->At(kTptTrack);
    if (!filter || filter->IsOn() ) {
       StTrackChair tracks(m_Table);
       tableCounter += MakeTableTracks(&tracks,filter);
       if (Debug()) printf(" TTable:tpc_Track %d \n", tableCounter);
    }
  }
  return tableCounter;
}
//_____________________________________________________________________________
Int_t StEventDisplayMaker::MakeEvent(const TObject *event, const char** pos)
{
//   static const Style_t UHitSty = 4; static const Size_t UHitSiz = 1.50; static const Color_t UHitCol=kBlue;
   static const Style_t UHitSty = 4; static const Size_t UHitSiz = 0.35; static const Color_t UHitCol=kBlue;
   static const Style_t NHitSty = 1; static const Size_t NHitSiz = 1.00; static const Color_t NHitCol=kGreen;
   static const Style_t TrakSty = 1; static const Size_t TrakSiz = 1.00; static const Color_t TrakCol=kRed;
   static const Style_t VertSty = 5; static const Size_t VertSiz = 3.50; static const Color_t VertCol=kBlack;

   TList garbage; garbage.SetOwner();

   if (VertCol){} // to supress the compilation warning


  if (!pos[1] || !pos[1][0]) return 1;
  memset(fColCash,0,sizeof(fColCash));

  if (!mEventHelper) mEventHelper = new StEventHelper;
  if (!mRedraw || fEventNeedRescan) {
     mEventHelper->Reset(event,"StL3.*");
     fEventNeedRescan=kFALSE;
  }
  mEventHelper->ls();

  int keyLen = strchr(pos[1],' ') - pos[1];
  int kase=0;
  const char *word=0;
  for (int i=1;(word=EHKindN[i]);i++) {
    if (strstr(pos[1],word)) kase |= EHKindS[i];
  }
  
  LOG_DEBUG << " Current selector \"kase\"= " << kase << endm;
  
  TString sel("^StSPtrVec");

  Style_t defSty=0; Size_t defSiz = 0; Color_t defCol= 0;
  TObjArray *stevs =0,*stevz=0;


  switch (kase) {


    case kHIT|kUNU:;
    case kHIT|kUSE:;
//      if (kase&kALL) {sel +=".*";}
//      else           {sel.Append(pos[1],keyLen);} 
      sel.Append(pos[1],keyLen); 
      sel += ".*Hit$";
      stevs = mEventHelper->SelHits  (sel.Data(),kase, (IsMarkedDrawn()? kMark2Draw : 0));
      sel.ReplaceAll(".*Hit",".*Point");
      stevz = mEventHelper->SelHits  (sel.Data(),kase, (IsMarkedDrawn() ? kMark2Draw : 0) );
      if (!stevs){ stevs = stevz; break;}
      if ( stevz){ stevs->AddAll(stevz); delete stevz;}
      break;

    case kTRK|kTGB:;
    case kTRK|kEGB:;
    case kTRK|kTPT:;
    case kTRK|kTGB|kHIT|kTHT:;
    case kTRK|kEGB|kHIT|kTHT:;
    case kTRK|kTPT|kHIT|kTHT:;
    case      kTGB|kHIT|kTHT:;
    case      kEGB|kHIT|kTHT:;
    case      kTPT|kHIT|kTHT:;
      if (kase&kHIT) kase |=kHRR;
      stevz = mEventHelper->SelTracks("",kase || (IsMarkedDrawn()?kMark2Draw : 0));
      stevs = mEventHelper->ExpandAndFilter(stevz,-1);
      break;

    case kPRM|kTRK:;
    case kPRM|kTHT|kHIT:;
    case kKNK|kTRK:;
    case kKNK|kTHT|kHIT:;
    case kV0 |kTRK:;
    case kV0 |kTHT|kHIT:;
    case kXI |kTRK:;
    case kXI |kTHT|kHIT:;
      if (kase&kHIT) kase |=kHRR;
      sel.Append(pos[1],keyLen); sel +="Vertex$";
      stevz = mEventHelper->SelVertex(sel.Data(),kase || (IsMarkedDrawn()?kMark2Draw : 0) );
      stevs = mEventHelper->ExpandAndFilter(stevz,-1);
      delete stevz;
      break;

     default: 
     Error("MakeEvent","Wrong case %o %s\n",kase,pos[1]);
     for (int i=1;(word=EHKindN[i]);i++){if (kase&EHKindS[i]) printf("|%s",word);}
     printf("\n");  
     assert(0);
  }


  if (!stevs) {
     fEventNeedRescan=kTRUE;
     return 0;
  }
  garbage.Add(stevs);
  TListIter nextGilter(StGlobalFilterABC::GetList());
  StGlobalFilterABC *gilt=0;
  while ((gilt=(StGlobalFilterABC*)nextGilter())) {
    if (!gilt->IsActive()) continue;
    gilt->SetEvent( GetRunNumber(),GetEventNumber());
    gilt->Filter(stevs,kase);
  }
  TObjArray *points=mEventHelper->MakePoints(stevs,kase);
  stevs=0;
  Int_t trackCounter = 0;

  if (!points ) {
//    fEventNeedRescan=kFALSE;
    return 0;
  }
//  garbage.Add(points);
  int ntrk = points->GetLast()+1;
  if (!ntrk ) return 0;

  TListIter nextFilter(mFilterList);
  int ncut = 0;
  Color_t col = kRed;
  StFilterABC *filt=0;
  Style_t sty; Size_t siz;
  StPoints3DABC *pnt;
  const char *L;

  for (int i = 0; i < ntrk; i++ )
  {
    pnt = (StPoints3DABC*)points->At(i);
    if (!pnt->Size()) continue;
 
    int kind = pnt->Kind();
    L = "P";
    if (kind==3 && (kase&kUSE || pnt->Size()>1)) kind=9;
    switch (kind) {
      case 1:  defSty = VertSty; defSiz = VertSiz  ; defCol = VertCol;        break;
      case 2:  defSty = TrakSty; defSiz = TrakSiz  ; defCol = TrakCol; L="L"; break;
      case 3:  defSty = NHitSty; defSiz = NHitSiz  ; defCol = NHitCol;        break;
//vf      case 4:  defSty = VertSty; defSiz = UHitSiz  ; defCol = VertCol;        break;
//vf      case 5:  defSty = VertSty; defSiz = UHitSiz  ; defCol = VertCol;        break;
      case 4:  defSty = VertSty; defSiz = UHitSiz*2; defCol = VertCol;        break;
      case 5:  defSty = VertSty; defSiz = UHitSiz*2; defCol = VertCol;        break;
      case 9:  defSty = UHitSty; defSiz = UHitSiz  ; defCol = UHitCol;	      break;
    }

    col = pnt->GetUniqueID();
    if (!col) col = defCol;
    siz = defSiz;sty=defSty;
    //		Filtration
    nextFilter.Reset();
    while ((filt=(StFilterABC*)nextFilter())) {
      if (!filt->Active()) 			continue;
      if (!filt->Accept(pnt,col,siz,sty)) 	break;
    }
    if (filt) {ncut++; continue;}

//  Draw it
    DrawIt(pnt,L,col,sty,siz);
    trackCounter++;
  }
  printf(" %d objects was filtered out\n",ncut);
  return trackCounter;
}
//_____________________________________________________________________________
void  StEventDisplayMaker::DrawIt(StPoints3DABC *pnt,const char *opt
                                 ,Color_t col,Style_t sty,Size_t siz)
{
    m_TrackCollector->Add(pnt);		//collect garbage
      TPolyLineShape *tracksShape=new TPolyLineShape(pnt,opt);
         // Set visual attributes
         tracksShape->SetVisibility(1);
         tracksShape->SetColorAttribute(col);
         tracksShape->SetStyleAttribute(sty);
         tracksShape->SetSizeAttribute((Size_t)siz);
         tracksShape->SetName (pnt->GetName());
         tracksShape->SetTitle(pnt->GetTitle());
         // Draw this group
         //if (!opt || (opt[0] != 'L')) {
         // fprintf(stderr,"  StEventDisplayMaker::DrawIt col = %d sty=%d size=%d \n",
         // col,sty,siz);
         // }
     // Make sure the event goes first
     // tracksShape->Draw();
      if ( !f3DViewer) {
        m_PadBrowserCanvas->GetListOfPrimitives()->AddFirst( tracksShape );
        m_PadBrowserCanvas->Modified();
     } else {
        DrawObject( tracksShape );
     }
    m_TrackCollector->Add(tracksShape);
}
//_____________________________________________________________________________
Int_t StEventDisplayMaker::MakeTableTracks(const StTrackChair *points,StVirtualEventFilter *filter)
{
  Int_t i = 0;
  Int_t trackCounter = 0;
  Int_t nRows = 0;
  if (points && (nRows = points->GetNRows()) ) {
    // Get the sign of the magnetic field
    St_dst_event_summary *summary = (St_dst_event_summary *)GetDataSet("dst/event_summary");
    float bField = -1;
    if (summary) bField = summary->GetTable()->field;
    for (i = 0; i < nRows; i++ ){
      Color_t trackColor = kRed;
      Style_t trackStyle = 1;
      Size_t trackSize   = 1;
      filter = (StVirtualEventFilter *)m_FilterArray->At(kTptTrack);
      if (!filter || filter->IsOn() ) {
        // ------------------------------ tracks filter ------------------------------------ //
        if (filter) trackColor =  filter->Channel(points->Table(),i,trackSize,trackStyle);//
        // ----------------------------------------------------------------------------------//
        if (trackColor > 0) {
           StHelixD *helix  = points->MakeHelix(i,bField);
           Float_t      len = points->Length(i);
    	   Int_t nSteps = Int_t(28*len*points->Curvature(i) + 1); 
	       Float_t step = len / nSteps;
           StHelix3DPoints *tracksPoints  = new StHelix3DPoints(helix,step,nSteps);
           m_TrackCollector->Add(tracksPoints);    // Collect to remove  
           TPolyLineShape *tracksShape   = new TPolyLineShape(tracksPoints,"L");
             tracksShape->SetVisibility(1);         tracksShape->SetColorAttribute(trackColor);
             tracksShape->SetLineStyle(trackStyle); tracksShape->SetSizeAttribute(trackSize);
           // Create a node to hold it
           TVolume *thisTrack = new TVolume("tracks",points->GetName(),tracksShape);
             thisTrack->Mark();   thisTrack->SetVisibility();
             trackCounter++;
           TVolumePosition *pp = m_EventsNode->Add(thisTrack); 
           if (!pp && trackCounter) {
              printf(" no track position %d\n",trackCounter);
           }
        }
        else if (trackColor == -1) break;
      }
    }
  }
  return trackCounter;
}
//_____________________________________________________________________________
Int_t StEventDisplayMaker::MakeTableHits(const TTable *points,StVirtualEventFilter *filter
                                        ,const char *keyColumn,const char *keyPositions[]
)
{
  Int_t totalHits = 0;
     
  TTable &ttt = *((TTable *)points);
  TString tr = keyColumn; 
  const char *packedList[] = {"dst_point_st"};
  const Int_t lPackedList = sizeof(packedList)/sizeof(char *);
  Bool_t packed = kFALSE;
  Int_t i = 0;
  for (i = 0; i< lPackedList && !packed ;i++) {
    if (!strcmp(ttt.GetType(),packedList[i])) packed = kTRUE;
  }
  if (ttt.GetNRows() ) {
    TTableSorter *track2Line = new TTableSorter (ttt,tr);
    m_TableCollector->Add(track2Line);    // Collect to remove  
    i = 0;
    Int_t nextKeyIndx = 0;
    Int_t maxTrackCounter = track2Line->CountKeys();
    for (i=0;i<maxTrackCounter;i++) 
    { 
       Color_t hitColor = kGreen;
       Style_t hitStyle = packed ?   8 : 5;
       Size_t  hitSize  = packed ? 0.6 : 0.9;
       // -------------------------- hits filter -------------------------------------- //
       if (filter) hitColor =  filter->Channel(track2Line,nextKeyIndx,hitSize,hitStyle);//
       if (filter->IsOff() ) break;                                                     //
       // ----------------------------------------------------------------------------- //
       if (hitColor > 0 && keyPositions[0]) {
         TTable3Points *hitsPoints = 0;
         if (packed)  
             hitsPoints = new St_Table3PackedPoints(track2Line,
                                              nextKeyIndx,
                                              keyPositions[0]);
         else if (keyPositions[1] && keyPositions[2])
             hitsPoints = new TTable3Points(track2Line,
                                              nextKeyIndx,
                                              keyPositions[0],keyPositions[1],keyPositions[2]);
         else { hitColor = -1; break; }
         if (hitsPoints) {
           m_HitCollector->Add(hitsPoints);    // Collect to remove  
           TPolyLineShape *hitsShape   = new TPolyLineShape(hitsPoints);
           hitsShape->SetVisibility(1);            hitsShape->SetColorAttribute(hitColor);
           hitsShape->SetStyleAttribute(hitStyle); hitsShape->SetSizeAttribute(hitSize);
           // Create a node to hold it
           TVolume *thisHit = new TVolume("tableHits",points->GetName(),hitsShape);
             thisHit->Mark();
             thisHit->SetVisibility();
           TVolumePosition *pp = m_EventsNode->Add(thisHit); 
           Int_t s = hitsPoints->Size();
           totalHits   += s;
           nextKeyIndx += s; 
           if (!pp && totalHits) printf(" no track position %d\n",totalHits);
         }
       }
       else if (hitColor == -1) break;
       else {
         const void *newID = track2Line->GetKeyAddress(nextKeyIndx);
         nextKeyIndx      += track2Line->CountKey(newID,nextKeyIndx,kFALSE); 
       }
    }
  }
  return totalHits;
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
  const char *filterNames[] = {
                                   "Primary Vertex"
                                 , "Tpc Hit"
                                 , "Svt Hit"
                                 , "Ftpc Hit"
                                 , "EmcTower Hit"
                                 , "EmcPreShower Hit"
                                 , "SmdPhi Hit"
                                 , "SmdEta Hit"
                                 , "Vertices"
                                 , "Global Tracks"
                                 , "Track"
                                 , "Track Tpc Hits"
                                 , "Track Svt Hits"
                                 , "Track Ftpc Hits" 
                                 , "TTable generic object" 
                                 , "tpt_track table object" 
                                } ;
   
  Int_t i;
  for (i =0;i<kEndOfEventList;i++) {
      StVirtualEventFilter *filter = (StVirtualEventFilter *)m_FilterArray->At(i);
      printf(" Filter for %16s",filterNames[i]);
      if (filter) {
          if (filter->IsOn()) printf(" is ON\n");
          else printf(" is       OFF\n");
      }
      else printf(" doesn't exist\n");
  }
}
//_____________________________________________________________________________
Int_t StEventDisplayMaker::MakeLoop(Int_t flag)
{
//   	fgEventLoop == 0  : out of 		StEventDisplayMaker::Make
//   	fgEventLoop == -1 : processing of 	StEventDisplayMaker::Make
//   	fgEventLoop == -2 : waiting in loop 	StEventDisplayMaker::Make
//      flag == 0           start loop
//      flag == 1           redraw
//      flag == 2           next event
//      flag == 3           exit of chain 
//
   const char* infos[] = {"Start loop","Redrawing...","Next event","Chain Exit",0};


   if (fgEventLoop == -2) { //We are in loop
     if (flag >0 || flag <=3) fgEventLoop = flag;
     return 0;
   }

   if (fgEventLoop == -1 && flag == 0) {
     fgEventLoop=-2;
     MakeInfo("Waiting...");
     while (fgEventLoop==-2)                              {gSystem->DispatchOneEvent(1);}
     MakeInfo("Waiting finished");
     int ans = fgEventLoop; fgEventLoop=-1;
     MakeInfo(infos[ans]);
     return ans;
   }

   if (fgEventLoop == 0) {
     if(fgStChain==0) return 0;
     StEventDisplayMaker *edMk = (StEventDisplayMaker*)fgStChain->GetMaker("EventDisplay");
     if (edMk==0) return 0;
     switch(flag) {
       case 1: MakeInfo("Redrawing...");
               edMk->ReDraw(); 
               MakeInfo("Redrawing finished");
               break;
       case 2: MakeInfo("Next event, please!"); 
               fgStChain->Clear(); 
               fgStChain->Make(); 
               MakeInfo("Next event done");
               break;
     }
     return 0;
   }
  return 2001;
}   
//_____________________________________________________________________________
void StEventDisplayMaker::MakeInfo(const char *info)
{
  if (!fgInfo) new StEventDisplayInfo(&fgInfo," Info ");
  fgInfo->SetText(info);
  fgInfo->Popup();
}

#define DISPLAY_FILTER_DEFINITION(filterName)                                \
Int_t  StEventDisplayMaker::_NAME3_(Set,filterName,Flag)(Int_t flag)         \
{ return SetFlag(flag,_NAME2_(k,filterName)); }                              \
StVirtualEventFilter *StEventDisplayMaker::_NAME2_(Set,filterName)(StVirtualEventFilter *filter) \
{ return SetFilter(filter,_NAME2_(k,filterName)); }


// -- Generic TTable  filters --

DISPLAY_FILTER_DEFINITION(Table)

// -- Tpt track  filter --

DISPLAY_FILTER_DEFINITION(TptTrack)

// --  end of filter list --

//_____________________________________________________________________________
// $Log: StEventDisplayMaker.cxx,v $
// Revision 1.129  2007/11/14 22:42:16  fine
// Add GEANT volume to represent SSD
//
// Revision 1.128  2007/09/01 02:25:57  fine
// introduce Event selection to fix isseu #1051
//
// Revision 1.127  2007/09/01 01:42:22  fine
// Quick fix to allow selection one event by event id
//
// Revision 1.126  2007/03/19 14:35:45  fine
// Remove  the obsolete refs to TEmcTower class
//
// Revision 1.125  2007/03/19 00:40:36  fine
// Complete the direct rendering and Emc Towers
//
// Revision 1.124  2007/03/15 16:27:29  fine
//  Allow user to select the arbitrary StEvent object to be drawn with StEventDisplay
//
// Revision 1.123  2007/02/22 03:51:06  fine
// Rescan event if empty
//
// Revision 1.122  2007/02/22 02:43:50  fine
// Activate G3/Sti geometry toggling
//
// Revision 1.121  2007/02/21 19:16:14  fine
// Add an extra control to choose between G3 and Sti detectors geometries
//
// Revision 1.120  2007/02/01 22:41:00  fine
// Add Sti geometry to the Event Display
//
// Revision 1.119  2006/12/22 00:54:27  fine
// Add Qt env test
//
// Revision 1.118  2006/11/30 23:03:02  fine
// replace the obsolete RHIC detecor button with the RnD
//
// Revision 1.117  2006/11/13 05:13:30  fine
// remove the compilation warnings
//
// Revision 1.116  2006/10/09 20:33:46  fine
// Fix to make it work under ROOT 4.04 and ROOT 5.12
//
// Revision 1.115  2006/08/24 23:38:21  fine
// Add the EmsTowers a component of the Detector Geometry (still disabled
//
// Revision 1.114  2006/08/24 19:03:43  fine
// Add the fake the Emc tower dataprovider to test
//
// Revision 1.113  2006/08/10 03:21:38  perev
// Assert==>assert
//
// Revision 1.112  2006/07/26 00:17:40  fine
// fix the non-initialized data-member. Thank Manuel
//
// Revision 1.111  2005/07/20 21:26:44  perev
// qqqqq
//
// Revision 1.110  2005/07/19 19:22:59  fine
// small adjustment (trakc always go first)  to make use of the trancluent OpenGL features
//
// Revision 1.109  2005/06/16 19:38:25  perev
// A lot of changes
//
// Revision 1.108  2004/11/18 22:27:02  fine
// clean up. Provide the object infor via class NAme and Title
//
// Revision 1.107  2004/11/16 04:33:05  perev
// Bug fix. Check for same value of StEvent pointer removed
//
// Revision 1.106  2004/11/09 19:40:00  fine
// Set the filter invokation after the default parameters set
//
// Revision 1.105  2004/11/08 20:12:11  fine
// Remove the Victor workaround for Global tracks
//
// Revision 1.104  2004/10/17 03:37:19  perev
// A lot of improvements
//
// Revision 1.103  2004/10/07 19:41:23  perev
// Tuning
//
// Revision 1.102  2004/09/28 03:55:23  perev
// Global filter introduced
//
// Revision 1.101  2004/07/29 20:35:37  fine
// Fix priblem with Emv his and default hit color
//
// Revision 1.100  2004/07/27 00:34:43  perev
// StrangeBugFix
//
// Revision 1.99  2004/07/21 00:56:06  fine
// Chnage the abstract custom filter interface to allow use the custom visual attributes
//
// Revision 1.98  2004/01/26 22:57:10  perev
// WarnOff
//
// Revision 1.97  2003/10/28 20:21:13  fine
// Adjust the maker size
//
// Revision 1.96  2003/10/28 06:07:30  fine
// workaround to make the new viewer happy
//
// Revision 1.95  2003/10/10 17:41:51  fine
// change the default view for HALL to unvisible
//
// Revision 1.94  2003/09/02 17:58:08  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.93  2003/02/28 00:33:54  fine
// Exchange Accept and AccpetCB methods
//
// Revision 1.92  2003/01/26 23:54:36  fine
// Add RStiostream.h header lost due removing the redundant headers files from StEvent package. Redundant iostream was removed
//
// Revision 1.91  2003/01/24 21:14:42  fine
// remove the redundant include file
//
// Revision 1.90  2003/01/22 15:42:05  fine
// fix compilation prpblem with no Qt
//
// Revision 1.89  2003/01/21 23:49:50  fine
// The all knowm problems have been fixed
//
// Revision 1.88  2003/01/17 02:19:40  fine
// Some clean up
//
// Revision 1.87  2003/01/17 01:49:41  fine
// add right  named class StPadControlPanel
//
// Revision 1.86  2003/01/17 01:36:16  fine
// working version of Qt-based StEventDisplayMaker class
//
// Revision 1.84  2002/12/19 01:21:45  fine
// CPP condition to use Qt staff has been introduced
//
// Revision 1.83  2002/12/13 00:47:40  fine
// first version with Qt interface
//
// Revision 1.82  2002/04/14 21:58:15  perev
// Increase size of hit 10 times
//
// Revision 1.81  2002/01/16 04:01:52  perev
// L3 tracks removed
//
// Revision 1.80  2001/11/01 00:45:31  fine
// const ** cast to avoid cv-qual warning introduced
//
// Revision 1.79  2001/09/26 23:26:56  perev
// Sorting by color added
//
// Revision 1.78  2001/09/17 21:31:53  jeromel
// Compiler Kaboum on syntax (label vs switch()). Slightly modified and now works.
//
// Revision 1.77  2001/09/17 00:06:11  perev
// Two column
//
// Revision 1.76  2001/09/01 19:51:23  perev
// StEvent added
//
// Revision 1.75  2001/07/30 19:29:33  fine
// Add vertex point into the default view
//
// Revision 1.74  2001/07/27 22:08:58  fine
// Fix StEventDisplay maker to count the Mag field sign properly
//
// Revision 1.73  2000/09/12 20:56:16  fine
// typo fixed
//
// Revision 1.72  2000/09/06 21:57:52  fine
// Dynamic color axis
//
// Revision 1.71  2000/08/29 19:26:00  fine
// New method to add/remove volumes and tables
//
// Revision 1.70  2000/08/29 04:39:22  fine
// RemoveName method introduced
//
// Revision 1.69  2000/08/27 16:55:09  fine
// Title with Run event number etc
//
// Revision 1.68  2000/08/26 03:14:45  fine
// New default filter from M.Panebratcev has been introduced
//
// Revision 1.67  2000/08/16 22:34:52  fine
// clean up
//
// Revision 1.66  2000/08/16 20:50:55  fine
// CTB object has been added to the list of the volumes to draw
//
// Revision 1.65  2000/08/15 23:15:45  fine
// globtrk has been replaced with primtrk for default views
//
// Revision 1.64  2000/08/15 22:17:27  fine
// New defaults to draw globtrk and point
//
// Revision 1.63  2000/08/04 21:03:43  perev
// Leaks + Clear() cleanup
//
// Revision 1.62  2000/07/21 15:50:19  fine
// Bug fix: needs some correction in ROOT/star as well
//
// Revision 1.61  2000/07/03 02:07:47  perev
// StEvent: vector<TObject*>
//
// Revision 1.60  2000/05/03 01:19:42  fine
// emc geometry has been introduced
//
// Revision 1.59  2000/04/26 05:07:35  fine
// buildgeometry method adjusted to new version of TVolumeView ctor
//
// Revision 1.58  2000/04/22 22:53:25  fine
//  new schema to build  the detector geometry based on new ROOT 2.24
//
// Revision 1.57  2000/04/22 20:01:17  fine
// replace St_Table with TTable
//
// Revision 1.56  2000/04/10 20:11:03  fine
// change the default valume fro SVT from SLDI to STSI
//
// Revision 1.55  2000/04/05 03:58:20  fine
// Adjusted for ROOT 2.24
//
// Revision 1.54  2000/03/15 17:22:19  fine
// some extra protection against of dead canvas
//
// Revision 1.53  2000/03/09 22:00:33  fine
// StTrackChair class to draw any track-like table: tpt_track, dst_track has been introduced
//
// Revision 1.52  2000/01/30 02:00:31  fine
// Some adjustment to new ROOT (2.23/11
//
// Revision 1.51  2000/01/24 22:56:46  fine
// new packing schema for ssd introduced
//
// Revision 1.50  1999/12/29 18:56:44  fine
// Change the default filter settings in favour of  StTable
//
// Revision 1.49  1999/12/27 21:45:45  fine
// Protection against of zero-pointer
//
// Revision 1.48  1999/12/22 15:27:34  fine
// Protection against of double names
//
// Revision 1.47  1999/12/21 18:58:15  fine
// new default for SizeAttributes
//
// Revision 1.46  1999/12/20 20:28:52  fine
// adjust some parameteres
//
// Revision 1.45  1999/12/19 00:12:45  fine
// some corrections for the packed tables
//
// Revision 1.44  1999/12/17 23:28:45  fine
// clean up for the sake of docs + new class St_Table3DPackedPoints introduced
//
// Revision 1.43  1999/12/12 01:07:22  fine
// remove the compilation warnings
//
// Revision 1.42  1999/11/24 19:01:15  fine
// all StVirtual::Filter have been renamed to Channel
//
// Revision 1.41  1999/11/24 16:26:27  fine
// Overlap the corrupted v.1.40 with 1.39
//
// Revision 1.39  1999/11/24 03:06:46  fine
// Improved Helix parameters and parser
//
// Revision 1.38  1999/11/22 18:41:45  fine
// Parser for single parameter fixed
//
// Revision 1.37  1999/11/22 16:14:31  fine
// palette() has been removed
//
// Revision 1.36  1999/11/21 00:56:58  fine
// SVT volume name typo fixed
//
// Revision 1.35  1999/11/16 14:40:15  fine
// reference type has been introduced
//
// Revision 1.34  1999/11/12 18:12:37  fine
// SVT view has been introduced
//
// Revision 1.33  1999/11/12 05:27:32  fine
// phase parameter for  StHelix fixed. Thanks  Wensheng and Thomas
//
// Revision 1.32  1999/11/11 17:29:10  fine
// typo Filer replaced with Filter
//
// Revision 1.31  1999/11/10 02:24:36  fine
// StVirtualFilter::Reset method has been introduced
//
// Revision 1.30  1999/11/09 22:47:39  fine
// psi angle was forgotten to be converted from degrees to rad.
//
// Revision 1.29  1999/11/08 17:47:54  fine
// The bug for tpt_track filtering was fixed
//
// Revision 1.28  1999/11/07 05:27:12  fine
// Take in account new data-member of tpt_track table: length
//
// Revision 1.27  1999/11/05 23:18:40  fine
// convertor degree to rad was introduced
//
// Revision 1.26  1999/11/05 02:29:25  fine
// helix drawing for tpt_track table improved
//
// Revision 1.25  1999/11/04 17:59:59  fine
//  several bugs fixed to draw table-objects
//
// Revision 1.24  1999/11/04 01:49:47  fine
// tpt_track drawing is turned On
//
// Revision 1.23  1999/11/02 01:49:26  fine
// A special case for tpt_track table has been introduced
//
// Revision 1.22  1999/10/14 13:42:14  fine
// Some big to draw tables have been fixed
//
// Revision 1.21  1999/10/09 18:17:10  fine
// Some correction to draw tptrack table
//
// Revision 1.20  1999/08/16 16:28:09  fine
// StVirtualEventFilter has been moved to St_base
//
// Revision 1.19  1999/08/09 01:36:47  fine
// MakeTable methods have been activated
//
// Revision 1.18  1999/08/07 20:31:22  fine
// MakeVertex method has been introduced
//
// Revision 1.17  1999/08/05 02:41:12  fine
//  fix problem with the hits attributes
//
// Revision 1.16  1999/08/05 02:14:09  fine
//  style attribute for hits has been fixed
//
// Revision 1.15  1999/08/04 03:52:01  fine
// Helix drawing improvements
//
// Revision 1.14  1999/08/03 19:18:37  fine
// Vertices collections have been introduced
//
// Revision 1.13  1999/08/03 14:50:02  fine
// Clean up
//
// Revision 1.12  1999/08/02 14:43:25  fine
// Vertices collection has been introduced, more simple geometry
//
// Revision 1.11  1999/08/02 02:21:51  fine
// new method ParseName has been introduced,but not activated yet
//_____________________________________________________________________________
