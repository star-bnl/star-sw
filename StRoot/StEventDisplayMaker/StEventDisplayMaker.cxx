//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
// $Id: StEventDisplayMaker.cxx,v 1.82 2002/04/14 21:58:15 perev Exp $

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
#include "TRootHelpDialog.h"

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
#include "TTable3Points.h"
#include "St_Table3PackedPoints.h"
#include "StVirtualEventFilter.h"
#include "TTable.h"
#include "TTableSorter.h"
#include "tables/St_tpt_track_Table.h"
#include "tables/St_dst_event_summary_Table.h"

#include "StTrackChair.h"

#include "StArray.h"
#include "StEvent.h"
#include "StDefaultFilter.h"
#include "StEventHelper.h"

// StVirtualEventFilter hitsOffFilter(0);
// StVirtualEventFilter hitsOnFilter(1);
// StVirtualEventFilter StEventDisplayMaker::m_DefaultFilters[StEventDisplayMaker::kEndOfEventList];
StDefaultFilter StEventDisplayMaker::m_DefaultFilters[StEventDisplayMaker::kEndOfEventList];

Int_t               StEventDisplayMaker::fgEventLoop = 0;
StEventDisplayInfo *StEventDisplayMaker::fgInfo      = 0;




ClassImp(StEventDisplayInfo)

//_____________________________________________________________________________
//
//                         StEventDisplayMaker
//_____________________________________________________________________________

ClassImp(StEventDisplayMaker)
//_____________________________________________________________________________
StEventDisplayMaker::StEventDisplayMaker(const char *name):StMaker(name)
{
  mEventHelper    =  0;
  m_Hall          =  0;  
  m_FullView      =  0;  
  m_ShortView     =  0; 
  m_Sensible      =  0; 
  m_EventsNode    =  0;

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
  memset(fColCash,0,kCOLORS*sizeof(void*));
  
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

  gROOT->LoadMacro("EventControlPanel.C");
  gSystem->DispatchOneEvent(1);
  gROOT->LoadMacro("PadControlPanel.C"  );
  gSystem->DispatchOneEvent(1);


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
Int_t StEventDisplayMaker::BuildGeometry()
{
  // Create STAR sub-detector definition

  m_Hall = (TVolume *)GetDataSet("HALL");
  if (!m_Hall) return kStErr;
  //
  // Create an iterator to navigate STAR geometry
  TDataSetIter volume(m_Hall,0);
// ---  Create "standard" TPC and SVT views ----
  TVolume *sector = 0;
  while ( (sector = ( TVolume *)volume()) ){
    Bool_t found = kFALSE;
    found = (m_VolumeList && m_VolumeList->FindObject(sector->GetName()));
    if (found) {
      sector->SetVisibility(TVolume::kBothVisible);
      sector->Mark();
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
  m_Hall->SetVisibility(TVolume::kBothVisible);
  m_ShortView = new TVolumeView(*m_Hall,2); 
//  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/HitsDrawFullView.gif"> </P> End_Html // 
//    if (strcmp(tpssNode->GetName(),"TPGV") && strcmp(tpssNode->GetName(),"TPSS")) continue;
  return 0;
}

//______________________________________________________________________________
void StEventDisplayMaker::AddName(const char *name)
{
  //  "name" - StEvent
  //  "g2t_tpc_hit(track_id,x[0]:x[1]:x[2])"
  //   Attention:     NO EXPRESSION, yet !!!

  if (!m_ListDataSetNames) m_ListDataSetNames = new TList;
  if (!m_ListDataSetNames->FindObject(name)) 
        m_ListDataSetNames->Add(new TObjString(name));
  if (strncmp(name,"StEvent",7)==0) {
    gROOT->ProcessLine("__StEventControlPanel__.Refresh();");
    gSystem->DispatchOneEvent(1);
  }
}
//______________________________________________________________________________
void StEventDisplayMaker::AddFilter(StFilterABC* filt)
{
    if (!mFilterList) mFilterList = new TList;
    if (mFilterList->FindObject(filt->GetName())) return;
    mFilterList->Add(filt);
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
  if (m_EventsNode) {
    if (!GetEventPad()) CreateCanvas();
    else                m_PadBrowserCanvas->Clear();

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
  if (m_PadBrowserCanvas) {
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
    m_PadBrowserCanvas->SetFillColor(kBlack);
    // Add three TPad's for GetRunNumber/GetEventNumber()/GetDateTime/

#if 0
     TPad *newpad = new TPad("RunNumber","RunNumber",0.02,0.9,0.2,0.96);
     newpad->Draw();
     newpad->cd();
     mRunNumberLabel = new TPaveLabel(0.01,0.01,0.99,0.99,"RunNumber");
     mRunNumberLabel->Draw();
     m_PadBrowserCanvas->cd();

     newpad = new TPad("EventNumber","EventNumber",0.02,0.9,0.2,0.96);
     newpad->Draw();
     newpad->cd();
     mEventNumberLabel = new TPaveLabel(0.01,0.01,0.99,0.99,"EventNumber");
     mEventNumberLabel->Draw();
     m_PadBrowserCanvas->cd();

     newpad = new TPad("DateTime","DateTime",0.02,0.9,0.2,0.96);
     newpad->Draw();
     newpad->cd();
     mDateTimeLabel = new TPaveLabel(0.01,0.01,0.99,0.99,"DateTime");
     mDateTimeLabel->Draw();
     m_PadBrowserCanvas->cd();
#endif
  }
  
   char buffer[100];
#if 0
   sprintf(buffer,"%d",GetRunNumber());
   mRunNumberLabel->SetLabel(buffer);
   printf(" GetRunNumber %s ;",buffer);

   sprintf(buffer,"%d",GetEventNumber());
   printf(" GetEventNumber %s ;",buffer);
   mEventNumberLabel->SetLabel(buffer);
 
   sprintf(buffer,"%d/%d",GetDate(),GetTime());
   printf(" DateTIme %s \n;",buffer);
   mDateTimeLabel->SetLabel(buffer);
#endif
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

  if (m_ShortView) m_ShortView->Draw();
  m_PadBrowserCanvas->Modified();
  m_PadBrowserCanvas->Update();
  return m_PadBrowserCanvas;
}



//_____________________________________________________________________________
Int_t StEventDisplayMaker::Make()
{

AGAIN: fgEventLoop = -1;


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
     m_PadBrowserCanvas->Modified();
     m_PadBrowserCanvas->Update();
   }

   printf("StEventDisplayMaker::EventLoop Started\n");
   int resLoop = MakeLoop(0);
   printf("StEventDisplayMaker::EventLoop FINISHED\n");
   switch (resLoop) {
     case 1: ClearCanvas(); goto AGAIN;     
     case 2: break;
     case 3: fgEventLoop = 0; return kStEOF;
   } 
   fgEventLoop = 0;
   return kStOK;
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
  StVirtualEventFilter *filter = 0;
  Int_t tableCounter = 0;
//  StTrackChair &tracks = *StTrackChair::Instance(m_Table);
//  if (!(&tracks)) {
  if (StTrackChair::IsTrack(m_Table)==-1) {
    filter = (StVirtualEventFilter *)m_FilterArray->At(kTable);
    if (!filter || filter->IsOn() ) {
       tableCounter += MakeTableHits(m_Table,filter,positions[1],&positions[2]);
       if (Debug()) printf(" TTable: %d \n", tableCounter);
    }
  }
  else {
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
   static const Style_t UHitSty = 4; static const Size_t UHitSiz = 3.5; static const Color_t UHitCol= 0;
   static const Style_t NHitSty = 1; static const Size_t NHitSiz = 1.00; static const Color_t NHitCol=18;
   static const Style_t TrakSty = 1; static const Size_t TrakSiz = 1.00; static const Color_t TrakCol= 0;
   static const Style_t VertSty = 5; static const Size_t VertSiz = 0.90; static const Color_t VertCol= 0;

   enum QWERTY {kTRK=1,kHIT=2,kUSE=4,kUNU=8};


  if (!pos[1] || !pos[1][0]) return 1;
  memset(fColCash,0,kCOLORS*sizeof(void*));

  if (!mEventHelper) mEventHelper = new StEventHelper;
  mEventHelper->Reset(event,"StL3.*");
  mEventHelper->ls();

  int keyLen = strchr(pos[1],' ') - pos[1];
  int kase=0;
  if (strstr(pos[1],"Track" )) kase |= kTRK;
  if (strstr(pos[1],"Hit"   )) kase |= kHIT;
  if (strstr(pos[1],"Used"  )) kase |= kUSE;
  if (strstr(pos[1],"Unused")) kase |= kUNU;
  int all = (strncmp(pos[1],"All",3)==0);

  TString sel("^StSPtrVec");

  Style_t defSty=0; Size_t defSiz = 0; Color_t defCol= 0;
  TObjArray *shaps =0;


  switch (kase) {

    case kHIT|kUNU:;
    case kHIT|kUSE:;
      if (all) {sel +=".*";}
      else     {sel.Append(pos[1],keyLen);} 
      sel += ".*Hit$";
      shaps = mEventHelper->SelHits  (sel.Data(),(kase>>2)&3);
      break;

    case kTRK:;
    case kTRK|kHIT:;
      if (all) { 
        shaps = mEventHelper->SelTracks(kase&3);
      } else {
        sel.Append(pos[1],keyLen); sel +="Vertex$";
        shaps = mEventHelper->SelVertex(sel.Data(),kase&3);
      } 
      break;

     default: Assert(0);
  }

  switch ( kase ) 
  {
    case (kHIT|kUNU):;
      defSty = NHitSty; defSiz = NHitSiz; defCol = NHitCol; break;

    case (kHIT|kUSE):;
    case (kTRK|kHIT):;
      defSty = UHitSty; defSiz = UHitSiz; defCol = UHitCol; break;

    case kTRK:;
      defSty = TrakSty; defSiz = TrakSiz; defCol = TrakCol; break;      

    default: Assert(0);
  }


  if (!shaps) return 0;
  Int_t trackCounter = 0;

  m_TrackCollector->Add(shaps);		//collect for garbage
  int ntrk = shaps->GetLast()+1;
  if (!ntrk ) return 0;

  Color_t rndCol = kRed;

  TListIter nextFilter(mFilterList);
  int ncut = 0;
  Color_t col = 0;
  StFilterABC *filt=0;
  Style_t sty; Size_t siz;
  StPoints3DABC *pnt;
  int P; const char *L;

  for (int i = 0; i < ntrk; i++ )
  {
    rndCol = (((rndCol-kRed)+1)%6)+kRed;
    pnt = (StPoints3DABC*)shaps->At(i);
//		Filtration
    nextFilter.Reset();
    while ((filt=(StFilterABC*)nextFilter())) {if (!filt->Accept(pnt)) break;}
    if (filt) {ncut++; continue;}
//
    P = pnt->Size()==1;
    L = (P) ? "P":"L";

    sty = defSty; siz = defSiz;
    if (P && !(kase&kHIT)) { sty = VertSty ; siz = VertSiz; }

    col = pnt->GetUniqueID();
    if (!col) col = defCol;
    if (!col) col = rndCol;
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
    TVolume        *thisTrack;
    TPolyLineShape *tracksShape;
    StPoints3DABC  *bigPnt;

    m_TrackCollector->Add(pnt);		//collect garbage
    if (opt[0] == 'P' && col < kCOLORS) {
      thisTrack = fColCash[col];
      if (!thisTrack) {
         bigPnt = new StPoints3DABC(pnt->GetName(),pnt->GetTitle(),0);
         tracksShape = new TPolyLineShape(bigPnt,opt);  
         tracksShape->SetVisibility(1);
         tracksShape->SetColorAttribute(col);
         tracksShape->SetLineStyle(sty);
         tracksShape->SetSizeAttribute(siz);
         thisTrack = new TVolume(pnt->GetName(),pnt->GetTitle(),tracksShape);
         fColCash[col]=thisTrack;
         thisTrack->Mark();   thisTrack->SetVisibility();
         m_EventsNode->Add(thisTrack); 

       } else {
         tracksShape = (TPolyLineShape*)thisTrack->GetShape();
         bigPnt = (StPoints3DABC*)tracksShape->GetPoints();
       }
       bigPnt->Add(pnt);
    
    } else {

      tracksShape = new TPolyLineShape(pnt,opt);
      tracksShape->SetVisibility(1);
      tracksShape->SetColorAttribute(col);
      tracksShape->SetLineStyle(sty);
      tracksShape->SetSizeAttribute(siz);

    // 		Create a node to hold it
      TVolume *thisTrack = new TVolume(pnt->GetName(),pnt->GetTitle(),tracksShape);
      thisTrack->Mark();   thisTrack->SetVisibility();
      m_EventsNode->Add(thisTrack); 
   }
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
     Info("Waiting...");
     while (fgEventLoop==-2) {gSystem->DispatchOneEvent(1);}
     Info("Waiting finished");
     int ans = fgEventLoop; fgEventLoop=-1;
     Info(infos[ans]);
     return ans;
   }

   if (fgEventLoop == 0) {
     if(fgStChain==0) return 0;
     StEventDisplayMaker *edMk = (StEventDisplayMaker*)fgStChain->GetMaker("EventDisplay");
     if (edMk==0) return 0;
     switch(flag) {
       case 1: Info("Redrawing...");
               edMk->ReDraw(); 
               Info("Redrawing finished");
               return 0;
       case 2: Info("NextEvent"); 
               fgStChain->Clear(); fgStChain->Make(); return 0;
     }
     return 0;
   }
  return 2001;
}   
//_____________________________________________________________________________
void StEventDisplayMaker::Info(const char *info)
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
