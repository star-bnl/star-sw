// $Id: EdFtpc.C,v 1.5 2010/04/29 23:57:51 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   25/02/2009
#ifndef __CINT__
# include "StuDraw3DEvent.h"
# include "TColor.h"
# include "StEvent.h"
# include "StTrack.h"
# include "StHit.h"
# include "StFtpcHit.h"
# include "StTrackNode.h"
# include "StTrackGeometry.h"
# include "StFtpcHitCollection.h"
# include "StFtpcSectorHitCollection.h"
# include "StFtpcPlaneHitCollection.h"
# include "StMeasuredPoint.h"
# include "StTrackDetectorInfo.h"
# include "StChain.h"
# include "TSystem.h"
# include "TROOT.h"
#endif

class StEvent;
StEvent* event = 0;
class StChain;
StChain* ftpChain = 0;
class StuDraw3DEvent;
StuDraw3DEvent *gEdFtp = 0;
//__________________________________________
void DrawUnusedHits() 
{
   const StHit *hit= 0;
   std::vector<float> hitPoints;
   const StFtpcHitCollection* ftpHits = event->ftpcHitCollection(); 
   if (ftpHits->numberOfHits()>0) {
      unsigned int n,m,h;
      for (n=0;n<ftpHits-> numberOfPlanes();++n ) {
         for (m=0; m<ftpHits->plane(n)->numberOfSectors(); m++) { 
            for (h=0; h<ftpHits->plane(n)->sector(m)->hits().size(); h++) {
                hit = ftpHits->plane(n)->sector(m)->hits()[h];
                hitPoints.push_back( hit->position().x());
                hitPoints.push_back( hit->position().y());
                hitPoints.push_back( hit->position().z());
        } } }
        std::vector<float>::iterator xyz = hitPoints.begin();
        
        gEdFtp->Points(hitPoints.size()/3,&*xyz,kUnusedHit);
        gEdFtp->SetComment("Unused FTPC hits");
        
        printf(" FTPC hits counter total : %d\n", hitPoints.size()/3);
     }
  }

//__________________________________________
void DrawTrackHits() 
{
   // tracks and its  "used hits"
   Style_t sty    = gEdFtp->Style(kUsedHit).Sty();
   Size_t  siz    = gEdFtp->Style(kUsedHit).Siz();
   
   Style_t styPnt = gEdFtp->Style(kTrackBegin).Sty();
   Size_t  sizPnt = gEdFtp->Style(kTrackBegin).Siz();
   
   StTrackType type = global;
   
   int trackCounter = 0;
   const StSPtrVecTrackNode& theNodes = event->trackNodes();
   for (unsigned int i=0; i<theNodes.size(); i++) {
       StTrack *track = theNodes[i]->track(type);
       if (track &&  track->flag() > 0
                 &&  track->detectorInfo()                
                 &&  ( track->detectorInfo()->numberOfPoints(kFtpcWestId) || track->detectorInfo()->numberOfPoints(kFtpcEastId)  )
                 && !track->bad() 
           )
        {
           ++trackCounter;
           double pt = track->geometry()->momentum().perp();
           Color_t trackColor = StDraw3DStyle::Pt2Color(pt);
//           if ( trackHitsOnly != kUsedHits) 
           {
              gEdFtp->Track(*track,trackColor);
              gEdFtp->SetComment(Form("Pt=%f /Gev",pt));
              gEdFtp->TrackInOut(*track, true,  trackColor,  styPnt, sizPnt);
              gEdFtp->TrackInOut(*track, false, trackColor,  styPnt, sizPnt);
           }
//           if ( trackHitsOnly != kTracksOnly) 
           {
              gEdFtp->Hits(*track,trackColor,sty,siz);
              // if (trackHitsOnly == kUsedHits) gEdFtp->SetModel(track);
           }
        }
     }
}

//__________________________________________
void rd() 
{  
   // redraw the event
   bool clear = true;
   if (event) {
      if (clear) gEdFtp->Clear();
      DrawTrackHits();
      DrawUnusedHits();
   }
}
//__________________________________________
void skip(int nEvents=1) {
   if(ftpChain) ftpChain->Skip(nEvents);
}

//__________________________________________
void ae() 
{
   gEdFtp->Clear();
 newevent:
     ftpChain->MakeEvent();
     event = (StEvent*)ftpChain->GetDataSet("StEvent");
     if (event && !event->trackNodes().empty()) {
         rd();     // Draw the tracks
    } else {
        printf(" event is empty %p\n", event);
        goto newevent;
   }
 }
 
//__________________________________________
 void EdFtpc(const char* file =
 "/star/institutions/bnl/fine/testfiles/st_physics_10169042_raw_4030001.event.root"
 , unsigned int nEvent=44, const char * detectorNames="FTPC,StarFloor,StarCompass,StarBeam")
 {
   // Start application open the file provided.
   if ( gSystem->AccessPathName(file)) {
      cout << endl << endl 
           << "** Error ** : The input file: <"<< file << "> does not exist !!!" 
           << endl << endl;
        return;
   }

   gROOT->Macro("Load.C"); 
   gSystem->Load("StDetectorDbMaker");
   TString bfcchain = Form("bfc.C(0,\"doevents\",\"%s\")",file);
   if (nEvent > 1) 
      bfcchain = Form("bfc.C(%d,%d-1,\"doevents\",\"%s\")",nEvent,nEvent,file);
   cout << bfcchain.Data() << endl;
   gROOT->Macro(bfcchain.Data());

   delete gEventDisplay; // destroy the built-in display
   ftpChain = (StChain *)StMaker::GetChain();
// -- Start new display   
   gEdFtp = new StuDraw3DEvent(detectorNames); // create our own one (with no detector geometry)
   gEdFtp->SetBkColor(kBlack);
   
   ae();  // Advance one event 
 }
