// $Id: StuDraw3DEvent.cxx,v 1.9 2008/06/09 15:03:25 fisyak Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008
#include "StuDraw3DEvent.h"
#include "TVirtualPad.h"
#include "TColor.h"
#include "StEventHelper.h"
#include "StEvent.h"
#include "StTrack.h"
#include "StHit.h"
#include "StTpcHit.h"
#include "StTrackNode.h"
#include "StTrackGeometry.h"
#include "StTpcHitCollection.h"
#include "StMeasuredPoint.h"
#include "TMath.h"
#include "StTrackDetectorInfo.h"

ClassImp(StuDraw3DEvent)
           
  ////////////////////////////////////////////////////////////////////////
  //
  //  Class StuDraw3DEvent - to draw the 3D StEvent primitives like StTrack, StHit, StVertex
  //  decoratated with the STAR detector geometry
  //
  //  It provides the simple way to visualize the event 
  //  primitives in 3D quickly against of the STAR detector 
  //  geometry.
  //  <begin_html> <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DClass.png">end_html
  //
  ////////////////////////////////////////////////////////////////////////
//___________________________________________________
StuDraw3DEvent::StuDraw3DEvent( const char *detectorName,TVirtualPad *pad): 
StDraw3D(detectorName,pad)
{
   // The detectorName is a comma separated list of the OpenInventor files with no extension
   // For all names on the list one should provide the iv file with the "iv extension:
   //                         <name>.iv
   if (!gEventDisplay) gEventDisplay = this;
}

//___________________________________________________
StuDraw3DEvent::~StuDraw3DEvent()
{
   if (gEventDisplay == this) gEventDisplay = 0;
}

//___________________________________________________
TObject *StuDraw3DEvent::Track(const StTrack &track, Color_t col,Style_t sty,Size_t siz)
{
   StTrackPoints trPnt(&track);
   TObject *l = Line(trPnt.Size(),trPnt.GetXYZ(0),col,sty,siz);
   SetModel((TObject*)&track);
   return l;
}

//___________________________________________________
TObject *StuDraw3DEvent::Track(const StTrack &track, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Track(track, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
TObject *StuDraw3DEvent::Hit(const StMeasuredPoint &hit
                  ,  Color_t col,  Style_t sty,  Size_t siz)
{
   // Draw the StMeasuredPoint, StHit, StVertex with the graphical attribute provided
   const StThreeVectorF& position = hit.position();
   TObject *p = Point(position.x(),position.y(),position.z(),col,sty,siz);
   SetModel((TObject*)&hit);
   return p;
}

//___________________________________________________
TObject *StuDraw3DEvent::Hit(const StMeasuredPoint &hit, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Hit(hit, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
TObject *StuDraw3DEvent::Vertex(const StMeasuredPoint &vertex
                  ,  Color_t col,  Style_t sty, Size_t siz)
{
   return Hit(vertex,col,sty,siz);
}

//___________________________________________________
TObject *StuDraw3DEvent::Vertex(const StMeasuredPoint &vtx, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Vertex(vtx, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
TObject *StuDraw3DEvent::TrackInOut(const StTrack &track, Bool_t in
                  ,  Color_t col,  Style_t sty,  Size_t siz)
{
   StInnOutPoints trInOut(&track,in);
   return Points(trInOut.Size(),trInOut.GetXYZ(0),col,sty,siz);
}

//___________________________________________________
TObject *StuDraw3DEvent::TrackInOut(const StTrack &track, EDraw3DStyle sty,Bool_t in)
{
   const StDraw3DStyle &style =  Style(sty);
   return TrackInOut(track, in, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
void StuDraw3DEvent::Tracks(const StEvent* event, StTrackType type)
{
   Hits(event,kTracksOnly,type);
}

//___________________________________________________
void StuDraw3DEvent::Tracks(const StSPtrVecTrackNode &theNodes
      , StTrackType type)
{
   // const double lineWidth    = 0.4;
 //  const double sstep        = mFewerPointsPerTrack ? 4 : 1;
   const Int_t lightness    = 50;
   const Int_t saturation   = 100;
   Int_t hue  = 0;

   //
   //  Handle options
   //
   // int  trackGrayLevel = mBlackBackground ? 1 : 0;

   StTrack *track;
   StThreeVectorD p;
   unsigned int i;
   // double pt;
   //    int  minFitPoints = mAllGlobalTracks ? 1 : mMinFitPoints;
   for (i=0; i<theNodes.size(); i++) {
      track = theNodes[i]->track(type);
      if (track && track->flag() > 0
         //  &&   track->fitTraits().numberOfFitPoints(kTpcId) >= minFitPoints) 
         )
      {
         double pt = track->geometry()->momentum().perp();
         hue = Int_t(256.*(1.-pt/1.5)); //color code from StuPostscript
         if (pt > 1.5 ) hue = 0;
         Int_t r,g,b;
         TColor::HLS2RGB(hue, lightness, saturation, r, g, b);
         // Normalize
         float factor = 1./TMath::Sqrt(1.*r*r+1.*g*g+1.*b*b);
         Color_t trackColor =  TColor::GetColor(r*factor,g*factor,b*factor);
         Track(*track,trackColor);
      }
   }
}

//___________________________________________________
void StuDraw3DEvent::Hits(const StEvent *event,EStuDraw3DEvent trackHitsOnly, StTrackType type)
{
   if (!event) return; // no event
   const StTpcHitCollection* hits = event->tpcHitCollection();
   if (!hits) return; // there is no hits
   unsigned int m, n, h;
   if (trackHitsOnly != kUnusedHitsOnly) {
      const Int_t lightness    = 50;
      const Int_t saturation   = 100;
      Int_t hue  = 0;

      StHit *hit;
      Style_t sty = Style(kUsedHit).Sty();
      Size_t  siz = Style(kUsedHit).Siz();
      Style_t styPnt = Style(kTrackBegin).Sty();
      Size_t  sizPnt = Style(kTrackBegin).Siz();

      const StSPtrVecTrackNode& theNodes = event->trackNodes();
      for (unsigned int i=0; i<theNodes.size(); i++) {
         StTrack *track = theNodes[i]->track(type);
         if (track && track->flag() > 0
               && track->detectorInfo() 
         //  &&   track->fitTraits().numberOfFitPoints(kTpcId) >= minFitPoints) 
         )
        {
            double pt = track->geometry()->momentum().perp();
            hue = Int_t(256.*(1.-pt/1.5)); //color code from StuPostscript
            if (pt > 1.5 ) hue = 0;
            Int_t r,g,b;
            TColor::HLS2RGB(hue, lightness, saturation, r, g, b);
            // Normalize
            float factor = 1./TMath::Sqrt(1.*r*r+1.*g*g+1.*b*b);
            Color_t trackColor =  TColor::GetColor(r*factor,g*factor,b*factor);
            if ( trackHitsOnly != kUsedHits) {
               Track(*track,trackColor);
               TrackInOut(*track, true,  trackColor,  styPnt, sizPnt);
               TrackInOut(*track, false, trackColor,  styPnt, sizPnt);
            }
            if ( trackHitsOnly != kTracksOnly) {
               // look for the hits:
               std::vector<float> hitPoints;
               const StPtrVecHit& trackHits = track->detectorInfo()->hits(kTpcId);
               for (m=0; m<trackHits.size(); m++) {
                  hit = trackHits[m];
                  hitPoints.push_back( hit->position().x());
                  hitPoints.push_back( hit->position().y());
                  hitPoints.push_back( hit->position().z());
              }
              std::vector<float>::iterator xyz = hitPoints.begin();
              Points(hitPoints.size()/3,&*xyz,trackColor,sty,siz);
              if (trackHitsOnly == kUsedHits) SetModel(track);
           }
        }
     }
   } else {
      const StTpcHit *hit;
      std::vector<float> hitPoints;
      for (n=0; n<hits->numberOfSectors(); n++) {
         for (m=0; m<hits->sector(n)->numberOfPadrows(); m++) { 
            for (h=0; h<hits->sector(n)->padrow(m)->hits().size(); h++) {
               hit = hits->sector(n)->padrow(m)->hits()[h];
                hitPoints.push_back( hit->position().x());
                hitPoints.push_back( hit->position().y());
                hitPoints.push_back( hit->position().z());
            }
         }
      }
      std::vector<float>::iterator xyz = hitPoints.begin();
      Points(hitPoints.size()/3,&*xyz,kUnusedHit);
      SetComment("Unused TPC hits");
   }
}

//___________________________________________________
StuDraw3DEvent *StuDraw3DEvent::Display(){ return gEventDisplay;}

StuDraw3DEvent *gEventDisplay = new StuDraw3DEvent();
