// $Id: StuDraw3DEvent.cxx,v 1.2 2008/05/05 02:28:37 fine Exp $
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

ClassImp(StuDraw3DEvent)
           
  ////////////////////////////////////////////////////////////////////////
  //
  //  Class StuDraw3DEvent - to draw the 3D StEvent primitives like StTrack, StHit, StVertex
  //  decoratated with the STAR detector geometry
  //
  //  It provides the simple way to visualize the event 
  //  primitives in 3D quickly against of the STAR detector 
  //  geometry.
  //  <begin_html> <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/Draw3DClass.png">end_html
  //
  ////////////////////////////////////////////////////////////////////////
//___________________________________________________
StuDraw3DEvent::StuDraw3DEvent(TVirtualPad *pad): StDraw3D(pad)
{ }

//___________________________________________________
TObject *StuDraw3DEvent::Track(const StTrack &track, Color_t col,Style_t sty,Size_t siz)
{
   StTrackPoints trPnt(&track);
   TObject *l = Line(trPnt.GetN(),trPnt.GetP(),col,sty,siz);
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
   return Points(trInOut.GetN(),trInOut.GetP(),col,sty,siz);
}

//___________________________________________________
TObject *StuDraw3DEvent::TrackInOut(const StTrack &track, EDraw3DStyle sty,Bool_t in)
{
   const StDraw3DStyle &style =  Style(sty);
   return TrackInOut(track, in, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
void StuDraw3DEvent::Tracks(const StEvent* event)
{
   const StSPtrVecTrackNode& theNodes = event->trackNodes();
   Tracks(theNodes);
}

//___________________________________________________
void StuDraw3DEvent::Tracks(const StSPtrVecTrackNode &theNodes)
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
      track = theNodes[i]->track(global);
      if (track && track->flag() > 0
         //  &&   track->fitTraits().numberOfFitPoints(kTpcId) >= minFitPoints) 
         )
      {
         double pt = track->geometry()->momentum().perp();
         hue = Int_t(256.*(1.-pt/1.5)); //color code from StuPostscript
         if (pt > 1.5 ) hue = 0;
         Int_t r,g,b;
         TColor::HLS2RGB(hue, lightness, saturation, r, g, b);
         Color_t trackColor =  TColor::GetColor(r,g,b);
         Track(*track,trackColor);
      }
   }    
}

//___________________________________________________
void StuDraw3DEvent::Hits(const StEvent *event,bool trackHitsOnly)
{
   if (event) 
    Hits(event->tpcHitCollection(),trackHitsOnly);
}

//___________________________________________________
void StuDraw3DEvent::Hits(const StTpcHitCollection* hits,bool trackHitsOnly)
{
   if (!hits) return;
   unsigned int m, n, h;
   const StTpcHit *hit;            
   for (n=0; n<hits->numberOfSectors(); n++) {
      for (m=0; m<hits->sector(n)->numberOfPadrows(); m++) { 
         for (h=0; h<hits->sector(n)->padrow(m)->hits().size(); h++) {
            hit = hits->sector(n)->padrow(m)->hits()[h];
            if (!trackHitsOnly || hit->trackReferenceCount())   Hit(*hit);
         }
      }
   }
}

//___________________________________________________
StuDraw3DEvent *StuDraw3DEvent::Display(){ return gEventDisplay;}

StuDraw3DEvent *gEventDisplay = new StuDraw3DEvent();
