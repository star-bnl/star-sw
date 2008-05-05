// $Id: StDraw3DEvent.cxx,v 1.5 2008/05/05 00:31:16 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008
#include "StDraw3DEvent.h"
#include "TVirtualPad.h"
#include "StEventHelper.h"
#include "StTrack.h"
#include "StMeasuredPoint.h"

ClassImp(StDraw3DEvent)
           
  ////////////////////////////////////////////////////////////////////////
  //
  //  Class StDraw3DEvent - to draw the 3D StEvent primitives like StTrack, StHit, StVertex
  //  decoratated with the STAR detector geometry
  //
  //  It provides the simple way to visualize the event 
  //  primitives in 3D quickly against of the STAR detector 
  //  geometry.
  //  <begin_html> <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/Draw3DClass.png">end_html
  //
  ////////////////////////////////////////////////////////////////////////
//___________________________________________________
StDraw3DEvent::StDraw3DEvent(TVirtualPad *pad): StDraw3D(pad)
{ }

//___________________________________________________
TObject *StDraw3DEvent::Track(const StTrack &track, Color_t col,Style_t sty,Size_t siz)
{
   StTrackPoints trPnt(&track);
   TObject *l = Line(trPnt.GetN(),trPnt.GetP(),col,sty,siz);
   SetModel((TObject*)&track);
   return l;
}

//___________________________________________________
TObject *StDraw3DEvent::Track(const StTrack &track, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Track(track, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
TObject *StDraw3DEvent::Hit(const StMeasuredPoint &hit
                  ,  Color_t col,  Style_t sty,  Size_t siz)
{
   // Draw the StMeasuredPoint, StHit, StVertex with the graphical attribute provided
   const StThreeVectorF& position = hit.position();
   TObject *p = Point(position.x(),position.y(),position.z(),col,sty,siz);
   SetModel((TObject*)&hit);
   return p;
}

//___________________________________________________
TObject *StDraw3DEvent::Hit(const StMeasuredPoint &hit, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Hit(hit, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
TObject *StDraw3DEvent::Vertex(const StMeasuredPoint &vertex
                  ,  Color_t col,  Style_t sty, Size_t siz)
{
   return Hit(vertex,col,sty,siz);
}

//___________________________________________________
TObject *StDraw3DEvent::Vertex(const StMeasuredPoint &vtx, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Vertex(vtx, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
TObject *StDraw3DEvent::TrackInOut(const StTrack &track, Bool_t in
                  ,  Color_t col,  Style_t sty,  Size_t siz)
{
   StInnOutPoints trInOut(&track,in);
   return Points(trInOut.GetN(),trInOut.GetP(),col,sty,siz);
}

//___________________________________________________
TObject *StDraw3DEvent::TrackInOut(const StTrack &track, EDraw3DStyle sty,Bool_t in)
{
   const StDraw3DStyle &style =  Style(sty);
   return TrackInOut(track, in, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
StDraw3DEvent *StDraw3DEvent::Display(){ return gEventDisplay;}

StDraw3DEvent *gEventDisplay = new StDraw3DEvent();
