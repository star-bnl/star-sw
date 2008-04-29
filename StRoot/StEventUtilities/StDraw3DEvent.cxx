// $Id: StDraw3DEvent.cxx,v 1.2 2008/04/29 17:16:58 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008
#include "StDraw3DEvent.h"
#include "TVirtualPad.h"
#include "StEventHelper.h"
#include "StTrack.h"
#include "StMeasuredPoint.h"

//___________________________________________________
StDraw3DEvent::StDraw3DEvent(TVirtualPad *pad): StDraw3D(pad)
{ }

//___________________________________________________
TObject *StDraw3DEvent::Track(const StTrack &track, Color_t col,Style_t sty,Size_t siz)
{
   StTrackPoints trPnt(&track);
   return Line(trPnt.GetN(),trPnt.GetP(),col,sty,siz);
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
   return Point(position.x(),position.y(),position.z(),col,sty,siz);
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

