#ifndef STAR_StDraw3DEvent
#define STAR_StDraw3DEvent

// $Id: StDraw3DEvent.h,v 1.5 2008/05/03 22:09:08 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008

#include "StDraw3D.h"
#include "StThreeVector.hh"

  ///////////////////////////////////////////////////////////////////////
  //
  // class StDraw3DEvent - to draw the StEvent primitives like StTrack and StMeasuredPoint 
  // as 3D points and 3D lines
  // decoratated with the STAR detector geometry
  //
  //
  //  <begin_html> <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/Draw3DClass.png">end_html
  //
  ///////////////////////////////////////////////////////////////////////
  
class StTrack;
class StMeasuredPoint;

class StDraw3DEvent : public StDraw3D
{
  public:
     StDraw3DEvent(TVirtualPad *pad = 0);
     virtual ~StDraw3DEvent(){;}
     virtual TObject *Track(const StTrack &track
                  ,  Color_t col
                  ,  Style_t sty= Style_t(-1)
                  ,  Size_t siz = Size_t (-1));
     virtual TObject *Track(const StTrack &track, EDraw3DStyle sty=kPrimaryTrack);
     virtual TObject *Hit(const StMeasuredPoint &hit
                  ,  Color_t col
                  ,  Style_t sty= Style_t(-1)
                  ,  Size_t siz = Size_t (-1));
     virtual TObject *Hit(const StMeasuredPoint &hit, EDraw3DStyle sty=kUsedHit);
     virtual TObject *Vertex(const StMeasuredPoint &hit
                  ,  Color_t col
                  ,  Style_t sty= Style_t(-1)
                  ,  Size_t siz = Size_t (-1));
     virtual TObject *Vertex(const StMeasuredPoint &hit, EDraw3DStyle sty=kVtx);
     virtual TObject *TrackInOut(const StTrack &track, Bool_t in
                  ,  Color_t col= Color_t(-1)
                  ,  Style_t sty= Style_t(-1)
                  ,  Size_t siz = Size_t (-1));
     virtual TObject *TrackInOut(const StTrack &track, EDraw3DStyle sty=kUsedHit, Bool_t in=kTRUE);
     template <class T> TObject *Vector(const StThreeVector<T> &vector
                  ,  Color_t col
                  ,  Style_t sty= Style_t(-1)
                  ,  Size_t siz = Size_t (-1));
     template <class T> TObject *Vector(const StThreeVector<T> &vector, EDraw3DStyle sty=kVtx);
     ClassDef(StDraw3DEvent,0);
};

//___________________________________________________
template<class T> TObject *StDraw3DEvent::Vector(const StThreeVector<T> &vector
                  ,  Color_t col,  Style_t sty,  Size_t siz)
{  return Point(vector.x(),vector.y(),vector.z(),col,sty,siz); }

//___________________________________________________
template <class T> TObject *StDraw3DEvent::Vector(const StThreeVector<T> &vector, EDraw3DStyle sty)
{
     const StDraw3DStyle &style =  Style(sty);
     return Vector(vector.x(),vector.y(),vector.z(),style.Col(),style.Sty(),style.Siz()); 
}

#endif
