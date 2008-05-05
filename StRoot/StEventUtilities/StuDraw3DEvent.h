#ifndef STAR_StuDraw3DEvent
#define STAR_StuDraw3DEvent

// $Id: StuDraw3DEvent.h,v 1.3 2008/05/05 02:31:35 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008

#include "StDraw3D.h"
#include "StThreeVector.hh"

  ///////////////////////////////////////////////////////////////////////
  //
  // class StuDraw3DEvent - to draw the StEvent primitives like StTrack and StMeasuredPoint 
  // as 3D points and 3D lines
  // decoratated with the STAR detector geometry
  //
  //
  //  <begin_html> <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/Draw3DClass.png">end_html
  //
  ///////////////////////////////////////////////////////////////////////
  
class StTrack;
class StMeasuredPoint;
class StEvent;
class StTpcHitCollection;
class StSPtrVecTrackNode;

class StuDraw3DEvent : public StDraw3D
{
  private:
     StuDraw3DEvent(const StuDraw3DEvent&){;}
     void operator=(const StuDraw3DEvent&){;}

  public:
     StuDraw3DEvent(TVirtualPad *pad = 0);
     virtual ~StuDraw3DEvent(){;}
     static StuDraw3DEvent *Display();
     virtual void     Tracks(const StEvent* event);
     virtual void     Tracks(const StSPtrVecTrackNode &tracks);
     virtual TObject *Track(const StTrack &track
                  ,  Color_t col
                  ,  Style_t sty= Style_t(-1)
                  ,  Size_t siz = Size_t (-1));
     virtual TObject *Track(const StTrack &track, EDraw3DStyle sty=kPrimaryTrack);
     virtual TObject *Hit(const StMeasuredPoint &hit
                  ,  Color_t col
                  ,  Style_t sty= Style_t(-1)
                  ,  Size_t siz = Size_t (-1));
     virtual void    Hits(const StEvent* event, bool trackHitsOnly=true);
     virtual void    Hits(const StTpcHitCollection* hits, bool trackHitsOnly=true);
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
     ClassDef(StuDraw3DEvent,0);
};

extern StuDraw3DEvent *gEventDisplay;

//___________________________________________________
template<class T> TObject *StuDraw3DEvent::Vector(const StThreeVector<T> &vector
                  ,  Color_t col,  Style_t sty,  Size_t siz)
{  return Point(vector.x(),vector.y(),vector.z(),col,sty,siz); }

//___________________________________________________
template <class T> TObject *StuDraw3DEvent::Vector(const StThreeVector<T> &vector, EDraw3DStyle sty)
{
     const StDraw3DStyle &style =  Style(sty);
     return Vector(vector.x(),vector.y(),vector.z(),style.Col(),style.Sty(),style.Siz()); 
}

#endif
