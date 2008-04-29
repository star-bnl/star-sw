#ifndef STAR_StDraw3DEvent
#define STAR_StDraw3DEvent

#include "StDraw3D.h"
  //
  // class StDraw3DEvent - to draw the StEvent primitives like 3StTrack and StMeasuredPoint 
  // as 3D points and 3D lines
  // decoratated with the STAR detector geometry
  //
  
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
     ClassDef(StDraw3DEvent,0);
};
#endif
