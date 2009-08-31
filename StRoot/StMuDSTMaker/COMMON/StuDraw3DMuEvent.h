#ifndef STAR_StuDraw3DMuEvent
#define STAR_StuDraw3DMuEvent

// $Id: StuDraw3DMuEvent.h,v 1.2 2009/08/31 23:59:23 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008

#include "StDraw3D.h"
#include "StEnumerations.h"

  ///////////////////////////////////////////////////////////////////////
  //
  // class StuDraw3DMuEvent - to draw the StEvent primitives like StMuTrack 
  // as 3D points and 3D lines
  // decoratated with the STAR detector geometry
  //
  //
  //  <begin_html> <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DClass.png">end_html
  //
  ///////////////////////////////////////////////////////////////////////
  
class StMuTrack;
class StMuDst;

enum EStuDraw3DMuEvent {kUnusedHitsOnly=-1,kUsedHits=0,kUsedHitsTracks=1,kTracksOnly=2};

class StuDraw3DMuEvent : public StDraw3D
{
  private:
     StuDraw3DMuEvent(const StuDraw3DMuEvent&):StDraw3D(){;}
     void operator=(const StuDraw3DMuEvent&){;}

     static StuDraw3DMuEvent *gMuEventDisplay;

  public:
     StuDraw3DMuEvent(const char *detectorName="TPC",TVirtualPad *pad = 0);
     virtual ~StuDraw3DMuEvent();
     static StuDraw3DMuEvent *Display();
     virtual void Tracks(StTrackType type=global);
     virtual TObject *Track(const StMuTrack &track
                  ,  Color_t col
                  ,  Style_t sty= Style_t(-1)
                  ,  Size_t siz = Size_t (-1));
     virtual TObject *Track(const StMuTrack &track, EDraw3DStyle sty=kPrimaryTrack);
     virtual TObject *TrackInOut(const StMuTrack &track, Bool_t in
                  ,  Color_t col= Color_t(-1)
                  ,  Style_t sty= Style_t(-1)
                  ,  Size_t siz = Size_t (-1));
     virtual TObject *TrackInOut(const StMuTrack &track, EDraw3DStyle sty=kUsedHit, Bool_t in=kTRUE);
     ClassDef(StuDraw3DMuEvent,0);
};


#endif
