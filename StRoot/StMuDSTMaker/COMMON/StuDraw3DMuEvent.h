#ifndef STAR_StuDraw3DMuEvent
#define STAR_StuDraw3DMuEvent

// $Id: StuDraw3DMuEvent.h,v 1.4 2009/09/06 19:01:51 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   01/09/2009

#include "StDraw3D.h"
#include "StEnumerations.h"

  
class StMuTrack;
class StMuDst;

/*! The constant defining the StMuTrack components to be rendered
 */
enum EStuDraw3DMuEvent {
                      kUnusedHitsOnly=-1 //!< Render the unused hits only
                     ,kUsedHits=0        //!< Render the "used" hits
                     ,kUsedHitsTracks=1  //!< Render the "used" hits and tracks
                     ,kTracksOnly=2      //!< Render the tracks only no hit
 };

//! \author Valery Fine(fine@bnl.gov)
//! \date 01/08/2009

///////////////////////////////////////////////////////////////////////
///
/*! \brief  Class StuDraw3DMuEvent - to draw the StMuDst primitives like StMuTrack 
     as 3D points and 3D lines decorated with the STAR detector geometry
 */
///
///  Class StuDraw3DMuEvent provides the simple way to visualize the event 
///  primitives in 3D quickly against of the STAR detector 
///  geometry.
///  One instance of the class is instantiated as soon as the class shared library
///  is loaded.
///  This allows to use the class object (invoke class methods) with one C++ statement. 
///  This  is to allow creating the 3D views "on fly", 
///  for example, from the GNU debugger (gdb) command prompt 
/// \n Try:
/// \code
///  > stardev	
///  > ln -s  $STAR/QtRoot/qtExamples/QtGBrowser/.rootrc
///  > root.exe $STAR/StRoot/macros/mudst/draw3DTracks.C
/// \endcode
///  to  read some MuDst ROOT file and get the pictire below:\n
///  <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/StMuTracks.png">
///
///////////////////////////////////////////////////////////////////////
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
