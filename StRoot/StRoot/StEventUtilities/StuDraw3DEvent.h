#ifndef STAR_StuDraw3DEvent
#define STAR_StuDraw3DEvent

// $Id: StuDraw3DEvent.h,v 1.22 2018/06/29 17:21:24 perev Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/05/2008

#include "StDraw3D.h"
#include "StThreeVector.hh"
#include "StEnumerations.h"


class StTrack;
class StGlobalTrack;
class StMeasuredPoint;
class StEvent;
class StEventHitIter;
class StTpcHitCollection;
class StSPtrVecTrackNode;
class StEmcRawHit;

/*! The constant defining the StTrack components to be rendered
 */
enum EStuDraw3DEvent {kUnusedHitsOnly=-1 //!< Render the unused hits only
                     ,kUsedHits=0        //!< Render the "used" hits
                     ,kUsedHitsTracks=1  //!< Render the "used" hits and tracks
                     ,kTracksOnly=2      //!< Render the tracks only no hit
      };

//! \author Valery Fine(fine@bnl.gov)
//! \date 27/04/2008

/*! \brief  Class StuDraw3DEvent - to draw the 3D StEvent primitives like 
 *          StTrack, StHit, StVertex decorated with the STAR detector geometry
 */
/// \author Valery Fine (fine@bnl.gov)
/// \date 27/04/2008
/// \sa  Ed.C
///
///  Class StuDraw3DEvent provides the simple way to visualize the event 
///  primitives in 3D quickly against of the STAR detector 
///  geometry.
///  One instance of the class is instantiated as soon as the class shared library
///  is loaded.
///  This allows to use the class object (invoke class methods) with one C++ statement. 
///  This  is to allow creating the 3D views "on fly", 
///  for example, from the GNU debugger (gdb) command prompt 
/// \n Try:
/// \code
///  > ln -s  $STAR/StRoot/macros/.rootrc
///  > root.exe Draw3D.C
/// \endcode
///  to get the test picture below:\n
///  <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DClass.png">
///  \image  html Draw3DClass.png "Example of the STAR reconstructed event rendering"
///
////////////////////////////////////////////////////////////////////////
class StuDraw3DEvent : public virtual StDraw3D
{
  private:
     StuDraw3DEvent(const StuDraw3DEvent&):StDraw3D(){;}
     void operator=(const StuDraw3DEvent&){;}

  public:
     StuDraw3DEvent(const char *detectorName="TPC",TVirtualPad *pad = 0);
     virtual ~StuDraw3DEvent();
     static StuDraw3DEvent *Display();
     virtual void     Tracks(const StEvent* event, StTrackType type=global);
     virtual void     Tracks(const StSPtrVecTrackNode &tracks, StTrackType type=global);
     virtual TObject *Track(const StTrack &track
                  ,  Color_t col
                  ,  Style_t sty= Style_t(-1)
                  ,  Size_t siz = Size_t (-1));
     virtual TObject *Track(const StTrack &track, EDraw3DStyle sty=kPrimaryTrack);
     virtual TObject *Track(const StGlobalTrack &track
                  ,  Color_t col
                  ,  Style_t sty= Style_t(-1)
                  ,  Size_t siz = Size_t (-1));
     virtual TObject *Track(const StGlobalTrack &track, EDraw3DStyle sty=kPrimaryTrack);
     virtual TObject *Hit(const StMeasuredPoint &hit
                  ,  Color_t col
                  ,  Style_t sty= Style_t(-1)
                  ,  Size_t siz = Size_t (-1));
     virtual void    Hits(const StEvent* event, EStuDraw3DEvent trackHitsOnly=kUsedHits, StTrackType type=global);
     virtual void    Hits(StEventHitIter &iter);
     virtual void    FtpcHits(const StEvent* event, EStuDraw3DEvent trackHitsOnly=kUsedHits, StTrackType type=global);
     virtual void    Hits(const StTrack &track,  EDraw3DStyle sty);
     virtual void    Hits(const StTrack &track);
     virtual void    Hits(const StTrack &track
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
     virtual void EmcHits(const StEvent* event, const char *det="bemc");
     virtual TObject *EmcHit(Int_t emcHitsSoftId, Color_t col,Style_t sty,Size_t siz, const char *det="bemc" );
     virtual TObject *EmcHit(const StEmcRawHit &emcHit, Color_t col,Style_t sty,Size_t siz,const char *det="bemc");
     static void Wait();
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
