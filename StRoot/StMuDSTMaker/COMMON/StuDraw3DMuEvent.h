#ifndef STAR_StuDraw3DMuEvent
#define STAR_StuDraw3DMuEvent

// $Id: StuDraw3DMuEvent.h,v 1.13 2010/01/28 05:27:06 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   01/09/2009

#include "StDraw3D.h"
#include "StEnumerations.h"

  
class StMuTrack;
class StMuDst;
class EEmcGeomSimple;
class StMuEmcCollection;

/*! The constant defining the StMuTrack components to be rendered
 */
enum EStuDraw3DMuEvent {
                      kUnusedHitsOnly=-1 //!< Render the unused hits only
                     ,kUsedHits=0        //!< Render the "used" hits
                     ,kUsedHitsTracks=1  //!< Render the "used" hits and tracks
                     ,kTracksOnly=2      //!< Render the tracks only no hit
 };

class StEmcTowerColor {
   private:
       StDraw3DStyle  fStyle; 
       StEmcTowerColor();
       //StEmcTowerColor(const StEmcTowerColor&);
       //void operator=(const StEmcTowerColor&);
   public:
     StEmcTowerColor(float energy);
     virtual ~StEmcTowerColor() {}
     virtual const StDraw3DStyle& operator()() const { return fStyle;}
};


///////////////////////////////////////////////////////////////////////
///
/*! \brief  Class StuDraw3DMuEvent - to draw the StMuDst primitives like StMuTrack 
     as 3D points and 3D lines decorated with the STAR detector geometry
 */
/// \author Valery Fine (fine@bnl.gov)
/// \date 01/08/2009
/// \sa  EdMu.C
///
///  Class StuDraw3DMuEvent provides the simple way to visualize the StMuDst event 
///  primitivies in 3D quickly against of the STAR detector 
///  geometry.
///  One instance of the class is instantiated as soon as the class shared library
///  is loaded.
///  This allows to use the class object (invoke the class methods) with one C++ statement. 
///  This  is to allow creating the 3D views "on fly", 
///  for example, from the GNU debugger (gdb) command prompt 
/// \n Try:
/// \code
///  > ln -s  $STAR/StRoot/macros/.rootrc
///  > root.exe $STAR/StRoot/macros/mudst/draw3DTracks.C
/// \endcode
///  to  read some MuDst ROOT file and get the pictire below:
///  \htmlonly
///  <P>You need to install the <a href="http://get.adobe.com/reader/?promoid=BUIGO">Adobe Reader version 9 or higher
///  <img src="http://www.adobe.com/images/shared/download_buttons/get_adobe_reader.png"></a>
///  to be able to "click and see" the interactive ( zoom, pan, select / highlight the pieces, etc )  3D image also
///  <center>
///  <a href="http://www.star.bnl.gov/public/comp/vis/StDraw3D/StuDraw3DMuEvent.pdf">
///  <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/StMuTracks.png"></a>
///  </center><p>
///  \endhtmlonly
///
///////////////////////////////////////////////////////////////////////
class StuDraw3DMuEvent : public virtual StDraw3D
{
  private: 
     EEmcGeomSimple *fEndcapGeom; // tower geomtry

     StuDraw3DMuEvent(const StuDraw3DMuEvent&):StDraw3D(){;}
     void operator=(const StuDraw3DMuEvent&){;}

     static StuDraw3DMuEvent *gMuEventDisplay;
  protected:
     EEmcGeomSimple *EndcapGeom();

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
     virtual void     Endcaps(Style_t sty=0);
     virtual void     Endcaps(const StMuEmcCollection &e, Style_t sty=0);
     virtual TObject *EmcHit(Int_t emcHitsSoftId, Color_t col,Style_t sty,Size_t siz, const char *detIdt="bemc");
     virtual TObject *EmcHit(Int_t emcHitsSoftId, float energy, const char *detIdt="bemc");
     template <class Predicate>
     TObject *EmcHit(Int_t emcHitsSoftId, const Predicate &sty, const char *detIdt="bemc");
     ClassDef(StuDraw3DMuEvent,0);
};

//_________________________________________________________________________________________________
template <class Predicate>
TObject *StuDraw3DMuEvent::EmcHit(Int_t emcHitsSoftId, const Predicate &sty, const char *detIdt)
{
   return EmcHit(emcHitsSoftId, sty().Col(),sty().Sty(), sty().Siz(), detIdt);
}


#endif
