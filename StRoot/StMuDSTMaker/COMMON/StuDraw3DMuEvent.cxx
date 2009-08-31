// $Id: StuDraw3DMuEvent.cxx,v 1.2 2009/08/31 23:59:22 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008
#include "StuDraw3DMuEvent.h"
#include "Gtypes.h"
#include "StHelixHelper.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StThreeVector.hh"

StuDraw3DMuEvent *StuDraw3DMuEvent::gMuEventDisplay = new StuDraw3DMuEvent();

ClassImp(StuDraw3DMuEvent)
           
  ////////////////////////////////////////////////////////////////////////
  //
  //  Class StuDraw3DMuEvent - to draw the 3D StEvent primitives like StMuTrack
  //  decoratated with the STAR detector geometry
  //
  //  It provides the simple way to visualize the event 
  //  primitives in 3D quickly against of the STAR detector 
  //  geometry.
  //  <begin_html> <img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Draw3DClass.png">end_html
  //
  ////////////////////////////////////////////////////////////////////////
//___________________________________________________
StuDraw3DMuEvent::StuDraw3DMuEvent( const char *detectorName,TVirtualPad *pad): 
StDraw3D(detectorName,pad)
{
   // The detectorName is a comma separated list of the OpenInventor files with no extension
   // For all names on the list one should provide the iv file with the "iv extension:
   //                         <name>.iv
   if (!gMuEventDisplay) gMuEventDisplay = this;
}

//___________________________________________________
StuDraw3DMuEvent::~StuDraw3DMuEvent()
{
   if (gMuEventDisplay == this) gMuEventDisplay = 0;
}

///
/// Tracks(StTrackType type) add all tracks opf the
///  seclted type from the current event to Event Display
///
//___________________________________________________
void StuDraw3DMuEvent::Tracks(StTrackType type)
{
   Int_t n_prim=0;
   Int_t n_glob=0;
   TObjArray *globTracks = 0;
   TObjArray *primTracks = 0;
   if (type == global && ( globTracks= StMuDst::globalTracks() ) ){
      n_glob=StMuDst::GetNGlobalTrack();
   } else if (primTracks = StMuDst::primaryTracks() ) {
      n_prim=StMuDst::GetNPrimaryTrack();
   }

   Int_t i_track=0;
   while (i_track < n_prim) {
       StMuTrack &pr_track = *(StMuTrack *)primTracks->UncheckedAt(i_track++);
       Track(pr_track,kPrimaryTrack);
       cout << ".";
   }
   i_track=0;
   while (i_track < n_glob) {
       StMuTrack &pr_track = *(StMuTrack *)globTracks->UncheckedAt(i_track++);
       Track(pr_track,kGlobalTrack);
       cout << "+";
   }
   cout << n_prim << "primary and " << n_glob << "global tracks have been rendered" << endl;;

}

//___________________________________________________
TObject *StuDraw3DMuEvent::Track(const StMuTrack &track, Color_t col,Style_t sty,Size_t siz)
{

   StHelixHelper trPnt(track.helix(),track.outerHelix(),track.length());
   Int_t size;
   Float_t *xyz = trPnt.GetPoints(size);
   TObject *l = Line(size,xyz,col,sty,siz);
   SetModel((TObject*)&track);
   return l;
}

//___________________________________________________
TObject *StuDraw3DMuEvent::Track(const StMuTrack &track, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Track(track, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
TObject *StuDraw3DMuEvent::TrackInOut(const StMuTrack &track, Bool_t in
                  ,  Color_t col,  Style_t sty,  Size_t siz)
{
   // to be completed yet
   const StThreeVectorF  &pnt = in ? track.firstPoint() : track.lastPoint();

   return Point(pnt.x(),pnt.y(),pnt.z(), col,sty,siz);
}

//___________________________________________________
TObject *StuDraw3DMuEvent::TrackInOut(const StMuTrack &track, EDraw3DStyle sty,Bool_t in)
{
   const StDraw3DStyle &style =  Style(sty);
   return TrackInOut(track, in, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
StuDraw3DMuEvent *StuDraw3DMuEvent::Display(){ return gMuEventDisplay;}

