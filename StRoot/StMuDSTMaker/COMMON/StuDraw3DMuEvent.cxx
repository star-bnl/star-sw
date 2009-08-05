// $Id: StuDraw3DMuEvent.cxx,v 1.1 2009/08/05 00:22:29 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008
#include "StuDraw3DMuEvent.h"
#include "Gtypes.h"
#include "StHelixHelper.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

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
#if 0
   // to be completed yet
   StInnOutPoints trInOut(track,in);
   return Points(trInOut.Size(),trInOut.GetXYZ(0),col,sty,siz);
#else 
   if (col && sty && siz && track.length())  {}
   return 0;
#endif
}

//___________________________________________________
TObject *StuDraw3DMuEvent::TrackInOut(const StMuTrack &track, EDraw3DStyle sty,Bool_t in)
{
   const StDraw3DStyle &style =  Style(sty);
   return TrackInOut(track, in, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
StuDraw3DMuEvent *StuDraw3DMuEvent::Display(){ return gMuEventDisplay;}

