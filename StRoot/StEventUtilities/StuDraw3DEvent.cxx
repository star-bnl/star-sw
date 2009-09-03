// $Id: StuDraw3DEvent.cxx,v 1.17 2009/09/03 16:49:14 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008
#include "StuDraw3DEvent.h"
#include "TVirtualPad.h"
#include "TColor.h"
#include "StEventHelper.h"
#include "StEvent.h"
#include "StTrack.h"
#include "StHit.h"
#include "StTpcHit.h"
#include "StTrackNode.h"
#include "StTrackGeometry.h"
#include "StTpcHitCollection.h"
#include "StMeasuredPoint.h"
#include "TMath.h"
#include "StTrackDetectorInfo.h"


//! StuDraw3DEvent( const char *detectorName,TVirtualPad *pad) ctor
/*!  
         \param detectorName (default = "TPC") - the names of the STAR detectors 
                                                 to be used as the "event primitives" background.
                               The detectorName is a comma separated list of the OpenInventor files 
                               with no extension\n
                               For all names on the list one should provide the iv file with 
                               the "iv" extension:\n
                                     \code   <name>.iv \endcode
         \param pad (default = 0) - The ROOT TPad to be used to render the event wired view
\note 
   If this is the first instance of the class then the global pointer to the 
         current "display" is to be set too \sa Display()
         \sa Plot3Dtracks.C
*/
//___________________________________________________
StuDraw3DEvent::StuDraw3DEvent( const char *detectorName,TVirtualPad *pad):StDraw3D(detectorName,pad)
{
   // The detectorName is a comma separated list of the OpenInventor files with no extension
   // For all names on the list one should provide the iv file with the "iv extension:
   //                         <name>.iv
   if (!gEventDisplay) gEventDisplay = this;
}

//! ~StuDraw3DEvent( ) dtor
/*! 
    Reset the global  \c gEventDisplay pointer to the current display if the current display is \c this 
*/
//___________________________________________________
StuDraw3DEvent::~StuDraw3DEvent()
{
   if (gEventDisplay == this) gEventDisplay = 0;
}

//! Add \a track to the display list with the \a col color \sty sty and \siz size if provided
/*! 
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
*/
//___________________________________________________
TObject *StuDraw3DEvent::Track(const StTrack &track, Color_t col,Style_t sty,Size_t siz)
{
   StTrackHelper trPnt(&track);
   Int_t size;
   Float_t *xyz = trPnt.GetPoints(size);
   TObject *l = Line(size,xyz,col,sty,siz);
   SetModel((TObject*)&track);
   return l;
}

//! This is an overloaded member function, provided for convenience.
/*! Add \a track to the display list with the \a sty style if provided 
 */
//___________________________________________________
TObject *StuDraw3DEvent::Track(const StTrack &track, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Track(track, style.Col(),style.Sty(),style.Siz() );
}

//! Add one \a hit to the display list with the \a col color \sty sty and \siz size if provided
/*! Draw the StMeasuredPoint, StHit, StVertex with the graphical attribute provided
   \param   col - ROOT marker color (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   sty - ROOT marker style (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   siz - ROOT marker size (see:  http://root.cern.ch/root/html/TAttMarker.html )
 */
//___________________________________________________
TObject *StuDraw3DEvent::Hit(const StMeasuredPoint &hit
                  ,  Color_t col,  Style_t sty,  Size_t siz)
{
   // Draw the StMeasuredPoint, StHit, StVertex with the graphical attribute provided
   const StThreeVectorF& position = hit.position();
   TObject *p = Point(position.x(),position.y(),position.z(),col,sty,siz);
   SetModel((TObject*)&hit);
   return p;
}

//! This is an overloaded member function, provided for convenience.
/*! Add \a hit to the display list with the \a sty style if provided 
 */
//___________________________________________________
TObject *StuDraw3DEvent::Hit(const StMeasuredPoint &hit, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Hit(hit, style.Col(),style.Sty(),style.Siz() );
}

//! This is an overloaded member function, provided for convenience.
/*! Add \a vertex to the display list with the \a col color \sty sty and \siz size if provided
 */
//___________________________________________________
TObject *StuDraw3DEvent::Vertex(const StMeasuredPoint &vertex
                  ,  Color_t col,  Style_t sty, Size_t siz)
{
   return Hit(vertex,col,sty,siz);
}

//! This is an overloaded member function, provided for convenience.
/*! Add \a vtx to the display list with the \a sty style if provided 
   \param   col - ROOT marker color (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   sty - ROOT marker style (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   siz - ROOT marker size (see:  http://root.cern.ch/root/html/TAttMarker.html )
*/
//___________________________________________________
TObject *StuDraw3DEvent::Vertex(const StMeasuredPoint &vtx, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Vertex(vtx, style.Col(),style.Sty(),style.Siz() );
}

//! Add all \b good hits for the given \a track to the display
/*! the \c style and \c size vizual attributes are defined by the kUsedHit style
    the \c color is defined by the tracl \c pt 
    \code double pt = track.geometry()->momentum().perp(); \endcode
 */
//___________________________________________________
void  StuDraw3DEvent::Hits(const StTrack &track)
{
    // Draw hits the "track" was built from
    // with the graphical attributes defined by "track"
   if (track.flag() > 0 &&  track.detectorInfo()&& !track.bad() ) {
      const Int_t lightness    = 50;
      const Int_t saturation   = 100;
      Int_t hue  = 0;

      Style_t sty = Style(kUsedHit).Sty();
      Size_t  siz = Style(kUsedHit).Siz();
      double pt = track.geometry()->momentum().perp();
      hue = Int_t(256.*(1.-pt/1.5)); //color code from StuPostscript
      if (pt > 1.5 ) hue = 0;
      Int_t r,g,b;
      TColor::HLS2RGB(hue, lightness, saturation, r, g, b);
      // Normalize
      float factor = 1./TMath::Sqrt(1.*r*r+1.*g*g+1.*b*b);
      Color_t trackColor =  TColor::GetColor(r*factor,g*factor,b*factor);
      Hits(track, trackColor,sty,siz);
   }
}

//! Add all hits of the given \a track to the display list with the \a col color \sty sty and \siz size if provided
/*!
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
*/
//___________________________________________________
void  StuDraw3DEvent::Hits(const StTrack &track
                  ,  Color_t col
                  ,  Style_t sty
                  ,  Size_t siz )
{
    // Draw hits the "track" was built from
    // with the graphical attributes "col", "sty", "siz"
   std::vector<float> hitPoints;
   const StPtrVecHit& trackHits = track.detectorInfo()->hits(kTpcId);
   for (unsigned int m=0; m<trackHits.size(); m++) {
      StHit *hit = trackHits[m];
      hitPoints.push_back( hit->position().x());
      hitPoints.push_back( hit->position().y());
      hitPoints.push_back( hit->position().z());
   }
   std::vector<float>::iterator xyz = hitPoints.begin();
   Points(hitPoints.size()/3,&*xyz,col,sty,siz); 
}

//! This is an overloaded member function, provided for convenience.
/*! Add all hits fof the given \a track to the display list with the \a sty style if provided
 */
//___________________________________________________
void  StuDraw3DEvent::Hits(const StTrack &track,  EDraw3DStyle sty)
{
   // Draw hits the "track" was built from
   // using the "sty" graphical style provided
   const StDraw3DStyle &style =  Style(sty);
   Hits(track, style.Col(),style.Sty(),style.Siz());
}

//! Add all \a in  point of the give \a track to the display list with the \a col color \sty sty and \siz size if provided
/*! 
   \param track - reference to the StTrack object from StEvent data structure
   \param in      flag \c true (default) is to be set to ad the \a track  \c in point \n
                      \c false is to be set to add the \a track  \c out point to the list
   \param   col - ROOT marker color (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   sty - ROOT marker style (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   siz - ROOT marker size (see:  http://root.cern.ch/root/html/TAttMarker.html )
*/
//___________________________________________________
TObject *StuDraw3DEvent::TrackInOut(const StTrack &track, Bool_t in
                  ,  Color_t col,  Style_t sty,  Size_t siz)
{
   StInnOutPoints trInOut(&track,in);
   return Points(trInOut.Size(),trInOut.GetXYZ(0),col,sty,siz);
}

//! This is an overloaded member function, provided for convenience.
/*! 
   \param track - reference to the StTrack object from StEvent data structure
   \param in    - flag \c true (default) is to be set to ad the \a track  \c in point \n
                      \c false is to be set to add the \a track  \c out point to the list
   \param   sty - EDraw3DStyle visual style of this object.
*/
//___________________________________________________
TObject *StuDraw3DEvent::TrackInOut(const StTrack &track, EDraw3DStyle sty,Bool_t in)
{
   const StDraw3DStyle &style =  Style(sty);
   return TrackInOut(track, in, style.Col(),style.Sty(),style.Siz() );
}

//___________________________________________________
void StuDraw3DEvent::Tracks(const StEvent* event, StTrackType type)
{
   Hits(event,kTracksOnly,type);
}

//___________________________________________________
void StuDraw3DEvent::Tracks(const StSPtrVecTrackNode &theNodes
      , StTrackType type)
{
   // const double lineWidth    = 0.4;
 //  const double sstep        = mFewerPointsPerTrack ? 4 : 1;
   const Int_t lightness    = 50;
   const Int_t saturation   = 100;
   Int_t hue  = 0;

   //
   //  Handle options
   //
   // int  trackGrayLevel = mBlackBackground ? 1 : 0;

   StTrack *track;
   StThreeVectorD p;
   unsigned int i;
   // double pt;
   //    int  minFitPoints = mAllGlobalTracks ? 1 : mMinFitPoints;
   for (i=0; i<theNodes.size(); i++) {
      track = theNodes[i]->track(type);
      if (track && track->flag() > 0
         //  &&   track->fitTraits().numberOfFitPoints(kTpcId) >= minFitPoints) 
         )
      {
         double pt = track->geometry()->momentum().perp();
         hue = Int_t(256.*(1.-pt/1.5)); //color code from StuPostscript
         if (pt > 1.5 ) hue = 0;
         Int_t r,g,b;
         TColor::HLS2RGB(hue, lightness, saturation, r, g, b);
         // Normalize
         float factor = 1./TMath::Sqrt(1.*r*r+1.*g*g+1.*b*b);
         Color_t trackColor =  TColor::GetColor(r*factor,g*factor,b*factor);
         Track(*track,trackColor);
      }
   }
}

//___________________________________________________
void StuDraw3DEvent::Hits(const StEvent *event,EStuDraw3DEvent trackHitsOnly, StTrackType type)
{
   if (!event) return; // no event
   const StTpcHitCollection* hits = event->tpcHitCollection();
   if (!hits) return; // there is no hits
   unsigned int m, n, h;
   if (trackHitsOnly != kUnusedHitsOnly) {
      const Int_t lightness    = 50;
      const Int_t saturation   = 100;
      Int_t hue  = 0;

      Style_t sty = Style(kUsedHit).Sty();
      Size_t  siz = Style(kUsedHit).Siz();
      Style_t styPnt = Style(kTrackBegin).Sty();
      Size_t  sizPnt = Style(kTrackBegin).Siz();

      const StSPtrVecTrackNode& theNodes = event->trackNodes();
      for (unsigned int i=0; i<theNodes.size(); i++) {
         StTrack *track = theNodes[i]->track(type);
         if (track &&  track->flag() > 0
                   &&  track->detectorInfo() 
                   && !track->bad() 
         //  &&   track->fitTraits().numberOfFitPoints(kTpcId) >= minFitPoints) 
         )
        {
            double pt = track->geometry()->momentum().perp();
            hue = Int_t(256.*(1.-pt/1.5)); //color code from StuPostscript
            if (pt > 1.5 ) hue = 0;
            Int_t r,g,b;
            TColor::HLS2RGB(hue, lightness, saturation, r, g, b);
            // Normalize
            float factor = 1./TMath::Sqrt(1.*r*r+1.*g*g+1.*b*b);
            Color_t trackColor =  TColor::GetColor(r*factor,g*factor,b*factor);
            if ( trackHitsOnly != kUsedHits) {
               Track(*track,trackColor);
               TrackInOut(*track, true,  trackColor,  styPnt, sizPnt);
               TrackInOut(*track, false, trackColor,  styPnt, sizPnt);
            }
            if ( trackHitsOnly != kTracksOnly) {
               Hits(*track,trackColor,sty,siz);
               if (trackHitsOnly == kUsedHits) SetModel(track);
           }
        }
     }
   } else {
      const StTpcHit *hit;
      std::vector<float> hitPoints;
      for (n=0; n<hits->numberOfSectors(); n++) {
         for (m=0; m<hits->sector(n)->numberOfPadrows(); m++) { 
            for (h=0; h<hits->sector(n)->padrow(m)->hits().size(); h++) {
               hit = hits->sector(n)->padrow(m)->hits()[h];
                hitPoints.push_back( hit->position().x());
                hitPoints.push_back( hit->position().y());
                hitPoints.push_back( hit->position().z());
            }
         }
      }
      std::vector<float>::iterator xyz = hitPoints.begin();
      Points(hitPoints.size()/3,&*xyz,kUnusedHit);
      SetComment("Unused TPC hits");
   }
}

//___________________________________________________
StuDraw3DEvent *StuDraw3DEvent::Display(){ return gEventDisplay;}

StuDraw3DEvent *gEventDisplay = new StuDraw3DEvent();

ClassImp(StuDraw3DEvent)

