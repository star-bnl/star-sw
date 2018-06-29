// $Id: StuDraw3DEvent.cxx,v 1.35 2018/06/29 17:21:24 perev Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008
#include "StuDraw3DEvent.h"
#include "TSystem.h"
#include "TVirtualPad.h"
#include "TColor.h"
#include "StEventHelper.h"
#include "StEvent.h"
#include "StTrack.h"
#include "StGlobalTrack.h"
#include "StHit.h"
#include "StTpcHit.h"
#include "StFtpcHit.h"
#include "StEmcRawHit.h"
#include "StTrackNode.h"
#include "StTrackGeometry.h"
#include "StTpcHitCollection.h"
#include "StEmcCollection.h"
#include "StFtpcHitCollection.h"
#include "StFtpcSectorHitCollection.h"
#include "StFtpcPlaneHitCollection.h"
#include "StEmcDetector.h"
#include "StEmcModule.h"
#include "StMeasuredPoint.h"
#include "TMath.h"
#include "StTrackDetectorInfo.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEventHitIter.h"
#include "StMessMgr.h"


//____________________________________________________________________________________
//! StuDraw3DEvent( const char *detectorName,TVirtualPad *pad) ctor
/*!  
         \param detectorName (default = "TPC") - the names of the STAR detectors 
                                                 to be used as the "event primitives" background.
                               The detectorName is a comma separated list of the OpenInventor files 
                               with no extension\n
                               For all names on the list one should provide the iv file with 
                               the "iv" extension:\n
                                     \code   <name>.iv \endcode   
         \param detectorName = 0  - no detector geometry is to be rendered
         \param pad (default = 0) - The ROOT TPad to be used to render the event wired view
\htmlonly
<table>
<tr>
<th>Event over detector geometry
<th>Event with no detector geometry
</tr>
<tr>
<td><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EventDisplayWGeom.png">
<td><img src="http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/EventDisplayWOGeom.png">
</tr></table>
\endhtmlonly
\sa StDraw3D::StDraw3D( const char *detectorName,TVirtualPad *pad) ctor
\note 
   If this is the first instance of the class then the global pointer to the 
         current "display" is to be set too \sa Display()
         \sa Plot3Dtracks.C
*/
ClassImp(StuDraw3DEvent)
//___________________________________________________
StuDraw3DEvent::StuDraw3DEvent( const char *detectorName,TVirtualPad *pad):StDraw3D(detectorName,pad)
{
   // The detectorName is a comma separated list of the OpenInventor files with no extension
   // For all names on the list one should provide the iv file with the "iv extension:
   //                         <name>.iv
   if (!gEventDisplay) gEventDisplay = this;
}

//____________________________________________________________________________________
//! ~StuDraw3DEvent( ) dtor
/*! 
    Reset the global  \c gEventDisplay pointer to the current display if the current display is \c this 
*/
//____________________________________________________________________________________
StuDraw3DEvent::~StuDraw3DEvent()
{
   if (gEventDisplay == this) gEventDisplay = 0;
}
//____________________________________________________________________________________
//! Add EMC hit defined \a emcHitsSoftId to the display list with the \a col color \a sty and \a size if provided
/*! 
   \param   emcHitsSoftId - StEmcRawHit soft ID that one wants to be present as the ROOT TTRAP 
                    object with ROOT visual attributes \a col \a sty \a siz
   \param   col - Tower color ( see: http://root.cern.ch/root/html/TAttFill.html ) 
   \param   sty - Tower style ( see: http://root.cern.ch/root/html/TAttFill.html ) 
   \param   siz - Tower size (cm) ( \sa StDraw3D::Tower( float radius, const StarRoot::StEta &eta,float phi,float dphi, Color_t col,Style_t sty, Size_t siz) )
   \return - a pointer to the ROOT "view" TObject of \a emcHit model
*/
//____________________________________________________________________________________
TObject *StuDraw3DEvent::EmcHit(Int_t emcHitsSoftId, Color_t col,Style_t sty,Size_t siz, const char *detId)
{  
   TObject *model = 0;
   StEmcGeom *emcGeom =StEmcGeom::getEmcGeom(detId);
   if (emcGeom) {
      Int_t softId=emcHitsSoftId;
      Float_t eta;
      Float_t phi;
      emcGeom->getEtaPhi(softId,eta,phi);
      Float_t etaStep = 1.0/emcGeom->NEta();
      Float_t phiStep = TMath::Pi()/60; 
      static int entries = 0;
        //if (entries) return 0;
        // printf(" m=%d, e=%d, s=%d; eta=%e deta=%e phi=%e dphi=%e id %d\n",m, e, s,eta,etaStep ,phi, phiStep, softId);
      entries++;
      model  = Tower(emcGeom->Radius(), StarRoot::StEta(eta,etaStep)
                         , phi, phiStep
                         , col,sty+(strcmp(detId,"bemc")?0:kBarrelStyle),siz);
   } else {
      LOG_ERROR <<  __FILE__ << ":  there is no geometry information for \"" << detId << "\"" << endm;
   }
   return model;
}
//____________________________________________________________________________________
//! Add \a emcHit to the display list with the \a col color \a sty and \a size if provided
/*! 
   \param   emcHit - StEmcRawHit reference one wants to be present as the ROOT TTRAP 
                    object with ROOT visual attributes \a col \a sty \a siz
   \param   col - Tower color ( see: http://root.cern.ch/root/html/TAttFill.html ) 
   \param   sty - Tower style ( see: http://root.cern.ch/root/html/TAttFill.html ) 
   \param   siz - Tower size (cm) ( \sa StDraw3D::Tower( float radius, const StarRoot::StEta &eta,float phi,float dphi, Color_t col,Style_t sty, Size_t siz) )
   \return - a pointer to the ROOT "view" TObject of \a emcHit model
*/
//____________________________________________________________________________________
TObject *StuDraw3DEvent::EmcHit(const StEmcRawHit &emcHit, Color_t col,Style_t sty,Size_t siz, const char *detId)
{  
   TObject *model = 0;
   if (!detId || !detId[0]) detId = "bemc";
   StEmcGeom *emcGeom =StEmcGeom::getEmcGeom(detId);
   if (emcGeom) {
      int m, e, s;
      Int_t softId;
      emcHit.modEtaSub(m,e,s);
      emcGeom->getId(m,e,s,softId);
      EmcHit(softId, col,sty,siz,detId);
      SetModel((TObject*)&emcHit);
   } else {
      LOG_ERROR <<  __FILE__ << ":  there is no geometry information for \"" << detId << "\"" << endm;
   }
   return model;
}

//____________________________________________________________________________________
//! Add all emcHits those can pass the internal filter from the given detector \a detId type from the \a event to the display list.
/*! 
    \param  event - The pointer to the instance of the StEvent class
    \param detId  - The Emc detector name as defined by StEmcGeom::getDetNumFromName(const Char_t *cdet) method
    \image html http://www.star.bnl.gov/public/comp/vis/StDraw3D/examples/Run.dAu.2008.9025036.666030.BEMC.hits.png "Event 666030 from Run 9025036 produced by Ed.C macros"  
    \note You normally do not need to use this method directly. It is just a pattern you can follow to customize the macro Ed.C to 
    use your own coloring schema and selection criteria.
*/
//______________________________________________________________________________________
void StuDraw3DEvent::EmcHits(const StEvent* event,const char *detId) 
{
   Color_t colorResponce = 0;
   if (!detId || !detId[0]) detId = "bemc";
   StEmcCollection* emcC =(StEmcCollection*)event->emcCollection();
   if (emcC) {
      StEmcDetector* det = emcC->detector( strcmp(detId,"bemc") ? kEndcapEmcTowerId : kBarrelEmcTowerId); 
      if (det) {
      for(unsigned int md=1; md <=det->numberOfModules(); md++) {
         StEmcModule*  module=det->module(md);
         StSPtrVecEmcRawHit&   hit=  module->hits();
     
         for(unsigned int ih=0;ih < hit.size();ih++){
            StEmcRawHit *h=hit[ih];
            double  rawAdc=h->adc()-5; // raw ADC 
            float energy =  h->energy();
	         Style_t style=0; // solid
//	         energy = (rawAdc+5)/5;
//	         printf(" EmcHits %d adc = %e energy = %e\n", ih,rawAdc, energy);
            if ( rawAdc>0  && energy > 0  && energy < 30) {
               // If edep less then MIP (~300 MeV), 60GeV <-> 4096 ADC counts
               if (  energy  < 0.3)   {   
                 colorResponce = kBlue; 
                // style = 4001;                 //wireframe 
               // If edep large then MIP but less then 1 GeV 
               } else if (  energy  < 1.0 ) colorResponce = kGreen;
               // If between 1 GeV and lowest HT threshold (4 GeV for Run7)
               else if (  energy  < 4.0 )   colorResponce = kYellow;
               // If above lowest HT thershold
               else                         colorResponce = kRed;
               if (energy > 1.0) {
                //  printf(" Emchit adc = %e energy = %e\n",rawAdc, energy);
               }
               static const double maxSize =  400.; // (cm)
               static const double scale   =  200.; // (cm/Gev)
               double size =(energy > 0.3 ? scale : scale/30.)*energy;
               if (size > maxSize)  size = maxSize ;
               EmcHit(*h,colorResponce,style, size,detId);
            }
         }
      }
   }
   }
}

//____________________________________________________________________________________
//! Add \a track to the display list with the \a col color \a sty and \a size if provided
/*! 
   \param   track - StTrack reference one wants to be present as the ROOT TPolyLine3D 
                    object with ROOT visual attributes \a col \a sty \a siz
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \return - a pointer to the ROOT "view" TObject of \a track model
*/
//____________________________________________________________________________________
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
/*! Add \a track to the display list with the \a sty pre-defined style
 */
//___________________________________________________
TObject *StuDraw3DEvent::Track(const StTrack &track, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Track(track, style.Col(),style.Sty(),style.Siz() );
}
//____________________________________________________________________________________
TObject *StuDraw3DEvent::Track(const StGlobalTrack &track, Color_t col,Style_t sty,Size_t siz)
{
   StTrackHelper trPnt(&track);
   Int_t size;
   Float_t *xyz = trPnt.GetPoints(size);
   TObject *l = Line(size,xyz,col,sty,siz);
   SetModel((TObject*)&track);
   return l;
}

//! This is an overloaded member function, provided for convenience.
/*! Add \a track to the display list with the \a sty pre-defined style
 */
//___________________________________________________
TObject *StuDraw3DEvent::Track(const StGlobalTrack &track, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Track(track, style.Col(),style.Sty(),style.Siz() );
}

//____________________________________________________________________________________
//! Add one \a hit to the display list with the \a col color \a sty and \a siz size if provided
/*! Draw the StMeasuredPoint, StHit, StVertex with the graphical attribute provided
   \param   hit - The reference to StMeasuredPoint  STAR object 
                  one wants to be present as ROOT TMarker3D 
   \param   col - ROOT marker color (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   sty - ROOT marker style (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   siz - ROOT marker size (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \return - a pointer to the ROOT "view" TObject of \a hit model
*/
//____________________________________________________________________________________
TObject *StuDraw3DEvent::Hit(const StMeasuredPoint &hit
                  ,  Color_t col,  Style_t sty,  Size_t siz)
{
   // Draw the StMeasuredPoint, StHit, StVertex with the graphical attribute provided
   const StThreeVectorF& position = hit.position();
   TObject *p = Point(position.x(),position.y(),position.z(),col,sty,siz);
   SetModel((TObject*)&hit);
   return p;
}

//____________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a hit to the display list with the \a sty pre-defined style if provided 
 */
//____________________________________________________________________________________
TObject *StuDraw3DEvent::Hit(const StMeasuredPoint &hit, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Hit(hit, style.Col(),style.Sty(),style.Siz() );
}

//____________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a vertex to the display list with the \a col color \a sty and \a siz size if provided
   \param   vertex - The reference to StMeasuredPoint  STAR object 
                  one wants to be present as ROOT TMarker3D 
   \param   col - ROOT marker color (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   sty - ROOT marker style (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   siz - ROOT marker size (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \return - a pointer to the ROOT "view" TObject of \a vertex model
 */
//____________________________________________________________________________________
TObject *StuDraw3DEvent::Vertex(const StMeasuredPoint &vertex
                  ,  Color_t col,  Style_t sty, Size_t siz)
{
   return Hit(vertex,col,sty,siz);
}

//____________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add \a vtx to the display list with the \a sty style if provided 
   \param   vtx - The reference to StMeasuredPoint  STAR object 
                  one wants to be present as ROOT TMarker3D 
   \param   sty - EDraw3DStyle EventDisplay pre-defined style 
   \return - pointer to the ROOT "view" TObject of \a vertex model
*/
//____________________________________________________________________________________
TObject *StuDraw3DEvent::Vertex(const StMeasuredPoint &vtx, EDraw3DStyle sty)
{
   const StDraw3DStyle &style =  Style(sty);
   return Vertex(vtx, style.Col(),style.Sty(),style.Siz() );
}

//____________________________________________________________________________________
//! Add all \b good hits for the given \a track to the display
/*! the \c style and \c size vizual attributes are defined by the kUsedHit style
    the \c color is defined by the track \c pt 
    \code double pt = track.geometry()->momentum().perp(); \endcode
   \return - a pointer to the ROOT "view" TObject of \a vtx model
 */
//____________________________________________________________________________________
void  StuDraw3DEvent::Hits(const StTrack &track)
{
    // Draw hits the "track" was built from
    // with the graphical attributes defined by "track"
   if (track.flag() > 0 &&  track.detectorInfo()&& !track.bad() ) {
      Style_t sty = Style(kUsedHit).Sty();
      Size_t  siz = Style(kUsedHit).Siz();
      double pt = track.geometry()->momentum().perp();
      Hits(track, StDraw3DStyle::Pt2Color(pt),sty,siz);
   }
}

//____________________________________________________________________________________
//! Add all hits of the given \a track to the display list with the \a col color \a sty and \a siz size if provided
/*!
   \param   track - StTrack reference one wants its "used" hits to be present as the ROOT TPolyMarker3D 
                    object with ROOT visual attributes \a col \a sty \a size
   \param   col - ROOT line color ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   sty - ROOT line style ( see: http://root.cern.ch/root/html/TAttLine.html ) 
   \param   siz - ROOT line width ( see: http://root.cern.ch/root/html/TAttLine.html ) 
*/
//____________________________________________________________________________________
void  StuDraw3DEvent::Hits(const StTrack &track
                  ,  Color_t col
                  ,  Style_t sty
                  ,  Size_t siz )
{
    // Draw hits the "track" was built from
    // with the graphical attributes "col", "sty", "siz"
   std::vector<float> hitPoints;
   const StPtrVecHit& trackHits = track.detectorInfo()->hits(kTpcId);
   unsigned int m=0;
   StHit *hit = 0;
   for (m=0; m<trackHits.size(); m++) {
      hit = trackHits[m];
      hitPoints.push_back( hit->position().x());
      hitPoints.push_back( hit->position().y());
      hitPoints.push_back( hit->position().z());
   }
   {
      std::vector<float>::iterator xyz = hitPoints.begin();
      Points(hitPoints.size()/3,&*xyz,col,sty,siz); 
   }
   // printf(" Tpc hits # %d\n", m);

   const StPtrVecHit& trackWestHits = track.detectorInfo()->hits(kFtpcWestId);

   hitPoints.clear();
   for (m=0; m<trackWestHits.size(); m++) {
      hit = trackWestHits[m];
      hitPoints.push_back( hit->position().x());
      hitPoints.push_back( hit->position().y());
      hitPoints.push_back( hit->position().z());
   }
   {
      std::vector<float>::iterator xyz = hitPoints.begin();
      Points(hitPoints.size()/3,&*xyz,col,sty,siz); 
   }
   // printf(" Ftpc West hits # %d\n", m);

   hitPoints.clear();
   const StPtrVecHit& trackEastHits = track.detectorInfo()->hits(kFtpcEastId);
   for (m=0; m<trackEastHits.size(); m++) {
      hit = trackEastHits[m];
      hitPoints.push_back( hit->position().x());
      hitPoints.push_back( hit->position().y());
      hitPoints.push_back( hit->position().z());
   }
   {
      std::vector<float>::iterator xyz = hitPoints.begin();
      Points(hitPoints.size()/3,&*xyz,col,sty,siz); 
   }
   // printf(" Ftpc East hits # %d\n", m);
}

//____________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add all hits fof the given \a track to the display list with the \a sty pre-defined style if provided
 */
//____________________________________________________________________________________
void  StuDraw3DEvent::Hits(const StTrack &track,  EDraw3DStyle sty)
{
   // Draw hits the "track" was built from
   // using the "sty" graphical style provided
   const StDraw3DStyle &style =  Style(sty);
   Hits(track, style.Col(),style.Sty(),style.Siz());
}

//____________________________________________________________________________________
//! Add  the \a in  point of the given \a track to the display list with the \a col color \a sty and \a siz size if provided
/*! 
   \param track - reference to the StTrack object from StEvent data structure
   \param in      flag \c true (default) is to be set to add the \a track  \c in point \n
                      \c false is to be set to add the \a track  \c out point to the list
   \param   col - ROOT marker color (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   sty - ROOT marker style (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \param   siz - ROOT marker size (see:  http://root.cern.ch/root/html/TAttMarker.html )
   \return - a pointer to the ROOT "view" TObject of star/end point of \a track model
*/
//____________________________________________________________________________________
TObject *StuDraw3DEvent::TrackInOut(const StTrack &track, Bool_t in
                  ,  Color_t col,  Style_t sty,  Size_t siz)
{
   StInnOutPoints trInOut(&track,in);
   return Points(trInOut.Size(),trInOut.GetXYZ(0),col,sty,siz);
}

//____________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! 
   \param track - reference to the StTrack object from StEvent data structure
   \param in    - flag \c true (default) is to be set to ad the \a track  \c in point \n
                      \c false is to be set to add the \a track  \c out point to the list
   \param   sty - EDraw3DStyle pre-defined visual style of this object.
   \return - a pointer to the ROOT "view" TObject of star/end point of \a track model
*/
//____________________________________________________________________________________
TObject *StuDraw3DEvent::TrackInOut(const StTrack &track, EDraw3DStyle sty,Bool_t in)
{
   const StDraw3DStyle &style =  Style(sty);
   return TrackInOut(track, in, style.Col(),style.Sty(),style.Siz() );
}


//____________________________________________________________________________________
//! Add all tracks of the given \a type from the \a event to the display list.
/*! 
    \param  event - The pointer to the instance of the StEvent class
    \param type   - The StTrack type as defined by StTrackType class

    The \c style and \c size of the visual attributes are defined by the StTrackType \c global style
    the \c color is defined by the track \c pt 
    \code double pt = track.geometry()->momentum().perp(); \endcode
 */
//____________________________________________________________________________________
void StuDraw3DEvent::Tracks(const StEvent* event, StTrackType type)
{
   Hits(event,kTracksOnly,type);
}

//____________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! Add all tracks of the given \a type from the \a theNodes to the display list.
    \param theNodes  - The reference to the StSPtrVecTrackNode track container from the StEvent object    
    \param type      - The StTrack type as defined by StTrackType class
    
    The \c style and \c size of the visual attributes are defined by the StTrackType \c global style
    the \c color is defined by the track \c pt 
    \code double pt = track.geometry()->momentum().perp(); \endcode
 */
//____________________________________________________________________________________
void StuDraw3DEvent::Tracks(const StSPtrVecTrackNode &theNodes
      , StTrackType type)
{
   StTrack *track;
   StThreeVectorD p;
   unsigned int i;
   // double pt;
   //    int  minFitPoints = mAllGlobalTracks ? 1 : mMinFitPoints;
   for (i=0; i<theNodes.size(); i++) {
     track = (StTrack *) theNodes[i]->track(type);
      if (track && track->flag() > 0
         //  &&   track->fitTraits().numberOfFitPoints(kTpcId) >= minFitPoints) 
         )
      {
         double pt = track->geometry()->momentum().perp();
         Track(*track,StDraw3DStyle::Pt2Color(pt));
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
      Style_t sty = Style(kUsedHit).Sty();
      Size_t  siz = Style(kUsedHit).Siz();
      Style_t styPnt = Style(kTrackBegin).Sty();
      Size_t  sizPnt = Style(kTrackBegin).Siz();

      const StSPtrVecTrackNode& theNodes = event->trackNodes();
      for (unsigned int i=0; i<theNodes.size(); i++) {
	StTrack *track = (StTrack *)theNodes[i]->track(type);
         if (track &&  track->flag() > 0
                   &&  track->detectorInfo() 
                   && !track->bad() 
         //  &&   track->fitTraits().numberOfFitPoints(kTpcId) >= minFitPoints) 
         )
        {
            double pt = track->geometry()->momentum().perp();
            Color_t trackColor = StDraw3DStyle::Pt2Color(pt);
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
void StuDraw3DEvent::FtpcHits(const StEvent *event,EStuDraw3DEvent trackHitsOnly, StTrackType type)
{
   if (!event) return; // no event
   unsigned int m, n, h;
   if (trackHitsOnly != kUnusedHitsOnly) {
      Style_t sty    = Style(kUsedHit).Sty();
      Size_t  siz    = Style(kUsedHit).Siz();
      Style_t styPnt = Style(kTrackBegin).Sty();
      Size_t  sizPnt = Style(kTrackBegin).Siz();
      int trackCounter = 0;
      const StSPtrVecTrackNode& theNodes = event->trackNodes();
      for (unsigned int i=0; i<theNodes.size(); i++) {
	StTrack *track = (StTrack *) theNodes[i]->track(type);
         if (track &&  track->flag() > 0
                   &&  track->detectorInfo()                
                   &&  ( track->detectorInfo()->numberOfPoints(kFtpcWestId) || track->detectorInfo()->numberOfPoints(kFtpcEastId)  )
                   && !track->bad() 
         )
        {
           ++trackCounter;
            double pt = track->geometry()->momentum().perp();
            Color_t trackColor = StDraw3DStyle::Pt2Color(pt);
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
     printf(" Ftpc tracks total : %d\n", trackCounter);
   } else {
      const StHit *hit= 0;
      std::vector<float> hitPoints;
      const StFtpcHitCollection* ftpHits = event->ftpcHitCollection(); 
      if (ftpHits->numberOfHits()>0) {
          for (n=0;n<ftpHits-> numberOfPlanes();++n ) {
             for (m=0; m<ftpHits->plane(n)->numberOfSectors(); m++) { 
               for (h=0; h<ftpHits->plane(n)->sector(m)->hits().size(); h++) {
                  hit = ftpHits->plane(n)->sector(m)->hits()[h];
                  hitPoints.push_back( hit->position().x());
                  hitPoints.push_back( hit->position().y());
                  hitPoints.push_back( hit->position().z());
           } } }
          std::vector<float>::iterator xyz = hitPoints.begin();
          Points(hitPoints.size()/3,&*xyz,kUnusedHit);
          SetComment("Unused FTPC hits");
          printf(" FTPC hits counter total : %d\n", hitPoints.size()/3);
     }
  }
}

//____________________________________________________________________________________
//! \return The pointer to the current instance of the StuDraw3DEvent  class to visualize StEvent components
//____________________________________________________________________________________
StuDraw3DEvent *StuDraw3DEvent::Display(){ return gEventDisplay;}

StuDraw3DEvent *gEventDisplay = new StuDraw3DEvent();
//____________________________________________________________________________________
void StuDraw3DEvent::Hits(StEventHitIter &iter)
{
  std::vector<float> hitPoints;
  for (const StHit *sth=0;(sth = *(iter));++iter) {
    const float *f = sth->position().xyz();
    hitPoints.push_back(f[0]);
    hitPoints.push_back(f[1]);
    hitPoints.push_back(f[2]);
  }
  std::vector<float>::iterator xyz = hitPoints.begin();
  Points(hitPoints.size()/3,&*xyz,kUsedHit);
}
//____________________________________________________________________________________
//_____________________________________________________________________________
void StuDraw3DEvent::Wait()
{
    if (gEventDisplay) gEventDisplay->UpdateModified();
    fprintf(stderr,"StvDraw::Waiting...\n");
    while(!gSystem->ProcessEvents()){gSystem->Sleep(200);}; 
}

