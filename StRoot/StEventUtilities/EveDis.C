// $Id: EveDis.C,v 1.2 2008/05/16 17:36:52 fine Exp $
// *-- Author :    Valery Fine(fine@bnl.gov)   27/04/2008
#include "StuDraw3DEvent.h"
#ifndef __CINT__
#  include "TVirtualPad.h"
#  include "TColor.h"
#  include "StEventHelper.h"
#  include "StEvent.h"
#  include "StTrack.h"
#  include "StHit.h"
#  include "StTpcHit.h"
#  include "StTrackNode.h"
#  include "StTrackGeometry.h"
#  include "StTpcHitCollection.h"
#  include "StMeasuredPoint.h"
#  include "StTrackDetectorInfo.h"
#endif

class EveDis : public StuDraw3DEvent {

           
  ////////////////////////////////////////////////////////////////////////
  //
  //  Class EveDis -  is template to allow the end user to create his/her 
  //                 own version of the display to apply the various cuts.
  //
  //  You have to apply ACliC to use the custom class this macro defines as follows:
  //
  //   root.exe
  //      root.exe [0].x Load.C
  //      root.exe [1].L EveDis.C++
  //      root.exe [2] EveDis display;
  //      root.exe [3] display.Draw3DTest()
  //
  //  See: http://www.star.bnl.gov/public/comp/vis/StDraw3D\n");
  //
  ////////////////////////////////////////////////////////////////////////
public:
//___________________________________________________
EveDis(const char *detectorName="TPC",TVirtualPad *pad=0)
: StuDraw3DEvent(detectorName,pad)
{
   // The detectorName is a comma separated list of the OpenInventor files with no extension
   // For all names on the list one should provide the iv file with the "iv extension:
   //                         <detectorName>.iv
}

//___________________________________________________
~EveDis(){ }

//___________________________________________________
void Hits(const StEvent *event,EStuDraw3DEvent trackHitsOnly, StTrackType type)
{
   if (!event) return; // no event
   const StTpcHitCollection* hits = event->tpcHitCollection();
   if (!hits) return; // there is no hits
   unsigned int m, n, h;
   if (trackHitsOnly != kUnusedHitsOnly) {
      const Int_t lightness    = 50;
      const Int_t saturation   = 100;
      Int_t hue  = 0;

      StHit *hit=0;
      Style_t sty    = Style(kUsedHit).Sty();
      Size_t  siz    = Style(kUsedHit).Siz();
      Style_t styPnt = Style(kTrackBegin).Sty();
      Size_t  sizPnt = Style(kTrackBegin).Siz();

      const StSPtrVecTrackNode& theNodes = event->trackNodes();
      for (unsigned int i=0; i<theNodes.size(); i++) {
         StTrack *track = theNodes[i]->track(type);
//--
//-- PAY YOUR ATTENTION:
//--
//-- Add YOUR OWN CUTS to select the tracks of your choice below
//
         if (track && track->flag() > 0
               && track->detectorInfo() 
         //  &&   track->fitTraits().numberOfFitPoints(kTpcId) >= minFitPoints) 
         )
        {
//--
//-- Create the coloring code
//
            double pt = track->geometry()->momentum().perp();
            hue = Int_t(256.*(1.-pt/1.5)); //color code from StuPostscript
            if (pt > 1.5 ) hue = 0;
            Int_t r,g,b;
            TColor::HLS2RGB(hue, lightness, saturation, r, g, b);
            // Normalize
            float factor = 1./TMath::Sqrt(1.*r*r+1.*g*g+1.*b*b);
            Color_t trackColor =  TColor::GetColor(r*factor,g*factor,b*factor);
            if ( trackHitsOnly != kUsedHits) {
//--
//-- PAY YOUR ATTENTION:    you  MUST call the "Track" method to get your tracks renndred
//
               Track(*track,trackColor);                               // render the track
               TrackInOut(*track, true,  trackColor,  styPnt, sizPnt); // draw the track start point
               TrackInOut(*track, false, trackColor,  styPnt, sizPnt); // draw the track end point
            }
            if ( trackHitsOnly != kTracksOnly) {
               // look for the hits:
//--               
//-- Create a container to accumulate the hit positions:
//               
               std::vector<float> hitPoints;
               const StPtrVecHit& trackHits = track->detectorInfo()->hits(kTpcId);
               for (m=0; m<trackHits.size(); m++) {
//--
//-- Select the hit object
//
                  hit = trackHits[m];
//--
//-- Add the selected hit position to the container
//
                  hitPoints.push_back( hit->position().x());
                  hitPoints.push_back( hit->position().y());
                  hitPoints.push_back( hit->position().z());
              }
//--
//-- PAY YOUR ATTENTION:  you  MUST call the "Points" to get your hits poistion renndred
//
              std::vector<float>::iterator xyz = hitPoints.begin();
              Points(hitPoints.size()/3,&*xyz,trackColor,sty,siz);
              if (trackHitsOnly == kUsedHits) SetModel(track);
           }
        }
     }
   } else {
      const StTpcHit *hit;
//--               
//-- Create a container to accumulate the hit positions:
//               
      std::vector<float> hitPoints;
      for (n=0; n<hits->numberOfSectors(); n++) {
         for (m=0; m<hits->sector(n)->numberOfPadrows(); m++) { 
            for (h=0; h<hits->sector(n)->padrow(m)->hits().size(); h++) {
//--               
//-- Select the hit object
//               
                hit = hits->sector(n)->padrow(m)->hits()[h];
//--
//-- Add the selected hit position to the container
//
                hitPoints.push_back( hit->position().x());
                hitPoints.push_back( hit->position().y());
                hitPoints.push_back( hit->position().z());
            }
         }
      }
      std::vector<float>::iterator xyz = hitPoints.begin();
//--               
//-- PAY YOUR ATTENTION:  you  MUST call the "Points" to get your hits poistion renndred
//               
      Points(hitPoints.size()/3,&*xyz,kUnusedHit);
      SetComment("Unused TPC hits");
   }
}
};
