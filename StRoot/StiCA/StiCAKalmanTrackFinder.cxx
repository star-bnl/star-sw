#include "StEvent/StEnumerations.h"
#include "StiCA/StiCAKalmanTrackFinder.h"
#include "StiCA/StiCAKalmanTrack.h"
#include "St_base/StMessMgr.h"
#include "StiCA/StiCATpcSeedFinder.h"
#include "Sti/StiKalmanTrackFitter.h" // just for err check
#include "Sti/StiTrackFinderFilter.h" // just for err check
#include "StiCA/StiCATpcTrackerInterface.h"


enum {kMaxTrackPerm = 10000,kMaxEventPerm=10000000};

/*! Find all tracks associated with the current event.
<p>
Algorithm: In a <b>while loop</b>, obtain track seeds from
current track seed finder and proceed to extend it through the
detector.
<p>Found tracks are added to the track container if no track
filter is set or if they satisfy the track filter requirements.
*/
//______________________________________________________________________________
void StiCAKalmanTrackFinder::findTracks()
{
  mEventPerm = kMaxEventPerm;

  assert(_trackContainer );
  assert(_trackSeedFinder);
//   _trackSeedFinder->reset();
//   _trackContainer->clear();
//   if (_trackFilter) _trackFilter->reset();
  StiCATpcTrackerInterface& caTrackerInt = StiCATpcTrackerInterface::Instance();
  caTrackerInt.SetNewEvent();
  StiCATpcSeedFinder::findTpcTracks(caTrackerInt); // find track starting with TPC (CA seed finder)
  findAllTracks(); // find track left
  caTrackerInt.SetStiTracks(_trackContainer);
#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE
  caTrackerInt.RunPerformance();
#endif /* DO_TPCCATRACKER_EFF_PERFORMANCE */
}
//________________________________________________________________________________
Int_t StiCAKalmanTrackFinder::Fit(StiKalmanTrack *track, Double_t rMin) {
  int errType = kNoErrors; // no err by default

  Int_t nTSeed=0,nTAdd=0,nTFail=0,nTFilt=0,status = kNoErrors;
  Int_t nTpcHits=0,nSvtHits=0,nSsdHits=0,nIstHits=0,nPxlHits=0;

  do { //technical do
    track->setFlag(-1);
    status = track->fit(kOutsideIn);
    if (status) 	{nTSeed++; errType = abs(status)*100 + kFitFail; break;}
    status = extendTrack(track,rMin); // 0 - can't extend. 1 - can extend and refit -1 - can extend and can't refit. 
    if ((status != kExtended) && (status != kNotExtended)) {nTFail++; errType = abs(status)*100 + kExtendFail; break;}
    if (_trackFilter){
      status = _trackFilter->filter(track);
      if (status) {nTFilt++; errType = abs(status)*100 + kCheckFail; break;}
    }
    if (errType!=kNoErrors) {track->reduce(); return errType;}

    //cout << "  ++++++++++++++++++++++++++++++ Adding Track"<<endl;
    //		Add DCA node
    StiHit dcaHit; dcaHit.makeDca();
    StiTrackNode *extenDca = track->extendToVertex(&dcaHit);
    if (extenDca) {
      track->add(extenDca,kOutsideIn);
    }
    //		End DCA node
    track->reduce();
    nTAdd++;
    track->setFlag(1);
    _trackContainer->push_back(track);
    track->setId(_trackContainer->size());
    track->reserveHits();
    nTpcHits+=track->getFitPointCount(kTpcId);
    nSvtHits+=track->getFitPointCount(kSvtId);
    nSsdHits+=track->getFitPointCount(kSsdId);
    nIstHits+=track->getFitPointCount(kIstId);
    nPxlHits+=track->getFitPointCount(kPxlId);
    //cout << "  ++++++++++++++++++++++++++++++ Added Track"<<endl;
    LOG_DEBUG << Form("StiCAKalmanTrackFinder::Fit:nbSeed=%d nTFail=%d nTFilt=%d nTAdd=%d", 
		      nTSeed,nTFail,nTFilt,nTAdd) << endm;
    LOG_DEBUG << Form("StiCAKalmanTrackFinder::Fit:nTpcHits=%d nSvtHits=%d  nSsdHits=%d nPxlHits=%d nIstHits=%d",
		      nTpcHits,nSvtHits,nSsdHits,nPxlHits,nIstHits)
	      << endm;
  } while(0);
  return errType;
}

void StiCAKalmanTrackFinder::PrintFitStatus(const int status, const StiCAKalmanTrack* track) 
{
     // let's analyse the error
   int status1 = status%100; // take only status of Fitter
   int status1r = status/100;
   switch (status1) {
     case StiCAKalmanTrackFinder::kNoErrors: {
       if (track) cout << " fitted with " << track->getFitPointCount() << " hits."<< endl;
       else cout << " fitted." << endl;
     }
       break;
     case StiCAKalmanTrackFinder::kApproxFail: {
       cout << " fit failed:" << endl;
       cout << "      Initial approximation of track failed." << endl; 
     }
       break;
     case StiCAKalmanTrackFinder::kFitFail: {
       cout << " fit failed:" << endl;
       cout << "      Track fit failed.";  
       int status2 = status1r%100; // take only status of Fitter
       switch (status2) {
         case StiKalmanTrackFitter::kNoErrors: {
           cout << " Check the code.";
         }
           break;
         case StiKalmanTrackFitter::kShortTrackBeforeFit: {
           cout << " Not enough hits in the track: ";
         }
           break;
         case StiKalmanTrackFitter::kShortTrackAfterFit: {
           if (track) cout << " Not enough hits can be fitted: " << track->getNNodes(kGoodHit) << " .";
           else cout << " Not enough hits can be fitted.";
         }
           break;
         case StiKalmanTrackFitter::kManyErrors: {
           cout << " Too many problems with this track.";
         }
           break;
       }
       cout << endl;
     }
       break;
     case StiCAKalmanTrackFinder::kExtendFail: {
       cout << " fit failed: " << endl;
       cout << "      Track extend failed.";
       int status2 = status1r%100; // take only status of Fitter
       int status2r = status1r/100;
       switch (status2) {
         case StiCAKalmanTrackFinder::kExtended: {
           cout << " Check the code.";
         }
           break;
         case StiCAKalmanTrackFinder::kNotExtended: {
           cout << " Check the code.";
         }
           break;
         case StiCAKalmanTrackFinder::kNotRefitedIn:
         case StiCAKalmanTrackFinder::kNotRefitedOut: {
           cout << " Track can't be refitted after extension ";
           if (status2 == StiCAKalmanTrackFinder::kNotRefitedIn) cout << "inside.";
           else cout << "outside.";

           int status3 = status2r%100; // take only status of Fitter
           switch (status3) { // TODO: make information more clear
             case StiCAKalmanTrack::kNoErrors: {
               cout << " Check the code.";
             }
               break;
             case StiCAKalmanTrack::kRefitFail: {
               cout << " Refit procedure fail.";
             }
               break;
             case StiCAKalmanTrack::kNotEnoughUsed: {
               cout << " sTNH.getUsed() <= 3 .";
             }
               break;
             case StiCAKalmanTrack::kInNodeNotValid: {
               cout << " Inner most node is not valid.";
             }
               break;
             case StiCAKalmanTrack::kBadQA: {
               cout << " qA is inappropriate.";
             }
               break;
             case StiCAKalmanTrack::kVertexNodeInvalid: {
               cout << " Prim node invalid.";
             }
               break;
             case StiCAKalmanTrack::kNodeNotValid: {
               cout << " Prim node Chi2 too big.";
             }
               break;
             case StiCAKalmanTrack::kTooManyDroppedNodes: {
               cout << " Too many dropped nodes.";
             }
               break;
           }
           
         }
           break;
       }
       cout << endl;       
     }
       break;
     case StiCAKalmanTrackFinder::kCheckFail: {
       cout << " fit failed " << endl;
       cout << "      Track check failed.";
       int status2 = status1r%100; // take only status of Fitter
       switch (status2) {
         case StiTrackFinderFilter::kNoErrors: {
           cout << " Check the code.";
         }
           break;
         case StiTrackFinderFilter::kNoEnoughValidHits: {
           if (track) cout << " Not enough valid hits in the track: " << track->getPointCount() << " .";
           else cout << " Not enough valid hits in the track.";
         }
           break;
         case StiTrackFinderFilter::kNoEnoughFittedValidHits: {
           if (track) cout << " Not enough fitted hits in the track: " << track->getFitPointCount() << " .";
           else cout << " Not enough fitted hits in the track.";
         }
           break;
         case StiTrackFinderFilter::kWeird: {
           cout << " Weird track, see StiTrackFinderFilter::accept().";
         }
           break;
       }
       cout << endl;
     }
       break;
   } // switch
} // void StiCAKalmanTrackFinder::PrintFitStatus(const int status, const StiCAKalmanTrack* track)
