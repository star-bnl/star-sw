#include <stdexcept>
#include "StiKalmanTrackFitter.h"
#include "StiKalmanTrack.h"
#include "StiKTNIterator.h"

/*! Fit given track with helicoical track model.
  <h3>Notes</h3>
	<ol>
  <li>The fit is performed along a direction (inside-out||outside-in) prescribed by 
      the value of the track flag "FittingDirection".</li>
  <li>In practice, the iteration through track nodes proceeds from first-to-last
      or last-to-first whether "trackingDirection==fitDirection"
  <li>Use track node methods to do the actual propagation. 
  <li>Node with no hits are allowed as the evaluation of the track chi2 
      and updates are performed only if nodes hold a hit.
	</ol>
*/
void StiKalmanTrackFitter::fit(StiTrack * stiTrack) //throw (Exception)
{
	StiKalmanTrack * track = dynamic_cast<StiKalmanTrack * >(stiTrack);
	if (track==0) 
			throw runtime_error("StiKalmanTrack::fit() - ERROR:\t Dynamic cast to StiKalmanTrack failed");
	StiHit * targetHit;
	StiKalmanTrackNode * targetNode; // parent node
	const StiDetector * targetDet;  // parent detector
	StiDirection fitDirection =  track->getFittingDirection(); 
	StiDirection trackingDirection =  track->getTrackingDirection(); 

	StiKTNBidirectionalIterator first;
	StiKTNBidirectionalIterator last;
	bool direction = (trackingDirection==fitDirection);
	if (direction)
		{ 
			//cout << "set =="<<endl;
			first = track->begin();
			last  = track->end();
		}
	else
		{
			//cout << "set !="<<endl;
			last  = track->begin();
			first = track->end();
		}
	StiKTNBidirectionalIterator source;
	for (source=first;source!=last;)
		{
			if (direction)
				{
					source++;//cout<<"=="<<endl;
				}
			else
				{
					source--;//cout<<"!="<<endl;
				}			
			targetNode= static_cast<StiKalmanTrackNode*>((*source).getFirstChild());
			targetDet = targetNode->getDetector();
			targetHit  = targetNode->getHit();
			// evolve state from that of source using dets source to source
			targetNode->propagate(&(*source),targetDet);	
			// if target has hit, get chi2 and update track parameters accordingly
			if (targetHit)
				{
					targetNode->evaluateChi2();
					targetNode->updateNode();
				}
		}
}


