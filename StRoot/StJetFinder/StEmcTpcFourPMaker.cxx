/***************************************************************************
 *
 * $Id: StEmcTpcFourPMaker.cxx,v 1.2 2003/04/24 14:15:15 thenry Exp $
 * 
 * Author: Thomas Henry February 2003
 ***************************************************************************
 *
 * Description:  Maker which creates a list of Four Momentums from the TPC
 * and EMC corresponding to charged particles and photons, but subtracting
 * some of the energy deposited in the EMC by the charged particles.
 *
 ***************************************************************************
 *
 * Revision 1.0  2003/02/20 thenry
 *
 **************************************************************************/
#include <string.h>
#include <iostream.h>
#include <math.h>
#include <sys/times.h>

#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StIOMaker/StIOMaker.h"
#include "StEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "../StSpinMaker/StMuTrackFourVec.h"

#include "StEmcClusterCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StEmcPoint.h"
#include "StEmcTpcFourPMaker.h"

ClassImp(StEmcTpcFourPMaker)
  
struct tms time_struct;

StEmcTpcFourPMaker::StEmcTpcFourPMaker(const char* name, StMuDstMaker* uDstMaker,
  long pBins, long thBins, double pRad, double thRad, double rsqr) 
  : StFourPMaker(name, uDstMaker), radiussqr(rsqr), binmap(pBins, thBins, pRad, thRad){
  seconds = 0;
}

Int_t StEmcTpcFourPMaker::Make() {
  cout <<" Start StEmcTpcFourPMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   
  clock_t start, end;
  times(&time_struct);
  start = time_struct.tms_stime;
  binmap.clearall();

  // Construct tracks out of (primary) tracks and EMC points. 
  // Must calculate the eta shift of the EMC
  StMuDst* uDst = muDst->muDst();
  StMuEvent* uEvent = uDst->event();
  double mField = uEvent->magneticField()/10.0;
  cout << "mField: " << mField << endl;
  StThreeVectorF vertex = uEvent->primaryVertexPosition();
  double SMDR = 2.2625;
  double etaShift = atan2(vertex.z()/100.0, SMDR);
  cout << "zVertex: " << vertex.z()/100.0 << endl;
  cout << "EtaShift: " << etaShift << endl;
  double HSMDR = SMDR/2.0;
  long nTracks = uDst->numberOfPrimaryTracks();

  // Calculate trackEmcPhi (Phi of the track at the radius of the EMC SMD)
  // pt=BeR, pt=0.3BR, pt GeV/c, B Tesla, R meters, R = pt/(Be) = pt/(0.3B)
  for(int i = 0; i < nTracks; i++)
  {
    StMuTrack* track = uDst->primaryTracks(i);
    if(track->flag() < 0) continue;
    double pt = track->pt();
    double R = pt/(0.3*mField);
    if(R < HSMDR) // just forget the track if it doesn't get to EMC radius. 
      continue;
    binmap.insertTrack(track);
  }

  // Retreive the points
  StMuEmcCollection* emc = uDst->emcCollection();
  int numPoints = emc->getNPoints();

  // Add the points
  cout << "NumPoints: " << numPoints << endl;
  // This just prints for debugging purposes:
  double twoPi = M_PI*2.0;
  for(int i = 0; i < numPoints; i++)
  {
    StMuEmcPoint* point = emc->getPoint(i);
    cout << "Point[" << i << "] eta: " << point->getEta() - etaShift;
    double phi = point->getPhi(); 
    while(phi < 0) phi += twoPi;
    while(phi > twoPi) phi -= twoPi;
    cout << "  phi: " << phi;
    cout << "  energy: " << point->getEnergy() << endl;
  }
  for(int i = 0; i < numPoints; i++)
  {
    StMuEmcPoint* point = emc->getPoint(i);
    binmap.insertPoint(point);
  }

  // Connect the points with the tracks when they are within radiussqr 
  binmap.correlate(radiussqr);

  // Veto the guess of Tpc particle identification using the 
  // Point correlations
  for(trackMap::iterator track = binmap.moddTracks.begin(); 
      track != binmap.moddTracks.end(); ++track)
  {
    trackMap::value_type &track_val = *track;
    pointerMap::iterator beg = binmap.findbegin(track_val);
    pointerMap::iterator ed = binmap.findend(track_val);
    bool foundPoint = false;
    for(pointerMap::iterator point = beg;
        point != ed; ++point)
    {
      // In this version of the code, there will never be more than one
      // point in this loop.
      pointerMap::value_type &point_val = *point;
      if(point_val.second.second != NULL) 
      {
        foundPoint = true;
        // If the energy of the point is close to the energy of the track,
        // then it is most likely an electron:
        double trackE = track_val.second.E();
        double pointE = binmap.correctedEmcPoint(point_val).E();
        double ediff = fabs(trackE - pointE);
        if(ediff < 0.2*pointE)
          track_val.second.probEIsOne();
        if(trackE < 0.5*pointE)
          track_val.second.probEIsZero();
        break;
      }
    }
    // It can't be an electron if there is no point:
    if(!foundPoint)
    {
      track_val.second.probEIsZero();
      continue;
    }
  }

  // Add TPC tracks
  long index = 0;
  for(trackMap::iterator track = binmap.moddTracks.begin(); 
      track != binmap.moddTracks.end(); ++track)
  {
    trackMap::value_type &track_val = *track;
    StMuTrackFourVec& newTrack = tPile[index];
    StProjectedTrack &pTrack = track_val.second;
    newTrack.Init(pTrack.getTrack(), pTrack.P(), index++);
    tracks.push_back(&newTrack);  
  }  

  // Now subtract the energy deposited by the tracks:
  for(pointMap::iterator point = binmap.moddPoints.begin(); 
      point != binmap.moddPoints.end(); ++point)
  {
    pointMap::value_type &point_val = *point;
    pointerMap::iterator beg = binmap.findbegin(point_val);
    pointerMap::iterator ed = binmap.findend(point_val);
    for(pointerMap::iterator track = beg;
        track != ed; ++track)
    {
      // Make sure not to dereference the std point which has no track:
      pointerMap::value_type &track_val = *track;
      if(track_val.second.second == NULL) continue;
      point_val.second.SubE(binmap.projectedTrack(track_val).depE());
    }
  }  

  // Add neutral pion and eta tracks using the remaining energy in the 
  // reducedPointEnergies array - not coded

  // Add photon tracks using the remainder of the energy in the
  // reducedPointEnergies array 
  for(pointMap::iterator point = binmap.moddPoints.begin(); 
      point != binmap.moddPoints.end(); ++point)
  {
    pointMap::value_type &point_val = *point;
    StMuTrackFourVec& newTrack = tPile[index];
    StCorrectedEmcPoint &cPoint = point_val.second;
    newTrack.Init(NULL, cPoint.P(), index++);
    tracks.push_back(&newTrack);  
  }  

  times(&time_struct);
  end = time_struct.tms_stime;
  seconds += ((double)(end - start)); //((double)sysconf(_SC_CLK_TCK));
  cout << "Seconds elapsed for binmap.clearall(): " << seconds << endl;

  return kStOk;
}

