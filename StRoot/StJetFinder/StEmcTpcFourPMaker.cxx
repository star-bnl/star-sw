/***************************************************************************
 *
 * $Id: StEmcTpcFourPMaker.cxx,v 1.10 2003/07/24 22:11:17 thenry Exp $
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
#include "StSpinMaker/StMuTrackFourVec.h"

#include "StEmcClusterCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StEmcPoint.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"
#include "StEmcTpcFourPMaker.h"
#include "SystemOfUnits.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"

ClassImp(StEmcTpcFourPMaker)
  
double PionAveDepRatio;
double KaonAveDepRatio;
double ProtonAveDepRatio;
double ElectronAveDepRatio;
double ChargedAveDep;

  // This is the Energy deposit function.  It calculates the amount of 
  // energy deposited by a charged particle 
double StProjectedTrack::depE(void) 
{ 
  double coef_0 = ChargedAveDep*(1.0-probElectron);
  double coef_1 = probElectron*ElectronAveDepRatio +
			  probPion*PionAveDepRatio + 
			  probProton*ProtonAveDepRatio +
			  probKaon*KaonAveDepRatio;
  cout << "coef_0: " << coef_0 << endl;
  cout << "coef_1: " << coef_1 << endl;
  return coef_0 + coef_1*E();
}

void StEmcTpcFourPMaker::SetDepRatios(double PIDR, double KDR, 
				 double PRDR, double EDR, double CAD)
{
  mPIDR = PIDR;
  mKDR = KDR;
  mPRDR = PRDR;
  mEDR = EDR;
  mCAD = CAD;
  SetDepRatios();
}

void StEmcTpcFourPMaker::SetDepRatios(void)
{
  PionAveDepRatio = mPIDR;
  KaonAveDepRatio = mKDR;
  ProtonAveDepRatio = mPRDR;
  ElectronAveDepRatio = mEDR;
  ChargedAveDep = mCAD;
}

StEmcTpcFourPMaker::StEmcTpcFourPMaker(const char* name, 
  StMuDstMaker* uDstMaker, 
  long pBins, long thBins, double pRad, double thRad, double rsqr,
  StEmcADCtoEMaker* adcToEMaker) 
  : StFourPMaker(name, uDstMaker), radiussqr(rsqr), binmap(pBins, thBins, pRad, thRad), adc2E(adcToEMaker) {
  seconds = 0;
  muEmc = NULL;
  emc = NULL;
  SetDepRatios(0.2,0.2,0.2,1.0,0.0);
  maxHits = 4800;
  fakePoints.resize(maxHits);
  useType = Hits;
}

Int_t StEmcTpcFourPMaker::Make() {
  SetDepRatios();
  cout <<" Start StEmcTpcFourPMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   
  binmap.clearall();

  // Construct tracks out of (primary) tracks and EMC points. 
  // Must calculate the eta shift of the EMC
  StMuDst* uDst = muDst->muDst();
  StMuEvent* uEvent = uDst->event();
  double mField = uEvent->magneticField()/10.0;
  cout << "mField: " << mField << endl;
  StThreeVectorF vertex = uEvent->primaryVertexPosition();
  binmap.setVertex(vertex);
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
    binmap.insertTrack(track, i);
  }

  // Retreive the points
  StEmcCollection *emc = NULL;
  if((useType != Hits) || (adc2E == NULL))
    { 
      if(adc2E == NULL)
	{
	  muEmc = uDst->emcCollection();
	}
      else
	{
	  if(muEmc != NULL)
	    {
	      delete muEmc;
	      muEmc = NULL;
	    }
	  StMuEmcUtil converter;
	  StEmcCollection *emc = adc2E->getEmcCollection();
	  muEmc = converter.getMuEmc(emc);
	}
    }
  else
    {
      emc = adc2E->getEmcCollection();
    }
  int numPoints = 0;
  int numClusters = 0;
  double twoPi = M_PI*2.0;

  // Add the points
  if((useType != Hits) || (adc2E == NULL))
    {
      numPoints = muEmc->getNPoints();
      numClusters = muEmc->getNClusters(1);
      cout << "NumPoints: " << numPoints << endl;
      cout << "NumClusters: " << numClusters << endl;
      // This just prints for debugging purposes:
      if(useType != Clusters) for(int i = 0; i < numPoints; i++)
	{
	  StMuEmcPoint* point = muEmc->getPoint(i);
	  cout << "Point[" << i << "] eta: " << point->getEta() - etaShift;
	  double phi = point->getPhi(); 
	  while(phi < 0) phi += twoPi;
	  while(phi > twoPi) phi -= twoPi;
	  cout << "  phi: " << phi;
	  cout << "  energy: " << point->getEnergy() << endl;
	}
      if(useType == Clusters) for(int i = 0; i < numClusters; i++)
	{
	  StMuEmcCluster* cluster = muEmc->getCluster(1,i);
	  cout << "Cluster[" << i << "] eta: " << cluster->getEta() - etaShift;
	  double phi = cluster->getPhi(); 
	  while(phi < 0) phi += twoPi;
	  while(phi > twoPi) phi -= twoPi;
	  cout << "  phi: " << phi;
	  cout << "  energy: " << cluster->getEnergy() << endl;
	}
      if(useType != Clusters) for(int i = 0; i < numPoints; i++)
	{
	  StMuEmcPoint* point = muEmc->getPoint(i);
	  binmap.insertPoint(point, i);
	}
      if(useType == Clusters) for(int i = 0; i < numClusters; i++)
	{
	  StMuEmcCluster* cluster = muEmc->getCluster(1,i);

	  // now add a fake point to the binmap
	  StMuEmcPoint& point = fakePoints[i];
	  point.setEta(cluster->getEta());
	  point.setPhi(cluster->getPhi());
	  point.setEnergy(cluster->getEnergy());
	  binmap.insertPoint(&point, i);
	}
    }
  else // useType == Hits
    {
      int pointIndex = 0;
      int& hitId = pointIndex;
      StEmcGeom* geom = StEmcGeom::getEmcGeom(detname[0].Data());
      StBemcData* data = adc2E->getBemcData();
      int numHits = data->NTowerHits;
      cout << "Number Hits: " << numHits;

      for(hitId = 1; hitId <= maxHits; hitId++)
	{
	  float eta, phi, energy;
	  if(data->TowerStatus[hitId-1] != 1) continue;
	  energy = data->TowerEnergy[hitId-1];
	  if(energy < 0.01) continue;
	  geom->getEtaPhi(hitId, eta, phi);
	  while(phi < 0) phi += twoPi;
	  while(phi > twoPi) phi -= twoPi;

	  // now add a fake point to the binmap
	  StMuEmcPoint& point = fakePoints[pointIndex];
	  point.setEta(eta);
	  point.setPhi(phi);
	  point.setEnergy(energy);
	  binmap.insertPoint(&point, pointIndex);
	}
    }

  // Connect the points with the tracks when they are within radiussqr 
  binmap.correlate(radiussqr);

  // Veto the guess of Tpc particle identification using the 
  // Point correlations
  for(trackToPoints::iterator trackit = binmap.t2p.begin(); 
      trackit != binmap.t2p.end(); ++trackit)
  {
    // If the energy of the point is close to the energy of the track,
    // then it is most likely an electron:
    StMuTrack* track= (*trackit).first;
    StProjectedTrack &pTrack = binmap.moddTracks[track];
    StMuEmcPoint* point = (*trackit).second;
    double trackE = pTrack.E();
    double pointE = binmap.moddPoints[point].E();
    double ediff = fabs(trackE - pointE);
    if(ediff < 0.2*pointE)
      pTrack.probEIsOne();
    if(trackE < 0.5*pointE)
      pTrack.probEIsZero();
    break;
  }

  // It can't be an electron if there is no point:
  for(trackMap::iterator trackit = binmap.moddTracks.begin();
      trackit != binmap.moddTracks.end(); ++trackit)
  {
    trackToPoints::iterator foundPoint = binmap.t2p.find((*trackit).first);
    if(foundPoint == binmap.t2p.end())
    {
      binmap.moddTracks[(*trackit).first].probEIsZero();
      continue;
    }
  }

  // Add TPC tracks
  long index = 0;
  tracks.clear();
  for(trackMap::iterator track = binmap.moddTracks.begin(); 
      track != binmap.moddTracks.end(); ++track)
  {
    trackMap::value_type &track_val = *track;
    StMuTrackFourVec& newTrack = tPile[index++];
    StProjectedTrack &pTrack = track_val.second;
    newTrack.Init(pTrack.getTrack(), pTrack.P(), pTrack.getIndex());
    tracks.push_back(&newTrack);
  }  


  // Now subtract the energy deposited by the tracks:
  StMuTrack* lasttrack = (*(binmap.t2p.begin())).first;
  double deposit = (binmap.moddTracks[lasttrack]).depE();
  DistanceToPointMap pointsDist;
  for(trackToPoints::iterator trackit = binmap.t2p.begin(); 
      trackit != binmap.t2p.end(); ++trackit)
    {
      if((*trackit).first != lasttrack)
	{
	  for(DistanceToPointMap::iterator d2p = pointsDist.begin();
	      d2p != pointsDist.end(); ++d2p)
	    {
	      StCorrectedEmcPoint& point = binmap.moddPoints[(*d2p).second];
	      if(point.E() > deposit)
		{
		  point.SubE(deposit);
		  deposit = 0;
		  break;
		}
	      else
		{
		  deposit -= point.E();
		  point.SetE(0);
		}
	    }
	  pointsDist.clear();
	  lasttrack = (*trackit).first;
	  deposit = (binmap.moddTracks[lasttrack]).depE();
	}

      StProjectedTrack& track = binmap.moddTracks[(*trackit).first];
      StCorrectedEmcPoint& point = binmap.moddPoints[(*trackit).second];
      pointsDist.insert(DistanceToPointMap::value_type
			(binmap.trackPointRadiusSqr(track, point), 
			 (*trackit).second));
    }

  // Add neutral pion and eta tracks using the remaining energy in the 
  // reducedPointEnergies array - not coded

  //start = clock();
  // Add photon tracks using the remainder of the energy in the
  // reducedPointEnergies array 
  for(pointMap::iterator point = binmap.moddPoints.begin(); 
      point != binmap.moddPoints.end(); ++point)
  {
    pointMap::value_type &point_val = *point;
    StMuTrackFourVec& newTrack = tPile[index++];
    StCorrectedEmcPoint &cPoint = point_val.second;
    //cout << "FourP E: " << cPoint.P().e() << endl;
    //cout << "FourP Phi: " << cPoint.P().phi() << endl;
    //cout << "FourP Eta: " << cPoint.P().pseudoRapidity() << endl;
    //cout << "Point/Track E: " << cPoint.E() << endl;
    //cout << "Point/Track Phi: " << cPoint.Phi() << endl;
    //cout << "Point/Track Eta: " << cPoint.Eta() << endl;
    if(cPoint.P().e() > .01)
      {
	newTrack.Init(NULL, cPoint.P(), cPoint.getIndex()+nTracks);
	tracks.push_back(&newTrack);  
      }
  }  
  //stop = clock();
  //timeLengths[timeindex] += static_cast<double>(stop-start)
  ///static_cast<double>(CLOCKS_PER_SEC);
  //cout << "Time to add points for jet finding: " << timeLengths[timeindex++] << endl;

  return kStOk;
}





