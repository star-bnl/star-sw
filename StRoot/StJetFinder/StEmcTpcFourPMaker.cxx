/***************************************************************************
 *
 * $Id: StEmcTpcFourPMaker.cxx,v 1.1 2003/04/04 21:36:42 thenry Exp $
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
  
StEmcTpcFourPMaker::StEmcTpcFourPMaker(const char* name, StMuDstMaker* uDstMaker) 
  : StFourPMaker(name, uDstMaker){
    tracks = new StMuTrackFourVec[MAXTRACKS];
    nTracks = 0;
    trackToPointIndices = new int[MAXTRACKS];
    trackEmcPhi = new double[MAXTRACKS];
    trackEmcPhiIsValid = new bool[MAXTRACKS];
    reducedPointEnergies = new double[MAXTRACKS];
    for(int i = 0; i < MAXTRACKS; i++)
    {
      trackToPointIndices[i] = -1;
      trackEmcPhiIsValid[i] = false;
    } 
    pointPhiIndices = new (int*)[PHIMODULES];
    pointPhiIndicesNum = new int[PHIMODULES];
    for(int i = 0; i < PHIMODULES; i++)
    {
      pointPhiIndices[i] = new int[MAXPOINTS];
      pointPhiIndicesNum[i] = 0;
    }
    pi = atan(1.0)*4.0;
    twoPi = pi*2.0;
    modAngle = pi/180.0*6.00001;
    pointRadius = 2.0*modAngle;
    probPion = new double[MAXPOINTS];
    probKaon = new double[MAXPOINTS];
    probProton = new double[MAXPOINTS];
    probElectron = new double[MAXPOINTS];

    PionAveDepRatio = 0.2;
    KaonAveDepRatio = 0.2;
    ProtonAveDepRatio = 0.2;
    ElectronAveDepRatio = 1.0;
}

Int_t StEmcTpcFourPMaker::Make() {
  cout <<" Start StEmcTpcFourPMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   
  for(int i = 0; i < nTracks; i++)
  {
    trackToPointIndices[i] = -1;
    trackEmcPhiIsValid[i] = false;
  }
  for(int i = 0; i < PHIMODULES; i++)
  {
    pointPhiIndicesNum[i] = 0;
  }

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
  nTracks = uDst->numberOfPrimaryTracks();

  // Calculate trackEmcPhi (Phi of the track at the radius of the EMC SMD)
  // pt=BeR, pt=0.3BR, pt GeV/c, B Tesla, R meters, R = pt/(Be) = pt/(0.3B)
  for(int i = 0; i < nTracks; i++)
  {
    StMuTrack* track = uDst->primaryTracks(i);
    double pt = track->pt();
    double R = pt/(0.3*mField);
    if(R < HSMDR) 
    {
      trackEmcPhiIsValid[i] = false; 
      continue;
    }
    // If a circle of Radius R is tangent to the origin, where
    // a circle of Radius SMDR is centered, the circles cross
    // where x^2 + y^2 = SMDR^2 and x^2 + (y-R)^2 = R^2, or:
    // y^2 - (y-R)^2 = SMDR^2 - R^2,
    // 2.0*y*R = SMDR^2,
    // y = SMDR^2/(2.0*R)
    // The angle of deviation is then: sin(dev) = y/SMDR, so:
    double y = HSMDR/R;
    double dev = asin(y);
    if(track->charge() > 0.0) dev *= -1.0;
    trackEmcPhi[i] = track->phi() + dev; 
    while(trackEmcPhi[i] < 0) trackEmcPhi[i] += twoPi;
    while(trackEmcPhi[i] > twoPi) trackEmcPhi[i] -= twoPi;
    if((track->eta() > 0.0) && (track->eta() < 1.0))
      if(track->p().mag() < 1.0)
    {
      cout << "trackEmcPhi[" << i << "]: " << trackEmcPhi[i];
      cout << "  dev: " << dev;
      cout << "  eta: " << track->eta();
      cout << "  e: " << track->p().mag();
      cout << "  eProb: " << track->pidProbElectron() << endl;
    }
    trackEmcPhiIsValid[i] = true;
  }

  // Retreive the points
  StMuEmcCollection* emc = uDst->emcCollection();
  int numPoints = emc->getNPoints();

  // Sort EMC Points into Phi bins
  cout << "NumPoints: " << numPoints << endl;
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
    double phi = point->getPhi();
    if(phi < 0) phi += pi;
    ////double radius = point->getRadius();
    //double radius = 2.0*modAngle;
    reducedPointEnergies[i] = point->getEnergy();
    int range = (int) ceil(pointRadius/modAngle);
    int phiBin = (int) floor(phi/modAngle);
    for(int j = phiBin - range; j <= phiBin + range; j++)
    {
      int k = j;
      while(k < 0) k+= PHIMODULES;
      while(k >= PHIMODULES) k -= PHIMODULES;
      pointPhiIndices[k][pointPhiIndicesNum[k]] = i;
      pointPhiIndicesNum[k]++;
    }
  }

  // Fill the trackToPointIndices array with indices of points which the 
  // track strikes within the point's radius
  for(int i = 0; i < nTracks; i++)
  {
    if(!trackEmcPhiIsValid[i]) continue;   
    StMuTrack* track = uDst->primaryTracks(i);
    double eta = track->eta();
    int phiBin = (int) floor(trackEmcPhi[i]/modAngle);
    for(int j = 0; j < pointPhiIndicesNum[phiBin]; j++)
    {
      cout << "PointPhiIndices[" << phiBin << "][" << j << "]: " << pointPhiIndices[phiBin][j] << endl;
      StMuEmcPoint* point = emc->getPoint(pointPhiIndices[phiBin][j]);
      double deta = eta - point->getEta()+etaShift;
      cout << "Deta: " << deta << endl;
      double pointPhi = point->getPhi(); 
      while(pointPhi < 0) pointPhi += twoPi;
      while(pointPhi > twoPi) pointPhi -= twoPi;
      double dphi = trackEmcPhi[i] - point->getPhi();
      while(dphi > pi) dphi -= twoPi; while(dphi < -pi) dphi += twoPi;
      cout << "Dphi: " << dphi << endl;
      double radius2 = deta*deta + dphi*dphi;
      cout << "PointRadius == " << pointRadius << endl;
      cout << "Radius == " << sqrt(radius2) << endl;
      if(radius2 < pointRadius*pointRadius)
      {
        trackToPointIndices[i] = pointPhiIndices[phiBin][j];
        break;
      }
    }
  }

  // Guess the identities of the tracked particles using the TPC information
  for(int i = 0; i < nTracks; i++)
  {
    StMuTrack* track = uDst->primaryTracks(i);
    probPion[i] = track->pidProbPion();
    probKaon[i] = track->pidProbKaon();
    probProton[i] = track->pidProbProton();
    probElectron[i] = track->pidProbElectron();
  }

  // Veto the above guess using the trackToPointIndices for vetoes.  
  for(int i = 0; i < nTracks; i++)
  {
    // If there is no point associated with the track, 
    if(trackToPointIndices[i] == -1)  // Then it can't be an electron!
    {
      probEClobber(i); continue;
    }
    // If the energy of the point is close to the energy of the track,
    // then it is most likely an electron:
    StMuTrack* track = uDst->primaryTracks(i);
    double simpleEnergy = track->momentum().mag();
    if(fabs(simpleEnergy - reducedPointEnergies[trackToPointIndices[i]]) 
       < .3) 
    {
      probElectron[i] = 1.0;
      probPion[i] = 0.0;
      probKaon[i] = 0.0;
      probProton[i] = 0.0;
    }
    // Also, if it doesn't leave enough energy in the EMC, it can't be an electron
    if(simpleEnergy - reducedPointEnergies[trackToPointIndices[i]] > .3)
      probEClobber(i); continue;    
  }

  // Add TPC tracks
  for(int i = 0; i < nTracks; i++)
  {
    StMuTrack* track = uDst->primaryTracks(i);
    StThreeVectorF mom = track->momentum();
    float mass = probElectron[i]*me + probProton[i]*mp +
      probPion[i]*mpi + probKaon[i]*mk;
    StLorentzVectorF P(sqrt(mass*mass + mom.mag2()), mom);
    if(trackToPointIndices[i] != -1)
    {
      int pointIndex = trackToPointIndices[i];
      // Subtract wieghted average energies from the EMC:
      reducedPointEnergies[pointIndex] -= (probPion[i]*PionAveDepRatio 
        + probKaon[i]*KaonAveDepRatio + probProton[i]*ProtonAveDepRatio 
        + probElectron[i]*ElectronAveDepRatio)*P.e();
    }
    tracks[i].Init(track, P, i);
  } 

  // Add neutral pion and eta tracks using the remaining energy in the 
  // reducedPointEnergies array

  // Add photon tracks using the remainder of the energy in the
  // reducedPointEnergies array 
  for(int i = 0; i < numPoints; i++)
  {
    if(reducedPointEnergies[i] > 0.0)
    {
      cout << "reducedPointEnergies[" << i << "]: " << reducedPointEnergies[i] << endl;
      StMuEmcPoint* point = emc->getPoint(i);
      double eta = point->getEta() - etaShift;
      double phi = point->getPhi();
      double pmag = reducedPointEnergies[i]; // Since mass = 0;
      //pmag*pmag = pz*pz + pt*pt; pz = pt*sinh(eta);
      //pmag*pmag = pt*pt*(1+sinh(eta)^2);
      //pt = pmag/sqrt(1+sinh(eta)^2);
      double pt = pmag/sqrt(1+sinh(eta)*sinh(eta));
      StLorentzVectorF P(pmag, pt*cos(phi), pt*sin(phi), pt*sinh(eta));
      tracks[nTracks].Init(NULL, P, i);
      cout << "tracks[" << nTracks << "].p() :" << tracks[nTracks].p() << endl;
      //tracks[i].Init(uDst->primaryTracks(i), P, i);
      cout << "P.mag: " << pmag*pmag - pt*pt*(1+sinh(eta)*sinh(eta)) << endl;
      nTracks++;
    }
  }

  return kStOk;
}

void StEmcTpcFourPMaker::probEClobber(int i)
{
  double renormalize = 1.0/(1.0 - probElectron[i]);
  probPion[i] *= renormalize;
  probKaon[i] *= renormalize;
  probProton[i] *= renormalize;
  probElectron[i] = 0;
}
