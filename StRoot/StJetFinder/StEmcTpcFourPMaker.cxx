 /***************************************************************************
 *
 * $Id: StEmcTpcFourPMaker.cxx,v 1.22 2004/04/27 22:27:08 thenry Exp $
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
using namespace std;
#include <string>
#include <iostream>
#include <math.h>
#include <sys/times.h>

#include "TFile.h"
#include "StChain.h"
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

double SMDR;
double mSMDR;
double mHSMDR;
double mtwoPi;
double mme;	
double mmpr;
double mmpi;
double mmk;

TH1F* adcValHist;
long eventNum;
long latestrunNum;

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
  EMCSanityThreshold = 200.0;
  maxPoints = 10000;
  minPointThreshold = .01;
  towerProxy = new SafetyArray("towerProxy");

  SMDR = 2.2625;
  mSMDR = 231.23;
  mHSMDR = 115.615;
  mtwoPi = M_PI*2.0;
  mme = .000511;	
  mmpr = .9383;
  mmpi = .1396;
  mmk = .4937;
  noAbortions = false;
  simpleCal = false;

  //adcValHist = new TH1F("PerEventYeild", "ADC Value", 1024, 0.0, 1024.0);
}

Int_t StEmcTpcFourPMaker::Make() {
  SetDepRatios();
  cout <<" Start StEmcTpcFourPMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   
  binmap.clearall();

  aborted = true;
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
  numCoincidences = 0;
  sumPtTracks = 0.0;
  sumEMC = 0.0;
  sumSubtracted = 0.0;
  sumTheorySubtracted = 0.0;

  // Calculate trackEmcPhi (Phi of the track at the radius of the EMC SMD)
  // pt=BeR, pt=0.3BR, pt GeV/c, B Tesla, R meters, R = pt/(Be) = pt/(0.3B)
  for(int i = 0; i < nTracks; i++)
  {
    StMuTrack* track = uDst->primaryTracks(i);
    if(track->flag() < 0) continue;
    if(track->eta() < GetEtaLow()) continue;
    if(track->eta() > GetEtaHigh()) continue;
    double pt = track->pt();
    double R = pt/(0.3*mField);
    if(R < HSMDR) // just forget the track if it doesn't get to EMC radius. 
      continue;
    if(static_cast<double>(track->nHits())
       /static_cast<double>(track->nHitsPoss()) < .51)
      continue;
    sumPtTracks += pt;
    binmap.insertTrack(track, i);
  }

  // Retreive the points
  StEmcCollection *emc = NULL;
  if(useType != Hits)
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
      if(adc2E == NULL)
      {
	muEmc = uDst->emcCollection();
        if(muEmc != NULL)
          cout << "muEmc != NULL" << endl;
        else
          cout << "muEmc == NULL" << endl;
      }
      else
	emc = adc2E->getEmcCollection();
    }
  int numPoints = 0;
  int numClusters = 0;
  double twoPi = M_PI*2.0;

  // Add the points
  if(useType != Hits)
    {
      numPoints = muEmc->getNPoints();
      numClusters = muEmc->getNClusters(1);
      cout << "NumPoints: " << numPoints << endl;
      cout << "NumClusters: " << numClusters << endl;
      // This just prints for debugging purposes:
      if(useType == Points) for(int i = 0; i < numPoints; i++)
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
      if(useType == Points) for(int i = 0; i < numPoints; i++)
	{
	  StMuEmcPoint* point = muEmc->getPoint(i);
	  binmap.insertPoint(point, i);
	  sumEMC += point->getEnergy();
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
	  sumEMC += point.getEnergy();
	}
    }
  else // useType == Hits
    {
      unsigned int runNumber = uEvent->runNumber();
      int hitId = 0;
      StEmcGeom* geom = StEmcGeom::getEmcGeom(detname[0].Data());
      StBemcData* data = NULL;
      int numHits = 0;
      TDataSet *mDb = NULL;
      emcCalib_st* emccalibtbl = NULL;
      emcGain_st* emcgaintbl = NULL;
      emcPed_st* emcpedtbl = NULL;
      
      if(adc2E)
	{
	  data = adc2E->getBemcData();
	  numHits = data->NTowerHits;
	  cout << "Number Hits: " << numHits;
	}
      else
	{
          if(!simpleCal)
          {
	    mDb = NULL;
	    TString DbName = "Calibrations/emc/y3"+detname[0];
	    mDb = GetInputDB(DbName.Data());
	    if(!mDb) return kFALSE;
	    TString TableName;

	    TableName = detname[0] + "Calib";
	    St_emcCalib* cptr = (St_emcCalib*) mDb->Find(TableName.Data());
	    if(cptr) emccalibtbl = cptr->GetTable();
	    if(!emccalibtbl) return kFALSE;

	    TableName = detname[0] + "Gain";
	    St_emcGain* gptr = (St_emcGain*) mDb->Find(TableName.Data());
	    if(gptr) emcgaintbl = gptr->GetTable();

	    TableName = detname[0] + "Ped";
	    St_emcPed* pptr = (St_emcPed*) mDb->Find(TableName.Data());
	    if(pptr) emcpedtbl = pptr->GetTable();
	    if(!emcpedtbl) return kFALSE;
          }
	}

      for(hitId = 1; hitId <= maxHits; hitId++)
	{
          cout << "Hit #: " << hitId << endl;
          if(muEmc != NULL)
            cout << "muEmc != NULL" << endl;
	  float eta, phi, energy;
          bool isGood;
          getValuesFromHitId(hitId, runNumber, eta, phi, energy, isGood, 
		towerProxy, geom, data, muEmc, simpleCal, mDb, emcgaintbl, 
                emccalibtbl, emcpedtbl);
          if(isGood == false)
            continue;
	  while(phi < 0) phi += twoPi;
	  while(phi > twoPi) phi -= twoPi;

	  sumEMC += energy;
	  if(energy < 0.01) continue;
	  
	  // now add a fake point to the binmap
	  StMuEmcPoint& point = fakePoints[hitId];
	  point.setEta(eta);
	  point.setPhi(phi);
	  point.setEnergy(energy);
	  binmap.insertPoint(&point, hitId);
	  //sumEMC += point.getEnergy();
	}
    }

  // Now Bail if the energy is absurd
  if(!noAbortions)
    if(sumEMC >= EMCSanityThreshold) return kStOK; 

  // Connect the points with the tracks when they are within radiussqr 
  binmap.correlate(radiussqr);
  numCoincidences = binmap.t2p.size(); // == binmap.pt2.size();
  cout << "Number Coincidences " << numCoincidences << endl;

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
    if(ediff < 0.5*pointE)
      pTrack.probEIsOne();
    if(trackE < 0.3*pointE)
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

  numberPoints = 0;
  double maxEtValue = 0;
  for(pointMap::iterator point = binmap.moddPoints.begin(); 
      point != binmap.moddPoints.end(); ++point)
    {
      pointMap::value_type &point_val = *point;
      StCorrectedEmcPoint &cPoint = point_val.second;
      double eta = cPoint.P().pseudoRapidity();
      double et = cPoint.P().e()*sqrt(1.0-tanh(eta)*tanh(eta));
      if(et > minPointThreshold)
	numberPoints++;
      if(et > maxEtValue)
	maxEtValue = et;
    }
  if(!noAbortions)
    if(numberPoints > maxPoints)  // If there are too many points
      return kStOK; // don't try to analyze this event
  // If it is a hightower trigger event, but somehow the high tower
  // is missing from the points, skip the event to avoid weird emc biases.
  if(!noAbortions)
    {
      const StTriggerId &trigger = uEvent->triggerIdCollection().nominal();
      if(trigger.isTrigger(1101) || trigger.isTrigger(2201))
	if(maxEtValue < 2.5)
	  return kStOK;
      if(trigger.isTrigger(1102) || trigger.isTrigger(2202))
	if(maxEtValue < 3.5)
	  return kStOK;
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
  StMuTrack* lasttrack = NULL;
  if(binmap.t2p.size() > 0)
    lasttrack = (*(binmap.t2p.begin())).first;
  double deposit = 0;
  if(lasttrack != NULL)
    deposit = (binmap.moddTracks[lasttrack]).depE();
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
		  sumSubtracted += deposit;
		  deposit = 0;
		  break;
		}
	      else
		{
		  deposit -= point.E();
		  sumSubtracted += point.E();
		  point.SetE(0);
		}
	    }
	  pointsDist.clear();
	  lasttrack = (*trackit).first;
	  deposit = (binmap.moddTracks[lasttrack]).depE();
	  sumTheorySubtracted += deposit;
	}

      StProjectedTrack& track = binmap.moddTracks[(*trackit).first];
      StCorrectedEmcPoint& point = binmap.moddPoints[(*trackit).second];
      pointsDist.insert(DistanceToPointMap::value_type
			(binmap.trackPointRadiusSqr(track, point), 
			 (*trackit).second));
    }
  cout << "sumTheorySubtracted: " << sumTheorySubtracted << endl;
  cout << "sumSubtracted: " << sumSubtracted << endl;

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
      if(cPoint.P().e() > minPointThreshold)
	{
	  //cout << "FourP E: " << cPoint.P().e() << endl;
	  //cout << "FourP Phi: " << cPoint.P().phi() << endl;
	  //cout << "FourP Eta: " << cPoint.P().pseudoRapidity() << endl;
	  //cout << "Point/Track E: " << cPoint.E() << endl;
	  //cout << "Point/Track Phi: " << cPoint.Phi() << endl;
	  //cout << "Point/Track Eta: " << cPoint.Eta() << endl;
	  newTrack.Init(NULL, cPoint.P(), cPoint.getIndex()+nTracks);
	  tracks.push_back(&newTrack);  
	}
    }  
  //stop = clock();
  //timeLengths[timeindex] += static_cast<double>(stop-start)
  ///static_cast<double>(CLOCKS_PER_SEC);
  //cout << "Time to add points for jet finding: " << timeLengths[timeindex++] << endl;

  if(adc2E)
    {
      unsigned int runNumber = uEvent->runNumber();
      StBemcData* histadcdata = adc2E->getBemcData();
      for(int hitId = 1; hitId <= maxHits; hitId++)
	{
	  if(towerProxy->isGood(runNumber, hitId-1) == false) { 
	    //cout << "throwing away tower: " << hitId-1 << endl; 
	    continue; }
	  //adcValHist->Fill(static_cast<double>(histadcdata->TowerADC[hitId-1]));
	}
    }
  eventNum = uEvent->eventId();
  latestrunNum = uEvent->runNumber();
  aborted = false;
  return kStOk;
}

Int_t StEmcTpcFourPMaker::Finish()
{
  TString name("/star/rcf/pwg/spin/henry/spindst/adchists/");
  stringstream evnumstrstrm;
  evnumstrstrm << latestrunNum << "-" << eventNum << ".root";
  TString numstr(evnumstrstrm.str().c_str());
  name += numstr;
  //TFile *outfile = new TFile(name, "RECREATE");

  //static_cast<TH1*>(adcValHist)->SetDirectory(static_cast<TDirectory*>(outfile));
  //outfile->Write();
  //outfile->Close();
  //delete outfile;
  return kStOk;
}

bool getValuesFromHitId(int hitId, int runNumber, float &eta, float &phi, 
  float &energy, bool &isGood, SafetyArray *towerProxy, StEmcGeom *geom, 
  StBemcData *data, StMuEmcCollection *muEmc, bool simpleCal, 
  TDataSet *mDb, emcGain_st* emcgaintbl,
  emcCalib_st* emccalibtbl, emcPed_st* emcpedtbl)
{
	  if(towerProxy->isGood(runNumber, hitId-1) == false) { 
            isGood = false;
	    return false; }
	  geom->getEtaPhi(hitId, eta, phi);
	  if(data)
	    {
	      if(data->TowerStatus[hitId-1] != 1) { isGood = false; return false; }
	      if(simpleCal)
                {
                  double et = 0.0125*static_cast<double>(data->TowerADC[hitId-1]);
                  energy = et/sqrt(1.0-tanh(eta)*tanh(eta));
                }
              else
                {
	          energy = towerProxy->energyFunction(
						  runNumber, 
						  hitId-1, 
						  data->TowerADC[hitId-1], 
						  data->TowerEnergy[hitId-1]);
                }
	    }
	  else // Calibration!!!!  MuDst does not contain energy.
	    {
              if(emccalibtbl == NULL)
                return false;
	      float ADC = 0; 
	      float PED = 0;
              if(muEmc != NULL)
              {
	        ADC = muEmc->getTowerADC(hitId, bemc);
                //cout << "ADC value: " << ADC << endl;
              }
              else
                return false;
              if(simpleCal)
                {
                  double et = 0.0125*ADC;
                  energy = et/sqrt(1.0-tanh(eta)*tanh(eta));
                }
              else
                {
	      	  PED = static_cast<float>(emcpedtbl[0].AdcPedestal[hitId-1])
		    /100.0;
	          //PED = static_cast<double>(static_cast<int>(PED));
	          float ADCSUB = ADC-PED;
	          energy = 0;
	          float ADCPOWER = 1;
	          for(int i = 0; i < 5; i++)
		    {
		      float c = 0;
		      c = emccalibtbl[0].AdcToE[hitId-1][i];
		      energy += c*ADCPOWER;
		      ADCPOWER *= ADCSUB;
		    }
	          if(PED <= 0) energy = 0;
	          float gain = 1;
	          if(emcgaintbl != NULL)
		    gain = emcgaintbl[0].Gain[hitId-1];
	          if(gain < .1)  // gain shouldn't have a large or negative effect
		    gain = 1;
	          //energy *= gain;
                }
	    }
  return true;
}
