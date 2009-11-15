#include "StPi0DataMakerUtil.h"

#include <TTree.h>
#include <TH1F.h>
#include <TF1.h>
#include <TClonesArray.h>
#include <TRandom.h>
#include <TSQLServer.h>
#include <TSQLRow.h>
#include <TSQLResult.h>
#include <TVector3.h>
#include <TMath.h>

#include <St_base/StMessMgr.h>
#include <StEvent/StEvent.h>
#include <StEvent/StEventTypes.h>
#include <StEvent/StTriggerId.h>
#include <StEvent/StTriggerData.h>
#include <StEmcUtil/geometry/StEmcGeom.h>
#include <StEmcUtil/database/StBemcTables.h>
#include <StEmcUtil/projection/StEmcPosition.h>
#include <StDaqLib/EMC/StEmcDecoder.h>
#include <tables/St_emcStatus_Table.h>
#include <tables/St_smdStatus_Table.h>
#include <St_db_Maker/St_db_Maker.h>
#include <StMcEventTypes.hh>
#include <StMcEvent.hh>
#include <StMcVertex.hh>
#include <StMcTrack.hh>
#include <StMcEventMaker/StMcEventMaker.h>
#include <StEmcADCtoEMaker/StEmcADCtoEMaker.h>
#include <StEmcADCtoEMaker/StBemcData.h>
#include <StEventUtilities/StuFtpcRefMult.hh>
#include <StJetMaker/StJetMaker.h>

#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

#include <StEmcPool/StPi0DataMaker/StPi0TriggerSimulatorMaker.h>

//_____________________________________________________________________________
Bool_t shuffleArray(Int_t *array, Int_t size, Bool_t init, Int_t numShuffle, Bool_t check) {
    Bool_t result = true;
    if (init) for (Int_t i = 0;i < size;i++) array[i] = i;
    for (Int_t i = 0;i < numShuffle;i++) {
        Int_t index1 = gRandom->Integer(size);
	Int_t index2 = gRandom->Integer(size);
	Int_t tmp = array[index1];
	array[index1] = array[index2];
	array[index2] = tmp;
    }
    if (check) {
	result = false;
	for (Int_t i = 0;(i < size) && (!result);i++) result |= (array[i] != i);
    }
    return result;
}
					  
//_____________________________________________________________________________
void getHitData(const StEmcRawHit *hit, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, StBemcTables *tables, TMyHitData &hitData) {
  if (!hit) return;
  hitData.detector = Int_t(hit->detector());
  StEmcGeom *geom = 0;
  if (hitData.detector == Int_t(kBarrelEmcTowerId)) geom = emcGeom;
  if (hitData.detector == Int_t(kBarrelEmcPreShowerId)) geom = psdGeom;
  if (hitData.detector == Int_t(kBarrelSmdEtaStripId)) geom = smdeGeom;
  if (hitData.detector == Int_t(kBarrelSmdPhiStripId)) geom = smdpGeom;
  Int_t tmp = 0;
  if (geom) geom->getId((Int_t)hit->module(), abs((Int_t)hit->eta()), abs((Int_t)hit->sub()), tmp);
  hitData.id = tmp;
  hitData.adc = hit->adc();
  Float_t ped = 0, pedRMS = 0;
  if (tables) tables->getPedestal(hit->detector() - kBarrelEmcTowerId + 1, hitData.id, hit->calibrationType(), ped, pedRMS);
  hitData.pedestal100 = UShort_t(ped * 100.0);
  hitData.pedestalRMS100 = UShort_t(pedRMS * 100.0);
  Float_t /*theta = 0, */etaCoord = 0, phiCoord = 0;
  if (geom) {
	geom->getEtaPhi(hitData.id, etaCoord, phiCoord);
	while (phiCoord >= TMath::Pi()) phiCoord -= TMath::TwoPi();
	while (phiCoord < -TMath::Pi()) phiCoord += TMath::TwoPi();
	//geom->getTheta(hit->module(), abs((Int_t)hit->eta()), theta); 
  }
  hitData.etaphiCoord = 0.0;
  if ((etaCoord > -1.0) && (phiCoord > -3.5)) {
	hitData.etaphiCoord = Int_t(100000.0 * (etaCoord + 1.0)) + ((phiCoord + 3.5) / 7.0);
  }
  hitData.energy = hit->energy();
}

//_____________________________________________________________________________
void getClusterData(const StEmcCluster *cluster, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, StBemcTables *tables, TMyClusterData &clusterData) {
  if (!cluster) return;
  clusterData.detector = cluster->hit()[0]->detector();
  clusterData.size = cluster->nHits();
  clusterData.energy = cluster->energy();
  clusterData.etaCoord = cluster->eta();
  clusterData.phiCoord = cluster->phi();
  clusterData.sigmaEta = cluster->sigmaEta();
  clusterData.sigmaPhi = cluster->sigmaPhi();
  clusterData.badClose = 0;
  clusterData.highestADCHit.adc = 0;
  clusterData.highestEnergyHit.energy = -1000;
#ifdef SAVE_CLUSTERHIT2
  clusterData.highestEnergyHit2.energy = -1000;
#endif
  for (Int_t i = 0;i < (Int_t)cluster->hit().size();i++) {
	const StEmcRawHit *hit = cluster->hit()[i];
	if (!hit) continue;
	Int_t idPrev, idNext;
	if (hit->energy() > clusterData.highestEnergyHit.energy) {
#ifdef SAVE_CLUSTERHIT2
	    clusterData.highestEnergyHit2 = clusterData.highestEnergyHit;
#endif
	    getHitData(hit, emcGeom, smdeGeom, smdpGeom, psdGeom, tables, clusterData.highestEnergyHit);
#ifdef SAVE_CLUSTERHIT2
	} else if (hit->energy() > clusterData.highestEnergyHit2.energy) {
	    getHitData(hit, emcGeom, smdeGeom, smdpGeom, psdGeom, tables, clusterData.highestEnergyHit2);
#endif
	}
	if (hit->adc() > clusterData.highestADCHit.adc) {
	    getHitData(hit, emcGeom, smdeGeom, smdpGeom, psdGeom, tables, clusterData.highestADCHit);
	}
	Int_t status = 0;
	if ((clusterData.detector == kBarrelSmdEtaStripId) && tables) {
		if (hit->eta() >= 2) {
			if (smdeGeom) {
				smdeGeom->getId(hit->module(), abs((Int_t)hit->eta() - 1), abs((Int_t)hit->sub()), idPrev);
				tables->getStatus(hit->detector() - kBarrelEmcTowerId + 1, idPrev, status);
				if (status != 1) clusterData.badClose |= 1;
			}
		} else {
			clusterData.badClose |= 16;
		}
		if (hit->eta() <= 149) {
			if (smdeGeom) {
				smdeGeom->getId(hit->module(), abs((Int_t)hit->eta() + 1), abs((Int_t)hit->sub()), idNext);
				tables->getStatus(hit->detector() - kBarrelEmcTowerId + 1, idNext, status);
				if (status != 1) clusterData.badClose |= 2;
			}
		} else {
			clusterData.badClose |= 32;
		}
	}
	if ((clusterData.detector == kBarrelSmdPhiStripId) && tables) {
		if (hit->sub() >= 2) {
			if (smdpGeom) {
				smdpGeom->getId(hit->module(), abs((Int_t)hit->eta()), abs((Int_t)hit->sub() - 1), idPrev);
				tables->getStatus(hit->detector() - kBarrelEmcTowerId + 1, idPrev, status);
				if (status != 1) clusterData.badClose |= 4;
			}
		} else {
			clusterData.badClose |= 64;
		}
		if (hit->sub() <= 14) {
			if (smdpGeom) {
				smdpGeom->getId(hit->module(), abs((Int_t)hit->eta()), abs((Int_t)hit->sub() + 1), idNext);
				tables->getStatus(hit->detector() - kBarrelEmcTowerId + 1, idNext, status);
				if (status != 1) clusterData.badClose |= 8;
			}
		} else {
			clusterData.badClose |= 128;
		}
	}
  }
}

//_____________________________________________________________________________
void getPointData(const StEmcPoint *point, StMaker *trigSimMaker, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, StBemcTables *tables, triggered_type triggersHT1, triggered_type triggersHT2, TMyPointData &pointData) {
  if (!point) return;
  pointData.etaCoord = point->position().pseudoRapidity();
  pointData.phiCoord = point->position().phi();
  pointData.energy = point->energy();
  pointData.trackDeta = point->sizeAtDetector(kBarrelSmdEtaStripId);
  pointData.trackDphi = point->sizeAtDetector(kBarrelSmdPhiStripId);
  pointData.trackDeta2 = point->deltaEta();
  pointData.trackDphi2 = point->deltaPhi();
  pointData.trackMCDeta = point->sizeAtDetector(kBarrelEmcTowerId);
  pointData.trackMCDphi = point->sizeAtDetector(kBarrelEmcPreShowerId);
  pointData.clusterBTOW.energy = 0;
#ifdef SAVE_BPRS
  pointData.clusterBPRS.energy = 0;
#endif
  pointData.clusterBSMDE.energy = 0;
  pointData.clusterBSMDP.energy = 0;
  if (point->cluster(kBarrelEmcTowerId).size() > 0) getClusterData(point->cluster(kBarrelEmcTowerId)[0], emcGeom, smdeGeom, smdpGeom, psdGeom, tables, pointData.clusterBTOW);
#ifdef SAVE_BPRS
  if (point->cluster(kBarrelEmcPreShowerId).size() > 0) getClusterData(point->cluster(kBarrelEmcPreShowerId)[0], emcGeom, smdeGeom, smdpGeom, psdGeom, tables, pointData.clusterBPRS);
#endif
  if (point->cluster(kBarrelSmdEtaStripId).size() > 0) getClusterData(point->cluster(kBarrelSmdEtaStripId)[0], emcGeom, smdeGeom, smdpGeom, psdGeom, tables, pointData.clusterBSMDE);
  if (point->cluster(kBarrelSmdPhiStripId).size() > 0) getClusterData(point->cluster(kBarrelSmdPhiStripId)[0], emcGeom, smdeGeom, smdpGeom, psdGeom, tables, pointData.clusterBSMDP);

  Bool_t triggeredHT1 = false;
  Bool_t triggeredHT2 = false;
  StPi0TriggerSimulatorMaker *trgSimFinal = trigSimMaker ? dynamic_cast<StPi0TriggerSimulatorMaker*>(trigSimMaker) : 0;
  const StPtrVecEmcCluster &bEmcClusters = point->cluster(kBarrelEmcTowerId);
  for(StPtrVecEmcClusterConstIterator cIter = bEmcClusters.begin();cIter != bEmcClusters.end(); cIter++) {
	const StEmcCluster *cluster = *cIter;
	if (cluster) {
		const StPtrVecEmcRawHit& bEmcHits = cluster->hit();
		for(StPtrVecEmcRawHitConstIterator hIter = bEmcHits.begin();hIter != bEmcHits.end(); hIter++) {
			const StEmcRawHit *hit = *hIter;
			if (hit) {
				Int_t id;
				if (emcGeom) emcGeom->getId(hit->module(), abs((Int_t)hit->eta()), abs((Int_t)hit->sub()), id);
				if (trgSimFinal) {
					triggeredHT1 |= trgSimFinal->towerTriggeredHT1[id - 1];
					triggeredHT2 |= trgSimFinal->towerTriggeredHT2[id - 1];
				}
			}
		}
	}
  }
  if (triggeredHT1) pointData.trigger.triggered |= triggersHT1;
  if (triggeredHT2) pointData.trigger.triggered |= triggersHT2;

  Int_t mod, eta, sub;
  pointData.towerCenterDeta = -1;
  pointData.towerCenterDphi = -1;
  if (emcGeom && (emcGeom->getBin(point->position().phi(), point->position().pseudoRapidity(), mod, eta, sub) == 0)) {
	if ((eta >= 1) && (eta <= 20) && (sub >= 1) && (sub <= 2)) {
		Float_t towerCenterEta, towerCenterPhi;
		emcGeom->getEta(mod, eta, towerCenterEta);
		emcGeom->getPhi(mod, sub, towerCenterPhi);
		pointData.towerCenterDeta = point->position().pseudoRapidity() - towerCenterEta;
		pointData.towerCenterDphi = point->position().phi() - towerCenterPhi;
		while (pointData.towerCenterDphi < -TMath::Pi()) pointData.towerCenterDphi += TMath::TwoPi();
		while (pointData.towerCenterDphi >= TMath::Pi()) pointData.towerCenterDphi -= TMath::TwoPi();
	}
  }
}

//_____________________________________________________________________________
void getTriggerData(const StEvent *event, const UInt_t *triggerIDs, TMyTriggerData &triggerData) {
  triggerData.triggered = 0;
  if (!event) return;
  if (event->triggerIdCollection() && event->triggerIdCollection()->nominal()) {  
    const StTriggerId *triggerId = event->triggerIdCollection()->nominal();
    if (triggerId) {
      UInt_t ind = 0;
      while (triggerIDs && triggerIDs[ind] && (ind < (sizeof(triggered_type) * 8))) {
        triggerData.triggered |= (triggerId->isTrigger(triggerIDs[ind])) ? (1 << ind) : 0;
	ind++;
      }
    }
    //cout << *(event->triggerIdCollection()->nominal());
  }
}

//_____________________________________________________________________________
void getTriggerDataFromDB(Int_t runId, const UInt_t *triggerIDs, const Char_t *connectString, TMyTriggerData &triggerData) {
/*
	TSQLServer *db = 0;
	Int_t nTry = 0;
	Int_t maxTries = 20;
	while (!db && (nTry < maxTries)) {
		db = TSQLServer::Connect(connectString, "", "");
		Int_t delayInSeconds = 20;
		time_t startTime, curTime;
		time(&startTime);
		if (!db) {
			{LOG_ERROR << "Connection to db failed. Re-trying after " << delayInSeconds << " seconds..." << endm;}
			do {
				time(&curTime);
			} while((curTime - startTime) < delayInSeconds);
		}
		nTry+=1;
	}
	if (!db) {
		{LOG_ERROR << "Could not connect to db server after " << nTry << " tries!" << endm;}
		return;
	}
	TString query("select psL0 from L0TriggerInfo where runNumber = "); query += runId;
	Int_t ind; Bool_t orNeeded;

	TString queryMB(query); ind = 0; queryMB.Append(" and ("); orNeeded = false;
	while (triggerIDsMB && triggerIDsMB[ind]) {
		if (orNeeded) queryMB.Append(" or ");
		queryMB.Append("offlineTriggerId = ");
		queryMB += triggerIDsMB[ind];
		orNeeded = true;
		ind++;
	}
	queryMB.Append(")");

	TString queryHT1(query); ind = 0; queryHT1.Append(" and ("); orNeeded = false;
	while (triggerIDsHT1 && triggerIDsHT1[ind]) {
		if (orNeeded) queryHT1.Append(" or ");
		queryHT1.Append("offlineTriggerId = ");
		queryHT1 += triggerIDsHT1[ind];
		orNeeded = true;
		ind++;
	}
	queryHT1.Append(")");

	TString queryHT2(query); ind = 0; queryHT2.Append(" and ("); orNeeded = false;
	while (triggerIDsHT2 && triggerIDsHT2[ind]) {
		if (orNeeded) queryHT2.Append(" or ");
		queryHT2.Append("offlineTriggerId = ");
		queryHT2 += triggerIDsHT2[ind];
		orNeeded = true;
		ind++;
	}
	queryHT2.Append(")");

	TSQLRow* row = 0;
	TSQLResult* result = 0;

	result = db->Query(queryMB);
	if (!result) {
		//{LOG_ERROR << "SQL query for MinBias failed..." << endm;}
	} else {
		row = 0; while(row = result->Next(), row) triggerData.psMB1 = (Int_t)atof(row->GetField(0));
	}

	result = db->Query(queryHT1);
	if (!result) {
		//{LOG_ERROR << "SQL query for HighTower-1 failed..." << endm;}
	} else {
		row = 0; while(row = result->Next(), row) triggerData.psHT1 = (Int_t)atof(row->GetField(0));
	}

	result = db->Query(queryHT2);
	if (!result) {
		//{LOG_ERROR << "SQL query for HighTower-2 failed..." << endm;}
	} else {
		row = 0; while(row = result->Next(), row) triggerData.psHT2 = (Int_t)atof(row->GetField(0));
	}
	db->Close();
	delete db;
*/
}

//_____________________________________________________________________________
void getTriggerSimulatedData(const StMaker *trigSimMaker, TMyTriggerSimulatedData &triggerSimulatedData) {
  const StPi0TriggerSimulatorMaker *trgSim = trigSimMaker ? dynamic_cast<const StPi0TriggerSimulatorMaker*>(trigSimMaker) : 0;
  if (trgSim) {
    const TMyTriggerSimulatedData *data = trgSim->getData();
    if (data) {
      triggerSimulatedData = *data;
    } else {LOG_ERROR << "Trigger simulator has NO data!" << endm;}
  }
}

//_____________________________________________________________________________
void getEventData(const StEvent *event, const StMcEvent *mc_event, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, StBemcTables *tables, const UInt_t *triggers, StMaker *trigSimMaker, StMaker *trigSimMakerEmbed, StMaker *trigSimMakerFinal, StMaker *adcToEMaker, triggered_type triggersHT1, triggered_type triggersHT2, Float_t jetConeRadius, StMaker *jetMaker, TMyEventData &eventData) {
  if (!event) return;
  const StEmcCollection *emcCollection = event->emcCollection();
  eventData.runId = event->runId();
  eventData.eventId = event->id();
  eventData.corruptedCrates = 0;
  StEmcADCtoEMaker *adcToE = adcToEMaker ? dynamic_cast<StEmcADCtoEMaker*>(adcToEMaker) : 0;
  eventData.corruptedCrates |= (adcToE && adcToE->isCorrupted()) ? (1 << ((sizeof(eventData.corruptedCrates) * 8) - 1)) : 0;
  getTriggerData(event, triggers, eventData.trigger);
  if (mc_event) {
    const StPtrVecMcTrack &tracks = mc_event->tracks();
    for (UInt_t i = 0;i < tracks.size();i++) {
	const StMcTrack *mcTrack = tracks[i];
	if (mcTrack && (i == 0) && (!tracks[i]->isShower())) {
	    TMySimulatedParticleData simulatedParticleData;
	    getMCParticleData(mcTrack, event, trigSimMakerFinal, tables, emcGeom, smdeGeom, smdpGeom, psdGeom, triggersHT1, triggersHT2, simulatedParticleData);
	    eventData.simulatedParticle = simulatedParticleData.summary;
	}
    }
  }

  getTriggerSimulatedData(trigSimMaker, eventData.triggerSimulated);
  getTriggerSimulatedData(trigSimMakerEmbed ? trigSimMakerEmbed : trigSimMaker, eventData.triggerSimulatedEmbed);
  getTriggerSimulatedData(trigSimMakerFinal ? trigSimMakerFinal : (trigSimMakerEmbed ? trigSimMakerEmbed : trigSimMaker), eventData.triggerSimulatedFinal);
  const StPrimaryVertex* primaryVertex = event->primaryVertex();
  eventData.zTPC = 0;
  eventData.nPrimary = 0;
  eventData.nGlobal = event->trackNodes().size();
  eventData.nPoints = 0;
  eventData.nClustersBTOW = 0;
#ifdef SAVE_BPRS
  eventData.nClustersBPRS = 0;
#endif
  eventData.nClustersBSMDE = 0;
  eventData.nClustersBSMDP = 0;
  eventData.nHitsBTOW = 0;
  eventData.nHitsBTOWstuckbit = 0;
#ifdef SAVE_BPRS
  eventData.nHitsBPRS = 0;
#endif
  eventData.nHitsBSMDE = 0;
  eventData.nHitsBSMDP = 0;
  eventData.totalBEMCHitsEnergy = 0;
  eventData.totalBEMCPointsEnergy = 0;
  eventData.totalBEMCPointsEt = 0;
  //eventData.uncorrectedNumberOfFtpcPrimaries = uncorrectedNumberOfFtpcPrimaries(*event); // buggy!
  eventData.uncorrectedNumberOfFtpcPrimariesEast = 0;
  eventData.uncorrectedNumberOfFtpcPrimariesWest = 0;
  eventData.uncorrectedNumberOfTpcPrimaries = 0;
  eventData.totalTPCPt = 0;
  if (primaryVertex) {
	  const StSPtrVecPrimaryTrack& tracks = primaryVertex->daughters();
	  for (StSPtrVecPrimaryTrackConstIterator iter = tracks.begin();iter != tracks.end();iter++) {
	    StTrack* track = (*iter);
	    if (track) {
		if (track->geometry() && track->node() && (track->fitTraits().numberOfFitPoints() >= 6) && (track->fitTraits().numberOfFitPoints() <= 11)) {
			if (track->node()->track(global)) {
			  if (track->node()->track(global)->geometry()) {
				Float_t eta = track->geometry()->momentum().pseudoRapidity();
				Float_t pt = track->geometry()->momentum().perp();
				Float_t dca = track->node()->track(global)->geometry()->helix().distance(primaryVertex->position());
				if ((pt < 3) && (dca < 3)) {
					if ((eta >= 2.8) && (eta < 3.8)) {
						eventData.uncorrectedNumberOfFtpcPrimariesWest++;
					}
					if ((eta <= -2.8) && (eta > -3.8)) {
						eventData.uncorrectedNumberOfFtpcPrimariesEast++;
					}
				}
			  }
			}
		}
		if (track->geometry() && track->node() && (track->fitTraits().numberOfFitPoints() > 15)) {
			if (track->node()->track(global)) {
			  if (track->node()->track(global)->geometry()) {
				Float_t eta = track->geometry()->momentum().pseudoRapidity();
				Float_t pt = track->geometry()->momentum().perp();
				Float_t dca = track->node()->track(global)->geometry()->helix().distance(primaryVertex->position());
				if ((pt > 0.2) && (dca < 3)) {
					if (fabs(eta) < 2.8) {
						eventData.uncorrectedNumberOfTpcPrimaries++;
						eventData.totalTPCPt += pt;
					}
				}
			  }
			}
		}
	    }
	  }
  } 
  if (primaryVertex) {
    eventData.zTPC = primaryVertex->position().z();
    eventData.nPrimary = primaryVertex->daughters().size();
  }

  eventData.bunchCrossingId7bit = 0;
  const StL0Trigger *l0trigger = (const StL0Trigger*)event->l0Trigger();
  if (l0trigger) {
    eventData.bunchCrossingId7bit = l0trigger->bunchCrossingId7bit(eventData.runId);
  }
  eventData.bunchCrossingId48bit = 0;
  eventData.spinBit = 0;
  const StTriggerData *triggerData = event->triggerData();
  if (triggerData) {
    eventData.bunchCrossingId48bit = triggerData->bunchId48Bit();
    eventData.spinBit = triggerData->spinBit();
    {LOG_DEBUG << " bunch " << triggerData->bunchId7Bit() << endm;}
    {LOG_DEBUG << " bunch48 " << eventData.bunchCrossingId48bit << endm;}
    {LOG_DEBUG << " spinBit " << eventData.spinBit << endm;}
  }
  const StTriggerDetectorCollection *trigDet = event->triggerDetectorCollection();
  if (trigDet) {
#ifdef MYPI0ANALYSIS_OLDBBCMAKER
    StBbcTriggerDetector &bbc = const_cast<StBbcTriggerDetector&>(trigDet->bbc());
    eventData.bbcEarliestEast = 255;
    {unsigned short earliest=0; for (int i=0; i<16; i++) if (bbc.tdc(i)<245 && bbc.tdc(i)>earliest && bbc.adc(i)>5 ) earliest = bbc.tdc(i); if (earliest>0) eventData.bbcEarliestEast = earliest;}
    eventData.bbcEarliestWest = 255;
    {unsigned short earliest=0; for (int i=24; i<40; i++) if (bbc.tdc(i)<245 && bbc.tdc(i)>earliest && bbc.adc(i)>5 ) earliest = bbc.tdc(i); if (earliest>0) eventData.bbcEarliestWest = earliest;}
#else
    const StBbcTriggerDetector &bbc = trigDet->bbc();
    eventData.bbcEarliestEast = bbc.tdcEarliestEast();
    eventData.bbcEarliestWest = bbc.tdcEarliestWest();
#endif
    const StZdcTriggerDetector &zdc = trigDet->zdc();
    //eventData.zdcE = zdc.adc(4); // unattenuated ADC East
    //eventData.zdcW = zdc.adc(0); // unattenuated ADC West
    //eventData.zdcE = zdc.adc(13); // attenuated ADC East
    //eventData.zdcW = zdc.adc(10); // attenuated ADC West
    //eventData.zdcEtdc = zdc.adc(8); // TDC East
    //eventData.zdcWtdc = zdc.adc(9); // TDC West
    {LOG_DEBUG << "ZDC: <" << zdc.adc(4) << "," << zdc.adc(0) << "> [" << zdc.adc(13) << "," << zdc.adc(10) << "] (" << zdc.adc(8) << "," << zdc.adc(9) << ")" << endm;}
  }
  {LOG_DEBUG << "TPC Z = " << eventData.zTPC << endm;}

  if (!emcCollection) return;
  const StEmcDetector *detector = 0;
  const StSPtrVecEmcPoint& bEmcPoints = emcCollection->barrelPoints();
  eventData.nPoints = bEmcPoints.size();
  const StEmcClusterCollection *clusterCollection = 0;
  detector = emcCollection->detector(kBarrelEmcTowerId);
  if (detector) {
    clusterCollection = detector->cluster();
    if (clusterCollection) eventData.nClustersBTOW = clusterCollection->clusters().size();
  }
#ifdef SAVE_BPRS
  detector = emcCollection->detector(kBarrelEmcPreShowerId);
  if (detector) {
    clusterCollection = detector->cluster();
    if (clusterCollection) eventData.nClustersBPRS = clusterCollection->clusters().size();
  }
#endif
  detector = emcCollection->detector(kBarrelSmdEtaStripId);
  if (detector) {
    clusterCollection = detector->cluster();
    if (clusterCollection) eventData.nClustersBSMDE = clusterCollection->clusters().size();
  }
  detector = emcCollection->detector(kBarrelSmdPhiStripId);
  if (detector) {
    clusterCollection = detector->cluster();
    if (clusterCollection) eventData.nClustersBSMDP = clusterCollection->clusters().size();
  }
  eventData.acceptanceBTOW = 0;
#ifdef SAVE_BPRS
  eventData.acceptanceBPRS = 0;
#endif
  eventData.acceptanceBSMDE = 0;
  eventData.acceptanceBSMDP = 0;
  const UInt_t numCrates = 30;
  Int_t cratesGoodTowers[numCrates];
  Int_t cratesHits[numCrates];
  for (UInt_t icrate = 0;icrate < numCrates;icrate++) {cratesGoodTowers[icrate] = 0; cratesHits[icrate] = 0;}
  detector = emcCollection->detector(kBarrelEmcTowerId);
  if (detector) {
    Int_t numMod = detector->numberOfModules();
    for (Int_t imod = 1;imod <= numMod;imod++) {
      const StEmcModule *module = detector->module(imod);
      if (module) {
	for (StPtrVecEmcRawHitConstIterator it = module->hits().begin();it != module->hits().end();it++) {
	  const StEmcRawHit *hit = *it;
	  if (hit) {
	    if (hit->energy() > 0) eventData.nHitsBTOW++;
	    if ((hit->energy() > 0) && ((hit->adc() & 7) == 4)) eventData.nHitsBTOWstuckbit++;
	    eventData.totalBEMCHitsEnergy += hit->energy();
	    Int_t id = -1;
	    if (emcGeom) emcGeom->getId(hit->module(), abs((Int_t)hit->eta()), abs((Int_t)hit->sub()), id);
	      Int_t half = hit->module() * 2 - (2 - hit->sub()) + 3;
	      if (half > 120) half -= 120;
	      Int_t crate = 30 - (half - 1) / 8;
	      cratesHits[crate - 1] += 1;
	  }
	}
      }
	for (Int_t sub = 1;sub <= 2;sub++) for (Int_t eta = 1;eta <= 20;eta++) {
	  if (emcGeom) {
	    Int_t id = 0;
	    Bool_t statusOk = true;
	    emcGeom->getId(imod, eta, sub, id);
	    Int_t status = 0;
	    tables->getStatus(kBarrelEmcTowerId - kBarrelEmcTowerId + 1, id, status);
	    statusOk = (status == 1);
	    if (statusOk) {
	      Int_t half = imod * 2 - (2 - sub) + 3;
	      if (half > 120) half -= 120;
	      Int_t crate = 30 - (half - 1) / 8;
	      cratesGoodTowers[crate - 1] += 1;
	      eventData.acceptanceBTOW++;
	    }
	  }
	}
    }
  }
    if (eventData.nHitsBTOW || eventData.nHitsBTOWstuckbit) {
	{LOG_DEBUG << "nHitsBTOW = " << eventData.nHitsBTOW << ", stuckbit = " << eventData.nHitsBTOWstuckbit << endm;}
    }
#ifdef SAVE_BPRS
  detector = emcCollection->detector(kBarrelEmcPreShowerId);
  if (detector) {
    Int_t numMod = detector->numberOfModules();
    for (Int_t imod = 1;imod <= numMod;imod++) {
      const StEmcModule *module = detector->module(imod);
      if (module) {
	eventData.nHitsBPRS += module->hits().size();
	for (Int_t sub = 1;sub <= 2;sub++) for (Int_t eta = 1;eta <= 20;eta++) {
	  if (psdGeom) {
	    Int_t id = 0;
	    Bool_t statusOk = true;
	    psdGeom->getId(imod, eta, sub, id);
	    Int_t status = 0;
	    tables->getStatus(kBarrelEmcPreShowerId - kBarrelEmcTowerId + 1, id, status);
	    statusOk = (status == 1);
	    if (statusOk) {
	      eventData.acceptanceBPRS++;
	    }
	  }
	}
      }
    }
  }
#endif
  detector = emcCollection->detector(kBarrelSmdEtaStripId);
  if (detector) {
    Int_t numMod = detector->numberOfModules();
    for (Int_t imod = 1;imod <= numMod;imod++) {
      const StEmcModule *module = detector->module(imod);
      if (module) {
	eventData.nHitsBSMDE += module->hits().size();
	for (Int_t sub = 1;sub <= 1;sub++) for (Int_t eta = 1;eta <= 150;eta++) {
	  if (smdeGeom) {
	    Int_t id = 0;
	    Bool_t statusOk = true;
	    smdeGeom->getId(imod, eta, sub, id);
	    Int_t status = 0;
	    tables->getStatus(kBarrelSmdEtaStripId - kBarrelEmcTowerId + 1, id, status);
	    statusOk = (status == 1);
	    if (statusOk) {
	      eventData.acceptanceBSMDE++;
	    }
	  }
	}
      }
    }
  }
  detector = emcCollection->detector(kBarrelSmdPhiStripId);
  if (detector) {
    Int_t numMod = detector->numberOfModules();
    for (Int_t imod = 1;imod <= numMod;imod++) {
      const StEmcModule *module = detector->module(imod);
      if (module) {
	eventData.nHitsBSMDP += module->hits().size();
	for (Int_t sub = 1;sub <= 15;sub++) for (Int_t eta = 1;eta <= 10;eta++) {
	  if (smdpGeom) {
	    Int_t id = 0;
	    Bool_t statusOk = true;
	    smdpGeom->getId(imod, eta, sub, id);
	    Int_t status = 0;
	    tables->getStatus(kBarrelSmdPhiStripId - kBarrelEmcTowerId + 1, id, status);
	    statusOk = (status == 1);
	    if (statusOk) {
	      eventData.acceptanceBSMDP++;
	    }
	  }
	}
      }
    }
  }

  //Float_t badCrates = 0;
  detector = emcCollection->detector(kBarrelEmcTowerId);
  for (UInt_t icrate = 0;icrate < numCrates;icrate++) {
    if (detector) eventData.corruptedCrates |= (detector->crateStatus(icrate) == crateHeaderCorrupt) ? (1 << icrate) : 0;
    if (cratesGoodTowers[icrate] > 0) {
      if (cratesHits[icrate] > 0) {
	if (cratesHits[icrate] == cratesGoodTowers[icrate]) {
	  //badCrates += 0.0001;
	} else {
	  //badCrates += 0.01;
	}
      } else {
	//badCrates += 1;
	//eventData.corruptedCrates |= (1 << icrate);
      }
    } else {
      //badCrates += 100;
    }
  }

  StThreeVectorF vPos;
  if (event->primaryVertex()) {vPos = event->primaryVertex()->position();} else {if (mc_event) if (mc_event->primaryVertex()) {vPos = mc_event->primaryVertex()->position();}}
  eventData.totalBEMCPointsEt = 0;
  eventData.totalBEMCPointsEnergy = 0;
  const StSPtrVecEmcPoint &points = emcCollection->barrelPoints();
  for(StSPtrVecEmcPointConstIterator pointIter = points.begin();pointIter != points.end();pointIter++) {
    const StEmcPoint *point = *pointIter;
    if (point) {
	StThreeVectorF pointPosition = point->position() - vPos;
	Float_t pointE = point->energy();
	Float_t pointEt = pointE * sin(pointPosition.theta());
	eventData.totalBEMCPointsEt += pointEt;
	eventData.totalBEMCPointsEnergy += pointE;
    }
  }
  Float_t sinHarmSum = 0;
  Float_t cosHarmSum = 0;
  for(StSPtrVecEmcPointConstIterator pointIter = points.begin();pointIter != points.end();pointIter++) {
    const StEmcPoint *point = *pointIter;
    if (point) {
	StThreeVectorF pointPosition = point->position() - vPos;
	Float_t pointE = point->energy();
	Float_t pointEt = pointE * sin(pointPosition.theta());
	sinHarmSum += pointEt * sin(2.0 * pointPosition.phi());
	cosHarmSum += pointEt * cos(2.0 * pointPosition.phi());
    }
  }
  Float_t phiOffset = (sinHarmSum != 0) ? (atan(cosHarmSum / sinHarmSum)) : (TMath::Pi() / 2.0);
  while(phiOffset < -TMath::Pi()) phiOffset += TMath::Pi();
  while(phiOffset > -TMath::Pi()) phiOffset -= TMath::Pi();
  StThreeVectorF jetPosition1;
  StThreeVectorF jetPosition2;
  for(StSPtrVecEmcPointConstIterator pointIter = points.begin();pointIter != points.end();pointIter++) {
    const StEmcPoint *point = *pointIter;
    if (point) {
	StThreeVectorF pointPosition = point->position() - vPos;
	StThreeVectorF pointPositionEnergy = pointPosition;
	pointPositionEnergy *= (point->energy() * sin(pointPosition.theta())) / pointPositionEnergy.mag();
	if ((Int_t((pointPositionEnergy.phi() - phiOffset)/TMath::Pi()) % 2) == 0) {
	    jetPosition1 += pointPositionEnergy;
	} else {
	    jetPosition2 += pointPositionEnergy;
	}
    }
  }
  Float_t etaSeed = (jetPosition1.mag() > jetPosition2.mag()) ? jetPosition1.pseudoRapidity() : jetPosition2.pseudoRapidity();
  Float_t phiSeed = (jetPosition1.mag() > jetPosition2.mag()) ? jetPosition1.phi() : jetPosition2.phi();
#ifdef SAVE_JETMY
  findJet(points, vPos.z(), jetConeRadius, etaSeed, phiSeed, eventData.jetMy);
#endif

    eventData.jet.eT = 0.0;
    StJetMaker *jetMk = jetMaker ? dynamic_cast<StJetMaker*>(jetMaker) : 0;
    if (jetMk) {
	{LOG_DEBUG << "Getting a jet from StJetMaker" << endm;}
	StJetMaker::jetBranchesMap &jetsMap = jetMk->getJets();
	for (StJetMaker::jetBranchesMap::iterator mapIter = jetsMap.begin();mapIter != jetsMap.end();++mapIter) {
	    //{LOG_DEBUG << "Jet branch " << (*mapIter).first << endm;}
	    StppJetAnalyzer *jetAnalyzer = (*mapIter).second;
	    if (jetAnalyzer) {
		StppJetAnalyzer::StjJetList &jetList = jetAnalyzer->getJets();
		Float_t eTMax = -100;
		for (StppJetAnalyzer::StjJetList::iterator jetIter = jetList.begin();jetIter != jetList.end();++jetIter) {
		    StProtoJet &jet = *jetIter;
		    //{LOG_DEBUG << "Jet eta = " << jet.eta() << ", phi = " << jet.phi() << ", eT = " << jet.eT() << ", size = " << jet.size() << endm;}
		    if (jet.eT() > eTMax) {
			eTMax = jet.eT();
			eventData.jet.eta = jet.eta();
			eventData.jet.phi = jet.phi();
			eventData.jet.eT = jet.eT();
			eventData.jet.nPoints = jet.size();
			eventData.jet.nTracks = jet.size();
		    }
		}
	    }
	    //{LOG_DEBUG << "Jet branch " << (*mapIter).first << " finished." << endm;}
	}
	if (eventData.jet.eT == 0.0) {LOG_INFO << "StJetMaker didn't provide a jet" << endm;}
    }
    if (eventData.jet.eT == 0.0) {
#ifdef SAVE_JETMY
	eventData.jet = eventData.jetMy;
#else
	findJet(points, vPos.z(), jetConeRadius, etaSeed, phiSeed, eventData.jet);
#endif
    }
}

//_____________________________________________________________________________
void findJet(const StSPtrVecEmcPoint &emcPoints, Float_t vZ, Float_t coneSize, Float_t etaSeed, Float_t phiSeed, TMyJetData &jet) {
    {LOG_DEBUG << "Running the jet cone algo on BEMC points" << endm;}
    // Jet cone algorithm, using the BEMC points
    StThreeVectorF vPos(0, 0, vZ);
    Float_t newJetEta = etaSeed;
    Float_t newJetPhi = phiSeed;
    Float_t lastJetEta = newJetEta + 0.001;
    Float_t lastJetPhi = newJetPhi + 0.001;
    Float_t newJetEnergy = 0;
    Float_t newJetNpoints = 0;
    Float_t jetStep = 1.0;
    Int_t iteration = 0;
    Float_t radius = 1.0;
    while (((newJetEta != lastJetEta) || (newJetPhi != lastJetPhi)) && (iteration < 20) && (jetStep > 0.0001)) {
	iteration++;
	Float_t sumW = 0;
	Float_t sumWDist = 0;
	Float_t sumWEta = 0;
	Float_t sumWDEta = 0;
	Float_t sumWDPhi = 0;
	newJetEnergy = 0;
	newJetNpoints = 0;
	for(StSPtrVecEmcPointConstIterator pointIter = emcPoints.begin();pointIter != emcPoints.end();pointIter++) {
	    const StEmcPoint *point = *pointIter;
	    if (point) {
		StThreeVectorF pointPosition = point->position() - vPos;
		radius = pointPosition.perp();
		Float_t pointE = point->energy();
		Float_t pointEt = pointE * sin(pointPosition.theta());
		Float_t eta = pointPosition.pseudoRapidity() - newJetEta;
		Float_t phi = pointPosition.phi() - newJetPhi;
		while(phi < -TMath::Pi()) phi += TMath::TwoPi();
		while(phi > TMath::Pi()) phi -= TMath::TwoPi();
		Float_t deta = eta;
		Float_t dphi = phi;
		Float_t dist2 = (deta * deta) + (dphi * dphi);
		Float_t dist = sqrt(dist2);
		//Float_t sigma = 0.7;
		//Float_t wDist = /*(1.0 / (sqrt(TMath::TwoPi()) * sigma)) * */exp(- (dist2 / (2 * sigma * sigma)));
		Float_t wDist = (dist < coneSize) ? 1.0 : 0.0;
		Float_t wEt = pointEt;
		Float_t w = wDist * wEt;
		sumW += w;
		sumWDist += wDist;
		sumWEta += wEt;
		sumWDEta += w * eta;
		sumWDPhi += w * phi;
		newJetEnergy += wDist * pointEt;
		newJetNpoints += wDist * 1.0;
	    }
	}
	if (sumW != 0) {
	    sumWDEta /= sumW;
	    sumWDPhi /= sumW;
	} else {
	    sumWDEta = 0;
	    sumWDPhi = 0;
	}
	jetStep = sqrt((sumWDEta * sumWDEta) + (sumWDPhi * sumWDPhi));
	lastJetEta = newJetEta;
	lastJetPhi = newJetPhi;
	newJetEta += sumWDEta;
	newJetPhi += sumWDPhi;
    }
    jet.eta = newJetEta;
    jet.phi = newJetPhi;
    jet.eT = -newJetEnergy;
    jet.nPoints = (UShort_t)newJetNpoints;
    jet.nTracks = 0;
    {LOG_INFO << "My jet eta = " << jet.eta << ", phi = " << jet.phi << ", eT = " << jet.eT << ", nPoints = " << jet.nPoints << endm;}
}
//_____________________________________________________________________________
void getMCDecayData(const StMcTrack *mcTrack, const StEvent *event, StMaker *trigSimMaker, StBemcTables *tables, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, triggered_type triggersHT1, triggered_type triggersHT2, TMySimulatedDecayData &mcDecayData) {
    if (!mcTrack) return;
    getMCParticleData(mcTrack, event, trigSimMaker, tables, emcGeom, smdeGeom, smdpGeom, psdGeom, triggersHT1, triggersHT2, mcDecayData.parent);
    const StMcVertex *decayVert = mcTrack->stopVertex();
    if (decayVert) {
	Bool_t firstDaughter = true;
        const StMcTrack *daughter1 = 0;
        const StMcTrack *daughter2 = 0;
	for (StMcTrackConstIterator tit = decayVert->daughters().begin();tit != decayVert->daughters().end();tit++) {
	    const StMcTrack *daughter = *tit;
	    if (daughter && (daughter->geantId() == 1)) {
		if (firstDaughter) daughter1 = daughter; else daughter2 = daughter;
		TMySimulatedParticleData &daughterData = firstDaughter ? mcDecayData.daughter1 : mcDecayData.daughter2;
		firstDaughter = false;
		getMCParticleData(daughter, event, trigSimMaker, tables, emcGeom, smdeGeom, smdpGeom, psdGeom, triggersHT1, triggersHT2, daughterData);
	    }
	}
    }
}

//_____________________________________________________________________________
void getMCParticleData(const StMcTrack *mcTrack, const StEvent *event, StMaker *trigSimMaker, StBemcTables *tables, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, triggered_type triggersHT1, triggered_type triggersHT2, TMySimulatedParticleData &mcParticleData) {
    if (!mcTrack) return;
    mcParticleData.summary.geantId = mcTrack->geantId();
    mcParticleData.summary.daughters = 0;
    mcParticleData.summary.z = 0;
    mcParticleData.summary.pT = mcTrack->momentum().perp();
    mcParticleData.theta = mcTrack->momentum().theta();
    mcParticleData.phi = mcTrack->momentum().phi();
    mcParticleData.phiCoord = 0;
    mcParticleData.startRadius = 0;
    mcParticleData.stopRadius = 0;
    const StMcVertex *startVert = mcTrack->startVertex();
    if (startVert) {
	mcParticleData.summary.z = startVert->position().z();
	mcParticleData.startRadius = startVert->position().perp();
	mcParticleData.phiCoord = startVert->position().phi();
    }
    const StMcVertex *stopVert = mcTrack->stopVertex();
    if (stopVert) {
	mcParticleData.stopRadius = stopVert->position().perp();
	for (StMcTrackConstIterator tit = stopVert->daughters().begin();tit != stopVert->daughters().end();tit++) {
	    const StMcTrack *daughter = *tit;
	    if (daughter) {
		if ((mcParticleData.summary.daughters & 0x0F) <= 0x0F) mcParticleData.summary.daughters += (1 << 0); // count all decay products
		if (((mcParticleData.summary.daughters & 0x30) <= 0x30) && (daughter->geantId() == 1)) mcParticleData.summary.daughters += (1 << 4); // count decay photons
		if (((mcParticleData.summary.daughters & 0xC0) <= 0xC0) && (daughter->geantId() == 7)) mcParticleData.summary.daughters += (1 << 6); // count decay neutral pions
	    }
	}
    }
}

//_____________________________________________________________________________
void getSMDThresholdData(const StEmcDetector *detector, StEmcGeom *geom, StBemcTables *tables, Float_t threshold, Bool_t thresholdEnergy, Bool_t thresholdEt, TMySMDThresholdData &smdThresholdData) {
    if (!detector || !geom || !tables || (threshold <= 0)) return;
    smdThresholdData.threshold = threshold;
    smdThresholdData.channelsAboveThreshold.SetBitNumber(18000, 1);
    smdThresholdData.channelsAboveThreshold.SetBitNumber(18000, 0); // to reserve the space
    for (UInt_t moduleIndex = 1;moduleIndex <= detector->numberOfModules();moduleIndex++) {
	const StEmcModule *module = detector->module(moduleIndex);
	if (module) {
	    for (StSPtrVecEmcRawHitConstIterator hitIter = module->hits().begin();hitIter != module->hits().end();hitIter++) {
		const StEmcRawHit *hit = *hitIter;
		if (hit) {
		    Int_t id = 0;
		    if (geom->getId((Int_t)hit->module(), abs((Int_t)hit->eta()), abs((Int_t)hit->sub()), id) == 0) {
			Bool_t aboveThresh = false;
			Int_t status = 0;
			tables->getStatus(hit->detector() - kBarrelEmcTowerId + 1, id, status);
			if (status == 1) {
			    if (thresholdEt) {
				Float_t theta = 0;
				geom->getTheta(hit->module(), abs((Int_t)hit->eta()), theta);
				Float_t eT = hit->energy() * sin(theta);
				if (eT > threshold) {
				    aboveThresh = true;
				}
			    } else if (thresholdEnergy) {
				Float_t energy = hit->energy();
				if (energy > threshold) {
				    aboveThresh = true;
				}
			    } else {
				Int_t adc = hit->adc();
				Float_t pedestal = 0;
				Float_t pedestalRMS = 0;
				tables->getPedestal(hit->detector() - kBarrelEmcTowerId + 1, id, hit->calibrationType(), pedestal, pedestalRMS);
				if (adc > pedestal + threshold) {
				    aboveThresh = true;
				}
			    }
			}
			if (aboveThresh) {
	    		    smdThresholdData.channelsAboveThreshold.SetBitNumber(id, 1);
			}
		    }
		}
	    }
	}
    }
}

//_____________________________________________________________________________
void associateTracksWithEmcPoints(StEvent *event, StMcEvent *mc_event, StSPtrVecEmcPoint &emcPoints, StMaker *trigSimMaker, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, StEmcPosition *emcPosition, StBemcTables *tables) {
  if(!event) {
    return;
  }
/*
I am reusing the existing data members in StEmcPoint which I will not use anyway
to carry the track association data, in order not to invent a new container:

1.
point->getSizeAtDetector(kBarrelEmcTowerId)
point->getSizeAtDetector(kBarrelEmcPreShowerId)
These two I fill with the deltaEta and deltaPhi - the distance to the closest MC photon.
I also tried to associate the MC photons based not on the minimal distance but on the minimal distance/point_energy,
when I thought it may make the wrong association with the low-energy splitted cluster. This didn't work any better
than just the minimal distance association.

2.
point->getSizeAtDetector(kBarrelSmdEtaStripId)
point->getSizeAtDetector(kBarrelSmdPhiStripId)
These two I fill with the deltaEta and deltaPhi - the distance to the closest TPC track.

3.
point->getDeltaEta()
point->getDeltaPhi()
These two I fill with the deltaEta2 and deltaPhi2 - the distance to the second closest TPC track.

4.
point->track()
I fill it with dummy tracks to hold the abbreviated track information in nTracks().
(point->nTracks() & 1) means some TPC track got associated with this point. It may not pass other cuts later, though.
(point->nTracks() & 2) means some MC photon got associated with this point. It may not pass other cuts later, though.
(point->nTracks() & 4) means some MC photon got associated with this point based on distance/energy criterion.
*/

  // reset all existing track-to-point associations
  for(StSPtrVecEmcPointIterator it = emcPoints.begin(); it != emcPoints.end(); it++) {
    StEmcPoint *point = *it;
    if (point) {
      // deletes all elements
      // this is NOT a structural container, so the tracks don't get deleted
      point->track().clear();
      point->setSizeAtDetector(kBarrelEmcTowerId, 100000);
      point->setSizeAtDetector(kBarrelEmcPreShowerId, 100000);
      point->setSizeAtDetector(kBarrelSmdEtaStripId, 100000);
      point->setSizeAtDetector(kBarrelSmdPhiStripId, 100000);
      point->setDeltaEta(100000);
      point->setDeltaPhi(100000);
    }
  }
  Double_t bFld = 0;
  StEventSummary* summary = event->summary();
  if(summary) {
    bFld = summary->magneticField()/10.; // bFld in Tesla
  }
  StThreeVectorF vPos;
  if (event->primaryVertex()) {vPos = event->primaryVertex()->position();} else {if (mc_event) if (mc_event->primaryVertex()) {vPos = mc_event->primaryVertex()->position();}}

  StThreeVectorD position, momentum;
  StSPtrVecTrackNode& trackNodes = event->trackNodes();
  StTrack* track;
  Int_t mod, eta, sub;
  for(StSPtrVecEmcPointIterator it=emcPoints.begin(); it!=emcPoints.end(); it++) {
    StEmcPoint *point = *it;
    if (point == 0) continue;
    StThreeVectorF pointPosition = point->position() - vPos;
    Bool_t hasTrack = false;
    Float_t closestDist2 = 100000;
    Float_t closestDeta = 10000;
    Float_t closestDphi = 10000;
    Float_t closestDist22 = 100000;
    Float_t closestDeta2 = 10000;
    Float_t closestDphi2 = 10000;

    for (size_t nodeIndex = 0;(nodeIndex < trackNodes.size()) && (!hasTrack); nodeIndex++) {
      size_t numberOfTracksInNode = trackNodes[nodeIndex]->entries(global);
      for (size_t trackIndex = 0; (trackIndex < numberOfTracksInNode) && (!hasTrack); trackIndex++) {
	track = trackNodes[nodeIndex]->track(global,trackIndex);
	if (track && track->flag() >= 0) {
	  Bool_t ok = emcPosition->trackOnEmc(&position, &momentum, track, bFld, EMCRadius); // bFld in Tesla
	  if (!ok) continue;
	  // get (mod, eta, sub) from track
	  emcGeom->getBin(position.phi(), position.pseudoRapidity(), mod, eta, sub);
	  const StPtrVecEmcCluster &bEmcClusters = point->cluster(kBarrelEmcTowerId);
	  for(StPtrVecEmcClusterConstIterator cIter=bEmcClusters.begin();(cIter != bEmcClusters.end()) && (!hasTrack); cIter++) {
	    const StEmcCluster *cluster = *cIter;
	    if (cluster) {
	      const StPtrVecEmcRawHit& bEmcHits = cluster->hit();
	      for(StPtrVecEmcRawHitConstIterator hIter=bEmcHits.begin();(hIter != bEmcHits.end()) && (!hasTrack); hIter++) {
		const StEmcRawHit *hit = *hIter;
		if (hit) {
		  // compare (mod, eta, sub) with emc hit	    
		  if((mod == (Int_t)hit->module()) && (eta == (Int_t)hit->eta()) && (sub == (Int_t)hit->sub())) {
		    hasTrack = true;
		  }
		}
	      }
	    }
  	  }
	  position -= vPos;
	  Float_t deta = (position.pseudoRapidity() - pointPosition.pseudoRapidity());
	  Float_t dphi = (position.phi() - pointPosition.phi());
	  while (dphi >= TMath::Pi()) dphi -= TMath::TwoPi();
	  while (dphi < -TMath::Pi()) dphi += TMath::TwoPi();
	  Float_t dist2 = (deta * deta) + (dphi * dphi);
	  if (dist2 < closestDist2) {
	    closestDist22 = closestDist2;
	    closestDeta2 = closestDeta;
	    closestDphi2 = closestDphi;
	    closestDist2 = dist2;
	    closestDeta = deta;
	    closestDphi = dphi;
	  } else if (dist2 < closestDist22) {
	    closestDist22 = dist2;
	    closestDeta2 = deta;
	    closestDphi2 = dphi;
	  }
	}
      }
    }

    if (hasTrack && ((point->nTracks() & 1) != 1)) {
      point->addTrack((StTrack*)1);
    }
    point->setSizeAtDetector(kBarrelSmdEtaStripId, closestDeta);
    point->setSizeAtDetector(kBarrelSmdPhiStripId, closestDphi);
    point->setDeltaEta(closestDeta2);
    point->setDeltaPhi(closestDphi2);
  }

    if (mc_event) {
        StPtrVecMcTrack &tracks = mc_event->tracks();
        for (Int_t i = 0;i < (Int_t)tracks.size();i++) {
    	    const StMcTrack *mcTrack = tracks[i];
            if (mcTrack && (mcTrack->geantId() == 1) && (!tracks[i]->isShower())) {
		const StMcVertex *startVert = mcTrack->startVertex();
                StThreeVectorF mom = mcTrack->momentum();
		StEmcPoint *closestPoint = 0;
		Float_t closestDist2 = 1000000;
		Float_t closestDeta = -1;
		Float_t closestDphi = -1;
		for(StSPtrVecEmcPointIterator it = emcPoints.begin();it != emcPoints.end();it++) {
		    StEmcPoint *point = *it;
		    if (point) {
			StThreeVectorF pointPosition = point->position() - startVert->position();
			Float_t deta = (mom.pseudoRapidity() - pointPosition.pseudoRapidity());
			Float_t dphi = (mom.phi() - pointPosition.phi());
			while(dphi >= TMath::Pi()) dphi -= TMath::TwoPi();
			while(dphi < -TMath::Pi()) dphi += TMath::TwoPi();
			Float_t dist2 = (deta * deta) + (dphi * dphi);
			if (dist2 < ((point->sizeAtDetector(kBarrelEmcTowerId) * point->sizeAtDetector(kBarrelEmcTowerId)) + (point->sizeAtDetector(kBarrelEmcPreShowerId) * point->sizeAtDetector(kBarrelEmcPreShowerId)))) {
			    point->setSizeAtDetector(kBarrelEmcTowerId, deta);
			    point->setSizeAtDetector(kBarrelEmcPreShowerId, dphi);
			}
			if (dist2 < closestDist2) {
			    closestDist2 = dist2;
			    closestPoint = point;
			    closestDeta = deta;
			    closestDphi = dphi;
			}
		    }
		}
		if (closestPoint) {
		    if ((closestPoint->nTracks() & 2) != 2) {
			closestPoint->addTrack((StTrack*)2);
			closestPoint->addTrack((StTrack*)2);
		    }
		    closestPoint->setSizeAtDetector(kBarrelEmcTowerId, closestDeta);
		    closestPoint->setSizeAtDetector(kBarrelEmcPreShowerId, closestDphi);
		}
	    }
	}
    }
}
