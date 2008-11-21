#include "StPi0AnalysisUtil.h"
#include "TBinStatistics.h"
#include "TCutParameters.h"
#include "TWeightCalculator.h"

#include <fstream>
using namespace std;

#include <TVector3.h>
#include <TRandom.h>
#include <TGraph.h>
#include <TDatime.h>
#include <TMath.h>

#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>
#include <StEmcPool/StPi0Common/Logger.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

#include "./badRuns_normal.C"
#include "./badRuns_strict.C"
#include "./badRuns_verystrict.C"

#include "./badRuns_beambg.C"

#include "./goodFTPCruns.C"

#include "./badRuns_bunchCrossingId7bit.C"

#include "./pp2005_goldenruns_a.C"

Int_t badRunsList[] = {-1};
//Int_t badRunsList[] = {4063046, 4063047, 4080080, 4080085, 4080086, 4080087, 4080089, -1}; // test: dAuMinBias early runs only
Bool_t isBadRun_default(Int_t runId, Int_t year, Int_t day, Int_t runday) {
	Bool_t result = false;
	if (year == 4) {
	  if ((day <= 36) || (day == 49) || (day == 59)) result = true;
	  if ((day == 65) && (runday <= 16)) result = true;
	  if ((day == 54) || (day == 55) || (day == 56)) result = true;
	  if ((day == 78) && (runday > 40)) result = true;
	  if ((day == 79) && (runday < 20)) result = true;
	  if (day < 41) result = true; // strange BSMDP pedestals problem before that day
	} else if (year == 6) {
	    //if (day < 111) result = true; // beginning of 2005 p+p
	    if (day > 164) result = true; // East side status changes so much (from M. Russcher)
	    if ((runId >= 6131062) && (runId <=6134011)) result = true; // SMD status is bad these days (from M. Russcher)
	    if (runId == 6146044) result = true; // tower 2076 has stuck bit 2048, comment out when fixed in status table
	    if (((runId >= 6128043) && (runId <= 6128054)) || ((runId >= 6141046) && (runId <= 6141059))) result = true; // tower 3287 has stuck bit 256, comment out when fixed in status table
	}
	Int_t badRunsListIndex = 0;
	while ((!result) && (badRunsList[badRunsListIndex] != -1)) result = (badRunsList[badRunsListIndex++] == runId);
	return result;
}

Bool_t isBadRun_embeddingonly(Int_t runId, Int_t year, Int_t day, Int_t runday) {
	Bool_t result = true;
	if ((runId == 4049021) || (runId == 4036043)) result = false;
	return result;
}

Bool_t isBadRun_normal(Int_t runId, Int_t year, Int_t day, Int_t runday) {
	Bool_t result = false;
	Int_t badRunsListIndex = 0;
	badRunsListIndex = 0; while ((!result) && (badRuns_normal_MB[badRunsListIndex] != -1)) result = (badRuns_normal_MB[badRunsListIndex++] == runId);
	badRunsListIndex = 0; while ((!result) && (badRuns_normal_HT1[badRunsListIndex] != -1)) result = (badRuns_normal_HT1[badRunsListIndex++] == runId);
	badRunsListIndex = 0; while ((!result) && (badRuns_normal_HT2[badRunsListIndex] != -1)) result = (badRuns_normal_HT2[badRunsListIndex++] == runId);
	return result;
}

Bool_t isBadRun_strict(Int_t runId, Int_t year, Int_t day, Int_t runday) {
	Bool_t result = false;
	Int_t badRunsListIndex = 0;
	badRunsListIndex = 0; while ((!result) && (badRuns_strict_MB[badRunsListIndex] != -1)) result = (badRuns_strict_MB[badRunsListIndex++] == runId);
	badRunsListIndex = 0; while ((!result) && (badRuns_strict_HT1[badRunsListIndex] != -1)) result = (badRuns_strict_HT1[badRunsListIndex++] == runId);
	badRunsListIndex = 0; while ((!result) && (badRuns_strict_HT2[badRunsListIndex] != -1)) result = (badRuns_strict_HT2[badRunsListIndex++] == runId);
	return result;
}

Bool_t isBadRun_verystrict(Int_t runId, Int_t year, Int_t day, Int_t runday) {
	Bool_t result = false;
	Int_t badRunsListIndex = 0;
	badRunsListIndex = 0; while ((!result) && (badRuns_verystrict_MB[badRunsListIndex] != -1)) result = (badRuns_verystrict_MB[badRunsListIndex++] == runId);
	badRunsListIndex = 0; while ((!result) && (badRuns_verystrict_HT1[badRunsListIndex] != -1)) result = (badRuns_verystrict_HT1[badRunsListIndex++] == runId);
	badRunsListIndex = 0; while ((!result) && (badRuns_verystrict_HT2[badRunsListIndex] != -1)) result = (badRuns_verystrict_HT2[badRunsListIndex++] == runId);
	return result;
}

Bool_t isBadRun_beambg(Int_t runId, Int_t year, Int_t day, Int_t runday) {
	Bool_t result = false;
	Int_t badRunsListIndex = 0;
	badRunsListIndex = 0; while ((!result) && (badRuns_beambg_MB[badRunsListIndex] != -1)) result = (badRuns_beambg_MB[badRunsListIndex++] == runId);
	badRunsListIndex = 0; while ((!result) && (badRuns_beambg_HT1[badRunsListIndex] != -1)) result = (badRuns_beambg_HT1[badRunsListIndex++] == runId);
	badRunsListIndex = 0; while ((!result) && (badRuns_beambg_HT2[badRunsListIndex] != -1)) result = (badRuns_beambg_HT2[badRunsListIndex++] == runId);
	return result;
}

Bool_t isGoodFtpcRun(Int_t runId, Int_t year, Int_t day, Int_t runday) {
	Bool_t result = false;
	Int_t goodRunsListIndex = 0;
	goodRunsListIndex = 0; while ((!result) && (goodFTPCruns[goodRunsListIndex] != -1)) result = (goodFTPCruns[goodRunsListIndex++] == runId);
	return result;
}

Bool_t isGoodPP2005Run(Int_t runId, Int_t year, Int_t day, Int_t runday) {
	Bool_t result = false;
	Int_t goodRunsListIndex = 0;
	goodRunsListIndex = 0; while ((!result) && (goodPP2005runs[goodRunsListIndex] != -1)) result = (goodPP2005runs[goodRunsListIndex++] == runId);
	return result;
}

Bool_t isGoodPP2005Run_ppProductionMinBias(Int_t runId, Int_t year, Int_t day, Int_t runday) {
	Bool_t result = false;
	Int_t goodRunsListIndex = 0;
	goodRunsListIndex = 0; while ((!result) && (goodPP2005runs_ppProductionMinBias[goodRunsListIndex] != -1)) result = (goodPP2005runs_ppProductionMinBias[goodRunsListIndex++] == runId);
	return result;
}

Bool_t isBadPP2005Run(Int_t runId, Int_t year, Int_t day, Int_t runday) {
	Bool_t result = false;
	Int_t badRunsListIndex = 0;
	badRunsListIndex = 0; while ((!result) && (badPP2005runs[badRunsListIndex] != -1)) result = (badPP2005runs[badRunsListIndex++] == runId);
	return result;
}

Bool_t isBadRun_bunchCrossingId7bit(Int_t runId, Int_t year, Int_t day, Int_t runday) {
	Bool_t result = false;
	Int_t badRunsListIndex = 0;
	badRunsListIndex = 0; while ((!result) && (badRuns_highBgEmptyBunch[badRunsListIndex] != -1)) result = (badRuns_highBgEmptyBunch[badRunsListIndex++] == runId);
	return result;
}

Bool_t isBadRun(Int_t parameter, Int_t runId, Int_t year, Int_t day, Int_t runday) {
	Bool_t resultBad = false;
	Bool_t resultGood = true;
	if (parameter > 0) {
		switch ((parameter / 1)    % 10) {
			case 1: resultBad |= isBadRun_default(runId, year, day, runday); break;
		}
		switch ((parameter / 10)   % 10) {
			case 1: resultBad |= isBadRun_normal(runId, year, day, runday); break;
			case 2: resultBad |= isBadRun_strict(runId, year, day, runday); break;
			case 3: resultBad |= isBadRun_verystrict(runId, year, day, runday); break;
		}
		switch ((parameter / 100)  % 10) {
			case 1: resultBad |= isBadRun_embeddingonly(runId, year, day, runday); break;
		}
		switch ((parameter / 1000) % 10) {
			case 1: resultBad |= isBadRun_bunchCrossingId7bit(runId, year, day, runday); break;
		}
		switch ((parameter / 10000) % 10) {
			case 1: resultBad |= isBadRun_beambg(runId, year, day, runday); break;
		}
		switch ((parameter / 100000) % 10) {
			case 1: if (year == 4) resultGood &= isGoodFtpcRun(runId, year, day, runday); break;
		}
		switch ((parameter / 1000000) % 10) {
			case 1: if (year == 6) resultGood &= isGoodPP2005Run(runId, year, day, runday); break;
			case 2: if (year == 6) resultGood &= isGoodPP2005Run(runId, year, day, runday) || isGoodPP2005Run_ppProductionMinBias(runId, year, day, runday); break;
		}
		switch ((parameter / 10000000) % 10) {
			case 1: resultBad |= isBadPP2005Run(runId, year, day, runday); break;
		}
	}
	return resultBad || (!resultGood);
}

event_list_type *badEventsList = 0;
Bool_t isBadEvent(Int_t runId, Int_t eventId, const Char_t *badEventsListFilename) {
    if (!badEventsList) {
	badEventsList = new event_list_type();
	readEventListFromFile(findFile(badEventsListFilename), badEventsList);
    }
    return badEventsList ? (find(badEventsList->begin(), badEventsList->end(), pair<Int_t, Int_t>(runId, eventId)) != badEventsList->end()) : false;
}

void readEventListFromFile(const Char_t *filename, event_list_type *eventList) {
    if (filename && *filename && eventList) {
	eventList->clear();
	cout << "Reading event list from file " << filename << "..."; cout.flush();
	ifstream ifstr(filename);
	while (ifstr.good()) {
	    pair<Int_t, Int_t> rec;
	    ifstr >> rec.first >> rec.second;
	    if (ifstr.good()) {
		eventList->push_back(rec);
	    }
	}
	cout << " done: " << eventList->size() << " entries" << endl;
    }
}

void writeEventListToFile(const Char_t *filename, const event_list_type *eventList) {
    if (filename && *filename && eventList) {
	ofstream ofstr(filename);
	if (ofstr.good()) {
	    cout << "Writing event list to file " << filename << ": " << eventList->size() << " entries" << endl;
	    for(event_list_type::const_iterator it = eventList->begin();it != eventList->end();++it) {
		ofstr << (*it).first << "\t" << (*it).second << endl;
	    }
	}
    }
}

void parseRunId(Int_t runId, Int_t &year, Int_t &day, Int_t &runDay) {
	year =   (runId / 1000000) % 1000;
	day =    (runId / 1000)    % 1000;
	runDay = (runId / 1)       % 1000;
}

void getCandidateParams(const TMyCandidateTreeData &candidate
    , const TMyEventData &event1, const TEventParameters &event1Parameters
    , const TMyPointData &point1, const TPointParameters &point1Parameters
    , const TMyEventData &event2, const TEventParameters &event2Parameters
    , const TMyPointData &point2, const TPointParameters &point2Parameters
    , const TCutParameters &cutParameters, TCandidateParameters &candidateParameters) {

	Float_t z1 = event1Parameters.zUse;
	Float_t z2 = event2Parameters.zUse;
	Float_t deltaPhi1 = event1.jet.phi;
	Float_t deltaEta1 = event1.jet.eta;
	Float_t deltaPhi2 = event2.jet.phi;
	Float_t deltaEta2 = event2.jet.eta;
	if (cutParameters.jetRotate == 2) {
	    //deltaPhi2 = -deltaPhi2;
	    //deltaPhi2 += TMath::Pi();
	    //deltaEta2 = -deltaEta2;
	    deltaEta2 = event2Parameters.jetBackEta;
	    deltaPhi2 = event2Parameters.jetBackPhi;
	}
	deltaPhi2 -= deltaPhi1;
	deltaPhi1 -= deltaPhi1;
	deltaEta2 -= deltaEta1;
	deltaEta1 -= deltaEta1;
	TVector3 v1;
	v1.SetPtEtaPhi(EMCRadius, candidate.point1.point.etaCoord, candidate.point1.point.phiCoord);
	v1.SetZ(v1.Z() - z1);
	if (cutParameters.jetRotate != 0) {
	    v1.SetPtEtaPhi(v1.Perp(), v1.Eta() - deltaEta1, v1.Phi() - deltaPhi1);
	}
	v1.SetMag(point1Parameters.energy);
	TVector3 v2;
	v2.SetPtEtaPhi(EMCRadius, candidate.point2.point.etaCoord, candidate.point2.point.phiCoord);
	v2.SetZ(v2.Z() - z2);
	if (cutParameters.jetRotate != 0) {
	    v2.SetPtEtaPhi(v2.Perp(), v2.Eta() - deltaEta2, v2.Phi() - deltaPhi2);
	}
	v2.SetMag(point2Parameters.energy);
	TVector3 pRec = v1 + v2;
	candidateParameters.openangle = v1.Angle(v2);
	candidateParameters.m = sqrt(2 * point1Parameters.energy * point2Parameters.energy * (1 - cos(candidateParameters.openangle)));
	candidateParameters.pTRec = pRec.Perp();
	candidateParameters.eta = pRec.Eta();
	candidateParameters.phi = pRec.Phi();
	candidateParameters.asymetry = TMath::Abs(point1Parameters.energy - point2Parameters.energy) / (point1Parameters.energy + point2Parameters.energy);
	candidateParameters.egamma = (point1Parameters.energy > point2Parameters.energy) ? point1Parameters.energy : point2Parameters.energy;
	candidateParameters.energy = point1Parameters.energy + point2Parameters.energy;
	TVector3 pRecCoord;
	pRecCoord.SetPtEtaPhi(EMCRadius, candidateParameters.eta, candidateParameters.phi);
	pRecCoord.SetZ(pRecCoord.Z() + ((z1 + z2) / 2.0));
	candidateParameters.etaCoord = pRecCoord.Eta();
	candidateParameters.phiCoord = pRecCoord.Phi();
	candidateParameters.towerId = (point1Parameters.energy > point2Parameters.energy) ? candidate.point1.point.clusterBTOW.highestEnergyHit.id : candidate.point2.point.clusterBTOW.highestEnergyHit.id;
	candidateParameters.pointEtaCoord = (point1Parameters.energy > point2Parameters.energy) ? candidate.point1.point.etaCoord : candidate.point2.point.etaCoord;
	candidateParameters.jetDeltaEta = candidateParameters.eta - event1.jet.eta;
	candidateParameters.jetDeltaPhi = candidateParameters.phi - event1.jet.phi;
	while (candidateParameters.jetDeltaPhi < -TMath::Pi()) candidateParameters.jetDeltaPhi += TMath::TwoPi();
	while (candidateParameters.jetDeltaPhi > +TMath::Pi()) candidateParameters.jetDeltaPhi -= TMath::TwoPi();
	candidateParameters.jetDist = sqrt((candidateParameters.jetDeltaEta * candidateParameters.jetDeltaEta) + (candidateParameters.jetDeltaPhi * candidateParameters.jetDeltaPhi));
	candidateParameters.jetBackDeltaEta = candidateParameters.eta - event1Parameters.jetBackEta;
	candidateParameters.jetBackDeltaPhi = candidateParameters.phi - event1Parameters.jetBackPhi;
	while (candidateParameters.jetBackDeltaPhi < -TMath::Pi()) candidateParameters.jetBackDeltaPhi += TMath::TwoPi();
	while (candidateParameters.jetBackDeltaPhi > +TMath::Pi()) candidateParameters.jetBackDeltaPhi -= TMath::TwoPi();
	candidateParameters.jetBackDist = sqrt((candidateParameters.jetBackDeltaEta * candidateParameters.jetBackDeltaEta) + (candidateParameters.jetBackDeltaPhi * candidateParameters.jetBackDeltaPhi));
        candidateParameters.massRegionLeft = cutParameters.massRegionLeft + (candidateParameters.pTRec * cutParameters.massRegionLeftPt);
        candidateParameters.massRegionRight = cutParameters.massRegionRight + (candidateParameters.pTRec * cutParameters.massRegionRightPt);
	candidateParameters.pTRecoToSimu = (candidate.point1.event.simulatedParticle.pT != 0) ? (candidateParameters.pTRec / candidate.point1.event.simulatedParticle.pT) : 0;
	candidateParameters.distTrackClosest = TMath::Min(point1Parameters.distTrack, point2Parameters.distTrack);
	candidateParameters.distTrackClosest2 = TMath::Min(point1Parameters.distTrack2, point2Parameters.distTrack2);
}

void getHitParams(const TMyEventData &event, const TMyHitData &hit, const TEventParameters &eventParameters, const TCutParameters &cutParameters, THitParameters &hitParameters) {
	hitParameters.etaCoord = (Float_t(Int_t(hit.etaphiCoord)) / 100000.0) - 1.0;
	hitParameters.phiCoord = ((hit.etaphiCoord - Float_t(Int_t(hit.etaphiCoord))) * 7.0) - 3.5;
	Float_t z = eventParameters.zUse;
	TVector3 v;
	v.SetPtEtaPhi(EMCRadius, hitParameters.etaCoord, hitParameters.phiCoord);
	v.SetZ(v.Z() - z);
	v.SetMag(hit.energy / (1.0 + cutParameters.calibrationSlope));
	hitParameters.eT = v.Perp();
	hitParameters.eta = v.Eta();
	hitParameters.phi = v.Phi();
	hitParameters.ped = hit.pedestal100 / 100.0;
}

void getPointParams(const TMyEventData &event, const TMyPointData &point, const TEventParameters &eventParameters, const TCutParameters &cutParameters, TPointParameters &pointParameters) {
	pointParameters.triggeredHT1 = (point.trigger.triggered & cutParameters.triggersHT1);
	pointParameters.triggeredHT2 = (point.trigger.triggered & cutParameters.triggersHT2);
	Float_t z = eventParameters.zUse;
	TVector3 v;
	v.SetPtEtaPhi(EMCRadius, point.etaCoord, point.phiCoord);
	v.SetZ(v.Z() - z);
	v.SetMag(point.energy / (1.0 + cutParameters.calibrationSlope));
	pointParameters.pTRec = v.Perp();
	pointParameters.eta = v.Eta();
	pointParameters.phi = v.Phi();
	pointParameters.energy = point.energy / (1.0 + cutParameters.calibrationSlope);
	pointParameters.etaCoord = point.etaCoord;
	pointParameters.phiCoord = point.phiCoord;
	pointParameters.towerId = point.clusterBTOW.highestEnergyHit.id;
	pointParameters.jetDeltaEta = pointParameters.eta - event.jet.eta;
	pointParameters.jetDeltaPhi = pointParameters.phi - event.jet.phi;
	while (pointParameters.jetDeltaPhi < -TMath::Pi()) pointParameters.jetDeltaPhi += TMath::TwoPi();
	while (pointParameters.jetDeltaPhi > +TMath::Pi()) pointParameters.jetDeltaPhi -= TMath::TwoPi();
	pointParameters.distJet = sqrt((pointParameters.jetDeltaEta * pointParameters.jetDeltaEta) + (pointParameters.jetDeltaPhi * pointParameters.jetDeltaPhi));
	pointParameters.jetBackDeltaEta = pointParameters.eta - eventParameters.jetBackEta;
	pointParameters.jetBackDeltaPhi = pointParameters.phi - eventParameters.jetBackPhi;
	while (pointParameters.jetBackDeltaPhi < -TMath::Pi()) pointParameters.jetBackDeltaPhi += TMath::TwoPi();
	while (pointParameters.jetBackDeltaPhi > +TMath::Pi()) pointParameters.jetBackDeltaPhi -= TMath::TwoPi();
	pointParameters.distJetBack = sqrt((pointParameters.jetBackDeltaEta * pointParameters.jetBackDeltaEta) + (pointParameters.jetBackDeltaPhi * pointParameters.jetBackDeltaPhi));
        pointParameters.distTrack = sqrt((point.trackDeta * point.trackDeta) + (point.trackDphi * point.trackDphi));
        pointParameters.distTrack2 = sqrt((point.trackDeta2 * point.trackDeta2) + (point.trackDphi2 * point.trackDphi2));
        pointParameters.distGamma = sqrt((point.trackMCDeta * point.trackMCDeta) + (point.trackMCDphi * point.trackMCDphi));
	pointParameters.smdAsymetry = ((point.clusterBSMDE.energy + point.clusterBSMDP.energy) != 0) ? (TMath::Abs(point.clusterBSMDE.energy - point.clusterBSMDP.energy) / (point.clusterBSMDE.energy + point.clusterBSMDP.energy)) : 0.0;
	pointParameters.towerCenterDist = sqrt((point.towerCenterDeta * point.towerCenterDeta) + (point.towerCenterDphi * point.towerCenterDphi));
	pointParameters.triggeredHT1Et = (pointParameters.pTRec >= cutParameters.HT1EtThreshold);
	pointParameters.triggeredHT2Et = (pointParameters.pTRec >= cutParameters.HT2EtThreshold);
	if (point.clusterBTOW.isValid() && point.clusterBTOW.highestEnergyHit.isValid()) getHitParams(event, point.clusterBTOW.highestEnergyHit, eventParameters, cutParameters, pointParameters.highestEnergyHitBTOW);
	pointParameters.passedCPV = (pointParameters.distTrack >= cutParameters.trackDistCutLow) && ((pointParameters.distTrack < cutParameters.trackDistCutHigh) || (cutParameters.trackDistCutHigh < 0));
	pointParameters.triggeredTowerHT1Et = (pointParameters.highestEnergyHitBTOW.eT >= cutParameters.HT1EtThreshold);
	pointParameters.triggeredTowerHT2Et = (pointParameters.highestEnergyHitBTOW.eT >= cutParameters.HT2EtThreshold);
}

void getPionParams(const TMyEventData &event, const TMySimulatedDecayData &pion, const TEventParameters &eventParameters, const TGammaParameters &gamma1Parameters, const TGammaParameters &gamma2Parameters, const TCutParameters &cutParameters, TPionParameters &pionParameters) {
	Float_t z = event.simulatedParticle.z;
	if (z == 0.0) z = eventParameters.zUse;
	pionParameters.pT = pion.parent.summary.pT;
	pionParameters.pTgammas = gamma1Parameters.pT + gamma2Parameters.pT;
	pionParameters.eta = -log(tan(pion.parent.theta * 0.5));
	pionParameters.phi = pion.parent.phi;
	TVector3 vp;
	vp.SetMagThetaPhi(EMCRadius / sin(pion.parent.theta), pion.parent.theta, pion.parent.phi);
	vp.SetZ(vp.Z() + z);
	pionParameters.etaCoord = vp.Eta();//pion.etaCoord;
	pionParameters.phiCoord = vp.Phi();//pion.phiCoord;
	TVector3 vg1;
	vg1.SetMagThetaPhi(EMCRadius / sin(pion.daughter1.theta), pion.daughter1.theta, pion.daughter1.phi);
	TVector3 vg2;
	vg2.SetMagThetaPhi(EMCRadius / sin(pion.daughter2.theta), pion.daughter2.theta, pion.daughter2.phi);
	pionParameters.openangle = vg1.Angle(vg2);
	pionParameters.asymetry = TMath::Abs(gamma1Parameters.energy - gamma2Parameters.energy) / (gamma1Parameters.energy + gamma2Parameters.energy);
	pionParameters.egamma = (gamma1Parameters.energy > gamma2Parameters.energy) ? gamma1Parameters.energy : gamma2Parameters.energy;
	pionParameters.energy = gamma1Parameters.energy + gamma2Parameters.energy;
	TVector3 v1reco;
	v1reco.SetPtEtaPhi(EMCRadius, pion.daughter1.associatedPoint.etaCoord, pion.daughter1.associatedPoint.phiCoord);
	v1reco.SetZ(v1reco.Z() - z);
	v1reco.SetMag(pion.daughter1.associatedPoint.energy);
	TVector3 v2reco;
	v2reco.SetPtEtaPhi(EMCRadius, pion.daughter2.associatedPoint.etaCoord, pion.daughter2.associatedPoint.phiCoord);
	v2reco.SetZ(v2reco.Z() - z);
	v2reco.SetMag(pion.daughter2.associatedPoint.energy);
	pionParameters.openangleReco = v1reco.Angle(v2reco);
	pionParameters.m = sqrt(2 * pion.daughter1.associatedPoint.energy * pion.daughter2.associatedPoint.energy * (1 - cos(pionParameters.openangleReco)));
	pionParameters.towerId = (pion.daughter1.associatedPoint.energy > pion.daughter2.associatedPoint.energy) ? pion.daughter1.associatedPoint.clusterBTOW.highestEnergyHit.id : pion.daughter2.associatedPoint.clusterBTOW.highestEnergyHit.id;
	pionParameters.pointEtaCoord = (pion.daughter1.associatedPoint.energy > pion.daughter2.associatedPoint.energy) ? pion.daughter1.associatedPoint.etaCoord : pion.daughter2.associatedPoint.etaCoord;
	pionParameters.jetDeltaEta = pionParameters.eta - event.jet.eta;
	pionParameters.jetDeltaPhi = pionParameters.phi - event.jet.phi;
	while (pionParameters.jetDeltaPhi < -TMath::Pi()) pionParameters.jetDeltaPhi += TMath::TwoPi();
	while (pionParameters.jetDeltaPhi > +TMath::Pi()) pionParameters.jetDeltaPhi -= TMath::TwoPi();
	pionParameters.jetDist = sqrt((pionParameters.jetDeltaEta * pionParameters.jetDeltaEta) + (pionParameters.jetDeltaPhi * pionParameters.jetDeltaPhi));
	pionParameters.jetBackDeltaEta = pionParameters.eta - eventParameters.jetBackEta;
	pionParameters.jetBackDeltaPhi = pionParameters.phi - eventParameters.jetBackPhi;
	while (pionParameters.jetBackDeltaPhi < -TMath::Pi()) pionParameters.jetBackDeltaPhi += TMath::TwoPi();
	while (pionParameters.jetBackDeltaPhi > +TMath::Pi()) pionParameters.jetBackDeltaPhi -= TMath::TwoPi();
	pionParameters.jetBackDist = sqrt((pionParameters.jetBackDeltaEta * pionParameters.jetBackDeltaEta) + (pionParameters.jetBackDeltaPhi * pionParameters.jetBackDeltaPhi));
}

void getGammaParams(const TMyEventData &event, const TMySimulatedParticleData &gamma, const TEventParameters &eventParameters, const TCutParameters &cutParameters, TGammaParameters &gammaParameters) {
	Float_t z = event.simulatedParticle.z;
	if (z == 0.0) z = eventParameters.zUse;
	gammaParameters.pT = gamma.summary.pT;
	gammaParameters.phi = gamma.phi;
	gammaParameters.phiCoord = gamma.phi;
	if (gamma.theta != 0) {
	    gammaParameters.eta = -log(tan(gamma.theta * 0.5));
	    TVector3 vg;
	    vg.SetMagThetaPhi(EMCRadius / sin(gamma.theta), gamma.theta, gamma.phi);
	    vg.SetZ(vg.Z() + z);
	    gammaParameters.etaCoord = vg.Eta();
	    gammaParameters.energy = gamma.summary.pT / sin(gamma.theta);
	} else {
	    gammaParameters.eta = 10e10;
	    gammaParameters.etaCoord = 10e10;
	    gammaParameters.energy = 0;
	}
	gammaParameters.towerId = gamma.associatedPoint.clusterBTOW.highestEnergyHit.id;
	gammaParameters.pointEtaCoord = gamma.associatedPoint.etaCoord;
	gammaParameters.associatedPointDeta = gammaParameters.etaCoord - gamma.associatedPoint.etaCoord;
	gammaParameters.associatedPointDphi = gammaParameters.phiCoord - gamma.associatedPoint.phiCoord;
	while (gammaParameters.associatedPointDphi < -TMath::Pi()) gammaParameters.associatedPointDphi += TMath::TwoPi();
	while (gammaParameters.associatedPointDphi > +TMath::Pi()) gammaParameters.associatedPointDphi -= TMath::TwoPi();
        gammaParameters.distAssociated = sqrt((gammaParameters.associatedPointDeta * gammaParameters.associatedPointDeta) + (gammaParameters.associatedPointDphi * gammaParameters.associatedPointDphi));
	gammaParameters.jetDeltaEta = gammaParameters.eta - event.jet.eta;
	gammaParameters.jetDeltaPhi = gammaParameters.phi - event.jet.phi;
	while (gammaParameters.jetDeltaPhi < -TMath::Pi()) gammaParameters.jetDeltaPhi += TMath::TwoPi();
	while (gammaParameters.jetDeltaPhi > +TMath::Pi()) gammaParameters.jetDeltaPhi -= TMath::TwoPi();
	gammaParameters.jetDist = sqrt((gammaParameters.jetDeltaEta * gammaParameters.jetDeltaEta) + (gammaParameters.jetDeltaPhi * gammaParameters.jetDeltaPhi));
	gammaParameters.jetBackDeltaEta = gammaParameters.eta - eventParameters.jetBackEta;
	gammaParameters.jetBackDeltaPhi = gammaParameters.phi - eventParameters.jetBackPhi;
	while (gammaParameters.jetBackDeltaPhi < -TMath::Pi()) gammaParameters.jetBackDeltaPhi += TMath::TwoPi();
	while (gammaParameters.jetBackDeltaPhi > +TMath::Pi()) gammaParameters.jetBackDeltaPhi -= TMath::TwoPi();
	gammaParameters.jetBackDist = sqrt((gammaParameters.jetBackDeltaEta * gammaParameters.jetBackDeltaEta) + (gammaParameters.jetBackDeltaPhi * gammaParameters.jetBackDeltaPhi));
	if (gamma.associatedPoint.isValid()) getPointParams(event, gamma.associatedPoint, eventParameters, cutParameters, gammaParameters.associatedPoint);
}

Float_t getMinimumOpenangle(Float_t m, Float_t energy) {return 2.0 * asin(m / energy);}

Int_t digitLow = 0;
Int_t digitHigh = 120;
Int_t digitDiff = digitHigh - digitLow;
Int_t *bunchPattern100 = 0;
Int_t *bunchPattern52 = 0;
Bool_t isGoodBunchCrossingId7bitPlusOffset(Int_t runId, Int_t year, Int_t day, Int_t runDay, Int_t bunchCrossingId7bitPlusOffset, Bool_t checkAbortGaps, Bool_t checkEmptyBuckets) {
	if (!bunchPattern100) {
		bunchPattern100 = new Int_t[digitDiff];
		for (Int_t i = 0;i < digitDiff;i++) if (bunchPattern100) bunchPattern100[i] = 1;
		for (Int_t i = 30;i <= 39;i++) if (bunchPattern100) bunchPattern100[i] = -1;
		for (Int_t i = 110;i <= 119;i++) if (bunchPattern100) bunchPattern100[i] = -1;
	}
	if (!bunchPattern52) {
		bunchPattern52 = new Int_t[digitDiff];
		for (Int_t i = 0;i < digitDiff;i++) if (bunchPattern52) bunchPattern52[i] = 1;
		for (Int_t i = 31;i <= 39;i++) if (bunchPattern52) bunchPattern52[i] = -1;
		for (Int_t i = 111;i <= 119;i++) if (bunchPattern52) bunchPattern52[i] = -1;
		for (Int_t i = 1;i < digitDiff;i += 2) if (bunchPattern52 && (bunchPattern52[i] != -1)) bunchPattern52[i] = 0;
	}
	Int_t *bunchPattern = 0;
	if ((year == 4) && (day >= 33) && (day < 60)) bunchPattern = bunchPattern100;
	if ((year == 4) && (day >= 60) && (day <= 80)) bunchPattern = bunchPattern52;
	if ((year == 6)) bunchPattern = bunchPattern52;
	Bool_t result = true;
	if (bunchPattern) {
		while (bunchCrossingId7bitPlusOffset < 0) bunchCrossingId7bitPlusOffset += digitDiff;
		while (bunchCrossingId7bitPlusOffset >= digitDiff) bunchCrossingId7bitPlusOffset -= digitDiff;
		if ((bunchCrossingId7bitPlusOffset >= 0) && (bunchCrossingId7bitPlusOffset < digitDiff)) {
			Int_t bunchAvailable = bunchPattern[bunchCrossingId7bitPlusOffset];
			if (checkAbortGaps && (bunchAvailable == -1)) result = false;
			if (checkEmptyBuckets && (bunchAvailable == 0)) result = false;
		} else result = false;
	}
	return result;
}

void getEventParams(const TMyEventData &event, const TCutParameters &cutParameters, TEventParameters &eventParameters) {
        eventParameters.isMB = event.trigger.isValid() && (event.trigger.triggered & cutParameters.triggersMB);
        eventParameters.isHT1 = event.trigger.isValid() && (event.trigger.triggered & cutParameters.triggersHT1);
        eventParameters.isHT2 = event.trigger.isValid() && (event.trigger.triggered & cutParameters.triggersHT2);
        eventParameters.isHT1Simulated = event.triggerSimulatedFinal.isValid() && (event.triggerSimulatedFinal.trigger.triggered & cutParameters.triggersHT1);
        eventParameters.isHT2Simulated = event.triggerSimulatedFinal.isValid() && (event.triggerSimulatedFinal.trigger.triggered & cutParameters.triggersHT2);
	eventParameters.bunchCrossingId7bitOffset = getBunchCrossingId7bitOffset(event.runId, false, cutParameters.bunchCrossingIdOffsetsFilename);
	eventParameters.bunchCrossingId7bitPlusOffset = event.bunchCrossingId7bit + eventParameters.bunchCrossingId7bitOffset;
	while (eventParameters.bunchCrossingId7bitPlusOffset < 0) eventParameters.bunchCrossingId7bitPlusOffset += digitDiff;
	while (eventParameters.bunchCrossingId7bitPlusOffset >= digitDiff) eventParameters.bunchCrossingId7bitPlusOffset -= digitDiff;
	parseRunId(event.runId, eventParameters.year, eventParameters.day, eventParameters.runDay);
	eventParameters.uncorrectedNumberOfFtpcPrimaries = event.uncorrectedNumberOfFtpcPrimariesEast;// + event->uncorrectedNumberOfFtpcPrimariesWest;
	eventParameters.clustersPerTrack = (event.nPrimary != 0) ? (Float_t(event.nClustersBTOW) / Float_t(event.nPrimary)) : -1;
	eventParameters.TPCPtToEMCEt = (event.totalBEMCPointsEt != 0) ? (event.totalTPCPt / event.totalBEMCPointsEt) : -0.1;
	Float_t bbcDiff = event.bbcEarliestWest - event.bbcEarliestEast;
	eventParameters.zBBC = cutParameters.zBBCcoeff0 + (bbcDiff * cutParameters.zBBCcoeff1);
	eventParameters.zTPC = event.zTPC;
	eventParameters.zUse = 0.0;
	if (cutParameters.simulation) {
	    eventParameters.zUse = event.simulatedParticle.z;
	    if (cutParameters.zSimuSmearing) {
		Float_t prob = 0.0;
		if (cutParameters.zSimuSmearingPt) {
		    prob = cutParameters.zSimuSmearingPt->getWeight(event.simulatedParticle.pT);
		}
		if (prob > 1.0) prob = 1.0;
		if (prob < -1e-6) prob = -1e-6;
		Bool_t doSmearing = (gRandom->Uniform() <= prob);
		if (doSmearing) {
		    Bool_t doSmearingZero = false;
		    if (cutParameters.zSimuSmearingZero) {
			Float_t prob = 0.0;
			if (cutParameters.zSimuSmearingZeroPt) {
			    prob = cutParameters.zSimuSmearingZeroPt->getWeight(event.simulatedParticle.pT);
			}
			if (prob > 1.0) prob = 1.0;
			if (prob < -1e-6) prob = -1e-6;
			doSmearingZero = (gRandom->Uniform() <= prob);
		    }
		    if (doSmearingZero) {
			eventParameters.zUse = 0.0;
		    } else {
			Float_t mean = cutParameters.zSimuMeanCoeff0 + (event.simulatedParticle.z * cutParameters.zSimuMeanCoeff1);
			Float_t spread = cutParameters.zSimuSpreadCoeff0 + (event.simulatedParticle.z * cutParameters.zSimuSpreadCoeff1);
			Float_t newVertex = gRandom->Gaus(mean, spread);
			eventParameters.zUse = newVertex;
		    }
		}
	    }
	} else {
	    if ((eventParameters.zUse == 0.0) && cutParameters.useZTPC) eventParameters.zUse = eventParameters.zTPC;
	    if ((eventParameters.zUse == 0.0) && cutParameters.useZBBC) eventParameters.zUse = eventParameters.zBBC;
	}
	if (event.triggerSimulatedFinal.isValid() && event.triggerSimulatedFinal.highestEtHit.isValid()) getHitParams(event, event.triggerSimulatedFinal.highestEtHit, eventParameters, cutParameters, eventParameters.highestEtHit);
        eventParameters.isHT1SimulatedEt = (eventParameters.highestEtHit.eT >= cutParameters.HT1EtThreshold);
        eventParameters.isHT2SimulatedEt = (eventParameters.highestEtHit.eT >= cutParameters.HT2EtThreshold);
	eventParameters.jetBackEta = -event.jet.eta;
	eventParameters.jetBackPhi = event.jet.phi + TMath::Pi();
	while (eventParameters.jetBackPhi < -TMath::Pi()) eventParameters.jetBackPhi += TMath::TwoPi();
	while (eventParameters.jetBackPhi > +TMath::Pi()) eventParameters.jetBackPhi -= TMath::TwoPi();
}

TObject *swap(TObject* oldObject, const TObject *newObject) {
	//cout << "swap " << oldObject << ", " << newObject << " started" << endl;
	TObject *newObj = newObject ? newObject->Clone() : 0;
	//cout << "swap: clone created " << newObj << endl;
	if (oldObject) delete oldObject;
	//cout << "swap finished: " << newObj << endl;
	return newObj;
}

TH1 *addHistogram(TH1 *oldHistogram, const TH1 *newHistogram) {
	if (!newHistogram) return oldHistogram;
	if (!oldHistogram) {
		return dynamic_cast<TH1*>(swap(oldHistogram, newHistogram));
	} else {
		TAxis *xax_old = oldHistogram->GetXaxis();
		//TAxis *yax_old = oldHistogram->GetYaxis();
		//TAxis *zax_old = oldHistogram->GetZaxis();
		TAxis *xax_new = newHistogram->GetXaxis();
		//TAxis *yax_new = newHistogram->GetYaxis();
		//TAxis *zax_new = newHistogram->GetZaxis();
		if (xax_old && xax_new) {
		    if (xax_old->GetNbins() != xax_new->GetNbins()) {
			Double_t xmin = min(xax_old->GetBinLowEdge(1), xax_new->GetBinLowEdge(1));
			Double_t xmax = max(xax_old->GetBinUpEdge(xax_old->GetNbins()), xax_new->GetBinUpEdge(xax_new->GetNbins()));
			Int_t nbins = max(xax_old->GetNbins(), xax_new->GetNbins());
			xax_old->Set(nbins, xmin, xmax);
			xax_new->Set(nbins, xmin, xmax);
		    }
		}
		oldHistogram->Add(newHistogram);
		return oldHistogram;
	}
}

Int_t *bunchCrossingRuns = 0;
Int_t *bunchCrossingOffsets = 0;
Int_t bunchCrossingNum = 0;
Int_t getBunchCrossingId7bitOffset(Int_t runId, Bool_t print, const Char_t *bunchCrossingId7bitFilename) {
        Int_t offset = 0;
        if (!bunchCrossingRuns || !bunchCrossingOffsets) {
                if (bunchCrossingRuns) delete [] bunchCrossingRuns;
                if (bunchCrossingOffsets) delete [] bunchCrossingOffsets;
                bunchCrossingNum = 0;
                const Int_t buffsize = 4096;
                bunchCrossingRuns = new Int_t[buffsize];
                bunchCrossingOffsets = new Int_t[buffsize];
                if (bunchCrossingRuns) for (Int_t i = 0;i < buffsize;bunchCrossingRuns[i++] = 0);
                if (bunchCrossingOffsets) for (Int_t i = 0;i < buffsize;bunchCrossingOffsets[i++] = 0);
                if (print) cout << "Reading input file " << bunchCrossingId7bitFilename << endl;
                TString bxFileExact = findFile(bunchCrossingId7bitFilename);
                ifstream ipsStr(bxFileExact);
                if (!ipsStr.good()) {
                        if (print) cout << "Cannot open file " << bunchCrossingId7bitFilename << " !" << endl;
                } else {
                        Int_t runIdread, offsetread;
                        while (ipsStr.good()) {
                                ipsStr >> runIdread;
                                ipsStr >> offsetread;
                                if (ipsStr.good() && (bunchCrossingNum < buffsize)) {
                                        if (bunchCrossingRuns) bunchCrossingRuns[bunchCrossingNum] = runIdread;
                                        if (bunchCrossingOffsets) bunchCrossingOffsets[bunchCrossingNum] = offsetread;
                                        bunchCrossingNum++;
                                }
                        }
                }
                ipsStr.close();
                if (bunchCrossingNum >= buffsize) bunchCrossingNum = buffsize - 1;
                if (print) cout << "Read " << bunchCrossingNum << " runs." << endl;
        }
        if (bunchCrossingRuns && bunchCrossingOffsets) {
                Bool_t found = false;
                for (Int_t i = 0;(i < bunchCrossingNum) && (!found);i++) {
                        if (bunchCrossingRuns[i] == runId) {
                                found = true;
                                offset = bunchCrossingOffsets[i];
                        }
                }
                if (!found) {
                        if (print) cout << "No offset found for run " << runId << " !" << endl;
                }
        } else {
                if (print) cout << "No data!" << endl;
        }
        return offset;
}

#include "vogel_copy.C"

#include "vogel2_copy.C"
