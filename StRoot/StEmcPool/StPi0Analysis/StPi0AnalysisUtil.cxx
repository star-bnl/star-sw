#include "StPi0AnalysisUtil.h"
#include "TBinStatistics.h"
#include "TCutParameters.h"
#include "TWeightCalculator.h"

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
	    deltaPhi2 = -deltaPhi2;
	    deltaEta2 = -deltaEta2;
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
	v1.SetMag(candidate.point1.point.energy);
	TVector3 v2;
	v2.SetPtEtaPhi(EMCRadius, candidate.point2.point.etaCoord, candidate.point2.point.phiCoord);
	v2.SetZ(v2.Z() - z2);
	if (cutParameters.jetRotate != 0) {
	    v2.SetPtEtaPhi(v2.Perp(), v2.Eta() - deltaEta2, v2.Phi() - deltaPhi2);
	}
	v2.SetMag(candidate.point2.point.energy);
	TVector3 pRec = v1 + v2;
	candidateParameters.openangle = v1.Angle(v2);
	candidateParameters.m = sqrt(2 * candidate.point1.point.energy * candidate.point2.point.energy * (1 - cos(candidateParameters.openangle)));
	candidateParameters.pTRec = pRec.Perp();
	candidateParameters.eta = pRec.Eta();
	candidateParameters.phi = pRec.Phi();
	candidateParameters.asymetry = fabs(candidate.point1.point.energy - candidate.point2.point.energy) / (candidate.point1.point.energy + candidate.point2.point.energy);
	candidateParameters.egamma = (candidate.point1.point.energy > candidate.point2.point.energy) ? candidate.point1.point.energy : candidate.point2.point.energy;
	candidateParameters.energy = candidate.point1.point.energy + candidate.point2.point.energy;
	TVector3 pRecCoord;
	pRecCoord.SetPtEtaPhi(EMCRadius, candidateParameters.eta, candidateParameters.phi);
	pRecCoord.SetZ(pRecCoord.Z() + ((z1 + z2) / 2.0));
	candidateParameters.etaCoord = pRecCoord.Eta();
	candidateParameters.phiCoord = pRecCoord.Phi();
	candidateParameters.towerId = (candidate.point1.point.energy > candidate.point2.point.energy) ? candidate.point1.point.clusterBTOW.highestEnergyHit.id : candidate.point2.point.clusterBTOW.highestEnergyHit.id;
	candidateParameters.pointEtaCoord = (candidate.point1.point.energy > candidate.point2.point.energy) ? candidate.point1.point.etaCoord : candidate.point2.point.etaCoord;
	candidateParameters.jetDeltaEta = candidateParameters.eta - event1.jet.eta;
	candidateParameters.jetDeltaPhi = candidateParameters.phi - event1.jet.phi;
	while (candidateParameters.jetDeltaPhi < -TMath::Pi()) candidateParameters.jetDeltaPhi += TMath::TwoPi();
	while (candidateParameters.jetDeltaPhi > +TMath::Pi()) candidateParameters.jetDeltaPhi -= TMath::TwoPi();
	candidateParameters.jetDist = sqrt((candidateParameters.jetDeltaEta * candidateParameters.jetDeltaEta) + (candidateParameters.jetDeltaPhi * candidateParameters.jetDeltaPhi));
        candidateParameters.massRegionLeft = cutParameters.massRegionLeft + (candidateParameters.pTRec * cutParameters.massRegionLeftPt);
        candidateParameters.massRegionRight = cutParameters.massRegionRight + (candidateParameters.pTRec * cutParameters.massRegionRightPt);
	candidateParameters.pTRecoToSimu = (candidate.point1.event.simulatedParticle.pT != 0) ? (candidateParameters.pTRec / candidate.point1.event.simulatedParticle.pT) : 0;
}

void getHitParams(const TMyEventData &event, const TMyHitData &hit, const TEventParameters &eventParameters, const TCutParameters &cutParameters, THitParameters &hitParameters) {
	hitParameters.etaCoord = (Float_t(Int_t(hit.etaphiCoord)) / 100000.0) - 1.0;
	hitParameters.phiCoord = ((hit.etaphiCoord - Float_t(Int_t(hit.etaphiCoord))) * 7.0) - 3.5;
	Float_t z = eventParameters.zUse;
	TVector3 v;
	v.SetPtEtaPhi(EMCRadius, hitParameters.etaCoord, hitParameters.phiCoord);
	v.SetZ(v.Z() - z);
	v.SetMag(hit.energy);
	hitParameters.eT = v.Perp();
	hitParameters.eta = v.Eta();
	hitParameters.phi = v.Phi();
}

void getPointParams(const TMyEventData &event, const TMyPointData &point, const TEventParameters &eventParameters, const TCutParameters &cutParameters, TPointParameters &pointParameters) {
	pointParameters.triggeredHT1 = (point.trigger.triggered & cutParameters.triggersHT1);
	pointParameters.triggeredHT2 = (point.trigger.triggered & cutParameters.triggersHT2);
	Float_t z = eventParameters.zUse;
	TVector3 v;
	v.SetPtEtaPhi(EMCRadius, point.etaCoord, point.phiCoord);
	v.SetZ(v.Z() - z);
	v.SetMag(point.energy);
	pointParameters.pTRec = v.Perp();
	pointParameters.eta = v.Eta();
	pointParameters.phi = v.Phi();
	pointParameters.energy = point.energy;
	pointParameters.etaCoord = point.etaCoord;
	pointParameters.phiCoord = point.phiCoord;
	pointParameters.towerId = point.clusterBTOW.highestEnergyHit.id;
	pointParameters.jetDeltaEta = pointParameters.eta - event.jet.eta;
	pointParameters.jetDeltaPhi = pointParameters.phi - event.jet.phi;
	while (pointParameters.jetDeltaPhi < -TMath::Pi()) pointParameters.jetDeltaPhi += TMath::TwoPi();
	while (pointParameters.jetDeltaPhi > +TMath::Pi()) pointParameters.jetDeltaPhi -= TMath::TwoPi();
	pointParameters.distJet = sqrt((pointParameters.jetDeltaEta * pointParameters.jetDeltaEta) + (pointParameters.jetDeltaPhi * pointParameters.jetDeltaPhi));
        pointParameters.distTrack = sqrt((point.trackDeta * point.trackDeta) + (point.trackDphi * point.trackDphi));
        pointParameters.distGamma = sqrt((point.trackMCDeta * point.trackMCDeta) + (point.trackMCDphi * point.trackMCDphi));
	pointParameters.smdAsymetry = ((point.clusterBSMDE.energy + point.clusterBSMDP.energy) != 0) ? (fabs(point.clusterBSMDE.energy - point.clusterBSMDP.energy) / (point.clusterBSMDE.energy + point.clusterBSMDP.energy)) : 0.0;
	pointParameters.towerCenterDist = sqrt((point.towerCenterDeta * point.towerCenterDeta) + (point.towerCenterDphi * point.towerCenterDphi));
	pointParameters.triggeredHT1Et = (pointParameters.pTRec >= cutParameters.HT1EtThreshold);
	pointParameters.triggeredHT2Et = (pointParameters.pTRec >= cutParameters.HT2EtThreshold);
	if (point.clusterBTOW.isValid() && point.clusterBTOW.highestEnergyHit.isValid()) getHitParams(event, point.clusterBTOW.highestEnergyHit, eventParameters, cutParameters, pointParameters.highestEnergyHitBTOW);
}

void getPionParams(const TMyEventData &event, const TMySimulatedDecayData &pion, const TEventParameters &eventParameters, const TGammaParameters &gamma1Parameters, const TGammaParameters &gamma2Parameters, const TCutParameters &cutParameters, TPionParameters &pionParameters) {
	Float_t z = event.simulatedParticle.z;
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
	pionParameters.asymetry = fabs(gamma1Parameters.energy - gamma2Parameters.energy) / (gamma1Parameters.energy + gamma2Parameters.energy);
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
}

void getGammaParams(const TMyEventData &event, const TMySimulatedParticleData &gamma, const TEventParameters &eventParameters, const TCutParameters &cutParameters, TGammaParameters &gammaParameters) {
	Float_t z = event.simulatedParticle.z;
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
		Bool_t doSmearing = (gRandom->Uniform() <= prob);
		if (doSmearing) {
		    Bool_t doSmearingZero = false;
		    if (cutParameters.zSimuSmearingZero) {
			Float_t prob = 0.0;
			if (cutParameters.zSimuSmearingZeroPt) {
			    prob = cutParameters.zSimuSmearingZeroPt->getWeight(event.simulatedParticle.pT);
			}
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

// NLO pQCD (W. Vogelsang et al.)
Float_t Qx[]  = {       1.25,      1.75,      2.25,      2.75,      3.25,      3.75,      4.25,      4.75,      5.25,      5.75,      6.25,      6.75,      7.25,      7.75,      8.25,      8.75,      9.25,      9.75,     10.25,     10.75,     11.25,     11.75,     12.25,     12.75,     13.25,     13.75,     14.25,     14.75,     15.25,     15.75,       20.00};
// Factorization scale mu = pT
Float_t Qy[]  = {  5.202E+08, 6.268E+07, 1.102E+07, 2.649E+06, 7.886E+05, 2.768E+05, 1.093E+05, 4.745E+04, 2.223E+04, 1.115E+04, 5.882E+03, 3.239E+03, 1.860E+03, 1.100E+03, 6.710E+02, 4.210E+02, 2.678E+02, 1.749E+02, 1.165E+02, 7.893E+01, 5.399E+01, 3.743E+01, 2.636E+01, 1.874E+01, 1.349E+01, 9.774E+00, 7.151E+00, 5.296E+00,      3.90,      2.90,    2.90*0.2};
// Factorization scale mu = pT/2
Float_t Q1y[] = {  3.421E+09, 1.720E+08, 2.556E+07, 6.150E+06, 1.618E+06, 5.143E+05, 1.902E+05, 7.861E+04, 3.539E+04, 1.726E+04, 8.937E+03, 4.813E+03, 2.731E+03, 1.596E+03, 9.665E+02, 6.008E+02, 3.808E+02, 2.480E+02, 1.651E+02, 1.120E+02, 7.643E+01, 5.296E+01, 3.742E+01, 2.656E+01, 1.915E+01, 1.391E+01, 1.016E+01, 7.529E+00,      5.55,      4.12,    4.12*0.2};
// Factorization scale mu = pT*2
Float_t Q2y[] = {  2.640E+08, 3.002E+07, 5.497E+06, 1.374E+06, 4.219E+05, 1.521E+05, 6.130E+04, 2.710E+04, 1.284E+04, 6.492E+03, 3.453E+03, 1.918E+03, 1.110E+03, 6.605E+02, 4.049E+02, 2.552E+02, 1.631E+02, 1.070E+02, 7.156E+01, 4.859E+01, 3.336E+01, 2.319E+01, 1.638E+01, 1.167E+01, 8.408E+00, 6.105E+00, 4.476E+00, 3.319E+00,      2.45,      1.82,    1.82*0.2};

TGraph *pQCDgr = 0;
TGraph *pQCD1gr = 0;
TGraph *pQCD2gr = 0;
TWeightCalculator pQCDweight("pQCDweight", "pQCD weight");
Double_t getNLOpQCD(Double_t *x, Double_t *p) {
        const Float_t pQCDmultiplier = (2 * 197 * 1e-9) / (0.93 * 0.95 * 1.03 * 2.21 * 1e3);
        if (!pQCDgr) {
                Float_t *QyL = new Float_t[sizeof(Qy) / sizeof(Qy[0])];
                bin_stat_list_type pQCDspectrum;
                for (Int_t i = 0;i < Int_t(sizeof(Qy) / sizeof(Qy[0]));i++) {
                        QyL[i] = log(Qy[i]);
                        TBinParameters par;
                        par.min = Qx[i];
                        par.max = Qx[i];
                        TBinStatistics bin;
                        bin.setParameters(par);
                        bin.setValue(Qy[i] * pQCDmultiplier);
                        bin.setError(Qy[i] * pQCDmultiplier * 0.1);
                        pQCDspectrum.push_back(bin);
                }
                pQCDweight.Fit(pQCDspectrum);
                pQCDgr = new TGraph(sizeof(Qx) / sizeof(Qx[0]), Qx, QyL);
        }
        if (!pQCD1gr) {
                Float_t *Q1yR = new Float_t[sizeof(Q1y) / sizeof(Q1y[0])];
                for (Int_t i = 0;i < Int_t(sizeof(Q1y) / sizeof(Q1y[0]));i++) Q1yR[i] = Q1y[i] / Qy[i];
                pQCD1gr = new TGraph(sizeof(Qx) / sizeof(Qx[0]), Qx, Q1yR);
        }
        if (!pQCD2gr) {
                Float_t *Q2yR = new Float_t[sizeof(Q2y) / sizeof(Q2y[0])];
                for (Int_t i = 0;i < Int_t(sizeof(Q2y) / sizeof(Q2y[0]));i++) Q2yR[i] = Q2y[i] / Qy[i];
                pQCD2gr = new TGraph(sizeof(Qx) / sizeof(Qx[0]), Qx, Q2yR);
        }
        Float_t y = 0;
        if (x && p) {
                if (p[0] == 0) {
                        y = exp(pQCDgr->Eval(x[0], 0, "S")) * pQCDmultiplier;
                } else if (p[0] == 1) {
                        y = exp(pQCDgr->Eval(x[0], 0, "S")) * pQCDmultiplier * pQCD1gr->Eval(x[0], 0, "S");
                } else if (p[0] == 2) {
                        y = exp(pQCDgr->Eval(x[0], 0, "S")) * pQCDmultiplier * pQCD2gr->Eval(x[0], 0, "S");
                } else if (p[0] == 3) {
                        y = exp(pQCDgr->Eval(x[0], 0, "S")) * pQCDmultiplier * x[0];
                } else if (p[0] == 10) {
			y = 1.0;
                } else if (p[0] == 11) {
                        y = pQCD1gr->Eval(x[0], 0, "S");
                } else if (p[0] == 12) {
                        y = pQCD2gr->Eval(x[0], 0, "S");
                }
        }
        return y;
}

  // Vogelsang calculation
// KKP FF
Float_t nloXSect[] = {1.769E+09, 1.867E+08, 2.532E+07, 5.069E+06, 1.347E+06, 4.329E+05, 1.594E+05, 6.592E+04, 2.986E+04, 1.448E+04, 7.452E+03, 4.029E+03, 2.276E+03, 1.323E+03, 7.979E+02, 4.946E+02, 3.128E+02, 2.026E+02, 1.336E+02, 8.977E+01, 6.126E+01, 4.223E+01, 2.965E+01, 2.098E+01, 1.506E+01, 1.087E+01, 7.935E+00, 5.843E+00, 4.330E+00, 3.232E+00, 2.433E+00, 1.840E+00, 1.398E+00, 1.069E+00, 8.220E-01, 6.337E-01, 4.909E-01, 3.818E-01, .984E-01};
// Kretzer FF
Float_t nloXSectK[] = {1.233E+09, 7.542E+07,9.887E+06,2.001E+06,5.456E+05,1.808E+05,6.890E+04,2.956E+04,1.381E+04,6.947E+03,3.688E+03,2.051E+03,1.193E+03,7.117E+02,4.391E+02,2.786E+02,1.799E+02,1.187E+02,7.960E+01,5.443E+01,3.773E+01,2.637E+01,1.874E+01,1.341E+01,9.719E+00,7.082E+00,5.217E+00,3.875E+00,2.893E+00,2.173E+00,1.645E+00,1.251E+00,9.548E-01,7.328E-01,5.653E-01,4.375E-01,3.398E-01,2.651E-01,2.077E-01};
//Pt for both:
Float_t nloPt[] = {1.0,1.5,2.0,2.500E+00,3.000E+00,3.500E+00,4.000E+00,4.500E+00,5.000E+00,5.500E+00,6.000E+00,6.500E+00,7.000E+00,7.500E+00,8.000E+00,8.500E+00,9.000E+00,9.500E+00,1.000E+01,1.050E+01,1.100E+01,1.150E+01,1.200E+01,1.250E+01,1.300E+01,1.350E+01,1.400E+01,1.450E+01,1.500E+01,1.550E+01,1.600E+01,1.650E+01,1.700E+01,1.750E+01,1.800E+01,1.850E+01,1.900E+01,1.950E+01,2.000E+01};
// additional NLO results: KKP with different scales
Float_t nloXSectmu2[] = {1.126E+09, 8.320E+07, 1.194E+07, 2.525E+06, 7.014E+05, 2.333E+05, 8.847E+04, 3.742E+04, 1.719E+04, 8.440E+03, 4.390E+03, 2.396E+03, 1.363E+03, 7.982E+02, 4.848E+02, 3.015E+02, 1.918E+02, 1.248E+02, 8.252E+01, 5.565E+01, 3.808E+01, 2.635E+01, 1.852E+01, 1.314E+01, 9.447E+00, 6.835E+00, 4.994E+00, 3.681E+00, 2.732E+00, 2.040E+00, 1.536E+00, 1.163E+00, 8.839E-01, 6.759E-01, 5.195E-01, 4.007E-01, 3.103E-01, 2.414E-01, 1.887E-01};
Float_t nloXSectmu05[] = {4.931E+10, 5.666E+08, 5.814E+07, 1.152E+07, 2.987E+06, 8.516E+05, 2.924E+05, 1.134E+05, 4.894E+04, 2.289E+04, 1.145E+04, 6.052E+03, 3.365E+03, 1.927E+03, 1.146E+03, 7.080E+02, 4.423E+02, 2.858E+02, 1.880E+02, 1.264E+02, 8.619E+01, 5.922E+01, 4.168E+01, 2.946E+01, 2.119E+01, 1.532E+01, 1.117E+01, 8.256E+00, 6.113E+00, 4.575E+00, 3.444E+00, 2.613E+00, 1.990E+00, 1.523E+00, 1.175E+00, 9.079E-01, 7.042E-01, 5.485E-01, 4.291E-01};

TGraph *pQCDPPgr = 0;
TGraph *pQCDPP1gr = 0;
TGraph *pQCDPP2gr = 0;
TWeightCalculator pQCDPPweight("pQCDPPweight", "pQCD p+p weight");
Double_t getNLOpQCDPP(Double_t *x, Double_t *p) {
        const Float_t pQCDPPmultiplier = (1 * 1 * 1e-9)/* / (30.0 * 1e-3)*/; // NSD cross section is 30.0 +/- 3.5 mb
        if (!pQCDPPgr) {
                Float_t *QyL = new Float_t[sizeof(nloXSect) / sizeof(nloXSect[0])];
                bin_stat_list_type pQCDPPspectrum;
                for (Int_t i = 0;i < Int_t(sizeof(nloXSect) / sizeof(nloXSect[0]));i++) {
                        QyL[i] = log(nloXSect[i]);
                        TBinParameters par;
                        par.min = nloPt[i];
                        par.max = nloPt[i];
                        TBinStatistics bin;
                        bin.setParameters(par);
                        bin.setValue(nloXSect[i] * pQCDPPmultiplier);
                        bin.setError(nloXSect[i] * pQCDPPmultiplier * 0.1);
                        pQCDPPspectrum.push_back(bin);
                }
                pQCDPPweight.Fit(pQCDPPspectrum);
                pQCDPPgr = new TGraph(sizeof(nloXSect) / sizeof(nloXSect[0]), nloPt, QyL);
        }
        if (!pQCDPP1gr) {
                Float_t *Q1yR = new Float_t[sizeof(nloXSectmu05) / sizeof(nloXSectmu05[0])];
                for (Int_t i = 0;i < Int_t(sizeof(nloXSectmu05) / sizeof(nloXSectmu05[0]));i++) Q1yR[i] = nloXSectmu05[i] / nloXSect[i];
                pQCDPP1gr = new TGraph(sizeof(nloXSectmu05) / sizeof(nloXSectmu05[0]), nloPt, Q1yR);
        }
        if (!pQCDPP2gr) {
                Float_t *Q2yR = new Float_t[sizeof(nloXSectmu2) / sizeof(nloXSectmu2[0])];
                for (Int_t i = 0;i < Int_t(sizeof(nloXSectmu2) / sizeof(nloXSectmu2[0]));i++) Q2yR[i] = nloXSectmu2[i] / nloXSect[i];
                pQCDPP2gr = new TGraph(sizeof(nloXSectmu2) / sizeof(nloXSectmu2[0]), nloPt, Q2yR);
        }
        Float_t y = 0;
        if (x && p) {
                if (p[0] == 0) {
                        y = exp(pQCDPPgr->Eval(x[0], 0, "S")) * pQCDPPmultiplier;
                } else if (p[0] == 1) {
                        y = exp(pQCDPPgr->Eval(x[0], 0, "S")) * pQCDPPmultiplier * pQCDPP1gr->Eval(x[0], 0, "S");
                } else if (p[0] == 2) {
                        y = exp(pQCDPPgr->Eval(x[0], 0, "S")) * pQCDPPmultiplier * pQCDPP2gr->Eval(x[0], 0, "S");
                } else if (p[0] == 3) {
                        y = exp(pQCDPPgr->Eval(x[0], 0, "S")) * pQCDPPmultiplier * x[0];
                } else if (p[0] == 10) {
			y = 1.0;
                } else if (p[0] == 11) {
                        y = pQCDPP1gr->Eval(x[0], 0, "S");
                } else if (p[0] == 12) {
                        y = pQCDPP2gr->Eval(x[0], 0, "S");
                }
        }
        return y;
}
