#ifdef DEFINE_HISTOGRAMS

DEFINE_HISTOGRAM_1D(Eta,                    candidateParameters.eta, "Eta;#eta");
DEFINE_HISTOGRAM_1D(Phi,                    candidateParameters.phi, "Phi;#phi");
DEFINE_HISTOGRAM_2D(EtaPhi,                 candidateParameters.eta, candidateParameters.phi, "Eta-Phi;#eta;#phi");
DEFINE_HISTOGRAM_1D(EtaCoord,               candidateParameters.etaCoord, "EtaCoord;#eta");
DEFINE_HISTOGRAM_1D(PhiCoord,               candidateParameters.phiCoord, "PhiCoord;#phi");
DEFINE_HISTOGRAM_2D(EtaPhiCoord,            candidateParameters.etaCoord, candidateParameters.phiCoord, "EtaCoord-PhiCoord;#eta;#phi");
DEFINE_HISTOGRAM_1D(OpenAngle,              candidateParameters.openangle, "Angle;open angle");
DEFINE_HISTOGRAM_1D(YieldDay,               day, "Yield per day;day");
DEFINE_HISTOGRAM_1D(Asymmetry,              candidateParameters.asymetry, "Asymmetry;asymmetry");
DEFINE_HISTOGRAM_2D(PtResolution,           candidate.point1.event.simulatedParticle.pT, candidateParameters.pTRec, "#pi^{0} p_{T} resolution - p_{T MC} vs. p_{T reco};p_{T MC};p_{T reco}");
DEFINE_HISTOGRAM_2D(PtResolutionPercent,    candidate.point1.event.simulatedParticle.pT, candidateParameters.pTRecoToSimu, "#pi^{0} p_{T} resolution;p_{T MC};p_{T reco} / p_{T MC}");
DEFINE_HISTOGRAM_2D(DayRun,                 day, runday, "Statistics per run;Day;Run");
DEFINE_HISTOGRAM_1D(Pt,                     candidateParameters.pTRec, "#pi^{0} p_{T};p_{T}, GeV/c");
DEFINE_HISTOGRAM_2D(PtMult,                 candidateParameters.pTRec, candidate.point1.event.uncorrectedNumberOfFtpcPrimariesEast, "Multiplicity vs. #pi^{0} p_{T};#pi^{0} p_{T}, GeV/c;Multiplicity");
DEFINE_HISTOGRAM_1D(M,                      candidateParameters.m, "#pi^{0} M_{inv};M_{inv}, GeV");
DEFINE_HISTOGRAM_2D(PtM,                    candidateParameters.pTRec, candidateParameters.m, "#pi^{0} M_{inv} vs. p_{T};p_{T}, GeV/c;M_{inv}, GeV/c^{2}");
DEFINE_HISTOGRAM_2D(PtOpenAngle,            candidateParameters.pTRec, candidateParameters.openangle, "#pi^{0} opening angle vs. p_{T};p_{T}, GeV/c;Opening angle");
DEFINE_HISTOGRAM_2D(EnergyOpenAngle,        candidateParameters.energy, candidateParameters.openangle, "#pi^{0} opening angle vs. energy;Energy, GeV;Opening angle");
DEFINE_HISTOGRAM_2D(EnergyOpenAngleMin,     candidateParameters.energy, getMinimumOpenangle(candidateParameters.m, candidateParameters.energy), "#pi^{0} min. opening angle vs. energy;Energy, GeV;Min. opening angle");
DEFINE_HISTOGRAM_2D(EnergyOpenAngleMinTrue, candidateParameters.energy, getMinimumOpenangle(truePionMass, candidateParameters.energy), "#pi^{0} min. opening angle (true pion mass) vs. energy;Energy, GeV;Min. open angle");
DEFINE_HISTOGRAM_2D(Eta1Eta2Coord,          candidate.point1.point.etaCoord, candidate.point2.point.etaCoord, "Eta1 vs. Eta2;#eta_{1};#eta_{2}");
DEFINE_HISTOGRAM_2D(Phi1Phi2Coord,          candidate.point1.point.phiCoord, candidate.point2.point.phiCoord, "Phi1 vs. Phi2;#phi_{1};#phi_{2}");
DEFINE_HISTOGRAM_1D(DeltaEtaCoord,          deta, "Eta1-Eta2;#eta_{1} - #eta_{2}");
DEFINE_HISTOGRAM_1D(DeltaPhiCoord,          dphi, "Phi1-Phi2;#phi_{1} - #phi_{2}");
DEFINE_HISTOGRAM_2D(DeltaEtaPhiCoord,       deta, dphi, "Eta1-Eta2 vs. Phi1-Phi2;#eta_{1} - #eta_{2};#phi_{1} - #phi_{2}");
DEFINE_HISTOGRAM_2D(CandidatePtEventEt,     candidateParameters.pTRec, candidate.point1.event.totalBEMCPointsEt, "Candidate p_{T} vs. event E_{T};#pi^{0} p_{T}, GeV/c;Event E_{T}, GeV");
DEFINE_HISTOGRAM_2D(CandidateEEventE,       candidateParameters.energy, candidate.point1.event.totalBEMCPointsEnergy, "Candidate energy vs. event energy;#pi^{0} energy, GeV;Event energy, GeV");
DEFINE_HISTOGRAM_1D(TPCVertYes,             PASSED(passedEventCuts1, EVENT_TPC_VERTEX_CUT) ? candidateParameters.pTRec : -1.0, "p_{T} in the events with TPC vertex");
DEFINE_HISTOGRAM_1D(TPCVertNo,              NOT_PASSED(passedEventCuts1, EVENT_TPC_VERTEX_CUT) ? candidateParameters.pTRec : -1.0, "p_{T} in the events without TPC vertex");
DEFINE_HISTOGRAM_1D(TPCVertYesBBCVertYes,   PASSED(passedEventCuts1, EVENT_TPC_VERTEX_CUT) && PASSED(passedEventCuts1, EVENT_BBC_VERTEX_CUT) ? candidateParameters.pTRec : -1.0, "p_{T} in the events with TPC vertex and with BBC vertex");
DEFINE_HISTOGRAM_1D(TPCVertYesBBCVertNo,    PASSED(passedEventCuts1, EVENT_TPC_VERTEX_CUT) && NOT_PASSED(passedEventCuts1, EVENT_BBC_VERTEX_CUT) ? candidateParameters.pTRec : -1.0, "p_{T} in the events with TPC vertex and without BBC vertex");
DEFINE_HISTOGRAM_1D(TPCVertNoBBCVertYes,    NOT_PASSED(passedEventCuts1, EVENT_TPC_VERTEX_CUT) && PASSED(passedEventCuts1, EVENT_BBC_VERTEX_CUT) ? candidateParameters.pTRec : -1.0, "p_{T} in the events without TPC vertex and with BBC vertex");
DEFINE_HISTOGRAM_1D(TPCVertNoBBCVertNo,     NOT_PASSED(passedEventCuts1, EVENT_TPC_VERTEX_CUT) && NOT_PASSED(passedEventCuts1, EVENT_BBC_VERTEX_CUT) ? candidateParameters.pTRec : -1.0, "p_{T} in the events without TPC vertex and without BBC vertex");
DEFINE_HISTOGRAM_1D(JetYes,                 PASSED(passedEventCuts1, EVENT_JET_ET_CUT) ? candidateParameters.pTRec : -1.0, "p_{T} in the events with jet above threshold");
DEFINE_HISTOGRAM_1D(JetNo,                  NOT_PASSED(passedEventCuts1, EVENT_JET_ET_CUT) ? candidateParameters.pTRec : -1.0, "p_{T} in the events without jet above threshold");
DEFINE_HISTOGRAM_1D(InJetPointYes,          (PASSED(passedPointCuts1, POINT_IN_JET_CUT) && PASSED(passedPointCuts2, POINT_IN_JET_CUT)) ? candidateParameters.pTRec : -1.0, "p_{T}, points in jet cone");
DEFINE_HISTOGRAM_1D(InJBkPtYes,((PASSED(passedPointCuts1,POINT_IN_JET_CUT)&&PASSED(passedPointCuts2,POINT_IN_JET_BACK_CUT))||(PASSED(passedPointCuts1,POINT_IN_JET_BACK_CUT)&&PASSED(passedPointCuts2,POINT_IN_JET_CUT)))?candidateParameters.pTRec:-1,"bk");
DEFINE_HISTOGRAM_1D(InJetPointNo,           !(PASSED(passedPointCuts1, POINT_IN_JET_CUT) && PASSED(passedPointCuts2, POINT_IN_JET_CUT)) ? candidateParameters.pTRec : -1.0, "p_{T}, points out of jet cone");
DEFINE_HISTOGRAM_1D(InJetYes,               PASSED(passedCandidateCuts, CANDIDATE_IN_JET_CUT) ? candidateParameters.pTRec : -1.0, "p_{T}, candidate in jet cone");
DEFINE_HISTOGRAM_1D(InJetNo,                NOT_PASSED(passedCandidateCuts, CANDIDATE_IN_JET_CUT) ? candidateParameters.pTRec : -1.0, "p_{T}, candidate out of jet cone");
DEFINE_HISTOGRAM_2D(CandidatePtJetEt,       candidateParameters.pTRec, TMath::Abs(candidate.point1.event.jet.eT), "Candidate p_{T} vs. jet E_{T};#pi^{0} p_{T}, GeV/c;Jet E_{T}, GeV");
DEFINE_HISTOGRAM_1D(PtSmd,(PASSED(passedPointCuts1,POINT_TYPE_SMDE_CUT|POINT_TYPE_SMDP_CUT)&&PASSED(passedPointCuts2,POINT_TYPE_SMDE_CUT|POINT_TYPE_SMDP_CUT))?candidateParameters.pTRec:-1,"Both points both SMD;p_{T}, GeV/c");
DEFINE_HISTOGRAM_1D(PtSmd1,(PASSED(passedPointCuts1,POINT_TYPE_SMDE_CUT|POINT_TYPE_SMDP_CUT)||PASSED(passedPointCuts2,POINT_TYPE_SMDE_CUT|POINT_TYPE_SMDP_CUT))?candidateParameters.pTRec:-1, "At least one point with both SMD;p_{T}, GeV/c");
DEFINE_HISTOGRAM_1D(PtSmd01,(PASSED(passedPointCuts1,POINT_TYPE_SMDE_CUT)||PASSED(passedPointCuts1,POINT_TYPE_SMDP_CUT)||PASSED(passedPointCuts2,POINT_TYPE_SMDE_CUT)||PASSED(passedPointCuts2,POINT_TYPE_SMDP_CUT))?candidateParameters.pTRec:-1,"SMD01");
DEFINE_HISTOGRAM_1D(PtSmdSz,(PASSED(passedPointCuts1,POINT_SMDE_SIZE_CUT|POINT_SMDP_SIZE_CUT)&&PASSED(passedPointCuts2,POINT_SMDE_SIZE_CUT|POINT_SMDP_SIZE_CUT))?candidateParameters.pTRec:-1,"Both points both SMD size;p_{T}, GeV/c");
DEFINE_HISTOGRAM_1D(PtSmdSz1,(PASSED(passedPointCuts1,POINT_SMDE_SIZE_CUT|POINT_SMDP_SIZE_CUT)||PASSED(passedPointCuts2,POINT_SMDE_SIZE_CUT|POINT_SMDP_SIZE_CUT))?candidateParameters.pTRec:-1, "At least one point with both SMD size;p_{T}, GeV/c");
DEFINE_HISTOGRAM_1D(PtSmdSz01,(PASSED(passedPointCuts1,POINT_SMDE_SIZE_CUT)||PASSED(passedPointCuts1,POINT_SMDP_SIZE_CUT)||PASSED(passedPointCuts2,POINT_SMDE_SIZE_CUT)||PASSED(passedPointCuts2,POINT_SMDP_SIZE_CUT))?candidateParameters.pTRec:-1,"SMDsz01");
DEFINE_HISTOGRAM_2D(TpcRefmultTrackDist,    candidate.point1.event.uncorrectedNumberOfTpcPrimaries, candidateParameters.distTrackClosest, "Mult. vs. closest track dist.;Event mult.;Dist. to closest track");

#else

#ifndef StPi0Analysis_TCandidateDataProcessor_H
#define StPi0Analysis_TCandidateDataProcessor_H

#include <TH1F.h>
#include <TH2F.h>

#include <list>
using std::list;

#include "StPi0AnalysisVersion.h"
#include "TDataProcessor.h"
#include "TInvariantMassDistribution.h"
#include "TPointDataProcessor.h"
#include "TEventDataProcessor.h"

class TCandidateDataProcessor : public TDataProcessor {
	public:
		typedef TCandidateDataProcessor this_type;
		typedef TDataProcessor inherited;
		typedef TInvariantMassDistribution distribution_type;
		typedef list<distribution_type> list_type;
		typedef TPointDataProcessor point_processor_type;
		typedef TEventDataProcessor event_processor_type;
		typedef TH1F hist_type;
		typedef TH2F hist2_type;

		TCandidateDataProcessor(const Char_t *name = 0, const Char_t *title = 0);
		TCandidateDataProcessor(const this_type &processor);
		virtual ~TCandidateDataProcessor();

		this_type &operator=(const this_type &processor);

		virtual void Print(Option_t* option) const;

		virtual Bool_t add(const inherited &processor);
 
		virtual Bool_t process(const void *data, const void *evt, Float_t wRef = -1.0);

		list_type invariantMassDistributions;
		list_type simulatedPtDistributions;
		list_type pointPtDistributions;
		list_type multiplicityPrimaryDistributions;
		list_type multiplicityPointsDistributions;
		list_type pointTrackDistDistributions;
		list_type pointTrackDist2Distributions;

		point_processor_type point1;
		point_processor_type point2;
		point_processor_type points;
		event_processor_type event1;
		event_processor_type event2;
		event_processor_type events;

#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DEF(hist_type,  NAME, TITLE)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DEF(hist2_type, NAME, TITLE)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DEF(hist3_type, NAME, TITLE)
#include "TCandidateDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D

		ClassDef(TCandidateDataProcessor, STPI0ANALYSIS_VERSION);
};

#endif

#endif
#undef DEFINE_HISTOGRAMS
