#ifdef DEFINE_HISTOGRAMS

DEFINE_HISTOGRAM_1D(EnergyTotal,                   event.totalBEMCHitsEnergy, "Total hits energy in event;Energy, GeV");
DEFINE_HISTOGRAM_1D(Z,                             eventParameters.zTPC, "Z_{TPC};Z_{TPC}");
DEFINE_HISTOGRAM_1D(Zused,                         eventParameters.zUse, "Z_{vert combined};Z_{vert combined}");
DEFINE_HISTOGRAM_1D(Zbbc,                          eventParameters.zBBC, "Z_{BBC};Z_{BBC}");
DEFINE_HISTOGRAM_2D(ZBBCvsTPC,                     eventParameters.zTPC, eventParameters.zBBC, "Z_{BBC} vs. Z_{TPC};Z_{TPC};Z_{BBC}");
DEFINE_HISTOGRAM_1D(ZBBCMinusTPC,                  (eventParameters.zTPC != 0) ? (eventParameters.zBBC - eventParameters.zTPC) : -1000, "Z_{BBC} - Z_{TPC};Z_{BBC} - Z_{TPC}");
DEFINE_HISTOGRAM_2D(BBCWMinusEvsTPC,               (eventParameters.zTPC != 0) ? eventParameters.zTPC : -1000, event.bbcEarliestWest - event.bbcEarliestEast, "BBC W-E vs. Z_{TPC};Z_{TPC};BBC W-E");
DEFINE_HISTOGRAM_2D(ZBBCtoTPC,                     eventParameters.zTPC, eventParameters.zBBC - eventParameters.zTPC, "Z_{BBC} - Z_{TPC} vs. Z_{TPC};Z_{TPC};Z_{BBC} - Z_{TPC}");
DEFINE_HISTOGRAM_1D(TracksNumber,                  event.nPrimary, "Number of primary tracks;Number of primary tracks");
DEFINE_HISTOGRAM_1D(PointsNumber,                  event.nPoints, "Number of BEMC points;Number of BEMC points");
DEFINE_HISTOGRAM_1D(ClustersNumber,                event.nClustersBTOW, "Number of tower clusters;Number of tower clusters");
DEFINE_HISTOGRAM_1D(HighestAdc,                    event.triggerSimulated.highestAdcHit.adc, "Highest ADC in event;Highest tower ADC in the event");
DEFINE_HISTOGRAM_1D(HighestAdcEmbed,               event.triggerSimulatedEmbed.highestAdcHit.adc, "Highest ADC in event - embed;Highest tower ADC in the event");
DEFINE_HISTOGRAM_1D(HighestAdcFinal,               event.triggerSimulatedFinal.highestAdcHit.adc, "Highest ADC in event - final;Highest tower ADC in the event");
DEFINE_HISTOGRAM_2D(DayRun,                        eventParameters.day, eventParameters.runDay, "Statistics per run;Day;Run");
DEFINE_HISTOGRAM_2D(DayRunAcceptanceBTOW,          -eventParameters.day, -eventParameters.runDay, "BTOW acceptance per run;Day;Run");
DEFINE_HISTOGRAM_2D(DayRunAcceptanceBPRS,          -eventParameters.day, -eventParameters.runDay, "BPRS acceptance per run;Day;Run");
DEFINE_HISTOGRAM_2D(DayRunAcceptanceBSMDE,         -eventParameters.day, -eventParameters.runDay, "BSMDE acceptance per run;Day;Run");
DEFINE_HISTOGRAM_2D(DayRunAcceptanceBSMDP,         -eventParameters.day, -eventParameters.runDay, "BSMDP acceptance per run;Day;Run");
DEFINE_HISTOGRAM_1D(StatDay,                       eventParameters.day, "Statistics per day;Day");
DEFINE_HISTOGRAM_1D(StatYear,                      eventParameters.year, "Statistics per year;Year");
DEFINE_HISTOGRAM_1D(NumFtpcPrimaries,              eventParameters.uncorrectedNumberOfFtpcPrimaries, "Number of FTPC primaries;Number of FTPC primaries");
DEFINE_HISTOGRAM_1D(NumFtpcPrimariesEast,          event.uncorrectedNumberOfFtpcPrimariesEast, "Number of East FTPC primaries;Number of East FTPC primaries");
DEFINE_HISTOGRAM_1D(NumFtpcPrimariesWest,          event.uncorrectedNumberOfFtpcPrimariesWest, "Number of West FTPC primaries;Number of West FTPC primaries");
DEFINE_HISTOGRAM_2D(NumFtpcPrimariesEastDay,       eventParameters.day, event.uncorrectedNumberOfFtpcPrimariesEast, "Number of East FTPC primaries vs. day;Day;Number of East FTPC primaries");
DEFINE_HISTOGRAM_2D(NumFtpcPrimariesWestDay,       eventParameters.day, event.uncorrectedNumberOfFtpcPrimariesWest, "Number of West FTPC primaries vs. day;Day;Number of West FTPC primaries");
//DEFINE_HISTOGRAM_2D(NumFtpcPrimariesPrimary,       eventParameters.uncorrectedNumberOfFtpcPrimaries, event.nPrimary, "Number of FTPC primaries - all primaries;Number of FTPC primaries;Number of all primaries");
DEFINE_HISTOGRAM_2D(NumFtpcPrimariesPoints,        eventParameters.uncorrectedNumberOfFtpcPrimaries, event.nPoints, "Number of FTPC primaries - BEMC points;Number of FTPC primaries;Number of BEMC points");
//DEFINE_HISTOGRAM_2D(NumFtpcPrimariesNeutralPoints, eventParameters.uncorrectedNumberOfFtpcPrimaries, event.nNeutralPoints, "Number of FTPC primaries - BEMC neutral points;Number of FTPC primaries;Number of neutral BEMC points");
//DEFINE_HISTOGRAM_2D(NumFtpcPrimariesClusters,      eventParameters.uncorrectedNumberOfFtpcPrimaries, event.nClustersTower, "Number of FTPC primaries - BEMC clusters;Number of FTPC primaries;Number of BEMC clusters");
//DEFINE_HISTOGRAM_2D(NumTpcPrimariesPoints,         event.nPrimary, event.nPoints, "Number of all primaries - BEMC points;Number of all primaries;Number of BEMC points");
//DEFINE_HISTOGRAM_2D(NumTpcPrimariesNeutralPoints,  event.nPrimary, event.nNeutralPoints, "Number of all primaries - BEMC neutral points;Number of all primaries;Number of neutral BEMC points");
DEFINE_HISTOGRAM_1D(TpcRefmult,                    event.uncorrectedNumberOfTpcPrimaries, "Number of primary TPC tracks;Number of primary TPC tracks");
DEFINE_HISTOGRAM_2D(FtpcTpcRefmult,                eventParameters.uncorrectedNumberOfFtpcPrimaries, event.uncorrectedNumberOfTpcPrimaries, "Number of FTPC primaries - TPC primaries;Number of FTPC primaries;Number of TPC primaries");
DEFINE_HISTOGRAM_2D(TpcRefmultPoints,              event.uncorrectedNumberOfTpcPrimaries, event.nPoints, "Number of TPC primaries - BEMC points;Number of TPC primaries;Number of BEMC points");
//DEFINE_HISTOGRAM_2D(ZFtpcRefmult,                  eventParameters.zUse, eventParameters.uncorrectedNumberOfFtpcPrimaries, "Number of primary FTPC tracks vs Z;Z;Number of primary FTPC tracks");
//DEFINE_HISTOGRAM_2D(ZTpcRefmult,                   eventParameters.zUse, event.uncorrectedNumberOfTpcPrimaries, "Number of primary TPC tracks vs Z;Z;Number of primary TPC tracks");
DEFINE_HISTOGRAM_1D(ClustersTracksRatio,           (event.nPrimary != 0) ? (Float_t(event.nClustersBTOW) / Float_t(event.nPrimary)) : -1, "#frac{Number of tower clusters}{Number of primary tracks};Number of tower clusters / Number of primary tracks");
DEFINE_HISTOGRAM_1D(TotalEMCEt,                    event.totalBEMCPointsEt, "Total EMC E_{T} per event;E_{T}");
DEFINE_HISTOGRAM_1D(TotalEMCE,                     event.totalBEMCPointsEnergy, "Total EMC E per event;E");
DEFINE_HISTOGRAM_1D(TotalTPCPt,                    event.totalTPCPt, "Total TPC p_{T} per event;p_{T}");
DEFINE_HISTOGRAM_1D(TPCPtToEMCEt,                  (event.totalBEMCPointsEt != 0) ? (event.totalTPCPt / event.totalBEMCPointsEt) : -0.1, "TPC p_{T} / EMC E_{T} per event;p_{T} / E_{T total}");
DEFINE_HISTOGRAM_1D(EMCEtToSum,                    ((event.totalBEMCPointsEt + event.totalTPCPt) != 0) ? (event.totalBEMCPointsEt / (event.totalBEMCPointsEt + event.totalTPCPt)) : -0.1, "EMC E_{T} / (EMC E_{T} + TPC p_{T}) per event;E_{T} / (E_{T} + p_{T})");
DEFINE_HISTOGRAM_2D(TPCPtEMCEt,                    event.totalTPCPt, event.totalBEMCPointsEt, "Total TPC p_{T} vs. EMC E_{T};Total TPC p_{T}, GeV/c;Total EMC E_{T}, GeV");
DEFINE_HISTOGRAM_1D(TpcGlobals,                    event.nGlobal, "Number of global TPC tracks;Number of global TPC tracks");
//DEFINE_HISTOGRAM_2D(NumTpcPrimariesGlobals,        event.uncorrectedNumberOfTpcPrimaries, event.nGlobal, "Number of all primaries - globals;Number of primary tracks;Number of global tracks");
DEFINE_HISTOGRAM_2D(TPCGlobalsEMCEt,               event.nGlobal, event.totalBEMCPointsEt, "Number of TPC global tracks vs. EMC E_{T};Number of TPC global tracks;Total EMC E_{T}, GeV");
DEFINE_HISTOGRAM_3D(DayRunBunchCrossing,           eventParameters.day, eventParameters.runDay, event.bunchCrossingId7bit, "Bunch crossing 7bit;Day;Run;Bunch crossing 7bit");
DEFINE_HISTOGRAM_3D(DayRunBunchCrossingOffset,     eventParameters.day, eventParameters.runDay, eventParameters.bunchCrossingId7bitPlusOffset, "Bunch crossing 7bit + offset;Day;Run;Bunch crossing 7bit - offset");
DEFINE_HISTOGRAM_1D(JetEta,                        event.jet.eta, "Jet eta;Jet eta");
DEFINE_HISTOGRAM_1D(JetPhi,                        event.jet.phi, "Jet phi;Jet phi");
DEFINE_HISTOGRAM_1D(JetEnergy,                     TMath::Abs(event.jet.eT), "Jet E_{T};Jet E_{T}, GeV");
DEFINE_HISTOGRAM_2D(TotalEMCJetEt,                 event.totalBEMCPointsEt, TMath::Abs(event.jet.eT), "Total EMC vs. Jet E_{T};Total EMC E_{T}, GeV;Jet E_{T}, GeV");
DEFINE_HISTOGRAM_2D(BbcWE,                         event.bbcEarliestWest, event.bbcEarliestEast, "BBC West vs. East;BBC West;BBC East")
DEFINE_HISTOGRAM_1D(BbcWMinusE,                    event.bbcEarliestWest - event.bbcEarliestEast, "BBC West - East;BBC West - East;Events")
DEFINE_HISTOGRAM_2D(NumberHitsBTOWstuckbit,        event.nHitsBTOW, event.nHitsBTOWstuckbit, "Number of BTOW hits: stuck bits vs. total;Number of BTOW hits;With stuck bits")
DEFINE_HISTOGRAM_1D(PartonicPt,                    event.simulatedParticle.pT, "PYTHIA partonic p_{T};p_{T}");

#else

#ifndef StPi0Analysis_TEventDataProcessor_H
#define StPi0Analysis_TEventDataProcessor_H

#include <TH1F.h>
#include <TH2F.h>
#include <TH3F.h>

#include <list>
using std::list;

#include "StPi0AnalysisVersion.h"
#include "TDataProcessor.h"
#include "THitDataProcessor.h"
#include "TBinStatistics.h"

class TEventDataProcessor : public TDataProcessor {
	public:
		typedef TEventDataProcessor this_type;
		typedef TDataProcessor inherited;
		typedef TBinStatistics bin_statistics_type;
		typedef list<bin_statistics_type> list_type;
		typedef TH1F hist_type;
		typedef TH2F hist2_type;
		typedef TH3F hist3_type;

		TEventDataProcessor(const Char_t *name = 0, const Char_t *title = 0);
		TEventDataProcessor(const this_type &processor);
		virtual ~TEventDataProcessor();

		this_type &operator=(const this_type &processor);

		virtual void Print(Option_t* option) const;

		virtual Bool_t add(const inherited &processor);
 
		virtual Bool_t process(const void *data, const void *evt, Float_t wRef = -1.0);

		list_type binStatistics;

		THitDataProcessor highestAdcHit;
		THitDataProcessor highestEtHit;

#define DEFINE_HISTOGRAMS
#define DEFINE_HISTOGRAM_1D(NAME, X, TITLE)       HISTO_DEF(hist_type,  NAME, TITLE)
#define DEFINE_HISTOGRAM_2D(NAME, X, Y, TITLE)    HISTO_DEF(hist2_type, NAME, TITLE)
#define DEFINE_HISTOGRAM_3D(NAME, X, Y, Z, TITLE) HISTO_DEF(hist3_type, NAME, TITLE)
#include "TEventDataProcessor.h"
#undef DEFINE_HISTOGRAM_1D
#undef DEFINE_HISTOGRAM_2D
#undef DEFINE_HISTOGRAM_3D

		ClassDef(TEventDataProcessor, STPI0ANALYSIS_VERSION);
};

#endif

#endif
#undef DEFINE_HISTOGRAMS
