#ifndef StPi0Analysis_Util_H
#define StPi0Analysis_Util_H

#include <TObject.h>
#include <TH1.h>

#include <fstream>
#include <list>
#include <map>
using namespace std;

class TMyEventData;
class TMyPointData;
class TMyHitData;
class TMyCandidateTreeData;
class TMySimulatedParticleData;
class TMySimulatedDecayData;

#include "StPi0AnalysisVersion.h"

class TCutParameters;
class TWeightCalculator;
class TBinStatistics;
typedef TBinStatistics bin_stat_type;
typedef list<bin_stat_type> bin_stat_list_type;

extern TWeightCalculator pQCDweight;
Double_t getNLOpQCD(Double_t *x, Double_t *p);
extern TWeightCalculator pQCDPPweight;
extern TWeightCalculator pQCDPPKweight;
Double_t getNLOpQCDPP(Double_t *x, Double_t *p);
extern TWeightCalculator pQCDPPweight_2;
Double_t getNLOpQCDPP_2(Double_t *x, Double_t *p);

enum TBinVariable {pT, eGamma};

struct TCandidateParameters {
	Float_t m;
	Float_t pTRec;
	Float_t energy;
	Float_t eta;
	Float_t phi;
	Float_t etaCoord;
	Float_t phiCoord;
	Float_t openangle;
	Float_t asymetry;
	Float_t egamma;
	Int_t towerId;
	Float_t pointEtaCoord;
	Float_t jetDeltaEta;
	Float_t jetDeltaPhi;
	Float_t jetDist;
	Float_t jetBackDeltaEta;
	Float_t jetBackDeltaPhi;
	Float_t jetBackDist;
	Float_t massRegionLeft;
	Float_t massRegionRight;
	Float_t pTRecoToSimu;
	Float_t distTrackClosest;
	Float_t distTrackClosest2;
};

struct THitParameters {
	Float_t etaCoord;
	Float_t phiCoord;
	Float_t eta;
	Float_t phi;
	Float_t eT;
	Float_t ped;
};

struct TPointParameters {
	Bool_t triggeredHT1;
	Bool_t triggeredHT2;
	Bool_t triggeredHT1Et;
	Bool_t triggeredHT2Et;
	Bool_t triggeredTowerHT1Et;
	Bool_t triggeredTowerHT2Et;
	Float_t pTRec;
	Float_t energy;
	Float_t eta;
	Float_t phi;
	Float_t etaCoord;
	Float_t phiCoord;
	Int_t towerId;
	Float_t jetDeltaEta;
	Float_t jetDeltaPhi;
	Float_t distJet;
	Float_t jetBackDeltaEta;
	Float_t jetBackDeltaPhi;
	Float_t distJetBack;
	Float_t distTrack;
	Bool_t passedCPV;
	Float_t distTrack2;
	Float_t distGamma;
	Float_t smdAsymetry;
	Float_t towerCenterDist;
	THitParameters highestEnergyHitBTOW;
};

struct TPionParameters {
	Float_t m;
	Float_t pT;
	Float_t pTgammas;
	Float_t energy;
	Float_t eta;
	Float_t phi;
	Float_t etaCoord;
	Float_t phiCoord;
	Float_t openangle;
	Float_t openangleReco;
	Float_t asymetry;
	Float_t egamma;
	Int_t towerId;
	Float_t pointEtaCoord;
	Float_t jetDeltaEta;
	Float_t jetDeltaPhi;
	Float_t jetDist;
	Float_t jetBackDeltaEta;
	Float_t jetBackDeltaPhi;
	Float_t jetBackDist;
};

struct TGammaParameters {
	Float_t energy;
	Float_t pT;
	Float_t eta;
	Float_t phi;
	Float_t etaCoord;
	Float_t phiCoord;
	Int_t towerId;
	Float_t pointEtaCoord;
	Float_t jetDeltaEta;
	Float_t jetDeltaPhi;
	Float_t jetDist;
	Float_t jetBackDeltaEta;
	Float_t jetBackDeltaPhi;
	Float_t jetBackDist;
	Float_t associatedPointDeta;
	Float_t associatedPointDphi;
	Float_t distAssociated;
	TPointParameters associatedPoint;
};

struct TEventParameters {
	Bool_t isMB;
	Bool_t isHT1;
	Bool_t isHT2;
	Bool_t isHT1Simulated;
	Bool_t isHT2Simulated;
	Bool_t isHT1SimulatedEt;
	Bool_t isHT2SimulatedEt;
	THitParameters highestEtHit;
	Int_t bunchCrossingId7bitOffset;
	Int_t bunchCrossingId7bitPlusOffset;
	Int_t year;
	Int_t day;
	Int_t runDay;
	Int_t uncorrectedNumberOfFtpcPrimaries;
	Float_t clustersPerTrack;
	Float_t TPCPtToEMCEt;
	Float_t zTPC;
	Float_t zBBC;
	Float_t zUse;
	Float_t jetBackEta;
	Float_t jetBackPhi;
};

Float_t getMinimumOpenangle(Float_t m, Float_t energy);

void getCandidateParams(const TMyCandidateTreeData &candidate
    , const TMyEventData &event1, const TEventParameters &event1Parameters
    , const TMyPointData &point1, const TPointParameters &point1Parameters
    , const TMyEventData &event2, const TEventParameters &event2Parameters
    , const TMyPointData &point2, const TPointParameters &point2Parameters
    , const TCutParameters &cutParameters, TCandidateParameters &candidateParameters);
void getHitParams(const TMyEventData &event, const TMyHitData &hit, const TEventParameters &eventParameters, const TCutParameters &cutParameters, THitParameters &hitParameters);
void getPointParams(const TMyEventData &event, const TMyPointData &point, const TEventParameters &eventParameters, const TCutParameters &cutParameters, TPointParameters &pointParameters);
void getPionParams(const TMyEventData &event, const TMySimulatedDecayData &pion, const TEventParameters &eventParameters, const TGammaParameters &gamma1Parameters, const TGammaParameters &gamma2Parameters, const TCutParameters &cutParameters, TPionParameters &pionParameters);
void getGammaParams(const TMyEventData &event, const TMySimulatedParticleData &gamma, const TEventParameters &eventParameters, const TCutParameters &cutParameters, TGammaParameters &gammaParameters);
void getEventParams(const TMyEventData &event, const TCutParameters &cutParameters, TEventParameters &eventParameters);

Bool_t isBadRun_default(Int_t runId, Int_t year, Int_t day, Int_t runday);
Bool_t isBadRun_embeddingonly(Int_t runId, Int_t year, Int_t day, Int_t runday);
Bool_t isBadRun_normal(Int_t runId, Int_t year, Int_t day, Int_t runday);
Bool_t isBadRun_strict(Int_t runId, Int_t year, Int_t day, Int_t runday);
Bool_t isBadRun_verystrict(Int_t runId, Int_t year, Int_t day, Int_t runday);

Bool_t isBadRun_beambg(Int_t runId, Int_t year, Int_t day, Int_t runday);

Bool_t isGoodFtpcRun(Int_t runId, Int_t year, Int_t day, Int_t runday);

Bool_t isBadRun_bunchCrossingId7bit(Int_t runId, Int_t year, Int_t day, Int_t runday);

Bool_t isGoodPP2005Run(Int_t runId, Int_t year, Int_t day, Int_t runday);
Bool_t isGoodPP2005Run_ppProductionMinBias(Int_t runId, Int_t year, Int_t day, Int_t runday);
Bool_t isBadPP2005Run(Int_t runId, Int_t year, Int_t day, Int_t runday);

Bool_t isBadRun(Int_t parameter, Int_t runId, Int_t year, Int_t day, Int_t runday);

typedef list<pair<Int_t, Int_t> > event_list_type;
Bool_t isBadEvent(Int_t runId, Int_t eventId, const Char_t *badEventsListFilename);
void readEventListFromFile(const Char_t *filename, event_list_type *eventList);
void writeEventListToFile(const Char_t *filename, const event_list_type *eventList);

void parseRunId(Int_t runId, Int_t &year, Int_t &day, Int_t &runDay);

Bool_t isGoodBunchCrossingId7bitPlusOffset(Int_t runId, Int_t year, Int_t day, Int_t runDay, Int_t bunchCrossingId7bitPlusOffset, Bool_t checkAbortGaps, Bool_t checkEmptyBuckets);

TObject *swap(TObject* oldObject, const TObject *newObject);

TH1 *addHistogram(TH1 *oldHistogram, const TH1 *newHistogram);

#define MEMBER_DATA_DEF(class, name, prefix) \
	protected: \
		class prefix##name; \
	public: \
		virtual const class &get##name() const {return prefix##name;} \
		virtual class &get##name() {return prefix##name;} \
		virtual void set##name(const class &newdata) {prefix##name = newdata;}

#define MEMBER_DEF(class, name) MEMBER_DATA_DEF(class, name, m)

#define HISTO_DATA_DEF(class, name, prefix, title) \
	protected: \
		class *prefix##name; \
	public: \
		virtual const class *get##name() const {return prefix##name;} \
		virtual void set##name(const class *newdata) { \
			prefix##name = dynamic_cast<class*>(swap(prefix##name, newdata)); \
			if (prefix##name) { \
				if (prefix##name->GetSumw2N() == 0) prefix##name->Sumw2(); \
				prefix##name->SetName(#name); \
				if (title) prefix##name->SetTitle(title); \
			}\
		} \
		virtual void set##name(const class &newdata) {set##name(&newdata);}

#define HISTO_DEF(class, name, title) HISTO_DATA_DEF(class, name, hist, title)

#define HISTO_INIT(name) this->hist##name = 0;

#define HISTO_DELETE(name) if (this->hist##name) {delete this->hist##name;} this->hist##name = 0;

#define MEMBER_DATA_SET(source, name) this->set##name(source.get##name());
#define MEMBER_SET(source, name) MEMBER_DATA_SET(source, name)
#define HISTO_SET(source, name) MEMBER_DATA_SET(source, name)

#define HISTO_ADD(source, class, name) \
	this->hist##name = dynamic_cast<class*>(addHistogram(this->hist##name, (const TH1 *)source.get##name()));

Int_t getBunchCrossingId7bitOffset(Int_t runId, Bool_t print, const Char_t *bunchCrossingId7bitFilename);

#define USE_FLOAT_COMPARE_PRECISION

class StPi0AnalysisUtil {public: Int_t i;}; // To make RootCint happy

#endif
