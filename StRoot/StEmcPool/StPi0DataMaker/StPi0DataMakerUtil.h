#ifndef StPi0DataMaker_Util_H
#define StPi0DataMaker_Util_H

#include <TObject.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

class TMyHitData;
class TMyClusterData;
class TMyPointData;
class TMyTriggerData;
class TMyTriggerSimulatedData;
class TMyEventData;
class TMySimulatedDecayData;
class TMySimulatedParticleData;
class TMySMDThresholdData;
class TMyJetData;

// buggy BBC TDC calculation limited by "year", comment when fixed
#define MYPI0ANALYSIS_OLDBBCMAKER

class TDataSet;
class StEmcRawHit;
class StEmcPoint;
class StEmcCluster;
class StEmcPosition;
class StEvent;
class StMcEvent;
class StMcTrack;
class StEmcGeom;
class StMaker;
class StEmcDetector;
class StEmcCollection;
class StSPtrVecEmcPoint;
class StBemcTables;

Bool_t shuffleArray(Int_t *array, Int_t size, Bool_t init, Int_t numShuffle, Bool_t check);
void associateTracksWithEmcPoints(StEvent *event, StMcEvent *mc_event, StSPtrVecEmcPoint &emcPoints, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, StEmcPosition *emcPosition, StBemcTables *tables);
void findJet(const StSPtrVecEmcPoint &emcPoints, Float_t vZ, Float_t coneSize, Float_t etaSeed, Float_t phiSeed, TMyJetData &jet);

void getHitData(const StEmcRawHit *hit, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, StBemcTables *tables, TMyHitData &hitData);
void getClusterData(const StEmcCluster *cluster, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, StBemcTables *tables, TMyClusterData &clusterData);
void getPointData(const StEmcPoint *point, StMaker *trigMaker, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, StBemcTables *tables, const UInt_t *triggers, TMyPointData &pointData);
void getTriggerData(const StEvent *event, const UInt_t *triggerIDs, TMyTriggerData &triggerData);
void getTriggerDataFromDB(Int_t runId, const UInt_t *triggerIDs, const Char_t *connectString, TMyTriggerData &triggerData);
void getTriggerSimulatedData(const StMaker *trigMaker, const UInt_t *triggers, TMyTriggerSimulatedData &triggerSimulatedData);
void getEventData (const StEvent *event, const StMcEvent *mc_event, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, StBemcTables *tables
	, const UInt_t *triggers, StMaker *trigMaker, StMaker *trigMakerEmbed, StMaker *trigMakerFinal
	, StMaker *adcToEMaker, Float_t jetConeRadius, StMaker *jetMaker, const Char_t *jetBranchName, StMaker *spinDbMaker
	, TDataSet *geantEventDS, Bool_t isPythia, TMyEventData &eventData);
void getMCDecayData(const StMcTrack *mcTrack, const StEvent *event, StBemcTables *tables, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, TMySimulatedDecayData &mcDecayData);
void getMCParticleData(const StMcTrack *mcTrack, const StEvent *event, StBemcTables *tables, StEmcGeom *emcGeom, StEmcGeom *smdeGeom, StEmcGeom *smdpGeom, StEmcGeom *psdGeom, TMySimulatedParticleData &mcParticleData);
void getSMDThresholdData(const StEmcDetector *detector, StEmcGeom *geom, StBemcTables *tables, Float_t threshold, Bool_t thresholdEnergy, Bool_t thresholdEt, TMySMDThresholdData &smdThresholdData);

class StPi0DataMakerUtil {public: Int_t i;}; // To make RootCint happy

#endif
