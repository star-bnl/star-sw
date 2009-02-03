
#undef StPi0DataStructures_STRUCTS_INCL
#ifndef StPi0DataStructures_StPi0DataStructures_H
#define StPi0DataStructures_STRUCTS_INCL
#endif
#ifdef DEFINE_DATA_STRUCTURES
#define StPi0DataStructures_STRUCTS_INCL
#endif

#ifdef StPi0DataStructures_STRUCTS_INCL

#ifndef DEFINE_DATA_STRUCTURES
#define StPi0DataStructures_StPi0DataStructures_H

#include <TObject.h>
#include <TBits.h>
class TFile;
class TTree;
class TH1F;

// Save BPRS information?
//#define SAVE_BPRS

// Save the second most energetic hit in the cluster?
//#define SAVE_CLUSTERHIT2

// Save the jet found by my jet cone algorithm, separately from the one found by the full jet maker?
//#define SAVE_JETMY

#define STPI0DATASTRUCTURES_VERSION 19

class CL {public: Int_t i;}; // To make RootCint happy
class StPi0DataStructures {public: Int_t i;}; // To make RootCint happy

typedef UChar_t triggered_type;

extern const Char_t *mcGammaTreeName;
extern const Char_t *mcGammaTreePlainName;
extern const Char_t *mcGammaBranchName;
extern const Char_t *mcGammaDatasetName;

extern const Char_t *mcPionTreeName;
extern const Char_t *mcPionTreePlainName;
extern const Char_t *mcPionBranchName;
extern const Char_t *mcPionDatasetName;

extern const Char_t *mcEtaTreeName;
extern const Char_t *mcEtaTreePlainName;
extern const Char_t *mcEtaBranchName;
extern const Char_t *mcEtaDatasetName;

extern const Char_t *mcNbarTreeName;
extern const Char_t *mcNbarTreePlainName;
extern const Char_t *mcNbarBranchName;
extern const Char_t *mcNbarDatasetName;

extern const Char_t *candidateTreeName;
extern const Char_t *candidateTreePlainName;
extern const Char_t *candidateDatasetName;

extern const Char_t *candidateTreeMixName;
extern const Char_t *candidateTreeMixPlainName;
extern const Char_t *candidateMixDatasetName;

extern const Char_t *candidateTreeSubmixName;
extern const Char_t *candidateTreeSubmixPlainName;
extern const Char_t *candidateBranchName;
extern const Char_t *candidateSubmixDatasetName;

extern const Char_t *eventTreeName;
extern const Char_t *eventTreePlainName;
extern const Char_t *eventBranchName;
extern const Char_t *eventDatasetName;

extern const Char_t *pointTreeName;
extern const Char_t *pointTreePlainName;
extern const Char_t *pointBranchName;
extern const Char_t *pointDatasetName;

extern const Char_t *clusterTreeName;
extern const Char_t *clusterTreePlainName;
extern const Char_t *clusterBranchName;
extern const Char_t *clusterDatasetName;

extern const Char_t *hitTreeName;
extern const Char_t *hitTreePlainName;
extern const Char_t *hitBranchName;
extern const Char_t *hitDatasetName;

extern const Char_t *smdThresholdTreeName;
extern const Char_t *smdThresholdTreePlainName;
extern const Char_t *smdThresholdBranchName;
extern const Char_t *smdThresholdDatasetName;

extern const Char_t *triggerSummaryName;
extern const Int_t triggerSummaryNbins;
extern const Float_t triggerSummaryMin;
extern const Float_t triggerSummaryMax;
extern const Char_t *eventSummaryName;
extern const Int_t eventSummaryNbins;
extern const Float_t eventSummaryMin;
extern const Float_t eventSummaryMax;

const Char_t *getBranchType(const Char_t *name);

TTree *createTree(TFile *file, const Char_t *name, const Char_t *title, const Char_t *branch, const Char_t *type, void *address, const Int_t maxVirtualSize = 5*1024*1024, const Int_t basketSize = 32000, const Int_t splitLevel = 99);
TH1F *createH1F(TFile *file, const Char_t *name, const Char_t *title, Int_t nbins, Float_t min, Float_t max);

#define DATASTRUCTURE_BEGIN(CL) \
class CL { \
public: \
    typedef CL this_type;
#define DATASTRUCTURE_TREE_BEGIN(CL) \
class CL : public TObject { \
public: \
    typedef TObject inherited; \
    typedef CL this_type;
#define DATA_DEF(TYPE, NAME, TITLE) \
    TYPE NAME;
#define DATASTRUCTURE_END(CL, VALID_CONDITION) \
    CL(Int_t i = 0); \
    CL(const this_type &data); \
    virtual ~CL(); \
    void Copy(void *objTo) const; \
    this_type &operator=(const this_type &data); \
    Bool_t isValid() const; \
    ClassDef(CL, STPI0DATASTRUCTURES_VERSION); \
};
#define DATASTRUCTURE_TREE_END(CL, VALID_CONDITION) \
    CL(Int_t i = 0); \
    CL(const this_type &data); \
    virtual ~CL(); \
    virtual void Copy(TObject &objTo) const; \
    this_type &operator=(const this_type &data); \
    Bool_t isValid() const; \
    ClassDef(CL, STPI0DATASTRUCTURES_VERSION); \
};

#endif

DATASTRUCTURE_BEGIN(TMyTriggerData)
DATA_DEF(triggered_type, triggered, "Satisfied triggers bit mask, (up to sizeof(triggered_type)*8 satisfied triggers))")
DATASTRUCTURE_END(TMyTriggerData, true)

DATASTRUCTURE_BEGIN(TMyHitData)
DATA_DEF(Byte_t, detector, "Detector ID (kBarrelEmcTowerId, kBarrelEmcPreShowerId, kBarrelSmdEtaStripId or kBarrelSmdPhiStripId, zero is not allowed)")
DATA_DEF(UShort_t, id, "Hit SoftId (counting from 1)")
DATA_DEF(UShort_t, adc, "Raw ADC, counts")
DATA_DEF(UShort_t, pedestal100, "Pedestal * 100, ADC counts")
DATA_DEF(UShort_t, pedestalRMS100, "Pedestal RMS * 100, ADC counts")
DATA_DEF(Float_t, etaphiCoord, "Eta and phi coordinate within the detector ([(eta + 1.0) * 100000.0] + ((phi + 3.5) / 7.0))")
DATA_DEF(Float_t, energy, "Calibrated hit energy, GeV")
DATASTRUCTURE_END(TMyHitData, detector && id)

DATASTRUCTURE_BEGIN(TMyTriggerSimulatedData)
DATA_DEF(TMyTriggerData, trigger, "Satisfied simulated triggers")
DATA_DEF(TMyHitData, highestAdcHit, "Highest ADC hit in the event")
DATA_DEF(TMyHitData, highestEtHit, "Highest E_{T} hit in the event")
DATASTRUCTURE_END(TMyTriggerSimulatedData, /*highestAdcHit.isValid()*/true)

DATASTRUCTURE_BEGIN(TMySimulatedParticleSummaryData)
DATA_DEF(Byte_t, geantId, "Particle GEANT Id (in PYTHIA - GEANT subprocess ID)")
DATA_DEF(Byte_t, daughters, "Decay products, total + (photons << 4) + (pi0 << 6), in PYTHIA - StMcEvent::subProcessId()")
DATA_DEF(Float_t, z, "Z of origin along the beam line, cm")
DATA_DEF(Float_t, pT, "Simulated p_{T} (or PYTHIA partonic pT), GeV/c")
DATASTRUCTURE_END(TMySimulatedParticleSummaryData, geantId)

DATASTRUCTURE_BEGIN(TMyJetData)
DATA_DEF(Float_t, eta, "Jet eta coordinate w.r.t. the vertex, pseudorapidity")
DATA_DEF(Float_t, phi, "Jet phi coordinate w.r.t. the vertex, rad")
DATA_DEF(Float_t, eT, "Jet E_{T} (>0 for real jet maker, <0 for my jet cone finder), GeV/c")
DATA_DEF(UShort_t, nTracks, "Number of primary tracks that belong to the jet")
DATA_DEF(UShort_t, nPoints, "Number of BEMC points that belong to the jet")
DATASTRUCTURE_END(TMyJetData, nTracks || nPoints || (eT != 0))

DATASTRUCTURE_BEGIN(TMyEventData)
DATA_DEF(UInt_t, runId, "Run Id")
DATA_DEF(UInt_t, eventId, "Event Id")
DATA_DEF(UShort_t, nPrimary, "Number of primary tracks in the event")
DATA_DEF(UShort_t, nGlobal, "Number of global tracks in the event")
DATA_DEF(UShort_t, nPoints, "Number of BEMC points in the event")
DATA_DEF(UShort_t, nClustersBTOW, "Number of BTOW clusters in the event")
#ifdef SAVE_BPRS
DATA_DEF(UShort_t, nClustersBPRS, "Number of BPRS clusters in the event")
#endif
DATA_DEF(UShort_t, nClustersBSMDE, "Number if BSMDE clusters in the event")
DATA_DEF(UShort_t, nClustersBSMDP, "Number of BSMDP clusters in the event")
DATA_DEF(UShort_t, nHitsBTOW, "Number of BTOW hits in the event")
DATA_DEF(UShort_t, nHitsBTOWstuckbit, "Number of BTOW hits with last ADC bits 100")
#ifdef SAVE_BPRS
DATA_DEF(UShort_t, nHitsBPRS, "Number of BPRS hits in the event")
#endif
DATA_DEF(UShort_t, nHitsBSMDE, "Number of BSMDE hits in the event")
DATA_DEF(UShort_t, nHitsBSMDP, "Number of BSMDP hits in the event")
DATA_DEF(UShort_t, uncorrectedNumberOfFtpcPrimariesEast, "Number of primary FTPC East tracks, uncorrected")
DATA_DEF(UShort_t, uncorrectedNumberOfFtpcPrimariesWest, "Number of primary FTPC West tracks, uncorrected")
DATA_DEF(UShort_t, uncorrectedNumberOfTpcPrimaries, "Number of primary TPC tracks, uncorrected")
DATA_DEF(Byte_t, bunchCrossingId7bit, "7-bit bunch crossing counter value")
DATA_DEF(Byte_t, bunchCrossingId48bit, "48-bit bunch crossing counter value")
DATA_DEF(Byte_t, spinBit, "Spin8 bit")
DATA_DEF(Byte_t, bunchCrossingFrom7bitDB, "Yellow bunch crossing at STAR from spinDb 7")
DATA_DEF(Byte_t, bunchCrossingFrom48bitDB, "Yellow bunch crossing at STAR from spinDb 48")
DATA_DEF(Byte_t, spinBitDBFrom7, "Spin8 bit from starDb 7")
DATA_DEF(Byte_t, spinBitDBFrom48, "Spin8 bit from starDb 48")
DATA_DEF(Byte_t, spinDBStatus, "Event status from spinDb: bit0-isValid, bit1-isTrans, bit2-isLong, bit3-isFilledUsing48, bit4-isMaskedUsing48, bit5-isFilledUsing7, bit6-isFilledUsingYellow, bit7-isMaskedUsingYellow")
DATA_DEF(Short_t, bunchCrossingOffset48bit7bitDB, "Difference between 48-bit and 7-bit offsets from spinDb, must be zero")
DATA_DEF(Float_t, totalBEMCHitsEnergy, "Sum of all BEMC hits calibrated energy, GeV")
DATA_DEF(Float_t, totalBEMCPointsEnergy, "Sum of all BEMC points calibrated energy, GeV")
DATA_DEF(Float_t, totalBEMCPointsEt, "Sum of all BEMC points E_{T}, GeV/c")
DATA_DEF(Float_t, zTPC, "TPC vertex position if found, cm")
DATA_DEF(Float_t, totalTPCPt, "Sum of all TPC primary tracks p_{T}, GeV/c")
DATA_DEF(Byte_t, bbcEarliestEast, "BBC earliest East TDC")
DATA_DEF(Byte_t, bbcEarliestWest, "BBC earliest West TDC")
DATA_DEF(UShort_t, corruptedCrates, "Bitmask of the corrupted BEMC crates data (bits 0-29), and the overall corruption flag (highest bit) as reported by ADCtoE maker")
DATA_DEF(Byte_t, mixerStatus, "Bitmask of the event mixer status when processing this event (bit 0 - event belongs to a class, bit 1 - enough events to mix with, bit 2 - any event to mix with)")
DATA_DEF(UShort_t, acceptanceBTOW, "Number of good towers according to the status table")
#ifdef SAVE_BPRS
DATA_DEF(UShort_t, acceptanceBPRS, "Number of good preshower towers according to the status table")
#endif
DATA_DEF(UShort_t, acceptanceBSMDE, "Number of good BSMDE strips according to the status table")
DATA_DEF(UShort_t, acceptanceBSMDP, "Number of good BSMDE strips according to the status table")
DATA_DEF(TMyTriggerData, trigger, "Satisfied triggers")
DATA_DEF(TMyTriggerSimulatedData, triggerSimulated, "Satisfied software triggers before applying status tables")
DATA_DEF(TMyTriggerSimulatedData, triggerSimulatedEmbed, "Satisfied sofware triggers in the underlying event before embedding")
DATA_DEF(TMyTriggerSimulatedData, triggerSimulatedFinal, "Satisfied software triggers in the final event, after status tables and embedding")
DATA_DEF(TMySimulatedParticleSummaryData, simulatedParticle, "Simulated particle")
#ifdef SAVE_JETMY
DATA_DEF(TMyJetData, jetMy, "Jet from my jet cone algorithm, uses BEMC points only")
#endif
DATA_DEF(TMyJetData, jet, "Jet from the real jet finder, uses BEMC and TPC")
DATASTRUCTURE_END(TMyEventData, runId/* && eventId*/)

DATASTRUCTURE_BEGIN(TMyClusterData)
DATA_DEF(Byte_t, detector, "Detector Id")
DATA_DEF(Byte_t, size, "Number of hits in the cluster")
DATA_DEF(Float_t, energy, "Cluster calibrated energy, GeV")
DATA_DEF(Float_t, etaCoord, "Eta coordinate w.r.t. the nominal IP (0,0,0), pseudorapidity")
DATA_DEF(Float_t, phiCoord, "Phi coordinate w.r.t. the nominal IP (0,0,0), rad")
DATA_DEF(Float_t, sigmaEta, "Eta RMS, pseudorapidity")
DATA_DEF(Float_t, sigmaPhi, "Phi RMS, rad")
DATA_DEF(Byte_t, badClose, "Bitmask, the bad cell is close (bit 0 at lower and 1 at higher eta, bit 2 at lower and 3 at higher phi, bits 4-7 - module border)")
DATA_DEF(TMyHitData, highestADCHit, "Highest ADC hit in the cluster")
DATA_DEF(TMyHitData, highestEnergyHit, "Most energetic hit in the cluster")
#ifdef SAVE_CLUSTERHIT2
DATA_DEF(TMyHitData, highestEnergyHit2, "Second most energetic hit in the cluster")
#endif
DATASTRUCTURE_END(TMyClusterData, detector && size && highestADCHit.isValid())

DATASTRUCTURE_BEGIN(TMyPointData)
DATA_DEF(Float_t, etaCoord, "Eta coordinate w.r.t. the nominal IP (0,0,0), pseudorapidity")
DATA_DEF(Float_t, phiCoord, "Phi coordinate w.r.t. the nominal IP (0,0,0), rad")
DATA_DEF(Float_t, energy, "Point energy (<= clusterBTOW.energy because of sharing), GeV")
DATA_DEF(Float_t, trackDeta, "Distance in eta to the closest primary track projected on the BEMC surface, pseudorapidity")
DATA_DEF(Float_t, trackDphi, "Distance in phi to the closest primary track projected on the BEMC surface, rad")
DATA_DEF(Float_t, trackDeta2, "Distance in eta to the second closest primary track projected on the BEMC surface, pseudorapidity")
DATA_DEF(Float_t, trackDphi2, "Distance in phi to the second closest primary track projected on the BEMC surface, rad")
DATA_DEF(Float_t, trackMCDeta, "Distance in eta to the closest MC track projected on the BEMC surface, pseudorapidity")
DATA_DEF(Float_t, trackMCDphi, "Distance in phi to the closest MC track projected on the BEMC surface, rad")
DATA_DEF(Float_t, towerCenterDeta, "Distance in eta to the closest tower center of all BTOW cluster hits, pseudorapidity")
DATA_DEF(Float_t, towerCenterDphi, "Distance in phi to the closest tower center of all BTOW cluster hits, rad")
DATA_DEF(TMyTriggerData, trigger, "Triggers satisfied by this point alone")
DATA_DEF(TMyClusterData, clusterBTOW, "Tower cluster")
#ifdef SAVE_BPRS
DATA_DEF(TMyClusterData, clusterBPRS, "Preshower cluster")
#endif
DATA_DEF(TMyClusterData, clusterBSMDE, "BSMDE cluster")
DATA_DEF(TMyClusterData, clusterBSMDP, "BSMDP cluster")
DATASTRUCTURE_END(TMyPointData, (energy != 0.0) && clusterBTOW.isValid())

DATASTRUCTURE_BEGIN(TMyCandidateData)
DATA_DEF(Byte_t, pointsMatched, "Bitmask to show if two points are matched (bit 0 - energy, bit 1 - distance to jet axis, bit 2 - closest in energy, bit 3 - closest in distance to jet axis)")
DATASTRUCTURE_END(TMyCandidateData, true)

DATASTRUCTURE_BEGIN(TMySimulatedParticleData)
DATA_DEF(TMySimulatedParticleSummaryData, summary, "Particle summary")
DATA_DEF(Float_t, theta, "Theta angle pointing at the point of origin, rad")
DATA_DEF(Float_t, phi, "Phi angle pointing at the point of origin, rad")
DATA_DEF(Float_t, phiCoord, "Phi coordinate at the point of origin, rad")
DATA_DEF(Float_t, startRadius, "Radius at which the MC track starts (direct or decay product), cm")
DATA_DEF(Float_t, stopRadius, "Radius at which the MC track stops (photon conversion or shower), cm")
DATA_DEF(TMyPointData, associatedPoint, "Associated BEMC point")
DATASTRUCTURE_END(TMySimulatedParticleData, summary.isValid())

DATASTRUCTURE_BEGIN(TMySimulatedDecayData)
DATA_DEF(TMySimulatedParticleData, parent, "Parent particle data")
DATA_DEF(TMySimulatedParticleData, daughter1, "Decay product 1")
DATA_DEF(TMySimulatedParticleData, daughter2, "Decay product 2")
DATASTRUCTURE_END(TMySimulatedDecayData, parent.isValid())

DATASTRUCTURE_BEGIN(TMySMDThresholdData)
DATA_DEF(Float_t, threshold, "BSMD strip energy threshold (!= 0.0), GeV")
DATA_DEF(TBits, channelsAboveThreshold, "Bitmask of strips with energy above threshold")
DATASTRUCTURE_END(TMySMDThresholdData, threshold != 0.0)

DATASTRUCTURE_TREE_BEGIN(TMyMCParticleTreeData)
DATA_DEF(TMyEventData, event, "Event data structure")
DATA_DEF(TMySimulatedParticleData, particle, "Simulated photon structure")
DATASTRUCTURE_TREE_END(TMyMCParticleTreeData, event.isValid() && particle.isValid())

DATASTRUCTURE_TREE_BEGIN(TMyMCDecayTreeData)
DATA_DEF(TMyEventData, event, "Event data structure")
DATA_DEF(TMySimulatedDecayData, decay, "Simulated decay structure")
DATASTRUCTURE_TREE_END(TMyMCDecayTreeData, event.isValid() && decay.isValid())

DATASTRUCTURE_TREE_BEGIN(TMyEventTreeData)
DATA_DEF(TMyEventData, event, "Event data structure")
DATASTRUCTURE_TREE_END(TMyEventTreeData, event.isValid())

DATASTRUCTURE_TREE_BEGIN(TMyPointTreeData)
DATA_DEF(TMyEventData, event, "Event data structure")
DATA_DEF(TMyPointData, point, "BEMC Point structure")
DATASTRUCTURE_TREE_END(TMyPointTreeData, event.isValid() && point.isValid())

DATASTRUCTURE_TREE_BEGIN(TMySMDThresholdTreeData)
DATA_DEF(TMyEventData, event, "Event structure")
DATA_DEF(TMySMDThresholdData, smdEta1, "BSMDE over threshold 1 structure")
DATA_DEF(TMySMDThresholdData, smdPhi1, "BSMDP over threshold 1 structure")
DATA_DEF(TMySMDThresholdData, smdEta2, "BSMDE over threshold 2 structure")
DATA_DEF(TMySMDThresholdData, smdPhi2, "BSMDP over threshold 2 structure")
DATA_DEF(TMySMDThresholdData, smdEta3, "BSMDE over threshold 3 structure")
DATA_DEF(TMySMDThresholdData, smdPhi3, "BSMDP over threshold 3 structure")
DATASTRUCTURE_TREE_END(TMySMDThresholdTreeData, event.isValid() && smdEta1.isValid() && smdPhi1.isValid())

DATASTRUCTURE_TREE_BEGIN(TMyCandidateTreeData)
DATA_DEF(TMyPointTreeData, point1, "First point structure")
DATA_DEF(TMyPointTreeData, point2, "Second point  structure")
DATA_DEF(TMyCandidateData, candidate, "Candidate information structure")
DATASTRUCTURE_TREE_END(TMyCandidateTreeData, point1.isValid() && point2.isValid() && candidate.isValid())

DATASTRUCTURE_TREE_BEGIN(TMyClusterTreeData)
DATA_DEF(TMyEventData, event, "Event data structure")
DATA_DEF(TMyClusterData, cluster, "BEMC cluster structure")
DATASTRUCTURE_TREE_END(TMyClusterTreeData, event.isValid() && cluster.isValid())

DATASTRUCTURE_TREE_BEGIN(TMyHitTreeData)
DATA_DEF(TMyEventData, event, "Event data structure")
DATA_DEF(TMyHitData, hit, "BEMC hit structure")
DATASTRUCTURE_TREE_END(TMyHitTreeData, event.isValid() && hit.isValid())

#ifndef DEFINE_DATA_STRUCTURES

#undef DATASTRUCTURE_BEGIN
#undef DATASTRUCTURE_TREE_BEGIN
#undef DATA_DEF
#undef DATASTRUCTURE_END
#undef DATASTRUCTURE_TREE_END

#endif

#endif
#undef StPi0DataStructures_STRUCTS_INCL
