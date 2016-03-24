#ifndef VecBosEvent_h
#define VecBosEvent_h

#include "TBuffer.h"
#include "TH1.h"
#include "TLorentzVector.h"
#include "TVector3.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

#include "StSpinPool/StJets/StJets.h"
#include "StSpinPool/StSpinDbMaker/cstructs/spinConstDB.hh"
#include "tables/St_g2t_tpc_hit_Table.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcVertex.hh"
#include "StMcEvent/StMcTrack.hh"

#include "Globals.h"
#include "VecBosEventInfo.h"
#include "VecBosMcEvent.h"
#include "VecBosJet.h"
#include "VecBosTrack.h"
#include "VecBosVertex.h"

#include "utils/ProtoEvent.h"

class StJet;


/**
 * This class describes the general data structure of events saved in the output ROOT tree. The functionality should be
 * common for both W and Z boson events.
 */
class VecBosEvent : public ProtoEvent
{
public:

   int                     l2bitET;
   int                     l2bitRnd;
   int                     l2EbitET;
   int                     l2EbitRnd;
   int                     trigAwaySum[16];   // for lumi
   int                     trigTotalSum;      // for lumi
   bool                    fIsMc;             ///< Flag to process this event as MC or Data
   int                     fEventId;          // event id
   int                     fRunId;
   int                     time;
   float                   zdcRate;
   int                     bx7;               //!
   int                     bx48;              //! raw from muDst
   int                     bxStar7;           //!
   int                     bxStar48;          //!
   int                     mSpinPattern4Bits; // using spinDb or -1 if failed
   int                     mSpinDirection;    // use spinDbEnum to interpret the value
   int                     mNumRecoilTracksTpc;
   bool                    zTag;
   Double_t                mCpuTimeEventAna;
   Double_t                mCpuTimeHistFill;
   UShort_t                mMuDstNumGTracks;
   UShort_t                mMuDstNumVertices;
   UShort_t                mMuDstNumPTracks;
   UShort_t                mMuDstNumOTracks;
   UShort_t                mNumGoodVertices;
   UShort_t                mNumGoodTracks;
   UShort_t                mNumBTracks;
   UShort_t                mNumETracks;
   UShort_t                mNumWithClusterTracks;
   UShort_t                mNumIsolatedTracks;
   StJets                 *mStJets;                     //!
   StJets                 *mStJetsNoEndcap;             //!< jets noEEMC
   DetEventBemc            bemc;                        ///< persistent data structure to hold hits in the barrel calorimeter. Used in eventdisplay
   DetEventEtow            etow;                        //!< transient data structure to hold hits in the endcap calorimeter
   DetEventEprs            eprs;                        //!< transient data structure to hold hits in the endcap preshower
   DetEventEsmd            esmd;                        //!< transient data structure to hold hits in the endcap shower max detector
   VecBosJetPtrSet         mJets;                       ///< This container owns all jets in the event
   VecBosJetPtrSet         mJetsRecoil;                 //
   VecBosJetPtrSet         mJetsWithIsoTrack;           //
   VecBosVertexPtrSet      mVertices;                   ///
   VecBosTrackPtrSet       mTracks;                     ///< This container owns all tracks in the event
   VecBosCandTrackPtrSet   mTracksCandidate;            ///< Container with pointers to candidate tracks sorted by cluster energy
     //VecBosCandOldIsoPtrSet  mTracksCandidateOldIsolation;///< Container with pointers to candidate tracks sorted by cluster energy (old isolation) TEST
   VecBosCandTrackPtrSet   mTracksCandidateNoEndcap;    //!< Container with pointers to candidate tracks sorted by cluster energy (no ETow)
   VecBosMcEvent          *mMcEvent;                    ///< Pointer to pure MC event
   TLorentzVector          mP4JetTotal;
   TLorentzVector          mP4JetFirst;
   TLorentzVector          mP4JetRecoil;
   TVector3                mP3TrackRecoilTpc;           ///< Vector sum of primary tracks, i.e. tracks coming from the primary vertex, except the lepton candidate track
   TVector3                mP3TrackRecoilTow;           ///< Vector sum of towers associated with primary tracks
   TVector3                mP3TrackRecoilNeutrals;      ///< Vector sum of all towers without matching track
   TVector3                mP3TrackRecoilTpcNeutrals;   ///< Vector sum of primary tracks and all towers
   TVector3                mP3TrackRecoilTpcNeutralsCorrected;   ///< Corrected vector sum of primary tracks and all towers
   Float_t                 mMinVertexDeltaZ;            ///< Minimum distance along z between vertices
   TVector3                mP3BalanceFromTracks;
   TVector3                mP3BalanceFromTracks2;
   TVector3                mP3BalanceFromJets;
   Double_t                mPtBalanceFromTracks;
   Double_t                mBalanceDeltaPhiFromTracks;
   Double_t                mBalanceDeltaPhiFromTracks2;
   Double_t                mBalanceDeltaPhiFromJets;
   Double_t                mPtBalanceCosPhiFromTracks;
   Double_t                mPtBalanceCosPhiFromTracks2;
   Double_t                mPtBalanceCosPhiFromJets;
   float                   mLumiEff;
   Double_t                An_evol_ZK;
   Double_t                An_noevo_ZK;

   static const float  sMinTrackIsoDeltaR;    //!< (rad) near-cone size
   static const float  sMinTrackIsoDeltaPhi;  //!< (rad) away-'cone' size, approx. 40 deg.
   static const float  sMaxVertexJetDeltaZ;   //!< distance between jet and vertex z coord, cm
   static const float  sMaxTrackJetDeltaZ;    //!< distance between jet and track z coord, cm
   static const float  sMinBTrackPt;          //!
   static float  sMinRecoilTrackPt;           //!< Minimum P_T of a single track (cluster) in the recoil. 0 obviously means no minimum
   static int  sRhicRunId;                    //!< RHIC run 2011  
   static const float  sMinTrackHitFrac;      //!
   static const float  sMinClusterEnergyFrac; //!
   static const float  sMaxJetCone;           //!

   VecBosEvent();
   virtual ~VecBosEvent();

   void           InitUsing(StMuDstMaker* stMuDstMaker);
   VecBosVertex*  AddVertex(StMuPrimaryVertex &stMuVertex);
   void           AddVertex(VecBosVertex *vbVertex);
   void           AddTrack(StMuTrack *stMuTrack, VecBosVertex *vbVertex = 0);
   void           AddStJets(StJets *stJets, StJets *stJetsNoEndcap);
   TClonesArray*  GetStJets();
   TClonesArray*  GetStJetsNoEndcap();
   bool           IsMc()                      const { return fIsMc; }
   UInt_t         GetEventId()                const { return fEventId; }
   UInt_t         GetRunId()                  const { return fRunId; }
   UInt_t         GetNumStJets();
   UInt_t         GetNumStJetsNoEndcap();
   UInt_t         GetNumJets()                const { return mJets.size(); }
   UInt_t         GetNumJetsRecoil()          const { return mJetsRecoil.size(); }
   UShort_t       GetNumJetsWithIsoTrack()    const { return mJetsWithIsoTrack.size(); }
   UShort_t       GetNumVertices()            const { return mVertices.size(); }
   UShort_t       GetNumTracks()              const { return mTracks.size(); }
   UShort_t       GetNumGoodVertices()        const { return mNumGoodVertices; }
   UShort_t       GetNumGoodTracks()          const { return mNumGoodTracks; }
   UShort_t       GetNumBTracks()             const { return mNumBTracks; }
   UShort_t       GetNumETracks()             const { return mNumETracks; }
   UShort_t       GetNumWithClusterTracks()   const { return mNumWithClusterTracks; }
   UShort_t       GetNumIsolatedTracks()      const { return mNumIsolatedTracks; }
   UShort_t       GetNumCandidateTracks()     const { return mTracksCandidate.size(); }
   UInt_t         GetNumTracksWithBCluster();
   TLorentzVector GetJetRecoil()              const { return mP4JetRecoil; }
   TVector3       GetTrackRecoil()            const { return mP3TrackRecoilTow; }
   TVector3       GetTrackRecoilNeutrals()    const { return mP3TrackRecoilNeutrals; }
   TVector3       GetTrackRecoilTpcNeutrals() const { return mP3TrackRecoilTpcNeutrals; }
   TVector3       GetTrackRecoilTpcNeutralsCorrected() const { return mP3TrackRecoilTpcNeutralsCorrected; }
   TVector3       GetRecoilCorrected() const { return mP3TrackRecoilTpcNeutralsCorrected; }
   VecBosVertex*  FindVertexById(const Short_t vertexId) const;
   VecBosTrack*   FindTrackById(const Short_t trackId) const;
   TVector3       CalcTrackRecoilTpcNeutralsCorrected();
   TVector3       CalcRecoilCorrected();
   TVector3       CalcRecoilCorrected_Wminus();
   void           SetCpuTimeEventAna(Double_t time) { mCpuTimeEventAna = time; }
   void           SetCpuTimeHistFill(Double_t time) { mCpuTimeHistFill = time; }
   bool           HasGoodVertex()             const { return mNumGoodVertices    > 0 ? true : false; } // Checks if at least one good vertex exist in the event
   bool           HasGoodTrack()              const { return mNumGoodTracks      > 0 ? true : false; }
   bool           HasIsolatedTrack()          const { return mNumIsolatedTracks  > 0 ? true : false; }
   bool           HasCandidateEle()           const { return mTracksCandidate.size() > 0 ? true : false; }
   bool           HasCandidateEleNoETOW()     const { return mTracksCandidateNoEndcap.size() > 0 ? true : false; }
   bool           HasJetRecoil()              const { return mP4JetRecoil.Mag()  > 0 ? true : false; }

   virtual void   Process();
   virtual void   ProcessPersistent();
   virtual void   ProcessMC(int McType);
   bool           IsRecoilJet(VecBosJet *vbJet) const;
   bool           IsRecoilJetWithZVertexCut(VecBosJet *vbJet) const;
   virtual void   Clear(const Option_t* opt="");
   virtual void   Print(const Option_t* opt="") const;
   void           GetGmt_day_hour(int &yyyymmdd, int &hhmmss) const;

   VecBosCluster  FindMaxBTow2x2(int iEta, int iPhi, float zVert);
   VecBosCluster  FindMaxETow2x1(int iEta, int iPhi, float zVert);
   VecBosCluster  FindMaxETow2x2(int iEta, int iPhi, float zVert);
   VecBosCluster  SumBTowPatch  (int iEta, int iPhi, int Leta, int  Lphi, float zVert);
   VecBosCluster  SumETowPatch  (int iEta, int iPhi, int Leta, int  Lphi, float zVert);
   TVector3       CalcP3InConeBTow(VecBosTrack *vbTrack, UShort_t cone1d2d = 2, Float_t scale = 1);
   TVector3       CalcP3InConeETow(VecBosTrack *vbTrack, UShort_t cone1d2d = 2, Float_t scale = 1);
   TVector3       CalcP3InConeTpc (VecBosTrack *vbTrack, UShort_t cone1d2d = 2, Float_t scale = 1);
   TVector3       CalcP3InConeTpc_noEleP (VecBosTrack *vbTrack, UShort_t cone1d2d = 2, Float_t scale = 1);

protected:

   void     ProcessJets();
   void     CalcRecoilFromTracks();

   StMuDst* mStMuDst;          //!

   ClassDef(VecBosEvent, 3);
};


TBuffer &operator>>(TBuffer &buf, VecBosEvent *&vbEvent);
TBuffer &operator<<(TBuffer &buf, VecBosEvent *&vbEvent);
TBuffer &operator>>(TBuffer &buf, VecBosEvent &vbEvent);
TBuffer &operator<<(TBuffer &buf, VecBosEvent &vbEvent);

#endif
