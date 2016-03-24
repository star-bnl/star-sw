#ifndef VecBosTrack_h
#define VecBosTrack_h

#include <vector>
#include <set>

#include "TObject.h"
#include "TVector3.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StarClassLibrary/StPhysicalHelix.hh"

#include "Globals.h"
#include "CalTower.h"
#include "VecBosCluster.h"


class VecBosEvent;
class VecBosVertex;
class VecBosJet;


/** Describes the track objects in event */
class VecBosTrack : public TObject
{
public:

   enum ETrackType {kUNKNOWN     = 0x00000000,
                    kBAD         = 0x00008000,
                    kGOOD        = 0x00000001,                     // comes from a good vertex
                    kBARREL      = 0x00000010, kENDCAP   = 0x0020, // from reco algo definition
                    kHAS_CLUSTER = 0x00000100,                     // has cluster above a threshold
                    kISOLATED    = 0x00000200,                     // cluster/cone energy > 0.88
                    kISOLATEDNOETOW = 0x00000300,               // cluster/cone energy > 0.88 (without encaps)
                    kISOLATEDOLD    = 0x00000500,                     // old isolation (TEST)
                    kIN_JET      = 0x00000400,                     // track belongs to a jet
                    kUNBALANCED  = 0x00000800,                     // does not have much energy in opposite direction
                    kHAS_CHARGE  = 0x00001000,                     // the charge of track can be measured
                    kCANDIDATE   = 0x10000000                      // kHAS_CLUSTER && kISOLATED && !kIN_JET && !kBALANCED && Pt > XX GeV
                   };

   VecBosEvent            *mEvent;             //!
   UInt_t                  mVbType;
   CalTower                mMatchedTower;
   const StMuTrack        *glMuTrack;          //!
   StMuTrack              *mStMuTrack;         //
   StPhysicalHelixD        mHelix;             //!
   const VecBosVertex     *mVertex;            //! pointer to mother vertex
   Short_t                 mVertexId;          // mId of the mother vertex
   VecBosJet              *mJet;               //! pointer to mother/closest jet if exists
   VecBosCluster           mCluster1x1;        //! XXX remove !
   VecBosCluster           mCluster2x2;
   VecBosCluster           mCluster4x4;
   TVector3                mP3AtDca;           // primary momentum vector
   TVector3                mP3AtBTow;          // momentum vector
   TVector3                mCoorAtBTow;        //
   TVector3                mP3InNearCone;
   TVector3                mP3InNearConeTow;
   TVector3                mP3InNearConeBTow;
   TVector3                mP3InNearConeETow;
   TVector3                mP3InNearConeNoETow;
   TVector3                mP3InNearConeTpc;
   TVector3                mP3InNearConeTpc_noEleP;
   TVector3                mP3InNearCone_noEleP;
   TVector3                mP3InNearConeNoETow_noEleP;
   TVector3                mP3InOppsCone;
   TVector3                mP3InOppsConeTow;
   TVector3                mP3InOppsConeBTow;
   TVector3                mP3InOppsConeETow;
   TVector3                mP3InOppsConeNoETow;
   TVector3                mP3InOppsConeTpc;
   float                   smallNearTpcPT;     //! GeV/c, around prim track direction
   float                   awayTpcPT;          //!
   float                   awayEmcET;          //!
   float                   awayBtowET;         //!
   float                   awayEtowET;         //! GeV/c, opposite in phi to prim track direction
   float                   awayTotET;          //!
   float                   awayTotET_noEEMC;   //! GeV, for nearCone 10 GeV is subtracted to avoid double counting
   UShort_t                mNumTracksInNearCone;
   TVector3                hadronicRecoil;     //!
   TVector3                ptBalance;          //!
   TVector3                ptBalance_noEEMC;   //!
   float                   sPtBalance;         //!
   float                   sPtBalance_noEEMC;  //! signed pT balance (GeV/c)
   float                   mMinDeltaZToJet;    // Min distance to a jet
   float                   mMinDeltaRToJet;    // Min distance to a jet
   TVector3                mDistToCluster;     // Distance to cluster

   // esmd shower info
   int                     hitSector;                       //!
   int                     esmdGlobStrip[mxEsmdPlane];      //!
   float                   esmdShower[mxEsmdPlane][41];     //!
   float                   esmdDca[mxEsmdPlane];            //!
   float                   esmdDcaGlob[mxEsmdPlane];        //!
   float                   esmdE[mxEsmdPlane];              //!
   int                     esmdNhit[mxEsmdPlane];           //!
   float                   esmdShowerCentroid[mxEsmdPlane]; //!
   float                   esmdShowerWidth[mxEsmdPlane];    //!
   TVector3                esmdXPcentroid;                  //!

   static const float      sMaxTrackClusterDist;            //! cm, dist between projected track and center of cluster
   static const float      sMinPt;                          //!
   static const float      sMaxEnergyInOppsCone;            //!
   static const float      sMinCandidateTrackClusterE;      //!

   VecBosTrack();
   ~VecBosTrack();

   bool         IsGood()       const { return (mVbType & kGOOD)        == kGOOD        ? true : false; }
   bool         IsBTrack()     const { return (mVbType & kBARREL)      == kBARREL      ? true : false; }
   bool         IsETrack()     const { return (mVbType & kENDCAP)      == kENDCAP      ? true : false; }
   bool         HasCluster()   const { return (mVbType & kHAS_CLUSTER) == kHAS_CLUSTER ? true : false; }
   bool         IsIsolated()   const { return (mVbType & kISOLATED)    == kISOLATED    ? true : false; }
   //bool         IsIsolatedOld()   const { return (mVbType & kISOLATEDOLD)    == kISOLATEDOLD    ? true : false; }
   bool         IsIsolatedNoEndcap() const { return (mVbType & kISOLATEDNOETOW) == kISOLATEDNOETOW  ? true : false; }
   bool         IsInJet()      const { return (mVbType & kIN_JET)      == kIN_JET      ? true : false; }
   bool         IsUnBalanced() const { return (mVbType & kUNBALANCED)  == kUNBALANCED  ? true : false; }
   bool         HasCharge()    const { return (mVbType & kHAS_CHARGE)  == kHAS_CHARGE  ? true : false; }
   bool         IsCandidate()  const;
   //bool         IsCandidateOldIsolation()  const;
   bool         IsCandidateNoEndcap()  const;
   bool         IsZelectronCandidate()  const;
   void         Process();
   short        GetChargeSign()         const { return mStMuTrack->charge(); }
   TVector3     GetP3AtDca()            const { return mP3AtDca; }
   TVector3     GetP3EScaled()          const { return mP3AtDca * ((Double_t) mCluster2x2.mEnergy / mP3AtDca.Mag()); }
   float        GetFitHitFrac()         const { return float(mStMuTrack->nHitsFit()) / mStMuTrack->nHitsPoss(); }

   float        GetClusterEnergyFrac()  const { return (mCluster2x2.mEnergy + mP3AtDca.Mag()) / mP3InNearConeNoETow.Mag(); } // as for the DIS 2014 preliminary results  
   //float        GetClusterEnergyFrac()         const { return (mCluster2x2.mEnergy) / mP3InNearConeNoETow_noEleP.Mag(); } // TEST
   float        GetClusterEnergyFrac_noEleP()  const { return (mCluster2x2.mEnergy) / mP3InNearConeNoETow_noEleP.Mag(); }

   //  s.f. Feb.19, 2015 - Lets use the encap to improve the isolation criterium 
   //  (at least on one side of the detector) for electrons at large eta
   //float        GetClusterEnergyFrac()  const { return (mCluster2x2.mEnergy + mP3AtDca.Mag()) / mP3InNearCone.Mag(); } // s.f. Feb.19, 2015
   float        GetClusterEnergyFracNoEndcap()  const { return (mCluster2x2.mEnergy + mP3AtDca.Mag()) / mP3InNearConeNoETow.Mag(); }

   float        GetClusterETFrac()      const { return (mCluster2x2.ET      + mP3AtDca.Pt())  / mP3InNearConeNoETow.Perp(); }
   TVector3     GetDistanceToCluster()  const { return mDistToCluster; }
   TVector3     CalcDistanceToCluster() const { return mCoorAtBTow - mCluster2x2.position; }
   TVector3     GetCoordAtBTow()        const { return mCoorAtBTow; }
   Short_t      GetVertexId()           const { return mVertexId; }
   void         SetVertexId(Short_t vId) { mVertexId = vId; }
   VecBosJet*   FindClosestJet();
   virtual void Clear(const Option_t* opt="");
   virtual void Print(const Option_t* opt="") const;

   void CheckChargeSeparation(); // move to private when VecBosEvent streamer is fixed

private:

   bool ExtendTrack2Barrel();
   bool ExtendTrack2Endcap();
   bool MatchTrack2BarrelCluster();
   void CalcEnergyInCones();       // Isolation and such

   ClassDef(VecBosTrack, 1);
};


inline bool operator==(const VecBosTrack& lhs, const VecBosTrack& rhs) { return (TVector3) lhs.mP3AtDca == (TVector3) rhs.mP3AtDca; }
inline bool operator!=(const VecBosTrack& lhs, const VecBosTrack& rhs) { return !operator==(lhs,rhs); }
inline bool operator< (const VecBosTrack& lhs, const VecBosTrack& rhs) { return lhs.mP3AtDca.Mag() < rhs.mP3AtDca.Mag(); }
inline bool operator> (const VecBosTrack& lhs, const VecBosTrack& rhs) { return  operator< (rhs,lhs); }
inline bool operator<=(const VecBosTrack& lhs, const VecBosTrack& rhs) { return !operator> (lhs,rhs); }
inline bool operator>=(const VecBosTrack& lhs, const VecBosTrack& rhs) { return !operator< (lhs,rhs); }


struct CompareVecBosTrack
{
   bool operator()(const VecBosTrack& lhs, const VecBosTrack& rhs) const { return lhs > rhs; }
};


struct CompareVecBosTrackPtr
{
   bool operator()(const VecBosTrack* lhs, const VecBosTrack* rhs) const { return (*lhs) > (*rhs); }
};


/**
 * Sorting based on the total cluster energy. Therefore valid only for tracks
 * with cluster, i.e. electron candidates.
 */
struct CompareVecBosCandTrackPtr
{
   bool operator()(const VecBosTrack* lhs, const VecBosTrack* rhs) const
   {
      return lhs->GetP3EScaled().Mag() > rhs->GetP3EScaled().Mag();
   }
};

// TEST:
//struct CompareVecBosCandOldIsoPtr
//{
//   bool operator()(const VecBosTrack* lhs, const VecBosTrack* rhs) const
//   {
//      return lhs->GetP3EScaled().Mag() > rhs->GetP3EScaled().Mag();
//   }
//};


typedef std::vector<VecBosTrack>                          VecBosTrackVec;
typedef VecBosTrackVec::iterator                          VecBosTrackVecIter;
typedef VecBosTrackVec::const_iterator                    VecBosTrackVecConstIter;

typedef std::vector<VecBosTrack*>                         VecBosTrackPtrVec;
typedef VecBosTrackPtrVec::iterator                       VecBosTrackPtrVecIter;
typedef VecBosTrackPtrVec::const_iterator                 VecBosTrackPtrVecConstIter;

typedef std::set<VecBosTrack, CompareVecBosTrack>         VecBosTrackSet;
typedef VecBosTrackSet::iterator                          VecBosTrackSetIter;
typedef VecBosTrackSet::const_iterator                    VecBosTrackSetConstIter;

typedef std::set<VecBosTrack*, CompareVecBosTrackPtr>     VecBosTrackPtrSet;
typedef VecBosTrackPtrSet::iterator                       VecBosTrackPtrSetIter;
typedef VecBosTrackPtrSet::const_iterator                 VecBosTrackPtrSetConstIter;

typedef std::set<VecBosTrack*, CompareVecBosCandTrackPtr> VecBosCandTrackPtrSet;
typedef VecBosCandTrackPtrSet::iterator                   VecBosCandTrackPtrSetIter;
typedef VecBosCandTrackPtrSet::const_iterator             VecBosCandTrackPtrSetConstIter;

//typedef std::set<VecBosTrack*, CompareVecBosCandOldIsoPtr> VecBosCandOldIsoPtrSet;
//typedef VecBosCandOldIsoPtrSet::iterator                   VecBosCandOldIsoPtrSetIter;
//typedef VecBosCandOldIsoPtrSet::const_iterator             VecBosCandOldIsoPtrSetConstIter;

#endif
