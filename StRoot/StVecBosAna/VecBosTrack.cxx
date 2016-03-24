
#include "VecBosTrack.h"

#include "VecBosEvent.h"
#include "VecBosVertex.h"
#include "VecBosJet.h"


ClassImp(VecBosTrack)

using namespace std;


const float VecBosTrack::sMaxTrackClusterDist       = 7;  // cm
const float VecBosTrack::sMinPt                     = 20; // GeV
const float VecBosTrack::sMaxEnergyInOppsCone       = 30; // was 40, was 15 GeV
const float VecBosTrack::sMinCandidateTrackClusterE = 25; // GeV


VecBosTrack::VecBosTrack() : TObject(),
   mEvent(0),
   mVbType(kUNKNOWN),
   mMatchedTower(),
   glMuTrack(0),
   mStMuTrack(0),
   mHelix(),
   mVertex(0),
   mVertexId(-1),
   mJet(0),
   mCluster1x1(1),
   mCluster2x2(2),
   mCluster4x4(4),
   mP3AtDca(),
   mP3AtBTow(),
   mCoorAtBTow(),
   mP3InNearCone(), mP3InNearConeTow(), mP3InNearConeBTow(), mP3InNearConeETow(), mP3InNearConeNoETow(), mP3InNearConeTpc(),
   mP3InNearCone_noEleP(),  mP3InNearConeNoETow_noEleP(), mP3InNearConeTpc_noEleP(),
   mP3InOppsCone(), mP3InOppsConeTow(), mP3InOppsConeBTow(), mP3InOppsConeETow(), mP3InOppsConeNoETow(), mP3InOppsConeTpc(),
   smallNearTpcPT  (0),
   awayTpcPT       (0),
   awayEmcET       (0),
   awayBtowET      (0),
   awayEtowET      (0),
   awayTotET       (0),
   awayTotET_noEEMC(0),
   mNumTracksInNearCone(0),
   hadronicRecoil(),
   ptBalance(),
   ptBalance_noEEMC(),
   sPtBalance(0),
   sPtBalance_noEEMC(0),
   mMinDeltaZToJet(-1),            // nonsense value
   mMinDeltaRToJet(-1),            // nonsense value
   mDistToCluster(-10, -10, -10),  // nonsense value
   hitSector(-1),
   esmdXPcentroid()
{
   Clear();
}


VecBosTrack::~VecBosTrack()
{
   //Info("~VecBosTrack()", "mStMuTrack: %x, %d", mStMuTrack, mStMuTrack->nHitsFit());
   //Info("~VecBosTrack()", "this: %x, mStMuTrack: %x", this, mStMuTrack);
   if (mStMuTrack) delete mStMuTrack;
   mStMuTrack = 0;
}


bool VecBosTrack::IsCandidate() const
{
   return (HasCluster() && IsIsolated() && IsUnBalanced() && HasCharge());
}

//---------------------------------------------------------------------------------
// to test the isolation cut vs the old version (as in previous publications):
//bool VecBosTrack::IsCandidateOldIsolation() const
//{
//   return (HasCluster() && IsIsolatedOld() && IsUnBalanced() && HasCharge());
//}
//---------------------------------------------------------------------------------

bool VecBosTrack::IsCandidateNoEndcap() const
{
   return (HasCluster() && IsIsolatedNoEndcap() && IsUnBalanced() && HasCharge());
}


bool VecBosTrack::IsZelectronCandidate() const
{
  //   return (HasCluster() && IsIsolated() && HasCharge());
   return (HasCluster() && HasCharge());
}


void VecBosTrack::Process()
{
   // Good track must come from a good vertex
   if (!mVertex || !mVertex->IsGood()) {
      mVbType = kBAD;
      return;
   }

   mVbType   = kGOOD; // this track has a good vertex
   mVertexId = mVertex->GetId();

   //if (mVecBosEvent->l2bitET && rank > 0 && primaryTrack->flag() == 301)
      //XXX:ds:if (secID == 20) continue; //poorly calibrated sector for Run 9+11+12?
      //XXX:ds:if (mTpcFilter[secID - 1].accept(primaryTrack) == false) continue;
   //if (mVecBosEvent->l2EbitET && ro.pseudoRapidity() > parE_trackEtaMin)
      //XXX:ds:if ( mTpcFilterE[secID - 1].accept(primaryTrack) == false) continue;
   //XXX:ds:if (!barrelTrack && !endcapTrack) continue;

   // Look for high Pt electron candidates
   if ( ExtendTrack2Barrel() )
   {
      mVbType |= kBARREL;

      // Do not proced if the track cannot be extended to barrel
      if ( !MatchTrack2BarrelCluster() ) return;

      CalcEnergyInCones();
      CheckChargeSeparation();
   }
   else if ( ExtendTrack2Endcap() ) {
      mVbType |= kENDCAP;
   }
}


VecBosJet* VecBosTrack::FindClosestJet()
{
   VecBosJet *closestJet = 0;

   // Find the min distance between the track and the closest jet
   VecBosJetPtrSetConstIter iJet = mEvent->mJets.begin();
   for ( ; iJet != mEvent->mJets.end(); ++iJet)
   {
      VecBosJet *vbJet = *iJet;

      Double_t deltaZ = fabs(vbJet->zVertex - mVertex->mPosition.Z());

      // Jet should come out of the same vertex
      if (deltaZ > VecBosEvent::sMaxTrackJetDeltaZ) continue;

      Double_t deltaR = vbJet->Vect().DeltaR(mP3AtDca);
      //Info("FindClosestJet()", "deltaR: %f ", deltaR);

      if (mMinDeltaRToJet < 0 || deltaR < mMinDeltaRToJet) {
         mMinDeltaRToJet = deltaR;
         closestJet = vbJet;
         //Info("FindClosestJet()", "New closest jet found: mMinDeltaRToJet = %f", mMinDeltaRToJet);
      }
   }

   if (closestJet && mMinDeltaRToJet <= VecBosEvent::sMinTrackIsoDeltaR)
   {
      //Info("FindClosestJet()", "Track is within jet: %f <= %f. Returning jet...", mMinDeltaRToJet, VecBosEvent::sMinTrackIsoDeltaR);
      mVbType |= VecBosTrack::kIN_JET;
      mJet = closestJet;
      return closestJet;
   }

   return 0;
}


void VecBosTrack::Clear(const Option_t*)
{
   mEvent                 = 0;
   mVbType                = kUNKNOWN;
   mMatchedTower.Clear();
   glMuTrack              = 0;
   mStMuTrack             = 0;
   //mHelix               = St
   mVertex                = 0;
   mVertexId              = -1;
   mJet                   = 0;
   mCluster1x1.Clear();
   mCluster2x2.Clear();
   mCluster4x4.Clear();
   mP3AtDca.SetXYZ(0, 0, 0);
   mCoorAtBTow.SetXYZ(0, 0, 0);
   mP3InNearCone.SetXYZ(0, 0, 0);
   mP3InNearConeTow.SetXYZ(0, 0, 0);
   mP3InNearConeBTow.SetXYZ(0, 0, 0);
   mP3InNearConeETow.SetXYZ(0, 0, 0);
   mP3InNearConeNoETow.SetXYZ(0, 0, 0);
   mP3InNearConeTpc.SetXYZ(0, 0, 0);
   mP3InNearCone_noEleP.SetXYZ(0, 0, 0);
   mP3InNearConeNoETow_noEleP.SetXYZ(0, 0, 0);
   mP3InNearConeTpc_noEleP.SetXYZ(0, 0, 0);
   mP3InOppsCone.SetXYZ(0, 0, 0);
   mP3InOppsConeTow.SetXYZ(0, 0, 0);
   mP3InOppsConeBTow.SetXYZ(0, 0, 0);
   mP3InOppsConeETow.SetXYZ(0, 0, 0);
   mP3InOppsConeNoETow.SetXYZ(0, 0, 0);
   mP3InOppsConeTpc.SetXYZ(0, 0, 0);
   awayTpcPT              = 0;
   awayEmcET              = 0;
   awayBtowET             = 0;
   awayEtowET             = 0;
   awayTotET              = 0;
   smallNearTpcPT         = 0;
   awayTotET_noEEMC       = 0;
   mNumTracksInNearCone   = 0;
   hadronicRecoil.SetXYZ(0, 0, 0);
   ptBalance.SetXYZ(0, 0, 0);
   ptBalance_noEEMC.SetXYZ(0, 0, 0);
   sPtBalance             = 0;
   sPtBalance_noEEMC      = 0;
   mMinDeltaZToJet        = -1;
   mMinDeltaRToJet        = -1;
   //mDistToCluster         = -1;
   mDistToCluster.SetXYZ(0, 0, 0);

   //hitSector              = -1;
   //esmdXPcentroid.SetXYZ(0, 0, 0);
   memset(esmdGlobStrip, -999, sizeof(esmdGlobStrip));
   memset(esmdShower, -999, sizeof(esmdShower));
   memset(esmdDca, -999., sizeof(esmdDca));
   memset(esmdDcaGlob, -999., sizeof(esmdDcaGlob));
   memset(esmdE, 0., sizeof(esmdE));
   memset(esmdNhit, 0, sizeof(esmdNhit));
   memset(esmdShowerCentroid, 999., sizeof(esmdShowerCentroid));
   memset(esmdShowerWidth, 999., sizeof(esmdShowerWidth));
}


void VecBosTrack::Print(const Option_t* opt) const
{
   if (string(opt).size())
      Info("Print", "opt: %s", opt);

   Double_t mP3AtDca_0to2Pi = mP3AtDca.Phi() < 0 ? 2*M_PI + mP3AtDca.Phi() : mP3AtDca.Phi();

   Info("Print", "mP3AtDca: %.2f, %.2f", mP3AtDca.PseudoRapidity(), mP3AtDca_0to2Pi);
   mP3AtDca.Print();
   Info("Print", "mCoorAtBTow:");
   mCoorAtBTow.Print();
   Info("Print", "mVertexId: %2d", mVertexId);

   //if (!mStMuTrack) { printf("mStMuTrack is NULL pointer???\n"); return; }

   //printf("\tTrack: HasCluster(): %d, mP3InNearCone: %.2f, awayTotET: %.2f mP3AtDca.Pt(): %.2f\n",
   //       HasCluster(), mP3InNearCone.Pt(), awayTotET, mP3AtDca.Pt());

   //mMatchedTower.print(opt);
   mCluster2x2.Print(opt);

   //printf("\tXYZ(track-mCluster2x2):  |3D dist|=%.1fcm  delZ=%.1fcm\n", CalcDistanceToCluster.Mag(), CalcDistanceToCluster.z());
   //printf("\tCluster4x4 :");
   //mCluster4x4.print(opt);
   //printf("\tnearET/GeV:    TPC=%.1f   Emc=%.1f (BTOW=%.1f ETOW=%.1f) sum=%.1f\n",
   //   mP3InNearConeTpc.Pt(), mP3InNearConeTow.Pt(), mP3InNearConeBTow.Pt(), mP3InNearConeETow.Pt(), mP3InNearCone.Pt());
   //printf("\tawayET/GeV:    TPC=%.1f   Emc=%.1f (BTOW=%.1f ETOW=%.1f) sum=%.1f\n",
   //   awayTpcPT, awayEmcET, awayBtowET, awayEtowET, awayTotET);
}


bool VecBosTrack::ExtendTrack2Barrel()
{
   // Initial rejection: XXX The Pt cut may need to be reconsidered: was 1.0 -> 0.2
   if ( mStMuTrack->pt() < 0.200 || mStMuTrack->flag() != 301 )
      return false;

   // Extrapolate track to the barrel @ R=entrance
   mHelix               = mStMuTrack->outerHelix();
   float  nomBTowRadius = gBTowGeom->Radius();
   pairD  segmentLength = mHelix.pathLength(nomBTowRadius); // XXX:ds: Length along the helix from the origin to the intersection point
   //printf(" R=%.1f path 1=%f, 2=%f, period=%f, R=%f\n", Rctb, segmentLength.first, segmentLength.second, mHelix.period(), 1./mHelix.curvature());

   if (fabs(segmentLength.first) > 1e7 || fabs(segmentLength.second) > 1e7) {
      Warning("ExtendTrack2Barrel", "Track cannot be propagated to barrel");
      return false;
   }

   // assert(segmentLength.first  < 0); // propagate backwards
   // assert(segmentLength.second > 0); // propagate forwards
   if (segmentLength.first >= 0 || segmentLength.second <= 0)
   {
      Warning("ExtendTrack2Barrel", "Unexpected solution for track crossing CTB. Swap segments segmentLength.first=%f, segmentLength.second=%f",
            segmentLength.first, segmentLength.second);
      float xx  = segmentLength.first;
      segmentLength.first  = segmentLength.second;
      segmentLength.second = xx;
   }

   // Extrapolate track to cylinder
   StThreeVectorD posAtBTow = mHelix.at(segmentLength.second);
   float etaAtBTow   = posAtBTow.pseudoRapidity();
   float phiAtBTow   = posAtBTow.phi();

   mCoorAtBTow.SetXYZ(posAtBTow.x(), posAtBTow.y(), posAtBTow.z());

   int iEta, iPhi;

   if ( ConvertEtaPhi2Bins(etaAtBTow, phiAtBTow, iEta, iPhi) )
   {
      //printf("phiAtBTow: %.0f deg,  etaAtBTow: %.2f, iEta: %d, iPhi: %d\n",
      //       posCTB.phi()/3.1416*180., posCTB. pseudoRapidity(),iEta, iPhi);
      //printf("hit Tower ID=%d\n",towerId);

      mMatchedTower.id   = gMapBTowEtaPhiBin2Id[ iEta + iPhi * mxBTetaBin];
      mMatchedTower.R    = mCoorAtBTow; //TVector3(posAtBTow.x(), posAtBTow.y(), posAtBTow.z());
      mMatchedTower.iEta = iEta;
      mMatchedTower.iPhi = iPhi;

      return true;
   }

   return false;
}


bool VecBosTrack::ExtendTrack2Endcap()
{
   if ( mStMuTrack->pt() < 1.0 || mStMuTrack->flag() != 311 )
      return false;

   //printf("******* extendTracksEndcap() nVert=%d\n",mVecBosEvent.vertex.size());
   //if (!mVecBosEvent->l2EbitET) return 0; //fire endcap trigger

   //double parE_zSMD = mGeomEmc->getZSMD(); // (cm), smd depth
   //int nTrE = 0;

   //VecBosVertexPtrSetIter iVertex = GetVecBosEvent()->mVertices.begin();

   //for ( ; iVertex != GetVecBosEvent()->mVertices.end(); ++iVertex)
   //{
   //   VecBosVertex &vertex = **iVertex;

   //   for (uint it = 0; it < vertex.eleTrack.size(); it++)
   //   {
   //      VecBosTrack &T = vertex.eleTrack[it];

   //      if (T.mStMuTrack->eta() < parE_trackEtaMin) continue; // to avoid extrapolation nonsense

   //      // Do eta sorting at track level (tree analysis)
   //      if (T.mP3AtDca.Eta() < mMinETrackEta || T.mP3AtDca.Eta() > mMaxETrackEta) continue;

   //      // Extrapolate track to the disk perpendicular to the z-axis
   //      const StPhysicalHelixD trkHlx       = T.mStMuTrack->outerHelix();
   //      StThreeVectorD         diskPosition = StThreeVectorD(0, 0, parE_zSMD);
   //      StThreeVectorD         diskNormal   = StThreeVectorD(0, 0, 1);

   //      //path length at intersection with plane
   //      double path = trkHlx.pathLength(diskPosition, diskNormal);

   //      StThreeVectorD r = trkHlx.at(path);
   //      float periodL = trkHlx.period();

   //      if (periodL < 2 * path) {
   //         printf(" Warn, long path fac=%.1f ", path / periodL);
   //         printf(" punchEEMC1 x,y,z=%.1f, %.1f, %.1f path=%.1f period=%.1f\n", r.x(), r.y(), r.z(), path, periodL);
   //      }

   //      //printf("hitR xyz=%f %f %f, detEta=%f\n",r.x(),r.y(),r.z(),eta);
   //      hE[69]->Fill(r.x(), r.y());

   //      int isec, isubSec, ietaBin;
   //      Float_t epsPhi, epsEta;
   //      TVector3 rCross(r.x(), r.y(), r.z());
   //      bool inEtow = mGeomEmc->getTower(rCross, isec, isubSec, ietaBin, epsPhi, epsEta);
   //      if (!inEtow) continue;
   //      hE[20]->Fill("@E", 1.);
   //      //printf("trk points EEMC tower isec=%d isub=%d ieta=%d epsPhi=%f epsEta=%f  trkPT=%f\n", isec,isubSec,ietaBin,epsPhi,epsEta,T.mStMuTrack->pt());

   //      nTrE++;
   //      T.mMatchedTower.id   = -999; //set negative for endcap towers
   //      T.mMatchedTower.R    = rCross;
   //      T.mMatchedTower.iEta = ietaBin;
   //      T.mMatchedTower.iPhi = isec * mxEtowSub + isubSec;

   //      //find global track extrapolation (for ESMD analysis)
   //      const StPhysicalHelixD trkHlxGlob = T.glMuTrack->outerHelix();
   //      double pathGlob = trkHlxGlob.pathLength(diskPosition, diskNormal);

   //      StThreeVectorD rGlob = trkHlxGlob.at(pathGlob);
   //      float periodLGlob    = trkHlxGlob.period();

   //      if (periodLGlob < 2 * pathGlob) {
   //         printf(" Warn, long path Global fac=%.1f ", pathGlob / periodLGlob);
   //         printf(" punchEEMC1 x,y,z=%.1f, %.1f, %.1f path=%.1f period=%.1f\n", r.x(), r.y(), r.z(), pathGlob, periodLGlob);
   //      }
   //      TVector3 rCrossGlob(rGlob.x(), rGlob.y(), rGlob.z());
   //      T.mMatchedTower.Rglob = rCrossGlob;
   //   }
   //}

   //if (nTrE <= 0) return -1;
   //hE[0]->Fill("TrE", 1.0);

   return false;
}


bool VecBosTrack::MatchTrack2BarrelCluster()
{
   //if (mMatchedTower.id <= 0) return; // skip endcap towers
   //printf("******* matchCluster() nVert=%d\n",mVecBosEvent->mVertices.size());
   //float trackPT = mStMuTrack->momentum().perp();

   //mCluster1x1.BuildAroundTower(*this)

   // Choose 2x2 cluster with maximum ET
   mCluster2x2 = mEvent->FindMaxBTow2x2(mMatchedTower.iEta, mMatchedTower.iPhi, mVertex->mPosition.Z());

   // Compute surroinding cluster energy. 4x4 cluster lower left tower
   mCluster4x4 = mEvent->SumBTowPatch(mCluster2x2.iEta - 1, mCluster2x2.iPhi - 1, 4, 4, mVertex->mPosition.Z()); // needed for lumi monitor

   //if (mCluster2x2.ET < mMinBClusterEnergy) continue; // too low energy

   //float frac24 = mCluster2x2.ET / mCluster4x4.ET;

   //if (frac24 < mMinBClusterEnergyIsoRatio) continue;

   // spacial separation (track - cluster)
   mDistToCluster = mCoorAtBTow - mCluster2x2.position;

   //float deltaPhi = mCoorAtBTow.DeltaPhi(mCluster2x2.position);

   // printf("aaa %f %f %f   phi=%f\n", mDistToCluster.x(), mDistToCluster.y(), mDistToCluster.z(),deltaPhi);

   if (mDistToCluster.Mag() <= sMaxTrackClusterDist)
   {
      mVbType |= kHAS_CLUSTER;
      return true;
   }

   return false;
}


/**
 * Calculates the energy in the cone around the track.
 */
void VecBosTrack::CalcEnergyInCones()
{
   // sum EMC-jet component. XXX:ds: Note: the track direction is taken at the origin
   mP3InNearConeBTow          = mEvent->CalcP3InConeBTow(this, 2); // '2'=2D cone
   mP3InNearConeETow          = mEvent->CalcP3InConeETow(this, 2); // '2'=2D cone
   mP3InNearConeTpc           = mEvent->CalcP3InConeTpc (this, 2); // '2'=2D cone
   mP3InNearConeTpc_noEleP    = mEvent->CalcP3InConeTpc_noEleP (this, 2); // '2'=2D cone

   mP3InNearConeTow    = mP3InNearConeBTow + mP3InNearConeETow;
   mP3InNearCone       = mP3InNearConeBTow + mP3InNearConeETow + mP3InNearConeTpc; // XXX:ds: double counting? yes, see correction below
   mP3InNearConeNoETow = mP3InNearConeBTow + mP3InNearConeTpc;

   mP3InNearCone_noEleP = mP3InNearConeBTow + mP3InNearConeETow + mP3InNearConeTpc_noEleP; // XXX:sf: added for comparison with Justin et al.
   mP3InNearConeNoETow_noEleP = mP3InNearConeBTow + mP3InNearConeTpc_noEleP;

   mP3InOppsConeBTow   = mEvent->CalcP3InConeBTow(this, 1, -1); // '1'=1D cone; 'scale=-1'-> we are looking to the opposite cone
   mP3InOppsConeETow   = mEvent->CalcP3InConeETow(this, 1, -1); // '1'=1D cone; 'scale=-1'-> we are looking to the opposite cone
   mP3InOppsConeTpc    = mEvent->CalcP3InConeTpc (this, 1, -1); // '1'=1D cone; 'scale=-1'-> we are looking to the opposite cone

   mP3InOppsConeTow    = mP3InOppsConeBTow + mP3InOppsConeETow;
   mP3InOppsCone       = mP3InOppsConeTow  + mP3InOppsConeTpc; // XXX:ds: double counting? yes, see correction below
   mP3InOppsConeNoETow = mP3InOppsCone     - mP3InOppsConeETow;

   if (GetClusterEnergyFrac() >= VecBosEvent::sMinClusterEnergyFrac)
   {
      mVbType |= kISOLATED;

      if ( mP3InOppsConeTow.Mag() < sMaxEnergyInOppsCone )
      {
         mVbType |= kUNBALANCED;
      }
   } else if (GetClusterEnergyFracNoEndcap() >= VecBosEvent::sMinClusterEnergyFrac) {
      mVbType |= kISOLATEDNOETOW;

      if ( mP3InOppsConeTow.Mag() < sMaxEnergyInOppsCone )
      {
         mVbType |= kUNBALANCED;
      }
   }

   if (GetClusterEnergyFrac_noEleP() >= 0.88)  // TEST
   {
      mVbType |= kISOLATEDOLD;
   }
 
   // else {
      //mEvent->mTracksCandidate.push_back(this);
   //}

}


void VecBosTrack::CheckChargeSeparation()
{
   if ( fabs( (GetChargeSign() * GetP3EScaled().Pt() ) / GetP3AtDca().Pt() ) >= 0.4 &&
        fabs( (GetChargeSign() * GetP3EScaled().Pt() ) / GetP3AtDca().Pt() ) <= 1.8 )
   {
      mVbType |= kHAS_CHARGE;
   }
}
