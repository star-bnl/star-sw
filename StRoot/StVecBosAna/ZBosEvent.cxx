
#include "ZBosEvent.h"
#include "ZBosMcEvent.h"

#include "utils/utils.h"

ClassImp(ZBosEvent)

using namespace std;


const float ZBosEvent::sMinZEleCandPtLight     = 15;
const float ZBosEvent::sMinZEleCandPtHard      = 25;
const float ZBosEvent::sMinZMass           = 73;  // Zmass -20%
const float ZBosEvent::sMaxZMass           = 114; // Zmass +20%



ZBosEvent::ZBosEvent() : VecBosEvent(), mZBosMass(91.1876), mCand1P3(), mCand2P3()
{
}


ZBosEvent::~ZBosEvent()
{
}


TVector3 ZBosEvent::GetCandidate1_P3() const { return mCand1P3; }
TVector3 ZBosEvent::GetCandidate2_P3() const { return mCand2P3; }
TVector3 ZBosEvent::GetVecBosonP3() const { return GetCandidate1_P3() + GetCandidate2_P3(); }
TLorentzVector ZBosEvent::GetVecBosonP4() const { return mP4ZBoson; }


/**
 * The primary method to identify and reconstruct the event with a Z boson.
 */
void ZBosEvent::Process()
{
   UShort_t vertexId = 0;

   VecBosVertexPtrSetIter iVertex = mVertices.begin();
   for ( ; iVertex != mVertices.end(); ++iVertex, vertexId++)
   {
      VecBosVertex &vertex = **iVertex;
      vertex.Process();
      vertex.mId = vertexId;

      if ( !vertex.IsGood()) continue;

      mNumGoodVertices++;

      VecBosVertexPtrSetIter iVertex2 = iVertex; // initialize with the current one
      ++iVertex2; // advance to the next one
      for ( ; iVertex2 != mVertices.end(); ++iVertex2) {
         VecBosVertex &vertex2 = **iVertex2;
         if ( !vertex2.IsGood()) continue;

         Double_t deltaZ = fabs(vertex.mPosition.Z() - vertex2.mPosition.Z());

         if (deltaZ < mMinVertexDeltaZ || mMinVertexDeltaZ < 0)
            mMinVertexDeltaZ = deltaZ;
      }
   }

   // Process tracks
   VecBosTrackPtrSetIter iTrack = mTracks.begin();
   for ( ; iTrack != mTracks.end(); ++iTrack)
   {
      VecBosTrack &track = **iTrack;
      track.Process();

      if (track.IsGood())     mNumGoodTracks++;
      if (track.IsBTrack())   mNumBTracks++;
      if (track.IsETrack())   mNumETracks++;
      if (track.HasCluster()) mNumWithClusterTracks++;
      if (track.IsIsolated()) {
         mNumIsolatedTracks++;

         // if ( track.IsUnBalanced() ) track.FindClosestJet();
      }

      if ( track.IsZelectronCandidate() ) {
         mTracksCandidate.insert(*iTrack);
      }
   }

   // Make sure two track candidate exists
   if (mTracksCandidate.size() <= 1) return;

   VecBosTrack &trackCand1  = **mTracksCandidate.begin();

   mCand1P3 = trackCand1.GetP3EScaled();
   mP4Cand1.SetPxPyPzE(mCand1P3.Px(), mCand1P3.Py(), mCand1P3.Pz(), trackCand1.mCluster2x2.mEnergy);
   mP4Cand2.SetPxPyPzE(0, 0, 0, 0);

   // Loop over candidates.
   VecBosTrackPtrSetIter iTrackCand = mTracksCandidate.begin();
   for (; iTrackCand != mTracksCandidate.end(); ++iTrackCand) {
      VecBosTrack &cand = **iTrackCand;

      if (cand == trackCand1) continue;

      if (cand.mStMuTrack->charge() != trackCand1.mStMuTrack->charge()) {
        // If the sign of the second track is different from the sign
        // of the first track pick it as Z second electron candidate.
        mCand2P3 = cand.GetP3EScaled();
        mP4Cand2.SetPxPyPzE(mCand2P3.Px(), mCand2P3.Py(), mCand2P3.Pz(), cand.mCluster2x2.mEnergy);
        break;
      }
      else {
        Info("Print", "this is not a Z0 event ");
        break;
      }
   }

   CalcZBosP4();
}


void ZBosEvent::ProcessMC()
{
   mMcEvent = new ZBosMcEvent();
}


void ZBosEvent::Clear(const Option_t* opt)
{
   VecBosEvent::Clear();
   mCand1P3.SetXYZ(0, 0, 0);
   mCand2P3.SetXYZ(0, 0, 0);
   mP4Cand1.SetPxPyPzE(0, 0, 0, 0);
   mP4Cand2.SetPxPyPzE(0, 0, 0, 0);

}


void ZBosEvent::Print(const Option_t* opt) const
{
   Info("Print", ":");
   //VecBosEvent::Print(opt);
   mCand1P3.Print();
   mCand2P3.Print();
}


bool ZBosEvent::PassedCutZBos(float minElePt) const
{
   if ( HasCandidateEle() &&
        mCand1P3.Pt() >= minElePt &&
        mCand2P3.Pt() >= minElePt )
   {
      return true;
   }

   return false;
}


bool ZBosEvent::PassedCutZMass(float minElePt) const
{
   if ( PassedCutZBos(minElePt)      &&
        mP4ZBoson.M() > sMinZMass    &&
        mP4ZBoson.M() < sMaxZMass )
   {
      return true;
   }

   return false;
}


void ZBosEvent::CalcZBosP4()
{
   mP4ZBoson = mP4Cand1 + mP4Cand2;
}


void ZBosEvent::Streamer(TBuffer &R__b)
{

   if (R__b.IsReading()) {
      //Info("Streamer", "Reading...");
      R__b.ReadClassBuffer(ZBosEvent::Class(), this);
   }
   else {
      //Info("Streamer", "Writing... ");
      R__b.WriteClassBuffer(ZBosEvent::Class(), this);
   }

}
