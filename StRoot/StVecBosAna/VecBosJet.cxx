#include "VecBosJet.h"
#include "TRefArray.h"
#include "StSpinPool/StJets/TrackToJetIndex.h"
#include "utils/utils.h"
#include "VecBosEvent.h"
#include "VecBosTrack.h"


VecBosJet::VecBosJet() : StJet(), mEvent(0), mVbTracks(), mVertexId(-1), mMinZDistToVertex(-1)
{
}


VecBosJet::VecBosJet(StJet &stJet, VecBosEvent* vbEvent) :
   StJet(stJet.E(), stJet.Px(), stJet.Py(), stJet.Pz(), stJet.nCell, stJet.charge),
   mEvent(vbEvent), mVbTracks(), mVertexId(-1), mMinZDistToVertex(-1)
{
  nTracks   = stJet.nTracks;
  nBtowers  = stJet.nBtowers;
  nEtowers  = stJet.nEtowers;
  tpcEtSum  = stJet.tpcEtSum;
  btowEtSum = stJet.btowEtSum;
  etowEtSum = stJet.etowEtSum;
  zVertex   = stJet.zVertex;
  jetEt     = Et();
  jetPt     = Pt();
  jetEta    = Eta();
  jetPhi    = Phi();

  TRefArray &jetTracks = stJet.tracks();
  jetTracks.Expand(stJet.nTracks); // Root tree seems to contain more tracks than 
  //Info("MyTest", "\n\n\n\nmy jet test starts here");
  //Info("MyTest", "Jet jetEt: %f, jetPt: %f, jetEta: %f, jetPhi: %f", stJet.jetEt, stJet.jetPt, stJet.jetEta, stJet.jetPhi);
  //Info("MyTest", "jetTracks.GetSize(): %d, %d, %d, %d", jetTracks.GetSize(), stJet.numberOfTracks(), stJet.nTracks, jetTracks.GetEntriesFast());

  TRefArrayIter jetTracksIter(&jetTracks);
  //jetTracksIter.Reset();

  while ( TrackToJetIndex *t2jIndex = (TrackToJetIndex*) jetTracksIter.Next() )
  {
     //Info("MyTest", "trackIndex(): %d, trackId(): %d, detectorId: %d", t2jIndex->trackIndex(), t2jIndex->trackId(),
     //      t2jIndex->detectorId());
     //utils::PrintTLorentzVector((TLorentzVector&) *t2jIndex);
     VecBosTrack* vbTrack = mEvent->FindTrackById(t2jIndex->trackId());
     if (!vbTrack) continue;
     AddTrack(vbTrack);
  }
}


VecBosJet::~VecBosJet()
{
}


void VecBosJet::AddTrack(VecBosTrack * const vbTrack)
{
   mVbTracks.insert(vbTrack);
}


Double_t VecBosJet::CalcDetEta() const
{
   const VecBosVertex *vbVertex = mEvent->FindVertexById(mVertexId);
   return StJet::detEta(vbVertex->mPosition.Z());
}


/** */
void VecBosJet::Process()
{
   if (!mEvent) return;

   // The only info about jet vertex is its z position. Use it to match vertex to jet
   VecBosVertex *closestVertex = 0;

   VecBosVertexPtrSetIter iVertex = mEvent->mVertices.begin();
   for ( ; iVertex != mEvent->mVertices.end(); ++iVertex)
   {
      VecBosVertex &vertex = **iVertex;
      Double_t deltaZ = fabs(zVertex - vertex.mPosition.Z());

      if (mMinZDistToVertex < 0 || deltaZ < mMinZDistToVertex) {
         mMinZDistToVertex = deltaZ;
         closestVertex = *iVertex;
      }
   }

   if (closestVertex && mMinZDistToVertex < VecBosEvent::sMaxVertexJetDeltaZ) {
      SetVertexId(closestVertex->GetId());
   }
}


/** */
TVector3 VecBosJet::FindIntersectionInnerBTow() const
{
   Warning("FindIntersectionInnerBTow", "Not implemented");
   //VecBosVertex *vbVertex = mEvent->GetVertexById(mVertexId);
}


/**
 * @brief
 *
 * @return
 */
//VecBosTrackPtrSet& VecBosJet::FindTracksInCone()
void VecBosJet::FindTracksInCone()
{
   Info("FindTracksInCone", "xxx");
   const TRefArray &jetTracks = tracks();

   Info("FindTracksInCone", "jetTracks.GetSize(): %d", jetTracks.GetSize());

   TRefArrayIter   *next = new TRefArrayIter(&jetTracks);
   TrackToJetIndex *t2jIndex;

   // Loop over the tracks in this jet
   while (next && ( t2jIndex = (TrackToJetIndex*) (*next)() ) )
   {
      Info("FindTracksInCone", "t2jIndex.trackIndex(): %d", t2jIndex->trackIndex());
   }
}


void VecBosJet::Print(const Option_t* opt) const
{
   Info("Print", "mVertexId: %d, zVertex: %.1f cm, nTracks: %d", mVertexId, zVertex, nTracks);
   Info("Print", "Eta: %f", Eta());
   Info("Print", "mVbTracks.size(): %d", mVbTracks.size());

   VecBosTrackPtrSetIter iTrack = mVbTracks.begin();
   for ( ; iTrack != mVbTracks.end(); ++iTrack)
   {
      Info("Print", "iTrack: %x", *iTrack);
   }
}
