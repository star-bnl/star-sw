#include "StProtoJetCut.h"
#include "StProtoJetListCut.h"

StJetFinder::JetList StProtoJetListCut::operator()(const StJetFinder::JetList& protojets) const
{
  StJetFinder::JetList result;
  for (StJetFinder::JetList::const_iterator iProtoJet = protojets.begin(); iProtoJet != protojets.end(); ++iProtoJet) {
    const StProtoJet& protojet = *iProtoJet;
    if (!cut(protojet)) result.push_back(protojet);
  }
  return result;
}

bool StProtoJetListCut::cut(const StProtoJet& protojet) const
{
  for (vector<StProtoJetCut*>::const_iterator iCut = mCutList.begin(); iCut != mCutList.end(); ++iCut) {
    const StProtoJetCut& cut = **iCut;
    if (cut(protojet)) return true;
  }
  return false;
}
