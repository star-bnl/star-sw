#ifndef StVertexId_hh
#define StVertexId_hh
#include "StVertexDefinitions.h"
enum StVertexId {
  kUndefinedVtxId = kUndefinedVertexIdentifier,
  kEventVtxId     = kEventVertexIdentifier,
  kV0VtxId        = kV0DecayIdentifier,
  kXiVtxId        = kXiDecayIdentifier,
  kKinkVtxId      = kKinkDecayIdentifier,
  kOtherVtxId     = kOtherTypeIdentifier
};
#endif
