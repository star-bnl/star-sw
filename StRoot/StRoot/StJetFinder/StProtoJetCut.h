// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 28 May 2010
//

#ifndef ST_PROTO_JET_CUT_H
#define ST_PROTO_JET_CUT_H

#include "TObject.h"
#include "StProtoJet.h"

class StProtoJetCut : public TObject {
public:
  virtual bool operator()(const StProtoJet& protojet) const = 0;

private:
  ClassDef(StProtoJetCut,0);
};

#endif	// ST_PROTO_JET_CUT_H
