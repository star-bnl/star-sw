//-*- Mode: C++ -*-

//* This file is property of and copyright by the ALICE HLT Project        *
//* ALICE Experiment at CERN, All rights reserved.                         *
//* See cxx source for full Copyright notice                               *

#ifndef ALIHLTTPCCASTARTHITSFINDER_H
#define ALIHLTTPCCASTARTHITSFINDER_H

#include "AliHLTTPCCADef.h"

class AliHLTTPCCATracker;
class AliHLTTPCCASliceData;

/**
 * @class AliHLTTPCCAStartHitsFinder
 *
 * find start hits for tracklets
 */
struct AliHLTTPCCAStartHitsFinder {
  static void run( AliHLTTPCCATracker &tracker, AliHLTTPCCASliceData &data, int iter );
};

#endif
