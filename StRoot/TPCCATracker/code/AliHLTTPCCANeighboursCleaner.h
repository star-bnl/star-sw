//-*- Mode: C++ -*-

//* This file is property of and copyright by the ALICE HLT Project        *
//* ALICE Experiment at CERN, All rights reserved.                         *
//* See cxx source for full Copyright notice                               *

#ifndef ALIHLTTPCCANEIGHBOURSCLEANER_H
#define ALIHLTTPCCANEIGHBOURSCLEANER_H


#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAParam.h"

class AliHLTTPCCASliceData;

/**
 * @class AliHLTTPCCANeighboursCleaner
 *
 */
struct AliHLTTPCCANeighboursCleaner {
  static void run( const int numberOfRows, AliHLTTPCCASliceData &data, const AliHLTTPCCAParam &param );
};

#endif
