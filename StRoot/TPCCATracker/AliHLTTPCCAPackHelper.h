//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAPACKHELPER_H
#define ALIHLTTPCCAPACKHELPER_H

#include "AliHLTTPCCADef.h"
class AliHLTTPCCARow;

 // version w\o this transformation at revision 21937
class PackHelper
{
 public:
  typedef short int TPackedY;
  typedef short int TPackedZ;

  static TPackedY PackY( const AliHLTTPCCARow& row, float y );
  static TPackedZ PackZ( const AliHLTTPCCARow& row, float z );
  static float UnpackY( const AliHLTTPCCARow& row, TPackedY y );
  static float UnpackZ( const AliHLTTPCCARow& row, TPackedZ z );
};

#endif
