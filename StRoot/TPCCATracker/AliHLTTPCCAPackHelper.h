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
  typedef int_v TPackedY_v;
  typedef int_v TPackedZ_v;
 //  typedef float TPackedY;
 //  typedef float TPackedZ;

  static TPackedY PackY( const AliHLTTPCCARow& row, float y );
  static TPackedZ PackZ( const AliHLTTPCCARow& row, float z );
  static float UnpackY( const AliHLTTPCCARow& row, TPackedY y );
  static float UnpackZ( const AliHLTTPCCARow& row, TPackedZ z );

  static TPackedY_v PackY( const AliHLTTPCCARow& row, float_v y );
  static TPackedZ_v PackZ( const AliHLTTPCCARow& row, float_v z );
  static float_v UnpackY( const AliHLTTPCCARow& row, TPackedY_v y );
  static float_v UnpackZ( const AliHLTTPCCARow& row, TPackedZ_v z );
};

#endif
