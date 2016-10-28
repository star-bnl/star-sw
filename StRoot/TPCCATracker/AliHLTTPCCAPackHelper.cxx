// @(#) $Id: AliHLTTPCCARow.cxx,v 1.2 2012/08/13 19:35:05 fisyak Exp $
//***************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Igor Kulakov <I.Kulakov@gsi.de>                         *
//                  Maksym Zyzak <M.Zyzak@gsi.de>                           *
//                  Ivan Kisel   <I.Kisel@compeng.uni-frankfurt.de>         *
//                                                                          *
// Permission to use, copy, modify and distribute this software and its     *
// documentation strictly for non-commercial purposes is hereby granted     *
// without fee, provided that the above copyright notice appears in all     *
// copies and that both the copyright notice and this permission notice     *
// appear in the supporting documentation. The authors make no claims       *
// about the suitability of this software for any purpose. It is            *
// provided "as is" without express or implied warranty.                    *
//***************************************************************************

#include "AliHLTTPCCAPackHelper.h"
#include "AliHLTTPCCARow.h"
#include "AliHLTTPCCADef.h"

// 1e2f is chousen because in this case multiplication of floats is very fast
PackHelper::TPackedY PackHelper::PackY( const AliHLTTPCCARow& row, float y ) {
  UNUSED_PARAM1(row);
  return y*1e2f;
}
PackHelper::TPackedZ PackHelper::PackZ( const AliHLTTPCCARow& row, float z ) {
  UNUSED_PARAM1(row);
  return z*1e2f;
}

float PackHelper::UnpackY( const AliHLTTPCCARow& row, PackHelper::TPackedY y ) {
  UNUSED_PARAM1(row);
  return static_cast<float>(y)*1e-2f;
}
float PackHelper::UnpackZ( const AliHLTTPCCARow& row, PackHelper::TPackedZ z ) {
  UNUSED_PARAM1(row);
  return static_cast<float>(z)*1e-2f;
}


PackHelper::TPackedY_v PackHelper::PackY( const AliHLTTPCCARow& row, float_v y ) {
  UNUSED_PARAM1(row);
  return static_cast<PackHelper::TPackedY_v>(y*1e2f);
}
PackHelper::TPackedZ_v PackHelper::PackZ( const AliHLTTPCCARow& row, float_v z ) {
  UNUSED_PARAM1(row);
  return static_cast<PackHelper::TPackedZ_v>(z*1e2f);
}

float_v PackHelper::UnpackY( const AliHLTTPCCARow& row, PackHelper::TPackedY_v y ) {
  UNUSED_PARAM1(row);
  return static_cast<float_v>(y)*1e-2f;
}
float_v PackHelper::UnpackZ( const AliHLTTPCCARow& row, PackHelper::TPackedZ_v z ) {
  UNUSED_PARAM1(row);
  return static_cast<float_v>(z)*1e-2f;
}

