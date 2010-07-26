/**************************************************************************
 * This file is property of and copyright by the ALICE HLT Project        *
 * All rights reserved.                                                   *
 *                                                                        *
 * Primary Authors:                                                       *
 *     Copyright 2009       Matthias Kretz <kretz@kde.org>                *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 **************************************************************************/

#include "AliHLTTPCCATrackletVector.h"

AliHLTTPCCATrackletVector::AliHLTTPCCATrackletVector()
    : fNHits( Vc::Zero ), fFirstRow( Vc::Zero ), fLastRow( Vc::Zero )
{
  const ushort_v x = std::numeric_limits<ushort_v>::max();
  for ( int i = 0; i < fRowHits.Size(); ++i ) {
    fRowHits[i] = x;
  }
}
