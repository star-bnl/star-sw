// @(#) $Id: AliHLTTPCCARow.cxx,v 1.1 2016/02/05 23:27:28 fisyak Exp $
//***************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
//                                                                          *
// Developed by:   Igor Kulakov <I.Kulakov@gsi.de>                          *
//                 Maksym Zyzak <M.Zyzak@gsi.de>                            *
//                                                                          *
// Permission to use, copy, modify and distribute this software and its     *
// documentation strictly for non-commercial purposes is hereby granted     *
// without fee, provided that the above copyright notice appears in all     *
// copies and that both the copyright notice and this permission notice     *
// appear in the supporting documentation. The authors make no claims       *
// about the suitability of this software for any purpose. It is            *
// provided "as is" without express or implied warranty.                    *
//***************************************************************************

#include "AliHLTTPCCARow.h"


//X AliHLTTPCCARow::AliHLTTPCCARow()
//X   :
//X   fFirstHit(0), fNHits(0), fX(0), fMaxY(0), fGrid(),
//X   fHy0(0),fHz0(0), fHstepY(0),fHstepZ(0), fHstepYi(0), fHstepZi(0),
//X   fFullSize(0), fFullOffset(0), fFullGridOffset(0),fFullLinkOffset(0)
//X {
//X   // dummy constructor
//X }

#include "BinaryStoreHelper.h"

void AliHLTTPCCARow::StoreToFile( FILE *f, const char *startPointer ) const
{
  BinaryStoreWrite( fGrid, f );
  BinaryStoreWrite( fNHits, f );
  //BinaryStoreWrite( fX, f );
  BinaryStoreWrite( fMaxY, f );
  BinaryStoreWrite( fHitNumberOffset, f );

  BinaryStoreWrite( fLinkUpData, startPointer, f );
  BinaryStoreWrite( fLinkDownData, startPointer, f );

  BinaryStoreWrite( fHitDataY, startPointer, f );
  BinaryStoreWrite( fHitDataZ, startPointer, f );

  BinaryStoreWrite( fClusterDataIndex, startPointer, f );

  BinaryStoreWrite( fHitWeights, startPointer, f );

  BinaryStoreWrite( fFirstHitInBin, startPointer, f );
}

void AliHLTTPCCARow::RestoreFromFile( FILE *f, char *startPtr )
{
  BinaryStoreRead( fGrid, f );
  BinaryStoreRead( fNHits, f );
  //BinaryStoreRead( fX, f );
  BinaryStoreRead( fMaxY, f );
  BinaryStoreRead( fHitNumberOffset, f );

  BinaryStoreRead( fLinkUpData, startPtr, f );
  BinaryStoreRead( fLinkDownData, startPtr, f );

  BinaryStoreRead( fHitDataY, startPtr, f );
  BinaryStoreRead( fHitDataZ, startPtr, f );

  BinaryStoreRead( fClusterDataIndex, startPtr, f );

  BinaryStoreRead( fHitWeights, startPtr, f );

  BinaryStoreRead( fFirstHitInBin, startPtr, f );
}
