//-*- Mode: C++ -*-
// @(#) $Id: AliHLTTPCCARow.h,v 1.2 2010/08/18 14:11:04 ikulakov Exp $
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCAROW_H
#define ALIHLTTPCCAROW_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAGrid.h"
#include "AliHLTTPCCAPackHelper.h"

typedef int StoredIsUsed;

/**
 * @class ALIHLTTPCCARow
 *
 * The ALIHLTTPCCARow class is a hit and cells container for one TPC row.
 * It is the internal class of the AliHLTTPCCATracker algorithm.
 *
 */
class AliHLTTPCCARow
{
  friend class AliHLTTPCCASliceData;
  public:

    AliHLTTPCCARow() {}

    int NHits()    const { return fNHits; }
    float MaxY()     const { return fMaxY; }
    float MaxZ()     const { return 250.f; }
    const AliHLTTPCCAGrid &Grid() const { return fGrid; }

    int   HitNumberOffset() const { return fHitNumberOffset; }

    void StoreToFile( FILE *f, const char *startPtr ) const;
    void RestoreFromFile( FILE *f, char *startPtr );

  int NUnusedHits()    const { return fNUnusedHits; }
  unsigned int* HitIndex() const { return fHitIndex; }
  private:
    AliHLTTPCCAGrid fGrid;   // grid of hits

    int fNHits;            // number of hits in this row
    float fMaxY;           // maximal Y coordinate of the row

    int fHitNumberOffset;  // index of the first hit in the hit array, used as

    int *fLinkUpData;   // hit index in the row above which is linked to the given (global) hit index
    int *fLinkDownData; // hit index in the row below which is linked to the given (global) hit index

    PackHelper::TPackedY *fHitPDataY;        // memory optimized packed data
    PackHelper::TPackedZ *fHitPDataZ;
  
    StoredIsUsed *fHitDataIsUsed;         // packed isUsed-flag of the given (global) hit index. Short because there is no bool_v
  
    int *fClusterDataIndex;    // see SliceData::ClusterDataIndex()

    unsigned int *fHitWeights;          // the weight of the longest tracklet crossed the cluster

    /**
     * pointer into SliceData::fMemory where the FirstHitInBin data for this row resides
     */
    unsigned int *fFirstHitInBin; //X

    int fNUnusedHits;            // number of unused hits in this row
    PackHelper::TPackedY *fUnusedHitPDataY;        // memory optimized packed data
    PackHelper::TPackedZ *fUnusedHitPDataZ;
    unsigned int *fHitIndex; // fIndexOfHitByIndexOfUnusedHit
    unsigned int *fFirstUnusedHitInBin; //X
};

#endif
