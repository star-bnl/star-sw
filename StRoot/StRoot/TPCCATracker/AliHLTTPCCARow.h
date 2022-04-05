//-*- Mode: C++ -*-
// @(#) $Id: AliHLTTPCCARow.h,v 1.2 2016/06/21 03:39:45 smirnovd Exp $
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

#ifdef HAVE_FLOAT16
typedef float16 StoredFloat;
#else
typedef float StoredFloat;
#endif

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

    AliHLTTPCCARow(): fGrid(), fNHits(0), fMaxY(0), fHitNumberOffset(0), fLinkUpData(0), fLinkDownData(0),
     fHitDataY(0), fHitDataZ(0), fHitDataIsUsed(0), fClusterDataIndex(0), fHitWeights(0), fFirstHitInBin(0) {}

    short NHits()    const { return fNHits; }
    //float X()        const { return fX; }
    float MaxY()     const { return fMaxY; }
    const AliHLTTPCCAGrid &Grid() const { return fGrid; }

    int   HitNumberOffset() const { return fHitNumberOffset; }

    void StoreToFile( FILE *f, const char *startPtr ) const;
    void RestoreFromFile( FILE *f, char *startPtr );

    static inline short_v NHits( const AliHLTTPCCARow *array, const ushort_v &indexes ) { return short_v( array, &AliHLTTPCCARow::fNHits, indexes ); }
    static inline short_v NHits( const AliHLTTPCCARow *array, const ushort_v &indexes, const ushort_m &mask ) { return short_v( array, &AliHLTTPCCARow::fNHits, indexes, mask ); }

  private:
    AliHLTTPCCAGrid fGrid;   // grid of hits

    short fNHits;            // number of hits in this row
    //float fX;              // X coordinate of the row
    float fMaxY;           // maximal Y coordinate of the row

    int fHitNumberOffset;  // index of the first hit in the hit array, used as

    short *fLinkUpData;   // hit index in the row above which is linked to the given (global) hit index
    short *fLinkDownData; // hit index in the row below which is linked to the given (global) hit index

    StoredFloat *fHitDataY;         // packed y coordinate of the given (global) hit index
    StoredFloat *fHitDataZ;         // packed z coordinate of the given (global) hit index

    short *fHitDataIsUsed;         // packed isUsed-flag of the given (global) hit index. Short because there is no bool_v
  
    int *fClusterDataIndex;    // see SliceData::ClusterDataIndex()

    unsigned short *fHitWeights;          // the weight of the longest tracklet crossed the cluster

    /**
     * pointer into SliceData::fMemory where the FirstHitInBin data for this row resides
     */
    unsigned short *fFirstHitInBin; //X
};

#endif
