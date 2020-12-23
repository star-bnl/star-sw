/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2007-2020 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2020 Goethe University of Frankfurt
 *               2007-2020 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Sergey Gorbunov
 *               2007-2019 Maksym Zyzak
 *               2007-2014 Igor Kulakov
 *               2014-2020 Grigory Kozlov
 *
 * TPCCATracker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * TPCCATracker is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

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
