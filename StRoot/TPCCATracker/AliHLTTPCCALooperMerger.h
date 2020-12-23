/*
 * This file is part of TPCCATracker package
 * Copyright (C) 2007-2020 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2020 Goethe University of Frankfurt
 *               2007-2020 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
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

#ifndef ALIHLTTPCCALOOPERMERGER_H
#define ALIHLTTPCCALOOPERMERGER_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCAParam.h"
#include "AliHLTTPCCATrackParam.h"
#include "AliHLTTPCCATracker.h"

#include "AliHLTTPCCAMergerOutput.h"

#include <vector>
#include <map>

class AliHLTTPCCAMergedTrack;
class AliHLTTPCCATracker;

/**
 * @class AliHLTTPCCALooperMerger
 *
 */

class AliHLTTPCCALooperMerger
{
  struct LooperSegment {
    float iTr;
    float QPt_abs;
    float DzDs_abs;
    float Cx, Cy, Cr;		// Circle x, y, r
    float x_up, y_up, z_up;
    float x_dn, y_dn, z_dn;
    float x_h_up, y_h_up, z_h_up;
    float x_h_dn, y_h_dn, z_h_dn;
    float h;
    int slice_mid;
    int iLooper;
    bool isUsed;
    static bool CompareL( const LooperSegment &a, const LooperSegment &b )
    {
      return ( a.z_h_dn < b.z_h_dn );
    }
  };
 public:

  AliHLTTPCCALooperMerger( AliHLTTPCCAMergerOutput& in_out, AliHLTResizableArray<AliHLTTPCCAGBHit>& hits )
   : fSliceParam()
   , fGBHits( hits )
   , fOutput( in_out )
   , fNLoopers( 0 )
  {}

  ~AliHLTTPCCALooperMerger()
  {
    fSegments.clear();
  }

  void SetSliceParam( const AliHLTTPCCAParam &v ) { fSliceParam = v; }
  void SetSlices (int i, AliHLTTPCCATracker *sl )
  {
    //copy sector parameters information
    slices[i] = sl;
  }
  void SetSliceData( int index, AliHLTTPCCASliceOutput *sliceData )
  {
    //copy sector output information: hits, reconstructed tracks...
    fkSlices[index] = sliceData;
  }

  void SetFirstSliceHits( int slice, int hit )
  {
    fFirstSliceHit[slice] = hit;
  }

  void StartLooperTest()
  {
    fSegments.clear();
    fNLoopers = 0;
  }

  void FillSegments();
  void CheckSegments();
  void SaveSegments();

 private:

  static const int fgkNSlices = AliHLTTPCCAParameters::NumberOfSlices;       //* N slices
  AliHLTTPCCAParam fSliceParam;           //* slice parameters (geometry, calibr, etc.)

  AliHLTResizableArray<AliHLTTPCCAGBHit>& fGBHits;
  int fFirstSliceHit[24];

  AliHLTTPCCASliceOutput *fkSlices[fgkNSlices]; //* array of input slice tracks
  AliHLTTPCCATracker *slices[fgkNSlices];
  AliHLTTPCCAMergerOutput &fOutput;       //* array of output merged tracks

  vector<LooperSegment> fSegments;
  int fNLoopers;
};

#endif
