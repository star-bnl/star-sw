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
//class AliHLTTPCCAMergerOutput;
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
//      if ( a.slice_mid < b.slice_mid ) return 1;
//      if ( a.slice_mid > b.slice_mid ) return 0;
      return ( a.z_h_dn < b.z_h_dn );
    }
  };
 public:

//  AliHLTTPCCALooperMerger();
  AliHLTTPCCALooperMerger( AliHLTTPCCAMergerOutput& in_out, AliHLTResizableArray<AliHLTTPCCAGBHit>& hits )
   : fOutput( in_out )
   , fGBHits( hits )
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
