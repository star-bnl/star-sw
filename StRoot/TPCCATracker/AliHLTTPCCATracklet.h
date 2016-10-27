//-*- Mode: C++ -*-
// ************************************************************************
// This file is property of and copyright by the ALICE HLT Project        *
// ALICE Experiment at CERN, All rights reserved.                         *
// See cxx source for full Copyright notice                               *
//                                                                        *
//*************************************************************************

#ifndef ALIHLTTPCCATRACKLET_H
#define ALIHLTTPCCATRACKLET_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCATrackParam.h"
#include "AliHLTArray.h"

#include "AliHLTTPCCAParameters.h"

/**
 * @class ALIHLTTPCCATracklet
 *
 * The class describes the reconstructed TPC track candidate.
 * The class is dedicated for internal use by the AliHLTTPCCATracker algorithm.
 */
class AliHLTTPCCATracklet
{
  public:
    AliHLTTPCCATracklet() : fNHits( 0 ), fFirstRow( 0 ), fLastRow( 0 ), fParam() {};
    AliHLTTPCCATracklet( const AliHLTTPCCATrackletVector &tv, int i ) :
      fNHits( tv.NHits()[i] ),
      fFirstRow( tv.FirstRow()[i] ),
      fLastRow( tv.LastRow()[i] ),
      fParam( tv.Param(), i )
    {
      for ( int row = 0; row < AliHLTTPCCAParameters::MaxNumberOfRows8; ++row ) {
        fRowHits[row] = tv.HitIndexAtRow( row )[i];
      }
    }

    bool operator<( const AliHLTTPCCATracklet &rhs ) const {
      return fFirstRow < rhs.fFirstRow ||
        ( fFirstRow == rhs.fFirstRow && ( fNHits < rhs.fNHits ||
                                          ( fNHits == rhs.fNHits && fParam.Chi2() < rhs.fParam.Chi2() ) ) );
    }

    int  NHits()                const { return fNHits;      }
    int  FirstRow()             const { return fFirstRow;   }
    int  LastRow()              const { return fLastRow;    }
    const AliHLTTPCCATrackParam &Param() const { return fParam;      }
    short  RowHit( int i )   const { return fRowHits[i];    }

    void SetNHits( int v )               {  fNHits = v;      }
    void SetFirstRow( int v )            {  fFirstRow = v;   }
    void SetLastRow( int v )             {  fLastRow = v;    }
    void SetParam( const AliHLTTPCCATrackParam &v ) { fParam = v;      }
    void SetRowHit( int irow, short ih )  { fRowHits[irow] = ih;    }

    void ClearRowHits() {
      const short_v zero( Vc::Zero );
      STATIC_ASSERT( AliHLTTPCCAParameters::MaxNumberOfRows8 % short_v::Size == 0, Size_of_fRowHits_array_needs_to_be_a_multiple_of_short_v__Size );
      for ( int i = 0; i < AliHLTTPCCAParameters::MaxNumberOfRows8; i += short_v::Size ) {
        zero.store( &fRowHits[i] );
      }
    }

  private:

    int fNHits;                 // N hits
    int fFirstRow;              // first TPC row
    int fLastRow;               // last TPC row
    AliHLTTPCCATrackParam fParam; // tracklet parameters
    AliHLTFixedArray<short, AliHLTArraySize<AliHLTTPCCAParameters::MaxNumberOfRows8>, VectorAlignment> fRowHits; // hit index for each TPC row
};

#endif
