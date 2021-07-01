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
    int  RowHit( int i )   const { return fRowHits[i];    }

    void SetNHits( int v )               {  fNHits = v;      }
    void SetFirstRow( int v )            {  fFirstRow = v;   }
    void SetLastRow( int v )             {  fLastRow = v;    }
    void SetParam( const AliHLTTPCCATrackParam &v ) { fParam = v;      }
    void SetRowHit( int irow, int ih )  { fRowHits[irow] = ih;    }

    void ClearRowHits() {
      std::cout<<"!WARNING: unused function, should be deleted\n";
      const int_v zero( Vc::Zero );
      STATIC_ASSERT( AliHLTTPCCAParameters::MaxNumberOfRows8 % int_v::Size == 0, Size_of_fRowHits_array_needs_to_be_a_multiple_of_int_v__Size );
    }

  private:

    int fNHits;                 // N hits
    int fFirstRow;              // first TPC row
    int fLastRow;               // last TPC row
    AliHLTTPCCATrackParam fParam; // tracklet parameters
    AliHLTFixedArray<int, AliHLTArraySize<AliHLTTPCCAParameters::MaxNumberOfRows8>, VectorAlignment> fRowHits; // hit index for each TPC row
};

#endif
