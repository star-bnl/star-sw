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

#ifndef ALIHLTTPCCATRACK_H
#define ALIHLTTPCCATRACK_H

#include "AliHLTTPCCADef.h"
#include "AliHLTTPCCATrackParam.h"
#include "AliHLTTPCCAParameters.h"
#include "AliHLTTPCCAHitId.h"
#include "AliHLTArray.h"

/**
 * @class ALIHLTTPCCAtrack
 *
 * The class describes the [partially] reconstructed TPC track [candidate].
 * The class is dedicated for internal use by the AliHLTTPCCATracker algorithm.
 */
class AliHLTTPCCATrack
{
  friend class AliHLTTPCCATrackletSelector;
  public:
    AliHLTTPCCATrack() : fNumberOfHits( 0 ) {}
    short NumberOfHits() const { return fNumberOfHits; }
    const AliHLTTPCCATrackParam &Param() const { return fParam; };
    const AliHLTTPCCAHitId &HitId( int i ) const { return fHitIdArray[i]; }

  private:
    AliHLTTPCCATrackParam fParam; // track parameters
    AliHLTFixedArray<AliHLTTPCCAHitId, AliHLTArraySize<AliHLTTPCCAParameters::MaxNumberOfRows8> > fHitIdArray;
    short fNumberOfHits;      // number of hits in the track

    // disallow copy
    AliHLTTPCCATrack( const AliHLTTPCCATrack & );
    AliHLTTPCCATrack &operator=( const AliHLTTPCCATrack & );
};

typedef AliHLTTPCCATrack Track;

#endif
