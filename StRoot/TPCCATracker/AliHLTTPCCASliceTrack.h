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


#ifndef ALIHLTTPCCASLICETRACK_H
#define ALIHLTTPCCASLICETRACK_H

#include "AliHLTTPCCATrackParam.h"

/**
 * @class AliHLTTPCCASliceTrack
 * AliHLTTPCCASliceTrack class is used to store TPC tracks,
 * which are reconstructed by the TPCCATracker slice tracker.
 *
 * The class contains:
 * - fitted track parameters at its first row, the covariance matrix, \Chi^2, NDF (number of degrees of freedom )
 * - n of clusters assigned to the track
 * - index of its first cluster in corresponding cluster arrays
 *
 * The class is used to transport the data between AliHLTTPCCATracker{Component} and AliHLTTPCCAGBMerger{Component}
 *
 */
class AliHLTTPCCASliceTrack
{
  public:

    int NClusters()                    const { return fNClusters;       }
    int FirstClusterRef()              const { return fFirstClusterRef; }
    const AliHLTTPCCATrackParam &Param() const { return fParam;           }

    void SetNClusters( int v )                   { fNClusters = v;       }
    void SetFirstClusterRef( int v )              { fFirstClusterRef = v; }
    void SetParam( const AliHLTTPCCATrackParam &v ) { fParam = v;           }

  private:

    AliHLTTPCCATrackParam fParam; //* fitted track parameters at its innermost cluster
    int fFirstClusterRef;       //* index of the index of the first track cluster in corresponding cluster arrays
    int fNClusters;             //* number of track clusters

};


#endif
