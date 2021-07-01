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

#include "AliHLTTPCCANeighboursFinder.h"
#include "AliHLTTPCCAMath.h"
#include "AliHLTTPCCATracker.h"
#include "AliHLTArray.h"
#include "AliHLTTPCCADef.h"

#ifdef USE_TBB
#include <tbb/parallel_for.h>
#include <tbb/blocked_range.h>
#endif //USE_TBB

#ifdef MAIN_DRAW
#include "AliHLTTPCCADisplay.h"
bool DRAW_EVERY_LINK = false;
#endif

#include "debug.h"

void AliHLTTPCCATracker::NeighboursFinder::execute()
{
  const int numberOfRows = fTracker->Param().NRows();

#ifdef MAIN_DRAW
  if ( AliHLTTPCCADisplay::Instance().DrawType() == 1 ) {
    AliHLTTPCCADisplay::Instance().SetSliceView();
    AliHLTTPCCADisplay::Instance().ClearView();
    AliHLTTPCCADisplay::Instance().SetCurrentSlice( fTracker );
    AliHLTTPCCADisplay::Instance().DrawSlice( fTracker, 0 );
    AliHLTTPCCADisplay::Instance().DrawSliceHits();
  }
#endif
  //tbb::affinity_partitioner partitioner;
  const int rowStep = AliHLTTPCCAParameters::RowStep;
#ifdef USE_TBB  
  tbb::parallel_for( tbb::blocked_range<int>( rowStep, numberOfRows - rowStep, 1000 ), *this );//, partitioner );
#else  //USE_TBB  
  for ( int iRow = rowStep; iRow < numberOfRows - rowStep; ++iRow ) {
    executeOnRow( iRow );
  }
#endif //USE_TBB
}
