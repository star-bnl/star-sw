// @(#) $Id: AliHLTTPCCANeighboursFinder.cxx,v 1.1 2016/02/05 23:27:28 fisyak Exp $
// **************************************************************************
// This file is property of and copyright by the ALICE HLT Project          *
// ALICE Experiment at CERN, All rights reserved.                           *
//                                                                          *
// Primary Authors: Sergey Gorbunov <sergey.gorbunov@kip.uni-heidelberg.de> *
//                  Ivan Kisel <kisel@kip.uni-heidelberg.de>                *
//                  for The ALICE HLT Project.                              *
//                                                                          *
// Developed by:   Igor Kulakov <I.Kulakov@gsi.de>                          *
//                 Maksym Zyzak <M.Zyzak@gsi.de>                            *
//                                                                          *
// Permission to use, copy, modify and distribute this software and its     *
// documentation strictly for non-commercial purposes is hereby granted     *
// without fee, provided that the above copyright notice appears in all     *
// copies and that both the copyright notice and this permission notice     *
// appear in the supporting documentation. The authors make no claims       *
// about the suitability of this software for any purpose. It is            *
// provided "as is" without express or implied warranty.                    *
//                                                                          *
//***************************************************************************

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

//   for ( int i = 0; i < AliHLTTPCCAParameters::NumberOfRows; ++i ) {
//     const AliHLTTPCCARow &row = fData.Row( i );
//     for ( int hit = 0; hit < row.NHits(); ++hit ) {
//       std::cout << hit << " " << fData.HitLinkUpData( row )[hit] << std::endl;
//       
//     }
//   }
}
