/**************************************************************************
 * This file is property of and copyright by the ALICE HLT Project        *
 * All rights reserved.                                                   *
 *                                                                        *
 * Primary Authors:                                                       *
 *     Copyright 2009       Matthias Kretz <kretz@kde.org>                *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 **************************************************************************/

#ifndef ALIHLTTPCCAPARAMETERS_H
#define ALIHLTTPCCAPARAMETERS_H

#include "AliHLTTPCCADef.h"

namespace AliHLTTPCCAParameters
{                                       /// Global parameters
  enum {
    /**
     * When extrapolating allow to skip this many rows without a hit
     */
    MaximumExtrapolationRowGap = 5, // XXX 4 or even 3 seem to give better results

    /**
     * When hits need to be removed from a track because it belongs to another track allow gaps of
     * this size
     */
    MaximumRowGap = 4,

    /**
     * This value defines the ratio of overall hits to shared hits in a track. The value of 10 means
     * that a track with 10 hits can have one shared hit.
     */
    MinimumHitsPerShared = 10,
    
    /**
     * The minimum number of hits a tracklet must have to not be discarded.
     * Used at AliHLTTPCCATrackletConstructor.cxx
     */
    MinimumHitsForTracklet = 4,
    
    /**
     * The minimum number of hits a tracklet  must have to not recalculate sinPhi
     * 
     */
    MinimumHitsForFragileTracklet  = 10,
    
    /**
     * The minimum number of hits a track must have to not be discarded.
     * Used at AliHLTTPCCATrackletSelector.cxx
     */
    MinimumHitsForTrack = MinimumHitsForTracklet, // 10

    /**
     * The minimum number of hits a global track must have to not be discarded.
     * Used at use find TODO:describe
     */
    MinimumHitsForGBTrack = 10, // 30


    /**
     * Maximum number of rows that a slice in the TPC can have at maximum.
     */
      //    NumberOfInnerRows = 13-1, // delete iRow = 12
    
    MaxNumberOfRows8 = 112, // this should be divideable by sfloat_v::Size = 8, (16 for Xeon Phi)

    /**
     * The number of slices in the TPC.
     */
    NumberOfSlices = 24, // TODO: !!! read from file

     /**
      * Step for finding of neighbour hits. Only 1 or 2 value allowed!
      */
    RowStep = 1,

      /**
       * Number of cells in grid will be GridCreationCoeff*NHitsOnRow
       */
    GridCreationCoeff = 2    

  };

    /**
     * Minimum size for cell in a grid.
     */
  static const float MinCellSize = 2.f;
  
  /**
   * Coefficient for size of region to find additional hit for tracksegment.
   */
  static const float HitPickUpFactor = 2.f;
  
  /**
   * Coefficient for size of region on neighbour rows for search neghbour hits. Size = coeff*dx. [cm/dx]
   * different for different iteration of finding
   */
  static const float NeighbourAreaSizeTgY[3] = {.6,  2., 2.}; // TODO choose appropriate and use > 1 iterations
  static const float NeighbourAreaSizeTgZ[3] = {2.,  2., 2.};

   // TODO describe
    // curv = 1/R = (1/pt[GeV] * Bz[kGauss] * c[=0.00028])  . 1GeV -> curv = 0.0014
    // errX ~= 0.12. dX ~= 2. curv+ = 0.06 - anyway additional error is significantly bigger
//#define USE_CURV_CUT  // TODO don't work, problem with error estimation
#ifdef USE_CURV_CUT
  static const float NeighbourCurvCut[3] = {0.03,  .1, 3.};//{0.03,  .1, 3.};
//  static const float NeighbourCurvCutY[3] = {0.03,  .1, 3.};//{0.03,  .1, 3.};
//  static const float NeighbourCurvCutZ[3] = {0.03,  .1, 3.};//{0.03,  .1, 3.};
#else
  static const float NeighbourChiCut[3] = {0.2,  3., 3.};//{0.2,  3., 3.};
#endif // 1

   /**
    * Min length of chain to make tracklet.
    */
  static const int NeighboursChainMinLength[3] = {3,  3, 3};
  
  /**
   * Distance between hits to be merged. [cm]
   */
  static const float MinHitsMergeDist = 2;
  
  /**
   * The minimum for q/Pt that TrackletConstructor ensures at the end.
   */
#ifndef __CINT__
  static const sfloat_v MinimumQPt = 1.e-8f;
#else
  static const float    MinimumQPt = 1.e-8f;
#endif
  /**
   *  use find TODO:describe
   */
  static const float MinTrackPurity = .8;

  /**
   *  if p < ExtraThresholdL  track is not reconstructed
   *  for link performance
   */
  static const float ExtraThresholdL = 0.2;
  
  /**
   *  if p < ExtraThreshold  track is not reconstructed
   */
  static const float ExtraThreshold = 0.05;
  
  /**
   *  if p < RefThreshold  track is not reference track
   */
  static const float RefThreshold = 1.;

  ///mvz start
  static const float XMax = 200.f;//maximum X of TPC
  static const float XMin = 50.f;//maximum X of TPC
  ///mvz end

} // namespace AliHLTTPCCAParameters
//namespace Parameters{  using namespace AliHLTTPCCAParameters; }

namespace AliHLTTPCCAPParameters{ /// parameters for global Performance
  /** 
   *   MC track is reconstructable if:
   *   - it have at least MinimumHitsForMCTrack hits
   *   - momentum of it is not lower than AliHLTTPCCAParameters::ExtraThreshold
   */

  enum{
      /**
     * The minimum number of hits for a MCtrack to be reconstructable
       */
    MinimumHitsForMCTrack = 10,
    
     /**
    * The minimum number of hits for a MCtrack to be reconstructable
       */
    MinimumMCPointsForMCTrack = 10,

      /**
       * MCTrack is reconstructed if correspondent recoTrack has NHits >= MinimumHitsForRecoTrack
       */
    MinimumHitsForRecoTrack = 4
  }; // enum

  /**
   *  max percent of one MCTrack MCPoints in current track should be more than this value, then thrack is not ghost.
   */
  static const float MinTrackPurity = .9;
} //  namespace AliHLTTPCCAPParameters
namespace PParameters{  using namespace AliHLTTPCCAPParameters; }

namespace AliHLTTPCCASPParameters{ /// parameters for Slice Performance
  enum{
      /**
     * The minimum number of rows for a MCtrack to be reconstructable
       */
    MinimumHitsForMCTrack = 5, // 30

    /**
     * The minimum number of consecutive rows for a MCtrack to be reconstructable
     */
    MinimumConsHitsForMCTrack = 5,

      /**
       * MCTrack is reconstructed if correspondent recoTrack has NHits >= MinimumHitsForRecoTrack
       */
    MinimumHitsForRecoTrack = 4
    
  }; // enum

  /**
   *  max percent of one MCTrack MCPoints in current track should be more than this value, then thrack is not ghost.
   */
  static const float MinTrackPurity = PParameters::MinTrackPurity;
} //  namespace AliHLTTPCCASPParameters
namespace SPParameters{   using namespace AliHLTTPCCASPParameters; }

#endif // ALIHLTTPCCAPARAMETERS_H
