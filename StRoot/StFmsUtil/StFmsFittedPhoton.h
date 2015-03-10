// $Id: StFmsFittedPhoton.h,v 1.1 2015/03/10 14:38:54 jeromel Exp $
//
// $Log: StFmsFittedPhoton.h,v $
// Revision 1.1  2015/03/10 14:38:54  jeromel
// First version of FmsUtil from Yuxi Pan - reviewd 2015/02
//
/**
 \file      StFmsFittedPhoton.h
 \brief     Declaration of StFmsFittedPhoton, a photon fitted to an FMS cluster
 \author    Steven Heppelmann <steveheppelmann@gmail.com>
 \author    Yuxi Pan <yuxipan@physics.ucla.edu>
 \author    Thomas Burton <tpb@bnl.gov>
 \date      2014
 \copyright Brookhaven National Lab
 */
#ifndef STROOT_STFMSPOINTMAKER_STFMSFITTEDPHOTON_H_
#define STROOT_STFMSPOINTMAKER_STFMSFITTEDPHOTON_H_

namespace FMSCluster {  // $NMSPC
/**
 Definition of a photon hit (SMD position info and reconstructed energy).

 The photon properties are determined by a shower-shape fit to a cluster of
 FMS towers.
 */
struct StFmsFittedPhoton {
  /** Constructor with optional position and energy. */
  StFmsFittedPhoton(double xx = -1., double yy = -1., double e = 0.)
      : energy(e), x(xx), y(yy) { }
  // Use default copy constructor and assignment operator
  /** Destructor */
  ~StFmsFittedPhoton() { }
  double energy;  ///< Fitted energy
  double x;  ///< Fitted (relative) x-position
  double y;  ///< Fitted (relative) y-position
};  // class StFmsFittedPhoton
}  // namespace FMSCluster

#endif  // STROOT_STFMSPOINTMAKER_STFMSFITTEDPHOTON_H_
