#ifndef __StEEmcPointUtils_h__
#define __StEEmcPointUtils_h__

#include "TObject.h"
#include "StEEmcPoint.h"

const Float_t kEEmcSmdSF=0.006;

class StEEmcPointUtils {

 public:

  /// Returns true if the two points are outside
  /// a given fraction of the minimum opening angle
  /// for a meson with a given mass.  Default is
  /// set for 30% of the pi0 minimum opening angle
  /// at a given energy.
  /// \param p1   point 1
  /// \param p2   point 2
  /// \param energy energy of candidate 
  /// \param mass mass of meson
  /// \param frac fraction of opening angle to test (default=0.3).
  Bool_t opening_angle_cut ( StEEmcPoint &p1, StEEmcPoint &p2,  Float_t energy, Float_t mass=0.135, Float_t frac=0.3 );

  /// Given a vector of points matching some energy in
  /// the endcap, return the best set of points using
  /// the following criteria:
  ///
  /// 1) assume an SMD sampling fraction of 0.6%
  /// 2) predict energy deposit in SMD
  /// 3) compute a "chi^2" = ( epredict - epoint )^2 + 4.0 ( esmdu - esmdv )^2
  ///
  /// The set of npoints which minimizes this "chi^2" will be returned
  ///
  /// NOTE-- points which lie along the same strips will not be considered.
  /// it will be possible that no set of points will find an optimal minimum.
  /// If so, stat returns false.
  ///
  /// \param points Input vector of points
  /// \param em_energy Electormagnetic energy we're fitting to
  /// \param stat false if it fails to find a good set of points
  /// \param chi2 the "chi2" of the minimization
  /// \param npoints the number of points we're looking for.
  StEEmcPointVec_t find_best_points ( StEEmcPointVec_t points, Float_t em_energy, Bool_t &stat, Float_t &chi2, Int_t npoint=1 );

  /// Test if the first npoint points in the vector have SMD strips
  /// which are along the same strips (means are equal)
  Bool_t no_parallel_smd_clusters( StEEmcPointVec_t points, Int_t npoint );

 private:
 protected:
  
  StEEmcPointUtils();
  ~StEEmcPointUtils(){ /* nada */ };

};

#endif
