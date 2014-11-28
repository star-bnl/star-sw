#include "StEEmcPointUtils.h"
#include <algorithm>

// ----------------------------------------------------------------------------
StEEmcPointUtils::StEEmcPointUtils()
{
}

// ----------------------------------------------------------------------------
Bool_t StEEmcPointUtils::opening_angle_cut ( StEEmcPoint &p1, StEEmcPoint &p2, Float_t energy, Float_t mass, Float_t frac )
{
  Float_t oangle = p1.position().Angle( p2.position() );
  Float_t oangle_min = 2.0 * mass * frac / energy; 
  return (oangle>oangle_min);
}

// ----------------------------------------------------------------------------
StEEmcPointVec_t StEEmcPointUtils::find_best_points( StEEmcPointVec_t points, Float_t em_energy, Bool_t &stat, Float_t &chi2, Int_t npoint )
{  

  /// Check sanity.  Number of requested points must
  /// be no bigger than our vector.
  chi2=1.0E9;
  if ( npoint > (Int_t)points.size() ) {
    stat=false;
    return points;
  }

  /// Loop over all permutations.  Calculate chi2 for
  /// the firsn npoint points.
  Bool_t go=true;
  Float_t chi2_min=1.0E9;
  StEEmcPointVec_t good_perm;
  while ( go ) {

    /// require all smd clusters in permutation 
    /// to be on different smd strips
    if ( no_parallel_smd_clusters( points, npoint ) )
      for ( Int_t ipoint=0; ipoint < npoint; ipoint++ ) {	
	Float_t edep_predict = 2.0 * kEEmcSmdSF * em_energy;	
	Float_t ediff1 = points[ipoint].energy() - edep_predict;
	Float_t ediff2 = points[ipoint].cluster(0).energy() - points[ipoint].cluster(1).energy();
	ediff2 *= 2.0;
	Float_t mychi2=ediff1*ediff1 + ediff2*ediff2;
	if ( mychi2 < chi2_min ) { 
	  good_perm=points;
	  chi2_min=mychi2;
	}
	
      }

    go = std::next_permutation ( points.begin(), points.end() );

  }

  return points;

}

// ----------------------------------------------------------------------------
Bool_t StEEmcPointUtils::no_parallel_smd_clusters( StEEmcPointVec_t points, Int_t npoint )
{

  if ( npoint <= 1 ) return true;

  for ( Int_t i = 0; i < npoint-1; i++ ) {
    Float_t u1=points[i].cluster(0).mean();
    Float_t v1=points[i].cluster(1).mean();
    for ( Int_t j = i+1; j < npoint; j++ ) {
      Float_t u2=points[j].cluster(0).mean();
      Float_t v2=points[j].cluster(1).mean();
      if ( u1==u2 ) return false;
      if ( v1==v2 ) return false;
    }
  }

  /// no clusters found along parallel strips
  return true;

}
