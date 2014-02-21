#ifndef __StarAgmlChecker_h__
#define __StarAgmlChecker_h__

#include "TGeoChecker.h"
#include "TObjectSet.h"
#include "TMath.h"

class StarAgmlChecker : public TGeoChecker
{
 public:
  StarAgmlChecker( TGeoManager *manager );
  ~StarAgmlChecker(){ /* nada */ };

  /// Produces plots showing the total amount of material within the top 
  /// volume, and the total amount of material in all of the daughters of
  /// the top volume.  Material is measured in number of radiation lengths.
  /// The plot is a 2D plot phi vs eta, wrapped by a TObjectSet.
  /// @param top Top volume
  /// @param nEta number of bins in eta
  /// @param mnEta minimum eta
  /// @param mxEta maximum eta
  /// @param nPhi number of bins in phi
  /// @param mnPhi minimum phi
  /// @param mxPhi maximum phi
  /// @param rmin  minimum radius
  /// @param rmax  maximum radius
  TObjectSet *MaterialPlot( const Char_t   *top   ="CAVE"  , 
			    const Int_t     nEta  =   100  , 
			    const Double_t  mnEta = -5.00  ,
			    const Double_t  mxEta = +5.00  ,
			    const Int_t     nPhi  =   360  ,
			    const Double_t  mnPhi = -TMath::Pi() ,
			    const Double_t  mxPhi = +TMath::Pi() ,
			    const Double_t  rmin  =    0.0 ,
			    const Double_t  rmax  =    400.0, // ~30cm outside of MAGP
			    const Double_t  zmin  = -4000.0,
			    const Double_t  zmax  = +4000.0,
			    const Option_t *opts = "top" );

 private:
 protected:
  void Fill( TObjectSet *set, Double_t rmin, Double_t rmax, Double_t zmin, Double_t zmax );

  ClassDef(StarAgmlChecker,1);

};

#endif
