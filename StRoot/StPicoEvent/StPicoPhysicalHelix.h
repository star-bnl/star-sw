/**
 * \class StPicoPhysicalHelix
 * \brief Helis parametrization for the particle
 * 
 * Parametrization of a physical helix (modification of StPhysicalHelix)
 * that uses ROOT classes
 *
 * \author Grigory Nigmatkulov 
 * \date May 07 2018
 */

#ifndef StPicoPhysicalHelix_h
#define StPicoPhysicalHelix_h

// ROOT headers
#include "TVector3.h"

// PicoDst headers
#include "StPicoHelix.h"

//_________________
class StPicoPhysicalHelix : public StPicoHelix {
  
 public:

  /// Empty constructor
  StPicoPhysicalHelix();
  /// Constructor with momentum, origin, signed Magnetic Field
  /// and Charge of particle (+/- 1)
  StPicoPhysicalHelix(const TVector3&,
		      const TVector3&,
		      Double_t, Double_t);
  /// Constructor with Curvature, dip angle, phase, origin, h
  StPicoPhysicalHelix(Double_t, Double_t, Double_t,
		      const TVector3&, Int_t h=-1);
  /// Destructor
  ~StPicoPhysicalHelix();

  /// Return the momentum at origin
  ///    \param bField magnetic field
  TVector3 momentum(Double_t) const;
  /// Return momemtum at S
  TVector3 momentumAt(Double_t, Double_t) const;
  /// Return charge of a particle
  Int_t charge(Double_t)   const;
  /// 2d DCA to x,y point signed relative to curvature
  Double_t curvatureSignedDistance(Double_t x, Double_t y) ;
  /// 2d DCA to x,y point signed relative to rotation 
  Double_t geometricSignedDistance(Double_t x, Double_t y) ;
  /// 3d DCA to 3d point signed relative to curvature
  Double_t curvatureSignedDistance(const TVector3&) ;
  /// 3d DCA to 3d point signed relative to rotation
  Double_t geometricSignedDistance(const TVector3&) ;
    
  ClassDef(StPicoPhysicalHelix,1)
};

#endif
