#ifndef VTRACK_INC
#define VTRACK_INC
// VTrack-includefile
//
//	This class contains all track-parameters, getters and setters.
//	It is the base class for all tracks and supplies a common interface.
//	This class is derived from CObject to support collection in Lists.
//
//	Parameters:
//		FPt		- transverse momentum
//		FEta		- pseudorapidity
//		FCharge		- integrated charge of track
//		FdEdX		- dE/dX information
//		FChi2		- chi-squares of the momentum fit
//		FPsi		- azimuthal angle of the momentum at (r0, phi0, z0)
//		FTanL		- tangent of dipangle at (r0, phi0, z0)
//		FR0		- r (distance to (0,0,0) in cylinder coordinates) of first point on track
//		FPhi0		- azimuthal angle of the first point on track
//		FZ0		- z coordinate of first point on track

// some defines

// some includes
#include "Common.h"
#include "types.h"
#include "THit.hpp"
// some typedefs


// class declaration

class VTrack
{
protected:
// protected data members
   float    FPt;         // transverse momentum
   float    FEta;        // pseudorapidity
   float    FCharge;     // integrated charge of track
   float    FdEdX;       // dE/dX information
   float    FChi2[2];    // chi-squares of the momentum fit
   float    FPsi;        // azimuthal angle of the momentum at (r0, phi0, z0)
   float    FTanL;       // tangent of dipangle at (r0, phi0, z0)
   float    FR0;         // r (distance to (0,0,0) in cylinder coordinates) of first point on track
   float    FPhi0;       // azimuthal angle of the first point on track
   float    FZ0;         // z coordinate of first point on track
   int      FSecondary;  // !=0: secondary
   THitList FHits;       // list of hits on track 
public:
// public methods
// general constructor
   VTrack() {
      FPt = FEta = FCharge = FdEdX = FChi2[0] = FChi2[1] = FPsi = FTanL = FR0 = FPhi0 = FZ0 = 0.0; 
   };
// special constructors
// set-all constructor
   VTrack(float pt, float eta, float charge, float dedx, float chi21, float chi22, 
          float psi, float tanl, float r0, float phi0, float z0)
   {
      FPt = pt; FEta = eta; FCharge = charge; FdEdX = dedx; FChi2[0] = chi21; FChi2[1] = chi22;
      FPsi = psi; FTanL = tanl; FR0 = r0; FPhi0 = phi0; FZ0 = z0;
   };
//
// delete candidate (remove all hits and mark them as not used)
//
   inline void RemoveTrack() {
//
// loop over all hits, set their 'used' flag to false and clear chi-squares of hits
//
      THit* temp;
      forall(temp, FHits) {temp->SetChi2Xy(0.0); temp->SetChi2Sz(0.0); temp->SetTrack(0);}
//
// remove all hits from hit-list
//
      FHits.clear();
   };
//
// virtual destructor (you want to destroy it's siblings without knowing their classnames)
//
   virtual ~VTrack() {RemoveTrack();};	// does nothing
//
// calculate dE/dX information from track-hits
//
   float dEdX();

// getters
   float GetPt() {return FPt;};		// returns Pt
   float GetEta() {return FEta;};	// returns Eta
   float GetCharge() {return FCharge;};	// returns Charge
   float GetdEdX() {return FdEdX;};	// returns dE/dX
   float GetChi2(int which) {return FChi2[which];};	// returns Chi2s
   float GetPsi() {return FPsi;};		// returns Psi
   float GetTanL() {return FTanL;};		// returns TanL
   float GetR0() {return FR0;};			// returns R0
   float GetPhi0() {return FPhi0;};		// returns Phi0
   float GetZ0() {return FZ0;};			// returns Z0
   int GetSecondary() {return FSecondary;};
// setters
   void SetPt (float NewValue) {FPt = NewValue;};	// sets Pt
   void SetEta (float NewValue)  {FEta = NewValue;};	// sets Eta
   void SetCharge (float NewValue)  {FCharge = NewValue;};	// sets Charge
   void SetdEdX (float NewValue)  {FdEdX = NewValue;};		// sets dE/dX
   void SetChi2 (float NewValue, int which) {FChi2[which] = NewValue;};	// sets Chi2s
   void SetPsi (float NewValue)  {FPsi = NewValue;};		// sets Psi
   void SetTanL (float NewValue)  {FTanL = NewValue;};	        // sets TanL
   void SetR0 (float NewValue)  {FR0 = NewValue;};		// sets R0
   void SetPhi0 (float NewValue)  {FPhi0 = NewValue;};		// sets Phi0
   void SetZ0 (float NewValue)  {FZ0 = NewValue;};		// sets Z0
   void SetSecondary(int NewValue ) {FSecondary = NewValue; };
};

#endif
