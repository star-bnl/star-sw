#ifndef THIT_INC
#define THIT_INC

// THit-includefile
//
//	This class contains all hit-parameters, getters and setters and additional
//	methods to calculate parameters.
//	This class is derived from CObject to support collection in Lists.
//
//	Parameters:
//		FID    			// ID           
//		FPadRow			// padrow number
//		FX			// x coordinate
//		FY			// y coordinate
//		FZ			// z coordinate
//		FCharge			// total charge assigned to this point 
//		FConformalX		// conformal x coordinate 
//		FConformalY		// conformal y coordinate 
//		FR			// radius (distance to (0,0,0)
//		FPhi			// azimuthal angle
//		FEta			// hit pseudorapidity
//		FS			// Track trajectory           
//		FDx			// error on the x coordinate  
//		FDy			// error on the y coordinate  
//		FDz			// error on the z coordinate  
//		FDz2			// error on z   square        
//		FDxy2			// error on x-y square        
//		FDphi			// Error in phi
//		FChi2Xy			// Chi2 in xy                
//		FChi2Sz			// Chi2 in sz                 


// some defines

// some includes
#include <math.h>
#include "Common.h"     // include common definitions
#include "stdlib.h"
#include "_memory.hpp"  // include the LEDA memory manager (that's the most efficient mm i know)

class TTrack ;
 
// class declaration

class THit 
{
private:
//
// private static members (the same for all hits)
//
   static double FSErrorScaleXy;	// Scaling of x-y-errors (default: 5.0 (ref: pablo))
   static double FSErrorScaleSz;	// Scaling of s-z-errors (default: 3.0 (ref: pablo))
   static double FSPhiMin;	// statics for calculation of phi- and eta-indices
   static double FSPhiMax;
   static int FSNumberOfPhiSlices;
   static double FSPhiSliceMultiplier;
   static double FSEtaMin;
   static double FSEtaMax;
   static int FSNumberOfEtaSlices;
   static double FSEtaSliceMultiplier;
//
// private data members
//
   TTrack* track;       // Track to which hit is assigned, -1 if not used    
   int	   Fid ;        // ID              
   int	   FPadRow;     // padrow number
   double  FX;          // x coordinate
   double  FY;          // y coordinate
   double  FZ;          // z coordinate
   double  FCharge;     // total charge assigned to this point 
   double  FConformalX;	// conformal x coordinate (result of conformal transformation, relativ to vertex)
   double  FConformalY;	// conformal y coordinate (result of conformal transformation, relativ to vertex)
   double  FR;		// radius (distance to (0,0,0)
   double  FPhi;        // azimuthal angle
   double  FEta;        // hit pseudorapidity
   double  FS;	        // Track trajectory
   double  FDx;	        // error on the x coordinate
   double  FDy;	        // error on the y coordinate
   double  FDz;	        // error on the z coordinate
   double  FDz2;        // error on z   square
   double  FDxy2;	// error on x-y square
   double  FDphi;	// Error in phi
   double  FChi2Xy;	// Chi2 in xy
   double  FChi2Sz;	// Chi2 in sz
   int     FMCID;

public:
// constructors
// general constructor
   THit() { 
      Fid = -1 ;
      FPadRow = 0, FX = FY = FZ = FCharge = FConformalX = FConformalY = FR = FPhi = FEta = FS = 0.0;
      FDx = FDy = FDz = FDz2 = FDxy2 = FDphi = FChi2Xy = FChi2Sz = 0.0; 
      track = 0; 
   };
// special constructors
// copy constructor; makes a copy of an input-hit
	THit(THit& oldhit );
// construct and calculate cylinder coordinates and conformal coordinates
	THit(double x, double y, double z, double dx, double dy, double dz, double charge, 
		double vertexx, double vertexy, BOOL DoFitSz);
// destructors
// no special destructor needed

// static methods; used to set static data members
// static data members are common to all hits
   static void SetErrorScaleXy (double NewValue) {FSErrorScaleXy = NewValue;};
   static void SetErrorScaleSz (double NewValue) {FSErrorScaleSz = NewValue;};
   static void SetPhiParameters(double phimin, double phimax, int numphislices)
   {
	FSPhiMin = phimin;
	FSPhiMax = phimax;
	FSNumberOfPhiSlices = numphislices;
	FSPhiSliceMultiplier = FSNumberOfPhiSlices / (FSPhiMax - FSPhiMin);
   };
   static void SetEtaParameters(double etamin, double etamax, int numetaslices) {
	FSEtaMin = etamin;
	FSEtaMax = etamax;
	FSNumberOfEtaSlices = numetaslices;
	FSEtaSliceMultiplier = FSNumberOfEtaSlices / (FSEtaMax - FSEtaMin);
   };
   static int GetNumberOfPhiSlices() {return FSNumberOfPhiSlices;};
   static int GetNumberOfEtaSlices() {return FSNumberOfEtaSlices;};
// public methods
// set conformal coordinates relativ to vertex
   void SetConformalCoordinates(double vertexx, double vertexy);
// set cylinder coordinates realtiv to (0,0,0); calculates cylinder coordinates from FX and FY
   void SetCylinderCoordinates();
// set fit parameters for sz; calculate from FSErrorScaleSz, FDz 
   void SetFitSz();
// get phi index
   WORD GetPhiIndex() ;
// get eta index
   WORD GetEtaIndex();
//
// getters
//
   TTrack* GetTrack() {return track;};		// Track using this hit
   BOOL    GetUsed () {return (track!=0);};	// hit used in track?
   int     GetId    () {return Fid    ;} 	// ID              
   int     GetPadrow() {return FPadRow;} 	// padrow number
   double  GetX() {return FX;};			// x coordinate
   double  GetY() {return FY;};			// y coordinate
   double  GetZ() {return FZ;};		// z coordinate
   double  GetCharge() {return FCharge;};	// total charge assigned to this point 
   double  GetConformalX() {return FConformalX;};// conformal x coordinate 
   double  GetConformalY() {return FConformalY;};// conformal y coordinate 
   double  GetR() {return FR;};			// radius (distance to (0,0,0)
   double  GetPhi() {return FPhi;};		// azimuthal angle
   double  GetEta() {return FEta;};		// hit pseudorapidity
   double  GetS() {return FS;};			// Track trajectory           
   double  GetDx() {return FDx;};		// error on the x coordinate  
   double  GetDy() {return FDy;};		// error on the y coordinate  
   double  GetDz() {return FDz;};		// error on the z coordinate  
   double  GetDz2() {return FDz2;};		// error on z   square        
   double  GetDxy2() {return FDxy2;};		// error on x-y square        
   double  GetDphi() {return FPhi;};		// Error in phi
   double  GetChi2Xy() {return FChi2Xy;};	// Chi2 in xy                
   double  GetChi2Sz() {return FChi2Sz;};	// Chi2 in sz                 
   double  GetMCID() {return FMCID;};		// MC track id for this hit

// setters
   void SetTrack(TTrack*  NewValue) {track = NewValue;};  // track tow which hit is assigned
   void SetId    (int NewValue) {Fid     = NewValue;};	  // ID           
   void SetPadrow(int NewValue) {FPadRow = NewValue;};	  // padrow number
   void SetX(double NewValue) {FX = NewValue;};		  // x coordinate
   void SetY(double NewValue) {FY = NewValue;};		  // y coordinate
   void SetZ(double NewValue) {FZ = NewValue;};		  // z coordinate
   void SetCharge(double NewValue) {FCharge = NewValue;}; // total charge assigned to this point 
   void SetConformalX(double NewValue) {FConformalX = NewValue;};// conformal x coordinate 
   void SetConformalY(double NewValue) {FConformalY = NewValue;};// conformal y coordinate 
   void SetR(double NewValue) {FR = NewValue;};		// radius (distance to (0,0,0)
   void SetPhi(double NewValue) {FPhi = NewValue;};	// azimuthal angle
   void SetEta(double NewValue) {FEta = NewValue;};	// hit pseudorapidity
   void SetS(double NewValue) {FS = NewValue;};		// Track trajectory           
   void SetDx(double NewValue) {FDx = NewValue;};	// error on the x coordinate  
   void SetDy(double NewValue) {FDy = NewValue;};	// error on the y coordinate  
   void SetDz(double NewValue) {FDz = NewValue;};	// error on the z coordinate  
   void SetDz2(double NewValue) {FDz2 = NewValue;};	// error on z   square        
   void SetDxy2(double NewValue) {FDxy2 = NewValue;};	// error on x-y square        
   void SetDphi(double NewValue) {FPhi = NewValue;};	// Error in phi
   void SetChi2Xy(double NewValue) {FChi2Xy = NewValue;}; // Chi2 in xy                
   void SetChi2Sz(double NewValue) {FChi2Sz = NewValue;}; // Chi2 in sz                 
   void SetMCID(double NewValue) {FMCID = (int)NewValue;};// mc track id for this hit

// setup the LEDA memory managment for Hits
   LEDA_MEMORY(THit)
};
//
// construct and calculate cylinder coordinates and conformal coordinates
//
inline THit::THit(double x, double y, double z, double dx, double dy, double dz, double charge, 
		double vertexx, double vertexy, BOOL DoFitSz)
{
// Initialize members
   FX = x; FY = y; FZ = z; FDx = dx; FDy = dy; FDz = dz; FCharge = charge; 
   track = 0 ;
//
// Calculate conformal coordinates
//
   SetConformalCoordinates(vertexx, vertexy);
//
// Calculate cylinder coordinates (relativ to (0,0,0))
//
   SetCylinderCoordinates();
//
// If fit in sz required -> calculate errors
//
   if ( DoFitSz ) SetFitSz();
}


// copy constructor; makes a copy of input-hit
inline THit::THit(THit& oldhit ) 
{
	FPadRow = oldhit.GetPadrow();				// padrow number
	FX = oldhit.GetX();							// x coordinate
	FY = oldhit.GetY();							// y coordinate
	FZ = oldhit.GetZ();							// z coordinate
	FCharge = oldhit.GetCharge();				// total charge assigned to this point 
	FConformalX = oldhit.GetConformalX();		// conformal x coordinate (result of conformal transformation, relativ to vertex)
	FConformalY = oldhit.GetConformalY();		// conformal y coordinate (result of conformal transformation, relativ to vertex)
	FR = oldhit.GetR();							// radius (distance to (0,0,0)
	FPhi = oldhit.GetPhi();						// azimuthal angle
	FEta = oldhit.GetEta();						// hit pseudorapidity
	FS = oldhit.GetS();							// Track trajectory
	FDx = oldhit.GetDx();						// error on the x coordinate
	FDy = oldhit.GetDy();						// error on the y coordinate
	FDz = oldhit.GetDz();						// error on the z coordinate
	FDz2 = oldhit.GetDz2();						// error on z   square
	FDxy2 = oldhit.GetDxy2();					// error on x-y square
	FDphi = oldhit.GetDphi();					// Error in phi
	FChi2Xy = oldhit.GetChi2Xy();				// Chi2 in xy
	FChi2Sz = oldhit.GetChi2Sz();				// Chi2 in sz
}


// public methods
// set conformal coordinates relativ to vertex
inline void THit::SetConformalCoordinates(double vertexx, double vertexy)
{
   double x, y, r2;
// Calculate x, y relativ to vertex
   x = FX - vertexx ;
   y = FY - vertexy ;
   r2 = x * x + y * y  ;

// Calculate conformal coordinates
   FConformalX =   x / r2 ;
   FConformalY = - y / r2 ;
// Calculate error; FSErrorScaleXy is static member (the same for all hits)
   FDxy2  = ( r2 * r2 ) /
			( square ( FSErrorScaleXy ) *
			( square(FDx) + square(FDy) ) ) ;
}

// set cylinder coordinates realtiv to (0,0,0); calculates cylinder coordinates from FX and FY
inline void THit::SetCylinderCoordinates()
{
	double r2;

	// Calculate cylinder coordinates (relativ to (0,0,0))
	r2 = FX * FX + FY * FY  ;
	FR = sqrt ( r2 ) ;
	FPhi = atan2(FY, FX) ;
	if ( FPhi < 0 ) 
		FPhi += 2.F * Pi ;
	// Calculate pseudorapidity (from General.hpp)
	FEta = seta(FR, FZ) ;
}

// set fit parameters for sz; calculate from FSErrorScaleSz, FDz 
inline void THit::SetFitSz()
{
   FS   = 0.F ;
   FDz2 = 1./ square ( FSErrorScaleSz * FDz );
}

// get phi index
inline WORD THit::GetPhiIndex()
{ 
   return (WORD) ((FPhi - FSPhiMin) * FSPhiSliceMultiplier + 1.0);
}

// get eta index
inline WORD THit::GetEtaIndex()
{
   return (WORD) ((FEta - FSEtaMin) * FSEtaSliceMultiplier + 1.0);
}


#endif
