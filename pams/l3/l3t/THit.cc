#include "THit.hpp"

//
// public methods
//
// set conformal coordinates relativ to vertex
void THit::SetConformalCoordinates(double vertexx, double vertexy)
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
//
// set cylinder coordinates realtiv to (0,0,0); calculates cylinder coordinates from FX and FY
//
void THit::SetCylinderCoordinates()
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
//
// set fit parameters for sz; calculate from FSErrorScaleSz, FDz
//
void THit::SetFitSz()
{
   FS   = 0.F ;
   FDz2 = 1./ square ( FSErrorScaleSz * FDz );
}


//
// construct and calculate cylinder coordinates and conformal coordinates
//
inline THit::THit(double x, double y, double z, 
                  double dx, double dy, double dz, double charge, 
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
   FPadRow = oldhit.GetPadrow();		// padrow number
   FX = oldhit.GetX();				// x coordinate
   FY = oldhit.GetY();				// y coordinate
   FZ = oldhit.GetZ();				// z coordinate
   FCharge = oldhit.GetCharge();		// total charge assigned to this point 
   FConformalX = oldhit.GetConformalX();	// conformal x coordinate
   FConformalY = oldhit.GetConformalY();	// conformal y coordinate 
   FR = oldhit.GetR();				// radius (distance to (0,0,0)
   FPhi = oldhit.GetPhi();			// azimuthal angle
   FEta = oldhit.GetEta();			// hit pseudorapidity
   FS = oldhit.GetS();				// Track trajectory
   FDx = oldhit.GetDx();			// error on the x coordinate
   FDy = oldhit.GetDy();			// error on the y coordinate
   FDz = oldhit.GetDz();			// error on the z coordinate
   FDz2 = oldhit.GetDz2();			// error on z   square
   FDxy2 = oldhit.GetDxy2();			// error on x-y square
   FDphi = oldhit.GetDphi();			// Error in phi
   FChi2Xy = oldhit.GetChi2Xy();		// Chi2 in xy
   FChi2Sz = oldhit.GetChi2Sz();		// Chi2 in sz
}
