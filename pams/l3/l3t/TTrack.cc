#include "TTrack.hpp"
//
// add new hit to track (only pointer to hit), update fit and mark hit as used
//
void TTrack::AddHit(THit* NewValue, int requiredhitsforfit, BOOL dofitsz)
{
	double dxy2, confx, confy, dz2, s, z, dd_xy, dd_sz;
//
// add hit to hitlist
//
   FHits.append(NewValue);
//
// mark hit as used
//
   NewValue->SetTrack(this);
//
// update chi2
// Check whether a fit update is needed
//
   if ( FHits.size() < requiredhitsforfit ) return ;
//
// Include hit in xy fit parameter calculation
//
   dxy2 = NewValue->GetDxy2();
   confx = NewValue->GetConformalX();
   confy = NewValue->GetConformalY();
   Fit.s11_xy += dxy2;
   Fit.s12_xy += dxy2 * confx;
   Fit.s22_xy += dxy2 * square(confx) ;
   Fit.g1_xy  += dxy2 * confy;
   Fit.g2_xy  += dxy2 * confx * confy;
 
   if ( FHits.size() > requiredhitsforfit)
   {
      dd_xy  = Fit.s11_xy * Fit.s22_xy - square ( Fit.s12_xy ) ;
      Fit.a1_xy  = ( Fit.g1_xy * Fit.s22_xy - Fit.g2_xy * Fit.s12_xy ) / dd_xy ;
      Fit.a2_xy  = ( Fit.g2_xy * Fit.s11_xy - Fit.g1_xy * Fit.s12_xy ) / dd_xy ;
   }
//
// Now in the sz plane, if fit required
//
   if ( dofitsz ) 
   {
      dz2 = NewValue->GetDz2();
      s = NewValue->GetS();
      z = NewValue->GetZ();
      Fit.s11_sz += dz2;
      Fit.s12_sz += dz2 * s;
      Fit.s22_sz += dz2 * s * s;
      Fit.g1_sz  += dz2 * z;
      Fit.g2_sz  += dz2 * s * z;

      if ( FHits.size() > requiredhitsforfit ) 
      {
         dd_sz  = Fit.s11_sz * Fit.s22_sz -  Fit.s12_sz * Fit.s12_sz ;
         Fit.a1_sz  = ( Fit.g1_sz * Fit.s22_sz - Fit.g2_sz * Fit.s12_sz ) / dd_sz ;
         Fit.a2_sz  = ( Fit.g2_sz * Fit.s11_sz - Fit.g1_sz * Fit.s12_sz ) / dd_sz ;
      }
   }
}

// add track-segment to track
void TTrack::AddSegment(TTrack& NewValue, float vertexx, float vertexy, float vertexr, float vertexphi, BOOL dofitsz)
{
   double dd_xy, dd_sz;
//   Get circle parameters
   Fit.s11_xy += NewValue.Fit.s11_xy;
   Fit.s12_xy += NewValue.Fit.s12_xy;
   Fit.s22_xy += NewValue.Fit.s22_xy;
   Fit.g1_xy  += NewValue.Fit.g1_xy;
   Fit.g2_xy  += NewValue.Fit.g2_xy;

/*		fault? you don't store this information!
   double dd_xy = Fit.s11_xy * Fit.s22_xy - square ( Fit.s12_xy ) ;
   double a1_xy = ( Fit.g1_xy * Fit.s22_xy - Fit.g2_xy * Fit.s12_xy ) / dd_xy ;
   double a2_xy = ( Fit.g2_xy * Fit.s11_xy - Fit.g1_xy * Fit.s12_xy ) / dd_xy ;
*/
   dd_xy = Fit.s11_xy * Fit.s22_xy - square ( Fit.s12_xy ) ;
   Fit.a1_xy = ( Fit.g1_xy * Fit.s22_xy - Fit.g2_xy * Fit.s12_xy ) / dd_xy ;
   Fit.a2_xy = ( Fit.g2_xy * Fit.s11_xy - Fit.g1_xy * Fit.s12_xy ) / dd_xy ;
//     Now in the sz plane
   if ( dofitsz ) 
	{
		Fit.s11_sz += NewValue.Fit.s11_sz  ;
		Fit.s12_sz += NewValue.Fit.s12_sz  ;
		Fit.s22_sz += NewValue.Fit.s22_sz  ;
		Fit.g1_sz  += NewValue.Fit.g1_sz   ;
		Fit.g2_sz  += NewValue.Fit.g2_sz   ;

/*		fault? you don't store this information!
		double dd_sz  = Fit.s11_sz * Fit.s22_sz - square ( Fit.s12_sz ) ;
		double a1_sz  = ( Fit.g1_sz * Fit.s22_sz - Fit.g2_sz * Fit.s12_sz ) / dd_sz ;
		double a2_sz  = ( Fit.g2_sz * Fit.s11_sz - Fit.g1_sz * Fit.s12_sz ) / dd_sz ;
*/
		dd_sz  = Fit.s11_sz * Fit.s22_sz - square ( Fit.s12_sz ) ;
		Fit.a1_sz  = ( Fit.g1_sz * Fit.s22_sz - Fit.g2_sz * Fit.s12_sz ) / dd_sz ;
		Fit.a2_sz  = ( Fit.g2_sz * Fit.s11_sz - Fit.g1_sz * Fit.s12_sz ) / dd_sz ;
	}
	//  Add hits to first track
    if ( NewValue.GetHits()->head()->GetPadrow() < FHits.head()->GetPadrow() )
	{
		// append new track to track
		FHits.conc(*NewValue.GetHits());
    }
	else 
	{
		// insert new track before track
		NewValue.GetHits()->conc(FHits);
		FHits.conc(*NewValue.GetHits());
	}
	// update chi-squares
	FChi2[0] += NewValue.GetChi2(0);
	FChi2[1] += NewValue.GetChi2(1);
	// Update track parameters (this is intended only for primaries!)
	FinishPrimaryTrack(vertexx, vertexy, vertexr, vertexphi, dofitsz);
}
//
// finish primary track; calculates track-parameters from intermediate parameters (was fill_primary)
//
void TTrack::FinishPrimaryTrack(float vertexx, float vertexy, float vertexr, float vertexphi, BOOL dofitsz)
{
	double dx_last, dy_last, xc, yc, rc, xcp, ycp ; 
	double angle_vertex, angle_last, d_angle ;

	// Get circle parameters
	xcp = - Fit.a2_xy / ( 2. * Fit.a1_xy ) ;
	ycp = - 1.   /  ( 2. * Fit.a1_xy ) ;
	xc = xcp + vertexx;
	yc = ycp + vertexy;

	// Get track parameters
	angle_vertex  = atan2 ( -ycp, -xcp ) ;
	if ( angle_vertex < 0. ) angle_vertex += 2. * Pi ;

	dx_last    = FHits.tail()->GetX() - xc ;
	dy_last    = FHits.tail()->GetY() - yc ;
	angle_last = atan2 ( dy_last, dx_last ) ;
	if ( angle_last < 0. ) angle_last = angle_last + 2. * Pi;

// Get the rotation
	d_angle = angle_last - angle_vertex;

	if ( d_angle >  Pi ) d_angle =   d_angle - 2 * Pi;
	if ( d_angle < -Pi ) d_angle =   d_angle + 2 * Pi;
// positive or negative charge?
	FCharge = ( ( d_angle < 0 ) ? 1 : -1 );
// first point of a primary track is vertex
	FR0   = vertexr;
	FPhi0 = vertexphi;
	FPsi  = (angle_vertex - FCharge * 0.5F * Pi);
	if ( FPsi < 0.0 ) 
		FPsi = (FPsi + 2.F * Pi );
	if ( FPsi > 2. * Pi) 
		FPsi = (FPsi - 2.F * Pi );
	rc = sqrt ( Fit.a2_xy * Fit.a2_xy + 1.0 ) / ( 2.0 * fabs(Fit.a1_xy) );
// momentum
	FPt = (2.9979e-3 * FSBField * rc );

// Get z parameters if needed       
	if ( dofitsz ) 
	{
           FTanL = - Fit.a2_sz ;
           FZ0 = (Fit.a1_sz + Fit.a2_sz * ( FSTrack - rc * d_angle * FCharge ) );
	}
	else
	{
		FTanL = FHits.head()->GetZ() /
			(float)sqrt ( FHits.head()->GetX()*FHits.head()->GetX() + 
                          FHits.head()->GetY()*FHits.head()->GetY()) ;
		FZ0 = 0.F ;
	}
//
// Store some more track info
//
	FEta = seta(FPt,(FPt*FTanL));

}
//***************************************************************
// finish secondary track; calculates track-parameters 
// from intermediate parameters (was fill_secondary)
//***************************************************************
void TTrack::FinishSecondaryTrack(BOOL dofitsz)
{
	double xc, yc, rc, dx1, dy1, dx2, dy2 ; 
	double angle1, angle2, dangle ;
   
	// Get circle parameters	
	xc = - Fit.a2_xy / ( 2. * Fit.a1_xy ) + FHits.head()->GetX() ;
	yc = - 1.   /  ( 2. * Fit.a1_xy ) + FHits.head()->GetY() ;
	rc = sqrt ( Fit.a2_xy * Fit.a2_xy + 1 ) / ( 2 * fabs(Fit.a1_xy) ) ;
//
// Get angles for initial and final point
// first hit
//
	dx1 = FHits.head()->GetX() - xc ;
	dy1 = FHits.head()->GetY() - yc ;
	angle1 = atan2 ( dy1, dx1 ) ;
	if ( angle1 < 0. ) 
		angle1 = angle1 + 2. * Pi ;
// last hit
	dx2 = FHits.tail()->GetX() - xc ;
	dy2 = FHits.tail()->GetY() - yc ;
	angle2 = atan2 ( dy2, dx2 ) ;
	if ( angle2 < 0. ) 
		angle2 = angle2 + 2. * Pi ;
// Get the rotation
	dangle = angle2 - angle1 ;
	if ( dangle >  Pi ) 
		dangle =   dangle - 2. * Pi  ;
	if ( dangle < -Pi ) 
		dangle =   dangle + 2. * Pi  ;
// positive or negative charged particle?
	FCharge = ( ( dangle < 0 ) ? 1 : -1 ) ;
// cylinder coordinates (of last hit)
	FR0 = (float)sqrt ( square(FHits.tail()->GetX()) +
						square(FHits.tail()->GetY()) ) ;
	FPhi0 = FHits.tail()->GetPhi() ;
	FPsi  = angle2 - FCharge * 0.5 * Pi;
	if ( FPsi < 0 ) 
		FPsi = (FPsi + 2 * Pi );
	rc = sqrt ( Fit.a2_xy * Fit.a2_xy + 1 ) / ( 2 * fabs(Fit.a1_xy) ) ;
// momentum
	FPt  = 2.9979e-3 * FSBField * rc;

// Store information in local table
	if (dofitsz)
		FTanL = - Fit.a2_sz ;
	else
		FTanL = FHits.head()->GetZ() /
			(float)sqrt(square(FHits.head()->GetX()) + square(FHits.head()->GetY()));
//
// z-coordinate of last point
//
	FZ0 = FHits.tail()->GetZ();
}

void TTrack::CalculateDeDx()
{
// calculation done by using truncated mean.
// this fast method fills a small histogram, searches for the global peak
// and uses the neighboring n entries to calculate a weighted average.
}
