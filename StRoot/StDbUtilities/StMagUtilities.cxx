/***********************************************************************
 *
 * $Id: StMagUtilities.cxx,v 1.14 2001/06/15 00:52:15 jhthomas Exp $
 *
 * Author: Jim Thomas   11/1/2000
 *
 ***********************************************************************
 *
 * Description: Utilities for the Magnetic Field
 *
 ***********************************************************************
 *
 * $Log: StMagUtilities.cxx,v $
 * Revision 1.14  2001/06/15 00:52:15  jhthomas
 * Protect discontinuity in distortions at CM
 *
 * Revision 1.13  2001/06/14 22:12:11  jhthomas
 * Speedup UndoBDistorion by adding table lookups
 *
 * Revision 1.12  2001/06/13 16:36:43  jhthomas
 * Improve the speed and timing of the PadRow13 correction.
 * Add 3D magnetic field functions so now both 2D and 3D are availble.
 *
 * Revision 1.3  2000/12/15 16:10:45  jhthomas
 * Add PadRow13, Clock, and Twist corrections to UndoDistortion
 *
 * Revision 1.2  2000/11/03 02:41:58  jhthomas
 * Added CVS comment structure to .h and .cxx files
 *
 ***********************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMagUtilities Class                                                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

// 
// StMagUtilities - Jim Thomas 10 October 2000
// A package of Bfield routines and distortion corrections for STAR.  
// Methods included to read the correct Bfield map and scale it 
// according to a scale factor provided during instantiation.
// All corrections automatically adjust themselves for different
// B field settings and E field settings.  Even reversed fields. 
// An enumerated argument provided at the time of instantiation selects
// a constant field (map=1) or the interpolation grid (map=2).
// Work in kGauss, cm - but note that the Bfield maps on disk are in gauss, cm.
//
// To do:  
// Pull parameters out of DB rather than from #define.  kChain version and Standalone version.
// Add a routine to distort the track if we are given a Geant Vector full of points == a track
// Add simulated B field map in the regions where the field is not mapped.
//

#define  nZ               57            // Standard STAR B field Map. Number of Z points in table
#define  nR               28            // Number of R points in table
#define  nPhi             37            // Number of Phi points in table
#define  neZ              69            // Standard STAR E field Map. Number of Z points in table
#define  neR              33            // Number of R points in table
#define  nePhi            13            // Number of Phi points in table ( add one for 360 == 0 )

#define  StarDriftV     5.45            // STAR Drift Velocity (cm/microSec) Magnitude
#define  StarMagE      148.0            // STAR Electric Field (V/cm) Magnitude
#define  GG           -127.5            // Gating Grid voltage (volts)

#define  TPC_Z0        208.7            // Z location of STAR TPC Gating Grid (cm)
#define  XTWIST       -0.379            // X Displacement of West end of TPC wrt magnet (mRad)
#define  YTWIST        0.153            // Y Displacement of West end of TPC wrt magnet (mRad)
#define  EASTCLOCKERROR  0.0            // Phi rotation of East end of TPC in milli-radians
#define  WESTCLOCKERROR -0.43           // Phi rotation of West end of TPC in milli-radians

#include "StMagUtilities.h"
#define   gufld gufld_
extern   "C" { void gufld(Float_t *, Float_t *) ; }

static EBField  gMap  =  kUndefined ;   // Global flag to indicate static arrays are full
static Float_t  gFactor  = 1.0 ;        // Multiplicative factor (allows scaling and sign reversal)
static Float_t  gRescale = 1.0 ;        // Multiplicative factor (allows re-scaling wrt which map read)
static Float_t  Const_0, Const_1, Const_2 ;  // OmegaTau parameters
static Float_t  Bz[nZ][nR], Br[nZ][nR] ;         
static Float_t  Radius[nR], ZList[nZ] ;         
static Float_t  Bz3D[nPhi][nZ][nR], Br3D[nPhi][nZ][nR], Bphi3D[nPhi][nZ][nR] ;         
static Float_t  R3D[nR], Z3D[nZ], Phi3D[nPhi] ;         
static Float_t  Ephi[neZ][nePhi][neR], Er[neZ][nePhi][neR] ;
static Float_t  EEphi[neZ][nePhi][neR], EEr[neZ][nePhi][neR] ;
static Float_t  eRadius[neR], ePhiList[nePhi], eZList[neZ]  ;         

//________________________________________


ClassImp(StMagUtilities)

StMagUtilities::StMagUtilities( )

{                                           // StMagUtilities constructor

  Float_t  B[3], X[3] = { 0, 0, 0 } ;

  if ( gMap == kUndefined ) 
    {
      gufld(X,B) ;                          // Read crude values from Chain to get scale
      gFactor = B[2] / 4.980 ;              // Select factor based on Chain values (kGauss) 
      gMap = kMapped ;                      // Do once & Select the B field map (mapped field or constant)
      Init() ;                              // Read the Magnetic and Electric Field Data Files, set constants
    }

}

StMagUtilities::StMagUtilities( const EBField map = kMapped, const Float_t factor = 1.0 )

{                                           // StMagUtilities constructor

  if ( gMap == kUndefined ) 
    {
      gFactor = factor ;
      gMap = map ;                          // Do once & select the requested map (mapped or constant)
      Init() ;                              // Read the Magnetic and Electric Field Data Files, set constants
    }
   
  if ( gMap != map || factor != gFactor ) 
    {
      cout << "StMagUtilities Warning: The Maps have already been read and scaled." << endl
	   << "                        Try instantiating with StMagUtilities( <empty> ) " << endl ;
    }

}

void StMagUtilities::Init ( )

{

  Float_t  B[3], X[3] = { 0, 0, 0 } ;
  Float_t  OmegaTau ;                       // OmegaTau carries the sign opposite of B for an electron

  ReadField() ;                             // Read the Magnetic and Electric Field Data Files
  BField(X,B) ;                             // Work in kGauss, cm and assume Bz dominates

  // Theoretically, OmegaTau is defined as shown in the next line.  
  // OmegaTau   =  -10. * B[2] * StarDriftV / StarMagE ;  // cm/microsec, Volts/cm
  // Instead, we will use scaled values from the Aleph collaboration

  OmegaTau   =  -11. * B[2] * StarDriftV / StarMagE ;  // B in kGauss, note that the sign of B is important 
  Const_0    =  1. / ( 1. + pow( OmegaTau, 2 ) ) ;
  Const_1    =  OmegaTau / ( 1. + pow( OmegaTau, 2 ) ) ;
  Const_2    =  pow( OmegaTau, 2 ) / ( 1. + pow( OmegaTau, 2 ) ) ;
  cout << "OmegaTau = " << OmegaTau << "   Please wait for the tables to fill ... ~60 seconds" << endl ;

}

//________________________________________
//
// Main Entry Point for requests for B field in Cartesian coordinates
//
//________________________________________

void StMagUtilities::BField( const Float_t x[3], Float_t B[3] )

{                          

  Float_t r, z, Br_value, Bz_value ;

  z  = x[2] ;
  r  = TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  
  if ( r != 0.0 )
    {
      InterpolateBfield( r, z, Br_value, Bz_value ) ;
      B[0] = Br_value * (x[0]/r) ;
      B[1] = Br_value * (x[1]/r) ;
      B[2] = Bz_value ; 
    }
  else
    {
      InterpolateBfield( r, z, Br_value, Bz_value ) ;
      B[0] = Br_value ;
      B[1] = 0.0 ;
      B[2] = Bz_value ;
    }

}

void StMagUtilities::B3DField( const Float_t x[3], Float_t B[3] )

{                          

  Float_t r, z, phi, Br_value, Bz_value, Bphi_value ;

  z  = x[2] ;
  r  = TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  
  if ( r != 0.0 )
    {
      phi = TMath::ATan2( x[1], x[0] ) ;
      if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
      Interpolate3Dfield( r, z, phi, Br_value, Bz_value, Bphi_value ) ;
      B[0] = Br_value * (x[0]/r) - Bphi_value * (x[1]/r) ;
      B[1] = Br_value * (x[1]/r) + Bphi_value * (x[0]/r) ;
      B[2] = Bz_value ; 
    }
  else
    {
      phi = 0 ;
      Interpolate3Dfield( r, z, phi, Br_value, Bz_value, Bphi_value ) ;
      B[0] = Br_value ;
      B[1] = Bphi_value ;
      B[2] = Bz_value ;
    }

  return ;

}

//________________________________________
//
// Main Entry Point for requests for B field in Radial coordinates
//
//________________________________________

void StMagUtilities::BrBzField( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value )

{
  
  InterpolateBfield( r, z, Br_value, Bz_value ) ;

}

void StMagUtilities::BrBz3DField( const Float_t r, const Float_t z, const Float_t phi, 
				  Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value )

{

  Float_t phiprime ;
  phiprime = phi ;
  if ( phiprime < 0 ) phiprime += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  Interpolate3Dfield( r, z, phiprime, Br_value, Bz_value, Bphi_value ) ;

}

//________________________________________
//
// Main Entry Point for requests to Undo the E and B field distortions
//
//________________________________________

void StMagUtilities::UndoDistortion( const Float_t x[3], Float_t Xprime[3] )

{

  Float_t Xprime1[3], Xprime2[3] ;
  
  FastUndoBDistortion    ( x, Xprime1 ) ;
  UndoPad13Distortion    ( Xprime1, Xprime2 ) ;
  UndoTwistDistortion    ( Xprime2, Xprime1 ) ;
  UndoClockDistortion    ( Xprime1, Xprime2 ) ;
  UndoMembraneDistortion ( Xprime2, Xprime ) ; 
  //UndoMembraneDistortion ( Xprime2, Xprime1 ) ;  // Replace the previous line with these two 
  //UndoEndcapDistortion   ( Xprime1, Xprime ) ;   // to enable the Endcap distortion corrections
  
}

//________________________________________
//
// Main Entry Point for requests to Do the E and B field distortions (for simulations)
//
//________________________________________

void StMagUtilities::DoDistortion( const Float_t x[3], Float_t Xprime[3] )

{

  UndoDistortion ( x, Xprime ) ;

  Xprime[0] = 2*x[0] - Xprime[0] ;
  Xprime[1] = 2*x[1] - Xprime[1] ;
  Xprime[2] = 2*x[2] - Xprime[2] ;

}

//________________________________________


void StMagUtilities::UndoBDistortion( const Float_t x[3], Float_t Xprime[3] )

{

  Double_t ah ;                             // ah carries the sign opposite of E (for forward integration)
  Float_t  B[3] ; 
  Int_t    sign, index = 1 , NSTEPS ;              
  
  if ( x[2] >= 0.0 ) sign =  1 ;                       // (TPC West)
  else               sign = -1 ;                       // (TPC East)  

  Xprime[0]  =  x[0] ;                                 // Integrate backwards from TPC plane to 
  Xprime[1]  =  x[1] ;                                 // the point the electron cluster was born. 
  Xprime[2]  =  sign * TPC_Z0 ;                        // Prepare for different readout planes

  for ( NSTEPS = 5 ; NSTEPS < 1000 ; NSTEPS += 2 )     // Choose ah to be about 1.0 cm, NSTEPS must be odd
    {
      ah = ( x[2] - sign * TPC_Z0 ) / ( NSTEPS - 1 ) ; // Going Backwards! See note above.
      if ( TMath::Abs(ah) < 1.0 ) break ;
    }

  for ( Int_t i = 1; i <= NSTEPS; ++i )                // Simpson's Integration Loop
    {
      if ( i == NSTEPS ) index = 1 ;
      Xprime[2] +=  index*(ah/3) ;
      B3DField( Xprime, B ) ;                          // Work in kGauss, cm
      Xprime[0] +=  index*(ah/3)*( Const_2*B[0] - Const_1*B[1] ) / B[2] ;
      Xprime[1] +=  index*(ah/3)*( Const_2*B[1] + Const_1*B[0] ) / B[2] ;
      if ( index != 4 ) index = 4; else index = 2 ;
    }    

}


#define NPOINTS 22                                  // Number of points on the Z interpolation grid

void StMagUtilities::FastUndoBDistortion( const Float_t x[3], Float_t Xprime[3] )

{
 
  static  Int_t   DoOnce = 0 ;
  static  Float_t xarray[2*NPOINTS-1], yarray[2*NPOINTS-1], zarray[NPOINTS] ;
  static  Float_t dXplus[2*NPOINTS-1][2*NPOINTS-1][NPOINTS], dYplus[2*NPOINTS-1][2*NPOINTS-1][NPOINTS] ;
  static  Float_t dXminus[2*NPOINTS-1][2*NPOINTS-1][NPOINTS], dYminus[2*NPOINTS-1][2*NPOINTS-1][NPOINTS] ;

  const   Int_t ORDER = 2 ;                         // Linear interpolation = 1, Quadratic = 2         
  Int_t   i, j, k ;
  Int_t   ilow, jlow, klow ;
  Float_t xx[3] ;
  Float_t save_dX[ORDER+1], saved_dX[ORDER+1] ;
  Float_t save_dY[ORDER+1], saved_dY[ORDER+1] ;

  if ( DoOnce == 0 )

    {
      for ( i = 0 ; i < 2*NPOINTS-1 ; i++ )
	{
	  xarray[i] = -1*TPC_Z0 + i*TPC_Z0/(NPOINTS-1) ;
	  xx[0] = xarray[i] ;
	  for ( j = 0 ; j < 2*NPOINTS-1 ; j++ )
	    {
	      yarray[j] = -1*TPC_Z0 + j*TPC_Z0/(NPOINTS-1) ;
	      xx[1] = yarray[j] ;
	      for ( k = 0 ; k < NPOINTS ; k++ )
		{
		  zarray[k] = k * TPC_Z0/(NPOINTS-1) ;
		  xx[2] = zarray[k] ;
		  if ( k == 0 ) xx[2] = 0.1 ;       // Stay off central membrane by a tiny bit
		  UndoBDistortion(xx,Xprime) ;
		  dXplus[i][j][k] = Xprime[0] ;
		  dYplus[i][j][k] = Xprime[1] ;
		  xx[2] = -1*zarray[k] ;            // Note sign flip for Z < 0
		  if ( k == 0 ) xx[2] = -0.1 ;      // Stay off central membrane by a tiny bit
		  UndoBDistortion(xx,Xprime) ;
		  dXminus[i][j][k] = Xprime[0] ;
		  dYminus[i][j][k] = Xprime[1] ;
		}
	    }
	}
      DoOnce = 1 ;
    }

  if ( x[2] >= 0 ) 
    {
      Search( 2*NPOINTS-1, xarray, x[0], ilow ) ;
      Search( 2*NPOINTS-1, yarray, x[1], jlow ) ;
      Search( NPOINTS,     zarray, x[2], klow ) ;
      if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
      if ( jlow < 0 ) jlow = 0 ;
      if ( klow < 0 ) klow = 0 ;
      if ( ilow + ORDER  >=  2*NPOINTS - 2 ) ilow =  2*NPOINTS - 2 - ORDER ;
      if ( jlow + ORDER  >=  2*NPOINTS - 2 ) jlow =  2*NPOINTS - 2 - ORDER ;
      if ( klow + ORDER  >=  NPOINTS - 1 )   klow =  NPOINTS - 1 - ORDER ;
      
      for ( i = ilow ; i < ilow + ORDER + 1 ; i++ )
	{
	  for ( j = jlow ; j < jlow + ORDER + 1 ; j++ )
	    {
	      save_dX[j-jlow] = Interpolate( &zarray[klow], &dXplus[i][j][klow], ORDER, x[2] )   ;
	      save_dY[j-jlow] = Interpolate( &zarray[klow], &dYplus[i][j][klow], ORDER, x[2] )   ;
	    }
	  saved_dX[i-ilow] = Interpolate( &yarray[jlow], save_dX, ORDER, x[1] )   ; 
	  saved_dY[i-ilow] = Interpolate( &yarray[jlow], save_dY, ORDER, x[1] )   ; 
	}
      Xprime[0] = Interpolate( &xarray[ilow], saved_dX, ORDER, x[0] )   ;
      Xprime[1] = Interpolate( &xarray[ilow], saved_dY, ORDER, x[0] )   ;
      Xprime[2] = x[2] ;
    }
  else
    {
      Search( 2*NPOINTS-1, xarray, x[0], ilow ) ;
      Search( 2*NPOINTS-1, yarray, x[1], jlow ) ;
      Search( NPOINTS,     zarray, -1*x[2], klow ) ;      // Note sign flip for Z < 0
      if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
      if ( jlow < 0 ) jlow = 0 ;
      if ( klow < 0 ) klow = 0 ;
      if ( ilow + ORDER  >=  2*NPOINTS - 2 ) ilow =  2*NPOINTS - 2 - ORDER ;
      if ( jlow + ORDER  >=  2*NPOINTS - 2 ) jlow =  2*NPOINTS - 2 - ORDER ;
      if ( klow + ORDER  >=  NPOINTS - 1 )   klow =  NPOINTS - 1 - ORDER ;
      
      for ( i = ilow ; i < ilow + ORDER + 1 ; i++ )
	{
	  for ( j = jlow ; j < jlow + ORDER + 1 ; j++ )
	    {
	      save_dX[j-jlow] = Interpolate( &zarray[klow], &dXminus[i][j][klow], ORDER, -1*x[2] )   ;
	      save_dY[j-jlow] = Interpolate( &zarray[klow], &dYminus[i][j][klow], ORDER, -1*x[2] )   ;
	    }
	  saved_dX[i-ilow] = Interpolate( &yarray[jlow], save_dX, ORDER, x[1] )   ; 
	  saved_dY[i-ilow] = Interpolate( &yarray[jlow], save_dY, ORDER, x[1] )   ; 
	}
      Xprime[0] = Interpolate( &xarray[ilow], saved_dX, ORDER, x[0] )   ;
      Xprime[1] = Interpolate( &xarray[ilow], saved_dY, ORDER, x[0] )   ;
      Xprime[2] = x[2] ;
    }
  
}

//________________________________________


#define  GAP13_14      1.595            // Width of the gap between the grids at row 13 and row 14 (cm)
#define  GAPRADIUS     121.8            // Radius of gap between rows 13 & 14 at phi = zero degrees (cm)
#define  NYARRAY       33               // Dimension of the vector to contain the YArray
#define  NZDRIFT       15               // Dimension of the vector to contain ZDriftArray

void StMagUtilities::UndoPad13Distortion( const Float_t x[3], Float_t Xprime[3] )

{

  const Int_t   TERMS    = 400 ;                 // Number of terms in the sum
  const Float_t SCALE    = 0.192 ;               // Set the scale for the correction
  const Float_t BOX      = 200.0 - GAPRADIUS ;   // Size of the box in which to work
  const Float_t PI       = TMath::Pi() ;

  static Float_t ZDriftArray[NZDRIFT] = {0,1,2,3,4,5,7.5,10,15,20,30,50,75,100,220} ;
  static Float_t YArray[NYARRAY] = { 50.0, 75.0,  100.0,
				     103.5, 104.0, 104.5, 
				     108.7, 109.2, 109.7,
				     113.9, 114.4, 114.9,
				     118.9, 119.6, 119.8, 
				     120.1, 120.5, 121.0, 121.5, 122.1, 122.6, 125.2, 
				     126.2, 127.195, 
				     128.2, 129.195,
				     130.2, 131.195,
				     132.2, 133.195, 137.195, 150., 200. } ;

  static Double_t C[TERMS] ;                     // Coefficients for series
  static Int_t    Flag = 0 ;                     // Calculate only once
  static Float_t  SumArray[NZDRIFT][NYARRAY] ;
  static Int_t    ilow, jlow, ORDER ;

  Float_t  y, Zdrift, save_sum[3] ;
  Double_t r, phi, phi0, sum = 0.0 ;

  if ( Flag == 0 ) 
    {                          // Put these coefficients in a table to save time
      C[0] = GAP13_14 * GG * SCALE / ( 2 * BOX ) ;   
      for ( Int_t i = 1 ; i < TERMS ; i++ )
	  C[i] = 2 * GG * SCALE * TMath::Sin( GAP13_14*i*PI/( 2*BOX ) ) / ( i * PI ) ;
      for ( Int_t i = 0; i < NZDRIFT ; i++ )
	{
	  Zdrift = ZDriftArray[i] ;
	  for ( Int_t j = 0; j < NYARRAY ; j++ )
	    {
	      sum = 0.0 ;
	      y = YArray[j] ;
	      for ( Int_t k = 1 ; k < TERMS ; k++ )
		{
		  sum += ( C[k] / StarMagE ) * ( 1. - TMath::Exp(-1*k*PI*Zdrift/BOX) )
		    * TMath::Sin(k*PI*(y-GAPRADIUS)/BOX) ;
		}
	      SumArray[i][j] = sum ;
	    }
	}
      Flag = 1 ;
    }
  
  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;               // Phi ranges from pi to -pi
  phi0   =  ( (Int_t)((TMath::Abs(phi)+PI/12.)/(PI/6.) + 6.0 ) - 6.0 ) * PI/6. ;
  if ( phi < 0 ) phi0 *= -1. ;
  y      =  r * TMath::Cos( phi0 - phi ) ;
  Zdrift =  TPC_Z0 - TMath::Abs(x[2]) ;

  ORDER = 2 ;                                       // Quadratic Interpolation of the table
  Search ( NZDRIFT, ZDriftArray,  Zdrift, ilow ) ;
  Search ( NYARRAY, YArray, y, jlow ) ;

  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( ilow + ORDER  >=    NZDRIFT - 1 ) ilow =   NZDRIFT - 1 - ORDER ;
  if ( jlow + ORDER  >=    NYARRAY - 1 ) jlow =   NYARRAY - 1 - ORDER ;

  for ( Int_t i = ilow ; i < ilow + ORDER + 1 ; i++ )
    {
      save_sum[i-ilow]   = Interpolate( &YArray[jlow], &SumArray[i][jlow], ORDER, y )   ;
    }

  sum  = Interpolate( &ZDriftArray[ilow], save_sum, ORDER, Zdrift )   ; 

  if ( r > 0.0 )
    {
      phi =  phi - ( Const_1*(-1*sum)*TMath::Cos(phi0-phi) + Const_0*sum*TMath::Sin(phi0-phi) ) / r ;      
      r   =  r   - ( Const_0*sum*TMath::Cos(phi0-phi) - Const_1*(-1*sum)*TMath::Sin(phi0-phi) ) ;  
    }                                               // Subtract to Undo the distortions

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;
  
}


//________________________________________


void StMagUtilities::UndoTwistDistortion( const Float_t x[3], Float_t Xprime[3] )

{

  Double_t        Zdrift ;
  Int_t           sign ;

  // Work in TPC coordinates but note that XTWIST and YTWIST reported in Magnet coord system 
  // so negate them (below)  
  
  if ( x[2] >= 0.0 ) sign =  1 ;                       // (TPC West)
  else               sign = -1 ;                       // (TPC East)  

  Zdrift = sign * ( TPC_Z0 - TMath::Abs(x[2]) ) ;
  Xprime[0] = x[0] - ( -1* Const_1 * -1*YTWIST/1000 + Const_2 * -1*XTWIST/1000 ) * Zdrift ;
  Xprime[1] = x[1] - (     Const_1 * -1*XTWIST/1000 + Const_2 * -1*YTWIST/1000 ) * Zdrift ;
  Xprime[2] = x[2] ;                                   // Subtract to undo the distortion 

}


//________________________________________


void StMagUtilities::UndoClockDistortion( const Float_t x[3], Float_t Xprime[3] )

{

  Double_t r, phi ;

  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;

  if ( x[2] < 0 )  phi += EASTCLOCKERROR/1000. ;    // Phi rotation error in milli-radians
  if ( x[2] >= 0 ) phi += WESTCLOCKERROR/1000. ;    // Phi rotation error in milli-radians

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;

}

//________________________________________

 
void StMagUtilities::UndoMembraneDistortion( const Float_t x[3], Float_t Xprime[3] )

{

  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  z      =  x[2] ;
  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM
 
  InterpolateEdistortion( r, phi, z, Er_integral, Ephi_integral ) ;

  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      phi =  phi - ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;

}

void StMagUtilities::UndoEndcapDistortion( const Float_t x[3], Float_t Xprime[3] )

{

  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  z      =  x[2] ;
  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM


  InterpolateEEdistortion( r, phi, z, Er_integral, Ephi_integral ) ;

  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      phi =  phi - ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;

}
  
//________________________________________


void StMagUtilities::ReadField( )

{

  FILE    *magfile, *efile, *eefile, *b3Dfile ;
  TString comment, filename, filename3D ;
  TString MapLocation ;
  TString BaseLocation = "$STAR/StarDb/StMagF/" ;     // Base Directory for Maps

  if ( gMap == kMapped )                    // Mapped field values
    {
      if ( TMath::Abs(gFactor) > 0.8 )      // Scale from full field data 
	{
	  if ( gFactor > 0 )
	    {
	      filename   = "bfield_full_positive_2D.dat" ;
	      filename3D = "bfield_full_positive_3D.dat" ;
	      comment    = "Measured Full Field" ;
	      gRescale   = 1 ;                // Normal field 
	    }
	  else
	    {
	      filename   = "bfield_full_negative_2D.dat" ;
	      filename3D = "bfield_full_negative_3D.dat" ;
	      comment    = "Measured Full Field Reversed" ;
	      gRescale   = -1 ;               // Reversed field
	    }
	}
      else                                  // Scale from half field data             
	{
	  filename   = "bfield_half_positive_2D.dat" ;
	  filename3D = "bfield_half_positive_3D.dat" ;
          comment    = "Measured Half Field" ;
	  gRescale   = 2 ;                    // Adjust scale factor to use half field data
	}
    }
  else if ( gMap == kConstant )             // Constant field values
    {
      filename = "const_full_positive_2D.dat" ;
      comment  = "Constant Full Field" ;
      gRescale = 1 ;                        // Normal field
    }
  else
    {
      fprintf(stderr,"No map available - you must choose a mapped field or a constant field\n");
      exit(1) ;
    }
      
  printf("Reading Magnetic Field:  %s,  Scale factor = %f \n",comment.Data(),gFactor);
  printf("Filename is %s, Adjusted Scale factor = %f \n",filename.Data(),gFactor*gRescale);
  printf("Version: 3D Mag Field Distortions + Twist + PadRow13 + Clock + Membrane\n" ) ;
  MapLocation = BaseLocation + filename ;
  gSystem->ExpandPathName(MapLocation) ;
  magfile = fopen(MapLocation.Data(),"r") ;

  if (magfile) 

    {
      Char_t cname[128] ;
      fgets  ( cname, sizeof(cname) , magfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , magfile ) ;
      fgets  ( cname, sizeof(cname) , magfile ) ;
      fgets  ( cname, sizeof(cname) , magfile ) ;
      fgets  ( cname, sizeof(cname) , magfile ) ;

      for ( Int_t j=0 ; j < nZ ; j++ ) 
	{
	  for ( Int_t k=0 ; k < nR ; k++ )
	    {
	      fgets  ( cname, sizeof(cname) , magfile ) ; 
	      sscanf ( cname, " %f %f %f %f ", &Radius[k], &ZList[j], &Br[j][k], &Bz[j][k] ) ;  
	    }
	}
    }

  else 

    { 
      fprintf(stderr,"File %s not found !\n",MapLocation.Data());
      exit(1);
    }

  fclose(magfile) ;
      
  MapLocation = BaseLocation + filename3D ;
  gSystem->ExpandPathName(MapLocation) ;
  b3Dfile = fopen(MapLocation.Data(),"r") ;
  printf("Reading 3D Magnetic Field file: %s \n",filename3D.Data());

  if (b3Dfile) 

    {
      Char_t cname[128] ;
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      
      for ( Int_t i=0 ; i < nPhi ; i++ ) 
	{
	  for ( Int_t j=0 ; j < nZ ; j++ ) 
	    {
	      for ( Int_t k=0 ; k < nR ; k++ )
		{
		  fgets  ( cname, sizeof(cname) , b3Dfile ) ; 
		  sscanf ( cname, " %f %f %f %f %f %f ",
			   &R3D[k], &Z3D[j], &Phi3D[i], &Br3D[i][j][k], &Bz3D[i][j][k], &Bphi3D[i][j][k] ) ;
		  Phi3D[i] *= TMath::Pi() / 180. ;   // Convert to Radians  phi = 0 to 2*Pi
		}
	    }
	}
    }

  else if ( gMap == kConstant )             // Constant field values

    {
      for ( Int_t i=0 ; i < nPhi ; i++ ) 
	{
	  for ( Int_t j=0 ; j < nZ ; j++ ) 
	    {
	      for ( Int_t k=0 ; k < nR ; k++ )
		{
		  Br3D[i][j][k] = Br[j][k] ;
		  Bz3D[i][j][k] = Bz[j][k] ;
		  Bphi3D[i][j][k] = 0 ;
		}
	    }
	}
    }

  else

    { 
      fprintf(stderr,"File %s not found !\n",MapLocation.Data());
      exit(1);
    }

  fclose(b3Dfile) ;

  filename = "membrane_efield.dat" ;
  MapLocation = BaseLocation + filename ;
  gSystem->ExpandPathName(MapLocation) ;
  efile = fopen(MapLocation.Data(),"r") ;
  printf("Reading CM Electric Field Distortion File: %s \n",filename.Data());

  if (efile) 
    {

      Char_t cname[128] ;
      fgets  ( cname, sizeof(cname) , efile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , efile ) ;
      fgets  ( cname, sizeof(cname) , efile ) ;
      fgets  ( cname, sizeof(cname) , efile ) ;
      fgets  ( cname, sizeof(cname) , efile ) ;
      fgets  ( cname, sizeof(cname) , efile ) ;
      
      for ( Int_t i=0 ; i < neZ ; i++ ) 
	{
	  for ( Int_t j=0 ; j < nePhi ; j++ )
	    {
	      for ( Int_t k=0 ; k < neR ; k++ )
		{
		  if ( j == nePhi-1 )
		    {
		      ePhiList[j] = 6.2832 ;    // Repeat phi = 0 column in phi == 2 PI column
		      Er[i][j][k] = Er[i][0][k] ;
		      Ephi[i][j][k] = Ephi[i][0][k] ;
		    }
		  else
		    {
		      fgets  ( cname, sizeof(cname) , efile ) ; 
		      sscanf ( cname, " %f %f %f %f %f", &eRadius[k], &ePhiList[j], 
			       &eZList[i], &Er[i][j][k], &Ephi[i][j][k] ) ;  
		      //ePhiList[j] *= TMath::Pi() / 180. ;  // Assume table uses  phi = 0 to 2*Pi
		    }
		}
	    }
	}
    }      

  else 
    { 
      fprintf(stderr,"File %s not found !\n",MapLocation.Data());
      exit(1);
    }

  fclose(efile) ;

  filename = "endcap_efield.dat" ;
  MapLocation = BaseLocation + filename ;
  gSystem->ExpandPathName(MapLocation) ;
  eefile = fopen(MapLocation.Data(),"r") ;
  printf("Reading Endcap Electric Field Distortion File: %s \n",filename.Data());

  if (eefile) 
    {

      Char_t cname[128] ;
      fgets  ( cname, sizeof(cname) , eefile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , eefile ) ;
      fgets  ( cname, sizeof(cname) , eefile ) ;
      fgets  ( cname, sizeof(cname) , eefile ) ;
      fgets  ( cname, sizeof(cname) , eefile ) ;
      fgets  ( cname, sizeof(cname) , eefile ) ;
      
      for ( Int_t i=0 ; i < neZ ; i++ ) 
	{
	  for ( Int_t j=0 ; j < nePhi ; j++ )
	    {
	      for ( Int_t k=0 ; k < neR ; k++ )
		{
		  if ( j == nePhi-1 )
		    {
		      ePhiList[j] = 6.2832 ;    // Repeat phi = 0 column in phi == 2 PI column
		      EEr[i][j][k] = EEr[i][0][k] ;
		      EEphi[i][j][k] = EEphi[i][0][k] ;
		    }
		  else
		    {
		      fgets  ( cname, sizeof(cname) , eefile ) ; 
		      sscanf ( cname, " %f %f %f %f %f", &eRadius[k], &ePhiList[j], 
			       &eZList[i], &EEr[i][j][k], &EEphi[i][j][k] ) ;  
		      //eePhiList[j] *= TMath::Pi() / 180. ;  // Assume table uses  phi = 0 to 2*Pi
		    }
		}
	    }
	}
    }      

  else 
    { 
      fprintf(stderr,"File %s not found !\n",MapLocation.Data());
      exit(1);
    }

  fclose(eefile) ;

  return ;

}


//________________________________________


void StMagUtilities::InterpolateBfield( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value )

{

  Float_t fscale ;

  fscale = 0.001*gFactor*gRescale ;               // Scale STAR maps to work in kGauss, cm

  const   Int_t ORDER = 1  ;                      // Linear interpolation = 1, Quadratic = 2        
  static  Int_t jlow, klow ;                            
  Float_t save_Br[ORDER+1] ;
  Float_t save_Bz[ORDER+1] ;

  Search ( nZ, ZList,  z, jlow ) ;
  Search ( nR, Radius, r, klow ) ;
  if ( jlow < 0 ) jlow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( klow < 0 ) klow = 0 ;
  if ( jlow + ORDER  >=    nZ - 1 ) jlow =   nZ - 1 - ORDER ;
  if ( klow + ORDER  >=    nR - 1 ) klow =   nR - 1 - ORDER ;

  for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
    {
      save_Br[j-jlow]   = Interpolate( &Radius[klow], &Br[j][klow], ORDER, r )   ;
      save_Bz[j-jlow]   = Interpolate( &Radius[klow], &Bz[j][klow], ORDER, r )   ;
    }
  Br_value  = fscale * Interpolate( &ZList[jlow], save_Br, ORDER, z )   ; 
  Bz_value  = fscale * Interpolate( &ZList[jlow], save_Bz, ORDER, z )   ; 

}


void StMagUtilities::Interpolate3Dfield( const Float_t r, const Float_t z, const Float_t phi, 
			 Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value )
{

  Float_t fscale ;

  fscale = 0.001*gFactor*gRescale ;               // Scale STAR maps to work in kGauss, cm

  const   Int_t ORDER = 1 ;                      // Linear interpolation = 1, Quadratic = 2   
  static  Int_t ilow, jlow, klow ;
  Float_t save_Br[ORDER+1],   saved_Br[ORDER+1] ;
  Float_t save_Bz[ORDER+1],   saved_Bz[ORDER+1] ;
  Float_t save_Bphi[ORDER+1], saved_Bphi[ORDER+1] ;

  Search( nPhi, Phi3D, phi, ilow ) ;
  Search( nZ,   Z3D,   z,   jlow ) ;
  Search( nR,   R3D,   r,   klow ) ;
  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( klow < 0 ) klow = 0 ;

  if ( ilow + ORDER  >=  nPhi - 1 ) ilow = nPhi - 1 - ORDER ;
  if ( jlow + ORDER  >=    nZ - 1 ) jlow =   nZ - 1 - ORDER ;
  if ( klow + ORDER  >=    nR - 1 ) klow =   nR - 1 - ORDER ;

  for ( Int_t i = ilow ; i < ilow + ORDER + 1 ; i++ )
    {
      for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
	{
	  save_Br[j-jlow]   = Interpolate( &R3D[klow], &Br3D[i][j][klow], ORDER, r )   ;
	  save_Bz[j-jlow]   = Interpolate( &R3D[klow], &Bz3D[i][j][klow], ORDER, r )   ;
	  save_Bphi[j-jlow] = Interpolate( &R3D[klow], &Bphi3D[i][j][klow], ORDER, r ) ; 
	}
      saved_Br[i-ilow]   = Interpolate( &Z3D[jlow], save_Br, ORDER, z )   ; 
      saved_Bz[i-ilow]   = Interpolate( &Z3D[jlow], save_Bz, ORDER, z )   ; 
      saved_Bphi[i-ilow] = Interpolate( &Z3D[jlow], save_Bphi, ORDER, z ) ; 
    }
  Br_value   = fscale * Interpolate( &Phi3D[ilow], saved_Br, ORDER, phi )   ;
  Bz_value   = fscale * Interpolate( &Phi3D[ilow], saved_Bz, ORDER, phi )   ;
  Bphi_value = fscale * Interpolate( &Phi3D[ilow], saved_Bphi, ORDER, phi ) ; 

  return ;
 
}

//________________________________________


void StMagUtilities::InterpolateEdistortion( const Float_t r, const Float_t phi, const Float_t z, 
					     Float_t &Er_integral, Float_t &Ephi_integral )

{

  const   Int_t ORDER = 1 ;                      // Linear interpolation = 1, Quadratic = 2         
  static  Int_t ilow, jlow, klow ;
  Float_t save_Er[ORDER+1],   saved_Er[ORDER+1] ;
  Float_t save_Ephi[ORDER+1], saved_Ephi[ORDER+1] ;

  Search( neZ,   eZList,   z,   ilow   ) ;
  Search( nePhi, ePhiList, phi, jlow   ) ;
  Search( neR,   eRadius,  r,   klow   ) ;
  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( klow < 0 ) klow = 0 ;

  if ( ilow + ORDER  >=    neZ - 1 ) ilow =   neZ - 1 - ORDER ;
  if ( jlow + ORDER  >=  nePhi - 1 ) jlow = nePhi - 1 - ORDER ;
  if ( klow + ORDER  >=    neR - 1 ) klow =   neR - 1 - ORDER ;

  for ( Int_t i = ilow ; i < ilow + ORDER + 1 ; i++ )
    {
      for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
	{
	  save_Er[j-jlow]     = Interpolate( &eRadius[klow], &Er[i][j][klow], ORDER, r )   ;
	  save_Ephi[j-jlow]   = Interpolate( &eRadius[klow], &Ephi[i][j][klow], ORDER, r )   ;
	}
      saved_Er[i-ilow]     = Interpolate( &ePhiList[jlow], save_Er, ORDER, phi )   ; 
      saved_Ephi[i-ilow]   = Interpolate( &ePhiList[jlow], save_Ephi, ORDER, phi )   ; 
    }
  Er_integral     = Interpolate( &eZList[ilow], saved_Er, ORDER, z )   ;
  Ephi_integral   = Interpolate( &eZList[ilow], saved_Ephi, ORDER, z )   ;

  return ;
 
}


void StMagUtilities::InterpolateEEdistortion( const Float_t r, const Float_t phi, const Float_t z, 
					      Float_t &Er_integral, Float_t &Ephi_integral )

{

  const   Int_t ORDER = 1 ;                      // Linear interpolation = 1, Quadratic = 2       
  static  Int_t ilow, jlow, klow ;
  Float_t save_Er[ORDER+1],   saved_Er[ORDER+1] ;
  Float_t save_Ephi[ORDER+1], saved_Ephi[ORDER+1] ;

  Search( neZ,   eZList,   z,   ilow   ) ;
  Search( nePhi, ePhiList, phi, jlow   ) ;
  Search( neR,   eRadius,  r,   klow   ) ;
  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( klow < 0 ) klow = 0 ;

  if ( ilow + ORDER  >=    neZ - 1 ) ilow =   neZ - 1 - ORDER ;
  if ( jlow + ORDER  >=  nePhi - 1 ) jlow = nePhi - 1 - ORDER ;
  if ( klow + ORDER  >=    neR - 1 ) klow =   neR - 1 - ORDER ;

  for ( Int_t i = ilow ; i < ilow + ORDER + 1 ; i++ )
    {
      for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
	{
	  save_Er[j-jlow]     = Interpolate( &eRadius[klow], &EEr[i][j][klow], ORDER, r )   ;
	  save_Ephi[j-jlow]   = Interpolate( &eRadius[klow], &EEphi[i][j][klow], ORDER, r )   ;
	}
      saved_Er[i-ilow]     = Interpolate( &ePhiList[jlow], save_Er, ORDER, phi )   ; 
      saved_Ephi[i-ilow]   = Interpolate( &ePhiList[jlow], save_Ephi, ORDER, phi )   ; 
    }
  Er_integral     = Interpolate( &eZList[ilow], saved_Er, ORDER, z )   ;
  Ephi_integral   = Interpolate( &eZList[ilow], saved_Ephi, ORDER, z )   ;

  return ;
 
}


//________________________________________


Float_t StMagUtilities::Interpolate( const Float_t Xarray[], const Float_t Yarray[], 
				     const Int_t ORDER, const Float_t x )

{

  Float_t y ;


  if ( ORDER == 2 )                // Quadratic Interpolation = 2 

    {
      y  = (x-Xarray[1]) * (x-Xarray[2]) * Yarray[0] / ( (Xarray[0]-Xarray[1]) * (Xarray[0]-Xarray[2]) ) ; 
      y += (x-Xarray[2]) * (x-Xarray[0]) * Yarray[1] / ( (Xarray[1]-Xarray[2]) * (Xarray[1]-Xarray[0]) ) ; 
      y += (x-Xarray[0]) * (x-Xarray[1]) * Yarray[2] / ( (Xarray[2]-Xarray[0]) * (Xarray[2]-Xarray[1]) ) ; 
      
    }

  else                             // Linear Interpolation = 1

    {
      y  = Yarray[0] + ( Yarray[1]-Yarray[0] ) * ( x-Xarray[0] ) / ( Xarray[1] - Xarray[0] ) ;
    }

  return (y) ;

}


//________________________________________


void StMagUtilities::Search( Int_t N, Float_t Xarray[], Float_t x, Int_t &low )

{

  // Search an ordered table by starting at the most recently used point

  Long_t middle, high ;
  Int_t  ascend = 0, increment = 1 ;

  if ( Xarray[N-1] >= Xarray[0] ) ascend = 1 ;  // Ascending ordered table if true
  
  if ( low < 0 || low > N-1 ) { low = -1 ; high = N ; }

  else                                            // Ordered Search phase
    {
      if ( (Int_t)( x >= Xarray[low] ) == ascend ) 
	{
	  if ( low == N-1 ) return ;          
	  high = low + 1 ;
	  while ( (Int_t)( x >= Xarray[high] ) == ascend )  
	    {
	      low = high ;
	      increment *= 2 ;
	      high = low + increment ;
	      if ( high > N-1 )  {  high = N ; break ;  }
	    }
	}
      else
	{
	  if ( low == 0 )  {  low = -1 ;  return ;  }
	  high = low - 1 ;
	  while ( (Int_t)( x < Xarray[low] ) == ascend )
	    {
	      high = low ;
	      increment *= 2 ;
	      if ( increment >= high )  {  low = -1 ;  break ;  }
	      else  low = high - increment ;
	    }
	}
    }

  while ( (high-low) != 1 )                      // Binary Search Phase
    {
      middle = ( high + low ) / 2 ;
      if ( (Int_t)( x >= Xarray[middle] ) == ascend )
	low = middle ;
      else
	high = middle ;
    }

  if ( x == Xarray[N-1] ) low = N-2 ;
  if ( x == Xarray[0]   ) low = 0 ;

  return ;
       
}
















