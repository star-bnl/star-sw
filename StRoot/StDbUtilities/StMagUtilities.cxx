/***********************************************************************
 *
 * $Id: StMagUtilities.cxx,v 1.5 2001/04/17 16:22:29 lasiuk Exp $
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
 * Revision 1.5  2001/04/17 16:22:29  lasiuk
 * exb transport code for drifting electrons.
 * The clock rotation and rotation of the TPC into
 * the global coordinate (magenet) system are takne
 * into account.  Transport alters x,y,z position of
 * space point.
 *
 * Revision 1.4  2001/02/08 22:26:09  jhthomas
 * Added corrections for CM electrostatic distortions
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
// Pull parameters out of DB rather than from #define.  kChain should use DB.  Also need Standalone.
// Note that phi less than 0 or greater 2 pi will be extrapolated - fix this by adding
// rows to the phi table in the reading routine for CM correction.  Increase phi table size to 14.
// Add table for row13 solutions (only need y by z), do once;  also table for UndoDistortion.
// Scale membrane correction to Efield value by allowing Omega Tau to change with Efield and DriftV
// Include Hui's Z offsets for the inner and outer sectors as part of DB - a value already exists
// Add a routine to distort the track if we are given a Geant Vector full of points == a track
// Move Read routine to a separate class so it can be called "new".  How to share data?
// Add simulated B field map in the regions where the field is not mapped.
//

#include "StMagUtilities.h"
#include "StGlobals.hh"
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#define   gufld gufld_
extern   "C" { void gufld(Float_t *, Float_t *) ; }

#define  nZ               57            // Standard STAR Map grid # of Z points in table
#define  nR               28            // Number of R points in table
#define  nPhi             12            // Number of Phi points in table
#define  neZ              69            // Standard STAR Map grid # of Z points in table
#define  neR              33            // Number of R points in table
#define  nePhi            12            // Number of Phi points in table
#define  StarDriftV     5.45            // STAR Drift Velocity (cm/microSec)
#define  StarMagE      148.0            // STAR Electric Field (V/cm) Magnitude
#define  TPC_Z0        208.7            // Z location of STAR TPC Gating Grid
#define  GAP13_14      1.595            // Width of the gap between the grids at row 13 and row 14 (cm)
#define  GAPRADIUS     121.8            // Radius of gap between rows 13 & 14 at phi = zero degrees (cm)
#define  GG           -127.5            // Gating Grid voltage (volts)
#define  XTWIST       -0.071            // X Displacement of West end of TPC (cm) wrt TPC_Z0
#define  YTWIST        0.019            // Y Displacement of West end of TPC (cm) wrt TPC_Z0
#define  EASTCLOCKERROR  0.0            // Phi rotation of East end of TPC in milli-radians
#define  WESTCLOCKERROR -0.43           // Phi rotation of West end of TPC in milli-radians

static EBField  gMap  =  kUndefined ;   // Global flag to indicate static arrays are full
static Float_t  gFactor  = 1.0 ;        // Multiplicative factor (allows scaling and sign reversal)
static Float_t  gRescale = 1.0 ;        // Multiplicative factor (allows re-scaling wrt which map was read)
static Float_t  Bz[nZ][nR], Br[nZ][nR] ;         
static Float_t  Radius[nR], ZList[nZ]  ;         
static Float_t  Ephi[neZ][nePhi][neR], Er[neZ][nePhi][neR] ;
static Float_t  eRadius[neR], ePhiList[nePhi], eZList[neZ]  ;         


//________________________________________

ClassImp(StMagUtilities)

StMagUtilities::StMagUtilities( )

{                                           // StMagUtilities constructor
    printf("Default c'tor --- no arguments\n");

  float b[3], x[3] = { 0, 0, 0 } ;
  
  if ( gMap == kUndefined ) 
    {
      gufld(x,b) ;                          // Read crude values from Chain to get scale
      gFactor = b[2] / 4.980 ;              // Select factor based on Chain values (kGauss) 

      mBNominal = StThreeVectorD(b[0]*kilogauss, b[1]*kilogauss, b[2]*kilogauss);
      
      gMap = kMapped ;                      // Do once & Select the B field map (mapped field or constant)
      ReadField() ;                         // Read the Magnetic and Electric Field Data Files
    }
    this->init();

}

//________________________________________

StMagUtilities::StMagUtilities( const EBField map = kMapped, const Float_t factor = 1.0 )

{                                           // StMagUtilities constructor
    printf("c'tor --- all default arguments\n");
    this->init();

  if ( gMap == kUndefined ) 
    {
      gFactor = factor ;
      gMap = map ;                          // Do once & select the requested map (mapped or constant)
      ReadField() ;                         // Read the Magnetic and Electric Field Data Files
    }

  if ( gMap != map || factor != gFactor ) 
    {
      cout << "StMagUtilities Warning: The Maps have already been read and scaled." << endl
	   << "                        Try instantiating with StMagUtilities( <empty> ) " << endl ;
    }
}

//________________________________________

void StMagUtilities::init()
{

    mVdrift = StarDriftV*centimeter/microsecond;
    mEField = StThreeVectorD(0.,0.,-1.*StarMagE*(volt/centimeter));
    
    mMobility = (mVdrift/abs(mEField));
    mOmegaTau = mMobility*.25*tesla;

    mFrischGrid = TPC_Z0*centimeter;

    mTpcCenter = StThreeVectorD(-2.76*millimeter, -.82*millimeter,-1.92*millimeter);

    PR(mVdrift/(centimeter/microsecond));

    PR(mMobility/(meter*meter/(volt*second)));
    PR(mOmegaTau);

    PR(mEField/(volt/centimeter));    
    PR(mBNominal/tesla);

    PR(mFrischGrid/centimeter);
    PR(mTpcCenter/millimeter);

    mWestClockRotation = WESTCLOCKERROR*milliradian;

    mThetaX = -.156*milliradian;
    mThetaY = -.381*milliradian;
    mThetaZ = -.040*milliradian;

    mCx = cos(mThetaX);
    mSx = sin(mThetaX);
    mCy = cos(mThetaY);
    mSy = sin(mThetaY);
    mCz = cos(mThetaZ);
    mSz = sin(mThetaZ);

    PR(mWestClockRotation/milliradian);
    PR(mThetaX/milliradian);
    PR(mThetaY/milliradian);
    PR(mThetaZ/milliradian);

    PR(mDriftField/(volt/centimeter));    
    PR(mEField/(volt/centimeter));    
    mDriftField = this->rotateToMagnet(mEField);
    PR(mDriftField/(volt/centimeter));


    
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

//________________________________________
//
// Main Entry Point for requests for B field in Radial coordinates
//
//________________________________________

void StMagUtilities::BrBzField( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value )

{
  InterpolateBfield( r, z, Br_value, Bz_value ) ;

}

//________________________________________
//
// Main Entry Point for requests to Undo the E and B field distortions
//
//________________________________________

void StMagUtilities::UndoDistortion( const Float_t x[3], Float_t Xprime[3] )

{
    //printf("StMagUtilities::UndoDistortion()\n");

    Float_t Xprime1[3], Xprime2[3] ;

    StThreeVectorD ip(x[0],x[1],x[2]);

//       PR(ip);
    this->calculateDriftTime(ip);
    
    StThreeVectorD rot = this->clockRotation(ip);

    //
    // account for which side of chamber here
    //
    if(ip.z() > 0) {
	rot.setZ(mFrischGrid);
    }
    else {
	rot.setZ(-1.*mFrischGrid);
    }
//     PR(rot);

    // magnet point
    StThreeVectorD  mp = rotateToMagnet(rot);
//     PR(mp);
    
    StThreeVectorD pt = mp + mTpcCenter;
//     PR(pt);
    StThreeVectorD transPt = this->transport(pt);
//     PR(transPt);

//      transPt -= mTpcCenter;
//      StThreeVectorD  newPt = rotateToTpc(transPt);
     StThreeVectorD newPt(transPt);
//       PR(newPt);

    
    Xprime[0] = newPt.x();
    Xprime[1] = newPt.y();
    Xprime[2] = newPt.z();
    

    
//     UndoBDistortion        ( x, Xprime1 ) ;
//    UndoPad13Distortion    ( Xprime1, Xprime2 ) ;
//    UndoTwistDistortion    ( Xprime2, Xprime1 ) ;
//    UndoClockDistortion    ( Xprime1, Xprime2 ) ;
//    UndoMembraneDistortion ( Xprime2, Xprime ) ;
  
}

//________________________________________
//
// Main Entry Point for requests to Do the E and B field distortions (for simulations)
//
//________________________________________

void StMagUtilities::DoDistortion( const Float_t x[3], Float_t Xprime[3] )

{
    printf("StMagUtilities::DoDistortion()\n");

  UndoDistortion ( x, Xprime ) ;

  Xprime[0] = 2*x[0] - Xprime[0] ;
  Xprime[1] = 2*x[1] - Xprime[1] ;
  Xprime[2] = 2*x[2] - Xprime[2] ;

}

//________________________________________


void StMagUtilities::UndoBDistortion( const Float_t x[3], Float_t Xprime[3] )

{
    printf("StMagUtilities::UndoBDistortion()\n");

  double   Const_1, Const_2 ;
  double   OmegaTau ;                                  // OmegaTau carries the sign opposite of B
  Float_t  B[3], ah ;                                  // ah carries the sign opposite of E (for forward integration)
  Int_t    sign, index = 1 , NSTEPS ;              
  
  if ( x[2] >= 0.0 ) sign =  1 ;                       // (TPC West)
  else               sign = -1 ;                       // (TPC East)  

  Xprime[0]  =  x[0] ;                                 // Integrate backwards from TPC plane to 
  Xprime[1]  =  x[1] ;                                 // the point the electron cluster was born. 
  Xprime[2]  =  sign * TPC_Z0 ;                        // Prepare for different readout planes

  BField( Xprime, B ) ;                                // Work in kGauss, cm and assume Bz dominates

  // Theoretically, OmegaTau is defined as shown here.  Instead, we will use scaled values from Aleph
  // OmegaTau   =  -10. * B[2] * StarDriftV / StarMagE ;  // cm/microsec, Volts/cm

  OmegaTau   =  -0.4 * B[2] ;                          // B in kGauss, note that the sign of B is important here and below
  Const_1    =  OmegaTau / ( 1. + pow( OmegaTau, 2 ) ) ;
  Const_2    =  pow( OmegaTau, 2 ) / ( 1. + pow( OmegaTau, 2 ) ) ;

  for ( NSTEPS = 5 ; NSTEPS < 1000 ; NSTEPS += 2 )     // Choose ah to be about 1 cm, NSTEPS must be odd
    {
      ah = ( x[2] - sign * TPC_Z0 ) / ( NSTEPS - 1 ) ; // Going Backwards! See note above.
      if ( TMath::Abs(ah) < 1.0 ) break ;
    }

  for ( Int_t i = 1; i <= NSTEPS; ++i )                // Simpson's Integration Loop
    {
      if ( i == NSTEPS ) index = 1 ;
      Xprime[2] +=  index*(ah/3) ;
      BField( Xprime, B ) ;                            // Work in kGauss, cm
      Xprime[0] +=  index*(ah/3)*( Const_2*B[0] - Const_1*B[1] ) / B[2] ;
      Xprime[1] +=  index*(ah/3)*( Const_2*B[1] + Const_1*B[0] ) / B[2] ;
      if ( index != 4 ) index = 4; else index = 2 ;
    }    

}


//________________________________________


void StMagUtilities::UndoPad13Distortion( const Float_t x[3], Float_t Xprime[3] )

{
    printf("StMagUtilities::UndoPad13Distortion()\n");

  const Int_t   TERMS    = 400 ;                 // Number of terms in the sum
  const Float_t SCALE    = 0.192 ;               // Set the scale for the correction
  const Float_t BOX      = 200.0 - GAPRADIUS ;   // Size of the box in which to work
  const Float_t PI       = TMath::Pi() ;

  static Float_t  C[TERMS] ;                     // Coefficients for series
  static Int_t    Flag = 0 ;                     // Calculate only once
  static Float_t  Const_3, Const_4, OmegaTau ;
  Float_t  r, Zdrift, Br_value, Bz_value ;
  Double_t phi, phi0, y, sum = 0.0 ;

  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;            // Phi ranges from pi to -pi
  phi0   =  ( (Int_t)((TMath::Abs(phi)+PI/12.)/(PI/6.) + 6.0 ) - 6.0 ) * PI/6. ;
  if ( phi < 0 ) phi0 *= -1. ;
  y      =  r * TMath::Cos( phi0 - phi ) ;
  Zdrift =  TPC_Z0 - TMath::Abs(x[2]) ;

  if ( Flag == 0 ) 
    {
      C[0] = GAP13_14 * GG * SCALE / ( 2 * BOX ) ;   
      for ( Int_t i = 1 ; i < TERMS ; i++ )
	  C[i] = 2 * GG * SCALE * TMath::Sin( GAP13_14*i*PI/( 2*BOX ) ) / ( i * PI ) ;
      BrBzField( 100.0, 1.0, Br_value, Bz_value ) ; // Work in kGauss, cm and assume Bz dominates
      OmegaTau   =  -0.4 * Bz_value ;         // B in kGauss, note that the sign of B is important
      Const_3    =  OmegaTau / ( 1. + pow( OmegaTau, 2 ) ) ;
      Const_4    =  1.0 / ( 1. + pow( OmegaTau, 2 ) ) ;    
      Flag = 1 ;
    }
  
  for ( Int_t k = 1 ; k < TERMS ; k++ )
    {
      sum += ( C[k] / StarMagE ) * ( 1. - TMath::Exp(-1*k*PI*Zdrift/BOX) )
	* TMath::Sin(k*PI*(y-GAPRADIUS)/BOX) ;
    }
  
  // Subtract to Undo the distortions
  if ( r > 0.0 )
    {
      phi =  phi - ( Const_3*(-1*sum)*TMath::Cos(phi0-phi) + Const_4*sum*TMath::Sin(phi0-phi) ) / r ;      
      r   =  r   - ( Const_4*sum*TMath::Cos(phi0-phi) - Const_3*(-1*sum)*TMath::Sin(phi0-phi) ) ;  
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;
  
}


//________________________________________


void StMagUtilities::UndoTwistDistortion( const Float_t x[3], Float_t Xprime[3] )

{
    printf("StMagUtilities::UndoTwistDistortion()\n");

  static Int_t    Flag = 0 ;                  // Do once
  static Float_t  Const_3, Const_4, OmegaTau ;
  Float_t         Zdrift, B[3] ;
  Int_t           sign ;

  if ( Flag == 0 ) 
    {
      BField( x, B ) ;                        // Work in kGauss, cm and assume Bz dominates
      OmegaTau   =  -0.4 * B[2] ;             // B in kGauss, note that the sign of B is important here
      Const_3    =  OmegaTau / ( 1. + pow( OmegaTau, 2 ) ) ;
      Const_4    =  1.0 / ( 1. + pow( OmegaTau, 2 ) ) ;    
      Flag = 1 ;
    }
  
  if ( x[2] >= 0.0 ) sign =  1 ;                       // (TPC West)
  else               sign = -1 ;                       // (TPC East)  

  Zdrift = sign * ( TPC_Z0 - TMath::Abs(x[2]) ) ;
  Xprime[0] = x[0] + ( -1* Const_3 * YTWIST/TPC_Z0 + Const_4 * XTWIST/TPC_Z0 ) * Zdrift ;
  Xprime[1] = x[1] + (     Const_3 * XTWIST/TPC_Z0 + Const_4 * YTWIST/TPC_Z0 ) * Zdrift ;
  Xprime[2] = x[2] ;                          // Add (above) to undo the distortion 

}


//________________________________________


void StMagUtilities::UndoClockDistortion( const Float_t x[3], Float_t Xprime[3] )

{
    printf("StMagUtilities::UndoClockDistortion()\n");

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
    printf("StMagUtilities::UndoMembraneDistortion()\n");

  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;
  double   Const_3, Const_4 ;
  double   OmegaTau ;                                  // OmegaTau carries the sign opposite of B
  Float_t  B[3] ;   

  BField( x, B ) ;                                     // Work in kGauss, cm and assume Bz dominates

  // Theoretically, OmegaTau is defined as shown here.  Instead, we will use scaled values from Aleph
  // OmegaTau   =  -10. * B[2] * StarDriftV / StarMagE ;  // cm/microsec, Volts/cm

  OmegaTau   =  -0.4 * B[2] ;                          // B in kGauss, note that the sign of B is important
  Const_3    =  OmegaTau / ( 1. + pow( OmegaTau, 2 ) ) ;
  Const_4    =  1.0 / ( 1. + pow( OmegaTau, 2 ) ) ;    

  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;
  z      =  x[2] ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Electric field table uses phi from 0 to 2*Pi

  InterpolateEdistortion( r, phi, z, Er_integral, Ephi_integral ) ;

  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      phi =  phi - ( Const_4*Ephi_integral - Const_3*Er_integral ) / r ;      
      r   =  r   - ( Const_4*Er_integral + Const_3*Ephi_integral ) ;  
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;

}
  
//________________________________________


void StMagUtilities::ReadField( )

{
    printf("StMagUtilities::ReadField()\n");

  FILE    *magfile, *efile ;
  TString comment, filename ;
  TString MapLocation ;
  TString BaseLocation = "$STAR/StarDb/StMagF/" ;     // Base Directory for Maps

  if ( gMap == kMapped )                    // Mapped field values
    {
      if ( TMath::Abs(gFactor) > 0.8 )      // Scale from full field data 
	{
	  if ( gFactor > 0 )
	    {
	      filename = "bfield_full_positive_2D.dat" ;
	      comment  = "Measured Full Field" ;
	      gRescale = 1 ;                // Normal field 
	    }
	  else
	    {
	      filename = "bfield_full_negative_2D.dat" ;
	      comment  = "Measured Full Field Reversed" ;
	      gRescale = -1 ;               // Reversed field
	    }
	}
      else                                  // Scale from half field data             
	{
	  filename = "bfield_half_positive_2D.dat" ;
          comment  = "Measured Half Field" ;
	  gRescale = 2 ;                    // Adjust scale factor to use half field data
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
  //printf("Version: Mag Field Distortions + PadRow13 + Twist + Clock + Membrane \n" ) ;
  printf("Version: Brian and Jon\n");
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
      
  filename = "boundary_13_efield.dat" ;
  MapLocation = filename ;
  MapLocation = BaseLocation + filename ;
  gSystem->ExpandPathName(MapLocation) ;
  efile = fopen(MapLocation.Data(),"r") ;
  printf("Reading Electric Field Distortion File: %s \n",filename.Data());

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
		  fgets  ( cname, sizeof(cname) , efile ) ; 
		  sscanf ( cname, " %f %f %f %f %f", &eRadius[k], &ePhiList[j], 
			   &eZList[i], &Er[i][j][k], &Ephi[i][j][k] ) ;  
		  //ePhiList[j] *= TMath::Pi() / 180. ;  // Assume table uses  phi = 0 to 2*Pi
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
  printf("READFIELD complete:\n");
}


//________________________________________


void StMagUtilities::InterpolateBfield( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value )

{

  Float_t fscale ;

  fscale = 0.001*gFactor*gRescale ;               // Scale STAR maps to work in kGauss, cm

  const   Int_t ORDER = 2  ;                      // Quadratic interpolation          
  Int_t   jlow, klow ;                            
  Float_t save_Br[ORDER+1] ;
  Float_t save_Bz[ORDER+1] ;

  jlow = Search( nZ,   ZList,   z   ) ;
  klow = Search( nR,   Radius,  r   ) ;
  if ( jlow < 0 ) jlow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( klow < 0 ) klow = 0 ;
  if ( jlow + ORDER  >=    nZ - 1 ) jlow =   nZ - 1 - ORDER ;
  if ( klow + ORDER  >=    nR - 1 ) klow =   nR - 1 - ORDER ;

  for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
    {
      save_Br[j-jlow]   = QuadInterp( &Radius[klow], &Br[j][klow], r )   ;
      save_Bz[j-jlow]   = QuadInterp( &Radius[klow], &Bz[j][klow], r )   ;
    }
  Br_value  = fscale * QuadInterp( &ZList[jlow], save_Br, z )   ; 
  Bz_value  = fscale * QuadInterp( &ZList[jlow], save_Bz, z )   ; 

}

//________________________________________


void StMagUtilities::InterpolateEdistortion( const Float_t r, const Float_t phi, const Float_t z, Float_t &Er_integral, Float_t &Ephi_integral )

{

  const   Int_t ORDER = 2 ;                      // Quadratic interpolation          
  Int_t   ilow, jlow, klow ;
  Float_t save_Er[ORDER+1],   saved_Er[ORDER+1] ;
  Float_t save_Ephi[ORDER+1], saved_Ephi[ORDER+1] ;

  ilow = Search( neZ,   eZList,   z   ) ;
  jlow = Search( nePhi, ePhiList, phi ) ;
  klow = Search( neR,   eRadius,  r   ) ;
  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( klow < 0 ) klow = 0 ;

  if ( ilow + ORDER  >=    nZ - 1 ) ilow =   nZ - 1 - ORDER ;
  if ( jlow + ORDER  >=  nPhi - 1 ) jlow = nPhi - 1 - ORDER ;
  if ( klow + ORDER  >=    nR - 1 ) klow =   nR - 1 - ORDER ;

  for ( Int_t i = ilow ; i < ilow + ORDER + 1 ; i++ )
    {
      for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
	{
	  save_Er[j-jlow]     = QuadInterp( &eRadius[klow], &Er[i][j][klow], r )   ;
	  save_Ephi[j-jlow]   = QuadInterp( &eRadius[klow], &Ephi[i][j][klow], r )   ;
	}
      saved_Er[i-ilow]     = QuadInterp( &ePhiList[jlow], save_Er, phi )   ; 
      saved_Ephi[i-ilow]   = QuadInterp( &ePhiList[jlow], save_Ephi, phi )   ; 
    }
  Er_integral     = QuadInterp( &eZList[ilow], saved_Er, z )   ;
  Ephi_integral   = QuadInterp( &eZList[ilow], saved_Ephi, z )   ;

  return ;
 
}


//________________________________________


Float_t StMagUtilities::QuadInterp( const Float_t Xarray[], const Float_t Yarray[], const Float_t x )

{
  Float_t y ;

  y  = (x-Xarray[1]) * (x-Xarray[2]) * Yarray[0] / ( (Xarray[0]-Xarray[1]) * (Xarray[0]-Xarray[2]) ) ; 
  y += (x-Xarray[2]) * (x-Xarray[0]) * Yarray[1] / ( (Xarray[1]-Xarray[2]) * (Xarray[1]-Xarray[0]) ) ; 
  y += (x-Xarray[0]) * (x-Xarray[1]) * Yarray[2] / ( (Xarray[2]-Xarray[0]) * (Xarray[2]-Xarray[1]) ) ; 

  return(y) ;

}

//________________________________________


Int_t StMagUtilities::Search( Int_t N, Float_t Xarray[], Float_t x )

{

  // Search an ordered table by starting at the most recently used point

  static Int_t low = 0 ;
  Long_t middle, high ;
  Int_t  ascend = 0, increment = 1 ;

  if ( Xarray[N-1] >= Xarray[0] ) ascend = 1 ;  // Ascending ordered table if true
  
  if ( low < 0 || low > N-1 ) { low = -1 ; high = N ; }

  else                                            // Ordered Search phase
    {
      if ( (Int_t)( x >= Xarray[low] ) == ascend ) 
	{
	  if ( low == N-1 ) return(low) ;          
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
	  if ( low == 0 )  {  low = -1 ;  return(low) ;  }
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

  return(low) ;
       
}

StThreeVectorD StMagUtilities::transport(StThreeVectorD& pos) {

    
//     printf("::transport\n");
//     PR(pos);
    double step = -6*centimeter;
    double h = step/mVdrift;
//     PR(h/microsecond);

    //StThreeVectorD p(pos);

    double time = mDriftTime;
//     PR(time/microsecond);

    while (time > 0.-h) {
	pos = rKIntegration(h,&time,pos);
// 	mPathIntegral.push_back(pos);
// 	PR(time/microsecond);
 	//PR(pos.z()/centimeter);
    }
//     cout << "Done time= " << (time/microsecond) << " us" << endl;

    h /=4.;

    StThreeVectorD rv;
    do {
	rv = pos;
	pos = rKIntegration(h,&time,pos);
    } while (time > 0.);

    double hh = h - time;
    time -= h;

    rv = rKIntegration(hh,&time,rv);
//     mPathIntegral.push_back(rv);

//     PR(rv/centimeter);

    return rv;
}

StThreeVectorD StMagUtilities::calculateDriftVelocity(StThreeVectorD r) {

    //
    // sign of electron charge (drifting electrons)
    //
    double q = -1.;

    //
    // wt = uB
    //
    
    float bArray[3] = {0};
    float rArray[] = {r.x(),r.y(),r.z()};
    
    this->BField(rArray,bArray);
    StThreeVectorD B(bArray[0]*kilogauss,bArray[1]*kilogauss,bArray[2]*kilogauss);
    
//     PR(B/tesla);
    
    double omegaTau = mMobility*abs(B);
//     PR(omegaTau);
//     PR(mOmegaTau);

    //
    //         u |E|       ^           ^   ^              ^   ^  ^
    // vd = ------------ ( E  + (wt) ( E x B)  +  (wt)^2 (E . B) B )
    //       1 + (wt)^2
    //

    double constant = q*(mMobility*abs(mDriftField))/(1+sqr(omegaTau));
//     PR(constant/(centimeter/microsecond));

    //
    // Get the field values
    //
    StThreeVectorD e;
    StThreeVectorD b;

    if(r.z() > 0) {
	e = StThreeVectorD(mDriftField.unit());
    }
    else {
	e = StThreeVectorD(-1.*mDriftField.unit().x(),
			   -1.*mDriftField.unit().y(),
			   -1.*mDriftField.unit().z());
    }
    
    b = StThreeVectorD(B.unit());
    //    PR(e);
//     PR(b);
    
    StThreeVectorD
	vDrift(e.x() + omegaTau*(e.y()*b.z()-e.z()*b.y()) + sqr(omegaTau)*(e*b)*b.x(),
	       e.y() + omegaTau*(e.z()*b.x()-e.x()*b.z()) + sqr(omegaTau)*(e*b)*b.y(),
	       e.z() + omegaTau*(e.x()*b.y()-e.y()*b.x()) + sqr(omegaTau)*(e*b)*b.z());

    vDrift*=constant;
    //PR(vDrift/(centimeter/microsecond));

    return vDrift;
}



StThreeVectorD StMagUtilities::rKIntegration(double h, double* time, StThreeVectorD r) {

//     int i;
//     StThreeVectorD k1,k2,k3,k4;
    StThreeVectorD p;

    StThreeVectorD vDrift = this->calculateDriftVelocity(r);
//     PR(vDrift/(centimeter/microsecond));

    StThreeVectorD k1 = h*vDrift;
//     PR(k1/centimeter);

    p = r + (k1)/2.;
//     PR(p/centimeter);

    vDrift = this->calculateDriftVelocity(p);
//     PR(vDrift/(centimeter/microsecond));
    StThreeVectorD k2 = h*vDrift;
//     PR(k2/centimeter);
    
    p = r - k1/2. + k2;
//     PR(p/centimeter);

    vDrift = this->calculateDriftVelocity(p);
//     PR(vDrift/(centimeter/microsecond));
    StThreeVectorD k3 = h*vDrift;
//     PR(k3/centimeter);
    
    p =  r + k3;
//     PR(p/centimeter);

    vDrift = this->calculateDriftVelocity(p);
//     PR(vDrift/(centimeter/microsecond));
    StThreeVectorD k4 = h*vDrift;
//     PR(k4/centimeter);
    
    p =  r + (k1 + 2.*k2 + 2.*k3 + k4)/6.;
//     PR(p/centimeter);

    *time = *time + h;
//     PR(*time/microsecond);
    return p;
}

//________________________________________

StThreeVectorD StMagUtilities::clockRotation(StThreeVectorD pos)
{
    double phi = pos.phi();
    if(pos.z() > 0) {
	phi += mWestClockRotation;
	pos.setPhi(phi);
// 	cout << "PHI: " << (phi/degree) << "\tposition= " << pos << "\tnewphi: " << (pos.phi()/degree) << endl;
    }
    else {
// 	cout << "nothing to do" << endl;
    }
    return pos;
}

//________________________________________

// StThreeVectorD StMagUtilities::coordinateShift(StThreeVectorD pos)
// {
    //printf("StMagUtilities::coordinateShift\n");

//     cout.precision(5);
//     cout << "( " << (mCy*mCz) << '\t' << (mSx*mSy*mCz-mCx*mSz) << '\t' << (mCx*mSy*mCz+mSx*mSz) << "\t)" << endl;
//     cout << "( " << (mCy*mSz) << '\t' << (mSx*mSy*mSz+mCx*mCz) << '\t' << (mCx*mSy*mSz-mSx*mCz) << "\t)" << endl;
//     cout << "( " << (-1.*mSy) << '\t' << (mCy*mSx) << '\t' << (mCy*mCx) << "\t)" << endl;


    
//     double xPrime = mTpcCenter.x() +
// 	pos.x()*mCy*mCz + pos.y()*(mSx*mSy*mCz-mCx*mSz) + pos.z()*(mCx*mSy*mCz+mSx*mSz);

//     double yPrime = mTpcCenter.y() +
// 	pos.x()*(mCy*mSz) + pos.y()*(mSx*mSy*mSz+mCx*mCz) + pos.z()*(mCx*mSy*mSz-mSx*mCz);

//     double zPrime =  mTpcCenter.z() +
// 	pos.x()*(-1.*mSy) + pos.y()*(mCy*mSx) +pos.z()*mCy*mCx;

//     return StThreeVectorD(xPrime,yPrime,zPrime);
    
// }

void StMagUtilities::calculateDriftTime(StThreeVectorD& pos) {

    mDriftTime = (pos.z()>0) ?
	((mFrischGrid - pos.z())/(mVdrift)) :
	((pos.z() + mFrischGrid)/(mVdrift));

//     PR(mDriftTime/microsecond);
}


StThreeVectorD StMagUtilities::rotateToMagnet(StThreeVectorD& pos) {

//      cout << "StMagUtilities::ROTATE TO MAGNET" << endl;

//     // used to be in shiftcoordinates
//      cout.precision(5);
//      cout << "( " << (mCy*mCz) << '\t' << (mSx*mSy*mCz-mCx*mSz) << '\t' << (mCx*mSy*mCz+mSx*mSz) << "\t)" << endl;
//      cout << "( " << (mCy*mSz) << '\t' << (mSx*mSy*mSz+mCx*mCz) << '\t' << (mCx*mSy*mSz-mSx*mCz) << "\t)" << endl;
//      cout << "( " << (-1.*mSy) << '\t' << (mCy*mSx) << '\t' << (mCy*mCx) << "\t)" << endl;

    double xPrime =
	pos.x()*mCy*mCz + pos.y()*(mSx*mSy*mCz-mCx*mSz) + pos.z()*(mCx*mSy*mCz+mSx*mSz);

    double yPrime =
	pos.x()*(mCy*mSz) + pos.y()*(mSx*mSy*mSz+mCx*mCz) + pos.z()*(mCx*mSy*mSz-mSx*mCz);

    double zPrime =
	pos.x()*(-1.*mSy) + pos.y()*(mCy*mSx) +pos.z()*mCy*mCx;

    return StThreeVectorD(xPrime,yPrime,zPrime);
}

StThreeVectorD StMagUtilities::rotateToTpc(StThreeVectorD& pos) {

//      cout << "StMagUtilities::ROTATE TO TPC" << endl;
//      cout.precision(5);
//      cout << "( " << (mCy*mCz) << '\t' << (mCy*mSz) << '\t' << (-1.*mSy) << "\t)" << endl;
//      cout << "( " << (mSx*mSy*mCz-mCx*mSz) << '\t' << (mSx*mSy*mSz+mCx*mCz) << '\t' << (mCy*mSx) << "\t)" << endl;
//      cout << "( " << (mCx*mSy*mCz+mSx*mSz) << '\t' << (mCx*mSy*mSz-mSx*mCz) << '\t' << (mCy*mCx) << "\t)" << endl;

    double xPrime =
	pos.x()*mCy*mCz + pos.y()*(mCy*mSz) + pos.z()*(-1.*mSy);

    double yPrime = 
	pos.x()*(mSx*mSy*mCz-mCx*mSz) + pos.y()*(mSx*mSy*mSz+mCx*mCz) + pos.z()*(mCy*mSx);

    double zPrime =
	pos.x()*(mCx*mSy*mCz+mSx*mSz) + pos.y()*(mCx*mSy*mSz-mSx*mCz) +pos.z()*mCy*mCx;

    return StThreeVectorD(xPrime,yPrime,zPrime);
}
