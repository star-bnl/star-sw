/***********************************************************************
 *
 * $Id: StMagUtilities.cxx,v 1.3 2000/12/15 16:10:45 jhthomas Exp $
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

/* 
 StMagUtilities - Jim Thomas 10 October 2000
 A package for
 the STAR magnet field.  Methods included to read in 
 A factor input to the ctor is provided to reverse the
 field sign when necessary. A map arg of the ctor chooses
 constant field (map=1) or the interpolation grid (map=2).
 Work in kGauss, cm - but maps in gauss, cm.
*/  
// Cut out Root specific calls such as Float_t and BinSearch
// Set up a global pointer for each field map, singleton by map
// but don't allow the user to instantiate an infinite number of maps (?)
// kChain should use DB to get values and disallow all forms of instantiation

#include "StMagUtilities.h"
#define gufld gufld_
extern "C" { void gufld(Float_t *, Float_t *) ; }

static StMagUtilities* gMagfield = 0 ;                    // Global pointer for Singleton

//________________________________________

ClassImp(StMagUtilities)

StMagUtilities::StMagUtilities( )

{                                           // StMagUtilities constructor
  if (!gMagfield) 
    {
      float b[3], x[3] = { 0, 0, 0 } ;
      fMap = kMapped ;
      gufld(x,b) ;                          // Read crude values from Chain to get scale
      fFactor = b[2] / 4.980 ;              // Select factor based on Chain values (kGauss) 
      ReadField() ;                         // Read the Mag Field Data File
      gMagfield = this ;                    // Pointer to be used by Singleton
    }
}

//________________________________________

StMagUtilities::StMagUtilities( const EBField map = kMapped, const Float_t factor = 1.0 )

{                                           // StMagUtilities constructor
  if (!gMagfield) 
    {
      fMap = map ;
      fFactor = factor ;
      ReadField() ;                         // Read the Mag Field Data File
      gMagfield = this ;                    // Pointer to be used by Singleton
    }
  else
    {
      cout << "\nWarning: The Magnetic Field Map has already been chosen" << endl ;
      cout << "         Instantiate the mag field pointer with StMagUtilities(<empty>)\n" << endl ;
    }
}

//________________________________________

void StMagUtilities::ReadField( )

{

  FILE *magfile ;
  TString comment, filename ;
  TString MapLocation ;
  TString BaseLocation = "$STAR/StarDb/StMagF/" ;     // Base Directory for Maps

  if ( fMap == kMapped )                    // Mapped field values
    {
      if ( TMath::Abs(fFactor) > 0.8 )      // Scale from full field data 
	{
	  if ( fFactor > 0 )
	    {
	      filename = "bfield_full_positive_2D.dat" ;
	      comment  = "Measured Full Field" ;
	      fRescale = 1 ;                // Normal field 
	    }
	  else
	    {
	      filename = "bfield_full_negative_2D.dat" ;
	      comment  = "Measured Full Field Reversed" ;
	      fRescale = -1 ;               // Reversed field
	    }
	}
      else                                  // Scale from half field data             
	{
	  filename = "bfield_half_positive_2D.dat" ;
          comment  = "Measured Half Field" ;
	  fRescale = 2 ;                    // Adjust scale factor to use half field data
	}
    }
  else if ( fMap == kConstant )             // Constant field values
    {
      filename = "const_full_positive_2D.dat" ;
      comment  = "Constant Full Field" ;
      fRescale = 1 ;                        // Normal field
    }
  else
    {
      fprintf(stderr,"No map available - you must choose a mapped field or a constant field\n");
      exit(1) ;
    }
      
  printf("Reading Magnetic Field:  %s,  Scale factor = %f \n",comment.Data(),fFactor);
  printf("Filename is %s, Adjusted Scale factor = %f \n",filename.Data(),fFactor*fRescale);
  printf("Version: Mag Field Distortions + PadRow13 + Twist + Clock \n" ) ;
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
  
}

//________________________________________

void StMagUtilities::InterpolateBfield( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value )

{

  Float_t fscale ;

  fscale = 0.001*fFactor*fRescale ;               // Scale STAR maps to work in kGauss, cm

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

Float_t StMagUtilities::QuadInterp( const Float_t Xarray[], const Float_t Yarray[], const Float_t x )

{

  Float_t y ;

  y  = (x-Xarray[1]) * (x-Xarray[2]) * Yarray[0] / ( (Xarray[0]-Xarray[1]) * (Xarray[0]-Xarray[2]) ) ; 
  y += (x-Xarray[2]) * (x-Xarray[0]) * Yarray[1] / ( (Xarray[1]-Xarray[2]) * (Xarray[1]-Xarray[0]) ) ; 
  y += (x-Xarray[0]) * (x-Xarray[1]) * Yarray[2] / ( (Xarray[2]-Xarray[0]) * (Xarray[2]-Xarray[1]) ) ; 

  return(y) ;

}

//________________________________________
//
// Main Entry Point for requests for B field in Cartesian coordinates
//
//________________________________________

void StMagUtilities::BField( const Float_t x[3], Float_t B[3] )

{                          

  Float_t r, z, Br_value, Bz_value ;

  if(!gMagfield)
    {
      if (!fMap)
	{
	  printf("Undefined MagF Field called, returning 0\n");
	  B[0] = B[1] = B[2] = 0 ;
	  return ;
	}
      else         
	{
	  gMagfield =  new StMagUtilities(fMap, fFactor);
	}
    }

  z  = x[2] ;
  r  = sqrt ( x[0]*x[0] + x[1]*x[1] ) ;
  
  if ( r != 0.0 )
    {
      gMagfield -> InterpolateBfield( r, z, Br_value, Bz_value ) ;
      B[0] = Br_value * (x[0]/r) ;
      B[1] = Br_value * (x[1]/r) ;
      B[2] = Bz_value ; 
    }
  else
    {
      gMagfield -> InterpolateBfield( r, z, Br_value, Bz_value ) ;
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

  if(!gMagfield)
    {
      if (!fMap)
	{
	  printf("Undefined MagF Field called, returning 0\n");
	  Br_value = Bz_value = 0 ;
	  return ;
	}
      else         
	{
	  gMagfield =  new StMagUtilities(fMap, fFactor);
	}
    }
  
  gMagfield -> InterpolateBfield( r, z, Br_value, Bz_value ) ;

}

//________________________________________
//
// Main Entry Point for requests for E field in Cartesian coordinates
//
//________________________________________

void StMagUtilities::EField( const Float_t x[3], Float_t E[3] )

{                          

  Int_t sign ;

  if ( x[2] >= 0.0 ) sign =  1 ;                       // (TPC West)
  else               sign = -1 ;                       // (TPC East)  

  E[0] = 0 ;
  E[1] = 0 ;
  E[2] = -1.*sign*StarMagE ;                           // EField points at the central membrane, Volts/cm

}

//________________________________________

void StMagUtilities::UndoDistortion( const Float_t x[3], Float_t Xprime[3] )

{

  Float_t Xprime1[3], Xprime2[3] ;
  UndoBDistortion     ( x, Xprime1 ) ;
  UndoPad13Distortion ( Xprime1, Xprime2 ) ;
  UndoTwistDistortion ( Xprime2, Xprime1 ) ;
  UndoClockDistortion ( Xprime1, Xprime  ) ;

}

//________________________________________

void StMagUtilities::UndoBDistortion( const Float_t x[3], Float_t Xprime[3] )

{

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
      ah = ( x[2] - sign * TPC_Z0 ) / ( NSTEPS - 1 ) ; // Going Backwards! 
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

void StMagUtilities::DoDistortion( const Float_t x[3], Float_t Xprime[3] )

{

  double   Const_1, Const_2 ;
  double   OmegaTau ;                                  // OmegaTau carries the sign opposite of B
  Float_t  B[3], ah ;                                  // ah carries the sign opposite of E (for forward integration)
  Int_t    sign, index = 1 , NSTEPS ;              
  
  if ( x[2] >= 0.0 ) sign =  1 ;                       // (TPC West)
  else               sign = -1 ;                       // (TPC East)  

  Xprime[0]  =  x[0] ;                                 // Integrate forward from the point
  Xprime[1]  =  x[1] ;                                 // the electron cluster was born 
  Xprime[2]  =  x[2] ;                                 // to the padplane to estimate the distortion

  BField( Xprime, B ) ;                                // Work in kGauss, cm and assume Bz dominates

  // Theoretically, OmegaTau is defined as shown here.  Instead, we will use scaled values from Aleph
  // OmegaTau   =  -10. * B[2] * StarDriftV / StarMagE ;  // cm/microsec, Volts/cm

  OmegaTau   =  -0.4 * B[2] ;                          // B in kGauss, note that the sign of B is important here and below
  Const_1    =  OmegaTau / ( 1. + pow( OmegaTau, 2 ) ) ;
  Const_2    =  pow( OmegaTau, 2 ) / ( 1. + pow( OmegaTau, 2 ) ) ;

  for ( NSTEPS = 5 ; NSTEPS < 1000 ; NSTEPS += 2 )     // Choose ah to be about 1 cm, NSTEPS must be odd
    {
      ah = ( sign*TPC_Z0 - x[2] ) / ( NSTEPS - 1 ) ;   // Integrate forward to create the distortion
      if ( TMath::Abs(ah) < 1.0 ) break ;
    }

  for ( Int_t i = 1 ; i <= NSTEPS ; ++i )              // Simpson's Integration Loop
    {
      if ( i == NSTEPS ) index = 1 ;
      Xprime[2] +=  index*(ah/3) ;
      BField( Xprime, B ) ;                            // Work in kGauss, cm
      Xprime[0] +=  index*(ah/3)*( Const_2*B[0] - Const_1*B[1] ) / B[2] ;
      Xprime[1] +=  index*(ah/3)*( Const_2*B[1] + Const_1*B[0] ) / B[2] ;
      if ( index != 4 ) index = 4; else index = 2 ;
    }    

  Xprime[2] = x[2] ;                                   // Report Z location as initial start position
                                                       // Just like DAQ does for the STAR TPC
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


//________________________________________


void StMagUtilities::UndoEDistortion( const Float_t x[3], Float_t Xprime[3] )

{

  double   Const_3, Const_4 ;
  double   OmegaTau ;                                  // OmegaTau carries the sign opposite of B
  Float_t  E[3], B[3], ah ;                            // ah carries the sign opposite of E (for forward integration)
  Int_t    sign, index = 1 , NSTEPS ;              
  
  if ( x[2] >= 0.0 ) sign =  1 ;                       // (TPC West)
  else               sign = -1 ;                       // (TPC East)  

  Xprime[0]  =  x[0] ;                                 // Integrate backwards from TPC plane to 
  Xprime[1]  =  x[1] ;                                 // the point the electron cluster was born. 
  Xprime[2]  =  sign * TPC_Z0 ;                        // Prepare for different readout planes

  BField( Xprime, B ) ;                                // Work in kGauss, cm and assume Bz dominates

  // Theoretically, OmegaTau is defined as shown here.  Instead, we will use scaled values from Aleph
  // OmegaTau   =  -10. * B[2] * StarDriftV / StarMagE ;  // cm/microsec, Volts/cm

  OmegaTau   =  -0.4 * B[2] ;                          // B in kGauss, note that the sign of B is important here
  Const_3    =  OmegaTau / ( 1. + pow( OmegaTau, 2 ) ) ;
  Const_4    =  1.0 / ( 1. + pow( OmegaTau, 2 ) ) ;    

  for ( NSTEPS = 5 ; NSTEPS < 1000 ; NSTEPS += 2 )     // Choose ah to be about 1 cm, NSTEPS must be odd
    {
      ah = ( x[2] - sign * TPC_Z0 ) / ( NSTEPS - 1 ) ; // Going Backwards! 
      if ( TMath::Abs(ah) < 1.0 ) break ;
    }

  for ( Int_t i = 1; i <= NSTEPS; ++i )                // Simpson's Integration Loop
    {
      if ( i == NSTEPS ) index = 1 ;
      Xprime[2] +=  index*(ah/3) ;
      EField( Xprime, E ) ;                            // Work in V/cm, and assume Ez dominates
      Xprime[0] +=  index*(ah/3)*( Const_4*E[0] + Const_3*E[1] ) / E[2] ;
      Xprime[1] +=  index*(ah/3)*( Const_4*E[1] - Const_3*E[0] ) / E[2] ;
      if ( index != 4 ) index = 4; else index = 2 ;
    }    

}


//________________________________________


void StMagUtilities::UndoPad13Distortion( const Float_t x[3], Float_t Xprime[3] )

{

  const Int_t   TERMS    = 400 ;                 // Number of terms in the sum
  const Float_t SCALE    = 0.192 ;               // Set the scale for the correction
  const Float_t BOX      = 200.0 - GAPRADIUS ;   // Size of the box in which to work
  const Float_t PI       = TMath::Pi() ;

  static Float_t  C[TERMS] ;                     // Coefficients for series
  static Int_t    Flag = 0 ;                     // Calculate only once
  static Float_t  Const_3, Const_4, OmegaTau ;
  Float_t r, Zdrift, Br_value, Bz_value ;
  Double_t phi, phi0, y, sum = 0.0 ;

  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;            // Phi ranges from pi to -pi
  phi0   =  ( (Int_t)((TMath::Abs(phi)+PI/12.)/(PI/6.) + 6.0 ) - 6.0 ) * PI/6. ;
  if ( phi < 0 ) phi0 *= -1. ;
  y      =  r * TMath::Cos( phi0 - phi ) ;
  Zdrift =  TPC_Z0 - TMath::Abs(x[2]) ;

  if ( Flag == 0 ) 
    {
      C[0] = GAP13 * GG * SCALE / ( 2 * BOX ) ;   
      for ( Int_t i = 1 ; i < TERMS ; i++ )
	  C[i] = 2 * GG * SCALE * TMath::Sin( GAP13*i*PI/( 2*BOX ) ) / ( i * PI ) ;
      BrBzField( 100.0, 1.0, Br_value, Bz_value ) ; // Work in kGauss, cm and assume Bz dominates
      OmegaTau   =  -0.4 * Bz_value ;         // B in kGauss, note that the sign of B is important here
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
  phi =  phi - ( Const_3*(-1*sum)*TMath::Cos(phi0-phi) + Const_4*sum*TMath::Sin(phi0-phi) ) / r ;      
  r   =  r   - ( Const_4*sum*TMath::Cos(phi0-phi) - Const_3*(-1*sum)*TMath::Sin(phi0-phi) ) ;  

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;
  
}

//________________________________________


void StMagUtilities::UndoTwistDistortion( const Float_t x[3], Float_t Xprime[3] )

{

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
  Xprime[0] = x[0] + ( -1* Const_3 * YTWIST/TPCLENGTH + Const_4 * XTWIST/TPCLENGTH ) * Zdrift ;
  Xprime[1] = x[1] + (     Const_3 * XTWIST/TPCLENGTH + Const_4 * YTWIST/TPCLENGTH ) * Zdrift ;
  Xprime[2] = x[2] ;                          // Add (above) to undo the distortion 

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








