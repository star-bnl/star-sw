/***********************************************************************
 *
 * $Id: StMagUtilities.cxx,v 1.2 2000/11/03 02:41:58 jhthomas Exp $
 *
 * Author: Jim Thomas   11/1/2000
 *
 ***********************************************************************
 *
 * Description: Utilities for the Magnetid Field
 *
 ***********************************************************************
 *
 * $Log: StMagUtilities.cxx,v $
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

  jlow = TMath::BinarySearch( nZ,   ZList,   z   ) ;
  klow = TMath::BinarySearch( nR,   Radius,  r   ) ;
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

void StMagUtilities::UndoDistortion( const Float_t x[3], Float_t Xprime[3] )

{

  double   Const_1, Const_2 ;
  double   OmegaTau ;                                  // OmegaTau carries the sign opposite of B
  Float_t  B[3], ah ;                                  // ah carries the sign opposite of E (for forward integration)
  Int_t    sign, index = 1 ;
  
  if ( x[2] >= 0.0 ) sign =  1 ;                       // (TPC West)
  else               sign = -1 ;                       // (TPC East)  

  Xprime[0]  =  x[0] ;                                 // Integrate backwards from TPC plane to 
  Xprime[1]  =  x[1] ;                                 // the point the electron cluster was born 
  Xprime[2]  =  sign * TPC_Z0 ;                        // on the ifferent readout planes

  BField( Xprime, B ) ;                                // Work in kGauss and cm
  OmegaTau   =  -10. * B[2] * StarDriftV / StarMagE ;  // cm/microsec, Volts/cm, Bz dominates
  Const_1    =  OmegaTau / ( 1. + pow( OmegaTau, 2 ) ) ;
  Const_2    =  pow( OmegaTau, 2 ) / ( 1. + pow( OmegaTau, 2 ) ) ;
  ah = ( x[2] - sign * TPC_Z0 ) / ( NSTEPS - 1 ) ;     // Going Backwards! 

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
  Int_t    sign, index = 1 ;
  
  if ( x[2] >= 0.0 ) sign =  1 ;                       // (TPC West)
  else               sign = -1 ;                       // (TPC East)  

  Xprime[0]  =  x[0] ;                                 // Integrate forward from the point
  Xprime[1]  =  x[1] ;                                 // the electron cluster was born 
  Xprime[2]  =  x[2] ;                                 // to the padplane to estimate the distortion

  BField( Xprime, B ) ;                                // Work in kGauss and cm
  OmegaTau   =  -10. * B[2] * StarDriftV / StarMagE ;  // cm/microsec, Volts/cm, Bz dominates
  Const_1    =  OmegaTau / ( 1. + pow( OmegaTau, 2 ) ) ;
  Const_2    =  pow( OmegaTau, 2 ) / ( 1. + pow( OmegaTau, 2 ) ) ;
  ah = ( sign*TPC_Z0 - x[2] ) / ( NSTEPS - 1 ) ;       // Integrate forward to create the distortion

  for ( Int_t i = 1; i <= NSTEPS; ++i )                // Simpson's Integration Loop
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








