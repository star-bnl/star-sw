//:>-----------------------------------------------------------------
//: FILE:       FtfBaseTrack.cxx
//: HISTORY:
//:          28oct1996 version 1.00
//:          11aug1999 ppy primary flag in FtfPara replace with vertexConstrainedFit
//:          11aug1999 ppy primary flag in track filled now 
//:           3sep1999 ppy fitLine, dpsi cannot be greater than 1. Check introduced
//:           5oct1999 ppy fitLine, bug corrected
//:           6oct1999 ppy Root switch added
//:           9mar2000 ppy extrapolation methods added
//:           9mar2000 ppy lots of changes to use void pointers
//:          28mar2000 ppy closestApporach split in two methods
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfBaseTrack
//: DESCRIPTION: Basic Description of a track P
//: AUTHOR:      ppy - Pablo Yepes, yepes@rice.edu
//:>------------------------------------------------------------------
#include "FtfBaseTrack.h"

#ifdef SL3ROOT
ClassImp(FtfBaseTrack)
#endif


void ftfMatrixDiagonal ( double *h, double &h11, double &h22, double &h33 ) ;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//      Track Initialization
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FtfBaseTrack::FtfBaseTrack ( ){
   firstHit = 0 ;
   lastHit  = 0 ;
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//       Fit a circle
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfBaseTrack::fitHelix (  ) 
{
   if ( fitCircle ( ) ){
      printf ( " Problem in Fit_Circle " ) ;
      return 1 ;
   }
//
//     Fit line in s-z plane now
//
   if ( fitLine ( )) {
      printf ( " Problem fitting a line " ) ;
      return 1 ;
   }
   return 0 ;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//       End of Fit Helix
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    
//  Fits circle parameters using algorithm
//  described by ChErnov and Oskov in Computer Physics
//  Communications.
// 
//  Written in FORTRAN by Jawluen Tang, Physics department , UT-Austin 
//  Moved to C by Pablo Yepes
//---------------------------------------------------------------
int FtfBaseTrack::fitCircle (  ) 
{
  double wsum  = 0.0 ;
  double xav   = 0.0 ;
  double yav   = 0.0 ;
//
//     Loop over hits calculating average
//
  for ( startLoop() ; done() ; nextHit() ) { 
    
        FtfBaseHit* cHit = (FtfBaseHit *)currentHit ;
	cHit->wxy = 1.0F/ (double)(cHit->dx*cHit->dx + cHit->dy*cHit->dy) ;
	wsum      += cHit->wxy ;
	xav       += cHit->wxy * cHit->x ;
	yav       += cHit->wxy * cHit->y ;
  }
  if ( getPara()->vertexConstrainedFit ) {
	 wsum += getPara()->xyWeightVertex ;
	 xav  += getPara()->xVertex ;
	 yav  += getPara()->yVertex ;
  }
  xav = xav / wsum ;
  yav = yav / wsum ;
//
//  CALCULATE <X**2>, <XY>, AND <Y**2> WITH <X> = 0, & <Y> = 0
//
  double xxav  = 0.0 ;
  double xyav  = 0.0 ; 
  double yyav  = 0.0 ;
  double xi, yi ;

  for ( startLoop() ; done() ; nextHit() ) { 
        FtfBaseHit* cHit = (FtfBaseHit *)currentHit ;
	xi        = cHit->x - xav ;
	yi        = cHit->y - yav ;
	xxav     += xi * xi * cHit->wxy ;
	xyav     += xi * yi * cHit->wxy ;
	yyav     += yi * yi * cHit->wxy ;
  }
  if ( getPara()->vertexConstrainedFit ) {
	xi        = getPara()->xVertex - xav ;
	yi        = getPara()->yVertex - yav ;
	xxav     += xi * xi * getPara()->xyWeightVertex ;
	xyav     += xi * yi * getPara()->xyWeightVertex ;
	yyav     += yi * yi * getPara()->xyWeightVertex ; 
  }
  xxav = xxav / wsum ;
  xyav = xyav / wsum ;
  yyav = yyav / wsum ;
//
//-->  ROTATE COORDINATES SO THAT <XY> = 0
//
//-->  SIGN(C**2 - S**2) = SIGN(XXAV - YYAV) >
//-->  &                                     > ==> NEW : (XXAV-YYAV) > 0
//-->  SIGN(S) = SIGN(XYAV)                  >

  double a = fabs( xxav - yyav ) ;
  double b = 4.0 * xyav * xyav ;

  double asqpb  = a * a + b  ;
  double rasqpb = sqrt ( asqpb) ;

  double splus  = 1.0 + a / rasqpb ;
  double sminus = b / (asqpb * splus) ;

  splus  = sqrt (0.5 * splus ) ;
  sminus = sqrt (0.5 * sminus) ;
//
//->  FIRST REQUIRE : SIGN(C**2 - S**2) = SIGN(XXAV - YYAV)
//
  double sinrot, cosrot ;
  if ( xxav <= yyav ) {
	 cosrot = sminus ;
	 sinrot = splus  ;
  }
  else {
	  cosrot = splus ;
	  sinrot = sminus ;
  }
//
//->  REQUIRE : SIGN(S) = SIGN(XYAV) * SIGN(C) (ASSUMING SIGN(C) > 0)
//
  if ( xyav < 0.0 ) sinrot = - sinrot ;
//
//-->  WE NOW HAVE THE SMALLEST ANGLE THAT GUARANTEES <X**2> > <Y**2>
//-->  TO GET THE SIGN OF THE CHARGE RIGHT, THE NEW X-AXIS MUST POINT
//-->  OUTWARD FROM THE ORGIN.  WE ARE FREE TO CHANGE SIGNS OF BOTH
//-->  COSROT AND SINROT SIMULTANEOUSLY TO ACCOMPLISH THIS.
//
//-->  CHOOSE SIGN OF C WISELY TO BE ABLE TO GET THE SIGN OF THE CHARGE
//
  if ( cosrot*xav+sinrot*yav < 0.0 ) {
	  cosrot = -cosrot ;
	  sinrot = -sinrot ;
  }
//
//->  NOW GET <R**2> AND RSCALE= SQRT(<R**2>)
//
  double rrav   = xxav + yyav ;
  double rscale = sqrt(rrav) ;

  xxav   = 0.0 ;
  yyav   = 0.0 ;
  xyav   = 0.0 ;
  double xrrav	 = 0.0 ;
  double yrrav	 = 0.0 ;
  double rrrrav  = 0.0 ;

  double xixi, yiyi, riri, wiriri, xold, yold ;
  for ( startLoop() ; done() ; nextHit() ) { 
        FtfBaseHit* cHit = (FtfBaseHit *)currentHit ;
	xold = cHit->x - xav ;
	yold = cHit->y - yav ;
//
//-->  ROTATE SO THAT <XY> = 0 & DIVIDE BY RSCALE SO THAT <R**2> = 1
//
	xi = (  cosrot * xold + sinrot * yold ) / rscale ;
	yi = ( -sinrot * xold + cosrot * yold ) / rscale ;
          
	xixi   = xi * xi ;
	yiyi   = yi * yi ;
	riri   = xixi + yiyi ;
	wiriri = cHit->wxy * riri ;

	xyav   += cHit->wxy * xi * yi ;
	xxav   += cHit->wxy * xixi ;
	yyav   += cHit->wxy * yiyi ;

	xrrav  += wiriri * xi ;
	yrrav  += wiriri * yi ;
	rrrrav += wiriri * riri ;
  }
//
//   Include vertex if required
//
  if ( getPara()->vertexConstrainedFit ) {
	xold = getPara()->xVertex - xav ;
	yold = getPara()->yVertex - yav ;
//
//-->  ROTATE SO THAT <XY> = 0 & DIVIDE BY RSCALE SO THAT <R**2> = 1
//
	xi = (  cosrot * xold + sinrot * yold ) / rscale ;
	yi = ( -sinrot * xold + cosrot * yold ) / rscale ;
          
	xixi   = xi * xi ;
	yiyi   = yi * yi ;
	riri   = xixi + yiyi ;
	wiriri = getPara()->xyWeightVertex * riri ;

	xyav   += getPara()->xyWeightVertex * xi * yi ;
	xxav   += getPara()->xyWeightVertex * xixi ;
	yyav   += getPara()->xyWeightVertex * yiyi ;

	xrrav  += wiriri * xi ;
	yrrav  += wiriri * yi ;
	rrrrav += wiriri * riri ;
  }
//
//    
//
//-->  DIVIDE BY WSUM TO MAKE AVERAGES
//
  xxav    = xxav   / wsum ;
  yyav    = yyav   / wsum ;
  xrrav   = xrrav  / wsum ;
  yrrav   = yrrav  / wsum ;
  rrrrav  = rrrrav / wsum ;
  xyav    = xyav   / wsum ;

  int const ntry = 5 ;
//
//-->  USE THESE TO GET THE COEFFICIENTS OF THE 4-TH ORDER POLYNIMIAL
//-->  DON'T PANIC - THE THIRD ORDER TERM IS ZERO !
//
  double xrrxrr = xrrav * xrrav ;
  double yrryrr = yrrav * yrrav ;
  double rrrrm1 = rrrrav - 1.0  ;
  double xxyy   = xxav  * yyav  ;        

  double c0  =          rrrrm1*xxyy - xrrxrr*yyav - yrryrr*xxav ;
  double c1  =        - rrrrm1      + xrrxrr      + yrryrr   - 4.0*xxyy ;        
  double c2  =   4.0  + rrrrm1                               - 4.0*xxyy ;           
  double c4  = - 4.0  ;                
//
//-->  COEFFICIENTS OF THE DERIVATIVE - USED IN NEWTON-RAPHSON ITERATIONS
//
  double c2d =   2.0 * c2 ;
  double c4d =   4.0 * c4 ;
//
//-->  0'TH VALUE OF LAMDA - LINEAR INTERPOLATION BETWEEN P(0) & P(YYAV)
//
//   LAMDA = YYAV * C0 / (C0 + YRRSQ * (XXAV-YYAV))
  double lamda  = 0.0 ;
  double dlamda = 0.0 ;
//
  double chiscl = wsum * rscale * rscale ;
  double dlamax = 0.001 / chiscl ;   
   
  double p, pd ;
  for ( int itry = 1 ; itry <= ntry ; itry++ ) {
     p      = c0 + lamda * (c1 + lamda * (c2 + lamda * lamda * c4 )) ;
     pd     = (c1 + lamda * (c2d + lamda * lamda * c4d)) ;
     dlamda = -p / pd ;
     lamda  = lamda + dlamda ;
     if (fabs(dlamda)<   dlamax) break ;
  }

  chi2[0]  = (double)(chiscl * lamda) ;
 // double dchisq = chiscl * dlamda ;	     
//
//-->  NOW CALCULATE THE MATRIX ELEMENTS FOR ALPHA, BETA & KAPPA
//
  double h11   = xxav  -     lamda ;
  double h14   = xrrav ;
  double h22   = yyav  -     lamda ; 
  double h24   = yrrav ;
  double h34   = 1.0   + 2.0*lamda ;
  if ( h11 == 0.0 || h22 == 0.0 ){
	  printf ( " Problems fitting a circle " ) ;
	  return 1 ;
  }
  double rootsq = (h14*h14)/(h11*h11) + 4.0*h34 ;

  double ratio, kappa, beta ;
  if ( fabs(h22) > fabs(h24) ) {
     ratio  = h24 / h22 ;
     rootsq = ratio * ratio + rootsq ;
     kappa = 1.0 / sqrt(rootsq) ;
     beta  = - ratio * kappa ;
  }
  else {
     ratio  = h22 / h24 ;
     rootsq = 1.0 + ratio * ratio * rootsq ;
     beta  = 1.0 / sqrt(rootsq) ;
     if ( h24 > 0 ) beta = - beta ;
     kappa = -ratio * beta ;
  }            
  double alpha = - (h14/h11) * kappa ;
//
//-->  transform these into the lab coordinate system
//-->  first get kappa and back to real dimensions
//
  double kappa1 = kappa / rscale ;
  double dbro   = 0.5   / kappa1 ;
//
//-->  next rotate alpha and beta and scale
//
  double alphar = (cosrot * alpha - sinrot * beta)* dbro ;
  double betar  = (sinrot * alpha + cosrot * beta)* dbro ;
//
//-->  then translate by (xav,yav)
//
  double acent  = (double)(xav - alphar) ;
  double bcent  = (double)(yav - betar ) ;
  double radius = (double)dbro ;
//
//   Get charge
//
  q = ( ( yrrav < 0 ) ? 1 : -1 ) ;
//
//    Get other track parameters
//
  double x0, y0 ;
  if ( getPara()->vertexConstrainedFit ) {
     flag = 1 ; // primary track flag
     x0   = getPara()->xVertex ;
     y0   = getPara()->yVertex ;
     phi0 = getPara()->phiVertex ;
     r0   = getPara()->rVertex ;
  } 
  else {
     FtfBaseHit* lHit = (FtfBaseHit *)lastHit ;
     flag =  0 ; // primary track flag
     x0   =  lHit->x  ;
     y0   =  lHit->y  ;
     phi0 =  atan2(lHit->y,lHit->x);
     if ( phi0 < 0 ) phi0 += twoPi ;
     r0   =  sqrt ( lHit->x * lHit->x + lHit->y * lHit->y )  ;
  }
  //
  psi  = (double)atan2(bcent-y0,acent-x0) ;
  psi  = psi + q * 0.5F * pi ;
  if ( psi < 0 ) psi = psi + twoPi ;
  pt   = (double)(2.9979e-3 * getPara()->bField * radius ) ;
//
//    Get errors from fast fit
//
   if ( getPara()->getErrors ) getErrorsCircleFit ( acent, bcent, radius ) ;
//
  return 0 ;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    End Fit Circle
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Fit Line in s-z plane
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfBaseTrack::fitLine ( )
{
//
//     initialization 
//
   double sum = 0.F ;
   double ss  = 0.F ;
   double sz  = 0.F ;
   double sss = 0.F ;
   double ssz = 0.F ;
//
//     find sum , sums ,sumz, sumss 
// 
   double dx, dy ;
   double radius = (double)(pt / ( 2.9979e-3 * getPara()->bField ) ) ;
   if ( getPara()->vertexConstrainedFit ) {
      dx   = ((FtfBaseHit *)firstHit)->x - getPara()->xVertex ;
      dy   = ((FtfBaseHit *)firstHit)->y - getPara()->yVertex ;
   }
   else {
      dx   = ((FtfBaseHit *)firstHit)->x - ((FtfBaseHit *)lastHit)->x ;
      dy   = ((FtfBaseHit *)firstHit)->y - ((FtfBaseHit *)lastHit)->y ;
   }
   double localPsi = 0.5F * sqrt ( dx*dx + dy*dy ) / radius ;
   double total_s ;
   if ( fabs(localPsi) < 1. ) {
      total_s = 2.0F * radius * asin ( localPsi ) ;
   } 
   else { 
      total_s = 2.0F * radius * M_PI ;
   } 

//
   FtfBaseHit *previousHit = 0  ;
	
   for ( startLoop() ; done() ; nextHit() ) {
      FtfBaseHit* cHit = (FtfBaseHit *)currentHit ;
      if ( currentHit != firstHit ) {
         dx   = cHit->x - previousHit->x ;
	 dy   = cHit->y - previousHit->y ;
	 dpsi = 0.5F * (double)sqrt ( dx*dx + dy*dy ) / radius ;
	 cHit->s = previousHit->s - 2.0F * radius * (double)asin ( dpsi ) ;
      }
      else
         cHit->s = total_s ;
        
      sum += cHit->wz ;
      ss  += cHit->wz * cHit->s ;
      sz  += cHit->wz * cHit->z ;
      sss += cHit->wz * cHit->s * cHit->s ;
      ssz += cHit->wz * cHit->s * cHit->z ;
      previousHit = cHit ;
   }

   double det = sum * sss - ss * ss;
   if ( fabs(det) < 1e-20){ 
      chi2[1] = 99999.F ;
      return 0 ;
   }
//
//     compute the best fitted parameters A,B
//
   tanl = (double)((sum * ssz - ss * sz ) / det );
   z0   = (double)((sz * sss - ssz * ss ) / det );
//
//     calculate chi-square 
//
   chi2[1] = 0.F ;
   double r1 ;
   for ( startLoop() ; done() ; nextHit() ) {
      FtfBaseHit* cHit = (FtfBaseHit *)currentHit ;
      r1   = cHit->z - tanl * cHit->s - z0 ;
      chi2[1] += (double) ( (double)cHit->wz * (r1 * r1) );
   }
//
//     calculate estimated variance
//      varsq=chi/(double(n)-2.) 
//     calculate covariance matrix 
//      siga=sqrt(varsq*sxx/det) 
//      sigb=sqrt(varsq*sum/det) 
//
    dtanl = (double) ( sum / det );
    dz0   = (double) ( sss / det );
   
    return 0 ;
} 
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    End Fit Line
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// CIRCOV - a covariance matrix calculation program for circle fitting 
// DESCRIPTION: 
// Compute the covariance matrix of an effective circle fitting algorithm 
// The circle equation is (X(I)-A)**2 + (Y(I)-B)**2 = R**2. 
// The functional minimum is W(I)*[(X(I)-A)**2+(Y(I)-B)**2-R*R]**2/(R*R) 
// For details about the algorithm, see 
// N.I. CHERNOV, G.A. OSOSKOV, COMPUT. PHYS. COMMUN. 33(1984) 329-333 
// INPUT ARGUMENTS: */
//      A              - Best fitted circle center in X axis, REAL 
//      B              - Best fitted circle center in Y axis, REAL 
//      R              - Best fitted radius                   REAL 
//
// From a routine written in Fortran by  AUTHOR:
//  Jawluen Tang, Physics department , UT-Austin 
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfBaseTrack::getErrorsCircleFit ( double a, double b, double r ) {
    
   double h[9] = { 0. };
   double dx, dy ;
   double h11, h22, h33 ;
   static int j ;
   static double ratio, c1, s1;
   static double hyp;

   
   for (j = 0; j < 9; j++ ) {
      h[j] = 0.;
   }
//
//    If circle fit was not used the
//    errors in the real space need to
//    be calculated
//
   if ( pt < getPara()->ptMinHelixFit ) {
      for ( startLoop() ; done() ; nextHit() ) { 

         FtfBaseHit* cHit = (FtfBaseHit *)currentHit ;
         cHit->wxy = 1.0F/ (double)(cHit->dx*cHit->dx + cHit->dy*cHit->dy) ;
      }
   }
//
//    Loop over points in fit
//
   for ( startLoop() ; done() ; nextHit() ) {
      FtfBaseHit* cHit = (FtfBaseHit *)currentHit ;
      dx = cHit->x - a;
      dy = cHit->y - b;
      hyp = (double)sqrt( dx * dx + dy * dy );
      s1 = dx / hyp;
      c1 = dy / hyp;
      ratio = r / hyp;
      h[0] += cHit->wxy * (ratio * (s1 * s1 - 1) + 1);
      h[1] += cHit->wxy * ratio * s1 * c1;
      h[2] += cHit->wxy * s1;
      h[4] += cHit->wxy * (ratio * (c1 * c1 - 1) + 1);
      h[5] += cHit->wxy * c1;
      h[8] += cHit->wxy ;
   }
   h[3]  = h[1];
   h[6]  = h[2];
   h[7]  = h[5];

   ftfMatrixDiagonal  ( h, h11, h22, h33 ) ;
//
//   Calculate pt error now
//
   dpt          = (double)(2.9979e-3 * getPara()->bField * h33 );
//
//     Get error in psi now
//
   if ( getPara()->vertexConstrainedFit ) {
      dx = a ;
      dy = b ;
   }
   else {
      dx = ((FtfBaseHit *)lastHit)->x + a - ((FtfBaseHit *)firstHit)->x ;
      dy = ((FtfBaseHit *)lastHit)->y + b + ((FtfBaseHit *)firstHit)->y ;
   }
   double w   = dy / dx ;
   dpsi  = (double)(( 1. / ( 1. + w*w ) ) * ( h22 / dx - dy * h11 / ( dx*dx ) )) ;

   return 0 ;
}

//*************************************************************************
//   Prints one track information
//*************************************************************************
void FtfBaseTrack::Print ( int level )
{
   double pmom, pz;   
/*
----->   Print info
*/
   if ( level > 9 ) {
      pz   = pt * tanl ;
      pmom = (double)sqrt ( pz * pz + pt * pt  ) ;
      printf ( " \n =======> Track      %d  <======== ", id ) ;
      printf ( " \n p,  pt, q         %7.2f  %7.2f  %2d ", pmom, pt, q ) ;
   }   
   if ( level > 19 ) {
      printf ( " \n r0,   z0,  nHits  %7.2f  %7.2f %d    ", r0, z0, nHits ) ;
      printf ( " \n phi0, psi, tanl   %7.2f  %7.2f %7.2f ", phi0, psi, tanl ) ;
   }  
   else printf ( "\n " ) ; 

   if ( level > 29 ) {
      printf ( " \n chi2 (s,z)        %6.2e  %6.2e ", chi2[0],
                                                      chi2[1] ) ;
   }
   else printf ( "\n " ) ;

   
   if ( fmod((double)level,10.) > 0 ) {
      printf ( " \n *** Clusters in this track *** " ) ;
      ((FtfBaseHit *)firstHit)->print ( 10 ) ;
      for ( startLoop() ; done() ; nextHit()  ) { 
        ((FtfBaseHit *)currentHit)->print ( 1 ) ;
      }
   }
   printf ( "\n " ) ; 
}
/*:>--------------------------------------------------------------------
**: METHOD:   Finds point of closest approach
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@rice.edu
**: ARGUMENTS:
**:          IN:    xBeam, yBeam: beam position
**:
**: RETURNS:    
**:             tHit            - Point closest approach to center
*:>------------------------------------------------------------------*/
Ftf3DHit FtfBaseTrack::closestApproach ( double xBeam, double yBeam ) {
   double rc, xc, yc ;
   return getClosest ( xBeam, yBeam, rc, xc, yc ) ;
}
/*:>--------------------------------------------------------------------
**: METHOD:   Finds point of closest approach
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@rice.edu
**: ARGUMENTS:
**:          IN:    xBeam, yBeam: beam position
**:         OUT:
**:                  rc, xc, yc  track circle radius and center
**:
**: RETURNS:    
**:             tHit            - Point closest approach to center
*:>------------------------------------------------------------------*/
Ftf3DHit FtfBaseTrack::getClosest ( double xBeam, double yBeam,
                                    double &rc, double &xc, double &yc ) {
   double xp, yp, zp ;
   xp = yp = 0. ;
//--------------------------------------------------------
//     Get track parameters
//--------------------------------------------------------
   double tPhi0 = psi + double(q) * 0.5 * M_PI / fabs((double)q) ;

   double x0   = r0 * cos(phi0) ;
   double y0   = r0 * sin(phi0) ;
   rc   = pt / ( bFactor * getPara()->bField )  ;
   xc   = x0 - rc * cos(tPhi0) ;
   yc   = y0 - rc * sin(tPhi0) ;

   getClosest ( xBeam, yBeam, rc, xc, yc, xp, yp ) ;

//-----------------------------------------------------------------
//     Get the z coordinate
//-----------------------------------------------------------------
   double angle  = atan2 ( (yp-yc), (xp-xc) ) ;
   if ( angle < 0. ) angle = angle + 2.0 * M_PI ;

   double dangle = angle  - tPhi0  ;
   dangle = fmod ( dangle, 2.0 * M_PI ) ;
   if ( fabs(dangle) < 1.e-10 ) dangle = 0 ; // Problems with -0.000 values
   if ( (float(q) * dangle) < 0 )
        dangle = dangle + float(q) * 2. * M_PI ;
   
   double stot   = fabs(dangle) * rc ;
   zp   = z0 - stot * tanl ;

   Ftf3DHit tHit(xp,yp,zp) ;
   return tHit ;
}
/*:>--------------------------------------------------------------------
**: METHOD:   Finds point of closest approach
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@rice.edu
**: ARGUMENTS:
**:          IN:    xBeam, yBeam: beam position
**:                 rc, xc, yc  : track circle radius and center
**:         OUT:
**:                 double xClosest, yClosest
**:
**: RETURNS: 0 if Ok
*:>------------------------------------------------------------------*/
int FtfBaseTrack::getClosest ( double xBeam, double yBeam,
                               double  rc, double xc, double yc,
                               double& xClosest, double& yClosest ) {
//----------------------------------------------------------
//     Shift center to respect beam axis
//----------------------------------------------------------
   double dx = xc - xBeam ;
   double dy = yc - yBeam ;
//----------------------------------------------------------
//     Solve the equations 
//----------------------------------------------------------
   double fact = rc / sqrt ( dx * dx + dy * dy ) ;
   double f1   = 1. + fact ;
   double f2   = 1. - fact ;

   double dx1 = dx * f1 ;
   double dy1 = dy * f1 ;
   double d1 = sqrt ( dx1 * dx1 + dy1 * dy1 ) ;

   double dx2 = dx * f2 ;
   double dy2 = dy * f2 ;
   double d2 = sqrt ( dx2 * dx2 + dy2 * dy2 ) ;
//---------------------------------------------------------------
//     Choose the closest
//---------------------------------------------------------------
   if ( d1 < d2 ) {
      xClosest = dx1 + xBeam ;
      yClosest = dy1 + yBeam ;
   }
   else {
      xClosest = dx2 + xBeam ;
      yClosest = dy2 + yBeam ;
   }
   return 0 ;
}
/*:>--------------------------------------------------------------------
**: METHOD:   Extrapolates track to cylinder with radius r
**:
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@rice.edu
**: ARGUMENTS:
**:          IN:
**:             track           - Global track pointer
**:             r               - Cylinder radius     
**:         OUT:
**:             x,y,z           - Extrapolated point
**:             xc,yc,rr        - Center and radius track circle in x-y plane
**:
**: RETURNS:    0=ok, <>0 error
**:>------------------------------------------------------------------*/
Ftf3DHit FtfBaseTrack::extraRadius ( double r ) { 
   double phi ;
//
// Default values 
//
   double x, y, z, rc, xc, yc ;
   x = y = z = 0.F ;
//
//    If error return with error 
//
   Ftf3DHit tHit(0,0,0) ;
   if ( extraRCyl ( r, phi, z, rc, xc, yc ) ) return tHit ;
//
//   Otherwise get point in cartesian coordinates
//
   x = r * cos(phi) ;
   y = r * sin(phi) ; 
   tHit.x = x ;
   tHit.y = y ;
   tHit.z = z ;

   return tHit ;
}
/*:>--------------------------------------------------------------------
**: METHOD:   Extrapolates track to cylinder with radius r
**:
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@rice.edu
**: ARGUMENTS:
**:          IN:
**:             r               - Cylinder radius
**:         OUT:
**:             phi,z           - Extrapolated point
**:             xc,yc,rc        - Center and radius track circle in x-y plane
**:
**: RETURNS:    0=ok, <>0 error
**:>------------------------------------------------------------------*/
int FtfBaseTrack::extraRCyl ( double &r,  double &phi, double &z,
                              double &rc, double &xc,  double &yc )  {

   double td  ;
   double fac1,sfac, fac2,deltat ;
//--------------------------------------------------------
//     Get track parameters
//--------------------------------------------------------
   double tPhi0 = psi + double(q) * 0.5 * M_PI / fabs((double)q) ;
   double x0    = r0 * cos(phi0) ;
   double y0    = r0 * sin(phi0) ;
   rc    = fabs(pt) / ( bFactor * getPara()->bField )  ;
   xc    = x0 - rc * cos(tPhi0) ;
   yc    = y0 - rc * sin(tPhi0) ;
//  
//     Check helix and cylinder intersect
// 
   fac1 = xc*xc + yc*yc ;
   sfac = sqrt( fac1 ) ;
//  
//  If they don't intersect return
//  
   if ( fabs(sfac-rc) > r || fabs(sfac+rc) < r ) {
      printf ( "particle does not intersect \n" ) ;
      return  1 ;
    }
//  
//     Find intersection
//  
   fac2   = ( r*r + fac1 - rc*rc) / (2.00 * r * sfac ) ;
   phi    = atan2(yc,xc) + float(q)*acos(fac2) ;
   td     = atan2(r*sin(phi) - yc,r*cos(phi) - xc) ;
/*
   double xx = x0 + rc * cos(phi);
   double yy = x0 + rc * cos(phi);
   double ttphi = atan2((yy-yc),(xx-xc));
   double ppsi = ttphi - double(q) * 0.5 * M_PI / fabs((double)q) ;
*/
//    Intersection in z
    
   if ( td < 0 ) td = td + 2. * M_PI ;
   deltat = fmod((-q*td + q*tPhi0),2*M_PI) ;
   if ( deltat < 0.      ) deltat += 2. * M_PI ;
   if ( deltat > 2.*M_PI ) deltat -= 2. * M_PI ;
   z = z0 + rc * tanl * deltat ;
// 
//    That's it
// 
   return 0 ;
}
/*:>--------------------------------------------------------------------
**: METHOD:   Calculates intersection of track with plane define by line
**:           y = a x + b and the z 
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@rice.edu
**: ARGUMENTS:
**:          IN:
**:             a, b            - Line parameters
**:         OUT:
**:             crossPoint      - intersection point
**:
**: RETURNS:    0=ok, <>0 track does not cross the plane
**:>------------------------------------------------------------------*/
int FtfBaseTrack::intersectorZLine ( double a, double b, Ftf3DHit& cross ) {
//
//   Calculate circle center and radius
//
   double x0    = r0 * cos(phi0) ;
   double y0    = r0 * sin(phi0) ;
   double trackPhi0 = psi + q * 0.5 * M_PI / fabs((double)q) ;
   double rc   = pt  / ( bFactor * bField )  ;
   double xc   = x0 - rc * cos(trackPhi0) ;
   double yc   = y0 - rc * sin(trackPhi0) ;

   double ycPrime = yc - b ;
   double aa = ( 1. + a * a ) ;
   double bb = -2. * ( xc + a * ycPrime ) ;
   double cc = ( xc * xc + ycPrime * ycPrime - rc * rc ) ;
   
   double racine = bb * bb - 4. * aa * cc ;
   if ( racine < 0 ) return 1 ;
   double rootRacine = sqrt(racine) ;

   double oneOverA = 1./aa;
//
//   First solution
//
   double x1 = 0.5 * oneOverA * ( -1. * bb + rootRacine ) ; 
   double y1 = a * x1 + b ;
   double r1 = sqrt(x1*x1+y1*y1);
//
//   Second solution
//
   double x2 = 0.5 * oneOverA * ( -1. * bb - rootRacine ) ; 
   double y2 = a * x2 + b ;
   double r2 = sqrt(x2*x2+y2*y2);
//
//    Choose close to (0,0) 
//
   double xHit ;
   double yHit ;
   if ( r1 < r2 ) {
      xHit = x1 ;
      yHit = y1 ;
   }
   else {
      xHit = x2 ;
      yHit = y2 ;
   }
//-------------------------------------------------------------------
//     Get the z coordinate
//-------------------------------------------------------------------
   double angle  = atan2 ( (yHit-yc), (xHit-xc) ) ;
   if ( angle < 0. ) angle = angle + 2.0 * M_PI ;
   //   printf ( " angle %f trackPhi0 %f \n ", angle, trackPhi0 ) ;
   double dangle = angle  - trackPhi0  ;
   dangle = fmod ( dangle, 2.0 * M_PI ) ;
   if ( (q * dangle) > 0 ) dangle = dangle - q * 2. * M_PI  ;

   double stot   = fabs(dangle) * rc ;
   double zHit   = z0 + stot * tanl ;
//
   cross.set ( xHit, yHit, zHit ) ;
//
   return 0 ;
}
/*:>--------------------------------------------------------------------
**: METHOD:   Calculates trajectory length between two points on a track
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@rice.edu
**: ARGUMENTS:
**:          IN:
**:             track           - Track pointer
**:             x1, y1          - Point 1
**:             x2, y2          - Point 2
**:         OUT:
**:
**: RETURNS:    0=ok, <>0 error
**:>------------------------------------------------------------------*/
double FtfBaseTrack::arcLength ( double x1, double y1, double x2, double y2 )
{
   double x0, y0, xc, yc, rc ;
   double angle_1, angle_2, d_angle, sleng_xy, sleng ;
/*----------------------------------------------------------
       Get track parameters
----------------------------------------------------------*/

   x0   = r0 * cos(phi0) ;
   y0   = r0 * sin(phi0) ;
   rc   = pt / ( bFactor * getPara()->bField )  ;
   double tPhi0 = psi + double(q) * 0.5 * M_PI / fabs((double)q) ;
   xc   = x0 - rc * cos(tPhi0) ;
   yc   = y0 - rc * sin(tPhi0) ;
/*
    Get angle difference 
*/
   angle_1  = atan2 ( (y1-yc), (x1-xc) ) ;
   if ( angle_1 < 0 ) angle_1 = angle_1 + 2. * M_PI ;
   angle_2  = atan2 ( (y2-yc), (x2-xc) ) ;
   if ( angle_2 < 0 ) angle_2 = angle_2 + 2. * M_PI ;
   d_angle  = double(q) * ( angle_1 - angle_2 ) ;
   d_angle  = fmod ( d_angle, 2. * M_PI ) ;
   if ( d_angle < 0 ) d_angle = d_angle + 2. * M_PI ;
/*----------------------------------------------------------
       Get total angle and total trajectory
----------------------------------------------------------*/
   sleng_xy = fabs ( rc ) * d_angle ;
   sleng    = sleng_xy * sqrt ( 1.0 + tanl * tanl )  ;
   return sleng ;

}
/*:>--------------------------------------------------------------------
**: METHOD:   Phi rotates the track
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@rice.edu
**: ARGUMENTS:
**:          IN:
**:             deltaPhi        - Angle to rotate in phi
**:
**: RETURNS:    0=ok, <>0 error
**:>------------------------------------------------------------------*/
int FtfBaseTrack::phiRotate ( double deltaPhi ) {
   phi0 += deltaPhi ;
   if ( phi0 > 2. * M_PI ) phi0 -= 2. * M_PI ;
   if ( phi0 <         0 ) phi0 += 2. * M_PI ;
   psi  += deltaPhi ;
   if ( psi > 2. * M_PI ) psi -= 2. * M_PI ;
   if ( psi <         0 ) psi += 2. * M_PI ;

   return 0 ;
}
/*:>--------------------------------------------------------------------
**: METHOD:   Updates track parameters to point of intersection with 
**:           cylinder of radius r
**:
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@rice.edu
**: ARGUMENTS:
**:          IN:
**:             radius         - Cylinder radius to extrapolate track
**:         OUT:
**:
**:>------------------------------------------------------------------*/
void FtfBaseTrack::updateToRadius  ( double radius ) {

   double phiExtra, zExtra, rCircle, xCircleCenter, yCircleCenter ;

   int ok = extraRCyl ( radius,  phiExtra, zExtra, rCircle, xCircleCenter, yCircleCenter ) ;
   if ( ok ) {
      printf ( "FtfBaseTrack::updateToRadius: track %d does not intersect radius %f\n", 
                id, radius ) ;
      return ;
   }

   double xExtra = radius * cos(phiExtra) ;
   double yExtra = radius * sin(phiExtra) ;

   double tPhi = atan2(yExtra-yCircleCenter,xExtra-xCircleCenter);
                    
// if ( tPhi < 0 ) tPhi += 2. * M_PI ;

   double tPsi = tPhi - double(q) * 0.5 * M_PI / fabs((double)q) ;
   if ( tPsi > 2. * M_PI ) tPsi -= 2. * M_PI ;
   if ( tPsi < 0.        ) tPsi += 2. * M_PI ;
//
//    Update track parameters
//
   r0   = radius ;
   phi0 = phiExtra ;
   z0   = zExtra ;
   psi  = tPsi ;
}
/*:>--------------------------------------------------------------------
**: METHOD:   Updates track parameters to point of closest approach
**:
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@rice.edu
**: ARGUMENTS:
**:          IN:
**:             xBeam          - x Beam axis
**:             yBeam          - y Beam axis
**:
**:>------------------------------------------------------------------*/
void FtfBaseTrack::updateToClosestApproach ( double xBeam, double yBeam ) {
   double rc, xc, yc ;
   Ftf3DHit closest = getClosest ( xBeam, yBeam, rc, xc, yc ) ;
//
   double tPhi = atan2(closest.y-yc,closest.x-xc);
                    
// if ( tPhi < 0 ) tPhi += 2. * M_PI ;

   double tPsi = tPhi - double(q) * 0.5 * M_PI / fabs((double)q) ;
   if ( tPsi > 2. * M_PI ) tPsi -= 2. * M_PI ;
   if ( tPsi < 0.        ) tPsi += 2. * M_PI ;
//
//   Update track parameters
//
   r0   = sqrt(closest.x*closest.x+closest.y*closest.y) ;
   phi0 = atan2(closest.y,closest.x) ;
   if ( phi0 < 0 ) phi0 += 2. * M_PI ;
   z0   = closest.z ;
   psi  = tPsi ;
}

