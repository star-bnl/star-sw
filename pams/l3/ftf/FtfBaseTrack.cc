//:>-----------------------------------------------------------------/
//: FILE:       FtfBaseTrack.cpp
//: HISTORY:
//:             28oct1996 version 1.00
//:             11aug1999 ppy primary flag in FtfPara replace with vertexConstrainedFit
//:             11aug1999 ppy primary flag in track filled now 
//:              3sep1999 ppy fitLine, dpsi cannot be greater than 1. Check introduced
//:              5oct1999 ppy fitLine, bug corrected
//:              6oct1999 ppy Root switch added
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfBaseTrack
//: DESCRIPTION: Basic Description of a track P
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "FtfBaseTrack.h"

#ifdef SL3ROOT
ClassImp(FtfBaseTrack)
#endif


void ftfMatrixDiagonal ( double *h, float &h11, float &h22, float &h33 ) ;

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
    
	currentHit->wxy = 1.0F/ (float)(currentHit->dx*currentHit->dx +
		                             currentHit->dy*currentHit->dy) ;
	wsum      += currentHit->wxy ;
	xav       += currentHit->wxy * currentHit->x ;
	yav       += currentHit->wxy * currentHit->y ;
  }
  if ( para->vertexConstrainedFit ) {
	 wsum += para->xyWeightVertex ;
	 xav  += para->xVertex ;
	 yav  += para->yVertex ;
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
	xi        = currentHit->x - xav ;
	yi        = currentHit->y - yav ;
	xxav     += xi * xi * currentHit->wxy ;
	xyav     += xi * yi * currentHit->wxy ;
	yyav     += yi * yi * currentHit->wxy ;
  }
  if ( para->vertexConstrainedFit ) {
	xi        = para->xVertex - xav ;
	yi        = para->yVertex - yav ;
	xxav     += xi * xi * para->xyWeightVertex ;
	xyav     += xi * yi * para->xyWeightVertex ;
	yyav     += yi * yi * para->xyWeightVertex ; 
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
	xold = currentHit->x - xav ;
	yold = currentHit->y - yav ;
//
//-->  ROTATE SO THAT <XY> = 0 & DIVIDE BY RSCALE SO THAT <R**2> = 1
//
	xi = (  cosrot * xold + sinrot * yold ) / rscale ;
	yi = ( -sinrot * xold + cosrot * yold ) / rscale ;
          
	xixi   = xi * xi ;
	yiyi   = yi * yi ;
	riri   = xixi + yiyi ;
	wiriri = currentHit->wxy * riri ;

	xyav   += currentHit->wxy * xi * yi ;
	xxav   += currentHit->wxy * xixi ;
	yyav   += currentHit->wxy * yiyi ;

	xrrav  += wiriri * xi ;
	yrrav  += wiriri * yi ;
	rrrrav += wiriri * riri ;
  }
//
//   Include vertex if required
//
  if ( para->vertexConstrainedFit ) {
	xold = para->xVertex - xav ;
	yold = para->yVertex - yav ;
//
//-->  ROTATE SO THAT <XY> = 0 & DIVIDE BY RSCALE SO THAT <R**2> = 1
//
	xi = (  cosrot * xold + sinrot * yold ) / rscale ;
	yi = ( -sinrot * xold + cosrot * yold ) / rscale ;
          
	xixi   = xi * xi ;
	yiyi   = yi * yi ;
	riri   = xixi + yiyi ;
	wiriri = para->xyWeightVertex * riri ;

	xyav   += para->xyWeightVertex * xi * yi ;
	xxav   += para->xyWeightVertex * xixi ;
	yyav   += para->xyWeightVertex * yiyi ;

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

         chi2[0]  = (float)(chiscl * lamda) ;
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
  float acent  = (float)(xav - alphar) ;
  float bcent  = (float)(yav - betar ) ;
  float radius = (float)dbro ;
//
//   Get charge
//
  q = ( ( yrrav < 0 ) ? 1 : -1 ) ;
//
//    Get other track parameters
//
  float x0, y0 ;
  if ( para->vertexConstrainedFit ) {
     flag = 1 ; // primary track flag
     x0   = para->xVertex ;
     y0   = para->yVertex ;
     phi0 = para->phiVertex ;
     r0   = para->rVertex ;
  } 
  else {
     flag =  0 ; // primary track flag
     x0   =  lastHit->x  ;
     y0   =  lastHit->y  ;
     phi0 =  atan2(lastHit->y,lastHit->x);
     r0   =  sqrt ( lastHit->x * lastHit->x + lastHit->y * lastHit->y )  ;
  }
  //
  psi  = (float)atan2(bcent-y0,acent-x0) ;
  psi  = psi + q * 0.5F * pi ;
  if ( psi < 0 ) psi = psi + twoPi ;
  pt   = (float)(2.9979e-3 * para->bField * radius ) ;
//
//    Get errors from fast fit
//
   if ( para->getErrors ) getErrorsCircleFit ( acent, bcent, radius ) ;
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
   double radius = (float)(pt / ( 2.9979e-3 * para->bField ) ) ;
   if ( para->vertexConstrainedFit ) {
      dx   = firstHit->x - para->xVertex ;
      dy   = firstHit->y - para->yVertex ;
   }
   else {
      dx   = firstHit->x - lastHit->x ;
      dy   = firstHit->y - lastHit->y ;
   }
   double dpsi = 0.5F * sqrt ( dx*dx + dy*dy ) / radius ;
   double total_s ;
   if ( fabs(dpsi) < 1. ) {
      total_s = 2.0F * radius * asin ( dpsi ) ;
   } 
   else { 
      total_s = 2.0F * radius * M_PI ;
   } 

//
   FtfBaseHit *previousHit ;
	
   for ( startLoop() ; done() ; nextHit() ) {
        
      if ( currentHit != firstHit ) {
         dx   = currentHit->x - previousHit->x ;
	 dy   = currentHit->y - previousHit->y ;
	 dpsi = 0.5F * (float)sqrt ( dx*dx + dy*dy ) / radius ;
	 currentHit->s = previousHit->s - 2.0F * radius * (float)asin ( dpsi ) ;
      }
      else
         currentHit->s = total_s ;
        
      sum += currentHit->wz ;
      ss  += currentHit->wz * currentHit->s ;
      sz  += currentHit->wz * currentHit->z ;
      sss += currentHit->wz * currentHit->s * currentHit->s ;
      ssz += currentHit->wz * currentHit->s * currentHit->z ;
      previousHit = currentHit ;
   }

   double det = sum * sss - ss * ss;
   if ( fabs(det) < 1e-20){ 
      chi2[1] = 99999.F ;
      return 0 ;
   }
//
//     compute the best fitted parameters A,B
//
   tanl = (float)((sum * ssz - ss * sz ) / det );
   z0   = (float)((sz * sss - ssz * ss ) / det );
//
//     calculate chi-square 
//
   chi2[1] = 0.F ;
   double r1 ;
   for ( startLoop() ; done() ; nextHit() ) {
      r1   = currentHit->z - tanl * currentHit->s - z0 ;
      chi2[1] += (float) ( (double)currentHit->wz * (r1 * r1) );
   }
//
//     calculate estimated variance
//      varsq=chi/(float(n)-2.) 
//     calculate covariance matrix 
//      siga=sqrt(varsq*sxx/det) 
//      sigb=sqrt(varsq*sum/det) 
//
    dtanl = (float) ( sum / det );
    dz0   = (float) ( sss / det );
   
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
int FtfBaseTrack::getErrorsCircleFit ( float a, float b, float r ) {
    
   double h[9] = { 0. };
   float dx, dy ;
   float h11, h22, h33 ;
   static int i, j ;
   static float ratio, c1, s1;
   static float hyp;

   
   for (j = 0; j < 9; j++ ) {
      h[j] = 0.;
   }
//
//    If circle fit was not used the
//    errors in the real space need to
//    be calculated
//
   if ( pt < para->ptMinHelixFit ) {
      for ( startLoop() ; done() ; nextHit() ) { 
         currentHit->wxy = 1.0F/ (float)(currentHit->dx*currentHit->dx +
                                         currentHit->dy*currentHit->dy) ;
      }
   }
//
//    Loop over points in fit
//
   for ( startLoop() ; done() ; nextHit() ) {
      dx = currentHit->x - a;
      dy = currentHit->y - b;
      hyp = (float)sqrt( dx * dx + dy * dy );
      s1 = dx / hyp;
      c1 = dy / hyp;
      ratio = r / hyp;
      h[0] += currentHit->wxy * (ratio * (s1 * s1 - 1) + 1);
      h[1] += currentHit->wxy * ratio * s1 * c1;
      h[2] += currentHit->wxy * s1;
      h[4] += currentHit->wxy * (ratio * (c1 * c1 - 1) + 1);
      h[5] += currentHit->wxy * c1;
      h[8] += currentHit->wxy ;
   }
   h[3]  = h[1];
   h[6]  = h[2];
   h[7]  = h[5];

   ftfMatrixDiagonal  ( h, h11, h22, h33 ) ;
//
//   Calculate pt error now
//
   dpt          = (float)(2.9979e-3 * para->bField * h33 );
//
//     Get error in psi now
//
   if ( para->vertexConstrainedFit ) {
      dx = a ;
      dy = b ;
   }
   else {
      dx = lastHit->x + a - firstHit->x ;
      dy = lastHit->y + b + firstHit->y ;
   }
   double w   = dy / dx ;
   dpsi  = (float)(( 1. / ( 1. + w*w ) ) * ( h22 / dx - dy * h11 / ( dx*dx ) )) ;

   return 0 ;
}

//*************************************************************************
//   Prints one track information
//*************************************************************************
void FtfBaseTrack::Print ( int level )
{
   float pmom, pz;   
/*
----->   Print info
*/
   if ( level > 9 ) {
      pz   = pt * tanl ;
      pmom = (float)sqrt ( pz * pz + pt * pt  ) ;
      printf ( " \n =======> Track      %d  <======== ", id ) ;
      printf ( " \n p,  pt, q         %7.2f  %7.2f  %2d ", pmom, pt, q ) ;
   }   
   if ( level > 19 ) {
      printf ( " \n r0, z0            %7.2f  %7.2f ", r0, z0 ) ;
      printf ( " \n phi0, psi, tanl   %7.2f  %7.2f %7.2f ", phi0, psi, tanl ) ;
   }  
   else printf ( "\n " ) ; 

   if ( level > 29 ) {
      printf ( " \n chi2 (s,z)        %6.2e  %6.2e ", chi2[0],
                                                      chi2[1] ) ;
   }
   else printf ( "\n " ) ;

   
   if ( fmod(level,10) > 0 ) {
      printf ( " \n *** Clusters in this track *** " ) ;
      firstHit->print ( 10 ) ;
      for ( startLoop() ; done() ; nextHit()  ) { 
        currentHit->print ( 1 ) ;
      }
   }
   printf ( "\n " ) ; 
}
