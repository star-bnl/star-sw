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
//:          28mar2000 ppy closestApproach split in two methods
//:          19jul2000 ppy fitHelix, for secondary tracks r0,phi0,z0
//:                        is given at a point on the helix close to
//:                        the inner most point. Previously it was given
//:                        exactly at the inner most point. Residuals
//:                        are slightly better with this change
//:          10Aug2001 ppy Adding bFieldPolarity
//:           8Sep2001 ppy when localPsi>=1 in fitLine angle was wrong, corrected
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfBaseTrack
//: DESCRIPTION: Basic Description of a track P
//: AUTHOR:      ppy - Pablo Yepes, yepes@rice.edu
//:>------------------------------------------------------------------
#include "FtfBaseTrack.h"

#include "FtfGeneral.h"
#include <rtsLog.h>
#include <limits.h>


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
	//LOG(ERR, " Problem in Fit_Circle " ) ;
	return 1 ;
    }
    //
    //     Fit line in s-z plane now
    //
    if ( fitLine ( )) {
	//LOG(ERR, " Problem fitting a line " ) ;
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
//-->  NOW CALCULATE THE MATRIX ELEMENTS FOR ALPH, BETA & KAPPA
//
  double h11   = xxav  -     lamda ;
  double h14   = xrrav ;
  double h22   = yyav  -     lamda ; 
  double h24   = yrrav ;
  double h34   = 1.0   + 2.0*lamda ;
  if ( h11 == 0.0 || h22 == 0.0 ){
    //LOG(ERR, " Problems fitting a circle " ) ;
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
  double alph = - (h14/h11) * kappa ;
//
//-->  transform these into the lab coordinate system
//-->  first get kappa and back to real dimensions
//
  double kappa1 = kappa / rscale ;
  double dbro   = 0.5   / kappa1 ;
//
//-->  next rotate alph and beta and scale
//
  double alphr = (cosrot * alph - sinrot * beta)* dbro ;
  double betar  = (sinrot * alph + cosrot * beta)* dbro ;
//
//-->  then translate by (xav,yav)
//
  double acent  = (double)(xav - alphr) ;
  double bcent  = (double)(yav - betar ) ;
  double radius = (double)dbro ;
//
//   Get charge
//

  q = ( ( yrrav < 0 ) ? 1 : -1 ) * getPara()->bFieldPolarity  ;
  pt   = (double)fabs(2.9979e-3 * getPara()->bField * radius ) ;
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
     psi  = (double)atan2(bcent-y0,acent-x0) ;
     psi  = psi + getPara()->bFieldPolarity*q * 0.5F * pi ;
     if ( psi < 0 ) psi = psi + twoPi ;
  } 
  else {
     FtfBaseHit* lHit = (FtfBaseHit *)lastHit ;
     flag =  0 ; // primary track flag
     psi  = (double)atan2(bcent-(lHit->y),acent-(lHit->x)) ;
     x0   = acent - radius * cos(psi);
     y0   = bcent - radius * sin(psi);
     psi  = psi + getPara()->bFieldPolarity*q * 0.5F * pi ;
     phi0 =  atan2(y0,x0);
     if ( phi0 < 0 ) phi0 += twoPi ;
     r0   =  sqrt ( x0 * x0 + y0 * y0 )  ;
  }
  //
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
   double radius = (double)fabs(pt / ( 2.9979e-3 * getPara()->bField ) ) ;
   if ( getPara()->vertexConstrainedFit ) {
      dx   = ((FtfBaseHit *)firstHit)->x - getPara()->xVertex ;
      dy   = ((FtfBaseHit *)firstHit)->y - getPara()->yVertex ;
   }
   else {
      dx   = ((FtfBaseHit *)firstHit)->x - ((FtfBaseHit *)lastHit)->x ;
      dy   = ((FtfBaseHit *)firstHit)->y - ((FtfBaseHit *)lastHit)->y ;
   }
   double localPsi = 0.5F * sqrt ( dx*dx + dy*dy ) / radius ;
   if ( localPsi > 1. ) localPsi = 1. ;
   //double total_s ;
   //total_s  = 2.0F * radius * asin ( localPsi ) ;
   length  = 2.0F * radius * asin ( localPsi ) ;
//
   FtfBaseHit *previousHit = 0  ;
	
   for ( startLoop() ; done() ; nextHit() ) {
      FtfBaseHit* cHit = (FtfBaseHit *)currentHit ;
      if ( currentHit != firstHit ) {
         dx   = cHit->x - previousHit->x ;
	 dy   = cHit->y - previousHit->y ;
	 dpsi = 0.5F * (double)sqrt ( dx*dx + dy*dy ) / radius ;
         if ( dpsi > 1.) {
//	    LOG(ERR,"FtfBaseHit::fitLine(): dpsi=%f\n", dpsi);
            dpsi = 1.;
         }
	 cHit->s = previousHit->s - 2.0F * radius * (double)asin ( dpsi ) ;
      }
      else
         cHit->s = length;
        
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
   dpt          = (double)fabs(2.9979e-3 * getPara()->bField * h33 );
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
      LOG(NOTE, " =======> Track      %d  <========\n", id ) ;
      LOG(NOTE, "p,  pt, q         %7.2f  %7.2f  %2d \n", pmom, pt, q ) ;
   }   
   if ( level > 19 ) {
     LOG(NOTE,  "r0,   z0,  nHits  %7.2f  %7.2f %d \n", r0, z0, nHits ) ;
     LOG(NOTE, "phi0, psi, tanl   %7.2f  %7.2f %7.2f \n", phi0, psi, tanl ) ;
   }  

   if ( level > 29 ) {
     LOG(NOTE, "chi2 (s,z)        %6.2e  %6.2e \n", chi2[0], chi2[1] ) ;
   }

   
   if ( fmod((double)level,10.) > 0 ) {
     LOG(NOTE, "*** Clusters in this track *** " ) ;
      ((FtfBaseHit *)firstHit)->print ( 10 ) ;
      for ( startLoop() ; done() ; nextHit()  ) { 
        ((FtfBaseHit *)currentHit)->print ( 1 ) ;
      }
   }
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
//
   // printf("in getClosest a %4.2f %4.2f %4.2f %4.2f %4.2f\n",xBeam,yBeam,rc,xc, yc);

   double tPhi0 = psi + double(q)*getPara()->bFieldPolarity * 0.5 * M_PI / fabs((double)q) ;

   double x0   = r0 * cos(phi0) ;
   double y0   = r0 * sin(phi0) ;
   rc   = pt / fabs( bFactor * getPara()->bField )  ;
   xc   = x0 - rc * cos(tPhi0) ;
   yc   = y0 - rc * sin(tPhi0) ;

   // printf("r/phi b %4.2f %4.2f %f\n",r0,phi0,getPara()->bField);

   // printf("in getClosest b %4.2f %4.2f %4.2f %4.2f %4.2f\n",xBeam,yBeam,rc,xc, yc);

   getClosest ( xBeam, yBeam, rc, xc, yc, xp, yp ) ;

//-----------------------------------------------------------------
//     Get the z coordinate
//-----------------------------------------------------------------
   double angle  = atan2 ( (yp-yc), (xp-xc) ) ;
   if ( angle < 0. ) angle = angle + 2.0 * M_PI ;

   double dangle = angle - tPhi0 ;

   if ( fabs(dangle) < 1.e-4 ) dangle = 0 ; // Problems with -0.000 values
   dangle = fmod ( dangle, 2.0 * M_PI ) ;
   if ( (float(q)*getPara()->bFieldPolarity * dangle) < 0 )
        dangle = dangle + getPara()->bFieldPolarity*float(q) * 2. * M_PI ;
   
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
   double fac1,sfac, fac2 ;
//--------------------------------------------------------
//     Get track parameters
//--------------------------------------------------------

   
   double tPhi0 = psi + getPara()->bFieldPolarity*double(q) * 0.5 * M_PI / fabs((double)q) ;
   double x0    = r0 * cos(phi0) ;
   double y0    = r0 * sin(phi0) ;
   rc    = fabs(pt / ( bFactor * getPara()->bField ))  ;
   xc    = x0 - rc * cos(tPhi0) ;
   yc    = y0 - rc * sin(tPhi0) ;
//  
//     Check helix and cylinder intersect
// 
   fac1 = xc*xc + yc*yc ;
   sfac = sqrt( fac1 ) ;
//  
//  If they don't intersect return
//  Trick to solve equation of intersection of two circles
//  rotate coordinates to have both circles with centers on x axis
//  pretty simple system of equations, then rotate back
//  
   if ( fabs(sfac-rc) > r || fabs(sfac+rc) < r ) {
//    l3Log ( "particle does not intersect \n" ) ;
      return  1 ;
    }
//  
//     Find intersection
//  
   fac2   = ( r*r + fac1 - rc*rc) / (2.00 * r * sfac ) ;
   phi    = atan2(yc,xc) + getPara()->bFieldPolarity*float(q)*acos(fac2) ;
   td     = atan2(r*sin(phi) - yc,r*cos(phi) - xc) ;
//    Intersection in z
    
   if ( td < 0 ) td = td + 2. * M_PI ;
   double dangle = tPhi0 - td ;
   dangle = fmod ( dangle, 2.0 * M_PI ) ;
   if ( r < r0 ) dangle *= -1 ;
// l3Log ( "dangle %f q %d \n", dangle, q ) ;
   if ( (getPara()->bFieldPolarity*float(q) * dangle) < 0 ) 
      dangle = dangle + getPara()->bFieldPolarity*float(q) * 2. * M_PI ;
 
   double stot = fabs(dangle) * rc ;
// l3Log ( "dangle %f z0 %f stot %f \n", dangle, z0, stot ) ;
   if ( r > r0 ) z = z0 + stot * tanl ;
   else          z = z0 - stot * tanl ;
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

//--------------------------------------------------------------------
// tom: split into two methods:
//      one gets both intersections within one turn of the helix,
//      the other selects the intersection closest to the origin. 
//      (Provided for compatibility with older programs.)
//--------------------------------------------------------------------

int FtfBaseTrack::intersectorZLine ( double a, double b, Ftf3DHit& cross ) {
    Ftf3DHit cross1, cross2;

    if (0 != intersectorZLine(a, b, cross1, cross2))
	return 1;

    double r1sq = cross1.x*cross1.x + cross1.y*cross1.y;
    double r2sq = cross2.x*cross2.x + cross2.y*cross2.y;

    if (r1sq < r2sq) { // cross 1 is closer to the beamline
	cross = cross1;
    } else {
	cross = cross2;
    }

    return 0;
}

int FtfBaseTrack::intersectorZLine ( double a, double b, 
				     Ftf3DHit& cross1, Ftf3DHit& cross2 ) {
    //
    //   Calculate circle center and radius
    //
    double x0    = r0 * cos(phi0) ; // x first point on track
    double y0    = r0 * sin(phi0) ;  // y first point on track
    
    double trackPhi0 = psi + getPara()->bFieldPolarity*q * 0.5 * M_PI / fabs((double)q) ;//
    double rc   = pt  / fabs( bFactor * getPara()->bField )  ;
    double xc   = x0 - rc * cos(trackPhi0) ;
    double yc   = y0 - rc * sin(trackPhi0) ;

    double ycPrime = yc - b ;
    double aa = ( 1. + a * a ) ;
    double bb = -2. * ( xc + a * ycPrime ) ;
    double cc = ( xc * xc + ycPrime * ycPrime - rc * rc ) ;
    
    double racine = bb * bb - 4. * aa * cc ;
    if ( racine < 0 ) {
	cross1.set(0,0,0);
	cross2.set(0,0,0);
	return 1 ;
    }
    double rootRacine = sqrt(racine) ;
    
    double oneOverA = 1./aa;
    //
    //   First solution
    //
    double x1 = 0.5 * oneOverA * ( -1. * bb + rootRacine ) ; 
    double y1 = a * x1 + b ;
    //
    //   Second solution
    //
    double x2 = 0.5 * oneOverA * ( -1. * bb - rootRacine ) ; 
    double y2 = a * x2 + b ;


    //-------------------------------------------------------------------
    //  Get the first z coordinate
    //-------------------------------------------------------------------
    double angle, dangle, stot;

    angle  = atan2 ( (y1-yc), (x1-xc) ) ;
    if ( angle < 0. ) angle = angle + 2.0 * M_PI ;
    
    dangle = angle  - trackPhi0  ;
    dangle = fmod ( dangle, 2.0 * M_PI ) ;
    
    if ( (getPara()->bFieldPolarity*q * dangle) > 0 ) 
       dangle = dangle - getPara()->bFieldPolarity*q * 2. * M_PI  ;
    
    stot   = fabs(dangle) * rc ;
    double z1   = z0 + stot * tanl ;
    
    cross1.set ( x1, y1, z1 ) ;


    //-------------------------------------------------------------------
    //  Get the second z coordinate
    //-------------------------------------------------------------------
    angle  = atan2 ( (y2-yc), (x2-xc) ) ;
    if ( angle < 0. ) angle = angle + 2.0 * M_PI ;
    
    dangle = angle  - trackPhi0  ;
    dangle = fmod ( dangle, 2.0 * M_PI ) ;
    
    if ( (getPara()->bFieldPolarity*q * dangle) > 0 ) 
       dangle = dangle - getPara()->bFieldPolarity*q * 2. * M_PI  ;
    
    stot   = fabs(dangle) * rc ;
    double z2   = z0 + stot * tanl ;
    
    cross2.set ( x2, y2, z2 ) ;
    
    return 0 ;
}



/*:>--------------------------------------------------------------------
**: METHOD:   Calculates intersection of track with plane define by line
**:           x = a and the z 
**:
**: AUTHOR:     ppy - P.P. Yepes,  yepes@rice.edu
**: ARGUMENTS:
**:          IN:
**:             a               - Line parameter 
**:         OUT:
**:             crossPoint      - intersection point
**:
**: RETURNS:    0=ok, <>0 track does not cross the plane
**:>------------------------------------------------------------------*/
int FtfBaseTrack::intersectorYCteLine ( double a, Ftf3DHit& cross ) {
//
//calculate circle center and radius
//
   double x0   = r0*cos(phi0);
   double y0   = r0*sin(phi0);
   
   double trackPhi0= psi + getPara()->bFieldPolarity*q*0.5*M_PI/fabs((float)q);
   double rcoc = pt  / fabs( bFactor * getPara()->bField )  ;
   double xcoc = x0 - (rcoc*cos(trackPhi0));
   double ycoc = y0 - (rcoc*sin(trackPhi0));
//
//   Calculate circle center and radius
//
   double xHit = a ;

   double f1 = (xHit-xcoc)*(xHit-xcoc);
   double r2 = rcoc*rcoc;
   if ( f1 > r2 ) {
      return 1 ;
   }

   double sf2  = sqrt(r2-f1);
   double y1   = ycoc + sf2;
   double y2   = ycoc - sf2;
   double yHit = y1;
   if ( fabs(y2) < fabs(y1) ) yHit=y2;

//Get z coordinate:
   double angle  = atan2 ( (yHit-ycoc), (xHit-xcoc) ) ;
   if ( angle < 0. ) angle = angle + 2.0 * M_PI ;

   double dangle = angle  - trackPhi0  ;
   dangle = fmod ( dangle, 2.0 * M_PI ) ;
   if ( (getPara()->bFieldPolarity*q * dangle) > 0 ) 
      dangle = dangle - getPara()->bFieldPolarity*q * 2. * M_PI  ;

   double stot   = fabs(dangle) * rcoc ;
   double zHit   = z0 + stot * tanl;

   cross.set(xHit,yHit,zHit);
//
   return 0 ;
}
//
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
   rc   = pt / fabs( bFactor * getPara()->bField )  ;
   
   double tPhi0 = psi + getPara()->bFieldPolarity*double(q) * 0.5 * M_PI / fabs((double)q) ;
   xc   = x0 - rc * cos(tPhi0) ;
   yc   = y0 - rc * sin(tPhi0) ;
/*
    Get angle difference 
*/
   angle_1  = atan2 ( (y1-yc), (x1-xc) ) ;
   if ( angle_1 < 0 ) angle_1 = angle_1 + 2. * M_PI ;
   angle_2  = atan2 ( (y2-yc), (x2-xc) ) ;
   if ( angle_2 < 0 ) angle_2 = angle_2 + 2. * M_PI ;
   d_angle  = getPara()->bFieldPolarity*double(q) * ( angle_1 - angle_2 ) ;
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
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//       Fit a circle
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FtfBaseTrack::refitHelix ( int mod, int modEqual, int rowMin, int rowMax ) {
   typedef FtfBaseHit* hitPointer; 

   if ( nHits < 1 || nHits > 500 ) {
     LOG(ERR, "FtfBaseTrack:refitHelix: nHits %d out of range \n", nHits ) ;
      return 1 ;
   }
//
   hitPointer* hitP = new hitPointer[nHits];
   int nHitsOrig = nHits ;

   int counter = 0 ;
   for ( startLoop() ; done() ; nextHit() ) {
      hitP[counter] = (FtfBaseHit *)currentHit ;     
//    hitP[counter]->print(1);
      counter++ ;
   }

   int row ;
   firstHit = 0 ;
   counter = 0 ;
   for ( int i = 0 ; i < nHitsOrig ; i++ ) {
      row = hitP[i]->row ; 
      hitP[i]->nextTrackHit = 0 ;
      if ( row%mod != modEqual ) continue ; 
      if ( row < rowMin || row > rowMax ) continue ;

      if ( firstHit == 0 ) firstHit = hitP[i] ; 
      else
         ((FtfBaseHit *)lastHit)->nextTrackHit = (void *)hitP[i] ;
      lastHit = (void *)hitP[i] ;
      counter++ ;
   }
   nHits = counter ;

   int problem = 0 ;
   if ( nHits > 5 ) fitHelix ( ) ;
   else problem = 1 ;
   //
   //  Put hits back
   //
   firstHit = 0 ;
   for ( int i = 0 ; i < nHitsOrig ; i++ ) {
      row = hitP[i]->row ; 
      if ( firstHit == 0 ) firstHit = hitP[i] ; 
      else
         ((FtfBaseHit *)lastHit)->nextTrackHit = (void *)hitP[i] ;
      lastHit = (void *)hitP[i] ;
   }
   nHits = nHitsOrig ;

   delete []hitP ;

   return problem ;
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
//    l3Log ( "FtfBaseTrack::updateToRadius: track %d does not intersect radius %f\n", 
//              id, radius ) ;
      return ;
   }
//
//   Check extrapolation falls inside volume
//
// if ( fabs(zExtra) > getPara()->zMax ) {
//     l3Log ( "problem extrapolating track \n" ) ;
//     return ;  
// }
   double xExtra = radius * cos(phiExtra) ;
   double yExtra = radius * sin(phiExtra) ;

   double tPhi = atan2(yExtra-yCircleCenter,xExtra-xCircleCenter);
                    
// if ( tPhi < 0 ) tPhi += 2. * M_PI ;

   if ( phiExtra < 0 ) phiExtra += 2 * M_PI ;
   
   double tPsi = tPhi - getPara()->bFieldPolarity * double(q) * 0.5 * M_PI / fabs((double)q) ;
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
**:             rMax           - radius of point of closest approach beyond which no update
**:
**:>------------------------------------------------------------------*/
void FtfBaseTrack::updateToClosestApproach ( double xBeam, double yBeam, double rMax ) {
   double rc, xc, yc ;
   Ftf3DHit closest = getClosest ( xBeam, yBeam, rc, xc, yc ) ;
// l3Log ( "FtfBaseTrack::updateClosestApproach: closest x y z %f %f %f \n", 
//               closest.x, closest.y, closest.z ) ;

   double radius = sqrt(closest.x*closest.x+closest.y*closest.y);
   if ( radius > rMax ) return ;
//
   double tPhi = atan2(closest.y-yc,closest.x-xc);
                    
// if ( tPhi < 0 ) tPhi += 2. * M_PI ;

   
   double tPsi = tPhi - getPara()->bFieldPolarity*double(q) * 0.5 * M_PI / fabs((double)q) ;
   if ( tPsi > 2. * M_PI ) tPsi -= 2. * M_PI ;
   if ( tPsi < 0.        ) tPsi += 2. * M_PI ;
//
//   Update track parameters
//
   r0   = radius ;
   phi0 = atan2(closest.y,closest.x) ;
   if ( phi0 < 0 ) phi0 += 2. * M_PI ;
   z0   = closest.z ;
   psi  = tPsi ;
}


/*:>--------------------------------------------------------------------
**: METHOD:   Extrapolation of a track to the given pathlenght 
**:
**:
**:
**: AUTHOR:     JB, Jens Berger,  jberger@bnl.gov
**: ARGUMENTS:
**:          IN:
**:             doubel pathlength: pathlength of the track, staring at x0,y0,z0
**:         OUT:
**:             one of pablos fancy Ftf3DHit
**:
**:>------------------------------------------------------------------*/
Ftf3DHit FtfBaseTrack::extrapolate2PathLength(double pathlength)
{
 // some helix para

 // BFilef scaled with 0.01
 double Bfield=fabs(getPara()->bField) * getPara()->bFieldPolarity * 0.01;
 double c=0.3;
 double lambda=atan(tanl);
 double kapa=(c*q*Bfield)/pt;
 double heli=-((q*Bfield)/fabs(q*Bfield));
 double phi=psi-heli*M_PI/2;
  // staring point
 double x0=r0*cos(phi0);
 double y0=r0*sin(phi0);

 Ftf3DHit CoordOfExtrapol;
 CoordOfExtrapol.x = x0 + (1/kapa)*(cos(phi+heli*pathlength*kapa*cos(lambda))-cos(phi));
 CoordOfExtrapol.y = y0 + (1/kapa)*(sin(phi+heli*pathlength*kapa*cos(lambda))-sin(phi));
 CoordOfExtrapol.z = z0 + pathlength*sin(lambda);

 // debug
 //cout<<"x0:"<<x0<<" y0:"<<y0<<" z0:"<<" lambda:"<<lambda<<z0<<" phi:"<<phi<<" h:"<<heli<<" kapa:"<<kapa<<endl;         
 //cout<<" xs:"<<CoordOfExtrapol.x<<" ys:"<<CoordOfExtrapol.y<<" zs:"<<CoordOfExtrapol.z<<endl;


 // return coord of extrapolation
 return CoordOfExtrapol;
}

/*:>--------------------------------------------------------------------
**: METHOD:   Get the Radius of the track, helix at X,Y center 
**:
**:
**:
**: AUTHOR:     JB, Jens Berger,  jberger@bnl.gov
**: ARGUMENTS:
**:          IN:
**:             nada
**:         OUT:
**:             radius
**:
**:>------------------------------------------------------------------*/
double FtfBaseTrack::getRadius()
{
 double radius=pt / fabs( bFactor * getPara()->bField );
 
 return radius;
}

/*:>--------------------------------------------------------------------
**: METHOD:   Get y center of the corresponding radius of the track, helix
**:
**:
**:
**: AUTHOR:     JB, Jens Berger,  jberger@bnl.gov
**: ARGUMENTS:
**:          IN:
**:             nada
**:         OUT:
**:             x center
**:
**:>------------------------------------------------------------------*/
double FtfBaseTrack::getXCenter() {
   
   double tPhi0 = psi + getPara()->bFieldPolarity*double(q) * 0.5 * M_PI / fabs((double)q) ;
   // staring point
   double x0=r0*cos(phi0);

   return ( x0- getRadius() * cos(tPhi0) );
 }
/*:>--------------------------------------------------------------------
**: METHOD:   Get y center of the corresponding radius of the track, helix
**:
**:
**:
**: AUTHOR:     JB, Jens Berger,  jberger@bnl.gov
**: ARGUMENTS:
**:          IN:
**:             nada
**:         OUT:
**:             y center
**:
**:>------------------------------------------------------------------*/
double FtfBaseTrack::getYCenter()
{
   
   double tPhi0 = psi + getPara()->bFieldPolarity*double(q) * 0.5 * M_PI / fabs((double)q) ;
   // staring point
   double y0=r0*sin(phi0);

   return ( y0 - getRadius() * sin(tPhi0) );
}
/*:>---------------------------------------------------------------------------------------
**: METHOD:   Extrapolate to a plane
**:
**: Vector 'R' defines the position of the center and
**: vector 'N' the normal vector of the plane.
**: For a straight line there is a simple analytical
**: solution. For curvatures > 0 the root is determined
**: by Newton method. In case no valid s can be found
**: the max. largest value for s is returned.
**:
**: THIS FUNCTION CAN ONLY BE USED WITH MAGNETIC FIELD > 0 !!!
**:
**:
**: AUTHOR: JB, Jens Berger, jberger@bnl.gov, stolen from Thomas Ullrich's StarClassLibrary
**:
**:
**: ARGUMENTS:
**:          IN:
**:             R(x,y,z) position of the plane, N(x,y,z) normal vector to plane
**:         OUT:
**:             plathlenght to plane
**:
**:>---------------------------------------------------------------------------------------*/
double FtfBaseTrack::pathLength(double Rx, double Ry, double Rz, double Nx, double Ny, double Nz )
{
 // THIS FUNCTION CAN ONLY BE USED WITH MAGNETIC FIELD > 0 !!!

 // BFilef scaled with 0.01
 double Bfield=fabs(getPara()->bField) * getPara()->bFieldPolarity * 0.01;

 // helicity
 int heli= (int) -((q*Bfield)/fabs(q*Bfield));
 int H = (heli>=0) ? 1 : -1;

  // origin
 double x0=r0*cos(phi0);
 double y0=r0*sin(phi0);
 // z0 = z0

 // dip angle == lambda
 double DipAngle    = atan(tanl); // lambda
 double CosDipAngle = cos(DipAngle);
 double SinDipAngle = sin(DipAngle);

 // phase == phi 
 double Phase       = psi-heli*M_PI/2; // phi
 double CosPhase    = cos(Phase);
 double SinPhase    = sin(Phase);
 if (fabs(Phase) > M_PI) Phase = atan2(SinPhase, CosPhase);  // force range [-pi,pi]

 // curvature == kapa
 double Curvature = (0.3*q*Bfield)/pt; // kapa


 // stolen part :->
 const double NoSolution = LONG_MAX ;
 const double MaxPrecisionNeeded = 0.0001;  // micrometer in STAR coord.
 const int    MaxIterations      = 20;

 double A = Curvature*( (x0*Nx+y0*Ny+z0*Nz) - (Rx*Nx+Ry*Ny+Rz*Nz) ) -
	           Nx*CosPhase - 
	           Ny*SinPhase;
 double t = H*Curvature*CosDipAngle;
	
 double a, f, fp, s;
 double sOld = s = 0;  
 int i;
 for (i=0; i<MaxIterations; i++)
 {
  a  = t*s+Phase;
  f  = A +
       Nx*cos(a) +
       Ny*sin(a) +
       Nz*Curvature*SinDipAngle*s;

  fp = -Nx*sin(a)*t +
		Ny*cos(a)*t +
		Nz*Curvature*SinDipAngle;
  s -= f/fp;
	    if (fabs(sOld-s) < MaxPrecisionNeeded) break;
	    sOld = s;
 }

 if (i == MaxIterations) s = NoSolution;

 return s;
}
