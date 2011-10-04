// $Id: StFgtNaiveDbMaker.cxx,v 1.3 2011/10/04 03:06:29 balewski Exp $
/* \class StFgtNaiveDbMaker        
\author Stephen Gliske

*/

#include "StFgtNaiveDbMaker.h"


// tmp, methods below whould  be moved to fgt-db-maker

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
double
StFgtNaiveDbMaker::gridAttenuation(float xLoc, float yLoc){ // range [0,1]
  double att=1.0 ; // default is no attenuation

  const int nR=4;
  double rA[nR]={11.5, 20.63,29.15, 38.1};// (cm) - location of inneficiency rings

  double const par_delR=1.; // cm 
  
  // R-grid
  double r=sqrt(xLoc*xLoc + yLoc*yLoc);
  for(int i=0; i<nR; i++){
    double eps=(r-rA[i])/par_delR;
    if(fabs(eps)>3) continue; // skip if too far
    att*= 1. - exp(-eps*eps/sqrt(2));
    //printf(" Ri=%d  eps=%f\n", i,eps);
  }
  //printf("r=%f att=%f  \n", r,att);

  //Phi edges: X, Y-axis
  for(int i=0;i<2;i++) {
    double eps=0;
    if (i==0) eps=(yLoc- 1.2)/par_delR; // dist to edge of quadrant
    if (i==1) eps=(xLoc- 1.2)/par_delR;
    if(fabs(eps)>3.) continue; // skip if too far
    att*= 1. - exp(-eps*eps/sqrt(2));
    //printf(" XYi=%d  eps=%f\n", i,eps);
  }

  
  //Phi edges: 30, 60 deg
  for(int i=0;i<2;i++) {
    double del=0;
    if(i==0) del=-xLoc*0.5 + yLoc*0.866;  //- x*sin(30) + y*cos(30)
    if(i==1) del= yLoc*0.5 - xLoc*0.866;  //- x*sin(60) + y*cos(60)
    double eps=del/par_delR;
    if(fabs(eps)>3.) continue; // skip if too far
    att*= 1. - exp(-eps*eps/sqrt(2));
    //printf(" Gphi=%ddeg  eps=%f\n", 30*(1+i),eps);
  }

  return att;
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
double
StFgtNaiveDbMaker::PchargeFraction(float xLoc, float yLoc){ // range [0,1]
  return 0.45;
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
double
StFgtNaiveDbMaker::PstripGain(int iStrip, int iQuad, int iDisc){
  return 3.;
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------
double
StFgtNaiveDbMaker::RstripGain(int iStrip, int iQuad, int iDisc){
  return 3.;
}



// $Log: StFgtNaiveDbMaker.cxx,v $
// Revision 1.3  2011/10/04 03:06:29  balewski
// cleanup
//
// Revision 1.2  2011/10/04 02:59:34  balewski
// added guestimates of gains, grid absorption, charge sharing
//
