/***************************************************************************
 *
 * $Id: StTpcDedxPid.cc,v 1.3 1999/05/20 21:42:55 ogilvie Exp $
 *
 * Author: Craig Ogilvie, April 1999
 ***************************************************************************
 *
 * Description:
 *           Implements StDedxPid class and 
 *           calculates number of sigma a track's dedx is from mean
 *           arguments for functions are mass in GeV for a given particle
 *
 ***************************************************************************
 *
 * $Log: StTpcDedxPid.cc,v $
 * Revision 1.3  1999/05/20 21:42:55  ogilvie
 * *** empty log message ***
 *
 * Revision 1.2  1999/05/20 16:17:37  ogilvie
 * added static dedx calibration data members, set, get functions
 *
 * Revision 1.1  1999/04/08 14:56:29  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTpcDedxPid.hh"
#include "StGlobalTrack.hh"
#include "StDedx.hh"
#include "SystemOfUnits.h"

double StTpcDedxPid::mTpcDedxGain = 0.174325e-06;
double StTpcDedxPid::mTpcDedxOffset = -2.71889 ;
double StTpcDedxPid::mTpcDedxRise = 776.626 ;


StTpcDedxPid::StTpcDedxPid(const StGlobalTrack& t) : StDedxPid(t) { 
 }

StTpcDedxPid::~StTpcDedxPid() { /* noop */ }

int StTpcDedxPid::detectorInfoAvailable() const
{
    return mTrack.tpcDedx() ? 1 : 0;
}

int StTpcDedxPid::meetsStandardPid() const
{
    int usededx =1;
    
    const StDedx* dedx = mTrack.tpcDedx() ;
    
    if (dedx == 0) {
	usededx = 0;
    }
    else {
	if (dedx->numberOfPointsUsed() < 5) {
	    usededx = 0;
	}
    }
    return usededx;
}
  
double StTpcDedxPid::numberOfSigma(double mass) const
{
    // returns the number of sigma a tracks dedx is away from
    // the expected mean for a track for a particle of this mass
    
    double deviation ;
    
    // check on quality of track/dedx
    
    //  if (meetsStandardPidQuality() != 0) { 
    //  deviation = -999. ;
    //  return deviation ;
    // }
    
    double dedx_expected = meanPidFunction(mass) ;
    double dedx_resolution =  sigmaPidFunction(mass) ;
    
    const StDedx* dedx = mTrack.tpcDedx() ; 
    
    deviation = (dedx->mean() - dedx_expected)/dedx_resolution ;
    
    return deviation;
}

double StTpcDedxPid::meanPidFunction(double mass) const
{     
    // can't we get bfield from somewhere else, and why does p
    // in StEvent need it anyway?
    //
    const double  bField = 0.5*tesla;
    // cast away const'ness (we know what we are doing here)
    double momentum  = abs(((StGlobalTrack&)mTrack).helix().momentum(bField));
    
    // placeholder constants for charcaterizeing bethe-bloch curve
    // use some data-base solution
    
    // double bpar[3] = {0.1221537e-06,-4.608514, 5613.} ;
    // double bpar[3] = {0.174325e-06,-2.71889, 776.626} ;
    double gamma =sqrt(pow(momentum/mass,2)+1.);
    double beta = sqrt(1. - 1./pow(gamma,2));
    double rise = mTpcDedxRise*pow(beta*gamma,2);
    double dedxmean;
    if ( beta > 0) 
	dedxmean = mTpcDedxGain/pow(beta,2)*
	  (0.5*log(rise)-pow(beta,2)- mTpcDedxOffset);
    else 
	dedxmean = 1000. ;
    return dedxmean ;
}

double StTpcDedxPid::sigmaPidFunction(double mass) const
{
    
    // calcuates average resolution of tpc dedx
    
    double resolution ;
    double dedx_expected = meanPidFunction(mass);
    //
    // resolution depends on the number of points used in truncated mean
    // 
    const StDedx* dedx = mTrack.tpcDedx() ;
    
    
    double nhit = dedx->numberOfPointsUsed() ;
    
    if (nhit > 0) 
	resolution = 0.4 * dedx_expected /sqrt(nhit) ;
    else 
	resolution = 1000.0 ;
    
    return resolution;
}

void StTpcDedxPid::setTpcDedxGain(double gain) {mTpcDedxGain = gain ;} 
void StTpcDedxPid::setTpcDedxOffset(double offset) {mTpcDedxOffset=offset;} 
void StTpcDedxPid::setTpcDedxRise(double rise) {mTpcDedxRise = rise ;} 

double StTpcDedxPid::getTpcDedxGain() { return mTpcDedxGain ;} 
double StTpcDedxPid::getTpcDedxOffset() { return mTpcDedxOffset ;} 
double StTpcDedxPid::getTpcDedxRise() { return mTpcDedxRise;} 








