/***************************************************************************
 *
 * $Id: StTpcDedxPid.cxx,v 1.1 1999/04/28 22:27:36 fisyak Exp $
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
 * $Log: StTpcDedxPid.cxx,v $
 * Revision 1.1  1999/04/28 22:27:36  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/04/28 22:27:36  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.1  1999/04/08 14:56:29  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTpcDedxPid.h"
Double_t StTpcDedxPid::mTpcDedxOffset = -3.75785 ;
Double_t StTpcDedxPid::mTpcDedxRise = 29.706 ;
Double_t StTpcDedxPid::mTpcDedxTcut = 1.50536 ;
StTpcDedxPid::StTpcDedxPid(const StGlobalTrack* t) : StDedxPid(t) { /* noop */ }
ClassImp(StTpcDedxPid)

StTpcDedxPid::StTpcDedxPid(StGlobalTrack* t) : StDedxPid(t) { /* noop */ }

StTpcDedxPid::~StTpcDedxPid() { /* noop */ }

Int_t StTpcDedxPid::detectorInfoAvailable() const
{
    return mTrack->tpcDedx() ? 1 : 0;
}

    
    const StDedx* dedx = mTrack->tpcDedx() ;
    
{
	usededx = 0;
    if (dedx == 0) {
      //  cout << "no dedx pointer" << endl;
      usededx = 0;
    }
    else {
	if (dedx->numberOfPointsUsed() < 5) {
	    usededx = 0;
	}
    }
    return usededx;
}
  
Double_t StTpcDedxPid::numberOfSigma(Double_t mass) const
{
    // returns the number of sigma a tracks dedx is away from
    // the expected mean for a track for a particle of this mass
    
    Double_t deviation ;
    
    // check on quality of track/dedx
    
    //  if (meetsStandardPidQuality() != 0) { 
    //  deviation = -999. ;
    //  return deviation ;
    // }
    
    Double_t dedx_expected = meanPidFunction(mass) ;
    Double_t dedx_resolution =  sigmaPidFunction(mass) ;
    
    const StDedx* dedx = mTrack->tpcDedx() ; 
    
    deviation = (dedx->mean() - dedx_expected)/dedx_resolution ;
    
    return deviation;
}

Double_t StTpcDedxPid::meanPidFunction(Double_t mass) const
{     
    const Double_t  bField = 0.5*tesla;
  gufld(x,b);
    Double_t momentum  = abs(((StGlobalTrack&)mTrack).helix().momentum(bField));
    
    // cast away const'ness (we know what we are doing here)
    Double_t momentum  = abs(mTrack->helix().momentum(bField));

    Double_t bpar[3] = {0.1221537e-06,-4.608514, 5613.} ;
    
    // double bpar[3] = {0.1221537e-06,-4.608514, 5613.} ;
    // double bpar[3] = {0.174325e-06,-2.71889, 776.626} ;
    Double_t rise = bpar[2]*pow(beta*gamma,2);
    Double_t dedxmean;
    //  "gamma " << gamma<< endl;
	dedxmean = bpar[0]/pow(beta,2)*(0.5*log(rise)-pow(beta,2)-bpar[1]);
    if ( beta > 0) 
	dedxmean = mTpcDedxGain/pow(beta,2)*
	  (0.5*log(rise)-mTpcDedxTcut*pow(beta,2)- mTpcDedxOffset);
    else 
	dedxmean = 1000. ;
    return dedxmean ;
}

Double_t StTpcDedxPid::sigmaPidFunction(Double_t mass) const
{
    
    // calcuates average resolution of tpc dedx
    
    Double_t resolution ;
    Double_t dedx_expected = meanPidFunction(mass);
    //
    // resolution depends on the number of points used in truncated mean
    // 
    const StDedx* dedx = mTrack->tpcDedx() ;
    
    
    Double_t nhit = dedx->numberOfPointsUsed() ;
    
    if (nhit > 0) 
	resolution = 0.4 * dedx_expected /sqrt(nhit) ;
    else 
	resolution = 1000.0 ;
    return resolution;
}
Int_t StTpcDedxPid::quickPid(Float_t rig, Float_t dedx) {return  quickid(&rig,&dedx);}








