// $Id: StFtpcSlowSimReadout.cc,v 1.22 2018/12/07 18:00:17 genevb Exp $
// $Log: StFtpcSlowSimReadout.cc,v $
// Revision 1.22  2018/12/07 18:00:17  genevb
// Use TRandom and allow control of the seed
//
// Revision 1.21  2011/07/26 09:40:52  jcs
// Change LOG_DEBUG statement to print out 2 ftpcAmpSlope values to be able to
// check which ftpcAmpSlope table is being used
//
// Revision 1.20  2009/11/14 13:18:33  jcs
// change LOG_INFO messages to LOG_DEBUG messages
//
// Revision 1.19  2007/01/15 15:02:20  jcs
// replace printf, cout and gMesMgr with Logger
//
// Revision 1.18  2005/10/11 12:41:00  jcs
// If isec=6, set isec=0 to avoid segmentation violations
//
// Revision 1.17  2005/10/11 11:27:37  jcs
// remove +twopi in call to WhichPad when calculating pad_min,twopi is added in WhichPad
//
// Revision 1.16  2003/09/02 17:58:16  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.15  2003/01/29 12:12:37  fsimon
// Additional comments to illustrate ASIC mapping
//
// Revision 1.14  2003/01/14 12:58:25  jcs
// use Geometry_ftpc/ftpcAsicMap to control corrections for error in Y2001-2002
// FTPC asic mapping
//
// Revision 1.13  2002/10/17 13:42:11  fsimon
// Charge scaling taken out (assumes high amplification)
// use scaling to match 2001/2002 data
//
// Revision 1.12  2002/10/16 12:31:53  fsimon
// gain factors and time offset included, Hardware <-> DAQ mapping taken into
// account for Db access
//
// Revision 1.11  2002/09/13 13:44:02  fsimon
// Added Random noise to each Timebin -> Better description of real data
//
// Revision 1.10  2002/09/13 13:41:11  fsimon
// Comment out anglefactor
//
// Revision 1.9  2002/06/07 10:32:55  fsimon
// Correct treatment of clusters on sector boundaries
// Correct assignment of pad numbers in WhichPad
//
// Revision 1.8  2002/04/19 22:24:13  perev
// fixes for ROOT/3.02.07
//
// Revision 1.7  2001/04/24 07:17:12  oldi
// Renaming of some variables (slice->sslice, gnch->ggnch, glow->gglow,
// ghigh->gghigh,  gdelta->ggdelta) to avoid compiler warnings (and bad coding
// style).
//
// Revision 1.6  2001/04/20 12:52:09  jcs
// change if/else statements for calculating polar coordinates to avoid
// problem with optimizer
// cleanup comments
//
// Revision 1.5  2001/04/02 12:04:37  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters from StarDb/ftpc
//
// Revision 1.4  2001/03/19 15:53:10  jcs
// use ftpcDimensions from database
//
// Revision 1.3  2001/03/06 23:36:16  jcs
// use database instead of params
//
// Revision 1.2  2001/01/11 18:28:53  jcs
// use PhysicalConstants.h instead of math.h, remove print statement
//
// Revision 1.1  2000/11/23 10:16:43  hummler
// New FTPC slow simulator in pure maker form
//
//
///////////////////////////////////////////////////////////////////////////
//  Author: W.G.Gong
//  Email: gong@mppmu.mpg.de
//  Date:  Oct 25, 1996
//
//  Modifications:
//         02/27/98    Janet Seyboth   remove loop variable definitions, now
//                                     in readout.h
//         02/18/98    Janet Seyboth   Remove all references to point file
///////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <Stiostream.h>
#include "PhysicalConstants.h"

#include "StMessMgr.h"
#include "StFtpcSlowSimField.hh"
#include "StFtpcSlowSimCluster.hh"
#include "StFtpcSlowSimReadout.hh"
#include "StFtpcClusterMaker/StFtpcParamReader.hh"
#include "StFtpcClusterMaker/StFtpcDbReader.hh"
#include "TF1.h"
#include "TRandom.h"


#ifndef DEBUG
#define DEBUG 0
#endif

StFtpcSlowSimReadout::StFtpcSlowSimReadout(StFtpcParamReader *paramReader,
                                           StFtpcDbReader *dbReader,
					   float *adcIn, 
					   const StFtpcSlowSimField *field)
{
  if (!(gRandom->GetSeed())) gRandom->SetSeed(20181207);
  LOG_INFO << "FtpcSlowSim Seed used = " << gRandom->GetSeed() << endm;
  mParam=paramReader;
  mDb=dbReader;
  mRandomNumberGenerator = mParam->randomNumberGenerator();
  number_plane = mDb->numberOfPadrowsPerSide();
  pad_pitch = mDb->padPitch();
  pad_length = mDb->padLength();
  sigma_prf = mParam->sigmaPadResponseFuntion();   
  shaper_time = mParam->readoutShaperTime();
  slice = mDb->microsecondsPerTimebin();

  mOuterRadius = mDb->sensitiveVolumeOuterRadius();

  mADCArray=adcIn;

  // set parameters for Polya distribution and initialize it
  gnch   = 50;
  glow   = 0.1;
  ghigh  = 2.5; 
  gdelta = (ghigh-glow) / (float) gnch;
  pcum   = new float[gnch];
  polya(gnch, glow, ghigh, gdelta);
  
  int imax=mDb->numberOfPadrows()
    *mDb->numberOfSectors()
    *mDb->numberOfPads()
    *mDb->numberOfTimebins();
  for(int i=0; i<imax; ++i) 
    mADCArray[i] = 0.0;
  
  if (mRandomNumberGenerator == 1) {
    // initialize the random number generator
    rmarin(1802, 9373);
  }
  
  // angle range in which each sector is calculated
  phiMin = mDb->phiOrigin() * degree;
  phiMax = mDb->phiEnd() * degree;    
  mGasGain = mDb->gasGain();
  mMaxAdc = mParam->maxAdc();
  mGaussIntSteps = mParam->gaussIntegrationSteps();
  mInverseFinalVelocity = 1 /  field->GetVelAtReadout(); 
}

StFtpcSlowSimReadout::~StFtpcSlowSimReadout()
{
  delete [] pcum;
}

void  StFtpcSlowSimReadout::Avalanche(const StFtpcSlowSimCluster *cl)
{
  // sample the gas gain according to Polya distribution
  
  int group = 10;                      // group 10 electrons together
  int nel   = (int) ( cl->GetElectron() / group );
  mFinalElectrons = 0;
  for (int i=0; i<nel; ++i) {
    mFinalElectrons += sample_polya(mGasGain);
  }
  mFinalElectrons *= group;
  
  // consider electron deflection due to Lorentz force
  // when electron approaches the strip
  // not implemented yet.
}

void  StFtpcSlowSimReadout::PadResponse(const StFtpcSlowSimCluster *cl)
{
    float prf = sigma_prf;
    float sig_phi = cl->GetSigPhi();
    pad_off = cl->GetPadOff();
      
    if(DEBUG) {
      LOG_DEBUG << "StFtpcSlowSimReadout::PadResponse: pad_off=" << pad_off <<  "sig_phi=" << sig_phi << endm;
    }
    sigma_pad =  ::sqrt(sig_phi*sig_phi + prf*prf ); 
}


void StFtpcSlowSimReadout::ShaperResponse(const StFtpcSlowSimCluster *cl)
{
  float srf = shaper_time*0.42466091; // time->sigma 1 / 2.35482
  float sig_rad = cl->GetSigRad();
  float rad_off = cl->GetRadOff();

  // convert the radial width to nsec
  float sig_time = sig_rad * mInverseFinalVelocity;
  time_off = 10000*rad_off * mInverseFinalVelocity;
  
  if(DEBUG) {
    LOG_DEBUG << "ShaperResponse: time_off=" << time_off <<  "sig_rad=" << sig_rad << endm;
  }
  sigma_tim = ::sqrt( sig_time*sig_time + srf*srf) ;
}

void StFtpcSlowSimReadout::Digitize(const StFtpcSlowSimCluster *cl, const int irow)
{
  float n_sigmas_to_calc  = 5.0;        
  // LOG_DEBUG <<"StFtpcSlowSimReadout::Digitize..." << endm;
  // get the readout position in radial direction
  float time_slice = mDb->microsecondsPerTimebin()*1000;// into nsec
  float time       = cl->GetDriftTime()*1000.;       // into nsec
  int     itim       = WhichSlice(time);
  
  // get the readout position in azimuthal direction
  float delta_phi  = mDb->radiansPerPad();
  float phi        = cl->GetPhi();
  int isec, jsec, nsecs;
  int     ipad       = WhichPad(phi,isec);

  if (DEBUG) {  
    LOG_DEBUG << "Digitize using parameters: mDb->radiansPerBoundary() = "<<mDb->radiansPerBoundary()<<" mDb->radiansPerPad() = "<<mDb->radiansPerPad()<<endm;
    LOG_DEBUG << " phiMin = "<< phiMin << " phiMax = " << phiMax <<endm;
  }

  

  // big if() loop
  if ( itim > 2 && itim < (mDb->numberOfTimebins()-3)) 
    {
      // and calculate the pad distribution
      
      float sigmaPadCentimeters   = sigma_pad *0.0001;  // into cm 
      float width_phi = n_sigmas_to_calc *sigmaPadCentimeters / mOuterRadius;
      //note: mOuterRadius is the radius of the Frisch grid, not the padplane,
      // but for this purpose it is good enough
      
      // store center of cluster
      float mid_phi = phi;
      float mid_time = time;
      float hypo = ::sqrt((pad_off/pad_pitch)*(pad_off/pad_pitch)
			+(time_off/((double)mDb->microsecondsPerTimebin()*1000))*
			(time_off/((double)mDb->microsecondsPerTimebin()*1000)));
      int n_sub_hits = (int) (2*hypo);
      int current_sub_hit;
      
      if(DEBUG) {
	LOG_DEBUG << "hypo=" << hypo << " mid_phi=" << mid_phi << " mid_time=" << mid_time << " phi_off=" << pad_off/mOuterRadius << " time_off=" << time_off << endm;
      }
      
      for(current_sub_hit=-n_sub_hits; current_sub_hit <= n_sub_hits; current_sub_hit++)
	{
	  if(n_sub_hits>0)
	    {
	      time = mid_time + ((time_off/(2*n_sub_hits))*current_sub_hit);
	      phi = mid_phi + ((pad_off/(mOuterRadius*(2*n_sub_hits)))*current_sub_hit);
	      //note: mOuterRadius is the radius of the Frisch grid, not the padplane,
	      // but for this purpose it is good enough
	    }
	  
	  ipad       = WhichPad(phi,isec);
	  if(DEBUG) {
	    LOG_DEBUG << current_sub_hit << "th subhit at time " << time << " phi " << phi << " => padpos " << mOuterRadius*phi <<" ipad = "<<ipad <<endm;
          }
	  int isec_min;
	  int isec_max;
	  int pad_max_save=0;
	  int npad;
	  int pad_min = WhichPad(phi-width_phi,isec_min);
	  int pad_max = WhichPad(phi+width_phi,isec_max);

	  if ( isec_min > isec_max )
	    nsecs = mDb->numberOfSectors() - isec_min + isec_max + 1;
	  else
	    if (isec_min == isec_max && pad_min >= pad_max )
	      nsecs = mDb->numberOfSectors() + 1;
	    else
	      nsecs = isec_max - isec_min + 1;
	  int isec = isec_min;
	  for (jsec=1; jsec<nsecs+1; ++jsec) {
	    if (isec != isec_max || (isec == isec_max && pad_min >= pad_max )) {
	      pad_max_save = pad_max;
	      pad_max = mDb->numberOfPads()-1;
	    }
	    npad    = (pad_max - pad_min + 1);
	    float* pad = new float[npad];  // signal dist. in pads
	    int i;
	    
	    float dphi = fmod(phi-phiMin+twopi,twopi);
	    dphi = dphi - isec*(phiMax-phiMin);
	    
	    for (i=0; i<npad; ++i ) {
	      float phi_low = PhiOfPad(i+pad_min,0) - 0.5*delta_phi; 
	      // low edge of pad
	      float phi_up  = PhiOfPad(i+pad_min,0) + 0.5*delta_phi; 
	      // up  edge of pad
	      pad[i] = InteGauss(mOuterRadius*phi_low, mOuterRadius*phi_up, 
				 mOuterRadius*dphi, sigmaPadCentimeters );
	      // integrate over this pad
	      //note: mOuterRadius is the radius of the Frisch grid, not the padplane,
	      // but for this purpose it is good enough:
	      // here padwidth=padpitch is assumed, too
	    } // end for loop
	    
	    
	    // and calculate the time distribution 
	    // include time offset database: subtract time offset!
	    // move time center of cluster
            if (DEBUG) {
	       LOG_DEBUG <<" Shifting time by " << endm;
	       LOG_DEBUG << mDb->timeOffset(GetHardSec(isec, irow)*mDb->numberOfPads()+GetHardPad(isec,ipad,irow)+1, irow) << " from "<< time << " with parameters " 
	     << GetHardSec(isec, irow)*mDb->numberOfPads()+GetHardPad(isec,ipad,irow)+1<< " , "<< irow << endm;
            }
	    
	    time = time - mDb->timeOffset(GetHardSec(isec, irow)*mDb->numberOfPads()+GetHardPad(isec,ipad,irow)+1, irow)/(0.001/mDb->microsecondsPerTimebin());
	    
            if (DEBUG) {
	       LOG_DEBUG << " to " << time << " for Sec "<< isec<<" ("<<GetHardSec(isec, irow)<<") pad "<< (ipad) <<" ("<<GetHardPad(isec,ipad,irow)<<") row " << irow <<endm;
            }

	    float width_tim = n_sigmas_to_calc*sigma_tim;
	    int tim_min = WhichSlice(time - width_tim);
	    int tim_max = WhichSlice(time + width_tim);
	    int ntim    = (tim_max - tim_min + 1);
	    
	    float* sca = new float[ntim];
	    int j;
	    for (j=0; j<ntim; ++j) {
	      float tim_low = TimeOfSlice(j+tim_min) 
		- 0.5*time_slice; 
	      // low edge of time
	      float tim_up  = TimeOfSlice(j+tim_min) 
		+ 0.5*time_slice; 
	      // up edge of time
	      
	      sca[j] = InteGauss(tim_low, tim_up, time, sigma_tim);
	      // integrate over this slice
	    }  // end for loop
	    
	    // Now fill the mADCArray[irow,isec,pad,tim] array
	    if(DEBUG) {
	      LOG_DEBUG << current_sub_hit << "th subhit from time " << tim_min << " to " << tim_min+ntim << " pad " << pad_min << " to " << pad_min+npad << endm;
            }
	    for (i=0; i<npad; ++i) 
	      for (j=0; j<ntim; ++j) {
		int k = irow*mDb->numberOfSectors()*mDb->numberOfPads()*mDb->numberOfTimebins()+isec*mDb->numberOfPads()*mDb->numberOfTimebins()+(i+pad_min)*mDb->numberOfTimebins() + (j+tim_min) ;
		mADCArray[k] += (float)(mFinalElectrons * pad[i] * sca[j])/(2*n_sub_hits+1);
		if(DEBUG) {
		  LOG_DEBUG << "Writing " <<  mADCArray[k] <<" to pad "<< (i+pad_min)<<endm;
                }

	      }
	    
	    // recycle sca[] and pad[]
	    delete [] sca;
	    delete [] pad;
	    pad_min = 0;
	    pad_max = pad_max_save;
	    ++isec;
	    if ( isec > mDb->numberOfSectors()-1 )
	      isec = 0;
	  }  // end of loop over sectors for multisector cluster
	} // end of loop over subhits
       } // end big if() loop
  
}


void StFtpcSlowSimReadout::OutputADC() 
{
  int num_pixels[11]={0}, num_pixels_occupied[11]={0};
  
  // Gaussian distribution for Noise, Sigma 1.5 ADC channels
  TF1* noise = new TF1("noise","gaus",-5,5);
  noise->SetParameters(1,0,1.5);
  LOG_DEBUG << "FTPC SlowSimulator using random noise with a sigma of 1.5" << endm;
  LOG_DEBUG << "FTPC SlowSimulator using gain tables (mDb->amplitudeSlope(0,1) = "<<mDb->amplitudeSlope(0,1)<<", mDb->amplitudeSlope(1,1) = "<<mDb->amplitudeSlope(1,1)<<"), amplitude offset and adcConversion = " << mParam->adcConversion()<< endm;


  for (int row=0; row<mDb->numberOfPadrows(); row++) { 
    for (int sec=0; sec<mDb->numberOfSectors(); sec++) {
      for (int pad=0; pad<mDb->numberOfPads(); pad++) {
	for (int bin=0; bin<mDb->numberOfTimebins(); bin++) {
	  int i=bin+mDb->numberOfTimebins()*pad+mDb->numberOfTimebins()*mDb->numberOfPads()*sec+mDb->numberOfTimebins()*mDb->numberOfPads()*mDb->numberOfSectors()*row;
	  
	  if (mADCArray[i] != 0){
	    mADCArray[i] =(mADCArray[i] / mParam->adcConversion());
	    //mADCArray[i] =(mADCArray[i] / 1.4); // Scale ADC Values to match real data
	  
	    // include gainfactors 
	    // remember that the Db access counts from 1 to 960 and not from 0 to 959 for the sector&pad index!
	    mADCArray[i] = mADCArray[i] - mDb->amplitudeOffset(GetHardSec(sec, row)*mDb->numberOfPads()+GetHardPad(sec,pad,row)+1, row);
            if (DEBUG) {
	      LOG_DEBUG << "Using AmpSlope :" << mDb->amplitudeSlope(GetHardSec(sec, row)*mDb->numberOfPads()+GetHardPad(sec,pad,row)+1, row) <<endm;
            }
	    if (mDb->amplitudeSlope(GetHardSec(sec, row)*mDb->numberOfPads()+GetHardPad(sec,pad,row)+1, row)!= 0)
	      mADCArray[i] = mADCArray[i] / (mDb->amplitudeSlope(GetHardSec(sec, row)*mDb->numberOfPads()+GetHardPad(sec,pad,row)+1, row));
	    else
	      mADCArray[i] = 0;
	    
	    
	    if(DEBUG)
	      num_pixels[(int) (bin/30)]++;
	  
	    // Add random noise to each timebin
	    mADCArray[i] += noise->GetRandom();
	    
	    if(mADCArray[i] >= mParam->zeroSuppressThreshold()) {
	    
	      // count up occupancy
	      if(DEBUG)
		num_pixels_occupied[(int) (bin/30)]++;
	    
	      if (mADCArray[i] >= mMaxAdc)  
		mADCArray[i] = mMaxAdc;          // reset overflow
	    }
	  }
	}
      }
    }
  }
 if (DEBUG) {
  LOG_DEBUG << "Occupancies:" << endm;
  for(int lastloop=0; lastloop<11;lastloop++)
    {
      if(num_pixels[lastloop]>0) {
        LOG_DEBUG << "bin " << lastloop << " has occupancy" << num_pixels_occupied[lastloop]/(float) num_pixels[lastloop] << endm;
      }
    }
  }
 
 delete noise;
 return;
}

float StFtpcSlowSimReadout::PhiOfPad(const int pad, const int deg_or_rad)
{
    return (pad+0.5)*mDb->radiansPerPad() + mDb->radiansPerBoundary()/2;
}

int StFtpcSlowSimReadout::WhichPad(const float phi, int &isec)
{
    // phi and phi_min in rad
    float dphi = fmod(phi-phiMin+twopi,twopi);
    isec = (int)(dphi/(phiMax-phiMin));
    // Due to insufficient numerical precision, it is possible that in some cases isec=6
    // but 0<=isec<=5. If isec=6, set isec=0 to avoid segmentation violations
    if (isec == 6) isec = 0;
    dphi = dphi - isec*(phiMax-phiMin)- mDb->radiansPerBoundary()/2;
    int ipad = (int) (dphi/mDb->radiansPerPad());  // no +0.5 since pad 0 really starts at 0 and ends at 1
    if (ipad < 0)  {
        ipad = 0;
    }
    if (ipad > mDb->numberOfPads() - 1) {
        ipad = mDb->numberOfPads() - 1;
    }
    
    return ipad;
}

int StFtpcSlowSimReadout::WhichSlice(const float time)
{
    int itim = (int) (time*0.001/mDb->microsecondsPerTimebin()) ;    // time in nsec
    if (itim < 0) {
        itim = 0;
    }
    if (itim > mDb->numberOfTimebins() - 1) {
        itim = mDb->numberOfTimebins() - 1;
    }
    return itim;
}

float StFtpcSlowSimReadout::TimeOfSlice(const int sslice)
{
    return (sslice+0.5)*1000*mDb->microsecondsPerTimebin();         // time in nsec
}

int StFtpcSlowSimReadout::GetHardPad(const int daqsec, const int daqpad, const int irow)
{
  // ATTENTION: This function is only used for correct db access (gain table, time offset...)
  // The asic mapping for the output is done in StFtpcRawWriter

  int pad = daqpad;
  if (pad <0) pad =0;
  if (pad > (mDb->numberOfPads() -1)) pad = mDb->numberOfPads() -1;
  if (irow>=10)
    if (mDb->Asic2EastNotInverted() && (pad>63)&&(pad<96)) // no turning for center FEE card in each sector in East
      return pad;                                          // for old data (prior to 2003)
    else
      return (mDb->numberOfPads()-pad-1);
  else
    return (mDb->numberOfPads()-pad-1);

}


int StFtpcSlowSimReadout::GetHardSec(const int daqsec, const int irow)
{
  int sec = daqsec;
  if (sec < 0) sec = 0;
  if (sec > (mDb->numberOfSectors() -1 )) sec = mDb->numberOfSectors() -1;

  if (irow>=10)
    return sec;      
  else
    return (mDb->numberOfSectors()-sec-1);
}
	  
void StFtpcSlowSimReadout::Print() const 
{
    
  LOG_INFO << "StFtpcSlowSimReadout::Print ";
  LOG_INFO << " Number of pad rows = " 
           << mDb->numberOfPadrows() << endm;
  LOG_INFO << " Number of pad per row = " 
           << mDb->numberOfPads() << endm;
  LOG_INFO << " Pad length = " 
           << pad_length 
           << " pitch = " 
           << pad_pitch << " [cm]" << endm;
  LOG_INFO << " Shaping time = " 
           << shaper_time << " [ns]" << endm;
  LOG_INFO << " Time slice = " 
           << mDb->microsecondsPerTimebin()*1000 << " [ns]" << endm;
  LOG_INFO << " Pad response sigma = " 
           << sigma_prf << " [um]" << endm;
                          
}


void StFtpcSlowSimReadout::polya(const int ggnch, const float gglow, 
                    const float gghigh, const float ggdelta)
{
// generate probability distribution from
// Polya function for gain fluctuation
// c.f.: Ronaldo Bellazzini and Mario Spezziga
//       La Rivista del Nuovo Cimento V17N12(1994)1.
//
//       m=3/2, gamma(m)=::sqrt(pi)/2=0.8862269
//       polya(k) = m*::pow((m*k),(m-1))*exp(-m*k)/gamma(m)
//
    float m_polya = 1.5;
    float c_polya = 1.6925687;

    float x;
    float p;
    pcum[0] = 0.0;
    int i;
    for (i=1; i<ggnch; ++i) {
        x       = m_polya*(i*ggdelta+gglow);
        p       = c_polya*::pow(double(x),double(m_polya-1.0))*exp(-x);
        pcum[i] = pcum[i-1] + p ;
    }

    for (i=0; i<ggnch; ++i) {
        pcum[i] /= pcum[ggnch-1];             // renormalize it
        //LOG_DEBUG << "i=" << i << " pcum=" << pcum[i] << endm;
    }
}

int StFtpcSlowSimReadout::sample_polya(const float gain)
{
    float ran;
  
    if (mRandomNumberGenerator == 0) 
      {
        ran = gRandom->Rndm();
      } 
    else 
      {
        ran = ranmar();
      }

    int     ich = Locate(gnch, pcum, ran);
    //LOG_DEBUG << "ich = " << ich << endm;
    return  (int) ( gain * ( glow + ich * gdelta ) );

}

float StFtpcSlowSimReadout::InteGauss(const float x_1, const float x_2,
                  const float x_0, const float sig)
{

     float x,x1,x2 ;

     x1 = (x_1-x_0) /sig;
     x2 = (x_2-x_0) /sig;
     if (x1 > x2) {
         x  = x2;  x2 = x1;  x1 = x;
     }

     float del_x = (x2-x1)/((float) (mGaussIntSteps-1) );

     // integrate the gauss function
     float sum = 0;
     x = x1 + 0.5*del_x ;
     for( int i=0; i<(mGaussIntSteps-1); ++i ) {
         sum += exp(-0.5*x*x);
           x += del_x;
     }

     return del_x*0.39894228*sum; // 1/::sqrt(twopi)=0.39894228
}

float StFtpcSlowSimReadout::ranmar()
{
  /* Universal random number generator proposed by Marsaglia */
  /* and Zaman in report FSU-SCRI-87-50 */
  
  /* From "A Review of Pseudorandom Number Generators" by */
  /* F. James, CERN report SOFTWR 88-20. */
  
  /* Rewritten as a function by Bill Long, 26-may-1989. */
  /* Also modified to move cd and cm from initialization */
  /* routine RMARIN to here as parameters. */
  
  float uni;
  float cd;
  float cm;
  int i = 97, j = 33;
  
  cd = (float) 7654321./(float)16777216.;
  cm = (float)16777213./(float)16777216.;
  
  // LOG_DEBUG << Form("cd = %20.17f; cm = %20.17f", cd, cm) << endm;
  
  uni = uc.u[i-1] - uc.u[j-1];
  if (uni < (float)0.0) uni += (float)1.0;
  
  uc.u[i-1] = uni;
  
  --i;
  if (i == 0) i = 97;
  
  --j;
  if (j == 0 ) j = 97;
  
  uc.c -= cd;
  if (uc.c < (float)0.0) uc.c += cm;
  
  uni -= uc.c;
  if (uni < (float)0.0) uni += (float) 1.0;
  
  return 0.5;
}

void StFtpcSlowSimReadout::rmarin(int ij, int kl)
{
  /*   Initializing routine for RANMAR, must be called before */
  /*   generating any psuedorandom numbers with RANMAR. The */
  /*   input values should be in the ranges: */
  /*       0 <= ij <= 31328 */
  /*       0 <= kl <= 30081 */
  
  /*   This shows correspondence between the simplified seeds */
  /*   ij,kl and the original Marsaglia-Zaman seeds i,j,k,l */
  /*   To get standard values in Marsaglia-Zaman paper */
  /*   (i=12, j=34, k=56, l=78) put ij=1802, kl=9373. */
  
  int ii, jj;
  int i, j, k, l, m;
  float s, t;
  
  i = (ij/177) % 177 + 2;
  j = (ij) % 177 + 2;
  k = (kl/169) % 178 + 1;
  l = (kl) % 169;
  
  LOG_DEBUG << "Ranmar initialized:" << ij << " " 
       << kl << " "
       << i << " "
       << j << " "
       << k << " "
       << l << endm;
  
  for(ii=0; ii<97; ii++) {
    
    s = 0.0; 
    t = 0.5;
    
    for(jj=0; jj<24; jj++) {
        
      m =  ( (i*j) % 179 )*k % 179;
        i = j;
        j = k;
        k = m;
        l = (53*l+1) % 169;
        
        if ( (l*m)%64 >= 32 ) s += t;
        t *= 0.5;

    }
    
    uc.u[ii] = s;
    // LOG_DEBUG << "ii = " << ii << " s= " << s << endm;
  }
  
  uc.c = 362436./16777216.;
  
  // LOG_DEBUG << "c= " << uc.c << endm;

}
  


















