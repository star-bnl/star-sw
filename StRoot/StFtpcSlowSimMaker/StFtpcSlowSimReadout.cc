// $Id: StFtpcSlowSimReadout.cc,v 1.1 2000/11/23 10:16:43 hummler Exp $
// $Log: StFtpcSlowSimReadout.cc,v $
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

#include <math.h>
#include <stdlib.h>

#include "StFtpcSlowSimField.hh"
#include "StFtpcSlowSimCluster.hh"
#include "StFtpcSlowSimReadout.hh"
#include "StFtpcClusterMaker/StFtpcParamReader.hh"

#ifndef DEBUG
#define DEBUG 0
#endif

StFtpcSlowSimReadout::StFtpcSlowSimReadout(StFtpcParamReader *paramReader,
					   float *adcIn, 
					   const StFtpcSlowSimField *field)
{
  mParam=paramReader;
  mRandomNumberGenerator = mParam->randomNumberGenerator();
  number_plane = mParam->numberOfPadrowsPerSide();
  pad_pitch = mParam->padPitch();
  pad_length = mParam->padLength();
  sigma_prf = mParam->sigmaPadResponseFuntion();   
  shaper_time = mParam->readoutShaperTime();
  slice = mParam->microsecondsPerTimebin();

  mOuterRadius = mParam->sensitiveVolumeOuterRadius();

  mADCArray=adcIn;

  // set parameters for Polya distribution and initialize it
  gnch   = 50;
  glow   = 0.1;
  ghigh  = 2.5; 
  gdelta = (ghigh-glow) / (float) gnch;
  pcum   = new float[gnch];
  polya(gnch, glow, ghigh, gdelta);
  
  int imax=mParam->numberOfPadrows()
    *mParam->numberOfSectors()
    *mParam->numberOfPads()
    *mParam->numberOfTimebins();
  for(int i=0; i<imax; ++i) 
    mADCArray[i] = 0.0;    //jcs
  
  if (mRandomNumberGenerator == 1) {
    // initialize the random number generator
    rmarin(1802, 9373);
  }
  
  // angle range in which each sector is calculated
  phiMin = mParam->simulationPhiStart() * M_PI/180.0;
  phiMax = mParam->simulationPhiEnd() * M_PI/180.0;    
  mGasGain = mParam->gasGain();
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
      
    if(DEBUG)
      cout << "pad_off=" << pad_off <<  "sig_phi=" << sig_phi << endl;
    sigma_pad =  sqrt(sig_phi*sig_phi + prf*prf ); 
}


void StFtpcSlowSimReadout::ShaperResponse(const StFtpcSlowSimCluster *cl)
{
  float srf = shaper_time*0.42466091; // time->sigma 1 / 2.35482
  float sig_rad = cl->GetSigRad();
  float rad_off = cl->GetRadOff();

  // convert the radial width to nsec
  float sig_time = sig_rad * mInverseFinalVelocity;
  time_off = 10000*rad_off * mInverseFinalVelocity;
  if(DEBUG)
    cout << "time_off=" << time_off <<  "sig_rad=" << sig_rad << endl;
  sigma_tim = sqrt( sig_time*sig_time + srf*srf) ;
}

void StFtpcSlowSimReadout::Digitize(const StFtpcSlowSimCluster *cl, const int irow)                                                              //jcs
{
  float n_sigmas_to_calc  = 5.0;        
  
  // get the readout position in radial direction
  float time_slice = mParam->microsecondsPerTimebin()*1000;// into nsec
  float time       = cl->GetDriftTime()*1000.;       // into nsec
  int     itim       = WhichSlice(time);
  
  // get the readout position in azimuthal direction
  float delta_phi  = mParam->radiansPerPad();
  float phi        = cl->GetPhi();
  int isec, jsec, nsecs;                                //jcs
  int     ipad       = WhichPad(phi,isec);         //jcs
  
  // big if() loop
  if ( itim > 2 && itim < (mParam->numberOfTimebins()-3) &&
       ipad > 2 && ipad < (mParam->numberOfPads()-3) ) 
    {      
      // and calculate the pad distribution
      
      float sigmaPadCentimeters   = sigma_pad *0.0001;  // into cm 
      float width_phi = n_sigmas_to_calc *sigmaPadCentimeters / mOuterRadius;
      //note: mOuterRadius is the radius of the Frisch grid, not the padplane,
      // but for this purpose it is good enough
      
      // store center of cluster
      float mid_phi = phi;
      float mid_time = time;
      float hypo = sqrt((pad_off/pad_pitch)*(pad_off/pad_pitch)
			+(time_off/(double)mParam->microsecondsPerTimebin()*1000)*
			(time_off/(double)mParam->microsecondsPerTimebin()*1000));
      int n_sub_hits = (int) (2*hypo);
      int current_sub_hit;
      
      if(DEBUG)
	cout << "hypo=" << hypo << " mid_phi=" << mid_phi << " mid_time=" << mid_time << " phi_off=" << pad_off/mOuterRadius << " time_off=" << time_off << endl;
      
      for(current_sub_hit=-n_sub_hits; current_sub_hit <= n_sub_hits; current_sub_hit++)
	{
	  if(n_sub_hits>0)
	    {
	      time = mid_time + ((time_off/(2*n_sub_hits))*current_sub_hit);
	      phi = mid_phi + ((pad_off/(mOuterRadius*(2*n_sub_hits)))*current_sub_hit);
	      //note: mOuterRadius is the radius of the Frisch grid, not the padplane,
	      // but for this purpose it is good enough
	    }
	  if(DEBUG)
	    cout << current_sub_hit << "th subhit at time " << time << " phi " << phi << " => padpos " << mOuterRadius*phi << endl;
	  ipad       = WhichPad(phi,isec);         //jcs
	  int isec_min;                                              //jcs
	  int isec_max;                                              //jcs
	  int pad_max_save;                                          //jcs
	  int npad;                                                  //jcs
	  int pad_min = WhichPad(phi-width_phi+2*M_PI,isec_min);       //jcs
	  int pad_max = WhichPad(phi+width_phi,isec_max);       //jcs
	  if ( isec_min > isec_max )                                 //jcs
	    nsecs = mParam->numberOfSectors() - isec_min + isec_max + 1;                 //jcs
	  else 
	    if (isec_min == isec_max && pad_min >= pad_max )           //jcs
	      nsecs = mParam->numberOfSectors() + 1;                                       //jcs
	    else                                                       //jcs
	      nsecs = isec_max - isec_min + 1;                        //jcs
	  int isec = isec_min;                                       //jcs
	  for (jsec=1; jsec<nsecs+1; ++jsec) {                           //jcs
	    if (isec != isec_max || (isec == isec_max && pad_min >= pad_max )) {  //jcs 
	      pad_max_save = pad_max;              //jcs
	      pad_max = mParam->numberOfPads()-1;                    //jcs
	    }                                          //jcs  
	    npad    = (pad_max - pad_min + 1);                          //jcs
	    float* pad = new float[npad];  // signal dist. in pads
	    int i;
	    
	    float dphi = fmod(phi-phiMin+2*M_PI,2*M_PI);   //jcs
	    isec = (int)(dphi/(phiMax-phiMin));
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
	    
	    // Now fill the mADCArray[irow,isec,pad,tim] array            //jcs
	    if(DEBUG)
	      cout << current_sub_hit << "th subhit from time " << tim_min << " to " << tim_min+ntim << " pad " << pad_min << " to " << pad_min+npad << endl;
	    for (i=0; i<npad; ++i) 
	      for (j=0; j<ntim; ++j) {
		int k = irow*mParam->numberOfSectors()*mParam->numberOfPads()*mParam->numberOfTimebins()+isec*mParam->numberOfPads()*mParam->numberOfTimebins()+(i+pad_min)*mParam->numberOfTimebins() + (j+tim_min) ;   //jcs
		mADCArray[k] += (float)(mFinalElectrons * pad[i] * sca[j])/(2*n_sub_hits+1);
	      }
	    
	    // recycle sca[] and pad[]
	    delete [] sca;
	    delete [] pad;
	    pad_min = 0;                                                 //jcs
	    pad_max = pad_max_save;                                      //jcs
	    ++isec;                                                      //jcs
	    if ( isec > mParam->numberOfSectors()-1 )
	      isec = 0;                                                  //jcs                                
	  }  // end of loop over sectors for multisector cluster       jcs
	} // end of loop over subhits
    } // end big if() loop
}


void StFtpcSlowSimReadout::OutputADC() const 
{
  int num_pixels[11]={0}, num_pixels_occupied[11]={0};
  
  for (int row=0; row<mParam->numberOfPadrows(); row++) { 
    for (int sec=0; sec<mParam->numberOfSectors(); sec++) {
      for (int pad=0; pad<mParam->numberOfPads(); pad++) {
	for (int bin=0; bin<mParam->numberOfTimebins(); bin++) {
	  int i=bin+mParam->numberOfTimebins()*pad+mParam->numberOfTimebins()*mParam->numberOfPads()*sec+mParam->numberOfTimebins()*mParam->numberOfPads()*mParam->numberOfSectors()*row;
	  
	  mADCArray[i] =(mADCArray[i] / mParam->adcConversion());

	  if(DEBUG)
	    num_pixels[(int) (bin/30)]++;
	  
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
 if (DEBUG) {
  cout << "Occupancies:" << endl;
  for(int lastloop=0; lastloop<11;lastloop++)
    {
      if(num_pixels[lastloop]>0)
      cout << "bin " << lastloop << " has occupancy" << num_pixels_occupied[lastloop]/(float) num_pixels[lastloop] << endl;
    }
  }
  return;
}

float StFtpcSlowSimReadout::PhiOfPad(const int pad, const int deg_or_rad)
{
    return (pad+0.5)*mParam->radiansPerPad() + mParam->radiansPerBoundary()/2;
}

int StFtpcSlowSimReadout::WhichPad(const float phi, int &isec)   //jcs
{
    // phi and phi_min in rad
    float dphi = fmod(phi-phiMin+2*M_PI,2*M_PI);   //jcs
    isec = (int)(dphi/(phiMax-phiMin));
    dphi = dphi - isec*(phiMax-phiMin)- mParam->radiansPerBoundary()/2;
    int ipad = (int) (dphi/mParam->radiansPerPad() +0.5) ;
    if (ipad < 0)  {
        ipad = 0;
    }
    if (ipad > mParam->numberOfPads() - 1) {
        ipad = mParam->numberOfPads() - 1;
    }
    return ipad;
}

int StFtpcSlowSimReadout::WhichSlice(const float time)
{
    int itim = (int) (time*0.001/mParam->microsecondsPerTimebin()) ;    // time in nsec
    if (itim < 0) {
        itim = 0;
    }
    if (itim > mParam->numberOfTimebins() - 1) {
        itim = mParam->numberOfTimebins() - 1;
    }
    return itim;
}

float StFtpcSlowSimReadout::TimeOfSlice(const int slice)
{
    return (slice+0.5)*1000*mParam->microsecondsPerTimebin();         // time in nsec
}

void StFtpcSlowSimReadout::Print() const 
{
    cout << " Number of pad rows = " 
         << mParam->numberOfPadrows() << endl;
    cout << " Number of pad per row = " 
         << mParam->numberOfPads() << endl;
    cout << " Pad length = " 
         << pad_length 
         << " pitch = " 
         << pad_pitch << " [cm]" << endl;
    cout << " Shaping time = " 
         << shaper_time << " [ns]" << endl;
    cout << " Time slice = " 
         << mParam->microsecondsPerTimebin()*1000 << " [ns]" << endl;
    cout << " Pad response sigma = " 
         << sigma_prf << " [um]" << endl;
                          
}


void StFtpcSlowSimReadout::polya(const int gnch, const float glow, 
                    const float ghigh, const float gdelta)
{
// generate probability distribution from
// Polya function for gain fluctuation
// c.f.: Ronaldo Bellazzini and Mario Spezziga
//       La Rivista del Nuovo Cimento V17N12(1994)1.
//
//       m=3/2, gamma(m)=sqrt(PI)/2=0.8862269
//       polya(k) = m*pow((m*k),(m-1))*exp(-m*k)/gamma(m)
//
    float m_polya = 1.5;
    float c_polya = 1.6925687;

    float x;
    float p;
    pcum[0] = 0.0;
    int i;
    for (i=1; i<gnch; ++i) {
        x       = m_polya*(i*gdelta+glow);
        p       = c_polya*pow(x,(m_polya-1.0))*exp(-x);
        pcum[i] = pcum[i-1] + p ;
    }

    for (i=0; i<gnch; ++i) {
        pcum[i] /= pcum[gnch-1];             // renormalize it
        //cout << "i=" << i << " pcum=" << pcum[i] << endl;
    }
}

int StFtpcSlowSimReadout::sample_polya(const float gain)
{
    float ran;
  
    if (mRandomNumberGenerator == 0) 
      {
	// use generator from math.h
        ran = (float) rand() / (float) RAND_MAX;
      } 
    else 
      {
        ran = ranmar();
      }

    int     ich = Locate(gnch, pcum, ran);
    //cout << "ich = " << ich << endl;
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

     return del_x*0.39894228*sum; // 1/sqrt(2*M_PI)=0.39894228
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
  
  /*
    printf(" cd = %20.17f; cm = %20.17f \n", cd, cm);
  */
  
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
  // printf(" Ranmar initialized: %d %d %d %d %d %d \n",ij,kl,i,j,k,l);
  
  cout << " Ranmar initialized:" << ij << " " 
       << kl << " "
                              << i << " "
       << j << " "
       << k << " "
       << l << endl;
  
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
    // printf(" ii = %d s= %f \n", ii,s);
  }
  
  uc.c = 362436./16777216.;
  
  // printf(" c= %f \n", uc.c);

}
  


















