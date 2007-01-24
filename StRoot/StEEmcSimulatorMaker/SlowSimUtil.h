#ifndef SlowSimUtil_h
#define SlowSimUtil_h
/*********************************************************************
 * $Id: SlowSimUtil.h,v 1.2 2007/01/24 21:07:01 balewski Exp $
 *********************************************************************
 * Descripion:
 *   Adds Poisson statistical fluctuations to SMD, Pre- and Post-Shower
 *   ADC values plus Gaussian resolution to the photoelectron peaks for
 *   Monte Carlo simulated events
 *********************************************************************/

#include "StEEmcUtil/EEfeeRaw/EEdims.h"
#include "TObject.h"

class SlowSimUtil {
 private:
  enum {mx1=50}; //example
  
 protected:

  // parameters, fixed :
  float mip2ene; // conversion from mips to  energy in GeV
  float sig1pe;  // width of the single photoelectron peak (in p.e.)
  float mip2pe[MaxSmdStrips]; // conversion from mip to p.e. from ANL
                              // cosmic ray measurements for SMD strips
  float Pmip2ene; // as above for pre- and post-shower elements
  float Pmip2pe; // as above for pre- and post-shower elements

  Float_t avgNumPePerMip(Int_t stripID); // avg # p.e. per mip

 public:

  SlowSimUtil();
};


#endif
/*****************************************************************
 * $Log: SlowSimUtil.h,v $
 * Revision 1.2  2007/01/24 21:07:01  balewski
 * 1) no cout or printf, only new Logger
 * 2) EndcapMixer:
 *    - no assert()
 *    - locks out on first fatal error til the end of the job
 *
 * Revision 1.1  2004/12/15 17:02:56  balewski
 * try 2
 *
 *
 *
 ********************************************************************/

