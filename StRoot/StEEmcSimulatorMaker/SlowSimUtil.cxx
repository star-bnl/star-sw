#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "SlowSimUtil.h"
//-------------------------------------------------
//-------------------------------------------------
SlowSimUtil::SlowSimUtil(){
   mip2ene = 0.001998*0.7;  // This is the SMD thickness of 0.7 mm 
                            // times the minimum ionizing energy loss of 
                            // 1.998 MeV/cm from the PDG book
   sig1pe = 0.85;           // from info from S. Vigdor on MAPMT test results
   Pmip2ene = 0.001998*0.5; // The pre- and post-shower tiles are
                            // only 5 mm thick.
   Pmip2pe = 2.6*1.5;       // 2.6 mip/tower scint * 1.5 light yield
                            // in pre- and post-shower elements

   // loop to init  mip2pe[] - this will eventually need to be
   // replaced by a table of measured values

   int i;
   for (i=0; i<MaxSmdStrips; i++) {
     // This is currently a trapezoidal-parameterization of
     // the light curve measurements
     mip2pe[i] = avgNumPePerMip(i);
   }


   //printf("SlowSimUtil:: constructor\n\n");
}

///
/// A parameterization of the average number of photoelectrons
/// per mip for a given SMD strip.  See elog 457.
///
Float_t SlowSimUtil::avgNumPePerMip(int stripID) {

  float y=0;
  float ya=0,yb=0,xa=1,xb=2;
  if ( stripID<1) {
    ;
  } else if (stripID<20) {
    xa=1; ya=2.;
    xb=20; yb=4.;
  } else if (stripID<250) {
    xa=20; ya=4.;
    xb=250; yb=6.;
  } else {
    xa=250; ya=6.;
    xb=290; yb=9.;
  }
  y=ya+(yb-ya)/(xb-xa) *(stripID -xa);
  return y;
}

