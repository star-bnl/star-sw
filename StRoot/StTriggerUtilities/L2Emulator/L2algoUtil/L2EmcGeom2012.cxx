#include <stdio.h>
#include <math.h>

#include "L2EmcGeom2012.h"


/*========================================
  ======================================== */
L2EmcGeom2012::L2EmcGeom2012(){
  par_maxADC=4095;
  par_maxET=60.0;


  int i;
  for(i=0;i<BtowGeom::mxEtaBin;i++ ){
    float avrEta=-0.975 +i*0.05; /* assume BTOW has fixed eta bin size */
    if(i==0) avrEta=-0.970;// first & lost towers are smaller
    if(i==39) avrEta=0.970;
    btow.cosh[i]=cosh(avrEta);
    btow.idealGain2Ene[i]=par_maxADC/par_maxET/btow.cosh[i];
    //  if (mLogFile && i%4==0)  fprintf(mLogFile,"aim: Btow iEtaBin=%2d eta=%.3f idealG=%.2f (GeV E_T), cosH=%.3f\n",i,avrEta, idealGainBtow[i], coshBtow[i]);
  }


  // set other hardcoded or calculated  params


}

