// $Id: St2009W_trigger.cxx,v 1.6 2011/09/14 14:23:21 stevens4 Exp $
//
//*-- Author : Ross Corliss, MIT

#include "St2009WMaker.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>

//________________________________________________
//________________________________________________
bool
St2009WMaker::passes_L0(){
  /*
    In 2009, L2W fed off the BHT3 L0 trigger, which required a single
    high tower to have an ADC of greater than 30.  This is the default
    threshold, but can be set from the macro if a different value is
    needed.
  */

  StMuEvent* muEve = mMuDstMaker->muDst()->event();
  for (int m=0;m<300;m++)
    if(muEve->emcTriggerDetector().highTower(m)>par_l0emulAdcThresh) return true;
  return false;
}

//________________________________________________
//________________________________________________
bool
St2009WMaker::passes_L2(){
  /*
    In 2009, the L2W trigger required a 2x2 patch of barrel towers
    where one tower has more than 5.0GeV and the sum of all four is
    E_T>13.0GeV.  These thresholds are the defaults, but can be set
    from the macro if a different value is needed.
  */
  
  for (int i=0;i<mxBtow;i++) //loop all towers
    if (wEve.bemc.statTile[0][i]==0) {//zero means good
      float adc = wEve.bemc.adcTile[0][i];
      float ET = adc*60./4096.; //ideal gains
      if (ET>par_l2emulSeedThresh) { // pass single tower thresh
	
	int iEta,iPhi; //get eta and phi bins for tower
	if( L2algoEtaPhi2IJ(positionBtow[i].Eta(),positionBtow[i].Phi(),iEta,iPhi)) continue;
	
	float maxET=0;
	
	//sum 2x2 cluster with ideal gains
	int I0=iEta-1; int J0=iPhi-1; 
	for(int I=I0;I<=I0+1;I++){ //loop eta
	  for(int J=J0;J<=J0+1;J++) { //loop phi
	    WeveCluster CL;CL.iEta=I;CL.iPhi=J; //tmp clus
	    for(int i=I;i<I+2;i++){//trim in eta-direction
	      if(i<0) continue;
	      if(i>=mxBTetaBin) continue;
	      for(int j=J;j<J+2;j++) {// wrap up in phi
		int jj=(j+mxBTphiBin)%mxBTphiBin;// keep it pos
		
		int softID = mapBtowIJ2ID[ i+ jj*mxBTetaBin];
		float adc = wEve.bemc.adcTile[kBTow][softID-1];
		if(adc<=0) continue; // skip towers w/o energy
		float ET = adc*60./4096.;
		CL.nTower++;
		CL.ET+=ET;
		CL.adcSum+=adc;
	      }
	    }
	    
	    if(maxET>CL.ET) continue;
	    maxET=CL.ET;
	  }
	}
	
	//passes L2 theshold
	if(maxET>par_l2emulClusterThresh) return true;
      }
    }
  return false;

}

 void 
 St2009WMaker::patchToEtaPhi(int patch, int*eta, int*phi)
 {
   if (patch<0 || patch>299)
     {
       printf("patchToEtaPhi p=%d, out of range. Eta phi not defined.\n",patch);
       return;
     }
   if (patch<150)
     {
       int m=14-patch/10;
       int n=patch%10;
       *eta=n/2+5;
       *phi=n%2+m*2;
     }
   else
     {
       int m=29-patch/10;
       int n=patch%10;
       *eta=4-n/2;
       *phi=1-n%2+m*2;
     }
   return;
 }

//$Log: St2009W_trigger.cxx,v $
//Revision 1.6  2011/09/14 14:23:21  stevens4
//update used for cross section PRD paper
//
//Revision 1.5  2010/12/02 18:31:43  rcorliss
//updated lumi code to match the starnote version
//
//Revision 1.4  2010/05/13 00:53:52  rcorliss
//fixed missing #include
//
//Revision 1.3  2010/05/12 19:04:05  rcorliss
//Corrected passes_L0() to use trigger patches instead of wEvent tiles.
//
//Revision 1.2  2010/01/13 03:34:20  stevens4
//give trig emulator access to barrel hits
//
//Revision 1.1  2009/11/23 23:00:18  balewski
//code moved spin-pool
//
