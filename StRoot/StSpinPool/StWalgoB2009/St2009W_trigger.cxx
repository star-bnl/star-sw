// $Id: St2009W_trigger.cxx,v 1.1 2009/11/23 23:00:18 balewski Exp $
//
//*-- Author : Ross Corliss, MIT

#include "St2009WMaker.h"

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
  if (!wEve.bemc.tileIn[0]) return false;//zero means bad
  for (int i=0;i<mxBtow;i++)
    if (wEve.bemc.statTile[0][i]==0)//zero means good
      if (wEve.bemc.adcTile[0][i]>par_l0emulAdcThresh) return true;
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
  for (int i=0;i<mxBtow;i++)
    if (wEve.bemc.statTile[0][i]==0)//zero means good
      if (wEve.bemc.eneTile[0][i]>par_l2emulSeedThresh){
	int ieta=i%mxBTetaBin;
	int iphi=i/mxBTetaBin;
	WeveCluster c=maxBtow2x2(ieta,iphi,0);
	if (c.ET>par_l2emulClusterThresh) return true;
      }
  return false;
}

//$Log: St2009W_trigger.cxx,v $
//Revision 1.1  2009/11/23 23:00:18  balewski
//code moved spin-pool
//
