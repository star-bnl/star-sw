#ifndef L2EMCGEOM_H
#define L2EMCGEOM_H
#include <stdio.h>

/*********************************************************************
 * $Id: L2EmcGeom.h,v 1.1 2007/12/19 02:30:17 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * StRoot-free Geometry container , common for BTOW + ETOW + ESMD
 *********************************************************************
 */
#include <string.h> // for memset

#include "L2EmcDb.h"

class BtowGeom{
 public:
  enum {mxEtaBins=40, mxRdo=(L2EmcDb::BTOW_MAXFEE*L2EmcDb::BTOW_DATSIZE) ,mxEtaBin=40, mxPhiBin=120, mxSubs=10 };
  // tmp, should be better protected
  float idealGain2Ene[mxEtaBins]; // (chan/GeV)
  float cosh[mxEtaBins]; 
  float gain2ET_rdo[mxRdo]; // (chan/GeV)coverts ADC to ET in GEV
  float gain2Ene_rdo[mxRdo]; // (chan/GeV)coverts ADC to Energy in GEV
  unsigned short thr_rdo[mxRdo];
  unsigned short ped_rdo[mxRdo];
  void clear() {  /*  clear content, set threshold @ max as default */
    memset(gain2ET_rdo,   0 ,sizeof(gain2ET_rdo));
    memset(gain2Ene_rdo,   0 ,sizeof(gain2Ene_rdo));
    memset(thr_rdo,    0xFFFF,sizeof(thr_rdo));
    memset(ped_rdo,    0,sizeof(ped_rdo));
  }
};

//--------------------------------------
class L2EmcGeom { // instanton class, created only once per week
  int par_maxADC;
  float par_maxET; // for maxADC scale, for ideal gains
  
 public:
  BtowGeom btow; // protect it better later
  float getMaxET(){ return par_maxET;}
  float getIdealAdc2ET(){ return par_maxADC/par_maxET;}
  L2EmcGeom();
  ~L2EmcGeom(){}; 


};


/***********************************************************

  Descripion:
	      
****************************************************/


#endif 


/*
*********************************************************************
  $ $

*/

