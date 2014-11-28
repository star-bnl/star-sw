#ifndef L2EMCGEOM_H
#define L2EMCGEOM_H
#include <stdio.h>

/*******************************************************
 * $Id: L2EmcGeom.h,v 1.6 2011/10/19 16:12:10 jml Exp $
 * \author Jan Balewski, IUCF, 2006 
 *******************************************************
 * Descripion:
 * StRoot-free Geometry container , common for BTOW + ETOW + ESMD
 *******************************************************
 */
#include <string.h> // for memset

#include "L2EmcDb.h"

//-------------------------------------------
class BtowGeom{
 public:
  enum { mxRdo=(L2EmcDb::BTOW_MAXFEE*L2EmcDb::BTOW_DATSIZE) ,mxEtaBin=40, mxPhiBin=120, mxSubs=10 };
  // tmp, should be better protected
  float idealGain2Ene[mxEtaBin]; // (chan/GeV)
  float cosh[mxEtaBin]; 
  float gain2ET_rdo[mxRdo]; // (chan/GeV)coverts ADC to ET in GEV
  float gain2Ene_rdo[mxRdo]; // (chan/GeV)coverts ADC to Energy in GEV
  unsigned short ped_shifted_rdo[mxRdo];
  unsigned short thr_rdo[mxRdo];
  unsigned short ped_rdo[mxRdo];
  void clear() {  /*  clear content, set threshold @ max as default */
    memset(gain2ET_rdo,    0 ,sizeof(gain2ET_rdo));
    memset(gain2Ene_rdo,   0 ,sizeof(gain2Ene_rdo));
    memset(ped_shifted_rdo,    0,sizeof(ped_shifted_rdo));
    memset(thr_rdo,    0xFFFF,sizeof(thr_rdo));
    memset(ped_rdo,    0,sizeof(ped_rdo));
  }
};

//-------------------------------------------
class EtowGeom{
 public:
  enum { mxRdo=(L2EmcDb::ETOW_MAXFEE*L2EmcDb::ETOW_DATUSED) ,mxEtaBin=12, mxPhiBin=60, mxSubs=5 };
  // tmp, should be better protected
  float idealGain2Ene[mxEtaBin]; // (chan/GeV)
  float cosh[mxEtaBin]; 
  float gain2ET_rdo[mxRdo]; // (chan/GeV)coverts ADC to ET in GEV
  float gain2Ene_rdo[mxRdo]; // (chan/GeV)coverts ADC to Energy in GEV
  unsigned short ped_shifted_rdo[mxRdo];
  unsigned short thr_rdo[mxRdo];
  unsigned short ped_rdo[mxRdo];
  void clear() {  /*  clear content, set threshold @ max as default */
    memset(gain2ET_rdo,    0 ,sizeof(gain2ET_rdo));
    memset(gain2Ene_rdo,   0 ,sizeof(gain2Ene_rdo));
    memset(ped_shifted_rdo,    0,sizeof(ped_shifted_rdo));
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
  EtowGeom etow; // protect it better later
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

