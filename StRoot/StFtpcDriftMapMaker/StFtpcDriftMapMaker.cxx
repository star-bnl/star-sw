// $Id: StFtpcDriftMapMaker.cxx,v 1.2 2001/01/09 22:52:22 jcs Exp $
// $Log: StFtpcDriftMapMaker.cxx,v $
// Revision 1.2  2001/01/09 22:52:22  jcs
// remove include St_fmg_Module.h - now obsolete
//
// Revision 1.1  2000/12/20 08:44:01  jcs
// Replace pam/ftpc/fmg with maker
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcDriftMapMaker class                                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include "StFtpcDriftMapMaker.h"
#include "StFtpcMagboltz1.hh"
#include "StFtpcMagboltz2.hh"
#include "StFtpcClusterMaker/StFtpcParamReader.hh"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "TH1.h"
#include "TH2.h"

#include "tables/St_fss_param_Table.h"
#include "tables/St_fss_gas_Table.h"
#include "tables/St_fcl_padtrans_Table.h"
#include "tables/St_fcl_det_Table.h"
#include "tables/St_fcl_zrow_Table.h"

#ifndef gufld
#define gufld gufld_
extern "C" void gufld(float *, float *);
#endif

ClassImp(StFtpcDriftMapMaker)

//_____________________________________________________________________________
StFtpcDriftMapMaker::StFtpcDriftMapMaker(const char *name):
StMaker(name),
m_fss_gas(0),
m_fss_param(0),
m_padtrans(0),
m_det(0),
m_zrow(0)
{
}
//_____________________________________________________________________________
StFtpcDriftMapMaker::~StFtpcDriftMapMaker(){
}
//_____________________________________________________________________________
Int_t StFtpcDriftMapMaker::Init(){
// Create tables
  St_DataSet *ftpc = GetDataBase("ftpc");
  assert(ftpc);
  St_DataSetIter       local(ftpc);

  m_fss_gas  = (St_fss_gas      *) local("fsspars/fss_gas");
  m_fss_param= (St_fss_param    *) local("fsspars/fss_param");
  m_padtrans = (St_fcl_padtrans *) local("fclpars/padtrans");
  m_det      = (St_fcl_det      *) local("fclpars/det");
  m_zrow     = (St_fcl_zrow     *) local("fclpars/zrow");

  
  // Create Histograms    

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFtpcDriftMapMaker::Make(){

  // create parameter reader
  StFtpcParamReader *paramReader = new StFtpcParamReader(m_fss_gas,
							 m_fss_param,
							 m_padtrans,
							 m_det,
							 m_zrow);
  
  // create magboltz
  StFtpcMagboltz1 *magboltz = new StFtpcMagboltz1();
  //  StFtpcMagboltz2 *magboltz = new StFtpcMagboltz2();
  // only use Magboltz2 if you have a lot of time.
  // measurements on a PII400MHz indicate about 17 days...

  int i, j;
  float thisField, thisRadius;
  float posVector[3], bVector[3];
  float bMag, bTheta, bRadial;
  float vDrift, psiAngle, pressure;
  float upPressure, upDrift, upAngle, pOff;
  float eFinal;

  eFinal=0.1;

  posVector[0]=0;

  for(i=0; i < paramReader->numberOfPadtransBins(); i++) 
    { 
      
      thisField = paramReader->minimumDriftField() 
	+ i*paramReader->stepSizeDriftField(); 
      thisRadius = paramReader->radiusTimesField() / thisField; 
      
      posVector[1]=thisRadius; 
      for(j=0; j < paramReader->numberOfPadrowsPerSide(); j++) 
 	{ 
 	  posVector[2]=paramReader->padrowZPosition(j); 
 	  /* sets posVector to (0, radius, z) */ 
	  
 	  gufld(posVector, bVector); 
//   	  printf("pos %f %f %f field %f %f %f\n", posVector[0], posVector[1], posVector[2], bVector[0], bVector[1], bVector[2]);
 	  bMag=sqrt(bVector[0]*bVector[0] + bVector[1]*bVector[1] +  
 		    bVector[2]*bVector[2]); 
 	  bRadial=sqrt(bVector[0]*bVector[0] + bVector[1]*bVector[1]); 
 	  bTheta=acos(bRadial/bMag)*90/acos(0.0); 
 	  pressure=760.0; 
 	  upPressure=760.0+paramReader->dvdpCalcOffset(); 
          pOff=paramReader->dvdpCalcOffset()*1.3332; 
          /* Torr -> hPa */
	  vDrift=0;
	  psiAngle=0;
	  upDrift=0;
	  upAngle=0;
 	  printf("loop %d of %d\n", i, paramReader->numberOfPadtransBins()); 
//     	  printf("calling magboltz with field %f bMag %f bTheta %f pressure %f vDrift %f psiAngle %f\n", thisField, bMag, bTheta, pressure, vDrift, psiAngle);
	  float gas1=paramReader->percentAr();
	  float gas2=paramReader->percentCO2();
	  float gas3=paramReader->percentNe();
	  float gas4=paramReader->percentHe();
	  float temperature=paramReader->baseTemperature();
 	  magboltz->magboltz_(&thisField, &bMag, &bTheta, &pressure, &gas1, &gas2, &gas3, &gas4, &temperature, &vDrift, &psiAngle, &eFinal); 
//   	  printf("called magboltz got field %f bMag %f bTheta %f pressure %f vDrift %f psiAngle %f\n", thisField, bMag, bTheta, pressure, vDrift, psiAngle);  
 	  magboltz->magboltz_(&thisField, &bMag, &bTheta, &upPressure, &gas1, &gas2, &gas3, &gas4, &temperature, &upDrift, &upAngle, &eFinal); 
	  printf("changing padtrans values from %f %f %f %f\n",paramReader->padtransVDrift(i,j), paramReader->padtransDeflection(i,j),paramReader->padtransdVDriftdP(i,j),paramReader->padtransdDeflectiondP(i,j));
	  paramReader->setPadtransEField(i,thisField);
	  paramReader->setPadtransVDrift(i,j,vDrift);
	  paramReader->setPadtransDeflection(i,j,psiAngle);
	  paramReader->setPadtransdVDriftdP(i,j,(upDrift-vDrift)/pOff);
	  paramReader->setPadtransdDeflectiondP(i,j,(upAngle-psiAngle)/pOff);
	  printf("                           to %f %f %f %f\n",paramReader->padtransVDrift(i,j), paramReader->padtransDeflection(i,j),paramReader->padtransdVDriftdP(i,j),paramReader->padtransdDeflectiondP(i,j));

 	} 
     } 
 
  delete magboltz;

  cout <<"finished fmg" << endl;
  MakeHistograms();
  return kStOK;
}
//_____________________________________________________________________________
void StFtpcDriftMapMaker::MakeHistograms() {

}
//_____________________________________________________________________________











