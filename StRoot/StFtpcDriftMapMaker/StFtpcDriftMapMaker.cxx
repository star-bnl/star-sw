// $Id: StFtpcDriftMapMaker.cxx,v 1.4 2001/03/09 13:54:27 jcs Exp $
// $Log: StFtpcDriftMapMaker.cxx,v $
// Revision 1.4  2001/03/09 13:54:27  jcs
// write out cstructs with new values so that they can be added to database
//
// Revision 1.3  2001/03/07 15:12:32  jcs
// use MySQL database instead of params
//
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
#include "StFtpcClusterMaker/StFtpcDbReader.hh"

#include "StMessMgr.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "TH1.h"
#include "TH2.h"

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
m_det(0),
    m_padrow_z(0),
    m_efield(0),
    m_vdrift(0),
    m_deflection(0),
    m_dvdriftdp(0),
    m_ddeflectiondp(0)
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
  m_det      = (St_fcl_det      *) local("fclpars/det");

  St_DataSet *ftpc_geometry_db = GetDataBase("Geometry/ftpc");
  if ( !ftpc_geometry_db ){
     return kStErr;
  }
  St_DataSetIter       dblocal_geometry(ftpc_geometry_db);

  m_padrow_z   = (St_ftpcPadrowZ  *)dblocal_geometry("ftpcPadrowZ" );

  St_DataSet *ftpc_calibrations_db = GetDataBase("Calibrations/ftpc");
  if ( !ftpc_calibrations_db ){
     return kStErr;
  }
  St_DataSetIter       dblocal_calibrations(ftpc_calibrations_db);

  m_efield     = (St_ftpcEField *)dblocal_calibrations("ftpcEField" );
  m_vdrift     = (St_ftpcVDrift *)dblocal_calibrations("ftpcVDrift" );
  m_deflection = (St_ftpcDeflection *)dblocal_calibrations("ftpcDeflection" );
  m_dvdriftdp     = (St_ftpcdVDriftdP *)dblocal_calibrations("ftpcdVDriftdP" );
  m_ddeflectiondp = (St_ftpcdDeflectiondP *)dblocal_calibrations("ftpcdDeflectiondP" );
  
  // Create Histograms    

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFtpcDriftMapMaker::Make(){

  // create parameter reader
  StFtpcParamReader *paramReader = new StFtpcParamReader(m_fss_gas,
							 m_fss_param,
							 m_det);

  // create FTPC data base reader
  StFtpcDbReader *dbReader = new StFtpcDbReader(paramReader,
                                                m_padrow_z,
                                                m_efield,
                                                m_vdrift,
                                                m_deflection,
                                                m_dvdriftdp,
                                                m_ddeflectiondp);
  
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

  for(i=0; i < paramReader->numberOfMagboltzBins(); i++) 
    { 
      
      thisField = paramReader->minimumDriftField() 
	+ i*paramReader->stepSizeDriftField(); 
      thisRadius = paramReader->radiusTimesField() / thisField; 
      
      posVector[1]=thisRadius; 
      for(j=0; j < paramReader->numberOfPadrowsPerSide(); j++) 
 	{ 
 	  posVector[2]=dbReader->padrowZPosition(j); 
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
 	  printf("loop %d of %d\n", i, paramReader->numberOfMagboltzBins()); 
//     	  printf("calling magboltz with field %f bMag %f bTheta %f pressure %f vDrift %f psiAngle %f\n", thisField, bMag, bTheta, pressure, vDrift, psiAngle);
	  float gas1=paramReader->percentAr();
	  float gas2=paramReader->percentCO2();
	  float gas3=paramReader->percentNe();
	  float gas4=paramReader->percentHe();
	  float temperature=paramReader->baseTemperature();
 	  magboltz->magboltz_(&thisField, &bMag, &bTheta, &pressure, &gas1, &gas2, &gas3, &gas4, &temperature, &vDrift, &psiAngle, &eFinal); 
//   	  printf("called magboltz got field %f bMag %f bTheta %f pressure %f vDrift %f psiAngle %f\n", thisField, bMag, bTheta, pressure, vDrift, psiAngle);  
 	  magboltz->magboltz_(&thisField, &bMag, &bTheta, &upPressure, &gas1, &gas2, &gas3, &gas4, &temperature, &upDrift, &upAngle, &eFinal); 
	  printf("changing magboltz values from %f %f %f %f\n",dbReader->magboltzVDrift(i,j), dbReader->magboltzDeflection(i,j),dbReader->magboltzdVDriftdP(i,j),dbReader->magboltzdDeflectiondP(i,j));
	  dbReader->setMagboltzEField(i,thisField);
	  dbReader->setMagboltzVDrift(i,j,vDrift);
	  dbReader->setMagboltzDeflection(i,j,psiAngle);
	  dbReader->setMagboltzdVDriftdP(i,j,(upDrift-vDrift)/pOff);
	  dbReader->setMagboltzdDeflectiondP(i,j,(upAngle-psiAngle)/pOff);
	  printf("                           to %f %f %f %f\n",dbReader->magboltzVDrift(i,j), dbReader->magboltzDeflection(i,j),dbReader->magboltzdVDriftdP(i,j),dbReader->magboltzdDeflectiondP(i,j));

 	} 
     } 
 
// Write out new ftpcDeflection

  void* cstruct = m_deflection -> GetTable();   // Get pointer of table and copy to c-structure
  Int_t   nrows = m_deflection -> GetNRows();      // Get number of rows in the table

  fTableName = new char[20];
  strcpy(fTableName,"ftpcDeflection");
// Create new TTable object for c-structure
  TTable* table = TTable::New(fTableName,fTableName,cstruct,nrows);

  fOutputFileName = new char[100];
  strcpy(fOutputFileName,"./ftpcDeflection.C");
  ofstream ofs_Deflection(fOutputFileName);  // Open a file
  table -> SavePrimitive(ofs_Deflection,0);  // Write information of c-structure from object of TTable to the file
  ofs_Deflection.close();                    // Close the file
 
// Write out new ftpcVDrift

  cstruct = m_vdrift -> GetTable();   // Get pointer of table and copy to c-structure
  nrows = m_vdrift -> GetNRows();      // Get number of rows in the table

  strcpy(fTableName,"ftpcVDrift");
// Create new TTable object for c-structure
  table = TTable::New(fTableName,fTableName,cstruct,nrows);

  strcpy(fOutputFileName,"./ftpcVDrift.C");
  ofstream ofs_VDrift(fOutputFileName);  // Open a file
  table -> SavePrimitive(ofs_VDrift,0);  // Write information of c-structure from object of TTable to the file
  ofs_VDrift.close();                    // Close the file
 
// Write out new ftpcdDeflectiondP

  cstruct = m_ddeflectiondp -> GetTable();   // Get pointer of table and copy to c-structure
  nrows = m_ddeflectiondp -> GetNRows();      // Get number of rows in the table

  strcpy(fTableName,"ftpcdDeflectiondP");
// Create new TTable object for c-structure
  table = TTable::New(fTableName,fTableName,cstruct,nrows);

  strcpy(fOutputFileName,"./ftpcdDeflectiondP.C");
  ofstream ofs_dDeflectiondP(fOutputFileName);  // Open a file
  table -> SavePrimitive(ofs_dDeflectiondP,0);  // Write information of c-structure from object of TTable to the file
  ofs_dDeflectiondP.close();                    // Close the file
 
// Write out new ftpcdVDriftdP

  cstruct = m_dvdriftdp -> GetTable();   // Get pointer of table and copy to c-structure
  nrows = m_dvdriftdp -> GetNRows();      // Get number of rows in the table

  strcpy(fTableName,"ftpcdVDriftdP");
// Create new TTable object for c-structure
  table = TTable::New(fTableName,fTableName,cstruct,nrows);

  strcpy(fOutputFileName,"./ftpcdVDriftdP.C");
  ofstream ofs_dVDriftdP(fOutputFileName);  // Open a file
  table -> SavePrimitive(ofs_dVDriftdP,0);  // Write information of c-structure from object of TTable to the file
  ofs_dVDriftdP.close();                    // Close the file

  delete paramReader;
  delete dbReader;
  delete magboltz;

  cout <<"finished fmg" << endl;
  MakeHistograms();
  return kStOK;
}
//_____________________________________________________________________________
void StFtpcDriftMapMaker::MakeHistograms() {

}
//_____________________________________________________________________________











