// $Id: StFtpcDriftMapMaker.cxx,v 1.23 2009/11/10 12:30:48 jcs Exp $
// $Log: StFtpcDriftMapMaker.cxx,v $
// Revision 1.23  2009/11/10 12:30:48  jcs
// replace StMagUtilities with StarMagField
//
// Revision 1.22  2007/04/28 17:56:10  perev
// Redundant StChain.h removed
//
// Revision 1.21  2006/08/02 13:57:57  jcs
// add deltaAr argument to allow user to change gas compostion (default: deltaAr=0)
//
// Revision 1.20  2005/12/12 14:43:53  jcs
// exit if error occurs while constructing StFtpcDbReader
//
// Revision 1.19  2003/09/30 08:58:20  jcs
// StMagUtilities constructer was changed; for safety's sake call with mode = 0
//
// Revision 1.18  2003/09/18 10:00:55  jcs
// remove obsolete version of StFtpcMagboltz2 (FORTRAN version in $CVSROOT/online/ftpc/Magboltz)
//
// Revision 1.17  2003/09/02 17:58:15  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.16  2002/09/24 09:50:09  jcs
// Remove gufld remains
//
// Revision 1.15  2002/01/22 22:09:59  jcs
// remove unused line of code to remove warning
//
// Revision 1.14  2001/10/29 13:00:38  jcs
// use new constructor in StFtpcDbReader
//
// Revision 1.13  2001/10/23 07:27:48  jcs
// implement new StFtpcDbReader constructor
//
// Revision 1.12  2001/10/22 09:40:18  jcs
// remove obsolete include  StFtpcParamReader.hh
//
// Revision 1.11  2001/08/10 15:34:40  jcs
// correct mistake - close ftpcDriftField
//
// Revision 1.10  2001/07/12 20:43:28  jcs
// remove tzero from ftpcDriftField
//
// Revision 1.9  2001/07/12 18:19:31  jcs
// compute drift map according to FTPC cathode voltage and magnetic field
//
// Revision 1.8  2001/05/17 20:45:19  jcs
// change to use Jim Thomas StMagUtilities
//
// Revision 1.7  2001/04/04 17:08:52  jcs
// remove references to StFtpcParamReader from StFtpcDbReader
//
// Revision 1.6  2001/04/02 12:06:34  jcs
// get FTPC calibrations,geometry from MySQL database
//
// Revision 1.5  2001/03/19 15:53:05  jcs
// use ftpcDimensions from database
//
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

#include <Stiostream.h>
#include <stdlib.h>
#include "St_db_Maker/St_db_Maker.h"
#include "StFtpcDriftMapMaker.h"
#include "StFtpcMagboltz1.hh"
#include "StFtpcClusterMaker/StFtpcDbReader.hh"

#include "StMessMgr.h"
#include "St_DataSetIter.h"
#include "TH1.h"
#include "TH2.h"

ClassImp(StFtpcDriftMapMaker)

//_____________________________________________________________________________
StFtpcDriftMapMaker::~StFtpcDriftMapMaker(){
}
//_____________________________________________________________________________
StFtpcDriftMapMaker::StFtpcDriftMapMaker(const StarMagField::EBField map,const Float_t factor,const Float_t deltaAr):
    m_dimensions(0),
    m_padrow_z(0),
    m_efield(0),
    m_vdrift(0),
    m_deflection(0),
    m_dvdriftdp(0),
    m_ddeflectiondp(0),
    m_gas(0),
    m_driftfield(0)
{
// Set Date,Time for offline database access so that the latest table entries are used
    mDbMaker     = (St_db_Maker*)GetMaker("db");
    mDbMaker->SetDateTime(20330101,0);

// Create tables

  St_DataSet *ftpc_geometry_db = GetDataBase("Geometry/ftpc");
  if ( !ftpc_geometry_db ){
     *gMessMgr<<"Could not locate MySQLDb:Geometry/ftpc"<<endm;
     exit(0);
  }
  St_DataSetIter       dblocal_geometry(ftpc_geometry_db);

  m_dimensions = (St_ftpcDimensions *)dblocal_geometry("ftpcDimensions");
  m_padrow_z   = (St_ftpcPadrowZ  *)dblocal_geometry("ftpcPadrowZ" );

  St_DataSet *ftpc_calibrations_db = GetDataBase("Calibrations/ftpc");
  if ( !ftpc_calibrations_db ){
     *gMessMgr<<"Could not locate MySQLDb:Calibrations/ftpc"<<endm;
     exit(0);
  }
  St_DataSetIter       dblocal_calibrations(ftpc_calibrations_db);

  m_gas           = (St_ftpcGas *)dblocal_calibrations("ftpcGas");



    m_driftfield    = (St_ftpcDriftField *)dblocal_calibrations("ftpcDriftField");
    m_efield     = (St_ftpcEField *)dblocal_calibrations("ftpcEField" );
    m_vdrift     = (St_ftpcVDrift *)dblocal_calibrations("ftpcVDrift" );
    m_deflection = (St_ftpcDeflection *)dblocal_calibrations("ftpcDeflection" );
    m_dvdriftdp     = (St_ftpcdVDriftdP *)dblocal_calibrations("ftpcdVDriftdP" );
    m_ddeflectiondp = (St_ftpcdDeflectiondP *)dblocal_calibrations("ftpcdDeflectiondP" );
    
  if (!m_efield || !m_vdrift || !m_deflection || !m_dvdriftdp || !m_ddeflectiondp 
                || !m_gas  || !m_driftfield) {
    cout<<"MySQLDb:Calibrations/ftpc not complete"<<endl;
    exit(0);
  }

  // create FTPC data base reader
  StFtpcDbReader *dbReader = new StFtpcDbReader(m_dimensions,
                                                m_padrow_z,
                                                m_efield,
                                                m_vdrift,
                                                m_deflection,
                                                m_dvdriftdp,
                                                m_ddeflectiondp,
                                                m_gas,
                                                m_driftfield);
   if (dbReader->returnCode != 0) {
     cout<<"Error constructing StFtpcDbReader"<<endl;
     return;
   }
   
    ftpcEField_st *ftpcEField = m_efield->GetTable();
    ftpcVDrift_st *ftpcVDrift = m_vdrift->GetTable();
    ftpcDeflection_st *ftpcDeflection = m_deflection->GetTable();
    ftpcdVDriftdP_st *ftpcdVDriftdP = m_dvdriftdp->GetTable();
    ftpcdDeflectiondP_st *ftpcdDeflectiondP = m_ddeflectiondp->GetTable();
    for ( Int_t iBin=0; iBin<dbReader->maximumNumberOfMagboltzBins(); iBin++) 
    {
      ftpcEField->e[iBin]   = 0.0;
      for(Int_t iPadrow=0; iPadrow<dbReader->numberOfPadrowsPerSide(); iPadrow++) {
         ftpcVDrift->v[iPadrow + dbReader->numberOfPadrowsPerSide()*iBin]   = 0.0;
         ftpcDeflection->psi[iPadrow + dbReader->numberOfPadrowsPerSide()*iBin]   = 0.0;
         ftpcdVDriftdP->dv_dp[iPadrow + dbReader->numberOfPadrowsPerSide()*iBin]   = 0.0;
         ftpcdDeflectiondP->dpsi_dp[iPadrow + dbReader->numberOfPadrowsPerSide()*iBin]   = 0.0;
      }
    }

    for ( Int_t iBin=0; iBin<dbReader->numberOfMagboltzBins(); iBin++) 
    {
      dbReader->setMagboltzEField(iBin,dbReader->minimumDriftField() + iBin*dbReader->stepSizeDriftField());
    }

  

    StarMagField *magField = new StarMagField(map, factor, kTRUE);

  
  // create magboltz
  StFtpcMagboltz1 *magboltz = new StFtpcMagboltz1();

  int i, j;
  float thisField, thisRadius;
  float posVector[3], bVector[3];
  float bMag, bTheta, bRadial;
  float vDrift, psiAngle, pressure;
  float upPressure, upDrift, upAngle, pOff;
  float eFinal;

  eFinal=0.1;

  posVector[0]=0;

  for(i=0; i < dbReader->numberOfMagboltzBins(); i++) 
    { 
      
      thisField = dbReader->minimumDriftField() 
	+ i*dbReader->stepSizeDriftField(); 
      thisRadius = dbReader->radiusTimesField() / thisField; 
      
      posVector[1]=thisRadius; 
      for(j=0; j < dbReader->numberOfPadrowsPerSide(); j++) 
 	{ 
 	  posVector[2]=dbReader->padrowZPosition(j); 
 	  /* sets posVector to (0, radius, z) */ 
	  
 	  magField->BField(posVector, bVector); 
	  printf("pos %f %f %f field %f %f %f\n", posVector[0], posVector[1], posVector[2], bVector[0], bVector[1], bVector[2]);
 	  bMag=::sqrt(bVector[0]*bVector[0] + bVector[1]*bVector[1] +  
 		    bVector[2]*bVector[2]); 
 	  bRadial=::sqrt(bVector[0]*bVector[0] + bVector[1]*bVector[1]); 
 	  bTheta=acos(bRadial/bMag)*90/acos(0.0); 
          // set sign of angle between E and B fields 
          bTheta = (factor/fabs(factor)) * bTheta;
 	  pressure=760.0; 
 	  upPressure=760.0+dbReader->pressureOffset();
          pOff=dbReader->pressureOffset()*1.3332; 
          /* Torr -> hPa */
	  vDrift=0;
	  psiAngle=0;
	  upDrift=0;
	  upAngle=0;
 	  printf("loop %d of %d\n", i, dbReader->numberOfMagboltzBins()); 
       	  printf("calling magboltz with field %f bMag %f bTheta %f pressure %f vDrift %f psiAngle %f\n", thisField, bMag, bTheta, pressure, vDrift, psiAngle);
cout<<"dbReader->percentAr() = "<<dbReader->percentAr()<<" deltaAr = "<<deltaAr<<endl;
	  float gas1=dbReader->percentAr() + deltaAr;
	  float gas2=dbReader->percentCO2() - deltaAr;;
	  float gas3=dbReader->percentNe();
	  float gas4=dbReader->percentHe();
	  float temperature=dbReader->baseTemperature();
          cout<<"calling magboltz with "<<gas1<<"% Ar and "<<gas2<<"% CO2"<<endl;
 	  magboltz->magboltz_(&thisField, &bMag, &bTheta, &pressure, &gas1, &gas2, &gas3, &gas4, &temperature, &vDrift, &psiAngle, &eFinal); 
     	  printf("called magboltz got field %f bMag %f bTheta %f pressure %f vDrift %f psiAngle %f\n", thisField, bMag, bTheta, pressure, vDrift, psiAngle);  
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

//-------------------------------------------------------------------
 
// Write out new ftpcEField

  cstruct = m_efield -> GetTable();   // Get pointer of table and copy to c-structure
  nrows = m_efield -> GetNRows();      // Get number of rows in the table

  fTableName = new char[20];
  strcpy(fTableName,"ftpcEField");
// Create new TTable object for c-structure
  table = TTable::New(fTableName,fTableName,cstruct,nrows);

  fOutputFileName = new char[100];
  strcpy(fOutputFileName,"./ftpcEField.C");
  ofstream ofs_ftpcEField(fOutputFileName);  // Open a file
  table -> SavePrimitive(ofs_ftpcEField,0);  // Write information of c-structure from object of TTable to the file
  ofs_ftpcEField.close();                    // Close the file
 

// Write out new ftpcDriftField

  cstruct = m_driftfield -> GetTable();   // Get pointer of table and copy to c-structure
  nrows = m_driftfield -> GetNRows();      // Get number of rows in the table

  fTableName = new char[20];
  strcpy(fTableName,"ftpcDriftField");
// Create new TTable object for c-structure
  table = TTable::New(fTableName,fTableName,cstruct,nrows);

  fOutputFileName = new char[100];
  strcpy(fOutputFileName,"./ftpcDriftField.C");
  ofstream ofs_ftpcDriftField(fOutputFileName);  // Open a file
  table -> SavePrimitive(ofs_ftpcDriftField,0);  // Write information of c-structure from object of TTable to the file
  ofs_ftpcDriftField.close();                    // Close the file

//-------------------------------------------------------------------
  delete dbReader;
  delete magboltz;

  cout <<"finished StFtpcDriftMapMaker" << endl;
}
//_____________________________________________________________________________

