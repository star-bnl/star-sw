// $Id: StFtpcSlowSimMaker.cxx,v 1.22 2003/09/29 21:37:28 oldi Exp $
// $Log: StFtpcSlowSimMaker.cxx,v $
// Revision 1.22  2003/09/29 21:37:28  oldi
// Small change to make it compatible with the new StFtpcTrackingParams class.
//
// Revision 1.21  2003/09/02 17:58:16  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.20  2003/07/18 18:31:47  perev
// test for nonexistance of XXXReader added
//
// Revision 1.19  2003/07/04 14:04:51  fsimon
// Add rotation of hits from global GEANT coordinates into local FTPC coordinates.
// This needs an instance of StFtpcTrackingParams
//
// Revision 1.18  2003/07/03 13:25:47  fsimon
// Added database access for cathode offset information.
//
// Revision 1.17  2003/06/10 13:15:12  jcs
// get min,max gas temperature and pressure limits from database
//
// Revision 1.16  2003/02/28 13:00:27  jcs
// for embedding, calculate temperature,pressure corrections using values from offline database
//
// Revision 1.15  2003/02/14 16:55:50  fsimon
// Add functionality that allows for different temperature corrections
// in west and east, important for embedding. In the absence af a daq
// dataset, the standard temperature values will be used.
//
// In this version: Hardcoded values for temperatures with existing daq
// datasets, set for dAu running with SVT on, runs 4036xxxx and 4035xxxx
//
// Revision 1.14  2003/01/29 12:10:27  fsimon
// Change call of StFtpcRawWriter to allow for switch for inversion of ASIC 2
// in FTPC E (error in Y2001-2002 DAQ mapping)
//
// Revision 1.13  2003/01/14 12:58:25  jcs
// use Geometry_ftpc/ftpcAsicMap to control corrections for error in Y2001-2002
// FTPC asic mapping
//
// Revision 1.12  2002/10/23 09:13:58  fsimon
// Use calibration Db instead of local (local commented out, uncomment for use)
//
// Revision 1.11  2002/10/16 12:29:15  fsimon
// Include ftpcAmpSlope, ftpcAmpOffset and ftpcTimeOffset in Database access
// permits usage of gain factors and time offset in the simulator
//
// Revision 1.10  2002/06/04 13:54:21  jcs
// move GetDataBase from Make to InitRun
//
// Revision 1.9  2001/10/29 12:56:55  jcs
// select FTPC drift maps according to flavor of magnetic field
//
// Revision 1.8  2001/10/19 09:42:34  jcs
// tZero now in data base in ftpcElectronics
//
// Revision 1.7  2001/04/23 20:34:40  oldi
// Output sent to StMessMgr now.
//
// Revision 1.6  2001/04/04 17:08:57  jcs
// remove references to StFtpcParamReader from StFtpcDbReader
//
// Revision 1.5  2001/04/02 12:04:34  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters from StarDb/ftpc
//
// Revision 1.4  2001/03/19 15:53:10  jcs
// use ftpcDimensions from database
//
// Revision 1.3  2001/03/06 23:36:09  jcs
// use database instead of params
//
// Revision 1.2  2001/01/11 18:28:47  jcs
// use PhysicalConstants.h instead of math.h, remove print statement
//
// Revision 1.1  2000/11/23 10:16:43  hummler
// New FTPC slow simulator in pure maker form
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpcSlowSimMaker class                                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include "StFtpcSlowSimMaker.h"
#include "StFtpcSlowSimulator.hh"
#include "StFtpcRawWriter.hh"

//VP#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDAQMaker/StFTPCReader.h"
#include "PhysicalConstants.h"

#include "StFtpcClusterMaker/StFtpcParamReader.hh"
#include "StFtpcClusterMaker/StFtpcDbReader.hh"
#include "StFtpcClusterMaker/StFtpcGeantReader.hh"

// include for Detector Rotations
#include "StFtpcTrackMaker/StFtpcTrackingParams.hh" 

#include "StDetectorDbMaker/StDetectorDbFTPCGas.h"
#include "St_db_Maker/St_db_Maker.h"

#include "StMessMgr.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "TH1.h"
#include "TH2.h"

#ifndef gufld
#define gufld gufld_
extern "C" void gufld(float *, float *);
#endif

#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"
#include "tables/St_fcl_ftpcndx_Table.h" 
#include "tables/St_fcl_ftpcsqndx_Table.h" 
#include "tables/St_fcl_ftpcadc_Table.h" 

ClassImp(StFtpcSlowSimMaker)

//_____________________________________________________________________________
StFtpcSlowSimMaker::StFtpcSlowSimMaker(const char *name):
StMaker(name),
    m_clusterpars(0),
    m_slowsimgas(0),
    m_slowsimpars(0),
    m_dimensions(0),
    m_asicmap(0),
    m_efield(0),
    m_vdrift(0),
    m_deflection(0),
    m_dvdriftdp(0),
    m_ddeflectiondp(0),
    m_ampslope(0),
    m_ampoffset(0),
    m_timeoffset(0),
    m_driftfield(0),
    m_gas(0),
    m_electronics(0),
    m_cathode(0)
{
}
//_____________________________________________________________________________
StFtpcSlowSimMaker::~StFtpcSlowSimMaker(){
}
//_____________________________________________________________________________
Int_t StFtpcSlowSimMaker::InitRun(int runnumber){
  Float_t x[3] = {0,0,0};
  Float_t b[3];
  gufld(x,b);
  Double_t gFactor = b[2]/4.980;

   mDbMaker     = (St_db_Maker*)GetMaker("db");
   Int_t dbDate = mDbMaker->GetDateTime().GetDate();
   cout<<"StFtpcSlowSimMaker: dbDate = "<<dbDate<<endl;

  gMessMgr->Info() << "StFtpcSlowSimMaker::InitRun: gFactor is "<<gFactor<<endm;

  // Load the correct FTPC drift maps depending on magnetic field

  // Full Field Positive ?
  if ( gFactor > 0.8 ) {
     SetFlavor("ffp10kv","ftpcVDrift");
     SetFlavor("ffp10kv","ftpcdVDriftdP");
     SetFlavor("ffp10kv","ftpcDeflection");
     SetFlavor("ffp10kv","ftpcdDeflectiondP");
  }
  else if ( gFactor > 0.2 ) {
     SetFlavor("hfp10kv","ftpcVDrift");
     SetFlavor("hfp10kv","ftpcdVDriftdP");
     SetFlavor("hfp10kv","ftpcDeflection");
     SetFlavor("hfp10kv","ftpcdDeflectiondP");
  }
  else if ( gFactor > -0.2 ) {
     SetFlavor("zf10kv","ftpcVDrift");
     SetFlavor("zf10kv","ftpcdVDriftdP");
     SetFlavor("zf10kv","ftpcDeflection");
     SetFlavor("zf10kv","ftpcdDeflectiondP");
  }
  else if ( gFactor > -0.8 ) {
     SetFlavor("hfn10kv","ftpcVDrift");
     SetFlavor("hfn10kv","ftpcdVDriftdP");
     SetFlavor("hfn10kv","ftpcDeflection");
     SetFlavor("hfn10kv","ftpcdDeflectiondP");
  }
  else {
     SetFlavor("ffn10kv","ftpcVDrift");
     SetFlavor("ffn10kv","ftpcdVDriftdP");
     SetFlavor("ffn10kv","ftpcDeflection");
     SetFlavor("ffn10kv","ftpcdDeflectiondP");
  }    

  St_DataSet *ftpc_geometry_db = GetDataBase("Geometry/ftpc");
  if ( !ftpc_geometry_db ){
     gMessMgr->Warning() << "StFtpcSlowSimMaker::Error Getting FTPC database: Geometry"<<endm;
     return kStWarn;
  }
  St_DataSetIter       dblocal_geometry(ftpc_geometry_db);

  m_dimensions = (St_ftpcDimensions *)dblocal_geometry("ftpcDimensions");
  m_asicmap   = (St_ftpcAsicMap *)dblocal_geometry("ftpcAsicMap");

  m_cathode      = (St_ftpcInnerCathode *)dblocal_geometry("ftpcInnerCathode");

  St_DataSet *ftpc_calibrations_db = GetDataBase("Calibrations/ftpc");
  if ( !ftpc_calibrations_db ){
     gMessMgr->Warning() << "StFtpcSlowSimMaker::Error Getting FTPC database: Calibrations"<<endm;
     return kStWarn;
  }
  St_DataSetIter       dblocal_calibrations(ftpc_calibrations_db);
 
  
  m_efield     = (St_ftpcEField *)dblocal_calibrations("ftpcEField" );
  m_vdrift     = (St_ftpcVDrift *)dblocal_calibrations("ftpcVDrift" );
  m_deflection = (St_ftpcDeflection *)dblocal_calibrations("ftpcDeflection" );
  m_dvdriftdp  = (St_ftpcdVDriftdP *)dblocal_calibrations("ftpcdVDriftdP" );
  m_ddeflectiondp = (St_ftpcdDeflectiondP *)dblocal_calibrations("ftpcdDeflectiondP" );
  m_gas        = (St_ftpcGas *)dblocal_calibrations("ftpcGas");
  m_driftfield = (St_ftpcDriftField *)dblocal_calibrations("ftpcDriftField");
  m_electronics = (St_ftpcElectronics *)dblocal_calibrations("ftpcElectronics");
  
  // Get Database for gain factors and time offset
  m_ampslope = (St_ftpcAmpSlope *)dblocal_calibrations("ftpcAmpSlope" );
  m_ampoffset = (St_ftpcAmpOffset *)dblocal_calibrations("ftpcAmpOffset");

  m_timeoffset = (St_ftpcTimeOffset *)dblocal_calibrations("ftpcTimeOffset");
 

  // instance tracking parameters for rotations
  StFtpcTrackingParams::Instance(kTRUE, 
				 (St_ftpcCoordTrans *)dblocal_calibrations("ftpcCoordTrans"),
				 GetDataBase("RunLog"),
				 (StBFChain*) GetChain());
  // get ftpc parameters
  St_DataSet *ftpcParsDb = GetInputDB("ftpc");
  assert(ftpcParsDb);
  St_DataSetIter ftpcPars(ftpcParsDb);
  

  StFtpcTrackingParams::Instance(0,
  				 (St_ftpcTrackingPars *)ftpcPars("ftpcTrackingPars"),
  				 (St_fde_fdepar *)ftpcPars("fdepars/fdepar"),
  				 (St_ftpcDimensions *)dblocal_geometry("ftpcDimensions"), 
  				 (St_ftpcPadrowZ *)dblocal_geometry("ftpcPadrowZ"));



  //cout << "Global Dbs read...\n";
 
  //cout << "Getting local Db ..\n";
  St_DataSet *ftpclocal = GetDataBase("ftpc");  // zum Verwenden der lokalen DB
  if ( !ftpclocal ){
     gMessMgr->Warning() << "StFtpcSlowSimMaker::Error Getting local FTPC database: Calibrations"<<endm;
     return kStWarn;
  } //assert(ftpc);
  // cout << "Local Db found, creating iterator...\n";
 
  St_DataSetIter       local(ftpclocal);
  
  //cout << "Iterator created ...\n";

  //m_timeoffset = (St_ftpcTimeOffset *)local("ftpcTimeOffset");
 
  m_clusterpars  = (St_ftpcClusterPars *)local("ftpcClusterPars");
  m_slowsimgas   = (St_ftpcSlowSimGas  *)local("ftpcSlowSimGas");
  m_slowsimpars  = (St_ftpcSlowSimPars *)local("ftpcSlowSimPars");

  //cout << "Local Dbs read...\n";
 


 
  return 0;
}
//_____________________________________________________________________________
Int_t StFtpcSlowSimMaker::Init(){


  // Create Histograms
  m_nadc    = new TH1F("fss_total_adc","Total number of adcs in both FTPCs",1000,0.,2000000.);
  m_nsqndx  = new TH1F("fss_sqndx","FTPC raw data sequence index",100,0.,100000.);
  m_nadc_index1  = new TH2F("fss_nadc_index1","Total number of adcs vs. number of adcs in FTPC East",100,0.,2000000.,100,0.,1000000.);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFtpcSlowSimMaker::Make(){

  St_DataSetIter geant(GetInputDS("geant"));
  St_g2t_vertex  *g2t_vertex  = (St_g2t_vertex *)  geant("g2t_vertex");
  St_g2t_track   *g2t_track   = (St_g2t_track *)   geant("g2t_track");
  St_g2t_ftp_hit *g2t_ftp_hit = (St_g2t_ftp_hit *) geant("g2t_ftp_hit");
  if (g2t_vertex && g2t_track && g2t_ftp_hit){

    St_DataSetIter local(m_DataSet); local.Cd("pixels");
    St_fcl_ftpcndx   *fcl_ftpcndx  = new St_fcl_ftpcndx("fcl_ftpcndx",2);
    local.Add(fcl_ftpcndx);
    St_fcl_ftpcsqndx *fcl_ftpcsqndx = new St_fcl_ftpcsqndx("fcl_ftpcsqndx",500000);
    local.Add(fcl_ftpcsqndx);
    St_fcl_ftpcadc   *fcl_ftpcadc  = new St_fcl_ftpcadc("fcl_ftpcadc",2000000);
    local.Add(fcl_ftpcadc);

    //cout <<" create data reader \n";

   // create data reader
    StFtpcGeantReader *geantReader = new StFtpcGeantReader(g2t_vertex,
							   g2t_track,
							   g2t_ftp_hit);
    // cout << "create FTPC database reader\n";
    //create FTPC database reader
    StFtpcDbReader *dbReader = new StFtpcDbReader(m_dimensions,
		                                m_asicmap,
                                                m_efield,
                                                m_vdrift,
                                                m_deflection,
                                                m_dvdriftdp,
                                                m_ddeflectiondp,
						m_gas,
						m_driftfield,
                                                m_electronics,
						m_ampslope,
                                                m_ampoffset,
                                                m_timeoffset,
						m_cathode);


    //cout << "create parameter reader\n";
    // create parameter reader
    StFtpcParamReader *paramReader = new StFtpcParamReader(m_clusterpars,
                                                           m_slowsimgas,
                                                           m_slowsimpars);

    cout<<"paramReader->gasTemperatureWest() = "<<paramReader->gasTemperatureWest()<<endl;
    cout<<"paramReader->gasTemperatureEast() = "<<paramReader->gasTemperatureEast()<<endl;

    if ( paramReader->gasTemperatureWest() == 0 && paramReader->gasTemperatureEast() == 0) {
       cout<<"Using the following values from database:"<<endl;
       cout<<"          EastIsInverted            = "<<dbReader->EastIsInverted()<<endl;
       cout<<"          Asic2EastNotInverted      = "<<dbReader->Asic2EastNotInverted()<<endl;
       cout<<"          tzero                     = "<<dbReader->tZero()<<endl;
       cout<<"          temperatureDifference     = "<<dbReader->temperatureDifference()<<endl;
       cout<<"          defaultTemperatureWest    = "<<dbReader->defaultTemperatureWest()<<endl;
       cout<<"          defaultTemperatureEast    = "<<dbReader->defaultTemperatureEast()<<endl;
       cout<<"          magboltzVDrift(0,0)       = "<<dbReader->magboltzVDrift(0,0)<<endl;
       cout<<"          magboltzDeflection(0,0)   = "<<dbReader->magboltzDeflection(0,0)<<endl;
    }

    // check db values for cathode offset and angle
    cout << "StFtpcSlowSimulator with cathode offset, using the following parameters:"<<endl;
    cout << "                    offsetCathodeWest  = " << dbReader->offsetCathodeWest() << endl;
    cout << "                    angleOffsetWest    = " << dbReader->angleOffsetWest() << endl;
    cout << "                    offsetCathodeEast  = " << dbReader->offsetCathodeEast() << endl;
    cout << "                    angleOffsetEast    = " << dbReader->angleOffsetEast() << endl;

  // get temperatures from offline db, used for embedding!
  // as long as there is no daq data, standard temperatures are used!

  St_DataSet *daqDataset;
  StDAQReader *daqReader;
  StFTPCReader *ftpcReader=NULL;
  daqDataset=GetDataSet("StDAQReader");
  if(daqDataset)
    {
      gMessMgr->Message("", "I", "OST") << "Using StDAQReader to get StFTPCReader" << endm;
      assert(daqDataset);
      daqReader=(StDAQReader *)(daqDataset->GetObject());
      assert(daqReader);
      ftpcReader=daqReader->getFTPCReader();

      if (!ftpcReader || !ftpcReader->checkForData()) {
	gMessMgr->Message("", "W", "OST") << "No FTPC data available!" << endm;
        delete paramReader;
        delete dbReader;
	return kStWarn;
      }

      // get pressure and gas temperature from offline DB; test and use valid values
      StDetectorDbFTPCGas * gas = StDetectorDbFTPCGas::instance();
      if ( !gas ){
          gMessMgr->Warning() << "StFtpcClusterMaker::Error Getting FTPC Online database: Conditions"<<endm;
          delete paramReader;
          delete dbReader;
          return kStWarn;
      }
      // Barometric Pressure
      if (gas->getBarometricPressure() >= dbReader->minPressure() && gas->getBarometricPressure() <= dbReader->maxPressure()) {
          gMessMgr->Info() <<"Change normalizedNowPressure from "<<paramReader->normalizedNowPressure()<<" to "<<gas->getBarometricPressure()<<endm;
          paramReader->setNormalizedNowPressure(gas->getBarometricPressure());
      }
      else {
          gMessMgr->Info() << "Invalid value ("<<gas->getBarometricPressure()<<") from offline database for barometric pressure - using previous value ("<<paramReader->normalizedNowPressure()<<")"<<endm;
      }

      // Calculate FTPC gas temperature from body temperatures
      // default values change depending on SVT high voltage on/off
      // currently using daqReader->SVTPresent() to test but may need
      // access to Conditions_svt/svtInterLocks

      // calculate average body temperature west

         Int_t numberBodyTemperaturesWest = 0;
         Float_t averageBodyTemperatureWest = 0.0;
	  
	 if (gas->getBody1West() >= dbReader->minGasTemperature() && gas->getBody1West()<= dbReader->maxGasTemperature() ) {
		 averageBodyTemperatureWest = averageBodyTemperatureWest + gas->getBody1West();
		 numberBodyTemperaturesWest++;
		 cout<<"gas->getBody1West() = "<<gas->getBody1West()<<" numberBodyTemperaturesWest = "<<numberBodyTemperaturesWest<<" averageBodyTemperatureWest = "<<averageBodyTemperatureWest<<endl;
         }		 
	 if (gas->getBody2West() >= dbReader->minGasTemperature() && gas->getBody2West()<= dbReader->maxGasTemperature() ) {
		 averageBodyTemperatureWest = averageBodyTemperatureWest + gas->getBody2West();
		 numberBodyTemperaturesWest++;
		 cout<<"gas->getBody2West() = "<<gas->getBody2West()<<" numberBodyTemperaturesWest = "<<numberBodyTemperaturesWest<<" averageBodyTemperatureWest = "<<averageBodyTemperatureWest<<endl;
         }		 
	 if (gas->getBody3West() >= dbReader->minGasTemperature() && gas->getBody3West()<= dbReader->maxGasTemperature() ) {
		 averageBodyTemperatureWest = averageBodyTemperatureWest + gas->getBody3West();
		 numberBodyTemperaturesWest++;
		 cout<<"gas->getBody3West() = "<<gas->getBody3West()<<" numberBodyTemperaturesWest = "<<numberBodyTemperaturesWest<<" averageBodyTemperatureWest = "<<averageBodyTemperatureWest<<endl;
         }		 
	 if (gas->getBody4West() >= dbReader->minGasTemperature() && gas->getBody4West()<= dbReader->maxGasTemperature() ) {
		 averageBodyTemperatureWest = averageBodyTemperatureWest + gas->getBody4West();
		 numberBodyTemperaturesWest++;
		 cout<<"gas->getBody4West() = "<<gas->getBody4West()<<" numberBodyTemperaturesWest = "<<numberBodyTemperaturesWest<<" averageBodyTemperatureWest = "<<averageBodyTemperatureWest<<endl;
         }		 
		 
	 averageBodyTemperatureWest = averageBodyTemperatureWest/numberBodyTemperaturesWest;
	 if (averageBodyTemperatureWest >= dbReader->minGasTemperature() && averageBodyTemperatureWest <= dbReader->maxGasTemperature()) {
            paramReader->setGasTemperatureWest(averageBodyTemperatureWest);
	    cout<<"Set paramReader->setGasTemperatureWest = averageBodyTemperatureWest = "<<averageBodyTemperatureWest<<endl;
         }
	 else if (paramReader->gasTemperatureWest() == 0 ) {
            // initialize FTPC gas temperatures to default values 
	    // if no value found in Calibrations_ftpc/ftpcGasOut for first event
            if ( !daqReader->SVTPresent()) {
               paramReader->setGasTemperatureWest(dbReader->defaultTemperatureWest());
	       cout<<"No valid body temperatures available for FTPC West; Initialize paramReader->gasTemperatureWest() to dbReader->defaultTemperatureWest() = "<<paramReader->gasTemperatureWest()<<" - !daqReader->SVTPresent()"<<endl;
	    }   
            if (daqReader->SVTPresent()) {
               mDbMaker     = (St_db_Maker*)GetMaker("db");
               Int_t dbDate = mDbMaker->GetDateTime().GetDate();
               cout<<"For dbDate = "<<dbDate<<endl;
               if (dbDate < 20021105) { 
	          // for year 2001 data (AuAu,pp) FTPC west gas temperature is higher when SVT on
                  paramReader->setGasTemperatureWest(dbReader->defaultTemperatureWest() + dbReader->temperatureDifference());
	          cout<<"No valid body temperatures available for FTPC West; Initialize paramReader->gasTemperatureWest() to dbReader->defaultTemperatureWest() = "<<paramReader->gasTemperatureWest()<<") + dbReader->temperatureDifference() = "<<dbReader->temperatureDifference()<<" - daqReader->SVTPresent() - year 2001 data"<<endl;
	       }
               if (dbDate >= 20021105) { 
                  paramReader->setGasTemperatureWest(dbReader->defaultTemperatureWest());
	          cout<<"No valid body temperatures available for FTPC West; Initialize paramReader->gasTemperatureWest() to dbReader->defaultTemperatureWest() = "<<paramReader->gasTemperatureWest()<<" - daqReader->SVTPresent() - year 2003 data"<<endl;
	       }
	    }   
	 }	 
         else {
	    cout<<"No valid body temperatures available for FTPC West; leave gasTemperatureWest  = "<<paramReader->gasTemperatureWest()<<endl; 
	 }   

      // calculate average body temperature east

         Int_t numberBodyTemperaturesEast = 0;
         Float_t averageBodyTemperatureEast = 0.0;
	  
	 if (gas->getBody1East() >= dbReader->minGasTemperature() && gas->getBody1East()<= dbReader->maxGasTemperature() ) {
		 averageBodyTemperatureEast = averageBodyTemperatureEast + gas->getBody1East();
		 numberBodyTemperaturesEast++;
		 cout<<"gas->getBody1East() = "<<gas->getBody1East()<<" numberBodyTemperaturesEast = "<<numberBodyTemperaturesEast<<" averageBodyTemperatureEast = "<<averageBodyTemperatureEast<<endl;
         }		 
	 if (gas->getBody2East() >= dbReader->minGasTemperature() && gas->getBody2East()<= dbReader->maxGasTemperature() ) {
		 averageBodyTemperatureEast = averageBodyTemperatureEast + gas->getBody2East();
		 numberBodyTemperaturesEast++;
		 cout<<"gas->getBody2East() = "<<gas->getBody2East()<<" numberBodyTemperaturesEast = "<<numberBodyTemperaturesEast<<" averageBodyTemperatureEast = "<<averageBodyTemperatureEast<<endl;
         }		 
	 if (gas->getBody3East() >= dbReader->minGasTemperature() && gas->getBody3East()<= dbReader->maxGasTemperature() ) {
		 averageBodyTemperatureEast = averageBodyTemperatureEast + gas->getBody3East();
		 numberBodyTemperaturesEast++;
		 cout<<"gas->getBody3East() = "<<gas->getBody3East()<<" numberBodyTemperaturesEast = "<<numberBodyTemperaturesEast<<" averageBodyTemperatureEast = "<<averageBodyTemperatureEast<<endl;
         }		 
	 if (gas->getBody4East() >= dbReader->minGasTemperature() && gas->getBody4East()<= dbReader->maxGasTemperature() ) {
		 averageBodyTemperatureEast = averageBodyTemperatureEast + gas->getBody4East();
		 numberBodyTemperaturesEast++;
		 cout<<"gas->getBody4East() = "<<gas->getBody4East()<<" numberBodyTemperaturesEast = "<<numberBodyTemperaturesEast<<" averageBodyTemperatureEast = "<<averageBodyTemperatureEast<<endl;
         }		 
		 
	 averageBodyTemperatureEast = averageBodyTemperatureEast/numberBodyTemperaturesEast;
	 if (averageBodyTemperatureEast >= dbReader->minGasTemperature() && averageBodyTemperatureEast <= dbReader->maxGasTemperature()) {
             paramReader->setGasTemperatureEast(averageBodyTemperatureEast);
	     cout<<"Set paramReader->setGasTemperatureEast = averageBodyTemperatureEast = "<<averageBodyTemperatureEast<<endl;
         }
	 else if (paramReader->gasTemperatureEast() == 0 ) {
            // initialize FTPC East gas temperature to default value 
	    // if no value found in Calibrations_ftpc/ftpcGasOut for first event
            if ( !daqReader->SVTPresent()) {
               paramReader->setGasTemperatureEast(dbReader->defaultTemperatureEast());
	       cout<<"No valid body temperatures available for FTPC East; Initialize paramReader->gasTemperatureEast() to dbReader->defaultTemperatureEast() = "<<paramReader->gasTemperatureEast()<<" - !daqReader->SVTPresent()"<<endl;
	    }   
            if (daqReader->SVTPresent()) {
               mDbMaker     = (St_db_Maker*)GetMaker("db");
               Int_t dbDate = mDbMaker->GetDateTime().GetDate();
               cout<<"For dbDate = "<<dbDate<<endl;
               if (dbDate < 20021105) { 
                  paramReader->setGasTemperatureEast(dbReader->defaultTemperatureEast());
	          cout<<"No valid body temperatures available for FTPC East; Initialize paramReader->gasTemperatureEast() to dbReader->defaultTemperatureEast() = "<<paramReader->gasTemperatureEast()<<" - daqReader->SVTPresent() - year 2001 data"<<endl;
	       }
               if (dbDate >= 20021105) { 
	          // for year 2003 data (dAu) FTPC east gas temperature is higher when SVT on
                  paramReader->setGasTemperatureEast(dbReader->defaultTemperatureEast() + dbReader->temperatureDifference());
	          cout<<"No valid body temperatures available for FTPC East; Initialize paramReader->gasTemperatureEast() to dbReader->defaultTemperatureEast() = "<<paramReader->gasTemperatureEast()<<" + dbReader->temperatureDifference() = "<<dbReader->temperatureDifference()<<" - daqReader->SVTPresent()  - year 2003 data"<<endl;
	       }
	    }   
	 }	 
         else {
	    cout<<"No valid body temperatures available for FTPC East; leave gasTemperatureEast  = "<<paramReader->gasTemperatureEast()<<endl;
         }	 


      gMessMgr->Message("", "I", "OST") << " Using normalizedNowPressure = "<<paramReader->normalizedNowPressure()<<" gasTemperatureWest = "<<paramReader->gasTemperatureWest()<<" gasTemperatureEast = "<<paramReader->gasTemperatureEast()<<endm; 

       paramReader->setAdjustedAirPressureWest(paramReader->normalizedNowPressure()*((dbReader->baseTemperature()+STP_Temperature)/(paramReader->gasTemperatureWest()+STP_Temperature)));
      gMessMgr->Info() <<" paramReader->setAdjustedAirPressureWest = "<<paramReader->adjustedAirPressureWest()<<endm;
      paramReader->setAdjustedAirPressureEast(paramReader->normalizedNowPressure()*((dbReader->baseTemperature()+STP_Temperature)/(paramReader->gasTemperatureEast()+STP_Temperature)));
     gMessMgr->Info() <<" paramReader->setAdjustedAirPressureEast = "<<paramReader->adjustedAirPressureEast()<<endm;
    }


    //cout <<" create data writer \n";

    // create data writer
    StFtpcRawWriter *dataWriter = new StFtpcRawWriter(fcl_ftpcndx,
						      fcl_ftpcsqndx,
						      fcl_ftpcadc,
						      dbReader->Asic2EastNotInverted());


    //cout <<"Create SlowSimulator \n";

    StFtpcSlowSimulator *slowsim = new StFtpcSlowSimulator(geantReader,
							   paramReader,
                                                           dbReader,
							   dataWriter);


    gMessMgr->Info() << "FTPC SlowSimulator starting... " <<endm;
    Int_t Res_fss = slowsim->simulate();

    delete slowsim;
    delete paramReader;
    delete dbReader;
    delete dataWriter;
    delete geantReader;

    if (Res_fss) {
      if(Debug()) gMessMgr->Message("", "I", "OST") << "finished fss" << endm;
    }
  }
  MakeHistograms(); // FTPC slow simulator histograms

  gMessMgr->Info() << "FTPC SlowSimulator done... " <<endm;
  return kStOK;
}
//_____________________________________________________________________________
void StFtpcSlowSimMaker::MakeHistograms() {

   if(Debug()) gMessMgr->Message("", "I", "OST") << "*** NOW MAKING HISTOGRAMS FOR FtpcSlowSim ***" << endm;

   // Create an iterator
   St_DataSetIter ftpc_raw(m_DataSet);

   //Get the tables
   St_fcl_ftpcadc   *adc = 0;
   St_fcl_ftpcndx   *ndx = 0;
   St_fcl_ftpcsqndx *sqndx = 0;
   adc              = (St_fcl_ftpcadc *) ftpc_raw.Find("fcl_ftpcadc");
   ndx              = (St_fcl_ftpcndx *) ftpc_raw.Find("fcl_ftpcndx");
   sqndx            = (St_fcl_ftpcsqndx *) ftpc_raw.Find("fcl_ftpcsqndx");
   // Fill histograms for FTPC slow simulator
   if (adc) {
     Float_t nadc = adc->GetNRows();
     gMessMgr->Message("", "I", "OST") << "total # adcs = " << adc->GetNRows() << ", nadc = " << nadc << endm;
     m_nadc->Fill(nadc);
   }
   if (ndx) {
     fcl_ftpcndx_st *r = ndx->GetTable();
     Float_t index1 = ++r->index;

     if (adc) {
       m_nadc_index1->Fill((float)adc->GetNRows(),(float)index1); 
     }
   }
   if (sqndx) {
     fcl_ftpcsqndx_st *r = sqndx->GetTable();
     for (Int_t i=0; i<sqndx->GetNRows();i++,r++) {
       m_nsqndx->Fill((float)r->index);
     }
   }
}
//_____________________________________________________________________________

