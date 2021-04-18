#include "SqlUtils.h"
#include <iostream.h>
#include <fstream.h>
#include "TSQLServer.h"
#include "TSQLRow.h"
#include "TSQLResult.h"
//#include "trigDetSums.h"


unsigned int libsLoaded = 0;

void loadLibs(){
  if( !libsLoaded){
	gROOT->Macro("LoadLogger.C");
         gSystem->Load("St_base.so");
	gSystem->Load("libStDb_Tables.so");
	gSystem->Load("StDbLib.so");
	gSystem->Load("libRMySQL.so");
	libsLoaded = 1;
  }
    
}

// Writes stuct into database
int write(int nrows,
	  unsigned int runNumber,
	  unsigned int beginTime,
	  unsigned long* timeOffset,
	  double* ctbWest,
	  double* ctbEast,
	  double* TOFp,
	  double* ctbTOFp,
	  double* zdcWest,
	  double* zdcEast,
	  double* zdcX,
	  double* mult,
	  double* L0,
	  double* bbcX,
	  double* bbcXctbTOFp,
	  double* bbcWest,
	  double* bbcEast,
	  double* bbcYellowBkg,
	  double* bbcBlueBkg,
	  double* pvpdWest,
	  double* pvpdEast){

  for(int i = 0; i < nrows ; i++){
    
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_rich");
    StDbTable* tdT = node->addDbTable("trigDetSums");
    
    
    trigDetSums_st values;
    values.runNumber = runNumber;
    values.timeOffset = timeOffset[i];
    values.ctbWest = ctbWest[i];
    values.ctbEast = ctbEast[i];
    values.ctbTOFp = ctbTOFp[i];
    values.tofp = TOFp[i];
    values.ctbTOFp = ctbTOFp[i];
    values.zdcWest = zdcWest[i];
    values.zdcEast = zdcEast[i];
    values.zdcX = zdcX[i];
    values.mult = mult[i];
    values.bbcX = bbcX[i];
    values.bbcXctbTOFp = bbcXctbTOFp[i];
    values.L0 = L0[i];
    values.bbcWest = bbcWest[i];
    values.bbcEast = bbcEast[i];
    values.bbcYellowBkg = bbcYellowBkg[i];
    values.bbcBlueBkg = bbcBlueBkg[i];
    values.pvpdWest = pvpdWest[i];
    values.pvpdEast = pvpdEast[i];
    
    //cout << "**** Row " << i << endl;
    cout << "runNumber " << values.runNumber 
	 << " timeOffset = " << values.timeOffset << endl;
    /*
    cout << "bbcEast = " << values.bbcEast 
	 << " bbcWest = " << values.bbcWest 
	 << " bbcX = " << values.bbcX             << endl;

	cout << "bbcBlueBkg = " << values.bbcBlueBkg     
	 << " bbcYellowBkg = " << values.bbcYellowBkg  << endl;

	cout << " zdcEast = " << values.zdcEast  
	 << " zdcWest = " << values.zdcWest  
	 << " zdcX = " << values.zdcX             <<  endl;

	cout << "pvpdEast = " << values.pvpdEast  
	 << " pvpdWest = " << values.pvpdWest  
	 << " All others = "  << values.ctbEast << " "  
	 << values.tofp  << " "  
	 << values.ctbTOFp << " "  
	 << values.mult  << " "   
	 << values.bbcXctbTOFp << " "   
	 << values.L0 << endl << endl;  
    */
    
    tdT->SetTable((char*)&values,1);
    mgr->setStoreTime(timeOffset[i]);
    mgr->storeDbTable(tdT);
    
    
  }
  return 0;
}

int FillTD(unsigned int runNumber,unsigned int startRunTime,unsigned int endRunTime ){

    unsigned int maxStartTime;
    loadLibs();
    
    int nrows,nfields;
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;
    
  // Get DATABASE Conditions_rich,
  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3502/Conditions_rich","","");
 cout<<endl<<startRunTime <<"STARTIME***"<<endl; 
 cout<<endRunTime <<"ENDTIME***"<<endl<<endl; 
  sprintf(query,
    "select UNIX_TIMESTAMP(beginTime),rs1,rs2,rs3,rs4,rs5,rs6,rs7,rs8,rs9,rs10,rs16 from richScalar where beginTime > from_unixtime(%u) and beginTime < from_unixtime(%u) order by 1 asc",startRunTime,endRunTime);

  res = db->Query(query);
  nrows = res->GetRowCount() + 1;
  if(nrows==0)
      {
	  delete row;
	  delete res;
	  delete db;
	  return -1;
      }
  
  nfields = res->GetFieldCount();

  int i;
  unsigned long* timeOffset = new unsigned long[nrows];
  double* ctbWest = new double[nrows];
  double* ctbEast = new double[nrows];
  double* ctbTOFp = new double[nrows];
  double* TOFp = new double[nrows];
  double* zdcWest = new double[nrows];
  double* zdcEast = new double[nrows];
  double* zdcX = new double[nrows];
  double* bbcX = new double[nrows];
  double* bbcXctbTOFp = new double[nrows];
  double* mult = new double[nrows];
  double* L0 = new double[nrows];
// six more
  double* bbcWest = new double[nrows];
  double* bbcEast = new double[nrows];
  double* bbcYellowBkg = new double[nrows];
  double* bbcBlueBkg = new double[nrows];
  double* pvpdWest = new double[nrows];
  double* pvpdEast = new double[nrows];

  //cout << endl << " ---------- This run: nrow  = " << nrows << endl;
  for(i = 1; i < nrows; i++){
	  row = res->Next();
	  timeOffset[i] = atoul(row->GetField(0));
	  bbcEast[i] = atod(row->GetField(1));
	  bbcWest[i] = atod(row->GetField(2));
	  bbcX[i] = atod(row->GetField(3));
	  bbcYellowBkg[i] = atod(row->GetField(4));
	  bbcBlueBkg[i] = atod(row->GetField(5));
	  zdcEast[i] = atod(row->GetField(6));
	  zdcWest[i] = atod(row->GetField(7));
	  zdcX[i] = atod(row->GetField(8));
	  pvpdEast[i] = atod(row->GetField(9));
	  pvpdWest[i] = atod(row->GetField(10));
	  ctbWest[i] = 0.0;
	  ctbEast[i] = 0.0;
	  TOFp[i] = 0.0;
	  ctbTOFp[i] = 0.0;
	  mult[i] = atod(row->GetField(11));
	  L0[i] = 0.0;
	  bbcXctbTOFp[i] = 0.0;
	  delete row;
  }
 delete res;

  /// Now need to get previus value before run
 
 sprintf(query,
 "select UNIX_TIMESTAMP(beginTime),rs1,rs2,rs3,rs4,rs5,rs6,rs7,rs8,rs9,rs10,rs16 from richScalar where beginTime < from_unixtime(%u) order by 1 desc limit 1 ",startRunTime);
cout<<"QUERY IS *^*:"<<query<<endl;
 res = db->Query(query);
     
     row = res->Next();
     if (!row) { cout<< "NO record returned"<<endl; return; }
     timeOffset[0] = atoul(row->GetField(0));
     bbcEast[0] = atod(row->GetField(1));
     bbcWest[0] = atod(row->GetField(2));
     bbcX[0] = atod(row->GetField(3));
     bbcYellowBkg[0] = atod(row->GetField(4));
     bbcBlueBkg[0] = atod(row->GetField(5));
     zdcEast[0] = atod(row->GetField(6));
     zdcWest[0] = atod(row->GetField(7));
     zdcX[0] = atod(row->GetField(8));
     pvpdEast[0] = atod(row->GetField(9));
     pvpdWest[0] = atod(row->GetField(10));
     ctbWest[0] = 0.0;
     ctbEast[0] = 0.0;
     TOFp[0] = 0.0;
     ctbTOFp[0] = 0.0;
     mult[0] = atod(row->GetField(11));
     L0[0] = 0.0;
     bbcXctbTOFp[0] = 0.0;
     delete row;

	///      

  int value = write(nrows,runNumber,startRunTime,timeOffset,ctbWest,ctbEast,TOFp,ctbTOFp,zdcWest,zdcEast,zdcX,mult,L0,bbcX,bbcXctbTOFp,bbcWest,bbcEast,bbcYellowBkg,bbcBlueBkg,pvpdWest,pvpdEast);

  delete timeOffset;
  delete ctbWest;
  delete ctbEast;
  delete ctbTOFp;
  delete TOFp;
  delete zdcWest;
  delete zdcEast;
  delete zdcX;
  delete bbcX;
  delete bbcXctbTOFp;
  delete mult;
  delete L0;
  delete bbcWest;
  delete bbcEast;
  delete bbcYellowBkg;
  delete bbcBlueBkg;
  delete pvpdWest;
  delete pvpdEast;
  
  delete res; // Delete query and database
  delete db;
  return value;

}

// Will Fill Tables for all Runs not filled in yet
void FillTD(){

    loadLibs();
    
    int nrows,nfields;
    unsigned int maxRunNumber;
    unsigned int runNumber,startRunTime,endRunTime;
    
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;
    
  
  ///
  // Get DATABASE Calibrations_rich 

  db = TSQLServer::Connect("mysql://robinson.star.bnl.gov:3306/Calibrations_rich","","");
//  db = TSQLServer::Connect("mysql://onlsun1.starp.bnl.gov:3309/Calibrations_rich","","");
// Get Next To Last Run Number (want to finish up current run if ended early.
  sprintf(query,"select runNumber from trigDetSums order by 1 desc limit 2,1");
cout<<"QUERY IS *^*: "<<query<<endl;
  res = db->Query(query);
  // Get Rows and Fields, should only have 1 row and two fields
  nrows = res->GetRowCount();
  nfields = res->GetFieldCount();

  if(nrows==0){
      cout << "oops" << endl;
  }
  // Put Fields into startRunTime and startEndTime
  // Use For loop incase multiple entries, will only use time from last one

  row = res->Next();
  
  sprintf(query,"%s",row->GetField(0));

  if(query[0] == '\0' ){
      maxRunNumber = 22026001;
      cout << "max run = " << maxRunNumber << endl;
  }
  else{
      maxRunNumber = atoul(row->GetField(0));
  }
  
  delete row;
  delete res; // Delete query and database
  delete db;

  if ( maxRunNumber < 22026001 ) { maxRunNumber = 22026001; }
  
  cout << "max runNumber is " << maxRunNumber << endl <<endl;

   // Get DATABASE RUNLOG
  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/RunLog","","");

// skip early runs with junk scalars by requiring dabaID > 250 (before 1/8)
//  sprintf(query,"select runNumber,startRunTime,endRunTime from runDescriptor where runNumber > %u and dataID > 14824 order by 1",
// skip early runs without scalar data  bwteen 1/9 and 1/11 
//  sprintf(query,"select runNumber,startRunTime,endRunTime from runDescriptor where runNumber > 8059018 order by 1");
  sprintf(query,"select runNumber,startRunTime,endRunTime from runDescriptor where runNumber > %u and dataID > 61736 order by 1",
  maxRunNumber);
cout<<"QUERY IS *^*: "<<query<<endl;
  res = db->Query(query);

  nrows = res->GetRowCount();
cout << "Row # is " << nrows <<endl<<endl;
  nfields = res->GetFieldCount();
cout << "Field Count is " << nfields <<endl<<endl;
  
  // Put Fields into startRunTime and startEndTime
  // Use For loop incase multiple entries, will only use time from last one
  for(int i = 0 ; i < nrows ; i++){
      row = res->Next();
      runNumber = atoul(row->GetField(0));
   	  cout << "Run Number is " << runNumber << endl;
      startRunTime = atoul(row->GetField(1));
      endRunTime = atoul(row->GetField(2));
      FillTD(runNumber,startRunTime,endRunTime);
      delete row;
  }
  
  
  delete res; // Delete query and database
  delete db;
    
}
