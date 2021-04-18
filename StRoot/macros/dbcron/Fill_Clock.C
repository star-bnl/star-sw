#include "SqlUtils.h"
#include <iostream.h>
#include <fstream.h>
#include "TSQLServer.h"
#include "TSQLRow.h"
#include "TSQLResult.h"

unsigned int libsLoaded = 0;

void loadLibs(){
  if( !libsLoaded){
      gROOT->Macro("LoadLogger.C");
     gSystem->Load("St_base.so");
	gSystem->Load("libStDb_Tables.so");
	gSystem->Load("StDbLib.so");
	gSystem->Load("libRMySQL.so");
	libsLoaded = 1;
    cout << "Libraries loaded successfully..." << "\n";
  }
    
}

// Writes stuct into database
int write(int nrows,
	  unsigned int runNumber,
	  unsigned int startRunTime,
	  unsigned int* time,
	  double* clock){

  //    cout << " BOOO nRows = " << nrows << endl;
    if(nrows == 0)
	cout << "Run: " << runNumber << " Has No Entries!!! " << endl;

    for(int i = 0; i < nrows; i++){

	cout << "time: " << time[i] << " Clock: " << clock[i] << endl;
    }
    
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("RunLog_onl");
    StDbTable* tdT = node->addDbTable("starClockOnl");
    
    starClockOnl_st* values = new starClockOnl_st[nrows];
    int* iList = new int[nrows];

    for(int i = 0; i < nrows ; i++){
	iList[i] = i;
	values[i].runNumber = runNumber;
	values[i].time = time[i];
	values[i].frequency = clock[i];
    }
    
    tdT->SetTable((char*)values,nrows,iList);
    cout<<"HERE A " <<startRunTime<<endl;
    mgr->setStoreTime(startRunTime);
    int value = mgr->storeDbTable(tdT);
        cout<<" ME would filled clock at "<<mgr->getDateStoreTime()<<" & run=";
        cout<<runNumber<<endl;
    
    return 0;
}



int Fill_Clock(unsigned int runNumber,unsigned int startRunTime,unsigned int endRunTime){

    loadLibs();
    
    int nrows,nfields;
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;
    
    // Get first Entry before 60 seconds into run starts
    db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3502/Conditions_trg","","");
    sprintf(query,
	    "select unix_timestamp(beginTime),frequency from trgClock where beginTime < from_unixtime(%u) order by 1 desc limit 1",startRunTime+61);
    
    res = db->Query(query);
    cout << query <<endl;
    nrows = res->GetRowCount();
    if(nrows == 0){
	cout << "Doesn't make sense" << endl;
	return -1;
    }

    unsigned int firstTime = 0;
    double firstClock = 0;
    
    row = res->Next();
    firstTime = atoul(row->GetField(0));
    firstClock = atod(row->GetField(1));
    
    delete row;
    delete res;

    cout << "First Time = " << firstTime << " First Clock = " << firstClock << endl;
  
    sprintf(query,
	    "select unix_timestamp(beginTime),frequency from trgClock where beginTime between from_unixtime(%u) and from_unixtime(%u)order by 1 limit 1",startRunTime+61,endRunTime);
    
  res = db->Query(query);
  nrows = res->GetRowCount();

  double* clock = new double[nrows + 3];
  unsigned int* time = new unsigned int[nrows + 3];
  double tempClock = 0;
  unsigned int tempTime = 0;
  
  nfields = res->GetFieldCount();

  int i=0;
    
  time[0] = firstTime;
  clock[0] = firstClock;
  int writeFields = 1;
  
  for(i = 0; i < nrows; i++){
      row = res->Next();
      tempTime = atoul(row->GetField(0));
      tempClock = atod(row->GetField(1));
      delete row;

      // Only write current is 5% differnet from previous current
      if( TMath::Abs(tempClock - firstClock) > 5000 ){ // If frequency changes by more than 5 kHz 
	  firstTime = tempTime;
	  firstClock = tempClock;
	  time[writeFields] = tempTime;
	  clock[writeFields] = tempClock;
	  writeFields++;
      }
  }
  
  delete res;

  // Now need to check first value after run and see if consistant with last value, if not add it in:
  sprintf(query,
	  "select unix_timestamp(beginTime),frequency from trgClock where beginTime >= from_unixtime(%u) order by 1 limit 1",endRunTime);

    res = db->Query(query);
    nrows = res->GetRowCount();
    
    if(nrows==0)
      {
	  delete row;
	  delete res;
	  delete db;
	  cout << "No entries after.. Kinda Weird! " << endl;
	  write(writeFields,runNumber,startRunTime,time,clock);
	  return -1;
      }

    row = res->Next();
    tempTime  = atoul(row->GetField(0));      
    tempClock = atod(row->GetField(1));
    delete row;
    delete res;
    
//MPD debug
cout << "TEMP TIME " <<tempTime<<endl;
cout << "TEMP CLOCK " <<tempClock<<endl;
cout << "First CLOCK " <<firstClock<<endl;
cout << "RunNUMBER " <<runNumber<<endl;
cout << "startRunTime " <<startRunTime<<endl;
cout << "Time " <<&time<<endl;
cout << "clock " <<&clock<<endl;
cout << "First CLOCK " <<firstClock<<endl;
cout << "SUBTRACTION =  " <<tempClock - firstClock<<endl;

    if( TMath::Abs(tempClock - firstClock) > 5000 ){
	cout << "Clock Changed After Run" << endl;
	time[writeFields] = tempTime;
	clock[writeFields] = tempClock;
	writeFields++;
    }
	
  // write values in
  int value = write(writeFields,runNumber,startRunTime,time,clock);
      
  delete res; // Delete query and database
  delete db;
  
  //delete clock;
  //delete time;

  return value;

}

// Will Fill Tables for all Runs not filled in yet
void Fill_Clock(){

    loadLibs();
    
    int nrows,nfields;
    unsigned int maxRunNumber;
    unsigned int runNumber;
    unsigned int startRunTime;
    unsigned int endRunTime;
    
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;
    TSQLRow *rowNext = 0;
    
    StDbManager* mgr=StDbManager::Instance();
    mgr->setVerbose(true);
    StDbConfigNode* node = 0;
    node = mgr->initConfig("RunLog_onl");
    if(!node) {
      cout<<" Error getting node "<<endl;
      return;
    }
    StDbTable* table = 0;
    table = node->addDbTable("starClockOnl");
    if(!table){
      cout<<" Error getting table"<<endl; 
      return;
    }

    StDataBaseI* dbI = 0;
    dbI = mgr->findDb("RunLog_onl");
//    if(!dbI->QueryDbFunction(table," where runNumber > 0 AND runNumber < 14999999","max")){
    if(!dbI->QueryDbFunction(table," where runNumber > 0 AND runNumber < 16999999","max")){
      cout<<"Error getting max"<<endl;
      return;
    }

    maxRunNumber=*(unsigned int*)table->getDataValue("runNumber");
    cout<<"Max runnumber="<<maxRunNumber;  
  
    // return;
  // Get DATABASE RUNLOG_rhic
    /*
    db = TSQLServer::Connect("mysql://robinson.star.bnl.gov:3306/RunLog_onl","","");

  
    sprintf(query,"select max(runNumber) from starClockOnl");
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
	maxRunNumber = 2214032;
	cout << "max run = " << maxRunNumber << endl;
    }
    else{
	maxRunNumber = atoul(row->GetField(0));
    }
    
    delete row;
    delete res; // Delete query and database
    delete db;
    */
    // Get DATABASE RUNLOG
    db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/RunLog","","");
    
    //MPD added and startRunTime > 1101242071 since desctiptor was on longer that trgClock
    sprintf(query,"select runNumber,startRunTime,endRunTime from runDescriptor where runNumber > %u and startRunTime > 1391530265 order by runNumber asc",maxRunNumber);
cout<<query<<endl;
    res = db->Query(query);
    
    nrows = res->GetRowCount();
    nfields = res->GetFieldCount();
    
    unsigned int currentTime = (unsigned int)time(NULL);
    // Put Fields into startRunTime and startEndTime
    // Use For loop incase multiple entries, will only use time from last one
    for(int i = 0 ; i < nrows ; i++){
	if (rowNext) {
	  row = rowNext;
	  rowNext = 0;
	} else {
	  row = res->Next();
	}
	runNumber = atoul(row->GetField(0));
	startRunTime = atoul(row->GetField(1));
	endRunTime = atoul(row->GetField(2));
	cout << " runNumber = " << runNumber << endl;
	cout << " startRunTime  = " << startRunTime << endl;
	cout << " endRunTime = " << endRunTime << endl;
	//MPD fixing -1 entries in offline db
	if (!endRunTime)
	  {
	    //GVB give up on runs 5 minutes after another run starts
	    //  Can't do if it's the latest run
	    bool skipit = (nrows != (i+1));
	    if (skipit)
	      {
	        // Get the next run's startRunTime
	        rowNext = res->Next();
	        skipit = (currentTime-atoul(rowNext->GetField(1)) > 300);
	      }
	    if (skipit)
	      {
	        cout << "run has no end time after next run started...skipping it" << endl;
	      } else {
	        cout << "run not finished ..gonna bail for now"<<endl;
	        return -1;
	      }
	  } else {
	    Fill_Clock(runNumber,startRunTime,endRunTime);
	  }
	delete row;
    }
    
    
    delete res; // Delete query and database
    delete db;
    
}
