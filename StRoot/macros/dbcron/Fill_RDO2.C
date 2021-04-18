#include "SqlUtils.h"
#include <iostream.h>
#include <fstream.h>
#include "TSQLServer.h"
#include "TSQLRow.h"
#include "TSQLResult.h"

unsigned int nbins;
unsigned long * unixTime = 0;
unsigned long * zdcX = 0;
unsigned long * zdcXError = 0;

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
	  unsigned int* sector,
	  unsigned int* mask){

    
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("RunLog_onl");
    StDbTable* tdT = node->addDbTable("tpcRDOMasks");
    
    tpcRDOMasks_st* values = new tpcRDOMasks_st[nrows];
    int* iList = new int[nrows];
    int map[12];
    for(int i = 0; i < 12 ; i++) map[i]=-1;
    int nrows2=0;

    // GVB: must try to pair them since
    // online DB stopped doing so for Run 9
    for(int i = 0; i < nrows ; i++){
      int sect = sector[i]; //1-24
      if (sect>24) continue; //nonsensical data
      int sectID = (sect-1)/2; //0-11
      unsigned int maski = ( sect%2 ? mask[i] : mask[i]<<(runNumber>19300000 ? 8 : 6));
      if (map[sectID]<0) {
        map[sectID] = nrows2;
        values[nrows2].runNumber = runNumber;
        values[nrows2].sector = sectID*2+1; //1-23
        values[nrows2].mask = maski;
	iList[nrows2] = sectID;
        nrows2++;
      } else {
        values[map[sectID]].mask = values[map[sectID]].mask | maski;
      }
    }
    
    tdT->SetTable((char*)values,nrows2,iList);
    //mgr->setStoreTime(915148801);
    mgr->setStoreTime(beginTime);
    if (0) { // testing
      ofstream tabOut(Form("tpcRDOMasks.%u.C",beginTime));
      St_tpcRDOMasks rdoMasks("tpcRDOMasks",nrows2);
      memcpy(rdoMasks.GetTable(),values,nrows2*sizeof(tpcRDOMasks_st));
      rdoMasks.SetNRows(nrows2);
      rdoMasks.SavePrimitive(tabOut,"");
    } else {
      int value = mgr->storeDbTable(tdT);
    }
    
    return 0;
}

int Fill_RDO2(unsigned int runNumber,unsigned int startRunTime){

    loadLibs();
    
    int nrows,nrows2,nfields,nfields2;
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;

//    cout << "runNumber2 = " << runNumber << endl;
  // Get DATABASE Condtions_rhic,
  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/Conditions_rts","","");
  
  // tpx is detector 20 (no more tpc => idx_det=0 as of Run 9)
  sprintf(query,
	  "select idx_subdet,rb_mask from subdets where idx_det=20 and idx_rn = %u",runNumber);
  res = db->Query(query);
  nrows = res->GetRowCount();
  //cout << "Retrieved " << nrows << " masks \n";
  
  unsigned int runDay = runNumber / 1000;
    
  if(nrows==0)
      {
	  delete res;
	  delete db;
	  return -1;
      }

  nfields = res->GetFieldCount();

  unsigned int* sector = new unsigned int[nrows];
  unsigned int* mask = new unsigned int[nrows];
  
  for(unsigned int i = 0; i < nrows; i++){
      row = res->Next();
      sector[i] = atoul(row->GetField(0));
      mask[i] = atoul(row->GetField(1));      
      // Mask out RDO 13-9 (or 14-3) 
      if(runNumber >= 2365020 && runNumber <= 2365032 && sector[i] == 13)
	  mask[i] &= 0xEFF;
      
      // Mask out RDO 17-11 (or 18-5) 
      if(runNumber >= 2346048 && runNumber <= 2351030 && sector[i] == 17)
	  mask[i] &= 0xBFF;
      
      // Mask out RDO 5-4 
      if(runNumber >= 2278007 && runNumber <= 2278045 && sector[i] == 5)
	  mask[i] &= 0xFF7;
      
      // Mask out RDO 9-4 
      if(runDay >= 2254 && runDay <= 2330 && sector[i] == 9)
	  mask[i] &= 0xFF7;
      
      // Mask out RDO 20-3
      if(runDay >= 2350 && runDay <= 5000 && sector[i] == 19)
	  mask[i] &= 0xEFF;
            
      delete row;
  }

  if(runNumber>19300000) {
    delete res; // Delete first query
    sprintf(query,
	  "select idx_subdet,rb_mask from subdets where idx_det=31 and idx_rn = %u",runNumber);
    res = db->Query(query);
    nrows2 = res->GetRowCount();
    if (nrows2 == 0) {
      // default to all iTPC RDOs active if no entries exist for iTPC
      for (unsigned int i = 0; i < nrows; i++) {
          mask[i] = (mask[i] << 2) | 0xF;
      }
    } else {
      // default to each iTPC RDO inactive if no entry exists for that sector
      for (unsigned int i = 0; i < nrows; i++) {
          mask[i] = (mask[i] << 2) & 0xF0;
      }
      nfields2 = res->GetFieldCount();
      for(unsigned int j = 0; j < nrows2; j++){
        row = res->Next();
        unsigned int sectorj = atoul(row->GetField(0));
        unsigned int maskj = atoul(row->GetField(1));      
        for (unsigned int i = 0; i < nrows; i++) {
          if (sectorj == sector[i]) {
            // bring in the new 4 bits from iTPC
            mask[i] |= maskj;
            break;
          }
          // NB: if a sector has an entry for det=31 but not for det=20, it will be skipped
        }
        delete row;
      }
    }
  }
  

  cout << "Prepare to write " << runNumber << "\n";
  int value = write(nrows,runNumber,startRunTime,sector,mask);

  delete sector;
  delete mask;
  
  delete res; // Delete query and database
  delete db;
  return value;

}

// Will Fill Tables for all Runs not filled in yet
void Fill_RDO2(){

    loadLibs();
    
    int nrows,nfields;
    unsigned int maxRunNumber;
    unsigned int runNumber;
    unsigned int startRunTime;
    
    char query[600];
    TSQLServer *db = 0;
    TSQLResult *res = 0;
    TSQLRow *row = 0;
    
  
  ///
  // Get DATABASE RUNLOG_rhic
  db = TSQLServer::Connect("mysql://robinson.star.bnl.gov:3306/RunLog_onl","","");

  
  sprintf(query,"select max(runNumber) from tpcRDOMasks");
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

printf("max %d\n",maxRunNumber); 
//  cout << "max runNumber is " << maxRunNumber << endl;
   // Get DATABASE RUNLOG
  db = TSQLServer::Connect("mysql://heston.star.bnl.gov:3501/RunLog","","");

  sprintf(query,"select runNumber,startRunTime from runDescriptor where runNumber > %u order by 1 asc",maxRunNumber);
  cout << "Query: " << query << endl;
  res = db->Query(query);

  nrows = res->GetRowCount();
  nfields = res->GetFieldCount();

  cout << "nRows = " << nrows << endl;
  // Put Fields into startRunTime and startEndTime
  // Use For loop incase multiple entries, will only use time from last one
  for(int i = 0 ; i < nrows ; i++){
      row = res->Next();
      runNumber = atoul(row->GetField(0));
      startRunTime = atoul(row->GetField(1));
      cout << "runNumber = " << runNumber << endl;
      Fill_RDO2(runNumber,startRunTime);
      delete row;
  }
  
  
  delete res; // Delete query and database
  delete db;
    
}
