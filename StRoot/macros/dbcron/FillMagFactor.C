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
  }
    
}


void write(int nrows, void* data, unsigned int* times){

  MagFactor_st* gvalues=(MagFactor_st*)data;
  

  cout<<" ****** Filling "<<nrows<<" rows of data ****** "<<endl;
  for(int i=0;i<nrows;i++){

    StDbManager* mgr=StDbManager::Instance();
    StDbConfigNode* node=mgr->initConfig("RunLog");
    StDbTable* table = node->addDbTable("MagFactor");

    mgr->setStoreTime(times[i]);
    table->SetTable((char*)&gvalues[i],1);
   if(!mgr->storeDbTable(table)){
      cout<<"Error Storing table for Time="<<mgr->getDateStoreTime()<<endl;
      return;
     }

    delete node;
  }

}


int Propogate(){


  char query[256];
  int numRowsPerCall=24;
  unsigned int timeStep=1200; // 20 minute averages
  unsigned int currentTime=(unsigned int)time(NULL);
//  unsigned int startRecordTime=1072656000;// Dec 28, 2003 rhic feed is on
//  unsigned int startRecordTime=1387227300; // Dec 16 2012
//  unsigned int startRecordTime=1518825600; // Feb 17 2018
//  unsigned int startRecordTime=1547845260; // Jan 18 2019
//    unsigned int startRecordTime=1574104214; // Nov 18 2019, Run 20 early cosmics
//    unsigned int startRecordTime=1592277498; // post-covid
    unsigned int startRecordTime=1611859309; // Run 21, when magnet PV became available
  
  StDbManager* mgr=StDbManager::Instance();

  //
  //-> get timestamp for last entry int offline db
  //
  StDbConfigNode* offlNode=mgr->initConfig("RunLog");
  if(!offlNode) return 10;
  StDbTable* offlT=offlNode->addDbTable("MagFactor"); if(!offlT) return 12;
  mgr->setRequestTime(time(NULL));
  unsigned int beginTime=0;
  if(mgr->fetchDbTable(offlT)) beginTime=offlT->getBeginTime();

  if(currentTime-beginTime<timeStep)return -1;
  if(beginTime<startRecordTime)beginTime=startRecordTime; 

  // 
  //-> connect  get table schema to/from onl db
  //
  StDbConfigNode* onlNode= mgr->initConfig("Conditions_rhic");
  if(!onlNode) return 16;
  StDbTable* onlT=onlNode->addDbTable("starMagnet");
  if(!onlT) return 18;

  // prepare memory for storing data
  MagFactor_st* offlfield = new MagFactor_st[numRowsPerCall]; 
  memset(offlfield,0,numRowsPerCall*sizeof(MagFactor_st));
  int row=0;
  unsigned int* bTimes = new unsigned int[numRowsPerCall];
  memset(bTimes,0,numRowsPerCall*sizeof(unsigned int));

  // db helper code special to this type of process
  StDataBaseI* dbI=mgr->findDb("Conditions_rhic"); if(!dbI) return 20;

  bool done=false;
  int signMask=1<<15;
  while(!done){

    unsigned int endTime=beginTime+timeStep; // 20 minutes
    sprintf(query," where beginTime>from_unixtime(%u) and beginTime<=from_unixtime(%u) ",beginTime,endTime);

    unsigned int* tmpTimes=0;
    tmpTimes=dbI->QueryDbTimes(onlT,query);

    if(tmpTimes){
      
      int tmpRows=onlT->GetNRows();
      int irow;
      if(row+tmpRows>numRowsPerCall)tmpRows=numRowsPerCall-row;

      for(int irow=0;irow<tmpRows;irow++){

        float current = *(float*)onlT->getDataValue("mainMagnetCurrent",irow);
	int mstatus   = *(int*)onlT->getDataValue("mainMagnetStatus",irow);
        float signoffield=1.0;
        if(mstatus & signMask) signoffield=-1.0;
        offlfield[row+irow].ScaleFactor=signoffield*floor(1000*(fabs(current)/4500.0))/1000.0;
        bTimes[row+irow]=tmpTimes[irow];

      }

      row+=tmpRows;

    }
    beginTime=endTime;
    if( (row>=numRowsPerCall) || (beginTime+timeStep>currentTime)) done=true;

  }

  write(row,offlfield,bTimes);

  delete offlNode;
  delete onlNode;

  return 0;
};
  

void FillMagFactor(){

  loadLibs();

  int retVal=Propogate();

    if(retVal==-1){
      cout<<"Result:: Nothing to be Done"<<endl;
    } else if(retVal==0){
      cout<<"Result:: Success"<<endl;
    } else if(retVal<15){
      cout<<"Result:: Error accessing offl db data"<<endl;
    } else if(retVal<21){
      cout<<"Result:: Error accessing onl db data"<<endl;
    }

};






