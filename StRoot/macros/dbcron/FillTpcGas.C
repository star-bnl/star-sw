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

  tpcGas_st* gasvalues=(tpcGas_st*)data;
  

  cout<<" ****** Filling "<<nrows<<" rows of data ****** "<<endl;
  for(int i=0;i<nrows;i++){

    StDbManager* mgr=StDbManager::Instance();
    StDbConfigNode* node=mgr->initConfig("Calibrations_tpc");
    StDbTable* table = node->addDbTable("tpcGas");

    mgr->setStoreTime(times[i]);
    table->SetTable((char*)&gasvalues[i],1);
    if(!mgr->storeDbTable(table)){
      cout<<"Error Storeing table for Time="<<mgr->getDateStoreTime()<<endl;
      return;
     }
    delete node;
  }

}


int PropogateGas(){


  char query[256];
  int numRowsPerCall=24;
  unsigned int timeStep=1200; // 20 minute averages
  unsigned int currentTime=(unsigned int)time(NULL);
  //unsigned int startGas=1072569600;// Jan 12, 2003 SC gas system works!
 // unsigned int startGas=1195855500;// Run 6 start up
//  unsigned int startGas=1293840000;// Run 11 start up
//  unsigned int startGas=1327449600;
//  unsigned int startGas=1388534400; // Run 14
//  unsigned int startGas=1518825600; // Run 18
//  unsigned int startGas=1547845260; // Run 19
//  unsigned int startGas=1573862400; // Run 20
//  unsigned int startGas=1592277498; // Run 20 post-covid
  unsigned int startGas=1611637812; // Run 21

  StDbManager* mgr=StDbManager::Instance();

  //
  //-> get timestamp for last entry int offline db
  //
  StDbConfigNode* offlNode=mgr->initConfig("Calibrations_tpc");
  if(!offlNode) return 10;
  StDbTable* offlT=offlNode->addDbTable("tpcGas"); if(!offlT) return 12;
  mgr->setRequestTime(time(NULL));
  if(!mgr->fetchDbTable(offlT)) return 14;

  unsigned int beginTime=offlT->getBeginTime();
//cout<<" beginTime = "<<beginTime<<endl;
  if(currentTime-beginTime<timeStep)return -1;
  if(beginTime<startGas)beginTime=startGas; 

  // 
  //-> connect  get table schema to/from onl db
  //
  StDbConfigNode* onlNode= mgr->initConfig("Conditions_tpc");
  if(!onlNode) return 16;
  StDbTable* onlT=onlNode->addDbTable("tpcGas");
  if(!onlT) return 18;

  // prepare memory for storing data
  tpcGas_st* offlgas = new tpcGas_st[numRowsPerCall]; 
  memset(offlgas,0,numRowsPerCall*sizeof(tpcGas_st));
  int row=0;
  unsigned int* bTimes = new unsigned int[numRowsPerCall];
  memset(bTimes,0,numRowsPerCall*sizeof(unsigned int));

  // db helper code special to this type of process
  StDataBaseI* dbI=mgr->findDb("Conditions_tpc"); if(!dbI) return 20;


  bool done=false;
  while(!done){

    unsigned int endTime=beginTime+timeStep; // 20 minutes
    
    sprintf(query," where beginTime>from_unixtime(%u) and beginTime<=from_unixtime(%u)and PT_B>900",beginTime,endTime);
//cout<<"QUERY :: "<<query<<endl;
//int zz; cin >>zz;
    bTimes[row]=(beginTime+endTime)/2;

    if( !dbI->QueryDbFunction(onlT,query,"AVG")) return 16;
assert(onlT);
      offlgas[row].barometricPressure   =*(float*)onlT->getDataValue("PT_B");
      offlgas[row].inputTPCGasPressure  =*(float*)onlT->getDataValue("PT_8");
      offlgas[row].nitrogenPressure     =*(float*)onlT->getDataValue("PI_15");
      offlgas[row].gasPressureDiff      =*(float*)onlT->getDataValue("PT_7");
      offlgas[row].inputGasTemperature  =*(float*)onlT->getDataValue("T4");
      offlgas[row].outputGasTemperature =*(float*)onlT->getDataValue("T5");
      offlgas[row].flowRateArgon1       =*(float*)onlT->getDataValue("FM_5");
      offlgas[row].flowRateArgon2       =*(float*)onlT->getDataValue("FM_4");
      offlgas[row].flowRateMethane      =*(float*)onlT->getDataValue("FM_1");
      offlgas[row].percentMethaneIn     =*(float*)onlT->getDataValue("CH4_M4");
      offlgas[row].ppmOxygenIn          =*(float*)onlT->getDataValue("O2_M1");
      offlgas[row].flowRateExhaust      =*(float*)onlT->getDataValue("PT_11");
      offlgas[row].percentMethaneOut    =*(float*)onlT->getDataValue("CH4_M3");
      offlgas[row].ppmWaterOut          =*(float*)onlT->getDataValue("H2O_M2");
      offlgas[row].ppmOxygenOut         =*(float*)onlT->getDataValue("O2_M5");
      offlgas[row].flowRateRecirculation=*(float*)onlT->getDataValue("FI_7");
    
      if(offlgas[row].barometricPressure==0){
	beginTime+=300; // skip ahead 5 minuts
      } else {
        beginTime=endTime;
        row++;
        if( (row==numRowsPerCall) || (beginTime+timeStep>currentTime)) done=true;
      }
  }

  write(row,offlgas,bTimes);

  delete offlNode;
  delete onlNode;

  return 0;
};
  

void FillTpcGas(){

  loadLibs();

  int retVal=PropogateGas();

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






