// writes  eemcHVsys to online-DB
//
#include "StDbManager.hh"
#include "StDbManager.hh"
#include "StDbConfigNode.hh"
#include "StDbTable.h"
#include "StDbXmlReader.h"
#include "StDbXmlWriter.h"
#include "StDbElementIndex.hh"
 
#include <time.h>  
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "kretDbBlobS.hh"
//---------------------------------------------------------------------
//---------------------------------------------------------------------
//---------------------------------------------------------------------
int main() {
  //VerA/online/ETOW

  StDbManager* mgr = StDbManager::Instance();  //addDbType, addDbDomain  
  StDbConfigNode* nodeHead = mgr->initConfig(dbConditions,dbEemc,"VerA");
  assert(nodeHead);  printf("node found 1\n");

  StDbConfigNode* nodeOnl = nodeHead->findConfigNode("online");
  assert(nodeOnl);  printf("node found 2\n");

  printf("access DB table  ...");
  StDbTable * tab;
  tab = nodeOnl->addDbTable("ETOW");
  int nrows;
  int* elist;
  if(tab->GetNRows()<=0) {
    printf("JB: FAILED : tab->GetNRows()=%d\n",tab->GetNRows());    return 0;
  }
  elist = tab->getElementID(nrows);
  printf("nrows=%d, elis=%d\n", nrows,*elist);
  

  // fill something in c-structs
  kretDbBlobS blob;
  strncpy(blob.comment,"comJ1 AA",KRETDbMaxComment);
  strncpy(blob.dataS,"bodyJ1 AA",KRETmxBlobSlen);

  tab->SetTable((char*)&blob,1);

  unsigned int tStore=(unsigned int) time(0);
  
  printf("JB: time    Store=%s",ctime((const time_t *)&tStore)); 
  mgr->setStoreTime(tStore);
  
  // printf("JB:  WARN -  ENTERING BLOCKED\n");   return 1;
  //   if(! mgr->storeAllTables(nodeY)) // not working ???

  printf("write DB table   ...");
  int ret= mgr->storeDbTable(tab);
  if( ret){
    printf("JB:  storeTables() done\n");
  } else {
    printf("JB:  Some problems with storeDbTable() ???\n"   );
    return 1;
  }

  return 0; 
} 

