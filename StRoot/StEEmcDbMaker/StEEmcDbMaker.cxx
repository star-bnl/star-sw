// *-- Author : Jan Balewski
// 
// $Id: StEEmcDbMaker.cxx,v 1.2 2003/02/18 19:55:53 balewski Exp $
// $Log: StEEmcDbMaker.cxx,v $
// Revision 1.2  2003/02/18 19:55:53  balewski
// add pedestals
//
// Revision 1.1  2003/01/28 23:18:34  balewski
// start
//
// Revision 1.5  2003/01/06 17:09:21  balewski
// DB-fix
//
// Revision 1.4  2003/01/03 23:37:56  balewski
// move to robinson
//
// Revision 1.3  2002/12/05 14:22:24  balewski
// cleanup, time stamp fixed
//
// Revision 1.2  2002/12/04 13:39:04  balewski
// remove dependency on dbase/
//
// Revision 1.1  2002/11/30 20:01:26  balewski
// start DB interface for EEMC RELATED ROUTINES
//
 
#include <TDatime.h>
#include <time.h>

#include "StChain.h"
#include "St_DataSetIter.h"
#include "StDAQMaker/StDAQReader.h"

#include "St_db_Maker/St_db_Maker.h" // tmp to ovveride time stamp

#include "StEventTypes.h"

#include "StEEmcDbMaker.h"

#include "StEEmcDbIndexItem1.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h" 

#include "tables/St_eemcDbADCconf_Table.h"
#include "tables/St_eemcDbPMTconf_Table.h"
#include "tables/St_eemcDbPMTcal_Table.h"
#include "cstructs/eemcConstDB.hh"


ClassImp(StEEmcDbMaker)

//_________________________________________________________
//________________________________________________________
//________________________________________________________
StEEmcDbMaker::StEEmcDbMaker(const char *name):StMaker(name){
  printf("\n Constructor :::::: %s\n",GetName());
  mfirstSecID=mlastSecID=mNSector=0;
  myTimeStampDay=0;
  myTimeStampUnix=0;
  mDbItem1=new  StEEmcDbIndexItem1[EEindexMax];

}


//________________________________________________________
//________________________________________________________
//_______________________________________________________
StEEmcDbMaker::~StEEmcDbMaker(){
  delete [] mDbItem1;

  if( mNSector) {
    delete [] mDbADCconf;
    delete [] mDbPMTconf;
    delete [] mDbPMTcal;
    delete [] mDbsectorID;
  }
}

//________________________________________________________
//________________________________________________________
//________________________________________________________
void  StEEmcDbMaker::setTimeStampDay( int tD) {

  if(myTimeStampDay) {
    printf("Logical error:  redeclaration of %s::myTimeStampDay =%d,STOP\n",GetName(),myTimeStampDay);
    exit(1);
  }
  myTimeStampDay=tD;

  int iy=tD/10000; tD%=10000;
  int im=tD/100;  tD%=100;
  int id=tD;
  printf("%d %d %d\n",iy,im,id);

  // build Unix time stamp 
  struct tm t;
  memset(&t,0,sizeof t);
  t.tm_sec=0;    /* seconds after the minute - [0,61] */
  t.tm_min=0;      /* minutes after the hour - [0,59] */
  t.tm_hour=0;      /* hours - [0,23] */
  t.tm_mday=id;     /* day of month - [1,31] */
  t.tm_mon=im-1;      /* month of year - [0,11] */
  t.tm_year=iy-1900;     /* years since 1900 */
  t.tm_isdst=0;    /* daylight savings time flag */  
  //int tm_wday;     /* days since Sunday - [0,6] */
  //int tm_yday;     /* days since January 1 - [0,365] */
  myTimeStampUnix =(unsigned int) mktime(&t);

  printf("%s::myTimeStampDay set to %d\n",GetName(),myTimeStampDay);
  printf("Unix Time stamp=%s",ctime((const time_t *)&myTimeStampUnix));


}

//________________________________________________________
//________________________________________________________
//________________________________________________________
Int_t StEEmcDbMaker::Init(){
  if( mNSector==0) setSectors(5,8);//default
  return StMaker::Init();
}


//________________________________________________________
//________________________________________________________
//________________________________________________________
void StEEmcDbMaker::setSectors(int sec1,int sec2){
  assert(mNSector==0) ; //you can do it just once, no memory realocation implemented

  mfirstSecID=sec1;
  mlastSecID=sec2;
  mNSector=mlastSecID - mfirstSecID+1;

  mDbADCconf=(eemcDbADCconf_st **) new void *[mNSector];
  mDbPMTconf=(eemcDbPMTconf_st **) new void *[mNSector];
  mDbPMTcal= (eemcDbPMTcal_st  **) new void *[mNSector];
  mDbsectorID=  new int [mNSector];
  
  printf("\n\n%s Use sectors from %d to %d\n",GetName(),mfirstSecID,mlastSecID);

}

//__________________________________________________
//__________________________________________________
//__________________________________________________


const StEEmcDbIndexItem1*  
StEEmcDbMaker::getT(int sec, int sub, int eta){
  char name[20];
  sprintf(name,"%2.2dT%c%2.2d",sec,sub,eta);
  int index=EEname2Index(name);
  return mDbItem1+index;  
}


//__________________________________________________
//__________________________________________________
//__________________________________________________

Int_t  StEEmcDbMaker::InitRun  (int runumber){
  printf("\n\nInitRun :::::: %s\n\n\n",GetName());
  mReloadDb();
  mOptimizeDb();
  return kStOK;
}  

//__________________________________________________
//__________________________________________________
//__________________________________________________

void  StEEmcDbMaker::mReloadDb  (){

  int found=0;
  int i;
  printf("%s::reloadDb use TimeStamp from:  %p %p \n",GetName(),GetMaker("StarDb"),GetMaker("db"));

  St_db_Maker* mydb = (St_db_Maker*)GetMaker("StarDb");
  if(mydb==0) mydb = (St_db_Maker*)GetMaker("db");
  assert(mydb);
  TDatime aa;
  
  if(myTimeStampDay==0) {    
    StEvent *stEvent= (StEvent *) GetInputDS("StEvent"); 
    assert(stEvent); 
    
    printf("StEvent time=%d, ID=%d, runID=%d\n",(int)stEvent->time(),(int)stEvent->id(),(int)stEvent->runId());
    
    StEvtHddr* fEvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
    printf("EvtHddr actual event time stamp= %d, yyyy/mm/dd=%d hh/mm/ss=%d\n",
	   (int)fEvtHddr->GetUTime(),fEvtHddr->GetDate(),fEvtHddr->GetTime());
  
    //  int time0; // (sec) GMT of the first event
    //  time0=fEvtHddr->GetUTime( ); //<<==== this is used by DB

  } else {
    printf("fixed TimeStampDay=%d \n",myTimeStampDay);
    mydb->SetDateTime(myTimeStampDay,0); // set ~day & ~hour by hand
  }
  // mydb->SetDateTime(20021201,0); // set ~day & ~hour by hand


  printf("JB: access DB info, first time, use timeStamp=\n  ");
  aa=mydb->GetDateTime();
  aa.Print();
  
  TDataSet *eedb=GetDataBase("Calibrations/eemc");
  if(eedb==0) {
    printf(" \n\n Did you setup 'setenv Calibrations_eemc VerD' or other VerX and/or ~dbServers.xml ?, JB\n\n");
    assert(eedb);
  }
  eedb->ls(2);

  // clear old DB tables  ...................
  
  for(i=0; i<mNSector; i++) {// clear old data
    mDbADCconf[i]=0;
    mDbPMTconf[i]=0;
    mDbPMTcal [i]=0;
    mDbsectorID[i]=-1;
  }

  for(i=0; i<EEindexMax; i++)
    mDbItem1[i].clear();

  int is;
  for(is=0; is< mNSector; is++) {
    int secID=is+mfirstSecID;
    char secTx[100];
    sprintf(secTx,"sector%2.2d",secID);
    char name[100];
    mDbsectorID[is]=secID;
 
    sprintf(name,"%s/eemcADCconf",secTx); //.................
    printf("request=%s==>",name);
    St_eemcDbADCconf *ds1= (St_eemcDbADCconf *)eedb->Find(name);

    if(ds1) {
      printf("Conf->NRows()=%d\n",(int)ds1->GetNRows());
      assert(ds1->GetNRows()==1); // DB TABLE HAS ONLY ONE ROW
      mDbADCconf[is]=(eemcDbADCconf_st *) ds1->GetArray();
      assert(mDbADCconf[is]); // db error if not delivered
      mCleanDbNames(mDbADCconf[is]->name, EEMCDbMaxAdcName);   
      found++;
    } else {
      printf("Not Found in DB, continue \n");
    }

    sprintf(name,"%s/eemcPMTconf",secTx);//.................
    printf("request=%s=\n",name);
    St_eemcDbPMTconf *ds2= (St_eemcDbPMTconf *)eedb->Find(name);
    if(ds2) {
      printf("Conf->NRows()=%d\n",(int)ds2->GetNRows());
      assert(ds2->GetNRows()==1);  // DB TABLE HAS ONLY ONE ROW
      mDbPMTconf[is]=(eemcDbPMTconf_st *) ds2->GetArray();
      assert(mDbPMTconf[is]); // db error
      mCleanDbNames(mDbPMTconf[is]->name, EEMCDbMaxAdcName);
      found++;
    } else {
      printf("Not Found in DB : %s , continue \n",name);
    }

    sprintf(name,"%s/eemcPMTcal",secTx); //.................
    printf("request=%s=\n",name);
    St_eemcDbPMTcal *ds3= (St_eemcDbPMTcal *)eedb->Find(name);
    if(ds3) {
      printf("Cal->NRows()=%d\n",(int)ds3->GetNRows());
      assert(ds3->GetNRows()==1);  // DB TABLE HAS ONLY ONE ROW
      mDbPMTcal[is]=(eemcDbPMTcal_st *) ds3->GetArray();
      assert(mDbPMTcal[is]); // db error
      mCleanDbNames(mDbPMTcal[is]->name, EEMCDbMaxAdcName);
      found++;
    } else {
      printf("Not Found in DB : %s , continue \n",name);
    }


  } // end of loop over sectors

  printf("Found %d EEMC related tables for the present time stamp\n",found);
  assert(found); // no relevant records were in db, makes no sense to use this maker for any work
}

//__________________________________________________
//__________________________________________________
//__________________________________________________

void  StEEmcDbMaker::mOptimizeDb(){

  int i, j;
  printf("\n\noptimizeDb :::::: %s\n\n\n",GetName());


  // primary information: slot,chan <--> element name
  for(i=0; i<mNSector; i++) {
    eemcDbADCconf_st *t= mDbADCconf[i];
    if(t==0) continue;
 
    //    printf("full name===%s===\n\n",t->name);
    
    for(j=0;j<EEMCDbMaxAdc; j++) { // loop within sector
      char *name=t->name+j*EEMCDbMaxName;
      //      printf("jjj i=%d j=%d %d %d '%s' kill!\n",i,j,t->slot[j],t->channel[j],name);// continue;
      if(*name==0) continue;

      //      printf("aaa %d %d\n",t->slot[j],t->channel[j]);
      // validate entries
      //tmp      assert(t->slot[j]>=0 && t->slot[j]<EEMC_MaxAdcSlot);
      //tmp assert(t->channel[j]>=0 && t->channel[j]<EEMC_MaxAdcChan);

      int index=EEname2Index(name);
      // store valid entry
      mDbItem1[index].crate=t->slot[j];
      mDbItem1[index].chan=t->channel[j];
      mDbItem1[index].setName(name);

      //      if(j>300) break;
      printf("Mapped %s -->index=%d -->slot/chan=%d/%d \n",mDbItem1[index].name,index,t->slot[j],t->channel[j]);
    }

  } 


  //---------------------------------------------------
  printf("\nAcquire secondary info for active elements\n");

  int index;
  for(index=0; index<EEindexMax; index++){
    if(mDbItem1[index].chan<0) continue;
    StEEmcDbIndexItem1 *item=mDbItem1+index;

    char *name=item->name;
    int secID=atoi(name);
    printf("update %s in sec=%d \n",name,secID);


    for(i=0; i<EEindexMax; i++) {
      if(secID==mDbsectorID[i]) break;
    }
    assert(i<EEindexMax ); // sector not loaded from DB ???, sth is wrong

    eemcDbPMTcal_st *t= mDbPMTcal[i];
    if(t==0) continue; // DB data for this sector not loaded from DB
    
    int found=0;
    for(j=0;j<EEMCDbMaxAdc; j++) { // loop within sector
      char *name1=t->name+j*EEMCDbMaxName;
      char *p=strstr(item->name,name1);
      //	printf("ppp %p %s %s \n",p,item->name,name1);
      if(p==0) continue;
      found+=1;
      mDbItem1[index].gain=t->gain[j];
      mDbItem1[index].hv=t->hv[j];
      printf(" found=%d gain=%f hv=%f\n",found,mDbItem1[index].gain,mDbItem1[index].hv);
      break;
    }
    
  }// end of loop over index


}

//__________________________________________________
//__________________________________________________
//__________________________________________________

void  StEEmcDbMaker::mPrintItems  (){

  printf("\n\nprintChan :::::: %s\n\nchan   slot -->  name\n ",GetName());

  // int i=0,j=0;
#if 0
  for(i=0; i<EEMC_MaxAdcSlot; i++) 
    for(j=0; j<EEMC_MaxAdcChan; j++) {
     char* name= DbChan2name[i][j];
     if(name[0]==0) continue;
     printf("%d  %d  %s\n",i,j,name);
    }


  printf("\nInverse relation  \n nameHashIndex --> slot + chan\n");
  for(i=0; i<EEMC_MaxNameHash; i++){ 
    if(DbName2chan[i].chan<0) continue;
    printf("%d   %d  %d \n",i, DbName2chan[i].slot, DbName2chan[i].chan);
  }

#endif
  return;

#if 0
  //  int i,j;
  printf("test:  chan,slot <-->  name <-->index\n ");
  for(i=0; i<EEMC_MaxAdcSlot; i++) 
    for(j=0; j<EEMC_MaxAdcChan; j++) {
     char* name= DbChan2name[i][j];
     if(name[0]==0) continue;
     printf("test %d  %d  %s\n",i,j,name);
     int chan,slot;
     name2chan(name,slot,chan);
     char name1[1000];
     chan2name(slot,chan,name1);
     assert(strstr(name,name1)); // consistency check

     //tmp     int k=name2HashIndex(name);
     //tmp assert(strstr(DbName2chan[k].name,name1));
     
    }

#endif
  printf("test OK\n");

  
}


//_________________________________________________________
//_________________________________________________________
//_________________________________________________________

Int_t StEEmcDbMaker::Make(){
  
  //  printf("\n\nMake :::::: %s\n\n\n",GetName());

  return kStOK;

}

//_________________________________________________________
//_________________________________________________________
//_________________________________________________________
void StEEmcDbMaker::mCleanDbNames(char * buf, int len){
  assert(buf); // dumm input, better crash
  int k;
  for(k=0;k<len;k++) 
    if( buf[k]==EEMCDbStringDelim) buf[k]=0;
}
