#include <time.h>
#include <string.h>

#include <TDatime.h>

#include "StMaker.h"

#include "St_DataSetIter.h"

//#include "St_db_Maker/St_db_Maker.h" // tmp to ovveride time stamp

#include "StEventTypes.h"

#include "StEEmcDb.h"

#include "EEmcDbItem.h"
#include "EEmcDbCrate.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h" 

#include "tables/St_eemcDbADCconf_Table.h"
#include "tables/St_eemcDbPMTcal_Table.h"
#include "tables/St_eemcDbPMTname_Table.h"
#include "tables/St_eemcDbPIXcal_Table.h"
#include "tables/St_eemcDbPMTped_Table.h"
#include "tables/St_eemcDbPMTstat_Table.h"
#include "tables/St_kretDbBlobS_Table.h"
#include "cstructs/eemcConstDB.hh"
#include "cstructs/kretConstDB.hh"

#include <StMessMgr.h>

ClassImp(StEEmcDb)

//________________________________________________________
//________________________________________________________
StEEmcDb::StEEmcDb(const Char_t *name) : TDataSet(name) {
  LOG_DEBUG<<endm;
  mfirstSecID=mlastSecID=mNSector=0;
//  myTimeStampDay=0;
//  myTimeStampUnix=0;

  //................ allocate memory for lookup tables
  byIndex=new  EEmcDbItem[EEindexMax];
  
  byCrate=new  EEmcDbItem ** [MaxAnyCrate];
  
  int i;
  for(i=0;i<MaxAnyCrate;i++){
    byCrate[i]=NULL;
    if(i==0 || (i>MaxTwCrateID && i<MinMapmtCrateID) ) continue; // to save memory for nonexisting crates
    byCrate[i]=new EEmcDbItem * [MaxAnyCh];
    memset(byCrate[i],0,sizeof(EEmcDbItem *)*MaxAnyCh);// clear all pointers
  }

  mDbFiber=0; nFiber=0;
  setDBname("Calibrations/eemc");

  mAsciiDbase = "";

  mxChGain=8;
  chGainL = new TString [mxChGain];
  nChGain=0;

  mxChMask=8;
  chMaskL = new TString [mxChMask];
  nChMask=0;

  setThreshold(5.0);  // defines threshold for ADCs
  // should be +2 or +3 sigma in the future
  //  setPreferedFlavor("onlped","eemcPMTped"); // tmp for tests,JB

}


//________________________________________________________
//________________________________________________________
//_______________________________________________________
StEEmcDb::~StEEmcDb(){

    if(mNSector) {
	delete [] mDbADCconf;
        delete [] mDbPMTcal;
        delete [] mDbPMTname;
        delete [] mDbPIXcal;
        delete [] mDbPMTped;
        delete [] mDbPMTstat;
        delete [] mDbsectorID;
    }

    delete mDbFiber;
    delete [] chGainL;
    delete [] chMaskL;
}

//------------------
//------------------
void StEEmcDb::setThreshold(float x){
 KsigOverPed=x;
LOG_INFO <<"::setThres KsigOverPed="<<KsigOverPed<<", threshold=ped+sig*KsigOverPed"<< endm;
}


//------------------
//------------------
void StEEmcDb::setPreferredFlavor(const char *flavor, const char *nameMask){
  strncpy(dbFlavor.flavor,flavor,DbFlavor::mx);
  strncpy(dbFlavor.nameMask,nameMask,DbFlavor::mx);
 LOG_INFO <<  "::setPreferredFlavor(flav='"<<dbFlavor.flavor <<"', mask='"<< dbFlavor.nameMask<<"')" <<endm;
}

//________________________________________________________
//________________________________________________________
//________________________________________________________
void StEEmcDb::setSectors(int sec1,int sec2)
{
  // you can do it just once, no memory realocation implemented
    mfirstSecID=sec1;
    mlastSecID=sec2;
    mNSector=mlastSecID - mfirstSecID+1;
    if ( mNSector==0 ) {
	LOG_ERROR <<  ":: Problem mNSector==0" <<endm;
    } else {
	mDbADCconf=(eemcDbADCconf_st **) new void *[mNSector];
	mDbPMTcal= (eemcDbPMTcal_st  **) new void *[mNSector];
	mDbPMTname=(eemcDbPMTname_st **) new void *[mNSector];
	mDbPIXcal= (eemcDbPIXcal_st  **) new void *[mNSector];
	mDbPMTped= (eemcDbPMTped_st  **) new void *[mNSector];
	mDbPMTstat=(eemcDbPMTstat_st **) new void *[mNSector];
	mDbsectorID=  new int [mNSector];

	clearItemArray();

	LOG_INFO <<  ":: Use sectors from "<< mfirstSecID<<" to "<< mlastSecID<<endm;
    }
}

//__________________________________________________
//__________________________________________________
//__________________________________________________

const EEmcDbCrate* StEEmcDb::getFiber(int icr) const {
  assert(icr>=0);
  assert(icr<nFiber);
  return mDbFiber+icr;
}

//__________________________________________________
//__________________________________________________
//__________________________________________________

void  StEEmcDb::setFiberOff(int icr) {
  if(icr<0 || icr>=nFiber ) {
    LOG_WARN << "::setFiberOff(icr="<<icr<< ") out of range, ignorred" << endm;
    return;
  }
  mDbFiber[icr].useIt=false;
}




//--------------------------------------------------
//--------------------------------------------------
void StEEmcDb::clearItemArray(){
  //printf("%s::clearItemArray()\n",GetName());
  nFound=0;

  int i;

  for(i=0; i<EEindexMax; i++)
    byIndex[i].clear();

  int j;
  for(i=0;i<MaxAnyCrate;i++) {
    if(byCrate[i]==NULL) continue;
    for(j=0;j<MaxAnyCh;j++)
      byCrate[i][j]=0;
  }

  memset(byStrip,0,sizeof(byStrip));
  
  if(mDbFiber) delete [] mDbFiber;
  nFiber=0;
  mDbFiberConfBlob=0;


  nFound=0;

  if ( ! mDbADCconf ){
    LOG_FATAL << ":: Cannot initialize arrays in clearItemArray()" << endm;
  } else {
    mDbADCconf[0]=0;
    for(i=0; i<mNSector; i++) {// clear pointers old DB tables
      mDbADCconf [i]=0;
      mDbPMTcal  [i]=0;
      mDbPMTname [i]=0;
      mDbPIXcal  [i]=0;
      mDbPMTped  [i]=0;
      mDbPMTstat [i]=0;
      mDbsectorID[i]=-1;
    }
  }

}


//__________________________________________________
//__________________________________________________
//__________________________________________________

void StEEmcDb::loadTables(StMaker *anyMaker) {
    if( mNSector==0) setSectors(1,12);//default

    // If an ascii file has been loaded, via setAsciiDatabase(),
    // issue a warning and return.
    if ( mAsciiDbase.Length() > 0 ) {
	LOG_WARN << "loadTables: Database not reloaded, values taken from " << mAsciiDbase.Data() << endm;
    } else {
	LOG_INFO <<  "loadTables: use(flav='"<<dbFlavor.flavor <<"', mask='"<< dbFlavor.nameMask<<"')" <<endm;
	clearItemArray();
	requestDataBase(anyMaker);
	 //............  reload all lookup tables ...............
	Bool_t ok = true;
	for(int is = 0;(is < mNSector) && ok;is++) {
	    if (optimizeMapping(is)) {
		LOG_FATAL<<  "::loadTables  Total failure, no DB info for Endcap was retrived, all ETOW channels will be cleared for every event. Fix the problem! JB"<<endm;
		ok = false;
	    } else {
		optimizeOthers(is); 
	    }
	}
	if (ok)	{
	    optimizeFibers();
            // overload some of DB info
	    for(int is=0;is<nChGain;is++) changeGainsAction(chGainL[is].Data());
	    for(int is=0;is<nChMask;is++) changeMaskAction(chMaskL[is].Data());
	    // exportAscii(); //tmp
	    for(int icr = 0;icr < getNFiber();icr++) {
		const EEmcDbCrate *fiber=getFiber(icr);
		//printf(" eemcDB : ");
		//fiber->print();
		LOG_INFO<<(*fiber)<<endm; //JCW 01/26
	    }
	    LOG_INFO <<  "::loadTables  Found "<< nFound<<" EEMC related tables "<<endm;
	}
    }
}  


//__________________________________________________
//__________________________________________________

void  StEEmcDb::requestDataBase(StMaker *anyMaker){

  int ifl;
  TString mask="";
  for(ifl=0;ifl<2;ifl++) { // loop over flavors
    if(ifl==1) {
      if( dbFlavor.flavor[0]==0) continue; // drop flavor change
      LOG_INFO <<  "::RequestDataBase()-->ifl="<<ifl<<" try flavor='"<<dbFlavor.flavor <<"' for  mask='"<< dbFlavor.nameMask<<"')" <<endm;
      
      anyMaker->SetFlavor(dbFlavor.flavor,dbFlavor.nameMask);
      mask=dbFlavor.nameMask;
    }
    
    TDataSet *eedb = anyMaker ? anyMaker->GetDataBase(dbName) : 0;
    if(!eedb) {
      LOG_FATAL <<  "::RequestDataBase()  Could not find dbName ="<<dbName <<endm;
      return ;
      // down-stream makers should check for presence of dataset
    }
    //eedb->ls(2);  
        
    int is;
    for(is=0; is< mNSector; is++) {
      int secID=is+mfirstSecID;
      
      mDbsectorID[is]=secID;
      getTable<St_eemcDbADCconf,eemcDbADCconf_st>(eedb,secID,"eemcADCconf",mask,mDbADCconf+is);
   
      getTable<St_eemcDbPMTcal,eemcDbPMTcal_st>(eedb,secID,"eemcPMTcal",mask,mDbPMTcal+is);   

      getTable<St_eemcDbPMTname,eemcDbPMTname_st>(eedb,secID,"eemcPMTname",mask,mDbPMTname+is);   

      getTable<St_eemcDbPIXcal,eemcDbPIXcal_st>(eedb,secID,"eemcPIXcal",mask,mDbPIXcal+is);   
      
      getTable<St_eemcDbPMTped,eemcDbPMTped_st>(eedb,secID,"eemcPMTped",mask,mDbPMTped+is);
      
      getTable<St_eemcDbPMTstat,eemcDbPMTstat_st>(eedb,secID,"eemcPMTstat",mask,mDbPMTstat+is);
      
    } // end of loop over sectors
    
    // misc tables 
    
    getTable<St_kretDbBlobS,kretDbBlobS_st>(eedb,13,"eemcCrateConf",mask,&mDbFiberConfBlob);
    if(mDbFiberConfBlob) {
     LOG_DEBUG<<  "::RequestDataBase()  eemcCrateConf dump "<<endm;
    }

  }// end of loop over flavors
 
}

//--------------------------------------------------
//--------------------------------------------------
int  StEEmcDb::optimizeMapping(int is){

  LOG_INFO <<"  conf ADC map for sector="<< mDbsectorID[is] <<endm;
  assert(mDbsectorID[is]>0);
  
  eemcDbADCconf_st *t= mDbADCconf[is];
  
  if(t==0) return -2; // it is fatal error
  LOG_INFO <<"mapping="<< t->comment <<endm;
  
  int j;
  for(j=0;j<EEMCDbMaxAdc; j++) { // loop over channels
    char *name=t->name+j*EEMCDbMaxName;
    
    if(*name==EEMCDbStringDelim) continue;
    
    //printf("%d '%s'  %d %d\n",j,name,t->crate[j],t->channel[j]);
    // printf("%d   %d %d\n",j,t->crate[j],t->channel[j]);
    
    int key=EEname2Index(name);
    assert(key>=0 && key<EEindexMax);
    EEmcDbItem *x=&byIndex[key];
    if(!x->isEmpty()) {
      //x->print(); // it is only on special error, never happends, too much changes needed
      LOG_INFO<<(*x)<<endm; // JCW 01/26
      assert(x->isEmpty());
    }
    x->crate=t->crate[j];
    x->chan=t->channel[j];
    x->setName(name);
    x->key=key;
    x->setDefaultTube(MinMapmtCrateID);
    // x->print();
    
    assert(x->crate>=0 && x->crate<MaxAnyCrate);
    assert(x->chan>=0 && x->chan<MaxAnyCh);
    assert(byCrate[x->crate]);// ERROR: duplicated crate ID from DB
    if(byCrate[x->crate][x->chan]) {
      LOG_FATAL <<  "::Fatal Error of eemc DB records: the same crate="<<x->crate<<", ch="<<x->chan<<" entered twice. Whole EEMC DB is erased from memory, all data will be ignored,  FIX IT !, JB"<<endm;
      //      byCrate[x->crate][x->chan]->print(); // first time
      //      x->print(); // second time
      LOG_FATAL<<(*byCrate[x->crate][x->chan])<<endm;// JCW 1/26
      LOG_FATAL<<(*x)<<endm;// JCW 1/26
      clearItemArray();
      return -1;
    }
    byCrate[x->crate][x->chan]=x;
    if(x->isSMD()) byStrip[x->sec-1][x->plane-'U'][x->strip-1]=x;
  }

  return 0;
  
}


//--------------------------------------------------
//--------------------------------------------------
void StEEmcDb::optimizeOthers(int is){

  int secID= mDbsectorID[is];
  //  printf("\n  optimizeDB for sector=%d\n",secID); //tmp
  int ix1,ix2;
  EEindexRange(secID,ix1,ix2);
  
 LOG_DEBUG<<"   EEindexRange("<<secID<<","<<ix1<<","<<ix2<<")"<<endm;
  
  //  if(dbg)printf(" Size: ped=%d cal=%d name=%d stat=%d \n",sizeof(ped->name)/EEMCDbMaxName,sizeof(cal->name)/EEMCDbMaxName,sizeof(tubeTw->name)/EEMCDbMaxName,sizeof(stat->name)/EEMCDbMaxName);
  
  assert(secID>0);

  eemcDbPMTcal_st  *calT=mDbPMTcal[is];  
  if(calT)  LOG_INFO <<"tower calib="<<calT->comment<<endm;
  eemcDbPMTname_st *tubeTw=mDbPMTname[is];
  if(tubeTw) LOG_INFO <<"PMT names="<<tubeTw->comment<<endm;

  eemcDbPIXcal_st  *calM= mDbPIXcal[is];
  if(calM) LOG_INFO <<"MAPMT calib="<<calM->comment<<endm;

  eemcDbPMTped_st  *ped=mDbPMTped[is];
  if(ped) LOG_INFO <<"ped tower & MAPMT="<<ped->comment<<endm;

  eemcDbPMTstat_st *stat=mDbPMTstat[is];
  if(stat) LOG_INFO <<"status tower & MAPMT="<<stat->comment<<endm;
  
  int key; 
  for(key=ix1;key<ix2; key++) { // loop  in this sector
    EEmcDbItem *x=byIndex+key;
    if(x->isEmpty()) continue;
    char *name=x->name;

    if(ped) { // pedestals 
      int j;
      int mx=sizeof(ped->name)/EEMCDbMaxName;
      for(j=0;j<mx; j++) {
	char *name1=ped->name+j*EEMCDbMaxName;
	if(strncmp(name,name1,strlen(name))) continue;
	x->ped=ped->ped[j];
	x->sigPed=ped->sig[j];
	x->thr=ped->ped[j]+KsigOverPed*ped->sig[j];
	//printf("%d found %s %d %d\n",j,name,strlen(name),strncmp(name,name1,strlen(name)));
	//x->print();
	break;
      }
    } // end of pedestals


    if(calT&& name[2]=='T') { // calibration for towers only
      int j;
      int mx=sizeof(calT->name)/EEMCDbMaxName;
      for(j=0;j<mx; j++) {
	char *name1=calT->name+j*EEMCDbMaxName;
	if(strncmp(name,name1,strlen(name))) continue;
	x->gain=calT->gain[j];
	break;
      }
    } // end of Tower gains


    if(calM && name[2]!='T') { // calibration for MAPMT
      int j;
      int mx=sizeof(calM->name)/EEMCDbMaxName;

      for(j=0;j<mx; j++) {
	char *name1=calM->name+j*EEMCDbMaxName;
	if(strncmp(name,name1,strlen(name))) continue;
	x->gain=calM->gain[j];
	break;
      }
    } // end of gains

    if(tubeTw && name[2]=='T') { // change tube for towers only
      int j;
      int mx=sizeof(tubeTw->name)/EEMCDbMaxName;
      for(j=0;j<mx; j++) {
	char *name1=tubeTw->name+j*EEMCDbMaxName;
	if(strncmp(name,name1,strlen(name))) continue;
	x->setTube(tubeTw->tubeName+j*EEMCDbMaxName);
	//x->print();
	break;
      }
    } // end of tube

    
    if(stat) { // status
      int j;
      int mx=sizeof(stat->name)/EEMCDbMaxName;
      for(j=0;j<mx; j++) {
	char *name1=stat->name+j*EEMCDbMaxName;
	if(strncmp(name,name1,strlen(name))) continue;
	x->stat=stat->stat[j];
	x->fail=stat->fail[j];
	//x->print();
	break;
      }
    } // end of status


    
  }// end of pixels in this sector


}

//--------------------------------------------------
//--------------------------------------------------
void StEEmcDb::exportAscii(const char *fname) const{
  LOG_INFO << "::exportAscii(\'" << fname << "') ..." << endm;

  FILE * fd=fopen(fname,"w");
  assert(fd);
  // fd=stdout;

  int nTot=0;

  fprintf(fd,"# EEmcDb::exportAscii(), see StRoot/StEEmcDb/EEmcDbItem::exportAscii()  for definition\n");

  int j;
  
  fprintf(fd,"%d  #fibers: {name,crID,crIDswitch,fiber,nCh,nHead,type,useIt}\n",nFiber);
  for(j=0;j<nFiber;j++) 
    mDbFiber[j].exportAscii(fd);

  fprintf(fd,"#tw/pre/post:  {name,crate,chan,sec,plane,strip,gain,ped,thr,stat,fail,tube,key}\n");
  fprintf(fd,"#or \n");
  fprintf(fd,"#smd: {name,crate,chan,sec,sub,eta,gain,ped,thr,stat,fail,tube,key}\n");
 

  for(j=0;j<EEindexMax; j++) { // loop over channels
    const  EEmcDbItem *x=byIndex+j;
    if(x->isEmpty())continue;
    x->exportAscii(fd);
    nTot++;
  }
  LOG_INFO << "::exportAscii()     nTot=" << nTot << ", done" << endm;
  fclose(fd);
}


 
//__________________________________________________
//__________________________________________________
//__________________________________________________

void  StEEmcDb::optimizeFibers  (){
  int icr=0;

  if( mDbFiberConfBlob==0) {
    LOG_FATAL <<" EEMC  FiberConf not found"<<endm;
    goto fatal;
  }
  
  assert(mDbFiberConfBlob);
  assert(nFiber==0);
  
  //  printf("dataS='%s'\n",mDbFiberConfBlob->dataS);

  /// if the contents of the mDbFiberConfBlob is the same
  //  for a new runs
  /// the data is not fetched you are left with the stale data
  /// And since strtok overwrites its argument the next call to
  /// mOptimizeFibers fails
  /// Cheers
  ///     Piotr

  {
  static char blobBuffer[KRETmxBlobSlen+1];
  char  *blob = blobBuffer;


  int   len = strlen(mDbFiberConfBlob->dataS); 
  assert(len<KRETmxBlobSlen ); // strnlen() is better but aborts under redhat72,JB
  strncpy(blob,mDbFiberConfBlob->dataS,len);
 
  blob=strtok(blob,";"); // init iterator
  if(strstr(blob,"<ver1>")==0) {
    LOG_FATAL <<" EEMC  FiberConf  missing opening key "<<endm;
    goto fatal;
  }
  
  int i=0;
  while((blob=strtok(0,";"))) {  // advance by one nam{
    i++;
    if(strstr(blob,"<#>")) continue; // ignore some records
    if(strstr(blob,"</ver1>")) goto done; // end of record, tmp
    
    // printf("i=%d -->'%s' \n",i,blob);
    if(nFiber==0) {
      nFiber=atoi(blob);
      mDbFiber=new EEmcDbCrate[ nFiber];
     LOG_DEBUG<<" mOptimizeFibers() map start: , nFiber="<<nFiber<<endm;
      icr=0;
      continue;
    }
    assert(icr<nFiber);
    mDbFiber[icr].setAll(blob);
    //  mDbFiber[icr].print();
    icr++;
  };

  }
  
  LOG_FATAL <<"  mOptimizeFibers() map missing/wrong terminating key "<<endm;
  
 fatal:
    LOG_FATAL <<"     EEMC  FiberConf  error, without it decoding of the _raw_ EEMC data from StEvent is not working (all data are dropped). However, this missing table is irreleveant for muDst analysis, JB"<<endm;
    return;

 done:
    LOG_INFO <<"   mOptimizeFibers() map found for nFiber="<<nFiber<<endm; 
   if(icr==nFiber)  return;

    LOG_FATAL <<"   mOptimizeFibers() map nFiber missmatch is="<<icr<<" should be="<<nFiber<<endm;
goto fatal;
} 

//--------------------------------------------------
//--------------------------------------------------
const  EEmcDbItem*  
StEEmcDb::getByIndex(int i) const {
  // Gets database entry by absolute index

  assert(i>=0);
  assert(i<EEindexMax);
  const  EEmcDbItem *x=byIndex+i;
  if(x->isEmpty()) return 0;
  return x;
}


//--------------------------------------------------
//--------------------------------------------------
const  EEmcDbItem*  StEEmcDb::getByCrate(int crateID, int channel) const {
  // crateID counts from 1, channel from 0 
  int type=0;
  int max=0;
  // printf("cr=%d ch=%d\n",crateID, channel);
  if(crateID>=MinTwCrateID && crateID<=MaxTwCrateID) {
    // Towers
    type =1;
    max=MaxTwCrateCh;
    if(channel>=max) return 0; // not all data blocks are used 

  } else if (crateID>=MinMapmtCrateID && crateID<=MaxMapmtCrateID ){
    //MAPMT
    type =2;
    max=MaxMapmtCrateCh;
  } else if (crateID>=17 && crateID<=46 ){ // only if working with ezTree
    return 0;  //BTOW
  }

  // printf("id=%d  type=%d ch=%d\n",crateID,type,channel);
  //printf(" p=%p \n",byCrate[crateID]);

  assert(type);
  assert( byCrate[crateID]);
  assert(channel>=0);
  assert(channel<max);
  return byCrate[crateID][channel];
}



//_________________________________________________________
//_________________________________________________________
//_________________________________________________________

template <class St_T, class T_st> void StEEmcDb 
::getTable(TDataSet *eedb, int secID, TString tabName, TString mask,  T_st** outTab ) {

  //  printf("\n\n%s ::TTT --> %s, size=%d\n",GetName(),tabName.Data(),sizeof(T_st));

  //  printf("%s ::TTT --> mask='%s' p=%p ss=%d\n",tabName.Data(),mask.Data(),*outTab,tabName.Contains(mask));

  if(!mask.IsNull() && !tabName.Contains(mask)) return ;
  char name[1000];
  if(secID<13) 
    sprintf(name,"sector%2.2d/%s",secID,tabName.Data());
  else
    sprintf(name,"misc/%s",tabName.Data());

  
 LOG_DEBUG<<" request="<< name <<endm;

  St_T *ds= (St_T *)eedb->Find(name);
  if(ds==0) {
    LOG_WARN <<" sector="<<secID<<" table='"<< name <<" not Found in DB, continue "<<endm;
    return ;
  }

  if(ds->GetNRows()!=1) {
    LOG_WARN <<" sector="<<secID<<" table='"<< name <<" no records, continue "<<endm; 
    return ;
  }
  
  T_st *tab=(T_st *) ds->GetArray();

  if(tab==0) {
    LOG_WARN <<" GetArray() failed"<<endm;
    return  ;
  }

  *outTab=tab;
 LOG_DEBUG<<"   map="<< (*outTab)->comment<<"'"<<endm;

  nFound++;
  return ; // copy the whole s-struct to allow flavor change;
}

//__________________________________________________
//__________________________________________________
//__________________________________________________

const EEmcDbItem*  
StEEmcDb::getByStrip0(int isec, int iuv, int istrip) const {
  //  printf("isec=%d iuv=%d istrip=%d \n",isec,iuv,istrip);
  assert(isec>=0 && isec<MaxSectors);
  assert(iuv>=0 && iuv<MaxSmdPlains);
  assert(istrip>=0 && istrip<MaxSmdStrips);
  return byStrip[isec][iuv][istrip];  
}



//__________________________________________________
//__________________________________________________
//__________________________________________________

const EEmcDbItem*  
StEEmcDb::getTile(int sec, char sub, int eta, char type) const {
  char name[20];
  sprintf(name,"%2.2d%c%c%2.2d",sec,type,sub,eta);
  int key=EEname2Index(name);
  return byIndex+key;  
}

//__________________________________________________
//__________________________________________________
//__________________________________________________

const EEmcDbItem* 
StEEmcDb::getStrip(int sec, char uv, int strip) const {
  char name[20];
  sprintf(name,"%2.2d%c%3.3d",sec,uv,strip);
  int key=EEname2Index(name);
  return byIndex+key;
}



//__________________________________________________
//__________________________________________________
//__________________________________________________

void StEEmcDb::setAsciiDatabase( const Char_t *ascii )
{
  // This method allows the user to initialize database
  // values for each EEMC detector from an ascii file
  // rather from a database query.  InitRun() will be
  // blocked from reloading the database on subsequent
  // run numbers.
  
  mAsciiDbase = ascii;

  //-- Clear database values
  //$$$  clearItemArray();

  //-- No provision (for now) for loading in partial info
  Int_t sec1  = 1;
  Int_t sec2  = 12;
  nFound      = 0;
  mfirstSecID = sec1;
  mlastSecID  = sec2;
  mNSector    = mlastSecID - mfirstSecID + 1;  

  //-- Open the specified file
  FILE *fd=fopen(ascii,"r"); 
  EEmcDbItem item;
  int nd=0;
  if(fd==0) goto crashIt;


  
  //-- Loop over all entries in the specified data file
  while (1) {

    //-- Read in each individual entry.  The return value is checked
    //-- for success or failure
    int ret = item.importAscii(fd);

    if(ret==0) break;
    if(ret==1) continue;
    if(ret <0) goto crashIt;

    //-- Index for the specified (named) item
    int key=EEname2Index(item.name);

    //-- Apply a sanity check on the item
    if(key!=item.key) {
      LOG_WARN<<Form(": name='%s' key=%d!=inpKey=%d, inpKey ignored",item.name,key,item.key)<<endm;
      item.key=key;
    }
    assert(key>=0 && key<EEindexMax);


    int isec=item.sec-sec1;
    if (sec1>sec2 ) isec+=12;
    
    if( isec <0 ) continue;
    if( isec >=  mNSector) continue; 
    nd++;

    //-- Copy the database record read in from the file into
    //-- our local arrays.
    EEmcDbItem *x=&byIndex[key];
    if(!x->isEmpty()) goto crashIt;
    *x=item; // copy this DB record
    //    x->print();

    assert(byCrate[x->crate]); // ERROR: unsupported crate ID
    if(byCrate[x->crate][x->chan]) {
      LOG_FATAL<<Form("Fatal Error of eemc DB records: the same crate=%d / channel=%d entered twice for :",x->crate,x->chan)<<endm;
      //      byCrate[x->crate][x->chan]->print(); // first time
      //      x->print(); // second time
      LOG_FATAL<<(*byCrate[x->crate][x->chan])<<endm;// JCW 1/26
      LOG_FATAL<<(*x)<<endm;// JCW 1/26
      assert(1==2);
    }
    byCrate[x->crate][x->chan]=x;
    // assert(2==311);
  }
  
  //--
  //-- Initialize the byStrip lookup table
  //--
  for ( Int_t mySec = 1; mySec <= 12; mySec++ ) 
    for ( Char_t uv = 'U'; uv <= 'V'; uv++ ) 
      for ( Int_t myStrip = 1; myStrip <= 288; myStrip++ ) byStrip[mySec-1][uv-'U'][myStrip-1] = (EEmcDbItem*)getStrip(mySec,uv,myStrip);

      
  //--
  LOG_INFO<<Form("setAsciiDataBase() done, found %d valid records",nd)<<endm;

  return;

 crashIt:// any failure of reading of the data
  LOG_FATAL<<Form("EEmcDb - no/corrupted input file, continue")<<endm;
  //clearItemArray();
  //  assert(2==3);


}

//________________________________________________________
//________________________________________________________
void  StEEmcDb::changeGains(char *fname) {
  assert(nChGain+1< mxChGain);
  chGainL[nChGain]=fname;
  nChGain++;
}

//________________________________________________________
//________________________________________________________
void  
StEEmcDb::changeMask(char *fname) {
  assert(nChMask+1< mxChMask);
  chMaskL[nChMask]=fname;
  nChMask++;
}


//________________________________________________________
//________________________________________________________
void  StEEmcDb::changeGainsAction(const char *fname) {
    
  /* Replace gains only for channels already initialized from the DB 
     format : {name, gains anythingElse}
     lines starting with '#' are ignored
     empty lines are not permitted
  */

  LOG_WARN <<"   ::changeGains('"<<fname<<"')"<<endm;
  FILE *fd=fopen(fname,"r");
  int nd=0,nl=0;
  const int mx=1000;
  char buf[mx];

  if(fd==0) goto end;
  char cVal[100];
  float xVal;
    
  while(1) {
    char *ret=fgets(buf,mx,fd);
    if(ret==0) break;
    
    nl++;
    if(buf[0]=='#') continue;
    int n=sscanf(buf,"%s %f",cVal,&xVal);
    assert(n==2);
    int key=EEname2Index(cVal);
    EEmcDbItem *x=byIndex+key;
    // printf("%s %p\n",cVal,x);
    if(x->isEmpty()) continue;
    // replace only initialized channels
    if(xVal<=0)   LOG_WARN<<Form("Warning ! buf=%s=",buf)<<endm;
    //    assert(xVal>0);
     x->gain=xVal;
     nd++;
  }
  fclose(fd);
    
 end:
  LOG_INFO <<"   ::changeGains('"<<fname<<"') done inpLines="<<nl<<" nChanged="<<nd<<endm;
  return;
  
} 

//________________________________________________________
//________________________________________________________
void  StEEmcDb::changeMaskAction(const char *fname) {

  /* Replace gains only for channels already initialized from the DB 
     format : {name, stat, fail enythingElse}
     lines starting with '#' are ignored
     empty lines are not permitted
  */

  LOG_INFO <<"   ::changeMask('"<<fname<<"') "<<endm;

  FILE *fd=fopen(fname,"r");

  int nd=0,nl=0;
  const int mx=1000;
  char buf[mx];

  if(fd==0) goto end;
  char cVal[100];
  int xStat,xFail ;

  while(1) {
    char *ret=fgets(buf,mx,fd);
    if(ret==0) break;

    nl++;
    if(buf[0]=='#') continue;
    int n=sscanf(buf,"%s %d %d",cVal,&xStat,&xFail);
    assert(n==3);
    int key=EEname2Index(cVal);
    EEmcDbItem *x=byIndex+key;
    if(x->isEmpty()) continue;
    assert(xStat>=0);
    assert(xFail>=0);
    x->stat=xStat;
    x->fail=xFail;
    nd++;
  }
  fclose(fd);

 end:
 LOG_INFO <<"   ::changeMask('"<<fname<<"') done inpLines="<<nl<<" nChanged="<<nd<<endm;
  return;

}

//-------------------------------------------------------------------
//-------------------------------------------------------------------
/*!
This method provides full DB information for a pixel
INPUT Barrel-like indexes from EmcCollection, decoding must be consistent with
 /StRoot/StEEmcSimulatorMaker/StEEmcFastMaker::mEE2ST()
OUTPUT: all avaliable DB information
*/
const EEmcDbItem *
StEEmcDb::StBarrelIndex2Item(int StDetId , int Bmod, int Beta, int  Bsub) const {

  const EEmcDbItem *x=0;
  int sec=Bmod; // range 1-12

  //..... towers,pre,post
  char sub='X'; // range 'A' - 'E'
  char cT='Y'; // range 'T','P','Q','R'
  int  eta=Beta; // range 1-12
  //...... SMD
  char uv='U'; // range 'U' or 'V'
  int strip=Beta;   // range 1-288
  
  switch(StDetId) { // covers all 6 layers of EEMC

  case kEndcapEmcTowerId:
    sub='A'+Bsub-1;  cT='T';
    x=getTile(sec,sub,eta,cT); break;    

  case kEndcapEmcPreShowerId:
    sub='A'+(Bsub-1)%5;  cT='P'+(Bsub-1)/5;
    x=getTile(sec,sub,eta,cT); break;       

  case kEndcapSmdVStripId:
    uv++;
  case kEndcapSmdUStripId: 
    x=getByStrip(sec,uv,strip); break;

  default:
    LOG_WARN <<"::getDb(), wrong detectorId=" << StDetId << ". This is fatal - bug in the code, fix it, JB"<<endm;
    assert(1==2); // trap for bug in the code
  }
  
  return x;
}
