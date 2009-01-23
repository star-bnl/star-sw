// $Id: EEmcDb.cxx,v 1.2 2009/01/23 00:14:50 ogrebeny Exp $

#include <iostream>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

//#include <TH1.h>
//#include <TFile.h>
//#include <TDirectory.h>
//#include <TClonesArray.h>


#define  __STDB_STANDALONE__

#include "EEmcDb.h"
#include "StEEmcUtil/EEfeeRaw/EEdims.h"

#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"

#include "StEEmcUtil/EEfeeRaw/EEname2Index.cxx"  
//--------------------------------------------------
//--------------------------------------------------
EEmcDb::EEmcDb(const char *name) : StEEmcDbMaker(name){
  // printf("CCC EEmcDb\n");
  mfirstSecID=mlastSecID=mNSector=0;
  timeStamp=0;
  setThreshold(1.0);  // defines threshold for ADCs
  
  //................ allocate memory for lookup tables
  byIndex=new  EEmcDbItem[EEindexMax];

  mxAdcCrate=MaxAnyCrate;
  mxAdcChan=MaxAnyCh;
  byCrate=new  EEmcDbItem ** [mxAdcCrate];

  int i;
  for(i=0;i<mxAdcCrate;i++){
    byCrate[i]=NULL;
    if(i==0 || (i>MaxTwCrateID && i<MinMapmtCrateID) ) continue; // to save memory for nonexisting crates
    byCrate[i]=new EEmcDbItem * [mxAdcChan];
    memset(byCrate[i],0,sizeof(EEmcDbItem *)*mxAdcChan);// clear all pointers
  }


} 

//--------------------------------------------------
//--------------------------------------------------
void EEmcDb::readAsciiDataBase(const char *fname,int sec1, int sec2) {

  printf("readAsciiDataBase('%s', sec1=%d sec2=%d)\n",fname,sec1,sec2);


  timeStamp=11;// sth dumm
 
  nFound=0;
  mfirstSecID=sec1;
  mlastSecID=sec2;
  mNSector=mlastSecID - mfirstSecID+1;
  if(sec1>sec2) {
    mNSector+=12;
    printf("EEmcDb::readAsciiDataBase()  will pass 12 o'clock, mNSector=%d\n",mNSector);
  }
  assert(mNSector>0);

  clearItemArray();
  
  FILE *fd=fopen(fname,"r");
  EEmcDbItem item;
  int nd=0;
  if(fd==0) goto crashIt;
  
  while (1  ) {
    int ret=item.importAscii(fd);
    if(ret==0) break;
    if(ret==1) continue;
    if(ret<0) goto crashIt;

    int key=EEname2Index(item.name);
    if(key!=item.key) goto crashIt;
    assert(key>=0 && key<EEindexMax);

    int isec=item.sec-sec1;
    if (sec1>sec2 ) isec+=12;
    
    if( isec<0) continue;
    if( isec>=  mNSector) continue;
    nd++;
    EEmcDbItem *x=&byIndex[key];
    if(!x->isEmpty()) goto crashIt;
    *x=item; // copy this DB record
    //    x->print();

    assert(byCrate[x->crate]);// ERROR: unsupported crate ID
    if(byCrate[x->crate][x->chan]) {
      printf("Fatal Error of eemc DB records: the same crate=%d / channel=%d entered twice for :\n",x->crate,x->chan);
      byCrate[x->crate][x->chan]->print(); // first time
      x->print(); // second time
      assert(1==2);
    }
    byCrate[x->crate][x->chan]=x;
    // assert(2==311);
  }
  
  printf("readAsciiDataBase() done, found %d valid records\n",nd);
  return;

 crashIt:// any failure of reading of the data
  clearItemArray();
  //  assert(2==3);
  printf("EEmcDb - no/corrupted input file, all cleared, continue\n");

}

//--------------------------------------------------
//--------------------------------------------------
const  EEmcDbItem* EEmcDb::getByIndex(int i){
  assert(i>=0);
  assert(i<EEindexMax);
  const  EEmcDbItem *x=byIndex+i;
  if(x->isEmpty()) return 0;
  return x;
}

//--------------------------------------------------
//--------------------------------------------------
const  EEmcDbItem* EEmcDb::getByCrate(int crateID, int channel) {
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
  } else if (crateID>=17 && crateID<=46 ){
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

//--------------------------------------------------
//--------------------------------------------------
void EEmcDb::setThreshold(float x){
  KsigOverPed=x;
  printf("setThres() KsigOverPed=%f, threshold=ped+sig*KsigOverPed\n",KsigOverPed);
} 


//--------------------------------------------------
//--------------------------------------------------
void EEmcDb::exportAscii(const char *fname) const{
  printf("EEmcDb::exportAscii(\'%s') ...\n",fname);
  
  FILE * fd=fopen(fname,"w");
  assert(fd);
  // fd=stdout;
 
  int nTot=0;

  fprintf(fd,"# EEmcDb::exportAscii()\ttime stamp   : %d / %s",(int)timeStamp,
	  ctime((const time_t *)&timeStamp));
  fprintf(fd,"# see StRoot/StEEmcDbMaker/EEmcDbItem::exportAscii()  for definition\n");
  
  int j;
  for(j=0;j<EEindexMax; j++) { // loop over channels
    const  EEmcDbItem *x=byIndex+j;
    if(x->isEmpty())continue;
    x->exportAscii(fd);
    nTot++;
  }
  printf("        nTot=%d, done\n",nTot);
  fclose(fd);
}   

//--------------------------------------------------
//--------------------------------------------------
void EEmcDb::clearItemArray(){
  printf("clearItemArray()\n");
  nFound=0;
  
  int i;
  
  for(i=0; i<EEindexMax; i++)
    byIndex[i].clear();
  
  int j;
  for(i=0;i<mxAdcCrate;i++) {
    if(byCrate[i]==NULL) continue;
    for(j=0;j<mxAdcChan;j++)
      byCrate[i][j]=0;
  }
}  

// $Log: EEmcDb.cxx,v $
// Revision 1.2  2009/01/23 00:14:50  ogrebeny
// Inherited EEmcDb from StEEmcDbMaker to fix run-time bug http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1378
//
// Revision 1.1  2009/01/18 01:01:28  ogrebeny
// Better separate EMC histogramming from OnlinePlots infrastructure
//
// Revision 1.2  2009/01/13 00:39:07  fine
// Add rootrc parameters
//
// Revision 1.1  2007/02/27 15:44:37  laue
// Initial version
//
// Revision 1.1  2006/10/04 20:31:44  laue
// Initial Version
//
// Revision 1.17  2004/02/26 04:21:43  balewski
// reading ASCII dump
//
// Revision 1.16  2004/02/17 03:32:22  balewski
// tower gains fixed
//
// Revision 1.15  2004/02/17 03:09:02  balewski
// *** empty log message ***
//
// Revision 1.14  2004/02/12 15:53:41  balewski
//  EEmcDb::requestDataBase(time,sec1=8,sec2=4) works
//
// Revision 1.13  2004/02/06 05:09:10  balewski
// *** empty log message ***
//
// Revision 1.12  2004/02/05 15:56:53  balewski
// drop use PIXname, use PMTname again
//
// Revision 1.11  2004/01/27 15:15:51  balewski
// *** empty log message ***
//
// Revision 1.10  2004/01/20 05:52:23  balewski
// cleanup
//
// Revision 1.9  2003/12/10 04:42:33  balewski
// *** empty log message ***
//
// Revision 1.8  2003/12/09 17:09:07  zolnie
// *** empty log message ***
//
// Revision 1.7  2003/12/08 22:19:20  zolnie
// allowed for gain
//
// Revision 1.6  2003/12/01 05:01:27  balewski
// DB & SMD
//
// Revision 1.5  2003/11/27 06:30:43  balewski
// added raw/pedSub
//
// Revision 1.4  2003/11/24 05:40:37  balewski
// new stuff for miniDaq
//
// Revision 1.3  2003/11/22 05:35:16  balewski
// *** empty log message ***
//
// Revision 1.2  2003/11/20 22:59:36  balewski
// cleanup
//
// Revision 1.1  2003/11/20 16:08:53  balewski
// start
//
