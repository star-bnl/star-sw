//*-- Author : Jan Balewski 
// $Id: StSpinSortMaker.cxx,v 1.5 2001/11/28 23:03:41 balewski Exp $
// $Log: StSpinSortMaker.cxx,v $
// Revision 1.5  2001/11/28 23:03:41  balewski
// ppLMV uses only tracks matched to CTB slats, runs with DAQ & MC data
//
// Revision 1.4  2001/04/24 21:58:26  balewski
// *** empty log message ***
//
// Revision 1.3  2001/04/19 21:30:36  balewski
// add I/O to ppDst
//
// Revision 1.2  2001/04/19 15:33:17  balewski
// *** empty log message ***
//
// Revision 1.1  2001/04/13 18:04:34  balewski
// *** empty log message ***
//
// Revision 1.1  2001/04/12 15:55:21  balewski
// *** empty log message ***
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                     
//  returns ID>0 of the spin configuration extracted from
//  the DB scheme table according to the event time stamp & 
// 
//////////////////////////////////////////////////////////////////////////
#include <assert.h>

#include <math.h>
#include <strings.h>
#include <stdio.h>

#include "StSpinSortMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "TH1.h"

#include "StEventTypes.h"
#include "tables/St_ppDbSpinAvr_Table.h"
#include "tables/St_ppDbSpinConf_Table.h"

#include "St_trg_Maker/St_trg_Maker.h"

#include "tables/St_event_header_Table.h" // not working bXing


ClassImp(StSpinSortMaker)

//_____________________________________________________________________________
StSpinSortMaker::StSpinSortMaker(const char *name):StMaker(name){
 //  const char *name -  the name of this constructor
  printf("CCCCCCCCCCCCCCC Constructor of class=%s= executed\n", name);
  Setup("A",50,0.,100.);
}
//_____________________________________________________________________________
StSpinSortMaker::~StSpinSortMaker(){
//  printf(" DDDDDDDDDDDDDD Destructor executed\n");
}
//_____________________________________________________________________________
Int_t StSpinSortMaker::Init(){

  printf("InInInInInInInInInIn    Initialization start \"%s\",  m_Mode=%d... \n", GetName(),m_Mode);
  printf("selcted SpinConfig version=\"%s\"\n",sVersion.Data());

  hi = new TH1F("IDi","all input events vs. time (sec)",ntbin,tstart,tstop);
  ha = new TH1F("IDa","N(spinID>0) vs. time (sec)",ntbin,tstart,tstop);

  h[0] = new TH1F("ID0","N(spinID<=0) vs. time (sec)",ntbin,tstart,tstop);
  h[1] = new TH1F("ID1","N(spinID=1) vs. time (sec)",ntbin,tstart,tstop);
  h[2] = new TH1F("ID2","N(spinID=2) vs. time (sec)",ntbin,tstart,tstop);
  h[3] = new TH1F("ID3","N(spinID=3) vs. time (sec)",ntbin,tstart,tstop);
  h[4] = new TH1F("ID4","N(spinID=4) vs. time (sec)",ntbin,tstart,tstop);
  h[5] = new TH1F("ID5","N(spinID>=5) vs. time (sec)",ntbin,tstart,tstop);
  h[6]=new TH1F("bX1","RevTic lower bits",10000,0.,1000000);
  h[7]=new TH1F("bX2","RevTic%120 ",122,-0.5,121.5);

  printf("InInInInInInInInInIn Initialization \"%s\"end\n",GetName());

  return StMaker::Init();
}


//_____________________________________________________________________________
Int_t StSpinSortMaker::Make(){
  int j=-1;
  int IDbunch=-2;
  ppDbSpinConf_st *conf=NULL;
  ppDbSpinAvr_st *avr=NULL;
  spinID=0; // clear old value
  
  printf("Mmmmmmmmmmmmmmmmmmm %s-maker event start mmmmmmmmmmmm\n",GetName());
  StEvent *stEvent= (StEvent *) GetInputDS("StEvent");  assert(stEvent);
  time_t t=stEvent->time();
  printf("StEvent Real event time stamp= %d=%s\n",(int)t,ctime(&t));

  StEvtHddr *fEvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
  printf("EvtHddr Real event time stamp= %d, yyyy/mm/dd=%d hh/mm/ss=%d\n",
	 (int)fEvtHddr->GetUTime(),fEvtHddr->GetDate(),fEvtHddr->GetTime());

  //---------- REAPLCE   EVENT TIME STAMP

#if 0 
  //t=time(0);
  //printf("Today is t=%d time stamp= %s\n",t, ctime(&t));
  t=986392801; //Wed Apr  4 10:00:01 2001
  //t-=8*3600;
  printf("My April 4-th:  is t=%d time stamp= %s\n",t, ctime(&t));

  fEvtHddr->SetGMTime( t); //<<==== this is used by DB
  // stEvent->setTime((Long_t)t); //<<== this is not used by DB, I do it for consistency
#endif

  printf("AA3 StEvent: time=%d, ID=%d \n",(int)stEvent->time(),(int)stEvent->id());
   printf("AA bunchCrossingNumber(0)=%d, (1)=%d\n",(int)stEvent->bunchCrossingNumber(0),(int)stEvent->bunchCrossingNumber(1));


  St_DataSet    *ds0=GetDataSet("dst/event_header"); assert(ds0);
    St_event_header *evhd=(St_event_header*) ds0->Find("event_header");
  assert(evhd);
  evhd->Print(0,1);

  //extract bXing ID from trigger Maker


  St_trg_Maker *trg=(St_trg_Maker *)GetMaker("trg");
  // assert(trg);
  int  bx_lo=3, bXing=33;

  if(trg) {
#if 0
    long long revTic;
    revTic=trg->jtd.bXing_hi;
    revTic=revTic << 32;
    revTic+=revTic=trg->jtd.bXing_lo;
    bXing=(revTic%120)+1;
    revTic=revTic;
    bx_lo=trg->jtd.bXing_lo>>1;
    static int bx_lo_off=0;
    if(bx_lo_off==0) bx_lo_off=bx_lo-1;
    bx_lo-=bx_lo_off;

    printf("JB2 bx_hi=0x%0x, bx_lo=0x%0x, bx_lo=%d, bXing=%d len=%d\n\n",
               trg->jtd.bXing_hi,trg->jtd.bXing_lo,  bx_lo,  bXing, sizeof(revTic));
#endif  
  } else {
    bx_lo=2;
    bXing=22;
  }
  

  h[6]->Fill(bx_lo);
  h[7]->Fill(bXing); 
  
  // get TRIG-bunch crossing ID
  IDbunch=bXing/2;
 

  if(!spinAvr) readDB();  // access DB for the first time
  if(spinAvr==NULL) {
    printf("%s-DB table not existing, set spinID=0\n",GetName()); 
    goto end;
  }

  // use SpinAvr for time stamp delimiter
  avr= (ppDbSpinAvr_st *) spinAvr->GetArray(); 
  
  if(!avr) goto end;
  printf("AA ppSpinAvr: author=\"%s\"   nSpinID=%d\n", avr->author,(int)avr->nSpinID);

  assert(avr);
  if(avr->nSpinID==-777) goto end;  // temporay NULL-table implementation
  assert(avr->nSpinID>0);
  
  //If you get here the event time stamp is valid
  conf=(ppDbSpinConf_st *)spinConf->GetArray();
  assert(conf);
  assert(conf->nBunch>0);  // temporay NULL-table check
  
  // match TRIG-bunch crossing to user spin conf scheme
  for(j=0; j<conf->nBunch; j++) {
    // I'm assuming Trig-bunch cross=yellow bunch crossing ??
    //printf("AA %d, %d %d\n",(int)stEvent->bunchCrossingNumber(1),(int)IDbunch,(int)conf->IDyell[j]);
    if(conf->IDyell[j]!=IDbunch) continue;
    // match found
    spinID=conf->IDspin[j];
    printf("%s choose spinID=%d\n",GetName(),spinID);
    goto end;
  }
  printf("JB: error: TRIG-bunch crossing=%d is not in DB spinConf table, STOP\n",IDbunch);
  assert(0);
  

  // index is found and stored in spinID -------------------------------------
 end:

  printf("AA ppSpinAvr: spinID=%d  j=%d\n",spinID,j);
  
  // histos only
  float tsec=stEvent->time()-time0;
  hi->Fill(tsec);
  if(spinID>0) ha->Fill(tsec);
  
  if(spinID<=0) h[0]->Fill(tsec);
  else if(spinID<5) h[spinID]->Fill(tsec);
  else h[5]->Fill(tsec);
  
  return kStOk;
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
void StSpinSortMaker::readDB(){
 printf("JB: access DB info, first time ");

 StEvent *stEvent= (StEvent *) GetInputDS("StEvent"); 
 assert(stEvent);

 printf("StEvent time=%d, ID=%d, runID=%d\n",(int)stEvent->time(),(int)stEvent->id(),(int)stEvent->runId());

 StEvtHddr* fEvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
 printf("EvtHddr actual event time stamp= %d, yyyy/mm/dd=%d hh/mm/ss=%d\n",
	 (int)fEvtHddr->GetUTime(),fEvtHddr->GetDate(),fEvtHddr->GetTime());

 time0=fEvtHddr->GetUTime( ); //<<==== this is used by DB
 TString myDBname="Calibrations/rhic/scheme"+sVersion;
 TDataSet *mdb=GetDataBase(myDBname);
 printf("mdb name=\"%s\" add=%d\n",myDBname.Data(),(int)mdb);
 assert(mdb);

 //from Jeff  GetDataBase("Calibrations/rhic");


 St_ppDbSpinConf *Rconf= (St_ppDbSpinConf *) mdb->Find("ppSpinConf"+sVersion);
 printf("conf add=%d\n",(int)Rconf);
 if(Rconf==NULL) return ;
 spinConf=Rconf;  // will be used to spin-sort events 
 
 St_ppDbSpinAvr *Ravr= (St_ppDbSpinAvr *) mdb->Find("ppSpinAvr"+sVersion);
 printf("avr add=%d\n",(int)Ravr);
 if(Ravr==NULL) return ;

 // print content 

 ppDbSpinConf_st *conf=( ppDbSpinConf_st *)Rconf->GetArray();
 assert(conf);
 printf("\nppSpinConf:  nBunch=%d, name=\"%s\"\n",(int)conf->nBunch, conf->name);
 
 for(int b=1;b<=conf->nBunch;b++) {
   int i=b-1;
   printf("i=%3d IDspin=%2d Yid=%3d  Bid=%3d \n",i,(int)conf->IDspin[i],(int)conf->IDyell[i],(int)conf->IDblue[i]);
 }
 printf("\n");
 
 // assert(Ravr);
 assert(Ravr->GetNRows()==1);
 spinAvr=Ravr; // will be used to trace event time stamp
 
 ppDbSpinAvr_st *avr= (ppDbSpinAvr_st *) Ravr->GetArray();
 
 printf("ppSpinAvr: author=\"%s\"   nSpinID=%d\n", avr->author,(int)avr->nSpinID);
 printf(" comment=\"%s\"\n", avr->comment);
 int i;
 for(i=0;i<avr->nSpinID;i++) {
   printf("confID=%3d, Y: Px=%.4f+-%.4f,  B: Px=%.4f+-%.4f, lum=%.2f, nBunch=%d \n",i+1,avr->yellPx[i],avr->yellPxEr[i],avr->bluePx[i],avr->bluePxEr[i],avr->lum[i],(int)avr->nBunch[i]);
   printf("            Y: Py=%.4f+-%.4f,  B: Py=%.4f+-%.4f\n",avr->yellPy[i],avr->yellPyEr[i],avr->bluePy[i],avr->bluePyEr[i]);
 }
 
 return ;
}




#if 0
 {// test1 of the DB-table valid range
 StEvent *stEvent= (StEvent *) GetInputDS("StEvent");  assert(stEvent);
 printf("AA StEvent: time=%d, ID=%d ",(int)stEvent->time(),(int)stEvent->id());

 TDataSet *mdb=GetDataBase("TestScheme/rhic/userA"); assert(mdb);
 St_ppDbAvrColl *Ravr= (St_ppDbAvrColl *) mdb->Find("ppAvrCollA");
 assert(Ravr);
 ppDbAvrColl_st *avr= (ppDbAvrColl_st *) Ravr->GetArray();
 assert(avr);
 printf("->Got Ravr=%d,  avr=%d\n",Ravr,avr);
 printf("AA ppAvrColl: author=\"%s\"   nSpinID=%d\n", avr->author,(int)avr->nSpinID);

 return kStOk;
 }

#endif







