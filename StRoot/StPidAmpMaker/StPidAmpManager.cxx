/***************************************************************************
 *
 * $Id: StPidAmpManager.cxx,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             StPidAmpManager manages multiple StPidAmpChannelCollections
 ***************************************************************************
 *
 * $Log: StPidAmpManager.cxx,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#include <float.h>
#include "StMessMgr.h"

#include "StPidAmpMaker/StPidAmpManager.h"
#include "StPidAmpMaker/Infrastructure/StPidAmpChannelCollectionVector.hh"
#include "StPidAmpMaker/StPidAmpTrkVector.h"
#include "StPidAmpMaker/Infrastructure/StPidAmpChannelCollection.hh"
#include "StPidAmpMaker/Include/StPidAmpConst.hh"
//--------------------------------
StPidAmpManager::StPidAmpManager(){
    mChannelCollections=new StPidAmpChannelCollectionVector();

}
//--------------------------------
StPidAmpManager::~StPidAmpManager(){
  /* no op */
}
//--------------------------------
StPidAmpManager::StPidAmpManager(const StPidAmpManager&){}//not yet
//--------------------------------
void StPidAmpManager::setNHits4BGNet(Int_t theNHits){
      netSets()->back()->setNHits4BGNet(theNHits);
}

//--------------------------------
void StPidAmpManager::bookADefaultChannelCollection(TString fitOpt, TString drawOpt){
     
   Int_t*    nhitsArray =new Int_t[2];
   Double_t*  ptArray   =new Double_t[2];

  nhitsArray[0]=0;
  nhitsArray[1]=NMaxHits; 

  ptArray[0]=0;
  ptArray[1]=FLT_MAX;

   StPidAmpChannelCollection* defaultChannelCollection=new StPidAmpChannelCollection(2, nhitsArray, 2, ptArray,noDependent,fitOpt,drawOpt);
   mChannelCollections->push_back(defaultChannelCollection);
}

//--------------------------------
void StPidAmpManager::bookAPtChannelCollection(Double_t x1, Double_t x2, Double_t x3,TString fitOpt, TString drawOpt){
     Int_t*    nhitsArray=new Int_t[2];
   Double_t*    ptArray   =new Double_t[3];

  nhitsArray[0]=0;
  nhitsArray[1]=NMaxHits; 

   ptArray[0]=x1;
   ptArray[1]=x2;
   ptArray[2]=x3;

   StPidAmpChannelCollection* ptChannelCollection=new StPidAmpChannelCollection(2, nhitsArray, 3, ptArray,ptDependent,fitOpt,drawOpt);
   mChannelCollections->push_back(ptChannelCollection);

}

//--------------------------------
void StPidAmpManager::bookAPtChannelCollection(Double_t x1, Double_t x2, Double_t x3, Double_t x4,TString fitOpt, TString drawOpt){

     Int_t*    nhitsArray=new Int_t[2];
   Double_t*    ptArray   =new Double_t[4];

  nhitsArray[0]=0;
  nhitsArray[1]=NMaxHits; 

   ptArray[0]=x1;
   ptArray[1]=x2;
   ptArray[2]=x3;
   ptArray[3]=x4;

   StPidAmpChannelCollection* ptChannelCollection=new StPidAmpChannelCollection(2, nhitsArray, 4, ptArray,ptDependent,fitOpt,drawOpt);
   mChannelCollections->push_back(ptChannelCollection);

}



//--------------------------------
void StPidAmpManager::bookAPtChannelCollection(Double_t x1, Double_t x2, Double_t x3, Double_t x4, Double_t x5,TString fitOpt, TString drawOpt){


     Int_t*    nhitsArray=new Int_t[2];
   Double_t*    ptArray   =new Double_t[5];

  nhitsArray[0]=0;
  nhitsArray[1]=NMaxHits; 

   ptArray[0]=x1;
   ptArray[1]=x2;
   ptArray[2]=x3;
   ptArray[3]=x4;
   ptArray[4]=x5;


   StPidAmpChannelCollection* ptChannelCollection=new StPidAmpChannelCollection(2, nhitsArray, 5, ptArray,ptDependent,fitOpt,drawOpt);
   mChannelCollections->push_back(ptChannelCollection);

}




//--------------------------------
void StPidAmpManager::bookANHitsChannelCollection(Int_t x1, Int_t x2, Int_t x3,TString fitOpt, TString drawOpt){
   Int_t*    nhitsArray=new Int_t[3];
   Double_t*  ptArray   =new Double_t[2];

   if ((x1>NMaxHits)||(x2>NMaxHits)||(x3>NMaxHits)||(x1*x2*x3<0)) {
          gMessMgr->Error()<<"illegal nhits"<<endm;
          return;
   } else{
 
  nhitsArray[0]=x1;
  nhitsArray[1]=x2; 
  nhitsArray[2]=x3;

  ptArray[0]=0;
  ptArray[1]=FLT_MAX;

   StPidAmpChannelCollection* nhitsChannelCollection=new StPidAmpChannelCollection(3, nhitsArray, 2, ptArray,nhitsDependent,fitOpt,drawOpt);
   mChannelCollections->push_back(nhitsChannelCollection);
}
}



//--------------------------------
void StPidAmpManager::bookANHitsChannelCollection(Int_t x1, Int_t x2, Int_t x3, Int_t x4,TString fitOpt, TString drawOpt){
     Int_t*    nhitsArray=new Int_t[4];
   Double_t*  ptArray   =new Double_t[2];

   if ((x1>NMaxHits)||(x2>NMaxHits)||(x3>NMaxHits)||(x4>NMaxHits)||(x1*x2*x3*x4<0)) {
          gMessMgr->Error()<<"illegal nhits"<<endm;
          return;
   } else{
 
  nhitsArray[0]=x1;
  nhitsArray[1]=x2; 
  nhitsArray[2]=x3;
  nhitsArray[3]=x4;

  ptArray[0]=0;
  ptArray[1]=FLT_MAX;

   StPidAmpChannelCollection* nhitsChannelCollection=new StPidAmpChannelCollection(4, nhitsArray, 2, ptArray,nhitsDependent,fitOpt,drawOpt);
   mChannelCollections->push_back(nhitsChannelCollection);
}
}
 


//--------------------------------
void StPidAmpManager::bookANHitsChannelCollection(Int_t x1, Int_t x2, Int_t x3, Int_t x4, Int_t x5,TString fitOpt, TString drawOpt){
     Int_t*    nhitsArray=new Int_t[5];
   Double_t*  ptArray   =new Double_t[2];

   if ((x1>NMaxHits)||(x2>NMaxHits)||(x3>NMaxHits)||(x4>NMaxHits)||(x5>NMaxHits)||(x1*x2*x3*x4*x5<0)) {
          gMessMgr->Error()<<"illegal nhits"<<endm;
          return;
   } else{
 
  nhitsArray[0]=x1;
  nhitsArray[1]=x2; 
  nhitsArray[2]=x3;
  nhitsArray[3]=x4;
  nhitsArray[4]=x5;

  ptArray[0]=0;
  ptArray[1]=FLT_MAX;

   StPidAmpChannelCollection* nhitsChannelCollection=new StPidAmpChannelCollection(5, nhitsArray, 2, ptArray,nhitsDependent,fitOpt,drawOpt);
   mChannelCollections->push_back(nhitsChannelCollection);
}
}
 

//--------------------------------
void StPidAmpManager::bookAPtNHitsChannelCollection(Int_t n, Int_t* nhitsAry,Int_t p, Double_t* ptAry,TString fitOpt, TString drawOpt){

  bool b=true; //I'd rather use bool instead of Bool_t, which may has trouble.

  for (int i=0; i<n; i++) {
  b=b && (nhitsAry[i]<NMaxHits && nhitsAry[i]>0); 
  }
  
  if (!b){
       gMessMgr->Error()<<"illegal nhits"<<endm;
        return;
  }else{
    
   StPidAmpChannelCollection* PtNHitsChannelCollection=new StPidAmpChannelCollection(n, nhitsAry, p, ptAry,ptNhitsDependent,fitOpt,drawOpt);
   mChannelCollections->push_back(PtNHitsChannelCollection);
  }
}
//--------------------------------
void StPidAmpManager::printAllSetsNames(){
   
   StPidAmpChannelCollectionConstIter iter;
   StPidAmpChannelCollection*         theSet;

   gMessMgr->Info()<<" ChannelCollections in store: "<<endm;

   for (iter=mChannelCollections->begin(); iter!=mChannelCollections->end(); iter++){
      theSet=*iter;
      gMessMgr->Info()<<theSet->name().c_str()<<endm;
   }

}
//--------------------------------
StPidAmpChannelCollectionVector* StPidAmpManager::netSets(){
     return mChannelCollections;
}
//--------------------------------
void StPidAmpManager::printNSets(){
  gMessMgr->Info()<<" number of ChannelCollections in store is "<<mChannelCollections->size()<<endm;
}
   
//--------------------------------
void StPidAmpManager::process(StPidAmpTrkVector* trks,TH3D* histo){

   if (mChannelCollections->size()==0) bookADefaultChannelCollection("BAR","BAR");

   StPidAmpChannelCollectionIter iter;
   StPidAmpChannelCollection*    theSet;

   for (iter=mChannelCollections->begin(); iter!=mChannelCollections->end(); iter++){
           theSet=*iter;


           theSet->process(trks,histo);
 
   }
}

//--------------------------------

void StPidAmpManager::clearStore(){
        mChannelCollections->clear();
}

//--------------------------------
ostream& operator<<(ostream& s, StPidAmpManager& store){
   StPidAmpChannelCollectionIter iter;
   StPidAmpChannelCollection*    theSet;

   for (iter=(store.netSets())->begin(); iter!=(store.netSets())->end(); iter++){
           theSet=*iter;
           s<<*theSet<<endl;
   }

  return s;
}

//--------------------------------
ClassImp(StPidAmpManager)
