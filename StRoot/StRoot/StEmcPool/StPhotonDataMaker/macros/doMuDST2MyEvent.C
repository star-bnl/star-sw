
#include "TString.h"
void doMuDST2MyEvent(unsigned long nEvents, char *infile,char *jobid,char *flag,char *coll) 
{
  Bool_t debug=kFALSE;//macro debug
  Bool_t debugg=kFALSE;

  gSystem->Load("StarRoot");
  gSystem->Load("libTable");
  gSystem->Load("libPhysics");
  gSystem->Load("St_base");

  gSystem->Load("liblog4cxx.so");
  gSystem->Load("StStarLogger.so");
  StLoggerManager::StarLoggerInit();

  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StarMagField");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTriggerDataMaker");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StPmdUtil");
  gSystem->Load("StMuDSTMaker");
  gSystem->Load("StBichsel");

   // libraries for the DB
  gSystem->Load("StDaqLib");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDetectorDbMaker");
 
  //gSystem->Load("StEmcDecoder");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");

  gSystem->Load("/star/u/russcher/MyEvent/MyEvent.so");
  gSystem->Load("StMyEventMaker");

  cout << "Finished loading libraries " << endl;

  StChain* chain = new StChain("bfc"); 
  
  StEmcADCtoEMaker* adcMaker=0;
  StPreEclMaker* preEcl=0;
  StEpcMaker *epc=0;  
  St_db_Maker* dbMk=0;
  StDetectorDbMaker *detMk=0;  

  TString output(jobid);
  output.Append("_");
  output.Append(coll);
  output.Append("_");
  output.Append(flag);
  output.Append(".root");

  TString input(infile);
  
  StIOMaker* ioMaker=0;
   
  StMuDstMaker* mudstMaker = new StMuDstMaker(0,0,"",input.Data(),"",25);
  dbMk = new St_db_Maker("db","MySQL:StarDb", "$STAR/StarDb");
  detDbMk = new StDetectorDbMaker();
  
  adcMaker=new StEmcADCtoEMaker("adcm");
  preEcl = new StPreEclMaker();
  epc = new StEpcMaker();
  adcMaker->setPrint(debug);
  preEcl->setPrint(debug);
  epc->setPrint(debug);

  StMyEventMaker* myMaker = new StMyEventMaker("myMaker",output.Data(),flag,coll,debugg);
  myMaker->setDbMaker(dbMk); 
  myMaker->setAdcMaker(adcMaker);

  chain->Init(); 
  
  if(preEcl) {
    cout<<endl<<"Using old clustering, setting parameters now!"<<endl;
    preEcl->SetClusterConditions("bemc",  4, .35, .0350, 0.02, kFALSE);
    preEcl->SetClusterConditions("bprs",  1, 500., 500., 501., kFALSE);
    preEcl->SetClusterConditions("bsmde", 5, .20, .0005, 0.10, kFALSE);
    preEcl->SetClusterConditions("bsmdp", 5, .20, .0005, 0.10, kFALSE);
    //set for AuAu200 (ht trigs):
    if(strcmp(coll,"auau200")==0){
      preEcl->SetClusterConditions("bemc",  4, .35, .1, 0.02, kFALSE);
      preEcl->SetClusterConditions("bprs",  1, 500., 500., 501., kFALSE);
      preEcl->SetClusterConditions("bsmde", 5, .20, .0005, 0.10, kFALSE);
      preEcl->SetClusterConditions("bsmdp", 5, .20, .0005, 0.10, kFALSE);
    }
  }
    
  for(unsigned long iev=0;iev<nEvents; iev++)
    {
      if(iev%100==0) cout<<"processing event: "<<iev<<endl;
      chain->Clear();
      Int_t iret = chain->Make(iev); 
      if(iev%500==0){
          //gObjectTable.Print();
  	  myMaker->saveHistograms();         
      }
      if(iret==2) cout <<"### Last  Event Processed. Status = "<<iret<< endl; 
      if(iret==3) cout <<"### Error Event Processed. Status = "<<iret<< endl; 
      if(iret==4) cout <<"### Fatal Event Processed. Status = "<<iret<< endl;
      if(iret>1) break;
    } 
  chain->Finish(); 
}








