class  StChain;
StChain *chain;

void rdMuFindBckg(int nevents=200000, char* outDir="./") { 

 char *muDstFile;

 /*************************************************************/
 // use single or list of muDsts: Example below from year 2006
 /*************************************************************/

#if 1  // barrelBckg run
  muDstFile="/star/data05/scratch/qattan/myruns_R7138017/*.MuDst.root";
  int RunNumber=7138017;
#endif

  /******************************************************/

  cout <<"MuDst chain file:\t"<<muDstFile<<endl;


  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("libPhysics");
  }
  
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  char *libL[]={
    "StEmcRawMaker","StEmcADCtoEMaker",
    "StDbBroker","St_db_Maker","StDaqLib",
    "StEEmcUtil",
    "StEEmcDbMaker",
    "libStEmcPoolBckgFinder"
  };
  printf(".C Load :");
  int i;
  for(i=0;i<sizeof(libL)/sizeof(char*);i++) {
    printf("'%s', ",libL[i]);
    assert(!gSystem->Load(libL[i]));
  }
  printf(" \ndone loading %d libraries\n",i);

  //  LoadLogger() ;
   
  chain= new StChain("StChain"); 

  //..............Instantiate the MuDstReader
  printf("muDst: =%s=\n",muDstFile);
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",muDstFile,"",10,"MuDst");
  muDstMaker->SetStatus("EztAll",1); // to get trigger data
  TChain* tree=muDstMaker->chain(); assert(tree); 
  int nEntries=(int) tree->GetEntries();
  printf("total eve in muDst chain =%d\n",nEntries); // return;

  if(nEntries<=0) return;
  //......StMuDbReader...
  StMuDbReader* dbRd = StMuDbReader::instance();
  
  //.......Database
  St_db_Maker *dbMk = new St_db_Maker("StarDb", "MySQL:StarDb"); // default
  //St_db_Maker *dbMk = new St_db_Maker("StarDb","/star/u/balewski/2005-jets-BTOW-DB-Fall/StarDb","MySQL:StarDb"); // private

  myDb=new StEEmcDbMaker("eemcDb"); //myDb->setSectors(11,12);
 
  mHList=new  TObjArray;


  StBemcBeamBckgFinderMaker *myMk= new StBemcBeamBckgFinderMaker("BemcBckgMaker");
  myMk->SetHList(mHList);

  //********* SET TRIGGER TYPE ************************// 
  //trig selection use case:  
  //myMk->SetTrigger(-1);      //accept any & print all
  //myMk->SetTrigger(0);       //accept any 
  
  //myMk->SetTrigger(127221);  //accept bemc-jp1-mb (year 2006 day122/run035 day122/run037)
  myMk->SetTrigger(137222);  //accept bemc-jp1-mb (year 2006 day137/run035 day138/run017)
  //myMk->SetTrigger(137622);    //accept bemc-jp0-etot-mb-L2jet (day 137/run035)

  //myMk->SetTrigger(1);     //accept bemc-jp1

  //myMk->SetTrigger(96233); //bemc-jp2-mb-b (year 2005)
  //myMk->SetTrigger(96221); //bemc-jp1-mb (year 2005)


  //********* SET ADC THRESHOLD ************************//
  myMk->SetAdcThreshold(10);

  //********* SET ADCSUM THRESHOLD *********************//
  myMk->SetAdcSumThreshold(150.);

  //********* SET PATTERN LENGTH ***********************//
  myMk->SetPatternLength(5);

  //********* SET MAXIMUM NUMBER OF PLOTS (POSTSCRIPT) TO PRODUCE WHEN EVENT=BACKGROUND *****************//
  myMk->SetMaxYesPlots(0);

  //********* SET MAXIMUM NUMBER OF PLOTS (POSTSCRIPT) TO PRODUCE WHEN EVENT= NOT BACKGROUND ************//
  myMk->SetMaxNoPlots(0);

  //*****************************************************************************************************//
  chain->ls(3);
  chain->Init();

  //  return;
#ifdef doOflJets  
  TTree *tr=jetReader->tree();
  jetReader->InitFile(jetTreeFile);
#endif
  

  chain->PrintInfo();
  int eventCounter=0;
  int stat=0;
  int t1=time(0);

  int Decision,eta1,phi1,eta2,Length; 
  float Adcsum;

  
  
  for (Int_t iev=0;iev<nevents; iev++) {

    chain->Clear();
    int iret = chain->Make(iev); 

    printf("*******************************************************************************\n");
    printf("Working on eventNumber = %d:\n",iev+1);


    myMk->GetDecision(Decision,eta1,phi1,eta2,Length,Adcsum);
    const int *idL=myMk->GetSoftIdList();

    if(Decision==1) {

      printf("\n");
      printf("EventDecision = YES --> THIS EVENT IS A BACKGROUND LOCATED AT: (eta1=%d phi1=%d eta2=%d), PATTERN LENGTH=%d, ADCSUM=%f\n",eta1,phi1,eta2,Length,Adcsum);
      
      printf("List of towers SoftIds in the background pattern found:\t");
      int len=0;
      while(*idL>0) { 
	printf("%d, ", *idL);
	len++;
	idL++;
      }
      printf("\n===============================================================================\n");    

    } else {      
     
      if(Decision==0) {
	printf("===============================================================================\n");
	printf("EventDecision = NO --> THIS EVENT IS NOT A BACKGROUND.\n");
	printf("===============================================================================\n");
      } else {
 
	printf("===============================================================================\n");
	printf("EventDecision = NOT SEARCHED ..... NOT OF TRIGGER TYPE.\n");
	printf("===============================================================================\n");
      } 
    }
    
    eventCounter++;
    if (iret) {
      cout << "Bad return code!" << endl;
      break;
    }
    
  } 


  chain->Finish(); 
  printf("\n*******************************************************************************\n");      
  //  return;
  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*eventCounter/(t2-t1);
  printf("Sorting Done: %d of nEve=%d, CPU rate=%.1f Hz, total time %.1f minute(s) \n\n",eventCounter,nEntries,rate,tMnt);


  TString outF=outDir; outF+=RunNumber; outF+=".hist.root";
  hf=new TFile(outF,"recreate");
  // mHList->ls();
  mHList->Write(); 
  printf("\n Histo saved -->%s<\n",outF.Data());

}

void LoadLogger() {
  gSystem->Load("liblog4cxx.so");
  gSystem->Load("StStarLogger.so");
  StLoggerManager::StarLoggerInit();
  
}
