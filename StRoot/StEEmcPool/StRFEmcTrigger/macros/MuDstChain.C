
// NOTE - chain needs to be declared global so for StHbtEventReader
//=========================================================================================
class  StChain;
class  StMuDstMaker;

StChain *chain;


void MuDstChain(const char *dir ="/star/data35/reco/productionPP/ReversedFullField/DEV/2004/117/",
		const char *file = "st_physics_5117072_raw_2010005.MuDst.root",
		//const char *dir = "/star/data29/reco/pp200/pythia6_203/default/pt15/year2003/gheisha_on/trs_if/",
		//const char *file = "rcf1205_2012_1000evts.MuDst.root",
	       const char *filter = "",
               const char *Eout="test")      
{
 int nevents =5;
 int total=0;
 
  if (gClassTable->GetID("TTable") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("libPhysics");
  } 
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  assert(gSystem->Load("StRFEmcTrigMaker")==0);

  chain= new StChain("StChain"); 
  chain->SetDebug(1);   
  StMuDebug::setLevel(1); 
  StMuDstMaker *muDstMaker = new StMuDstMaker(0,0,dir,file,filter,10,"MuDst");
  
  StRFEmcTrigMaker *trig = new StRFEmcTrigMaker("RFTrig"); 
  trig->setDataMode(0);//0 for MuDst and 1 for StEvent

  Int_t EHT;
  Int_t EJP;
  Int_t ETOT;
  Int_t BHT;
  Int_t BJP;
  Int_t BTOT;
  Int_t BBCtrig;
  int EHTtrig;
  int thres=5;
  //chain->PrintInfo();
  chain->Init();
  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "****************************************** " << endl;
    cout << "Working on eventNumber " << iev << endl;
    cout << "*************************1***************** " << endl;
    chain->Clear();
    int iret = chain->Make(iev); 
    total++;
    EHT= trig->getEEMC_HT_ADC();
    EJP= trig->getEEMC_JP_ADC();
    ETOT= trig->getEEMC_TOT_ADC();
    BHT= trig->getBEMC_HT_ADC();
    BJP= trig->getBEMC_JP_ADC();
    BTOT= trig->getBEMC_TOT_ADC();
    cout <<"In the script now!"<<endl;
    printf("ETOT=%d,EHT =%d,EJP=%d\n",ETOT,EHT,EJP);
    printf("BTOT=%d,BHT=%d,BJP=%d\n",BTOT,BHT,BJP);
 
    BBCtrig= trig->getBBCtrig();   
    EHTtrig= trig->getEEMCtrigHT(thres);
    cout <<"Thres = "<< thres << "  EHTtrig = " << EHTtrig<<endl;
    cout << "BBCtrig = " << BBCtrig<<endl;
 
    if (iret) {
      cout << "Bad return code!" << endl;
      break;
    }
  } 
  chain->Finish(); 
  cout << "****************************************** " << endl;
  cout << "total number of events  " << total << endl;
  cout << "****************************************** " << endl;      
}







