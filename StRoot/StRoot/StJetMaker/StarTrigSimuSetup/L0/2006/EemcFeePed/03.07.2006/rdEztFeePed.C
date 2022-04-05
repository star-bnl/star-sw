TObjArray  *HList;
class StEEstaleMaker;

int rdEztFeePed(
 int nEve=3000,
 Int_t nFiles  = 20,
 char* file="R5135068ezB.lis", 
 char* inDir   = "../oldPanitkin/muDst/",
 TString outPath="./"
 ){ 
  // file="st_physics_6008023_raw_1030001.MuDst.root";// pedestal, 1987eve
  file="st_physics_7064056_raw_1040001.MuDst.root";// commisCu, ~20Keve;
  inDir   = "/star/data03/daq/2006/064/";

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
  assert( !gSystem->Load("StEEmcPoolmuEztFeePed")); 
  //  assert( !gSystem->Load("StEEmcUtil")); 

  // create chain    
  chain = new StChain("StChain"); 

  printf("adding muDst from '%s' ....\n",file);
  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree); 
  int nEntries=tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);
  printf("in=%s%s=\n",inDir,file);
  //return;

  HList=new  TObjArray;

  myMk3=new StFeePedMaker("eeFeePed","MuDst");
  myMk3->SetHList(HList);
 
  gMessMgr->SwitchOff("D");
  gMessMgr->SwitchOn("I");
 

  muMk->SetStatus("*",0);
  muMk->SetStatus("EztAll",1);
  chain->Init();
  chain->ls(3);
  // muMk->printArrays();

  printf("All Ezt-branches set\n");
  int eventCounter=0;
  int stat=0;
  int t1=time(0);
  StMuTimer timer;
  timer.start();

  //---------------------------------------------------
  while ( 1) {// loop over events
    if(eventCounter>=nEve) break;
    eventCounter++;
    chain->Clear();
    stat = chain->Make();
    if(stat) break;
    if(eventCounter%1000!=0)continue;
    
    printf("\n\n ====================%d  processing  ==============\n", eventCounter);
    
  }
  printf("sorting done, nEve=%d of %d\n",nEve, nEntries);
  int t2=time(0);
  if(t1==t2) t2++;
  float rate=1.*eventCounter/(t2-t1);
  float nMnts=(t2-t1)/60.;
  printf("sorting done %d of   nEve=%d, elapsed rate=%.1f Hz, tot %.1f minutes\n",eventCounter,nEntries,rate,nMnts);
  
  if (eventCounter) {
    cout << "CPU time/event= " << timer.elapsedTime()/eventCounter << " sec  "
	 << " rate= " << eventCounter/timer.elapsedTime() <<  " Hz" << endl;
  }
  
  //--------------------  calculate FEE peds & produce output files ------
  
  printf("name   Nentries chi2  gaus_ampl gaus_mean gaus_sig err_ampl err_mean err_sig \n");

  TIterator* iter = HList->MakeIterator();

  TH1F* h;
  char oName[100];
  FILE *fo;
  oName[0]=0;
  int nPed=0;
  TString fName=outPath+"eemcPed4.dat";
  FILE *fo2=fopen(fName.Data(),"w");

  while((h=(TH1F*)iter->Next())) {
    char *txt=h->GetName();
    int cr=atoi(txt+2);
    int chan=atoi(txt+6);
    int ped4=0;
    if(chan%32==0) {
      if(oName[0]) fclose(fo);
      sprintf(oName,"%scrate%dboard%d.ped4",outPath.Data(),cr, 1+chan/32);
      FILE *fo=fopen(oName,"w");
      if(fo==0) {
        printf("\n\nDid you created dir=%s ???  ,JB\n\n",outPath.Data());
        printf("ABORT macro\n");
        assert(fo);
      }
      printf("  write to '%s'\n",oName);
    }
    
    int ret=-1;
    // Fit range max -low, max+high, default: max-5, max+4
    if(cr==4 && chan==127)  ret=myMk3->fitPed(h,20); // just example
    else   ret=myMk3->fitPed(h);
    
    if (ret) {
      printf("%s empty\n",h->GetName());
      fprintf(fo2," %d %3d 5000 %d\n",cr,chan,ped4);
      fprintf(fo," %d\n",ped4);
      continue;
    }
    TF1* fit= h->GetFunction("pedFun");
    assert(fit);
    nPed++;
    
    // fit->Print(); return; 
    
    float ped=fit->GetParameter(1);
    int ped4=(25-ped)/4;
    if(ped>24)  ped4=(22-ped)/4;
    fprintf(fo2,"%d %d %.1f %d\n",cr,chan,ped,ped4);
    fprintf(fo,"%d\n",ped4);
    
    printf("%s %8.0f %8.3f %9.3f %8.2f %7.2f %6.2f %7.2f %6.2f\n",
           h->GetName(),
           h->GetEntries(),
           fit->GetChisquare(),
           fit->GetParameter(0),
           fit->GetParameter(1),   // pedestal centroid
           fit->GetParameter(2),   // sigma of gaussian
           fit->GetParError(0),
           fit->GetParError(1),
           fit->GetParError(2));
    //     break;
  }
  
  myMk3->saveHisto(outPath+"feePed");
  fclose(fo2);
  fclose(fo);
  printf("Fit found %d pedestals\n",nPed);

  //  h=(TH1F*)HList->FindObject("cr5_ch095"); // this is 'stuck bit'  channel
  h=(TH1F*)HList->FindObject("cr5_ch094"); // a regular channel
  h->Draw(); gPad->SetLogy();

}
