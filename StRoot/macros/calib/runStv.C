class StMaker;
void rmMaker(const StMaker *top,const char *key);
int runStv(const char *daqFile,const char *flg,int nEvents,int myFlag)
{
int stv = (myFlag     )%10;
int trs = (myFlag/10  )%10;
int mcm = (myFlag/100 )%10;
int knn = (myFlag/1000)%10;

printf("\n***  runStv(\"%s\",\"%s\",%d,%d)\n\n",daqFile,flg,nEvents,myFlag);
gROOT->LoadMacro("bfc.C");
TString opt(flg),fil(daqFile);
int fr=1,to=nEvents;
int ierr = 0;

opt += ",StvPulls";


bfc(-1,opt,fil);
chain->SetAttr("blackList","Svt","St_db_Maker::");

// StIOInterFace *iomk = (StIOInterFace *)chain->GetMaker("inputStream");
// if (iomk && !fil.Contains(".geant.")) {
//  TString dir("/star/simu/simu/perev/StiVMC/offline/users/perev/fitErrSim/");
//  iomk->SetBranch("geantBranch",dir,"r");
// }


StMaker *mk=0;

//StMaker::lsMakers(chain);

if (nEvents>=0) chain->Init();

//==========================================
rmMaker(chain,"StKinkMaker");
//==========================================
if (knn){
 chain->SetAttr("seedFinders","KN","Stv");
 chain->SetAttr("seedFinders","KN","Sti");
}
if (nEvents>=0) chain->EventLoop(nEvents);
return 99;
}

void rmMaker(const StMaker *top, const char *key)
{
  TRegexp rex(key);
  for (int del=1;del;) {
    TDataSetIter   iter((TDataSet*)top,20);
    del=0;
    for(const TDataSet *mk=top;mk;mk = iter.Next()) {
      if(strncmp(".maker",mk->GetTitle(),6)!=0) continue;
      TString ts(mk->ClassName()); ts+="::";
      ts+=mk->GetName();
      if (ts.Index(rex)<0) continue;
      printf("***  Remove %s  *** \n",ts.Data());
      delete mk; del = 1; break;
    }
  }

  printf("***  Maker  %s Not Found ***\n",ts.Data());

}

