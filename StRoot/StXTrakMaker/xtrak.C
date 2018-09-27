class StMaker;
void rmMaker(const StMaker *top, const char *key);
void xtrak(int flag=0)
{
gROOT->LoadMacro("./bfc.C");
TString opt,fil;
int fr=1,to=1;
int ierr = 0;
int KNN = 0;




#if 1 //Same but Sti
opt = "in,y2016,AgML,usexgeom,FieldOn,MakeEvent,Sti,NoSsdIt,NoSvtIt,";
opt+= "Idst,BAna,l0,Tree,logger,genvtx,tpcDB,bbcSim,btofsim,mtdSim,tags,emcY2,EEfs,evout,";
opt+= "-dstout,IdTruth,big,MiniMcMk";
fil = "/star/simu/simu/perev/StvKutta/OutDirSti/hijing_18_fieldon_ideal_sdt20160517_10evts.event.root";
#endif
//////////////////////////////////////////////////////////////////////////////
bfc(-1,opt,fil);
//chain->SetAttr("seedFinders","KN","Stv");
// chain->SetAttr(".call","SetDebug(1)","St_db_Maker::");
//chain->SetAttr("makePulls",1,"StiMaker::");
// bfc(fr,to,opt,fil);


//int ixt = gSystem->Load("StvUtil");
gSystem->Load("StXTrakMaker");
chain->AddAfter("Sti",     new StXTrakMaker    );
chain->AddAfter("StXTrak", new StXTrakTestMaker);

StMaker::lsMakers(chain);

int stv = opt.Contains("Stv");
if (stv) { StvDebug::SetDebug(11);}
else     { StiDebug::SetDebug(11);}

#if 1
int sim =0;
if (fil.Contains(".fz")) sim = 1;
if (!sim) {
rmMaker(chain,"geant");
} else {
gSystem->Setenv("STARFPE","NO");
}
#endif

chain->Init();
if (KNN) chain->SetAttr("seedFinders","kn","Stv");

chain->EventLoop(30);
chain->Finish();
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

