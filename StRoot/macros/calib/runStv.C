class StMaker;
void rmMaker(const StMaker *top,const char *key);
int runStv(const char *daqFile,const char *flg,int nEvents)
{
gROOT->LoadMacro("bfc.C");
gROOT->LoadMacro("$STAR/StarVMC/GeoTestMaker/GeoTestLoad.C");
GeoTestLoad();
gSystem->Load("/usr/lib/mysql/libmysqlclient_r.so");
TString opt(flg),fil(daqFile);
int fr=1,to=nEvents;
int ierr = 0;

opt += " StiPulls";


bfc(-1,opt,fil);
// StMaker::lsMakers(chain);
// chain->SetAttr(".call","SetDebug(1)","St_db_Maker::");
// chain->SetAttr(".call","SetActive(0)","St_geant_Maker::");
// chain->SetAttr(".call","SetActive(0)","StTofHitMaker::");
// chain->SetAttr(".call","SetActive(0)","StTofrMatchMaker::");
// chain->SetAttr(".call","SetActive(0)","StTofCalibMaker::");

chain->SetAttr(".call","SetActive(0)",".*Tof.*Maker::");
chain->SetAttr(".call","SetActive(0)",".*Emc.*Maker");
chain->SetAttr(".call","SetActive(0)","dEdxY2");

StMaker *mk=0;
gSystem->Load("StvUtil.so");
gSystem->Load("Stv.so");
gSystem->Load("StvMaker.so");
mk = new StvMaker();
chain->AddAfter("Sti",mk);

rmMaker(chain,"::geant");
rmMaker(chain,"MuDst.*");
rmMaker(chain,"StFtpc.*::");
rmMaker(chain,"StEmc.*::");
rmMaker(chain,"StEEmc.*::");
rmMaker(chain,"Sti");
rmMaker(chain,"StKinkMaker");
rmMaker(chain,"StXiFinderMaker");


//StMaker::lsMakers(chain);

chain->Init();

chain->EventLoop(nEvents);
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

