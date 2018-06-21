// Conver Zebra file into ROOT format

class StMaker;
void rmMaker(const StMaker *top, const char *key);

void fz2root(const char *file=
            "/star/rcf/simu/auau200/hijing/b0_20/inverse/year2001/hadronic_on/gstardata/rcf0191_01_380evts.fzd"
            , unsigned int nEvent=99999) 
{
   gSystem->Setenv("STARFPE","NO");
   gROOT->LoadMacro("bfc.C");
   bfc(-1,"fzin, GeantOut",file);

   rmMaker(chain,"St_db_Maker::db");
   rmMaker(chain,"StMagFMaker::MagField");
   rmMaker(chain,"StDetectorDbMaker::detDb");

   chain->SetDEBUG(0);

   StMaker::lsMakers(chain);
   chain->Init();
   chain->EventLoop(nEvent);
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
// QA :INFO  -    St_geant_Maker::geant
// QA :INFO  -    St_db_Maker::db
// QA :INFO  -    StMagFMaker::MagField
// QA :INFO  -    StDetectorDbMaker::detDb
// QA :INFO  -    StTreeMaker::outputStream
// QA :INFO  -  StBFChain::bfc
// QA :INFO  -    St_geant_Maker::geant
// QA :INFO  -    St_db_Maker::db
// QA :INFO  -    StMagFMaker::MagField
// QA :INFO  -    StDetectorDbMaker::detDb
// QA :INFO  -    StTreeMaker::outputStream
