class EEfeeDataBlock;
class EEfeeRawEvent;
class EEfeeRunDescr;

TFile *f;

void rdFeeTTree(int max=200000) {
  gSystem->Load("StRoot/StEEmcUtil/EEfeeRaw/libEEfeeRaw.so");
  
  TString fname="run00006.root";
  fname="miniDaq/feb24/run00003.root";
  fname="/star/u/eemcdb/ezdaqRead/fee.root";

  TFile   *f  = new TFile(fname);
  TTree   *t  = (TTree *)f->Get("fee");
  TBranch *bd = t->GetBranch("desc");
  TBranch *be = t->GetBranch("evt");
 
  EEfeeRawEvent  *eve = new  EEfeeRawEvent();
  EEfeeRunDescr  *des = new  EEfeeRunDescr();

  be->SetAddress(&eve);
  bd->SetAddress(&des);

  Int_t nentries = (Int_t)t->GetEntries();
  printf(" N entries=%d\n",nentries);
  if(nentries<=0) {
    printf(" file=%s is empty, STOP\n",fname.Data());
    return;
  }

  Int_t nbe=0;
  Int_t nbd=0;
  for(Int_t i=0; i<nentries; i++) {
    nbe += be->GetEntry(i);
    nbd += bd->GetEntry(i);
    if(i%1000==0)des->print();
    //eve->print();
    if(i>=max) break;
  }

}



