class EEfeeDataBlock;
class EEfeeRawEvent;
class EEfeeRunDescr;

TFile *f;

void rdFeeTTree() {
  
  gSystem->Load("EEfeeRaw.so");
  TFile   *f  = new TFile("myFeeMC-minb200-100.root");
  TTree   *t  = (TTree *)f->Get("fee");
  TBranch *bd = t->GetBranch("desc");
  TBranch *be = t->GetBranch("evt");
 
  EEfeeRawEvent  *eve = new  EEfeeRawEvent();
  EEfeeRunDescr  *des = new  EEfeeRunDescr();

  be->SetAddress(&eve);
  bd->SetAddress(&des);

  Int_t nentries = (Int_t)t->GetEntries();
  cout << nentries << endl;
  Int_t nbe=0;
  Int_t nbd=0;
  for(Int_t i=0; i<nentries; i++) {
    nbe += be->GetEntry(i);
    nbd += bd->GetEntry(i);
    des->print();
    eve->print();
  }

}



