void runDAU(Int_t n=200000000)
{
  Float_t ERROR=0.;
  //gSystem->Load("MyDecay.so");
  MyDecay *d=new MyDecay("gammaDecayDAU");
  d->setDAU(kTRUE);
//  d->setETAMTSCALE(0.47);
  d->setETAMTSCALE(0.44);
  d->doDecay(n);
}
