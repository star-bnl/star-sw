void runPP(Int_t n=200000000)
{
  Float_t ERROR=0.;
  //gSystem->Load("MyDecay.so");
  MyDecay *d=new MyDecay("gammaDecayPP");
  d->setPP05(kTRUE);
//  d->setETAMTSCALE(0.48);
  d->setETAMTSCALE(0.46);
  d->doDecay(n);
}
