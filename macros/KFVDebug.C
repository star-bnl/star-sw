void KFVDebug() {
  StKFVertexMaker *kfv = (StKFVertexMaker *) chain->Maker("KFVertex");
  if (! kfv) return;
  kfv->SetDebug(3);
  TCanvas *c1 = new TCanvas("c1","c1",1400,500);
  kfv->SetCanvas(c1);
  chain->MakeEvent();
}
