{
CShape->ProjectionY("bin"); bin->SetNormFactor(1.); bin->Draw();
for (int i=6; i<20; i++) {
  char line[20];
  sprintf(line,"bin%i",i);
  TString name(line);
  CShape->ProjectionY(name.Data(),i,i); 
  TH1 *h = (TH1 *) gROOT->FindObject(name.Data());
  h->SetNormFactor(1.); 
  h->SetLineColor(i);
  h->Draw("same");
}
}
