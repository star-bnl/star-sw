layerOcc(TString baseN)
{


  TH1D *layerOcc = new TH1D("layerOcc","",55,0,54.);
  TString thisL;
  TH1D *hI=0;
  for(int i=0; i<55;i++)
    {
      thisL=baseN+i;
      hI=(TH1D*)gDirectory->Get(thisL);


      if(hI) layerOcc->SetBinContent(i,hI->GetEntries());
    }

  layerOcc->Draw();

}
