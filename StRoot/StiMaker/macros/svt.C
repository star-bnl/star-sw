
svt(TString baseName, double min=-1.5, double max=1.5)
{
  TString thisL;
  TString thisL2;
  TH1D *h=0;
  TF1 * f;
  int ii;
  for(int i=0; i<6;i++)
    {
      thisL=baseName+i;
      thisL2=baseName+i+".gif";
      h=(TH1D*)gDirectory->Get(thisL);
      h->Fit("gaus","","",min,max);
      c1->Print(thisL2);
    }

}
