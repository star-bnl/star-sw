// Loop through detector layers to report 
// various parameters
residu(TString baseName, double min=-1.5, double max=1.5)
{
  double xMin, xMax;
  TString name;
  name = baseName;
  name += "_a";     TH1D * amp  = new TH1D(name,name,55,0,54.);
  name = baseName;
  name += "_mean";  TH1D * mean = new TH1D(name,name,55,0,54.);
  name = baseName;
  name += "_rms";   TH1D * rms = new TH1D(name,name,55,0,54.);
  TString thisL;
  TH1D *h=0;
  TF1 * f;
  int ii;
  for(int i=0; i<51;i++)
    {
      thisL=baseName+i;
      if (i<6)
	{
	  xMin = -0.2;
	  xMax = 0.2;
	}
      else
	{
	  xMin = min;
	  xMax = max;
	}
      h=(TH1D*)gDirectory->Get(thisL);
      h->Fit("gaus","","",xMin,xMax);
      double entries = h->GetEntries();
      if (entries>0)
	{
	  ii = i+1;
	  f = h->GetFunction("gaus");
	  double a = f->GetParameter("Constant"); double ea = f->GetParError(0);
	  double m = f->GetParameter("Mean");double em = f->GetParError(1);
	  double r = f->GetParameter("Sigma");double er = f->GetParError(2);
	  amp->SetBinContent(ii,a);amp->SetBinError(ii,ea);
	  mean->SetBinContent(ii,m);mean->SetBinError(ii,em);
	  rms->SetBinContent(ii,r);rms->SetBinError(ii,er);
	}
    }
  cout << "Fitting completed" << endl;

  c1->Clear();
  c1->Divide(1,3);
  c1->cd(1); amp->Draw();
  c1->cd(2); mean->Draw();
  c1->cd(3); rms->Draw();
  
}
