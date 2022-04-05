eff(){
  c1=new TCanvas("yieldvspt","mass",500,500);
  c1->Divide(1,1);
  c2=new TCanvas("yieldvse","msass",500,500);
  c2->Divide(1,1);
  c3=new TCanvas("mass","msass",500,500);
  c3->Divide(1,1);
 
  f=new TFile("test.root");
  //gFile->cd("pions");
  h0=(TH1F*) gPt->Clone();
  h1=(TH1F*) McPt->Clone();
  h2=(TH1F*) gEnergy->Clone();
  h3=(TH1F*) McEnergy->Clone();
  h1->Clear();
  h1->SetTitle("efficiency vs pt");
  h3->Clear();
  h3->SetTitle("efficiency vs energy");
  int nb1=h1->GetNbinsX();
  //printf("nb1=%d\n",nb1); 
  int nb2=h3->GetNbinsX();
  int i;
  for(i=1;i<=nb1;i++)
    {
      float n0=h0->GetBinContent(i);
      float n1=h1->GetBinContent(i);
      //printf("n0=%f\n",n0);      
      float eff,error;
      if(n0==0)
	{
	  eff=0;
	  error=0;
	}
      else
	{
	  eff=n1/n0;
	  error=sqrt(n1*(n0-n1)/pow(n0,3));
	}
    
      h1->SetBinContent(i,eff);
      h1->SetBinError(i,error);
    }
  for(i=1;i<=nb2;i++)
    {
      float n2=h2->GetBinContent(i);
      float n3=h3->GetBinContent(i);
      //printf("n2=%f\n",n2);      
      float eff,error;
      if(n2==0)
	{
	  eff=0;
	  error=0;
	}
      else
	{
	  eff=n3/n2;
	  error=sqrt(n3*(n2-n3)/pow(n2,3));
	}
    
      h3->SetBinContent(i,eff);
      h3->SetBinError(i,error);
    }
  
  c1->cd(1);
  h1->SetStats(kFALSE);
  h1->Draw();
  h1->Fit("pol0","R","",7.5,15.0);
  h1->SetLineColor(4);
  c1->Print("yieldvspt.gif");
  c2->cd(1);
  h3->SetStats(kFALSE);
  h3->Draw();
  h3->Fit("pol0","R","",15.0,35.0);
  h3->SetLineColor(4);
  c2->Print("yieldvse.gif");
  //THStack *hs=new THStack("hs","stacked histogram");

  c3->cd(1);
  hMassAny->Draw();
  //hMassAny->Fit("gaus","R","",0.08,0.18);
  hMassAny->SetLineColor(4);
  c3->Print("mass.gif");


 
}
