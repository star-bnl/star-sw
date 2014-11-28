void res2(TString baseName="R_dzVsTanL_3")
{
  TCanvas * c = new TCanvas();
  TString name;
  TString name1;
  TString name2;
  TString nameGif;
  for (int layer=0;layer<6;++layer)
    {
      int nSector;
      switch (layer)
	{
	case 0: nSector = 4; break;
	case 1: nSector = 4; break;
	case 2: nSector = 6; break;
	case 3: nSector = 6; break;
	case 4: nSector = 8; break;
	case 5: nSector = 8; break;
	}
      for (int sector=0;sector<nSector;++sector)
	{
	  name = baseName+"_L"+layer+"_S"+sector;
	  name1 = name + "_1";
	  name2 = name + "_2";
	  nameGif = name+".gif";
	  TH2D * h2 = (TH2D *) gDirectory->Get(name);
	  h2->FitSlicesY(0,40,60,10);
	  h2->Draw("ZCOL");
	  TH1D * mean = (TH1D*) gDirectory->Get(name1);
	  TH1D * rms  = (TH1D*) gDirectory->Get(name2);
	  h2->Draw("ZCOL");
	  mean->SetLineColor(4);
	  mean->SetMarkerColor(4);
	  mean->SetLineWidth(3);
	  mean->Draw("SAME");
	  //rms->SetLineColor(4);
	  //rms->SetMarkerColor(4);
	  //rms->SetLineWidth(3);
	  //rms->Draw("SAME");
	  c->Print(nameGif);
	}
    }
}

  /*
  R_dzVsTanL_3_L0_S0.FitSlicesY(0,40,60,10);
  R_dzVsTanL_3_L0_S0.Draw("ZCOL");
  R_dzVsTanL_3_L0_S0_1.SetLineColor(4);
  R_dzVsTanL_3_L0_S0_1.SetLineWidth(2);
  R_dzVsTanL_3_L0_S0_1.SetMarkerColor(4);
  R_dzVsTanL_3_L0_S0_1.Draw("SAME");
    
  TH1D *h2_1 = (TH1D*)gDirectory->Get("h2_1");

  */

