
void saveTrackQA(Char_t* signalFile="signalShapes.root")
{
  gStyle->SetPalette(1);
  TFile f(signalFile);
  Char_t buffer[100];
  Char_t quadName[10];
  Char_t layerName[10];


  Char_t* trkChi2="";
  Char_t* numTrkPerEv="numTracksPerEvent";
  Char_t* numPointsPerTrack="numPointsPerTrack";
  Char_t* vtxDist="z_Vtx_From_trk_fit";
  Char_t* ipProj="ProjToIP";

  TCanvas c;
  TH1D* h=(TH1D*)f.Get(numTrkPerEv);
  if(h!=0)
    {
      h->SetTitle("Number of Tracks per Event");
      h->SetName(numTrkPerEv);
      h->Draw();
      h->GetYaxis()->SetTitle("dN");
      h->GetXaxis()->SetTitle("number of tracks in ev");
      h->SetFillColor(kYellow-9);
      c.SetLogy();
      c.SaveAs("numTrkPerEv.png");
    }
  
  h=(TH1D*)f.Get(numPointsPerTrack);
  if(h!=0)
    {
      h->SetTitle("Number of Points per Track");
      h->SetName(numPointsPerTrack);
      h->SetFillColor(kYellow-9);
      h->Draw();
      c.SaveAs("numPointsPerTrack.png");
    }
      c.SetLogy(false);
	h=(TH1D*)f.Get(vtxDist);
      if(h!=0)
    {
      h->SetTitle("Vertex Distribution");
      h->SetName(vtxDist);
      h->GetXaxis()->SetTitle("z [cm]");
      h->GetYaxis()->SetTitle("dN");
      h->SetFillColor(kYellow-9);
      h->Draw();
      c.SaveAs("zVtxDist.png");

    }

      TH2D* h2=(TH2D*)f.Get(ipProj);
      if(h2!=0)
	{
	  h2->SetTitle("Vertex Distribution");
	  h2->SetName(vtxDist);
	  h2->Draw("colz");
	  h2->GetXaxis()->SetTitle("z [cm]");
	  h2->GetYaxis()->SetTitle("dca [cm]");
	  c.SaveAs("z_Dca.png");
	}

      c.SetLogy(false);
  for(int iD=1;iD<7;iD++)
    {
      for(int iQ=0;iQ<4;iQ++)
	{
	  if(iQ==0)
	    sprintf(quadName,"A");
	  if(iQ==1)
	    sprintf(quadName,"B");
	  if(iQ==2)
	    sprintf(quadName,"C");
	  if(iQ==3)
	    sprintf(quadName,"D");
	  for(int iL=0;iL<2;iL++)
	    {
	      if(iL==0)
		sprintf(layerName,"P");
	      else
		sprintf(layerName,"R");
	      sprintf(buffer,"chargeTrackCluster%s_disc%d_quad%d",layerName,iD,iQ);
	      cout <<"loading " << buffer <<endl;
	      TH1D* h=(TH1D*)f.Get(buffer);
	      TF1* fLandau=new TF1("fLandau","landau(0)",0,10000);
	      h->Fit(fLandau);
	      h->GetXaxis()->SetTitle("cluster charge [ACD counts]");
	      h->GetYaxis()->SetTitle("dN");
	      h->SetFillColor(kYellow-9);
	      h->Draw();
	      sprintf(buffer,"%s.png",buffer);
	      cout <<"save as " << buffer <<endl;
	      c.SaveAs(buffer);
	    }
	}
    }
}
