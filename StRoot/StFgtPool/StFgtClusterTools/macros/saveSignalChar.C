
void saveSignalChar(Char_t* signalFile="signalShapes.root")
{
  gStyle->SetPalette(1);
  TFile f(signalFile);
  Char_t buffer[100];
  Char_t quadName[10];
  Char_t layerName[10];

  for(int iD=1;iD<7;iD++)
    {
      for(int iQ=0;iQ<4;iQ++)
	{
	  if(iD>1 && iQ >1)
	    continue;
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
	      sprintf(buffer,"Canvas_Disc_%id_Quad%s_Layer%s",iD,quadName,layerName);
	      TCanvas c(buffer,buffer,10,10,3000,2000);
	      c.Divide(3,3);
	      c.cd(1);
	      sprintf(buffer,"numFSigCloseCluster%s_disc%d_quad%d",layerName,iD,iQ);
	      cout <<"loading " << buffer <<endl;
	      //	     TCanvas c2;
	      TH1D* h=(TH1D*)f.Get(buffer);
	      sprintf(buffer,"numFSigCloseCluster%s_disc%d_quad%s",layerName,iD,quadName);
	      if(h!=0)
		{
		  h->SetTitle(buffer);
		  h->SetName(buffer);
		  h->SetFillColor(kYellow-9);
		  h->Draw();
		}
	      sprintf(buffer,"numFSigCloseCluster%s_disc%d_quad%s.png",layerName,iD,quadName);
	      //	     c2.SaveAs(buffer);
	      c.cd(2);
	      sprintf(buffer,"maxTbCloseCluster%s_disc%d_quad%d",layerName,iD,iQ);
	      cout <<"loading " << buffer <<endl;
	      TH1D* h=(TH1D*)f.Get(buffer);
	      sprintf(buffer,"maxTbCloseCluster%s_disc%d_quad%s",layerName,iD,quadName);
	      if(h!=0)
		{
		  h->SetName(buffer);
		  h->SetTitle(buffer);
		  h->SetFillColor(kYellow-9);
		  h->Draw();
		}
	      c.cd(3);
	      sprintf(buffer,"maxAdcCloseCluster%s_disc%d_quad%d",layerName,iD,iQ);
	      cout <<"loading " << buffer <<endl;
	      TH1D* h=(TH1D*)f.Get(buffer);
	      sprintf(buffer,"maxAdcCloseCluster%s_disc%d_quad%s",layerName,iD,quadName);
	      if(h!=0)
		{
		  h->SetName(buffer);
		  h->SetTitle(buffer);
		  h->SetFillColor(kYellow-9);
		  h->Draw();
		}

	      c.cd(4);
	      sprintf(buffer,"maxAdcTrackCluster%s_disc%d_quad%d",layerName,iD,iQ);
	      cout <<"loading " << buffer <<endl;
	      TH1D* h=(TH1D*)f.Get(buffer);
	      sprintf(buffer,"maxAdcTrackCluster%s_disc%d_quad%s",layerName,iD,quadName);
	      if(h!=0)
		{
		  h->SetName(buffer);
		  h->SetTitle(buffer);
		  h->SetFillColor(kYellow-9);
		  h->Draw();
		}
	      c.cd(5);
	      sprintf(buffer,"numFirstHighCloseCluster%s_disc%d_quad%d",layerName,iD,iQ);
	      cout <<"loading " << buffer <<endl;
	      TH1D* h=(TH1D*)f.Get(buffer);
	      sprintf(buffer,"maxFirstHighCloseCluster%s_disc%d_quad%s",layerName,iD,quadName);
	      if(h!=0)
		{
		  h->SetName(buffer);
		  h->SetTitle(buffer);
		  h->SetFillColor(kYellow-9);
		  h->Draw();
		}
	      c.cd(6);
	      sprintf(buffer,"r_phi_ChargeCorrelationInDisk_%d_quad_%d",iD,iQ);
	      cout <<"loading " << buffer <<endl;
	      TH2D* h2=(TH2D*)f.Get(buffer);
	      sprintf(buffer,"r_phi_ChargeCorrelationInDisk_%d_quad_%s",iD,quadName);
	      if(h2!=0)
		{
		  h2->SetName(buffer);
		  h2->SetTitle(buffer);
		  h2->GetXaxis()->SetRangeUser(0,15000);
		  h2->GetYaxis()->SetRangeUser(0,15000);
		  h2->Draw("colz");
		}
	      sprintf(buffer,"EnergyForDisk%d_Quad%s_layerR.png",iD,quadName,layerName);
	      TH1D* projR=h2->ProjectionX(buffer);
	      sprintf(buffer,"EnergyForDisk%d_Quad%s_layerP.png",iD,quadName,layerName);
	      TH1D* projP=h2->ProjectionY(buffer);
	      c.cd(7);
	      projR->SetFillColor(kYellow-9);
	      projR->Draw();
	      c.cd(8);
	      projP->SetFillColor(kYellow-9);
	      projP->Draw();
	      c.cd(9);
	      sprintf(buffer,"clusterSize%sInDisk_%d_quad_%d",layerName,iD,iQ);
	      TH1D* hClusSize=(TH1D*)f.Get(buffer);
	      if(hClusSize!=0)
		{
		  hClusSize->SetFillColor(kYellow-9);
		  hClusSize->GetXaxis()->SetTitle("number of strips");
		  hClusSize->GetXaxis()->SetRangeUser(0,10);
		  hClusSize->Draw();
		}
	      sprintf(buffer,"signalInfoForDisk%d_Quad%s_layer%s.png",iD,quadName,layerName);
	      c.SaveAs(buffer);

	    }
	}
    }

}
