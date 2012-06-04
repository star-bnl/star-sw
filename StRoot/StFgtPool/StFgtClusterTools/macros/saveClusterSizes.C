
void saveClusterSizes(Char_t* signalFile="signalShapes.root")
{
  //clusterSizePInDisk_6_quad_3-
  TFile f(signalFile);
  Char_t buffer[100];
  Char_t quadName[10];
  Char_t layerName[10];
  Int_t counter=0;
  sprintf(buffer,"ClusterSizes");
  TCanvas c(buffer,buffer,10,10,4000,3000);
  c.Divide(6,5);
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
	      counter++;

  //clusterSizePInDisk_6_quad_3-
	      sprintf(buffer,"clusterSize%sInDisk_%d_quad_%d",layerName,iD,iQ);
	      cout <<"loading " << buffer <<" counter: " << counter <<endl;
	      //	     TCanvas c2;
	      TH1D* h=(TH1D*)f.Get(buffer);
	      h->SetFillColor(kYellow);
	      sprintf(buffer,"ClusterSize%s_disc%d_quad%s",layerName,iD,quadName);
	      c->cd(counter);
	      if(h!=0)
		{
		  h->SetTitle(buffer);
		  h->SetAxisRange(0.0,10);
		  h->SetName(buffer);
		  h->Draw();
		}

	    }
	}
    }
  sprintf(buffer,"ClusterSizes.png");
  c.SaveAs(buffer);
  sprintf(buffer,"ClusterSizes.C");
  c.SaveAs(buffer);
}
