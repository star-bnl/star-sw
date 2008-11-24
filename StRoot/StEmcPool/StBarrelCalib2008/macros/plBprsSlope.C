plBprsSlope(int id0=1, int page=2 ) {
 gStyle->SetOptStat(1110);
  gStyle->SetOptFit(1);
  gStyle->SetPalette(1,0);
 
  char *fnameO="calib-nov-21-2008/barrelMipSpectV6ok.hist.root"; 
  fd=new TFile(fnameO); assert(fd->IsOpen());
  printf("Read %s\n", fd->GetName()); 
  //fd->ls();


  switch(page) {
  case 1: 
    {
    c=new TCanvas("aa","aa",1000,600); c->Divide(5,4);
    int k=1;
    for(int box=1; box<=4; box++)
    for(int pmt=1;pmt<=5;pmt++) 
      { char tt[100];
	sprintf(tt,"gainBPM%d_%d",box,pmt);
	
	TH1F *h=fd->Get(tt); 
	if(h==0) { 	printf("missing=%s \n",tt); k++; continue;}
	c->cd(k);
	h->Draw(); h->SetMaximum(10);
	k++;
      }   
   
  }   break;
    //........................

  case 2: 
    { gStyle->SetOptStat(0);
      int id1=id0, id2=id1+100;
      char tt[100];
      sprintf(tt,"BPRS softID [%d,%d]",id1,id2);	
      TH2F *h2Cr=new TH2F("aa5","BPRS comparison (both axis); slope (raw) ; average MIP (fit)",20,-0.15,-0.015,20,0,40);
      h2Cr->SetTitle(tt);
      c=new TCanvas(tt,tt,450,350);
      h2Cr->Draw("box"); gPad->SetGrid();
      for(int id=id1;id<=id2;id++) {
	sprintf(tt,"bprs%da",id);	
	TH1F *h=(TH1F *)fd->Get(tt); 
	if(h==0) { printf("missing=%s \n",tt);continue;}
	TF1 *ffS=h->GetFunction("expo");
	if(ffS==0) continue;
	float slope=ffS->GetParameter(1);
	
	sprintf(tt,"bprs%dm",id);	
	h=(TH1F *)fd->Get(tt); 
	if(h==0) { printf("missing=%s \n",tt);continue;}
	ffS=h->GetFunction("gaus");
	if(ffS==0) continue;
	float mean=ffS->GetParameter(1);
	
	printf("id=%d sl=%f mean=%f\n",id,slope,mean);
	//	h->Draw();
	h2Cr->Fill(slope,mean);
    }   
   
  }   break;
    //........................
    //........................
 default: 
  } // case END
}

