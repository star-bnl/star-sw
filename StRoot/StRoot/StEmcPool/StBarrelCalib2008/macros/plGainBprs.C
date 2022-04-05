plGainBprs(int page=1, int box0=0) {
 gStyle->SetOptStat(1110);
  gStyle->SetOptFit(1);
  gStyle->SetPalette(1,0);
 
  char *fnameO="calib-nov-21-2008/barrelMipSpectV6ok.hist.root"; 
  fd=new TFile(fnameO); assert(fd->IsOpen());
  printf("Read %s\n", fd->GetName()); 
  //fd->ls();

  char *fname1="mipGainBprs+Btow_v6.hist.root";
  fout=new TFile(fname1,"update"); assert(fout->IsOpen());
  printf("Write output to %s\n", fout->GetName()); 


  switch(page) {
  case 1: 
    {
    c=new TCanvas("aa","aa",1000,600); c->Divide(5,4);
    int k=1;
    for(int box=1; box<=4; box++)
    for(int pmt=1;pmt<=5;pmt++) 
      { char tt[100];
	sprintf(tt,"gainBPM%d_%d",box+box0,pmt);
	
	TH1F *h=fd->Get(tt); 
	if(h==0) { 	printf("missing=%s \n",tt); k++; continue;}
	c->cd(k);
	h->Draw(); h->SetMaximum(10);
	k++;
      }   
   
  }   break;
    //........................
  case 2:  // BPRS
  case 3:  // BTOW
    {
      // QA of gains
      float cut_minYield=20;
      float cut_mpvRE=0.5; // relative error of MPV
      float cut_mipL=1.5; // lowest acceptable MIP peak position
      gStyle->SetOptStat(0);

      // setup for BPRS
      TString tt1="bprsMipGain2D", tt3="bprsMipGain", tt4="bprsMipSig", tt5="bprsMipStat";
      char  *core1="bprs";
      if(page==3) { // changes for BTOW
	tt1="btowMipGain2D"; tt3="btowMipGain"; tt4="btowMipSig"; tt5="btowMipStat";
	core1="btow";
      }
 
      float gMax=30;
      c=new TCanvas(tt1,tt1,1200,800);
      h2g=new TH2F(tt1,tt1+"  Z=MIP position(ADC); phiBin, X=1+(softID-1)/20 ; etaBin, Y=1+(softID-1)%20",240,0.5,240.5,20,0.5,20.5);
      h1g=new TH1F(tt3,tt3+" ; softID; MIP ADC",4800,0.5,4800.5);
      h1sig=new TH1F(tt4,tt4+"; softID;  MIP sigma",4800,0.5,4800.5);
      h1stat=new TH1F(tt5,tt5+"; softID;  MIP status",4800,0.5,4800.5);
      h1g->GetYaxis()->SetTitleSize(0.055);
      h1sig->GetYaxis()->SetTitleSize(0.055);
      h1stat->GetYaxis()->SetTitleSize(0.055);
      
      pad=new TPad("pad2", "apd2",0.0,0.7,1.,1.); pad->Draw();
      pad->cd();h2g->Draw("colz");  h2g->SetMaximum(gMax); gPad->SetGrid();
      if(page==3) markBtow2D();
      if(page==2) markBprs2D();

      c->cd(); pad=new TPad("pad1", "apd1",0.0,0.0,1.,0.7); pad->Draw();
      pad->Divide(1,2); 
      pad->cd(1);h1g->Draw();  if(page==2) markBprs1D();
      pad->cd(2);h1sig->Draw();gPad->SetGrid(); if(page==2) markBprs1D();
      //tmp   pad->cd(3);h1stat->Draw(); gPad->SetGrid();if(page==2) markBprs1D();
      //return;
      //............. LOOP over towers ........
      for(int id=1;id<=4800;id++) {
	char txt[100];
	sprintf(txt,"%s%dm",core1,id);
	TH1F *h=fd->Get(txt); 
	if(h==0) { h1stat->Fill(id,1);	printf("missing=%s \n",txt);  continue;}
	//	h->Draw();
	float yield=h->GetEntries();
	if(yield<cut_minYield) {h1stat->Fill(id,2);	printf("  low MIP yield=%s %d \n",txt,yield);  continue;}
	  
	TF1 *ff=h->GetFunction("gaus");
	float mean=ff->GetParameter(1);
	float meanE=ff->GetParError(1);
	float sig=ff->GetParameter(2);
	float mpvRE=fabs(meanE/mean);

	if( (mpvRE>cut_mpvRE) ||
	    (mean<1 || mean>40) ) {
	  mean=h->GetMean();
	  sig=h->GetRMS();
	  if(mean< cut_mipL || mean>40) { h1stat->Fill(id,4);	printf("  too low2 MIP=%s %f\n",txt,mean);  continue;}

	  meanE=sig/sqrt(yield);
	  mpvRE=fabs(meanE/mean);
	  printf("   %s bad fit, use mean/RMS\n", txt);
	}
	if( mpvRE>cut_mpvRE) { h1stat->Fill(id,8);	printf("   bad mpvRE=%s %f\n",txt,mpvRE);  continue;}
	printf("id=%d yield=%d  MIP=%.1f +/- %.1f\n",id,yield, mean,sig);
	int ix=1+(id-1)/20;
	int iy=1+(id-1)%20;
	h2g->Fill(ix,iy,mean);
	h1g->SetBinContent(id,mean);
	h1g->SetBinError(id,meanE);
	h1sig->SetBinContent(id,sig);
      }
      int nKill=(int) h1stat->GetEntries();
      tt5="nKilled="; tt5+=nKill;   tx=new TText(-500,26,tt5); tx->Draw(); tx->SetTextSize(0.08);tx->SetTextColor(kRed);

      fout->cd();
      h2g->Write();
      h1g->Write();
      h1sig->Write();
      h1stat->Write();
      
   }   break;
    //........................
 default: 
  } // case END
}


//-----------------------
void markBtow2D() {
  lnEW=new TLine(120.5,0, 120.5,23); lnEW->Draw(); lnEW->SetLineColor(kBlue);
  tx=new TText(100,21.7,"West"); tx->Draw();  tx->SetTextColor(kBlue);  
  tx=new TText(140,21.7,"East"); tx->Draw();  tx->SetTextColor(kBlue);

  tx=new TText(-5,-1.5,"module=1"); tx->Draw();  tx->SetTextColor(kBlue);
  tx=new TText(108,-1.5,"module=60,61"); tx->Draw();  tx->SetTextColor(kBlue);
  tx=new TText(240,-1.5,"module=120"); tx->Draw();  tx->SetTextColor(kBlue);  
}
//-----------------------
void markBprs2D() {
  lnEW=new TLine(120.5,-2, 120.5,23); lnEW->Draw(); lnEW->SetLineColor(kBlue);
  tx=new TText(100,21.7,"West"); tx->Draw();  tx->SetTextColor(kBlue);  
  tx=new TText(140,21.7,"East"); tx->Draw();  tx->SetTextColor(kBlue);

  lnEW=new TLine(17.5,-2, 17.5,21); lnEW->Draw(); lnEW->SetLineColor(kBlue);
  lnEW=new TLine(77.5,-2, 77.5,21); lnEW->Draw(); lnEW->SetLineColor(kBlue);
 
  lnEW=new TLine(145.5,-2, 145.5,21); lnEW->Draw(); lnEW->SetLineColor(kBlue);
  lnEW=new TLine(205.5,-2, 205.5,21); lnEW->Draw(); lnEW->SetLineColor(kBlue);

  tx=new TText(1,-1.5,"PSD-1W"); tx->Draw();  tx->SetTextColor(kBlue);
  tx=new TText(80,-1.5,"PSD-1W"); tx->Draw();  tx->SetTextColor(kBlue);
  tx=new TText(40,-1.5,"PSD-19W"); tx->Draw();  tx->SetTextColor(kBlue);

  tx=new TText(206,-0.5,"PSD-1E"); tx->Draw();  tx->SetTextColor(kBlue);
  tx=new TText(130,-1.5,"PSD-1E"); tx->Draw();  tx->SetTextColor(kBlue);
  tx=new TText(180,-1.5,"PSD-20E"); tx->Draw();  tx->SetTextColor(kBlue);

}

//-----------------------
void markBprs1D() {
  float y=500;
  lnEW=new TLine(2400,-2, 2400,y); lnEW->Draw(); lnEW->SetLineColor(kBlue);
  //  tx=new TText(100,21.7,"West"); tx->Draw();  tx->SetTextColor(kBlue);  
  // tx=new TText(140,21.7,"East"); tx->Draw();  tx->SetTextColor(kBlue);

  lnEW=new TLine(340,0,340,y); lnEW->Draw(); lnEW->SetLineColor(kBlue);
  lnEW=new TLine(1540,0,1540,y); lnEW->Draw(); lnEW->SetLineColor(kBlue);
 
  lnEW=new TLine(2900,0,2900,y); lnEW->Draw(); lnEW->SetLineColor(kBlue);
  lnEW=new TLine(4100,0,4100,y); lnEW->Draw(); lnEW->SetLineColor(kBlue);

  tx=new TText(10,-2,"PSD-1W"); tx->Draw();  tx->SetTextColor(kBlue);
  tx=new TText(1600,-2,"PSD-1W"); tx->Draw();  tx->SetTextColor(kBlue);
  tx=new TText(1000,-2,"PSD-19W"); tx->Draw();  tx->SetTextColor(kBlue);

  tx=new TText(2600,-2,"PSD-1E"); tx->Draw();  tx->SetTextColor(kBlue);
  tx=new TText(3200,-2.,"PSD-1E"); tx->Draw();  tx->SetTextColor(kBlue);
  tx=new TText(4200,-2.,"PSD-20E"); tx->Draw();  tx->SetTextColor(kBlue);

}
