class TH1F;
enum {mxH=2, mxQ=2};
TH1F *hA[mxQ][mxH];

void setWALmodel(int plot=1) {
  TString outF="WALModel_a.hist.root";
  TFile *fd=new TFile(outF,"recreate");

  //........... initialize histos .....
  TString titA[mxH]={"AL","ALL"};
  
  for (int q=0;q<mxQ;q++) {
    TString Q="P"; if (q) Q="N";
    for (int i=0;i<mxH; i++) {
      TString tit=+titA[i]+"_"+Q;
      hA[q][i]=newEtaBinHisto("modW_"+tit,"model W "+tit);
      if(i==0) hA[q][i]->SetLineColor(kRed);
      if(i==1) hA[q][i]->SetLineColor(kGreen);
    }
  }
  
  //......... populate bins, use errors if known 
  //fillDataBin20();//for barrel-integral based on 2009 analysis

  //......... populate bins with arbitrary model shape
    for (int iq=0;iq<mxQ;iq++) 
      fillDataBin11_18(iq); 

    if(plot) { // special case
      ln=new TLine(10,0,20,0);
      c=new TCanvas();
      gStyle->SetOptStat(0);
      c->Divide(2,2);
      float yMx=0.9; int k=1;
      for (int i=0;i<mxH; i++) {
	for (int q=0;q<mxQ;q++) {
	  c->cd(k++);
	  TH1F *h=hA[q][i];
	  h->Draw(); h->SetFillColor(16);
	  h->SetMinimum(-yMx); h->SetMaximum(yMx);
	  h->SetAxisRange(11,18);
	  gPad->SetGrid();
	  ln->Draw();

	  if(i==0) {
	    ar=new TArrow(10.6,yMx*0.9,18.3,yMx*0.9, 0.025);  ar->Draw(); ar->SetLineColor(kBlue);
	    tx1=new TLatex(10.8,yMx*.7,"-1.3  "); tx1->Draw(); tx1->SetTextColor(kBlue);	  	   
	    tx1=new TLatex(14.8,yMx*.7," polBeam #eta        1.3"); tx1->Draw(); tx1->SetTextColor(kBlue);	  
	  }
	  if(i==1) {
	    ar=new TArrow(14.6,yMx*0.9,18.3,yMx*0.9, 0.025);  ar->Draw(); ar->SetLineColor(kBlue);
	    tx1=new TLatex(14.8,yMx*.7,"0     polBeam #eta        1.3"); tx1->Draw(); tx1->SetTextColor(kBlue);	  
	  }
	}
      }
      printf("WARN: histos NOT saved\n");
      return;
    }
      

  fd->Write();  
  fd->ls();
  fd->Close();

}

//------------------
TH1F *newEtaBinHisto(TString name="aa", TString title="bb", TString yLable=""){
  int nb=22;
  TH1F *h=new TH1F(name, title+";  (STAR #eta <9< polBeam #eta)              #eta-bins;"+yLable,nb,0.5,nb+0.5);
  return h;
}
//------------------
TH1F *newSpin4Histo(TString name="aa", TString title="bb"){
  int nb=16; // WARN: counts from 0 from historic reasons
  return new TH1F(name, title+"; spin4 index  ",nb,-0.5,nb-0.5);
}

//-----------------
void fillDataBin11_18(int iq=0) {//arbitrary model shape
  double ALP[8]={-0.4,-0.2,0.1,0.3,0.4,0.6,0.5,0.4};
  double ALLP[4]={0.5,-0.1,-0.3,-0.4};

  double ALN[8]={0.4,0.6,0.8,0.4,0.2,0.4,0.6,0.3};
  double ALLN[4]={-0.6,-0.5,-0.3,-0.7};

  double *AL=ALP, *ALL=ALLP;
  if(iq) { AL=ALN; ALL=ALLN;}

  for(int k=11; k<=18;k++) {
    hA[iq][0]->SetBinContent(k,AL[k-11]);//AL
    int j=k-15;
    if (j<0) j=14-k;
    printf("k=%d  AL=%.1f  j=%d  ALL=%.1f\n",k,AL[k-11],j,ALL[j]);
    hA[iq][1]->SetBinContent(k,ALL[j]);//ALL
  }
}


//-----------------
void fillDataBin20() {
  // for barrel-integral based on 2009 analysis
  int bin=20; // for physics eta-range[-1,+1]
  
  hA[0][0]->SetBinContent(bin,-0.27);//AL,W+
  hA[0][1]->SetBinContent(bin,0.5);//ALL,W+

  hA[1][0]->SetBinContent(bin,0.14);//AL,W-
  hA[1][1]->SetBinContent(bin,-0.3);//ALL,W-

}
