/*
 * $Id: Efficiency.C,v 1.1 2003/08/08 15:10:56 andrewar Exp $
 *
 * \author Claude Pruneau
 *
 * $Log: Efficiency.C,v $
 * Revision 1.1  2003/08/08 15:10:56  andrewar
 * Initial checkin. Claude wrote first macro, new modifications
 * include additional methods to overlay plots from two different files.
 *
 *
 * /

 
TCanvas *c1 = new TCanvas ("ratioc","ratioc", 0, 0, 900, 600);  

void Efficiency ( char * infilename1 ="test.root", 
		  char * histogram1 = "matchedMcPtDistributionLo08", 
		  char * histogram2 = "accMcPtDistributionLo08");


void EffPt(char * infilename1 ="test.root")
{
  TLine *line = new TLine(0.0,1.0,0.20,1.0);
  gStyle->SetOptStat(00000);
  gStyle->SetTitleColor(kWhite);
  gStyle->SetStatColor(kWhite);
  TFile *f1 = new TFile(infilename1);

  TH1F *numerator1 = (TH1F*)f1->Get("matchedMcPtDistributionLo08");
  TH1F *denominator1 = (TH1F*)f1->Get("accMcPtDistributionLo08");
  int n = numerator1->GetNbinsX();
  TH1F *ratioLo = new TH1F("ratioLo","ratioLo",n,0.,5.);
  numerator1->Sumw2();
  denominator1->Sumw2();
  ratioLo->SetTitle("Efficiency");
  ratioLo->Divide(numerator1,denominator1,1.,1.,"b");
  ratioLo->Rebin(2);
  ratioLo->Scale(0.5);
  ratioLo->SetMinimum(0.0);
  ratioLo->SetMaximum(1.0);

  TH1F *numerator2 = (TH1F*)f1->Get("matchedMcPtDistributionMe08");
  TH1F *denominator2 = (TH1F*)f1->Get("accMcPtDistributionMe08");
  TH1F *ratioMe = new TH1F("ratioMe","ratioMe",n,0.,5.);
  numerator2->Sumw2();
  denominator2->Sumw2();
  ratioMe->SetTitle("Efficiency");
  ratioMe->Divide(numerator2,denominator2,1.,1.,"b");
  ratioMe->Rebin(2);
  ratioMe->Scale(0.5);
  ratioMe->SetMinimum(0.0);
  ratioMe->SetMaximum(1.0);
  ratioMe->SetLineColor(2);

  TH1F *numerator3 = (TH1F*)f1->Get("matchedMcPtDistributionHi08");
  TH1F *denominator3 = (TH1F*)f1->Get("accMcPtDistributionHi08");
  TH1F *ratioHi = new TH1F("ratioHi","ratioHi",n,0.,5.);
  numerator3->Sumw2();
  denominator3->Sumw2();
  ratioHi->SetTitle("Efficiency");
  ratioHi->Divide(numerator3,denominator3,1.,1.,"b");
  ratioHi->Rebin(2);
  ratioHi->Scale(0.5);
  ratioHi->SetMinimum(0.0);
  ratioHi->SetMaximum(1.0);
  ratioHi->SetLineColor(4); 

  ratioLo->Draw("E");
  ratioMe->Draw("E.SAME");
  ratioHi->Draw("E.SAME");
}

void EffPt(char * infilename1 ="test.root",
	     char * infilename2 ="test.root",
	     char * GeantId="08",
	     char * density="Hi")
{

  TString numeratorName = "matchedMcPtDistribution";
  numeratorName+=density;
  numeratorName+=GeantId;
  TString denominatorName = "accMcPtDistribution";
  denominatorName+=density;
  denominatorName+=GeantId;

  TLine *line = new TLine(0.0,1.0,0.20,1.0);
  gStyle->SetOptStat(00000);
  gStyle->SetTitleColor(kWhite);
  gStyle->SetStatColor(kWhite);
  TFile *f1 = new TFile(infilename1);

  TH1F *numerator1 = (TH1F*)f1->Get(numeratorName);
  TH1F *denominator1 = (TH1F*)f1->Get(denominatorName);
  int n = numerator1->GetNbinsX();
  TH1F *ratioLo = new TH1F("ratioLo","ratioLo",n,0.,5.);
  numerator1->Sumw2();
  denominator1->Sumw2();
  ratioLo->SetTitle("Efficiency");
  ratioLo->Divide(numerator1,denominator1,1.,1.,"b");
  ratioLo->Rebin(2);
  ratioLo->Scale(0.5);
  ratioLo->SetMinimum(0.0);
  ratioLo->SetMaximum(1.0);

  TFile *f2 = new TFile(infilename2);
  
  TH1F *numerator2 = (TH1F*)f2->Get(numeratorName);
  TH1F *denominator2 = (TH1F*)f2->Get(denominatorName);

  TH1F *ratioMe = new TH1F("ratioMe","ratioMe",n,0.,5.);
  numerator2->Sumw2();
  denominator2->Sumw2();
  ratioMe->SetTitle("Efficiency");
  ratioMe->Divide(numerator2,denominator2,1.,1.,"b");
  ratioMe->Rebin(2);
  ratioMe->Scale(0.5);
  ratioMe->SetMinimum(0.0);
  ratioMe->SetMaximum(1.0);

  ratioLo->SetMarkerStyle(21);
  ratioLo->SetMarkerColor(4);
  ratioLo->SetLineColor(1);
  ratioMe->SetMarkerStyle(21);
  ratioMe->SetMarkerColor(2);
  ratioMe->SetLineColor(1);



  ratioLo->Draw("E");
  ratioMe->Draw("E.SAME");

}
	
void EffPHit(char * infilename1 ="test.root",
	     char * infilename2 ="test.root",
	     char * GeantId="08",
	     char * density="Hi")
{
  TString numerator2DName = "matchedMcHitsVsEtaDistribution";
  numerator2DName+=density;
  numerator2DName+=GeantId;
  TString denominator2DName = "accMcHitsVsEtaDistribution";
  denominator2DName+=density;
  denominator2DName+=GeantId;
  TString numeratorName = "matchedMcHitsDist";
  numeratorName+=density;
  numeratorName+=GeantId;
  TString denominatorName = "accMcHitsDist";
  denominatorName+=density;
  denominatorName+=GeantId;

  cout <<"Name: "<<numerator2DName<<endl;

  TLine *line = new TLine(0.0,1.0,0.20,1.0);
  gStyle->SetOptStat(00000);
  gStyle->SetTitleColor(kWhite);
  gStyle->SetStatColor(kWhite);
  TFile *f1 = new TFile(infilename1);

  TH2F *numerator2d = (TH2F*)f1->Get(numerator2DName);
  TH2F *denominator2d = (TH2F*)f1->Get(denominator2DName);
  TH1D *numerator1 = numerator2d->ProjectionY(numeratorName);
  TH1D *denominator1 = denominator2d->ProjectionY(denominatorName);

  
  int n = numerator1->GetNbinsX();
  TH1D *ratioLo = new TH1D("ratioLo","ratioLo",n,0,50);
  numerator1->Sumw2();
  denominator1->Sumw2();
  ratioLo->SetTitle("Efficiency");
  ratioLo->Divide(numerator1,denominator1,1.,1.,"b");
  ratioLo->SetMinimum(0.0);
  ratioLo->SetMaximum(1.0);

  TFile *f2 = new TFile(infilename2);

  TH2F *numerator2d = (TH2F*)f2->Get(numerator2DName);
  TH2F *denominator2d = (TH2F*)f2->Get(denominator2DName);
  TH1D *numerator1 = numerator2d->ProjectionY(numeratorName);
  TH1D *denominator1 = denominator2d->ProjectionY(denominatorName);

  int n = numerator1->GetNbinsX();
  TH1D *ratioMe = new TH1D("ratioLo","ratioLo",n,0,50);
  numerator1->Sumw2();
  denominator1->Sumw2();
  ratioMe->SetTitle("Efficiency");
  ratioMe->Divide(numerator1,denominator1,1.,1.,"b");
  ratioMe->SetMinimum(0.0);
  ratioMe->SetMaximum(1.0);

  int sum=0;
  for(int loopi=0;loopi<n;loopi++) 
    sum +=denominator1->GetBinContent(loopi);

  ratioLo->SetMarkerColor(4);
  ratioMe->SetMarkerColor(2);
  ratioLo->SetMarkerStyle(21);
  ratioMe->SetMarkerStyle(21);

  cout <<"Sum:"<<sum<<endl;
  denominator1->Scale(1./sum);
  denominator1->SetMaximum(1);
  denominator1->Draw();
  ratioLo->Draw("E.SAME");
  ratioMe->Draw("E.SAME");

}

void EffPhi(char * infilename1 ="test.root")
{
  TLine *line = new TLine(0.0,1.0,0.20,1.0);
  gStyle->SetOptStat(00000);
  gStyle->SetTitleColor(kWhite);
  gStyle->SetStatColor(kWhite);
  TFile *f1 = new TFile(infilename1);

  TH1F *numerator1 = (TH1F*)f1->Get("matchedMcPhiDistributionLo08");
  TH1F *denominator1 = (TH1F*)f1->Get("accMcPhiDistributionLo08");
  int n = numerator1->GetNbinsX();
  TH1F *ratioLo = new TH1F("ratioLo","ratioLo",n,-3.2,3.2);
  numerator1->Sumw2();
  denominator1->Sumw2();
  ratioLo->SetTitle("Efficiency");
  ratioLo->Divide(numerator1,denominator1,1.,1.,"b");
  ratioLo->Rebin(4);
  ratioLo->Scale(0.25);
  ratioLo->SetMinimum(0.0);
  ratioLo->SetMaximum(1.0);

  TH1F *numerator2 = (TH1F*)f1->Get("matchedMcPhiDistributionMe08");
  TH1F *denominator2 = (TH1F*)f1->Get("accMcPhiDistributionMe08");
  TH1F *ratioMe = new TH1F("ratioMe","ratioMe",n,-3.2,3.2);
  numerator2->Sumw2();
  denominator2->Sumw2();
  ratioMe->SetTitle("Efficiency");
  ratioMe->Divide(numerator2,denominator2,1.,1.,"b");
  ratioMe->Rebin(4);
  ratioMe->Scale(0.25);
  ratioMe->SetMinimum(0.0);
  ratioMe->SetMaximum(1.0);
  ratioMe->SetLineColor(2);

  TH1F *numerator3 = (TH1F*)f1->Get("matchedMcPhiDistributionHi08");
  TH1F *denominator3 = (TH1F*)f1->Get("accMcPhiDistributionHi08");
  TH1F *ratioHi = new TH1F("ratioHi","ratioHi",n,-3.2,3.2);
  numerator3->Sumw2();
  denominator3->Sumw2();
  ratioHi->SetTitle("Efficiency");
  ratioHi->Divide(numerator3,denominator3,1.,1.,"b");
  ratioHi->Rebin(4);
  ratioHi->Scale(0.25);
  ratioHi->SetMinimum(0.0);
  ratioHi->SetMaximum(1.0);
  ratioHi->SetLineColor(4); 

  ratioLo->Draw("E");
  ratioMe->Draw("E.SAME");
  ratioHi->Draw("E.SAME");
}

void EffPhi(char * infilename1 ="test.root",
	     char * infilename2 ="test.root",
	     char * GeantId="08",
	     char * density="Hi")
{

  TString numeratorName = "matchedMcPhiDistribution";
  numeratorName+=density;
  numeratorName+=GeantId;
  TString denominatorName = "accMcPhiDistribution";
  denominatorName+=density;
  denominatorName+=GeantId;

  TLine *line = new TLine(0.0,1.0,0.20,1.0);
  gStyle->SetOptStat(00000);
  gStyle->SetTitleColor(kWhite);
  gStyle->SetStatColor(kWhite);
  TFile *f1 = new TFile(infilename1);

  TH1F *numerator1 = (TH1F*)f1->Get(numeratorName);
  TH1F *denominator1 = (TH1F*)f1->Get(denominatorName);
  int n = numerator1->GetNbinsX();
  TH1F *ratioLo = new TH1F("ratioLo","ratioLo",n,-3.2,3.2);
  numerator1->Sumw2();
  denominator1->Sumw2();
  ratioLo->SetTitle("Efficiency");
  ratioLo->Divide(numerator1,denominator1,1.,1.,"b");
  ratioLo->Rebin(4);
  ratioLo->Scale(0.25);
  ratioLo->SetMinimum(0.0);
  ratioLo->SetMaximum(1.0);

  TFile *f2 = new TFile(infilename2);

  TH1F *numerator2 = (TH1F*)f2->Get(numeratorName);
  TH1F *denominator2 = (TH1F*)f2->Get(denominatorName);
  TH1F *ratioHi = new TH1F("ratioHi","ratioHi",n,-3.2,3.2);
  numerator2->Sumw2();
  denominator2->Sumw2();


  ratioHi->SetTitle("Efficiency");
  ratioHi->Divide(numerator2,denominator2,1.,1.,"b");
  ratioHi->Rebin(4);
  ratioHi->Scale(0.25);
  ratioHi->SetMinimum(0.0);
  ratioHi->SetMaximum(1.0);


  ratioLo->SetMarkerColor(4);
  ratioHi->SetMarkerColor(2);
  ratioLo->SetMarkerStyle(21);
  ratioHi->SetMarkerStyle(21);


  ratioLo->Draw("E");
  ratioHi->Draw("E.SAME");
}

void EffEta(char * infilename1 ="test.root")
{
  TLine *line = new TLine(0.0,1.0,0.20,1.0);
  gStyle->SetOptStat(00000);
  gStyle->SetTitleColor(kWhite);
  gStyle->SetStatColor(kWhite);
  TFile *f1 = new TFile(infilename1);

  TH1F *numerator1 = (TH1F*)f1->Get("matchedMcEtaDistributionLo08");
  TH1F *denominator1 = (TH1F*)f1->Get("accMcEtaDistributionLo08");
  int n = numerator1->GetNbinsX();
  TH1F *ratioLo = new TH1F("ratioLo","ratioLo",n,-2.,2.);
  numerator1->Sumw2();
  denominator1->Sumw2();
  ratioLo->SetTitle("Efficiency");
  ratioLo->Divide(numerator1,denominator1,1.,1.,"b");
  ratioLo->Rebin(2);
  ratioLo->Scale(0.5);
  ratioLo->SetMinimum(0.0);
  ratioLo->SetMaximum(1.0);

  TH1F *numerator2 = (TH1F*)f1->Get("matchedMcEtaDistributionMe08");
  TH1F *denominator2 = (TH1F*)f1->Get("accMcEtaDistributionMe08");
  TH1F *ratioMe = new TH1F("ratioMe","ratioMe",n,-2.,2.);
  numerator2->Sumw2();
  denominator2->Sumw2();
  ratioMe->SetTitle("Efficiency");
  ratioMe->Divide(numerator2,denominator2,1.,1.,"b");
  ratioMe->Rebin(2);
  ratioMe->Scale(0.5);
  ratioMe->SetMinimum(0.0);
  ratioMe->SetMaximum(1.0);
  ratioMe->SetLineColor(2);

  TH1F *numerator3 = (TH1F*)f1->Get("matchedMcEtaDistributionHi08");
  TH1F *denominator3 = (TH1F*)f1->Get("accMcEtaDistributionHi08");
  TH1F *ratioHi = new TH1F("ratioHi","ratioHi",n,-2.,2.);
  numerator3->Sumw2();
  denominator3->Sumw2();
  ratioHi->SetTitle("Efficiency");
  ratioHi->Divide(numerator3,denominator3,1.,1.,"b");
  ratioHi->Rebin(2);
  ratioHi->Scale(0.5);
  ratioHi->SetMinimum(0.0);
  ratioHi->SetMaximum(1.0);
  ratioHi->SetLineColor(4); 

  ratioLo->Draw("E");
  ratioMe->Draw("E.SAME");
  ratioHi->Draw("E.SAME");
}


void EffEta(char * infilename1 ="test.root",
	     char * infilename2 ="test.root",
	     char * GeantId="08",
	     char * density="Hi")
{

  TString numeratorName = "matchedMcEtaDistribution";
  numeratorName+=density;
  numeratorName+=GeantId;
  TString denominatorName = "accMcEtaDistribution";
  denominatorName+=density;
  denominatorName+=GeantId;

  TLine *line = new TLine(0.0,1.0,0.20,1.0);
  gStyle->SetOptStat(00000);
  gStyle->SetTitleColor(kWhite);
  gStyle->SetStatColor(kWhite);
  TFile *f1 = new TFile(infilename1);

  TH1F *numerator1 = (TH1F*)f1->Get(numeratorName);
  TH1F *denominator1 = (TH1F*)f1->Get(denominatorName);
  int n = numerator1->GetNbinsX();
  TH1F *ratioLo = new TH1F("ratioLo","ratioLo",n,-2.,2.);
  numerator1->Sumw2();
  denominator1->Sumw2();
  ratioLo->SetTitle("Efficiency");
  ratioLo->Divide(numerator1,denominator1,1.,1.,"b");
  ratioLo->Rebin(4);
  ratioLo->Scale(0.25);
  ratioLo->SetMinimum(0.0);
  ratioLo->SetMaximum(1.0);

  TFile *f2 = new TFile(infilename2);

  TH1F *numerator3 = (TH1F*)f2->Get(numeratorName);
  TH1F *denominator3 = (TH1F*)f2->Get(denominatorName);
  TH1F *ratioHi = new TH1F("ratioHi","ratioHi",n,-2.,2.);
  numerator3->Sumw2();
  denominator3->Sumw2();
  ratioHi->SetTitle("Efficiency");
  ratioHi->Divide(numerator3,denominator3,1.,1.,"b");
  ratioHi->Rebin(4);
  ratioHi->Scale(0.25);
  ratioHi->SetMinimum(0.0);
  ratioHi->SetMaximum(1.0);


  ratioLo->SetMarkerColor(4);
  ratioHi->SetMarkerColor(2);
  ratioLo->SetMarkerStyle(21);
  ratioHi->SetMarkerStyle(21);


  ratioLo->Draw("E");
  ratioHi->Draw("E.SAME");
}

void Efficiency ( char * infilename1,
		  char * histogram1,
		  char * histogram2)
{
  TLine *line = new TLine(0.0,1.0,0.20,1.0);
  gStyle->SetOptStat(00000);
  gStyle->SetTitleColor(kWhite);
  gStyle->SetStatColor(kWhite);
  TFile *f1 = new TFile(infilename1);

  TH1F *numerator = (TH1F*)f1->Get(histogram1);
  TH1F *denominator = (TH1F*)f1->Get(histogram2);
  int n = numerator->GetNbinsX();
  TH1F *ratio = new TH1F("ratio","ratio",n,0.,5.);
  
  numerator->Sumw2();
  denominator->Sumw2();

  c1->SetFillColor(kWhite);
  c1->SetBorderMode(0);
  
  ratio->SetTitle("Efficiency");
  ratio->Divide(numerator,denominator,1.,1.,"b");
  ratio->SetMaximum(1.0);
  ratio->SetMinimum(0.0);
  ratio->Rebin(4);
  ratio->Scale(0.25);
  ratio->SetXTitle("pt");
  ratio->SetYTitle("ratio");
  //denominator->Draw();
  //numerator->SetLineColor(2);
  //numerator->Draw("SAME");
  ratio->Draw("E");
}
