void CreatePsFile ( char * infilename = "ittf.root",
		    char * bin = "Hi",
		    char * partType="08",
		    char * PrOrGl="Pr")
{
  TLine *line = new TLine(0.0,0.0,3.0,.0); 
//  gStyle->SetOptStat(00000);
  gStyle->SetTitleBorderSize(0);
  gStyle->SetTitleColor(kWhite);
  gStyle->SetStatColor(kWhite);
  gStyle->SetTitleX(0.3);
  gStyle->SetTitleW(0.4);

  TFile *f = new TFile(infilename);

  ostrstream nameofresfile;
  nameofresfile<<"Resolution"<<bin<<partType<<infilename<<ends;
  TFile *fres = new TFile(nameofresfile.str());

  ostrstream psfilename;
  psfilename<<"Ps_"<<bin<<partType<<PrOrGl<<".ps"<<ends;

// --------------------- bias plots ---------------------------------//
  ostrstream nameofhistoPBias, nameofhistoPhiBias, nameofhistoEtaBias;

  TProfile *pbias, *phibias, *etabias;
  
  nameofhistoPBias<<"primaryPBiasVsP"<<bin<<partType<<ends;
  nameofhistoPhiBias<<"primaryPhiBiasVsP"<<bin<<partType<<ends;
  nameofhistoEtaBias<<"primaryEtaBiasVsP"<<bin<<partType<<ends;

  pbias  = (TProfile*)f->Get(nameofhistoPBias.str());
  phibias = (TProfile*)f->Get(nameofhistoPhiBias.str());
  etabias = (TProfile*)f->Get(nameofhistoEtaBias.str());

  TCanvas canvas("canvas","canvas", 600,800);
  canvas.Divide(1,3,0.,0.);
  canvas.SetFillColor(kWhite);
  canvas.SetBorderMode(0);
  canvas_1.SetFillColor(kWhite);
  canvas_1.SetBorderMode(0);
  canvas_2.SetFillColor(kWhite);
  canvas_2.SetBorderMode(0);
  canvas_3.SetFillColor(kWhite);
  canvas_3.SetBorderMode(0);

  ostrstream pbiastitle, phibiastitle, etabiastitle;
  pbiastitle  << PrOrGl << ": P_{rec} - P  : "<<partType<<ends;
  phibiastitle<< PrOrGl <<": Phi_{rec} - Phi  : "<<partType<<ends;
  etabiastitle<< PrOrGl <<": Eta_{rec} - Eta  : "<<partType<<ends;
  
  pbias->SetTitle(pbiastitle.str());
  phibias->SetTitle(phibiastitle.str());
  etabias->SetTitle(etabiastitle.str());
  pbias->SetXTitle("P");
  phibias->SetXTitle("P");
  etabias->SetXTitle("P");

  TPostScript psfile(psfilename.str(),111);
  psfile.NewPage();
  
  canvas.cd(1);
  pbias->SetLineColor(kBlue);
  pbias->SetMarkerColor(kBlue);
  pbias->SetMarkerStyle(21);
  pbias->SetMinimum(-0.015);
  pbias->SetMaximum(0.015);
  pbias->Draw();
  line->Draw("SAME");
  
  canvas.cd(2);
  phibias->SetLineColor(kGreen);
  phibias->SetMarkerColor(kGreen);
  phibias->SetMarkerStyle(21);
  phibias->SetMinimum(-0.015);
  phibias->SetMaximum(0.015);
  phibias->Draw();
  line->Draw("SAME");
  
  canvas.cd(3);
  etabias->SetLineColor(kRed);
  etabias->SetMarkerColor(kRed);
  etabias->SetMarkerStyle(21);
  etabias->SetMinimum(-0.015);
  etabias->SetMaximum(0.015);
  etabias->Draw();
  line->Draw("SAME");
       
  canvas.Update();
  
  psfile.NewPage();

// ---------------------  pulls  ---------------------------------//
  ostrstream nameofhistoCurvPull, nameofhistoTanLPull;
  TH1D *curvaturepulls, *tanlambdapulls;
  nameofhistoCurvPull<<"curv"<<PrOrGl<<"_pull"<<bin<<partType<<ends;
  nameofhistoTanLPull<<"tan"<<PrOrGl<<"_pull"<<bin<<partType<<ends;
  curvaturepulls  = (TH1D*)f->Get(nameofhistoCurvPull.str());
  tanlambdapulls  = (TH1D*)f->Get(nameofhistoTanLPull.str());
  TCanvas canvasP ("canvasP","canvasP", 600,800);
  canvasP.Divide(1,2,0.,0.);
  canvasP.SetFillColor(kWhite);
  canvasP.SetBorderMode(0);
  canvasP_1.SetFillColor(kWhite);
  canvasP_1.SetBorderMode(0);
  canvasP_2.SetFillColor(kWhite);
  canvasP_2.SetBorderMode(0);

  ostrstream curvaturetitle, tantitle;
  curvaturetitle  << PrOrGl << ": curvature pull : "<<partType<<ends;
  tantitle<< PrOrGl <<": tan(\\lambda) pull : "<<partType<<ends;

  curvaturepulls->SetTitle(curvaturetitle.str());
  tanlambdapulls->SetTitle(tantitle.str());

  canvasP.cd(1);
  curvaturepulls->SetLineColor(kBlue);
  curvaturepulls->SetMarkerColor(kBlue);
  curvaturepulls->SetMarkerStyle(21);
  curvaturepulls->Draw();
  canvasP.cd(2);
  tanlambdapulls->SetLineColor(kRed);
  tanlambdapulls->SetMarkerColor(kRed);
  tanlambdapulls->SetMarkerStyle(21);
  tanlambdapulls->Draw();
  canvasP.Update();
  psfile.NewPage();
       
// ---------------------  efficiency  ---------------------------------//
TCanvas canvaseff ("canvaseff","canvaseff",200, 300);  
 canvaseff.Divide(1,2,0,0);
 canvaseff.SetFillColor(kWhite);
 canvaseff.SetBorderMode(0);
 canvaseff_1.SetFillColor(kWhite);
 canvaseff_1.SetBorderMode(0);
 canvaseff_2.SetFillColor(kWhite);
 canvaseff_2.SetBorderMode(0);

 ostrstream histogram1, histogram2, ptefftitle;
 ostrstream etahistogram1, etahistogram2, etaefftitle;
 
 histogram1<<"accMcPtDistribution"<<bin<<partType<<ends;
 histogram2<<"matchedMcPtDistribution"<<bin<<partType<<ends;
 etahistogram1<<"accMcEtaDistribution"<<bin<<partType<<ends;
 etahistogram2<<"matchedMcEtaDistribution"<<bin<<partType<<ends;
 ptefftitle<<"Pt Efficiency  ("<<bin<<") : "<<partType<<ends;
 etaefftitle<<"Eta Efficiency  ("<<bin<<") : "<<partType<<ends;

 TH1F *numerator = (TH1F*)f->Get(histogram2.str());
 TH1F *denominator = (TH1F*)f->Get(histogram1.str());
 TH1F *ratio = new TH1F("ratio","ratio",100,0.0,5.);

 TH1F *etanumerator = (TH1F*)f->Get(etahistogram2.str());
 TH1F *etadenominator = (TH1F*)f->Get(etahistogram1.str());
 TH1F *etaratio = new TH1F("etaratio","etaratio",80,-2.,2.);
  
 numerator->Sumw2();
 denominator->Sumw2();
 etanumerator->Sumw2();
 etadenominator->Sumw2();

 ratio->SetTitle(ptefftitle.str());
 etaratio->SetTitle(etaefftitle.str());

 ratio->Divide(numerator,denominator,1.,1.,"b");
 ratio->SetMaximum(1.1);
 ratio->SetMinimum(0.0);

 etaratio->Divide(etanumerator,etadenominator,1.,1.,"b");
 etaratio->SetMaximum(1.1);
 etaratio->SetMinimum(0.0);

 canvaseff.cd(1);
 ratio->SetLineColor(kBlue);
 ratio->SetMarkerColor(kBlue);
 ratio->SetMarkerStyle(21);
 ratio->SetXTitle("p_{t}");
 ratio->SetYTitle("ratio");
 ratio->Draw("E");

 canvaseff.cd(2);
 etaratio->SetLineColor(kRed);
 etaratio->SetMarkerColor(kRed);
 etaratio->SetMarkerStyle(23);
 etaratio->SetXTitle("eta");
 etaratio->SetYTitle("ratio");
 etaratio->Draw("E");

 canvaseff.Update();
 psfile.NewPage();
       
// --------------------- resolution plots ---------------------------------//
  
TCanvas canvasres ("canvasres","canvasres", 200,300);
  canvasres.Divide(1,3,0,0);
  canvasres.SetFillColor(kWhite);
  canvasres.SetBorderMode(0);
  canvasres_1.SetFillColor(kWhite);
  canvasres_1.SetBorderMode(0);
  canvasres_2.SetFillColor(kWhite);
  canvasres_2.SetBorderMode(0);
  canvasres_3.SetFillColor(kWhite);
  canvasres_3.SetBorderMode(0);
  canvasres.cd();
  
//  psfile->NewPage();

  
  TH1D *ptres, *phires, *etares;
  ptres  = (TH1D*)fres->Get("ptressigma");
  phires = (TH1D*)fres->Get("phiressigma");
  etares = (TH1D*)fres->Get("etaressigma");

  
  ostrstream ptrestitle, phirestitle, etarestitle;
  ptrestitle  <<" P_{t} resolution  : "<<partType<<ends;
  phirestitle<<" Phi resolution  : "<<partType<<ends;
  etarestitle<<" Eta resolution  : "<<partType<<ends;
  
  ptres->SetTitle(ptrestitle.str());
  ptres->SetXTitle("P_{t}");
  phires->SetTitle(phirestitle.str());
  phires->SetXTitle("P_{t}");
  etares->SetTitle(etarestitle.str());
  etares->SetXTitle("P_{t}");
  
  canvasres.cd(1);
  ptres->SetLineColor(kBlue);
  ptres->SetMarkerColor(kBlue);
  ptres->SetMarkerStyle(21);
  ptres->SetMinimum(0.);
  ptres->SetMaximum(0.1);
  ptres->Draw();
  
  canvasres.cd(2);
  phires->SetLineColor(kGreen);
  phires->SetMarkerColor(kGreen);
  phires->SetMarkerStyle(21);
  phires->SetMinimum(0.);
  phires->SetMaximum(0.1);
  phires->Draw();
  
  canvasres.cd(3);
  etares->SetLineColor(kRed);
  etares->SetMarkerColor(kRed);
  etares->SetMarkerStyle(21);
  etares->SetMinimum(0.);
  etares->SetMaximum(0.1);
  etares->Draw();
       
  canvasres.Update();
//  psfile.NewPage();

 psfile.Close();
}
