// Plot histograms for all 6 centralities averaged to 3
// and also minimum bias. Reads from file.

gROOT->Reset();
void plotGraphs() {
  Char_t* part   = "Pions";
  //Char_t* part = "Protons";

  Char_t pstype[255] = "ps";
  //Char_t pstype[255] = "eps";
  //Char_t pstype[255] = "gif";

  gROOT->SetStyle("Bold");
  //gROOT->SetStyle("Video");
  gStyle->SetOptStat(kFALSE);

  const Int_t nHar = 2;
  const Int_t nCen = 8;      // six centralities plus two min. bias
  Int_t   nYbins;
  Int_t   nPtbins;
  Float_t max;
  Float_t min;
  Float_t yCM  = 2.92;
  Float_t yMin = 2.3;

  Char_t* fileName = "_avg.root";
  Char_t temp[30];
  strcpy(temp,part);
  Char_t infile[255] = strcat(temp, fileName);
  cout << "in file = " << infile << endl;

  Char_t* plotName = "plots";
  strcpy(temp,plotName);
  strcat(temp,part);
  Char_t outdir[255] = strcat(temp,"/");
  cout << "out dir = " << outdir << endl;
  Char_t outfile[255];

  TF1* p2 = new TF1("p2", "pol3", 0, 2.);
  TF1* p3 = new TF1("p3", "pol4", 0, 2.);
  TF1* p4 = new TF1("p4", "pol5", 0, 2.);
  p2->SetParLimits(0,0.,0.);  // does not work
  p3->SetParLimits(0,0.,0.);
  p4->SetParLimits(0,0.,0.);

  char selText[2];
  char harText[2];
  char cenText[2];

  // Read input files
  TH1F *Y[nCen][nHar];
  TFile *file = new TFile(infile,"READ");
  for(Int_t i = 0; i < nCen; i++) {
    for(Int_t j = 0; j < nHar; j++) {
      if(i < 3 || i > 5){           // six centralities combined to three
	sprintf(harText, "%d", j + 1);
	sprintf(cenText, "%d", i + 1);
	TString *histName = new TString("Flow_vY_Sel2_Har");
	histName->Append(*harText);
	histName->Append("_Cen");
	histName->Append(*cenText);
	Y[i][j] = (TH1F *) file->Get(histName->Data());
	Y[i][j]->Rebin();
	Y[i][j]->Scale(0.5);
	delete histName;
      }
    }
  }
  nYbins = Y[0][0]->GetNbinsX();
  
  TH1F *Pt[nCen][nHar];
  for(Int_t i = 0; i < nCen; i++) {
    for(Int_t j = 0; j < nHar; j++) {
      if(i < 3 || i > 5){
	sprintf(harText, "%d", j + 1);
	sprintf(cenText, "%d", i + 1);
	TString *histName = new TString("Flow_vPt_Sel2_Har");
	histName->Append(*harText);
	histName->Append("_Cen");
	histName->Append(*cenText);
	Pt[i][j] = (TH1F *)file->Get(histName->Data());
	Pt[i][j]->Rebin();
	Pt[i][j]->Scale(0.5);
	delete histName;
      }
    }
  }
  nPtbins = Pt[0][0]->GetNbinsX();
    
  // Make graphs for Y projection  
  // flowY[cen][har][reflected]
  TGraphErrors *flowY[nCen][nHar][2];
  for (Int_t i = 0; i < nCen; i++) {
    for(Int_t j = 0; j < nHar; j++) {
      for(Int_t k = 0; k < 2; k++) {
	flowY[i][j][k] = new TGraphErrors();
      }
    }
  }
  
  // Make graphs for pt projection
  TGraphErrors *flowPt[nCen][nHar][2];
  for (Int_t i = 0; i < nCen; i++) {
    for(Int_t j = 0; j < nHar; j++) {
      for(Int_t k = 0; k < 2; k++) {
	flowPt[i][j][k] = new TGraphErrors();
      }
    }
  }
  
  // Fill graphs with Y projection
  for (Int_t i = 0; i < nCen; i++) {
    for(Int_t j = 0; j < nHar; j++) {
      for(Int_t k = 0; k < nYbins; k++) {
	if (i < 3 || i > 5){
	  if(Y[i][j]->GetBinCenter(k+1) > yMin && 
	     Y[i][j]->GetBinContent(k+1) != 0){
	    if(j) {
	      flowY[i][j][0]->SetPoint(k, Y[i][j]->GetBinCenter(k+1),
				       Y[i][j]->GetBinContent(k+1));
	    } else {
	      flowY[i][j][0]->SetPoint(k, Y[i][j]->GetBinCenter(k+1),
				       -1 * Y[i][j]->GetBinContent(k+1));
	    }
	    flowY[i][j][0]->SetPointError(k, 0., Y[i][j]->GetBinError(k+1));
	    flowY[i][j][1]->SetPoint(k, 2 * yCM -Y[i][j]->GetBinCenter(k+1),
				     Y[i][j]->GetBinContent(k+1));
	    flowY[i][j][1]->SetPointError(k, 0., Y[i][j]->GetBinError(k+1));
	  }
	}
      }
    }
  }
  
  // Fill graphs with Pt projection
  for (Int_t i = 0; i < nCen; i++) {
    for(Int_t j = 0; j < nHar; j++) {
      for(Int_t k = 0; k < nPtbins; k++) {
	if (i < 3 || i > 5){
	  if(j) {
	    flowPt[i][j][0]->SetPoint(k, Pt[i][j]->GetBinCenter(k+1),
				      Pt[i][j]->GetBinContent(k+1));
	  } else {
	    flowPt[i][j][0]->SetPoint(k, Pt[i][j]->GetBinCenter(k+1),
				      -1 * Pt[i][j]->GetBinContent(k+1));
	  }
	  flowPt[i][j][0]->SetPointError(k, 0., Pt[i][j]->GetBinError(k+1));
	} else {
	  flowPt[i][j][0]->SetPoint(k,-1,0);
	}
      }
    }
  }
  
  // Create Canvas
  TCanvas *canvas = new TCanvas("Flow","Flow",100,100,840,600);
  //canvas->Size(28,20);
  canvas->cd();
  TLegend *legend = new TLegend();

  // Draw Graphs

  // Minimum Bias cross section weighted 
  Char_t title[255] = "Minimum Bias ";
  flowY[7][0][0]->SetTitle(strcat(title,part));
  flowY[7][0][0]->SetMarkerStyle(20);
  flowY[7][0][1]->SetMarkerStyle(24);
  flowY[7][1][0]->SetMarkerStyle(21);
  flowY[7][1][1]->SetMarkerStyle(25);
  flowY[7][0][0]->SetMarkerColor(2);
  flowY[7][0][1]->SetMarkerColor(2);
  flowY[7][1][0]->SetMarkerColor(3);
  flowY[7][1][1]->SetMarkerColor(3);
  flowY[7][0][0]->SetMarkerSize(2);
  flowY[7][0][1]->SetMarkerSize(2);
  flowY[7][1][0]->SetMarkerSize(2);
  flowY[7][1][1]->SetMarkerSize(2);

  canvas->Clear();
  TH1F * hist = new TH1F(title,title,10,1,5);
  max = 6.;
  min = -6.;
  hist->SetMaximum(max);
  hist->SetMinimum(min);
  hist->Draw();
  flowY[7][0][0]->Draw("P");
  //flowY[7][0][1]->Draw("P");
  flowY[7][1][0]->Draw("P");
  flowY[7][1][1]->Draw("P");
  
  TLine* lineYcm = new TLine(yCM, min, yCM, max);
  lineYcm->Draw();
  
  TLatex l; 
  l.SetNDC();
  l.SetTextColor(4); 
  l.SetTextAlign(12); 
  l.SetTextSize(0.06); 
  l.SetIndiceSize(0.5);  
  
  l.SetTextAngle(90); 
  l.DrawLatex(.1,0.6,"Flow (%)" ); 
  l.SetTextAngle(0); 
  l.DrawLatex(0.7,0.07,"rapidity" ); 

  l.SetTextColor(2); 
  l.DrawLatex(0.7,0.38,"v_{1}"); 
  l.SetTextColor(8); 
  l.DrawLatex(0.7,0.76,"v_{2}"); 
  
  sprintf(outfile,"%smb_weighted.%s",outdir,pstype);
  canvas->Print(outfile,pstype);
  if (!Pause()) return;

  // Minimum Bias versus pt cross section weighted
  sprintf(title,"Minimum Bias ");
  flowPt[7][0][0]->SetTitle(strcat(title,part));
  flowPt[7][0][0]->SetMarkerStyle(20);
  flowPt[7][1][0]->SetMarkerStyle(21);
  flowPt[7][0][0]->SetMarkerColor(2);
  flowPt[7][1][0]->SetMarkerColor(3);
  flowPt[7][0][0]->SetMarkerSize(2);
  flowPt[7][1][0]->SetMarkerSize(2);

  delete hist;
  canvas->Clear();
  hist = new TH1F(title,title,10,0,2);
  hist->SetMaximum(15);
  hist->SetMinimum(-5);
  hist->Draw();

  //gStyle->SetFuncColor(2);
  flowPt[7][0][0]->Fit("p4", "BR");
  flowPt[7][0][0]->Draw("P");
  //gStyle->SetFuncColor(3);
  flowPt[7][1][0]->Fit("p2", "BR");
  flowPt[7][1][0]->Draw("P");

  l.SetTextColor(4); 
  l.SetTextAngle(90); 
  l.DrawLatex(.1,0.6,"Flow (%)" ); 
  l.SetTextAngle(0); 
  l.DrawLatex(0.7,0.07,"p_{t} (GeV/c)" ); 

  l.SetTextColor(2); 
  l.DrawLatex(0.7,0.4,"v_{1}"); 
  l.SetTextColor(8); 
  l.DrawLatex(0.7,0.65,"v_{2}"); 
  
  sprintf(outfile,"%smb_weighted_vPt.%s",outdir,pstype);
  canvas->Print(outfile,pstype);
  if (!Pause()) return;

  // Set Minimum and Maximum FlowValues and Marker Types
  for (Int_t i = 0; i < nCen-2; i++) {
    for(Int_t k = 0; k < 2; k++) {
      flowY[i][0][k]->SetMarkerColor(i+2);
      flowY[i][0][k]->SetMarkerSize(2);
      flowY[i][0][k]->SetMarkerStyle(20+(i%3)+4*(k%2));
      flowY[i][1][k]->SetMarkerColor(i+2);
      flowY[i][1][k]->SetMarkerSize(2);
      flowY[i][1][k]->SetMarkerStyle(20+(i%3)+4*(k%2));
      
      flowPt[i][0][k]->SetMarkerColor(i+2);
      flowPt[i][0][k]->SetMarkerSize(2);
      flowPt[i][0][k]->SetMarkerStyle(20+(i%3)+4*(k%2));
      flowPt[i][1][k]->SetMarkerColor(i+2);
      flowPt[i][1][k]->SetMarkerSize(2);
      flowPt[i][1][k]->SetMarkerStyle(20+(i%3)+4*(k%2));
    }
  }
    
  // Harmonic 1 all Centralities
  sprintf(title,"v1 ");
  flowY[0][0][0]->SetTitle(strcat(title,part));
  delete hist;
  canvas->Clear();
  TH1F * hist = new TH1F(title,title,10,1,5);
  hist->SetMaximum(8);
  hist->SetMinimum(-8);
  hist->Draw();
  flowY[0][0][0]->Draw("P");
  for (Int_t i = 0; i < 6; i++) {
    for(Int_t k = 0; k < 2; k++) {
      flowY[i][0][k]->Draw("P");
    }
  }
  
  legend->Clear();
  legend->SetX1NDC(0.2);
  legend->SetY1NDC(0.17);    
  legend->SetX2NDC(0.39);
  legend->SetY2NDC(0.37);
  Char_t EntryName[255];
  for(Int_t i = 0; i < 3; i++){
    sprintf(EntryName,"%d + %d",2*i+1,2*i+2);
    legend->AddEntry(flowY[i][0][0],EntryName);
  }
  legend->SetHeader("Centralities");
  legend->Draw();

  sprintf(outfile,"%sv1_all.%s",outdir,pstype);
  canvas->Print(outfile,pstype);

  // Harmonic 1 all Centralities redrawn
  sprintf(title,"v1 ");
  flowY[0][0][0]->SetTitle(strcat(title,part));
  delete hist;
  canvas->Clear();
  TH1F * hist = new TH1F(title,title,10,1,5);
  max = 8;
  min = -8;
  hist->SetMaximum(max);
  hist->SetMinimum(min);
  hist->Draw();
  flowY[0][0][0]->Draw("P");
  for (Int_t i = 0; i < 6; i++) {
    //for(Int_t k = 0; k < 2; k++) {
    for(Int_t k = 0; k < 1; k++) {
      flowY[i][0][k]->Draw("P");
    }
  }
  
  legend->Clear();
  legend->SetX1NDC(0.2);
  legend->SetY1NDC(0.17);    
  legend->SetX2NDC(0.38);
  legend->SetY2NDC(0.37);
  Char_t EntryName[255];
  for(Int_t i = 0; i < 3; i++){
    sprintf(EntryName,"%d + %d",2*i+1,2*i+2);
    legend->AddEntry(flowY[i][0][0],EntryName);
  }
  legend->SetHeader("Centralities");
  legend->Draw();

  delete lineYcm;
  TLine* lineYcm = new TLine(yCM, min, yCM, max);
  lineYcm->Draw();

  l.SetTextColor(4); 
  l.SetTextAngle(90); 
  l.DrawLatex(.1,0.6,"Flow (%)" ); 
  l.SetTextAngle(0);
  l.DrawLatex(0.7,0.07,"rapidity" ); 

  sprintf(outfile,"%sv1_all.%s",outdir,pstype);
  canvas->Print(outfile,pstype);
  if (!Pause()) return;

  // Harmonic 2 all Centralities
  sprintf(title,"v2 ");
  flowY[0][1][0]->SetTitle(strcat(title,part));
  delete hist;
  canvas->Clear();
  TH1F * hist = new TH1F(title,title,10,1,5);
  max = 5;
  min = -1;
  hist->SetMaximum(max);
  hist->SetMinimum(min);
  hist->Draw();
  flowY[0][1][0]->Draw("P");
  for (Int_t i = 0; i < 6; i++) {
    for(Int_t k = 0; k < 2; k++) {
      flowY[i][1][k]->Draw("P");
    }
  }
  
  legend->Clear();
  legend->SetX1NDC(0.2);
  legend->SetY1NDC(0.17);    
  legend->SetX2NDC(0.38);
  legend->SetY2NDC(0.37);
  Char_t EntryName[255];
  for(Int_t i = 0; i < 3; i++){
    sprintf(EntryName,"%d + %d",2*i+1,2*i+2);
    legend->AddEntry(flowY[i][1][0],EntryName);
  }
  legend->SetHeader("Centralities");
  legend->Draw();
  
  delete lineYcm;
  TLine* lineYcm = new TLine(yCM, min, yCM, max);
  lineYcm->Draw();

  l.SetTextAngle(90); 
  l.DrawLatex(.1,0.6,"Flow (%)" ); 
  l.SetTextAngle(0); 
  l.DrawLatex(0.7,0.07,"rapidity" ); 

  sprintf(outfile,"%sv2_all.%s",outdir,pstype);
  canvas->Print(outfile,pstype);
  if (!Pause()) return;

  // Harmonic 1 all Centralities versus pt
  sprintf(title,"v1 ");
  flowPt[0][0][0]->SetTitle(strcat(title,part));
  delete hist;
  canvas->Clear();
  hist = new TH1F(title,title,10,0,2);
  hist->SetMaximum(20);
  hist->SetMinimum(-10);
  hist->Draw();
  flowPt[0][0][0]->Draw("P");
  for (Int_t i = 0; i < 6; i++) {
    //flowPt[i][0][0]->Fit("p3", "R");
    flowPt[i][0][0]->Draw("P");
  }
  
  legend->Clear();
  legend->SetX1NDC(0.2);
  legend->SetY1NDC(0.65);    
  legend->SetX2NDC(0.38);
  legend->SetY2NDC(0.85);
  Char_t EntryName[255];
  for(Int_t i = 0; i < 3; i++){
    sprintf(EntryName,"%d + %d",2*i+1,2*i+2);
    legend->AddEntry(flowPt[i][0][0],EntryName);
  }
  legend->SetHeader("Centralities");
  legend->Draw();
  
  l.SetTextAngle(90); 
  l.DrawLatex(.1,0.6,"Flow (%)" ); 
  l.SetTextAngle(0); 
  l.DrawLatex(0.7,0.07,"p_{t} (GeV/c)" ); 
  
  sprintf(outfile,"%sv1_all_vPt.%s",outdir,pstype);
  canvas->Print(outfile,pstype);
  if (!Pause()) return;
  
  // Harmonic 2 all Centralities versus pt
  sprintf(title,"v2 ");
  flowPt[0][1][0]->SetTitle(strcat(title,part));
  delete hist;
  canvas->Clear();
  hist = new TH1F(title,title,10,0,2);
  if (strcmp(part,"Pions")==0) {
    max = 20;
    min = 0;
  } else {
    max = 20;
    min = -10;
  }
  hist->SetMaximum(max);
  hist->SetMinimum(min);
  hist->Draw();
  flowPt[0][1][0]->Draw("P");
  for (Int_t i = 0; i < 6; i++) {
    //flowPt[i][1][0]->Fit("p2", "R");
    flowPt[i][1][0]->Draw("P");
  }
  
  legend->Clear();
  legend->SetX1NDC(0.2);
  legend->SetY1NDC(0.6);    
  legend->SetX2NDC(0.38);
  legend->SetY2NDC(0.8);
  Char_t EntryName[255];
  for(Int_t i = 0; i < 3; i++){
    sprintf(EntryName,"%d + %d",2*i+1,2*i+2);
    legend->AddEntry(flowPt[i][1][0],EntryName);
  }
  legend->SetHeader("Centralities");
  legend->Draw();
  
  l.SetTextAngle(90); 
  l.DrawLatex(.1,0.6,"Flow (%)" ); 
  l.SetTextAngle(0); 
  l.DrawLatex(0.7,0.07,"p_{t} (GeV/c)" ); 

  sprintf(outfile,"%sv2_all_vPt.%s",outdir,pstype);
  canvas->Print(outfile,pstype);
  if (!Pause()) return;
 
  return;

  // minimum Bias with statistics weighting
  flowY[6][0][0]->SetMarkerStyle(20);
  flowY[6][0][1]->SetMarkerStyle(24);
  flowY[6][1][0]->SetMarkerStyle(21);
  flowY[6][1][1]->SetMarkerStyle(25);
  flowY[6][0][0]->SetMarkerColor(2);
  flowY[6][0][1]->SetMarkerColor(2);
  flowY[6][1][0]->SetMarkerColor(3);
  flowY[6][1][1]->SetMarkerColor(3);
  flowY[6][0][0]->SetTitle("Minimum bias vs Y whithout crossection weighting");

  flowY[6][0][0]->Draw("AP");
  flowY[6][0][1]->Draw("P");
  flowY[6][1][0]->Draw("P");
  flowY[6][1][1]->Draw("P");
  
  legend->Clear();
  legend->SetX1NDC(0.2);
  legend->SetY1NDC(0.75);    
  legend->SetX2NDC(0.4);
  legend->SetY2NDC(0.9);
  legend->AddEntry(flowY[6][0][0],"v1");
  legend->AddEntry(flowY[6][1][0],"v2");
  legend->SetHeader("Harmonics");
  legend->Draw();
  
  sprintf(outfile,"%smb_not_weighted.%s",outdir,pstype);
  canvas->Print(outfile,pstype);
  canvas->Clear();
 
  // Minimum Bias versus pt
  flowPt[6][0][0]->SetTitle("Minimum Bias vs Pt without crossection weighting");
  flowPt[6][0][0]->SetMarkerStyle(20);
  flowPt[6][1][0]->SetMarkerStyle(21);
  flowPt[6][0][0]->SetMarkerColor(2);
  flowPt[6][1][0]->SetMarkerColor(3);

  flowPt[6][0][0]->Draw("AP");
  flowPt[6][1][0]->Draw("P");

  legend->Clear();
  legend->SetX1NDC(0.2);
  legend->SetY1NDC(0.75);    
  legend->SetX2NDC(0.4);
  legend->SetY2NDC(0.9);
  legend->AddEntry(flowPt[6][0][0],"v1");
  legend->AddEntry(flowPt[6][1][0],"v2");
  legend->SetHeader("Harmonics");
  legend->Draw();

  sprintf(outfile,"%smb_not_weighted_vPt.%s",outdir,pstype);
  canvas->Print(outfile,pstype);
  canvas->Clear(); 
 
}

bool Pause() {
  char temp[10];
  cout << "next?, quit? q" << endl;
  fgets(temp, sizeof(temp), stdin);
  if (strstr(temp,"q")!=0) return kFALSE;

  return kTRUE;
}
