{
//=========Macro generated from canvas: CanvasName/ STAR Maker Histogram Canvas
//=========  (Mon Aug  1 11:01:00 2011) by ROOT version5.22/00
   TCanvas *CanvasName = new TCanvas("CanvasName", " STAR Maker Histogram Canvas",0,0,600,780);
   gStyle->SetOptFit(1);
   CanvasName->Range(0,0,1,1);
   CanvasName->SetFillColor(10);
   CanvasName->SetBorderMode(0);
   CanvasName->SetBorderSize(2);
   CanvasName->SetGridx();
   CanvasName->SetGridy();
   CanvasName->SetFrameFillColor(0);
   CanvasName->SetFrameBorderMode(0);
   
   TPaveLabel *pl = new TPaveLabel(0.08,0.96,0.88,1,"st_physics_9043046_raw_2030002.hist.root","br");
   pl->SetFillColor(18);
   pl->SetTextFont(32);
   pl->SetTextSize(0.5);
   pl->Draw();
   
   TPaveText *pt = new TPaveText(0.9,0.96,0.99,1,"br");
   pt->SetFillColor(18);
   pt->SetTextFont(32);
   TText *text = pt->AddText("General");
   text = pt->AddText("Hists");
   pt->Draw();
   
   pl = new TPaveLabel(0.7,0.01,0.95,0.03,"Mon Aug  1 11:01:00 2011","br");
   pl->SetTextSize(0.6);
   pl->Draw();
   
   pl = new TPaveLabel(0.1,0.01,0.16,0.03,"1","br");
   pl->SetTextSize(0.6);
   pl->Draw();
  
// ------------>Primitives in pad: PadName
   TPad *PadName = new TPad("PadName", "Pad Title",0,0.05,1,0.95);
   PadName->Draw();
   PadName->cd();
   PadName->Range(0,0,1,1);
   PadName->SetFillColor(10);
   PadName->SetBorderMode(0);
   PadName->SetBorderSize(2);
   PadName->SetGridx();
   PadName->SetGridy();
   PadName->SetFrameFillColor(0);
   PadName->SetFrameBorderMode(0);
  
// ------------>Primitives in pad: PadName_1
   PadName_1 = new TPad("PadName_1", "PadName_1",0.01,0.6766667,0.49,0.99);
   PadName_1->Draw();
   PadName_1->cd();
   PadName_1->Range(-0.15,-0.13125,1.35,1.18125);
   PadName_1->SetFillColor(10);
   PadName_1->SetBorderMode(0);
   PadName_1->SetBorderSize(2);
   PadName_1->SetFrameFillColor(0);
   PadName_1->SetFrameBorderMode(0);
   PadName_1->SetFrameBorderMode(0);
   
   TH1 *StEQaGtrkFitProb = new TH1F("StEQaGtrkFitProb","StE globtrk: prob. fit is correct",100,0,1.2);
   StEQaGtrkFitProb->SetDirectory(0);
   
   TPaveStats *ptstats = new TPaveStats(0.78,0.675,0.98,0.995,"brNDC");
   ptstats->SetName("stats");
   ptstats->SetBorderSize(1);
   ptstats->SetFillColor(10);
   ptstats->SetFillStyle(0);
   ptstats->SetTextAlign(12);
   text = ptstats->AddText("StEQaGtrkFitProb");
   text->SetTextSize(0.04906667);
   text = ptstats->AddText("Entries = 0      ");
   text = ptstats->AddText("Mean  =      0");
   text = ptstats->AddText("RMS   =      0");
   text = ptstats->AddText("Underflow =      0");
   text = ptstats->AddText("Overflow  =      0");
   ptstats->SetOptStat(111111);
   ptstats->SetOptFit(111);
   ptstats->Draw();
   StEQaGtrkFitProb->GetListOfFunctions()->Add(ptstats);
   ptstats->SetParent(StEQaGtrkFitProb->GetListOfFunctions());
   StEQaGtrkFitProb->SetLineWidth(2);
   StEQaGtrkFitProb->GetXaxis()->SetTitleOffset(1.2);
   StEQaGtrkFitProb->Draw("");
   
   pt = new TPaveText(0.01,0.9327546,0.5368512,0.995,"blNDC");
   pt->SetName("title");
   pt->SetBorderSize(1);
   pt->SetFillColor(10);
   text = pt->AddText("StE globtrk: prob. fit is correct");
   pt->Draw();
   PadName_1->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_2
   PadName_2 = new TPad("PadName_2", "PadName_2",0.51,0.6766667,0.99,0.99);
   PadName_2->Draw();
   PadName_2->cd();
   PadName_2->Range(-6.25,-0.13125,56.25,1.18125);
   PadName_2->SetFillColor(10);
   PadName_2->SetBorderMode(0);
   PadName_2->SetBorderSize(2);
   PadName_2->SetFrameFillColor(0);
   PadName_2->SetFrameBorderMode(0);
   PadName_2->SetFrameBorderMode(0);
   
   TH1 *StEQaGtrkNPntFitT = new TH1F("StEQaGtrkNPntFitT","StE globtrk: N fit pnts on trk, tpc",50,0,50);
   StEQaGtrkNPntFitT->SetDirectory(0);
   
   ptstats = new TPaveStats(0.78,0.675,0.98,0.995,"brNDC");
   ptstats->SetName("stats");
   ptstats->SetBorderSize(1);
   ptstats->SetFillColor(10);
   ptstats->SetFillStyle(0);
   ptstats->SetTextAlign(12);
   text = ptstats->AddText("StEQaGtrkNPntFitT");
   text->SetTextSize(0.04906667);
   text = ptstats->AddText("Entries = 0      ");
   text = ptstats->AddText("Mean  =      0");
   text = ptstats->AddText("RMS   =      0");
   text = ptstats->AddText("Underflow =      0");
   text = ptstats->AddText("Overflow  =      0");
   ptstats->SetOptStat(111111);
   ptstats->SetOptFit(111);
   ptstats->Draw();
   StEQaGtrkNPntFitT->GetListOfFunctions()->Add(ptstats);
   ptstats->SetParent(StEQaGtrkNPntFitT->GetListOfFunctions());
   StEQaGtrkNPntFitT->SetLineWidth(2);
   StEQaGtrkNPntFitT->GetXaxis()->SetTitleOffset(1.2);
   StEQaGtrkNPntFitT->Draw("");
   
   pt = new TPaveText(0.01,0.9327546,0.568311,0.995,"blNDC");
   pt->SetName("title");
   pt->SetBorderSize(1);
   pt->SetFillColor(10);
   text = pt->AddText("StE globtrk: N fit pnts on trk, tpc");
   pt->Draw();
   PadName_2->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_3
   PadName_3 = new TPad("PadName_3", "PadName_3",0.01,0.3433333,0.49,0.6566667);
   PadName_3->Draw();
   PadName_3->cd();
   PadName_3->Range(-45,-0.13125,405,1.18125);
   PadName_3->SetFillColor(10);
   PadName_3->SetBorderMode(0);
   PadName_3->SetBorderSize(2);
   PadName_3->SetFrameFillColor(0);
   PadName_3->SetFrameBorderMode(0);
   PadName_3->SetFrameBorderMode(0);
   
   TH1 *StEQaGtrkPhifT_1_East = new TH1D("StEQaGtrkPhifT_1_East","StE globtrk: phi of first point on trk, tpc",64,0,360);
   StEQaGtrkPhifT_1_East->SetStats(0);
   StEQaGtrkPhifT_1_East->SetLineWidth(2);
   StEQaGtrkPhifT_1_East->GetXaxis()->SetTitleOffset(1.2);
   StEQaGtrkPhifT_1_East->Draw("");
   
   TH1 *StEQaGtrkPhifT_2_West = new TH1D("StEQaGtrkPhifT_2_West","StE globtrk: phi of first point on trk, tpc",64,0,360);
   StEQaGtrkPhifT_2_West->SetStats(0);
   StEQaGtrkPhifT_2_West->SetLineStyle(2);
   StEQaGtrkPhifT_2_West->SetLineWidth(2);
   StEQaGtrkPhifT_2_West->GetXaxis()->SetTitleOffset(1.2);
   StEQaGtrkPhifT_2_West->Draw("same");
   
   TLegend *leg = new TLegend(0.8,0.84,0.98,0.98,NULL,"brNDC");
   leg->SetBorderSize(1);
   leg->SetTextFont(62);
   leg->SetLineColor(1);
   leg->SetLineStyle(1);
   leg->SetLineWidth(1);
   leg->SetFillColor(0);
   leg->SetFillStyle(0);
   TLegendEntry *entry=leg->AddEntry("NULL","Legend","h");
   entry->SetLineColor(1);
   entry->SetLineStyle(1);
   entry->SetLineWidth(1);
   entry->SetMarkerColor(1);
   entry->SetMarkerStyle(21);
   entry->SetMarkerSize(1);
   entry->SetTextFont(62);
   entry=leg->AddEntry("StEQaGtrkPhifT_1_East","East","l");
   entry->SetLineColor(1);
   entry->SetLineStyle(1);
   entry->SetLineWidth(2);
   entry->SetMarkerColor(1);
   entry->SetMarkerStyle(21);
   entry->SetMarkerSize(1);
   entry=leg->AddEntry("StEQaGtrkPhifT_2_West","West","l");
   entry->SetLineColor(1);
   entry->SetLineStyle(2);
   entry->SetLineWidth(2);
   entry->SetMarkerColor(1);
   entry->SetMarkerStyle(21);
   entry->SetMarkerSize(1);
   leg->Draw();
   
   pt = new TPaveText(0.01,0.9327546,0.6836633,0.995,"blNDC");
   pt->SetName("title");
   pt->SetBorderSize(1);
   pt->SetFillColor(10);
   text = pt->AddText("StE globtrk: phi of first point on trk, tpc");
   pt->Draw();
   PadName_3->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_4
   PadName_4 = new TPad("PadName_4", "PadName_4",0.51,0.3433333,0.99,0.6566667);
   PadName_4->Draw();
   PadName_4->cd();
   PadName_4->Range(-2.5,-0.13125,2.5,1.18125);
   PadName_4->SetFillColor(10);
   PadName_4->SetBorderMode(0);
   PadName_4->SetBorderSize(2);
   PadName_4->SetFrameFillColor(0);
   PadName_4->SetFrameBorderMode(0);
   PadName_4->SetFrameBorderMode(0);
   
   TH1 *StEQaGtrkEtaT = new TH1F("StEQaGtrkEtaT","StE globtrk: eta, tpc",40,-2,2);
   StEQaGtrkEtaT->SetDirectory(0);
   
   ptstats = new TPaveStats(0.78,0.675,0.98,0.995,"brNDC");
   ptstats->SetName("stats");
   ptstats->SetBorderSize(1);
   ptstats->SetFillColor(10);
   ptstats->SetFillStyle(0);
   ptstats->SetTextAlign(12);
   text = ptstats->AddText("StEQaGtrkEtaT");
   text->SetTextSize(0.04906667);
   text = ptstats->AddText("Entries = 0      ");
   text = ptstats->AddText("Mean  =      0");
   text = ptstats->AddText("RMS   =      0");
   text = ptstats->AddText("Underflow =      0");
   text = ptstats->AddText("Overflow  =      0");
   ptstats->SetOptStat(111111);
   ptstats->SetOptFit(111);
   ptstats->Draw();
   StEQaGtrkEtaT->GetListOfFunctions()->Add(ptstats);
   ptstats->SetParent(StEQaGtrkEtaT->GetListOfFunctions());
   StEQaGtrkEtaT->SetLineWidth(2);
   StEQaGtrkEtaT->GetXaxis()->SetTitleOffset(1.2);
   StEQaGtrkEtaT->Draw("");
   
   pt = new TPaveText(0.01,0.9327546,0.3620749,0.995,"blNDC");
   pt->SetName("title");
   pt->SetBorderSize(1);
   pt->SetFillColor(10);
   text = pt->AddText("StE globtrk: eta, tpc");
   pt->Draw();
   PadName_4->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_5
   PadName_5 = new TPad("PadName_5", "PadName_5",0.01,0.01,0.49,0.3233333);
   PadName_5->Draw();
   PadName_5->cd();
   PadName_5->Range(-3.75,-0.13125,3.75,1.18125);
   PadName_5->SetFillColor(10);
   PadName_5->SetBorderMode(0);
   PadName_5->SetBorderSize(2);
   PadName_5->SetFrameFillColor(0);
   PadName_5->SetFrameBorderMode(0);
   PadName_5->SetFrameBorderMode(0);
   
   TH1 *StEQaGtrkImpactT_1_East = new TH1D("StEQaGtrkImpactT_1_East","StE globtrk: log10 impact param from prim vtx, tpc",120,-3,3);
   StEQaGtrkImpactT_1_East->SetStats(0);
   StEQaGtrkImpactT_1_East->SetLineWidth(2);
   StEQaGtrkImpactT_1_East->GetXaxis()->SetTitleOffset(1.2);
   StEQaGtrkImpactT_1_East->Draw("");
   
   TH1 *StEQaGtrkImpactT_2_West = new TH1D("StEQaGtrkImpactT_2_West","StE globtrk: log10 impact param from prim vtx, tpc",120,-3,3);
   StEQaGtrkImpactT_2_West->SetStats(0);
   StEQaGtrkImpactT_2_West->SetLineStyle(2);
   StEQaGtrkImpactT_2_West->SetLineWidth(2);
   StEQaGtrkImpactT_2_West->GetXaxis()->SetTitleOffset(1.2);
   StEQaGtrkImpactT_2_West->Draw("same");
   
   TH1 *StEQaGtrkImpactT_3_All = new TH1D("StEQaGtrkImpactT_3_All","StE globtrk: log10 impact param from prim vtx, tpc",120,-3,3);
   StEQaGtrkImpactT_3_All->SetStats(0);
   StEQaGtrkImpactT_3_All->SetLineStyle(3);
   StEQaGtrkImpactT_3_All->SetLineWidth(2);
   StEQaGtrkImpactT_3_All->GetXaxis()->SetTitleOffset(1.2);
   StEQaGtrkImpactT_3_All->Draw("same");
   
   leg = new TLegend(0.8,0.84,0.98,0.98,NULL,"brNDC");
   leg->SetBorderSize(1);
   leg->SetTextFont(62);
   leg->SetLineColor(1);
   leg->SetLineStyle(1);
   leg->SetLineWidth(1);
   leg->SetFillColor(0);
   leg->SetFillStyle(0);
   entry=leg->AddEntry("NULL","Legend","h");
   entry->SetLineColor(1);
   entry->SetLineStyle(1);
   entry->SetLineWidth(1);
   entry->SetMarkerColor(1);
   entry->SetMarkerStyle(21);
   entry->SetMarkerSize(1);
   entry->SetTextFont(62);
   entry=leg->AddEntry("StEQaGtrkImpactT_1_East","East","l");
   entry->SetLineColor(1);
   entry->SetLineStyle(1);
   entry->SetLineWidth(2);
   entry->SetMarkerColor(1);
   entry->SetMarkerStyle(21);
   entry->SetMarkerSize(1);
   entry=leg->AddEntry("StEQaGtrkImpactT_2_West","West","l");
   entry->SetLineColor(1);
   entry->SetLineStyle(2);
   entry->SetLineWidth(2);
   entry->SetMarkerColor(1);
   entry->SetMarkerStyle(21);
   entry->SetMarkerSize(1);
   entry=leg->AddEntry("StEQaGtrkImpactT_3_All","All","l");
   entry->SetLineColor(1);
   entry->SetLineStyle(3);
   entry->SetLineWidth(2);
   entry->SetMarkerColor(1);
   entry->SetMarkerStyle(21);
   entry->SetMarkerSize(1);
   leg->Draw();
   
   pt = new TPaveText(0.01,0.9327546,0.71,0.995,"blNDC");
   pt->SetName("title");
   pt->SetBorderSize(1);
   pt->SetFillColor(10);
   text = pt->AddText("StE globtrk: log10 impact param from prim vtx, tpc");
   pt->Draw();
   PadName_5->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_6
   PadName_6 = new TPad("PadName_6", "PadName_6",0.51,0.01,0.99,0.3233333);
   PadName_6->Draw();
   PadName_6->cd();
   PadName_6->Range(1.625,-0.13125,5.375,1.18125);
   PadName_6->SetFillColor(10);
   PadName_6->SetBorderMode(0);
   PadName_6->SetBorderSize(2);
   PadName_6->SetFrameFillColor(0);
   PadName_6->SetFrameBorderMode(0);
   PadName_6->SetFrameBorderMode(0);
   
   TH1 *StEQaGtrkEtaF_1_East = new TH1D("StEQaGtrkEtaF_1_East","StE globtrk: |eta|, ftpc",80,2,5);
   StEQaGtrkEtaF_1_East->SetStats(0);
   StEQaGtrkEtaF_1_East->SetLineWidth(2);
   StEQaGtrkEtaF_1_East->GetXaxis()->SetTitleOffset(1.2);
   StEQaGtrkEtaF_1_East->Draw("");
   
   TH1 *StEQaGtrkEtaF_2_West = new TH1D("StEQaGtrkEtaF_2_West","StE globtrk: |eta|, ftpc",80,2,5);
   StEQaGtrkEtaF_2_West->SetStats(0);
   StEQaGtrkEtaF_2_West->SetLineStyle(2);
   StEQaGtrkEtaF_2_West->SetLineWidth(2);
   StEQaGtrkEtaF_2_West->GetXaxis()->SetTitleOffset(1.2);
   StEQaGtrkEtaF_2_West->Draw("same");
   
   leg = new TLegend(0.8,0.84,0.98,0.98,NULL,"brNDC");
   leg->SetBorderSize(1);
   leg->SetTextFont(62);
   leg->SetLineColor(1);
   leg->SetLineStyle(1);
   leg->SetLineWidth(1);
   leg->SetFillColor(0);
   leg->SetFillStyle(0);
   entry=leg->AddEntry("NULL","Legend","h");
   entry->SetLineColor(1);
   entry->SetLineStyle(1);
   entry->SetLineWidth(1);
   entry->SetMarkerColor(1);
   entry->SetMarkerStyle(21);
   entry->SetMarkerSize(1);
   entry->SetTextFont(62);
   entry=leg->AddEntry("StEQaGtrkEtaF_1_East","East","l");
   entry->SetLineColor(1);
   entry->SetLineStyle(1);
   entry->SetLineWidth(2);
   entry->SetMarkerColor(1);
   entry->SetMarkerStyle(21);
   entry->SetMarkerSize(1);
   entry=leg->AddEntry("StEQaGtrkEtaF_2_West","West","l");
   entry->SetLineColor(1);
   entry->SetLineStyle(2);
   entry->SetLineWidth(2);
   entry->SetMarkerColor(1);
   entry->SetMarkerStyle(21);
   entry->SetMarkerSize(1);
   leg->Draw();
   
   pt = new TPaveText(0.01,0.9327546,0.3935347,0.995,"blNDC");
   pt->SetName("title");
   pt->SetBorderSize(1);
   pt->SetFillColor(10);
   text = pt->AddText("StE globtrk: |eta|, ftpc");
   pt->Draw();
   PadName_6->Modified();
   PadName->cd();
   PadName->Modified();
   CanvasName->cd();
   CanvasName->Modified();
   CanvasName->cd();
   CanvasName->SetSelected(CanvasName);
}
