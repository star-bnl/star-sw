{
//=========Macro generated from canvas: CanvasName/ STAR Maker Histogram Canvas
//=========  (Wed May  5 16:39:55 2010) by ROOT version5.22/00
   TCanvas *CanvasName = new TCanvas("CanvasName", " STAR Maker Histogram Canvas",4,23,600,780);
   gStyle->SetOptFit(1);
   CanvasName->Range(0,0,1,1);
   CanvasName->SetFillColor(10);
   CanvasName->SetBorderMode(0);
   CanvasName->SetBorderSize(2);
   CanvasName->SetGridx();
   CanvasName->SetGridy();
   CanvasName->SetFrameFillColor(0);
   CanvasName->SetFrameBorderMode(0);
   
   TPaveLabel *pl = new TPaveLabel(0.08,0.96,0.88,1,"st_physics_8352025_raw_1030011.hist.root","br");
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
   
   pl = new TPaveLabel(0.7,0.01,0.95,0.03,"Wed May  5 16:39:52 2010","br");
   pl->SetTextSize(0.6);
   pl->Draw();
   
   pl = new TPaveLabel(0.1,0.01,0.16,0.03,"2","br");
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
   PadName_1->Range(-62.5,-0.7875,62.5,7.0875);
   PadName_1->SetFillColor(10);
   PadName_1->SetBorderMode(0);
   PadName_1->SetBorderSize(2);
   PadName_1->SetFrameFillColor(0);
   PadName_1->SetFrameBorderMode(0);
   PadName_1->SetFrameBorderMode(0);
   
   TH1 *StEQaVtxPrZ = new TH1F("StEQaVtxPrZ","StE  vertex,prim: z ",50,-50,50);
   StEQaVtxPrZ->SetBinContent(0,66);
   StEQaVtxPrZ->SetBinContent(1,2);
   StEQaVtxPrZ->SetBinContent(3,1);
   StEQaVtxPrZ->SetBinContent(4,2);
   StEQaVtxPrZ->SetBinContent(5,1);
   StEQaVtxPrZ->SetBinContent(6,2);
   StEQaVtxPrZ->SetBinContent(8,1);
   StEQaVtxPrZ->SetBinContent(9,3);
   StEQaVtxPrZ->SetBinContent(10,2);
   StEQaVtxPrZ->SetBinContent(11,2);
   StEQaVtxPrZ->SetBinContent(12,1);
   StEQaVtxPrZ->SetBinContent(13,4);
   StEQaVtxPrZ->SetBinContent(14,2);
   StEQaVtxPrZ->SetBinContent(16,1);
   StEQaVtxPrZ->SetBinContent(17,2);
   StEQaVtxPrZ->SetBinContent(18,3);
   StEQaVtxPrZ->SetBinContent(19,2);
   StEQaVtxPrZ->SetBinContent(20,4);
   StEQaVtxPrZ->SetBinContent(21,1);
   StEQaVtxPrZ->SetBinContent(22,3);
   StEQaVtxPrZ->SetBinContent(23,6);
   StEQaVtxPrZ->SetBinContent(24,2);
   StEQaVtxPrZ->SetBinContent(25,2);
   StEQaVtxPrZ->SetBinContent(26,4);
   StEQaVtxPrZ->SetBinContent(27,5);
   StEQaVtxPrZ->SetBinContent(28,1);
   StEQaVtxPrZ->SetBinContent(29,5);
   StEQaVtxPrZ->SetBinContent(30,3);
   StEQaVtxPrZ->SetBinContent(31,1);
   StEQaVtxPrZ->SetBinContent(32,1);
   StEQaVtxPrZ->SetBinContent(33,2);
   StEQaVtxPrZ->SetBinContent(34,4);
   StEQaVtxPrZ->SetBinContent(35,1);
   StEQaVtxPrZ->SetBinContent(36,2);
   StEQaVtxPrZ->SetBinContent(37,2);
   StEQaVtxPrZ->SetBinContent(39,3);
   StEQaVtxPrZ->SetBinContent(41,2);
   StEQaVtxPrZ->SetBinContent(42,1);
   StEQaVtxPrZ->SetBinContent(43,1);
   StEQaVtxPrZ->SetBinContent(44,1);
   StEQaVtxPrZ->SetBinContent(45,3);
   StEQaVtxPrZ->SetBinContent(46,2);
   StEQaVtxPrZ->SetBinContent(47,1);
   StEQaVtxPrZ->SetBinContent(48,5);
   StEQaVtxPrZ->SetBinContent(49,1);
   StEQaVtxPrZ->SetBinContent(50,2);
   StEQaVtxPrZ->SetBinContent(51,62);
   StEQaVtxPrZ->SetEntries(230);
   StEQaVtxPrZ->SetDirectory(0);
   
   TPaveStats *ptstats = new TPaveStats(0.78,0.675,0.98,0.995,"brNDC");
   ptstats->SetName("stats");
   ptstats->SetBorderSize(1);
   ptstats->SetFillColor(10);
   ptstats->SetFillStyle(0);
   ptstats->SetTextAlign(12);
   text = ptstats->AddText("StEQaVtxPrZ");
   text->SetTextSize(0.04906667);
   text = ptstats->AddText("Entries = 230    ");
   text = ptstats->AddText("Mean  =   1.62");
   text = ptstats->AddText("RMS   =   26.1");
   text = ptstats->AddText("Underflow =     66");
   text = ptstats->AddText("Overflow  =     62");
   ptstats->SetOptStat(111111);
   ptstats->SetOptFit(111);
   ptstats->Draw();
   StEQaVtxPrZ->GetListOfFunctions()->Add(ptstats);
   ptstats->SetParent(StEQaVtxPrZ->GetListOfFunctions());
   StEQaVtxPrZ->SetLineWidth(2);
   StEQaVtxPrZ->GetXaxis()->SetTitleOffset(1.2);
   StEQaVtxPrZ->Draw("");
   
   pt = new TPaveText(0.01,0.9384133,0.3445973,0.995,"blNDC");
   pt->SetName("title");
   pt->SetBorderSize(1);
   pt->SetFillColor(10);
   text = pt->AddText("StE  vertex,prim: z ");
   pt->Draw();
   PadName_1->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_2
   PadName_2 = new TPad("PadName_2", "PadName_2",0.51,0.6766667,0.99,0.99);
   PadName_2->Draw();
   PadName_2->cd();
   PadName_2->Range(-45,806.4984,405,1089.014);
   PadName_2->SetFillColor(10);
   PadName_2->SetBorderMode(0);
   PadName_2->SetBorderSize(2);
   PadName_2->SetFrameFillColor(0);
   PadName_2->SetFrameBorderMode(0);
   PadName_2->SetFrameBorderMode(0);
   
   TH1 *StEQaV0VtxPhiDist = new TH1F("StEQaV0VtxPhiDist","StE V0 azimuthal distribution relative to primvtx",36,0,360);
   StEQaV0VtxPhiDist->SetBinContent(1,958);
   StEQaV0VtxPhiDist->SetBinContent(2,938);
   StEQaV0VtxPhiDist->SetBinContent(3,935);
   StEQaV0VtxPhiDist->SetBinContent(4,952);
   StEQaV0VtxPhiDist->SetBinContent(5,987);
   StEQaV0VtxPhiDist->SetBinContent(6,1050);
   StEQaV0VtxPhiDist->SetBinContent(7,1003);
   StEQaV0VtxPhiDist->SetBinContent(8,948);
   StEQaV0VtxPhiDist->SetBinContent(9,950);
   StEQaV0VtxPhiDist->SetBinContent(10,967);
   StEQaV0VtxPhiDist->SetBinContent(11,975);
   StEQaV0VtxPhiDist->SetBinContent(12,943);
   StEQaV0VtxPhiDist->SetBinContent(13,984);
   StEQaV0VtxPhiDist->SetBinContent(14,945);
   StEQaV0VtxPhiDist->SetBinContent(15,997);
   StEQaV0VtxPhiDist->SetBinContent(16,1039);
   StEQaV0VtxPhiDist->SetBinContent(17,993);
   StEQaV0VtxPhiDist->SetBinContent(18,903);
   StEQaV0VtxPhiDist->SetBinContent(19,985);
   StEQaV0VtxPhiDist->SetBinContent(20,947);
   StEQaV0VtxPhiDist->SetBinContent(21,949);
   StEQaV0VtxPhiDist->SetBinContent(22,978);
   StEQaV0VtxPhiDist->SetBinContent(23,877);
   StEQaV0VtxPhiDist->SetBinContent(24,876);
   StEQaV0VtxPhiDist->SetBinContent(25,926);
   StEQaV0VtxPhiDist->SetBinContent(26,849);
   StEQaV0VtxPhiDist->SetBinContent(27,896);
   StEQaV0VtxPhiDist->SetBinContent(28,884);
   StEQaV0VtxPhiDist->SetBinContent(29,849);
   StEQaV0VtxPhiDist->SetBinContent(30,903);
   StEQaV0VtxPhiDist->SetBinContent(31,845);
   StEQaV0VtxPhiDist->SetBinContent(32,868);
   StEQaV0VtxPhiDist->SetBinContent(33,907);
   StEQaV0VtxPhiDist->SetBinContent(34,888);
   StEQaV0VtxPhiDist->SetBinContent(35,953);
   StEQaV0VtxPhiDist->SetBinContent(36,959);
   StEQaV0VtxPhiDist->SetEntries(33806);
   StEQaV0VtxPhiDist->SetDirectory(0);
   
   ptstats = new TPaveStats(0.78,0.675,0.98,0.995,"brNDC");
   ptstats->SetName("stats");
   ptstats->SetBorderSize(1);
   ptstats->SetFillColor(10);
   ptstats->SetFillStyle(0);
   ptstats->SetTextAlign(12);
   text = ptstats->AddText("StEQaV0VtxPhiDist");
   text->SetTextSize(0.04906667);
   text = ptstats->AddText("Entries = 33806  ");
   text = ptstats->AddText("Mean  =  176.7");
   text = ptstats->AddText("RMS   =  103.7");
   text = ptstats->AddText("Underflow =      0");
   text = ptstats->AddText("Overflow  =      0");
   ptstats->SetOptStat(111111);
   ptstats->SetOptFit(111);
   ptstats->Draw();
   StEQaV0VtxPhiDist->GetListOfFunctions()->Add(ptstats);
   ptstats->SetParent(StEQaV0VtxPhiDist->GetListOfFunctions());
   StEQaV0VtxPhiDist->SetLineWidth(2);
   StEQaV0VtxPhiDist->GetXaxis()->SetTitleOffset(1.2);
   StEQaV0VtxPhiDist->Draw("");
   
   pt = new TPaveText(0.01,0.9384133,0.71,0.995,"blNDC");
   pt->SetName("title");
   pt->SetBorderSize(1);
   pt->SetFillColor(10);
   text = pt->AddText("StE V0 azimuthal distribution relative to primvtx");
   pt->Draw();
   PadName_2->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_3
   PadName_3 = new TPad("PadName_3", "PadName_3",0.01,0.3433333,0.49,0.6566667);
   PadName_3->Draw();
   PadName_3->cd();
   PadName_3->Range(-45,390.0369,405,778.6681);
   PadName_3->SetFillColor(10);
   PadName_3->SetBorderMode(0);
   PadName_3->SetBorderSize(2);
   PadName_3->SetFrameFillColor(0);
   PadName_3->SetFrameBorderMode(0);
   PadName_3->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_4
   PadName_4 = new TPad("PadName_4", "PadName_4",0.51,0.3433333,0.99,0.6566667);
   PadName_4->Draw();
   PadName_4->cd();
   PadName_4->Range(-2.5,0.726247,2.5,4.25559);
   PadName_4->SetFillColor(10);
   PadName_4->SetBorderMode(0);
   PadName_4->SetBorderSize(2);
   PadName_4->SetLogy();
   PadName_4->SetFrameFillColor(0);
   PadName_4->SetFrameBorderMode(0);
   PadName_4->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_5
   PadName_5 = new TPad("PadName_5", "PadName_5",0.01,0.01,0.49,0.3233333);
   PadName_5->Draw();
   PadName_5->cd();
   PadName_5->Range(-3.75,-0.7971751,3.75,4.164276);
   PadName_5->SetFillColor(10);
   PadName_5->SetBorderMode(0);
   PadName_5->SetBorderSize(2);
   PadName_5->SetLogy();
   PadName_5->SetFrameFillColor(0);
   PadName_5->SetFrameBorderMode(0);
   PadName_5->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_6
   PadName_6 = new TPad("PadName_6", "PadName_6",0.51,0.01,0.99,0.3233333);
   PadName_6->Draw();
   PadName_6->cd();
   PadName_6->Range(1.625,-0.6697358,5.375,3.017323);
   PadName_6->SetFillColor(10);
   PadName_6->SetBorderMode(0);
   PadName_6->SetBorderSize(2);
   PadName_6->SetLogy();
   PadName_6->SetFrameFillColor(0);
   PadName_6->SetFrameBorderMode(0);
   PadName_6->Modified();
   PadName->cd();
   PadName->Modified();
   CanvasName->cd();
   CanvasName->Modified();
   CanvasName->cd();
   CanvasName->SetSelected(CanvasName);
}
