{
//=========Macro generated from canvas: CanvasName/ STAR Maker Histogram Canvas
//=========  (Fri Nov 14 10:53:47 2008) by ROOT version5.12/00h-rc2
   TCanvas *CanvasName = new TCanvas("CanvasName", " STAR Maker Histogram Canvas",1,1,600,756);
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
   
   pl = new TPaveLabel(0.7,0.01,0.95,0.03,"Fri Nov 14 10:53:47 2008","br");
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
   PadName_1 = new TPad("PadName_1", "PadName_1",0.01,0.676667,0.49,0.99);
   PadName_1->Draw();
   PadName_1->cd();
   PadName_1->Range(-62.5,-0.2625,62.5,2.3625);
   PadName_1->SetFillColor(10);
   PadName_1->SetBorderMode(0);
   PadName_1->SetBorderSize(2);
   PadName_1->SetFrameFillColor(0);
   PadName_1->SetFrameBorderMode(0);
   PadName_1->SetFrameBorderMode(0);
   
   TH1 *StEQaVtxPrZ = new TH1F("StEQaVtxPrZ","StE  vertex,prim: z ",50,-50,50);
   StEQaVtxPrZ->SetBinContent(0,6);
   StEQaVtxPrZ->SetBinContent(2,1);
   StEQaVtxPrZ->SetBinContent(4,1);
   StEQaVtxPrZ->SetBinContent(11,1);
   StEQaVtxPrZ->SetBinContent(13,2);
   StEQaVtxPrZ->SetBinContent(17,1);
   StEQaVtxPrZ->SetBinContent(18,1);
   StEQaVtxPrZ->SetBinContent(19,2);
   StEQaVtxPrZ->SetBinContent(22,1);
   StEQaVtxPrZ->SetBinContent(24,2);
   StEQaVtxPrZ->SetBinContent(27,2);
   StEQaVtxPrZ->SetBinContent(28,2);
   StEQaVtxPrZ->SetBinContent(30,1);
   StEQaVtxPrZ->SetBinContent(31,1);
   StEQaVtxPrZ->SetBinContent(32,2);
   StEQaVtxPrZ->SetBinContent(33,1);
   StEQaVtxPrZ->SetBinContent(34,1);
   StEQaVtxPrZ->SetBinContent(36,1);
   StEQaVtxPrZ->SetBinContent(41,2);
   StEQaVtxPrZ->SetBinContent(45,1);
   StEQaVtxPrZ->SetBinContent(46,1);
   StEQaVtxPrZ->SetBinContent(49,2);
   StEQaVtxPrZ->SetBinContent(51,3);
   StEQaVtxPrZ->SetEntries(38);
   StEQaVtxPrZ->SetDirectory(0);
   
   TPaveStats *ptstats = new TPaveStats(0.78,0.675,0.98,0.995,"brNDC");
   ptstats->SetName("stats");
   ptstats->SetBorderSize(1);
   ptstats->SetFillColor(10);
   ptstats->SetFillStyle(0);
   ptstats->SetTextAlign(12);
   text = ptstats->AddText("StEQaVtxPrZ");
   text->SetTextSize(0.0490667);
   text = ptstats->AddText("Entries = 38     ");
   text = ptstats->AddText("Mean  =  3.778");
   text = ptstats->AddText("RMS   =  24.81");
   text = ptstats->AddText("Underflow =      6");
   text = ptstats->AddText("Overflow  =      3");
   ptstats->SetOptStat(111111);
   ptstats->SetOptFit(111);
   ptstats->Draw();
   StEQaVtxPrZ->GetListOfFunctions()->Add(ptstats);
   ptstats->SetParent(StEQaVtxPrZ->GetListOfFunctions());
   StEQaVtxPrZ->SetLineWidth(2);
   StEQaVtxPrZ->GetXaxis()->SetTitleOffset(1.2);
   StEQaVtxPrZ->Draw("");
   
   pt = new TPaveText(0.01,0.938413,0.344597,0.995,"blNDC");
   pt->SetName("title");
   pt->SetBorderSize(1);
   pt->SetFillColor(10);
   text = pt->AddText("StE  vertex,prim: z ");
   pt->Draw();
   PadName_1->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_2
   PadName_2 = new TPad("PadName_2", "PadName_2",0.51,0.676667,0.99,0.99);
   PadName_2->Draw();
   PadName_2->cd();
   PadName_2->Range(-45,-1.38063,405,23.4256);
   PadName_2->SetFillColor(10);
   PadName_2->SetBorderMode(0);
   PadName_2->SetBorderSize(2);
   PadName_2->SetFrameFillColor(0);
   PadName_2->SetFrameBorderMode(0);
   PadName_2->SetFrameBorderMode(0);
   
   TH1 *StEQaV0VtxPhiDist = new TH1F("StEQaV0VtxPhiDist","StE V0 azimuthal distribution relative to primvtx",36,0,360);
   StEQaV0VtxPhiDist->SetBinContent(1,15);
   StEQaV0VtxPhiDist->SetBinContent(2,14);
   StEQaV0VtxPhiDist->SetBinContent(3,16);
   StEQaV0VtxPhiDist->SetBinContent(4,10);
   StEQaV0VtxPhiDist->SetBinContent(5,6);
   StEQaV0VtxPhiDist->SetBinContent(6,9);
   StEQaV0VtxPhiDist->SetBinContent(7,8);
   StEQaV0VtxPhiDist->SetBinContent(8,11);
   StEQaV0VtxPhiDist->SetBinContent(9,6);
   StEQaV0VtxPhiDist->SetBinContent(10,10);
   StEQaV0VtxPhiDist->SetBinContent(11,9);
   StEQaV0VtxPhiDist->SetBinContent(12,5);
   StEQaV0VtxPhiDist->SetBinContent(13,20);
   StEQaV0VtxPhiDist->SetBinContent(14,9);
   StEQaV0VtxPhiDist->SetBinContent(15,8);
   StEQaV0VtxPhiDist->SetBinContent(16,8);
   StEQaV0VtxPhiDist->SetBinContent(17,4);
   StEQaV0VtxPhiDist->SetBinContent(18,8);
   StEQaV0VtxPhiDist->SetBinContent(19,8);
   StEQaV0VtxPhiDist->SetBinContent(20,12);
   StEQaV0VtxPhiDist->SetBinContent(21,4);
   StEQaV0VtxPhiDist->SetBinContent(22,2);
   StEQaV0VtxPhiDist->SetBinContent(23,3);
   StEQaV0VtxPhiDist->SetBinContent(24,6);
   StEQaV0VtxPhiDist->SetBinContent(25,7);
   StEQaV0VtxPhiDist->SetBinContent(26,8);
   StEQaV0VtxPhiDist->SetBinContent(27,10);
   StEQaV0VtxPhiDist->SetBinContent(28,8);
   StEQaV0VtxPhiDist->SetBinContent(29,8);
   StEQaV0VtxPhiDist->SetBinContent(30,13);
   StEQaV0VtxPhiDist->SetBinContent(31,15);
   StEQaV0VtxPhiDist->SetBinContent(32,15);
   StEQaV0VtxPhiDist->SetBinContent(33,17);
   StEQaV0VtxPhiDist->SetBinContent(34,13);
   StEQaV0VtxPhiDist->SetBinContent(35,13);
   StEQaV0VtxPhiDist->SetBinContent(36,18);
   StEQaV0VtxPhiDist->SetEntries(356);
   StEQaV0VtxPhiDist->SetDirectory(0);
   
   ptstats = new TPaveStats(0.78,0.675,0.98,0.995,"brNDC");
   ptstats->SetName("stats");
   ptstats->SetBorderSize(1);
   ptstats->SetFillColor(10);
   ptstats->SetFillStyle(0);
   ptstats->SetTextAlign(12);
   text = ptstats->AddText("StEQaV0VtxPhiDist");
   text->SetTextSize(0.0490667);
   text = ptstats->AddText("Entries = 356    ");
   text = ptstats->AddText("Mean  =    186");
   text = ptstats->AddText("RMS   =  116.2");
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
   
   pt = new TPaveText(0.01,0.938413,0.71,0.995,"blNDC");
   pt->SetName("title");
   pt->SetBorderSize(1);
   pt->SetFillColor(10);
   text = pt->AddText("StE V0 azimuthal distribution relative to primvtx");
   pt->Draw();
   PadName_2->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_3
   PadName_3 = new TPad("PadName_3", "PadName_3",0.01,0.343333,0.49,0.656667);
   PadName_3->Draw();
   PadName_3->cd();
   PadName_3->Range(-45,2.42656,405,50.6609);
   PadName_3->SetFillColor(10);
   PadName_3->SetBorderMode(0);
   PadName_3->SetBorderSize(2);
   PadName_3->SetFrameFillColor(0);
   PadName_3->SetFrameBorderMode(0);
   PadName_3->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_4
   PadName_4 = new TPad("PadName_4", "PadName_4",0.51,0.343333,0.99,0.656667);
   PadName_4->Draw();
   PadName_4->cd();
   PadName_4->Range(-2.5,-0.656749,2.5,2.90044);
   PadName_4->SetFillColor(10);
   PadName_4->SetBorderMode(0);
   PadName_4->SetBorderSize(2);
   PadName_4->SetLogy();
   PadName_4->SetFrameFillColor(0);
   PadName_4->SetFrameBorderMode(0);
   PadName_4->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_5
   PadName_5 = new TPad("PadName_5", "PadName_5",0.01,0.01,0.49,0.323333);
   PadName_5->Draw();
   PadName_5->cd();
   PadName_5->Range(-3.75,-0.332114,3.75,2.70874);
   PadName_5->SetFillColor(10);
   PadName_5->SetBorderMode(0);
   PadName_5->SetBorderSize(2);
   PadName_5->SetLogy();
   PadName_5->SetFrameFillColor(0);
   PadName_5->SetFrameBorderMode(0);
   PadName_5->Modified();
   PadName->cd();
  
// ------------>Primitives in pad: PadName_6
   PadName_6 = new TPad("PadName_6", "PadName_6",0.51,0.01,0.99,0.323333);
   PadName_6->Draw();
   PadName_6->cd();
   PadName_6->Range(1.625,-0.209492,5.375,1.60514);
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
