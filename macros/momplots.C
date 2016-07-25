TChain *chain = 0;
void momplots(const Char_t *dir = "./") {
  ///afs/rhic.bnl.gov/star/users/andrewar/public/momplots.C 
  gSystem->Load("StMiniMcEvent");
  TString Input(dir);
  chain = new TChain("StMiniMcTree");
  TFileSet *fs = new TFileSet(Input);
  TDataSet *set;
  TString Path();
  TDataSetIter next(fs,9999);
  TString pngName;
  while (set = next()) { //loop over DIR 
    Path = set->Path();
    if (! Path.EndsWith(".root")) continue;
    cout << "next set " << Path << endl;
    TString InputFile(gSystem->DirName(Input));
    InputFile += "/";
    InputFile += Path;
    TFile *f = new TFile(InputFile,"read");
    if (! f) {cout << "Can't open " << InputFile << endl; continue;}
    else     {cout << InputFile << " has been opened" << endl;}
    delete f;
    chain->Add(InputFile);
  }  
  TString RCNegCutString("mMatchedPairs.mPtMc>.2 && abs(mMatchedPairs.mEtaMc)<.5 &&mMatchedPairs.mNHitMc>=15 && (mMatchedPairs.mGeantId==8) ");
  TString RCPosCutString("mMatchedPairs.mPtMc>.2 && abs(mMatchedPairs.mEtaMc)<.5 &&mMatchedPairs.mNHitMc>=15 && (mMatchedPairs.mGeantId==9) ");
  TString MCCutString("mMcTracks.mPtMc>.2 && abs(mMcTracks.mEtaMc)<.5 && mMcTracks.mNHitMc>=15");

  TCanvas *c1 = 0;
  TCanvas *Can = new TCanvas("MomDiff","MomDiff",400,400);

  TH2D * momDiffPos = new TH2D("momDiffPos","",20,0.,4.,40,-.05,.05);
  TH2D * momDiffNeg = new TH2D("momDiffNeg","",20,0.,4.,40,-.05,.05);
  TH2D * momRelPos  = new TH2D("momRelPos","",20,0.,4.,40,-.05,.05);
  TH2D * momRelNeg  = new TH2D("momRelNeg","",20,0.,4.,40,-.05,.05);

  TH2D * dcaPos     = new TH2D("dcaPos","",20,0.,4.,120,-3.,3.);
  TH2D * dcaNeg     = new TH2D("dcaNeg","",20,0.,4.,120,-3.,3.);
  
   StMiniMcTree->Draw("(mMatchedPairs.mPtPr-mMatchedPairs.mPtMc):mMatchedPairs.mPtMc>>momDiffNeg",RCNegCutString);
   StMiniMcTree->Draw("(mMatchedPairs.mPtPr-mMatchedPairs.mPtMc):mMatchedPairs.mPtMc>>momDiffPos",RCPosCutString);
   StMiniMcTree->Draw("(mMatchedPairs.mPtPr-mMatchedPairs.mPtMc)/mMatchedPairs.mPtMc:mMatchedPairs.mPtMc>>momRelNeg",RCNegCutString);
   StMiniMcTree->Draw("(mMatchedPairs.mPtPr-mMatchedPairs.mPtMc)/mMatchedPairs.mPtMc:mMatchedPairs.mPtMc>>momRelPos",RCPosCutString);

   //TH1D *resPos=makeSlicePlot(momDiffPos,"respos");

   StMiniMcTree->Draw("mMatchedPairs.mDcaXYGl:mMatchedPairs.mPtMc>>dcaPos",RCPosCutString);
   StMiniMcTree->Draw("mMatchedPairs.mDcaXYGl:mMatchedPairs.mPtMc>>dcaNeg",RCNegCutString);
   
   
   momDiffPos->FitSlicesY();
   momDiffNeg->FitSlicesY();
   momRelPos->FitSlicesY();
   momRelNeg->FitSlicesY();

   TProfile *meandiffpos=(TProfile*)gDirectory->Get("momDiffPos_1");
   TProfile *meandiffneg=(TProfile*)gDirectory->Get("momDiffNeg_1");
   TProfile *meanrelpos=(TProfile*)gDirectory->Get("momRelPos_1");
   TProfile *meanrelneg=(TProfile*)gDirectory->Get("momRelNeg_1");

   meandiffpos->SetMarkerStyle(28);
   meandiffpos->SetMarkerColor(4);
   meandiffneg->SetMarkerStyle(23);
   meandiffneg->SetMarkerColor(3);

   TLegend *ll = new TLegend(.3,.7,.59,.89);
   ll->AddEntry(meandiffpos,"#pi+","p");
   ll->AddEntry(meandiffneg,"#pi-","p");

   meandiffpos->SetMaximum(.1);
   meandiffpos->SetMinimum(-.1);
   meandiffpos->GetXaxis()->SetTitle("Mc Pt [GeV/c]");
   meandiffpos->GetYaxis()->SetTitle("< Rc Pr Pt - Mc Pt > [GeV/c]");
   meandiffpos->Draw();
   meandiffneg->Draw("same");
   ll->Draw("same");
   c1 = Can;
   c1->Update(); pngName = c1->GetName(); pngName += ".png"; pngName.ReplaceAll(" ","");
   TVirtualX::Instance()->WritePixmap(c1->GetCanvasID(),-1,-1,pngName.Data());
   

   TCanvas *can2 = new TCanvas("RelMomDiff","RelMomDiff",400,400);
   meanrelpos->SetMarkerStyle(28);
   meanrelpos->SetMarkerColor(4);
   meanrelneg->SetMarkerStyle(23);
   meanrelneg->SetMarkerColor(3);

   ll = new TLegend(.3,.7,.59,.89);
   ll->AddEntry(meanrelpos,"#pi+","p");
   ll->AddEntry(meanrelneg,"#pi-","p");

   meanrelpos->SetMaximum(.02);
   meanrelpos->SetMinimum(-.02);
   meanrelpos->GetXaxis()->SetTitle("Mc Pt [GeV/c]");
   meanrelpos->GetYaxis()->SetTitle("< (Rc Pr Pt - Mc Pt)/(Mc Pt) > [GeV/c]");
   meanrelpos->Draw();
   meanrelneg->Draw("same");
   ll->Draw("same");
   c1 = can2;
   c1->Update(); pngName = c1->GetName(); pngName += ".png"; pngName.ReplaceAll(" ","");
   TVirtualX::Instance()->WritePixmap(c1->GetCanvasID(),-1,-1,pngName.Data());

   TCanvas *cansig1 = new TCanvas("Sigma","Sigma",400,400);
   TProfile *sigrelpos=(TProfile*)gDirectory->Get("momRelPos_2");
   TProfile *sigrelneg=(TProfile*)gDirectory->Get("momRelNeg_2");

   sigrelpos->SetMarkerStyle(28);
   sigrelpos->SetMarkerColor(4);
   sigrelneg->SetMarkerStyle(23);
   sigrelneg->SetMarkerColor(3);

   TLegend *ll = new TLegend(.3,.7,.59,.89);
   ll->AddEntry(sigrelpos,"#pi+","p");
   ll->AddEntry(sigrelneg,"#pi-","p");

   sigrelpos->SetMaximum(.1);
   sigrelpos->SetMinimum(0.);
   sigrelpos->GetXaxis()->SetTitle("Mc Pt [GeV/c]");
   sigrelpos->GetYaxis()->SetTitle("#sigma { (Rc Pr Pt - Mc Pt)/(Mc Pt) } [%/100]");
   sigrelpos->Draw();
   sigrelneg->Draw("same");
   ll->Draw("same");
   c1 = cansig1;
   c1->Update(); pngName = c1->GetName(); pngName += ".png"; pngName.ReplaceAll(" ","");
   TVirtualX::Instance()->WritePixmap(c1->GetCanvasID(),-1,-1,pngName.Data());


   //TH2D * eDifPos = new TH2D("eDifPos","",20,0.,4.,40,-.05,.05);
   //TH2D * eDifNeg = new TH2D("eDifNeg","",20,0.,4.,40,-.05,.05);
   //StMiniMcTree->Draw("(sqrt(mMatchedPairs.mPtPr*mMatchedPairs.mPtPr+mMatchedPairs.mPzPr*mMatchedPairs.mPzPr)/.26-sqrt(mMatchedPairs.mPtPr*mMatchedPairs.mPtPr+mMatchedPairs.mPzPr*mMatchedPairs.mPzPr)):mMatchedPairs.mPtMc>>eDifNeg",RCNegCutString);

}
