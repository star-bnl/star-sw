//
//Evaluation Macro
//
//A. Rose
//
//Compares momentum components for Pi- (GEANTID=9
//


void RunStiEvaluation(const char* evalFName="TestEvaluation.root",
		      const char* histFName="EvalHists.root",
		      const char* postFName="EvalHists.ps",
		      Int_t debug = 0)
{
  //File stuff - input/output files open and setup
  //test for existence of file
  cout << "Opening file: " << evalFName << endl;
  evalFile = new TFile(evalFName);

  //create output file for histograms
  //histFile = new TFile(histFName);
  

  //Int_t   GeantId  = 9;

  //create comparison hists
  //Pions (negative)
  canvas1=new TCanvas("canvas1","Pi- Momentum Comparison", 100,50,800,700);
  canvas1->Divide(3,2);
  canvas1->cd(1);
  TestTree->Draw("(stiTrackPx/500):globalTrackPx");
  htemp->SetYTitle("StiTrack Px");
  htemp->SetXTitle("Global Track Px");
  canvas1->cd(2);
  TestTree->Draw("(stiTrackPy/500):globalTrackPy");
  htemp->SetYTitle("StiTrack Px");
  htemp->SetXTitle("Global Track Px");
  canvas1->cd(3);
  TestTree->Draw("(stiTrackPz/500):globalTrackPz");
  htemp->SetYTitle("StiTrack Px");
  htemp->SetXTitle("Global Track Px");

  canvas1->cd(4);
  TestTree->Draw("(stiTrackPx/500):globalTrackPx","(abs(stiTrackPx/500.)<2.)&&abs(globalTrackPx)<2.");
  htemp->SetYTitle("StiTrack Px");
  htemp->SetXTitle("Global Track Px");
  canvas1->cd(5);
  TestTree->Draw("(stiTrackPy/500):globalTrackPy","(abs(stiTrackPy/500.)<2.)&&abs(globalTrackPy)<2.");
  htemp->SetYTitle("StiTrack Px");
  htemp->SetXTitle("Global Track Px");
  canvas1->cd(6);
  TestTree->Draw("(stiTrackPz/500):globalTrackPz","(abs(stiTrackPz/500.)<2.)&&abs(globalTrackPz)<2.");
  htemp->SetYTitle("StiTrack Px");
  htemp->SetXTitle("Global Track Px");


  canvas2=new TCanvas("canvas2","Momentum Fractional Errors", 110,50,810,700);
  canvas2->Divide(3);
  canvas2->cd(1);
  TestTree->Draw("((stiTrackPx/500)-globalTrackPx)/(stiTrackPx/500)");  
  htemp->GetXaxis()->SetMinimum(-1.0);
  htemp->GetXaxis()->SetMaximum(1.0);

  canvas2->cd(2);
  TestTree->Draw("((stiTrackPy/500)-globalTrackPy)/(stiTrackPy/500)");  
  canvas2->cd(3);
  TestTree->Draw("((stiTrackPz/500)-globalTrackPz)/(stiTrackPz/500)");  

  histFile->close();
  evalFile->close();
}

