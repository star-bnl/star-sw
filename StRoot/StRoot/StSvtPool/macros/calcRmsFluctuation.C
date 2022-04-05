//
// macro to transfer data from old geometry tables to new data base structures
//

void calcRmsFluctuation(const char* fileName)
{
  // Baseline shared libraries
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  
  // DB-specific libs
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("St_Tables.so");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 
  gSystem->Load("geometry");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StDaqLib");
  gSystem->Load("StDAQMaker");
  gSystem->Load("StSvtClassLibrary");
  gSystem->Load("StSvtDaqMaker");
  gSystem->Load("StSvtDbMaker");
  gSystem->Load("StSvtCalibMaker");

  TFile *file = new TFile("rms.root","NEW");

  TH1F *histAll = new TH1F("histAll","RMS fluctuation per anode",1000,0.,20.);
  TH1F *hist = new TH1F("hist","RMS fluctuation per anode",1000,0.,20.);
  TH1F *hist2 = new TH1F("hist2","RMS mean per anode",1000,0.,20.);
  TH2F *hist2D = new TH2F("hist2D","RMS fluctuation per anode vs RMS mean value",100,0.,20.,100,0.,20.);
      
  chain  = new StChain("StChain");
  
  StDAQMaker  *DAQMk       = new StDAQMaker("DAQInput",fileName);
  StSvtDaqMaker *svtDaqMk    = new StSvtDaqMaker("SvtDaq","FULL","ZS");
  StSvtPedMaker *svtPedMk = new StSvtPedMaker("svtPed");
  
  //
  // Initialize all makers
  //
  //chain->Init();
  //chain->InitRun(0);
  DAQMk->Init();
  svtDaqMk->Init();

  // set RMS data set
  svtDaqMk->SetSvtRMSPed();

  //
  // Execute Make for all makers
  //
  DAQMk->Make();
  svtDaqMk->Make();

  // read RMS from DAQ file
  svtDaqMk->GetSvtRMSPed();

  // retrieve hybrid collection of RMS
  St_DataSet *dataSet = (TObjectSet*)svtPedMk->GetDataSet("StSvtRMSPedestal");
  assert(dataSet);
  StSvtHybridCollection* fSvtRms = (StSvtHybridCollection*)(dataSet->GetObject());
  assert(fSvtRms);

  //
  // for display purpose
  //
  StSvtHybridPixels* rms_temp;
  int  index_hyb;
  float rms, rmsTotal=0, rmsSqTotal=0, rmsMean=0, rmsSqMean=0, rmsErr=0;

  // must divide rms from DAQ by 16
    for (int barrel = 1;barrel <= fSvtRms->getNumberOfBarrels();barrel++) {
      for (int ladder = 1;ladder <= fSvtRms->getNumberOfLadders(barrel);ladder++) {
	for (int wafer = 1;wafer <= fSvtRms->getNumberOfWafers(barrel);wafer++) {
	  for (int hybrid = 1;hybrid <= fSvtRms->getNumberOfHybrids();hybrid++) {
	    
	    // check if the hybrid is part of the SVT
	    index_hyb = fSvtRms->getHybridIndex(barrel, ladder, wafer, hybrid);
	    if (index_hyb < 0) continue;
	    rms_temp = (StSvtHybridPixels*)fSvtRms->at(index_hyb);
	   
	    for (int anode=1; anode<=240; anode++) {

	      rmsTotal=0; rmsSqTotal=0;
	      for (int time=0; time<128; time++) {
		rms = rms_temp->At(rms_temp->getPixelIndex(anode, time))/16;
		rms_temp->AddAt(rms,rms_temp->getPixelIndex(anode, time));
		rmsTotal += rms;
		rmsSqTotal += rms*rms;
		histAll->Fill(rms);
	      }
	      rmsMean = rmsTotal/128;
	      rmsSqMean = rmsSqTotal/128;
	      rmsErr = sqrt(rmsSqMean - rmsMean*rmsMean);

	      hist->Fill(rmsErr);
	      hist2->Fill(rmsMean);
	      hist2D->Fill(rmsMean,rmsErr);
	    }

	    rms = rms_temp->At(rms_temp->getPixelIndex(120,64));	
	    cout << "index = " << index_hyb << ", rms = " << rms << endl;
	  }
	}
      }
    }

    file->Write();
}
