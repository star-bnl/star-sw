TString MikesMinimalOpts = "            P2017a PicoVtxDefault -hitfilt ";


TString DanielSuggestedOpts =  "ry2017a in AgML tpcDB btof Tree picoWrite UseXgeom BAna ppOpt VFPPVnoCTB beamline3D l3onl fpd trgd analysis PicoVtxDefault -hitfilt epdDb epdHit";


class StSPtrVecEpdHit;

void DrawWheel(StSPtrVecEpdHit hits, TString outputFileName);

void AnalyzePedAsPhysRun(char* inputDirectory, char* inputFileName)
{

  TString _infile = inputDirectory;
  _infile += inputFileName;

  TString outfile = inputFileName;
  outfile.ReplaceAll(".daq","_Levels.txt");


  gROOT->Macro("Load.C");
  gSystem->Load("StEpdUtil");

  gROOT->LoadMacro("bfc.C");

  //  TString mychain = " epdDb epdHit "; //"epdDb epdHit";
  //  bfc(0,MikesMinimalOpts + mychain,_infile);
  bfc(0,DanielSuggestedOpts,_infile);

  const int N = 2; // process N events
  StSPtrVecEpdHit hits;
  for ( int i=0; i<N; i++ ) {

    chain->Clear(); 
    chain->Make(); // in principle should check return status...

    StEvent* event = (StEvent*) chain->GetInputDS("StEvent");
    StMuDst* muDst = (StMuDst*) chain->GetInputDS("muDst");
    if (!muDst){
      cout << "Cant find muDst :-(\n";
      return;}

    cout << "\n\n\n\n\n --------- here in the macro -----------\n\n\n i= " << i << "\n\n";
    cout << "Looking at EPD hits in StEvent....\n\n";
    if (event->epdCollection() == 0){ cout << "No StEpdCollection!\n";}
    else {
      StEpdCollection* epdCollection = event->epdCollection();
      hits = epdCollection->epdHits();
      //      StPtrVecEpdHitIterator it = hits->begin(); I'd rather avoid iterators...
      cout << "StEpdCollection size is " << hits.size() << endl;
    }
    cout << "\n\n\n\n\n\n ----------------------------------\n\n\n";
  }
  DrawWheel(hits, outfile);

  //  new TBrowser();

};



// Here, we'll loop over all StEpdHits
// Every hit will have its outline drawn
// If a hit has ADC > 10, then it will also
// be filled in with dots
void DrawWheel(StSPtrVecEpdHit hits, TString outputFileName){

  /*
  int lowLimitB[4]   = {25, 80,130,190};
  int hiLimitB[4]    = {80,130,190,260};
  int lowLimitC[4]   = {35, 110,200,290};
  int hiLimitC[4]    = {110,200,290,380};
  int levelColor[4] = {2,3,4,6};
  */

  int lowLimitB[2] = {10,149};
  int hiLimitB[2]  = {150,3000};
  int lowLimitC[2] = {10,149};
  int hiLimitC[2]  = {150,3000};
  int levelColor[2] = {3,2};


  StEpdGeom* geo = new StEpdGeom;
  TCanvas* WheelCan = new TCanvas("EpdWheels","EpdWheels",1200,600);
  WheelCan->Draw();
  WheelCan->Divide(2,1);

  TPad* eastPad = WheelCan->cd(1);
  eastPad->Range(-100,-100,100,100);
  TPaveLabel* eastLab = new TPaveLabel(60,90,90,99,"East");
  eastLab->Draw();
  double labelRadius=96;
  for (int pp=1; pp<13; pp++){
    double phi=geo->TileCenter(pp,1,-1).Phi();   // note third argument - means "east"
    TText* ppLabel = new TText(labelRadius*cos(phi),labelRadius*sin(phi),Form("PP%d",pp));
    ppLabel->SetTextAlign(22);
    ppLabel->SetTextColor(kBlue);
    ppLabel->SetTextSize(.02);
    ppLabel->Draw();
  }
  
  TPad* westPad = WheelCan->cd(2);
  westPad->Range(-100,-100,100,100);
  TPaveLabel* westLab = new TPaveLabel(60,90,90,99,"West");
  westLab->Draw();
  for (int pp=1; pp<13; pp++){
    double phi=geo->TileCenter(pp,1,1).Phi();   // note third argument - means "west"
    TText* ppLabel = new TText(labelRadius*cos(phi),labelRadius*sin(phi),Form("PP%d",pp));
    ppLabel->SetTextAlign(22);
    ppLabel->SetTextColor(kBlue);
    ppLabel->SetTextSize(.02);
    ppLabel->Draw();
  }


  ofstream ofs;
  ofs.open(outputFileName.Data());
  //Form("ExtractedLevels.txt"));


  int nCorners;
  double xcorn[6];
  double ycorn[6];
  TPad* thePad;
  int ADCthreshold=10;   // I get this by looking at the adc distribution.
  for (unsigned int tile=0; tile<hits.size(); tile++){
    StEpdHit* theHit = hits[tile];
    geo->GetCorners(theHit->id(),&nCorners,xcorn,ycorn);
    xcorn[nCorners]=xcorn[0];      ycorn[nCorners]=ycorn[0];   // need to close the polylines
    TPolyLine* pline = new TPolyLine(nCorners+1,xcorn,ycorn);
    pline->SetLineWidth(1);
    pline->SetLineColor(1);
    int ADCval = theHit->adc();
    pline->SetFillColor(4);  // blue if nothing
    int LevelNumber = 0;
    for (int ilevel=0; ilevel<2; ilevel++){
      int lowCutoff,highCutoff;
      if (theHit->tile()<10){lowCutoff=lowLimitC[ilevel];  highCutoff=hiLimitC[ilevel];}
      else {lowCutoff=lowLimitB[ilevel];  highCutoff=hiLimitB[ilevel];}
      if ((ADCval>lowCutoff)&&(ADCval<highCutoff)){
	pline->SetFillColor(levelColor[ilevel]);
	LevelNumber = ilevel+1;
      }
    }

    if (ADCval>3500){
      pline->SetFillColor(6);   // this is to catch stuff up at like 4096
      LevelNumber = 6;
    }

    ofs << theHit->side() << "\t" << theHit->position() << "\t" << theHit->tile() << "\t" << LevelNumber << "\t" << theHit->adc() << endl;


    //    if (theHit->adc()>ADCthreshold) pline->SetFillColor(2);
    thePad = (geo->IsWest(theHit->id()))?westPad:eastPad;
    thePad->cd();
    pline->Draw();
    pline->Draw("f");

    TText* ttlabel = new TText(geo->TileCenter(theHit->id()).X(),geo->TileCenter(theHit->id()).Y(),Form("%d",theHit->tile()));
    ttlabel->SetTextAlign(22);
    ttlabel->SetTextSize(0.015);
    ttlabel->Draw();

  }

  TString imageName = outputFileName + ".png";
  WheelCan->SaveAs(imageName.Data());

  ofs.close();


}


/*
  Double_t x[5] = {-20,-10,0,10,-20};
  Double_t y[5] = {10,40,50,5,10};
  TPolyLine *pline = new TPolyLine(5,x,y);
  pline->SetFillColor(38);
  pline->SetLineColor(2);
  pline->SetLineWidth(4);
  pline->Draw("f");
  pline->Draw();

*/

  /*
  cout << "PP\tTT\tADC\tTDC\tTAC\tnMIP\n";
  for (unsigned int tile=0; tile<hits.size(); tile++){
    StEpdHit* theHit = hits[tile];
    cout << theHit->position() << "\t"
	 << theHit->tile() << "\t"
	 << theHit->adc() << "\t"
	 << theHit->tdc() << "\t"
	 << theHit->tac() << "\t"
	 << theHit->nMIP() << "\n";
  }
  */
