TString MikesMinimalOpts = "            P2017a PicoVtxDefault -hitfilt ";


TString DanielSuggestedOpts =  "ry2017a in AgML tpcDB btof Tree picoWrite UseXgeom BAna ppOpt VFPPVnoCTB beamline3D l3onl fpd trgd analysis PicoVtxDefault -hitfilt epdDb epdHit";


class StSPtrVecEpdHit;

void DrawWheel(StSPtrVecEpdHit hits, TString outputFileName);

void AnalyzePedAsPhysRun(char* inputDirectory, char* inputFileName)
{

  TString _infile = inputDirectory;
  _infile += "/";
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


  int lowLimitB[4] = {-1,10,149,3001};
  int hiLimitB[4]  = {9,150,3000,5000};
  int lowLimitC[4] = {-1,10,149,3001};
  int hiLimitC[4]  = {9,150,3000,5000};
  int levelColor[4] = {4,3,2,6};


  int tileLevel[12][31][2];
  int tileAdc[12][31][2];

  for (int ew=0; ew<2; ew++){
    for (int pp=1; pp<13; pp++){
      for (int tt=1; tt<32; tt++){
	tileLevel[pp-1][tt-1][ew] = 0;
	tileAdc[pp-1][tt-1][ew] = 0;
      }
    }
  }

  for (unsigned int tile=0; tile<hits.size(); tile++){
    StEpdHit* theHit = hits[tile];
    int pp = theHit->position();
    int tt = theHit->tile();
    int ew = theHit->side();
    int ewIndex = (ew<0)?0:1;
    int adc = theHit->adc();
    int Level,lowCutoff,hiCutoff;
    for (Level=0; Level<4; Level++){
      if (tt<10){lowCutoff=lowLimitC[Level];  hiCutoff=hiLimitC[Level];}
      else {lowCutoff=lowLimitB[Level];  hiCutoff=hiLimitB[Level];}
      if ((adc>lowCutoff)&&(adc<hiCutoff)) break;
    }
    if (Level>3) cout << "ERROR!!\n\n";
    tileLevel[pp-1][tt-1][ewIndex] = Level;
    tileAdc[pp-1][tt-1][ewIndex] = adc;
  }

  ofstream ofs;
  ofs.open(outputFileName.Data());

  for (int ew=0; ew<2; ew++){
    int ewSigned = (ew==0)?-1:1;
    for (int pp=1; pp<13; pp++){
      for (int tt=1; tt<32; tt++){
	ofs << ewSigned << "\t" << pp << "\t" << tt << "\t" << 	tileLevel[pp-1][tt-1][ew] << "\t" << tileAdc[pp-1][tt-1][ew] << endl;
      }
    }
  }

  ofs.close();



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


  int nCorners;
  double xcorn[6];
  double ycorn[6];
  TPad* thePad;

  for (int ew=0; ew<2; ew++){
    for (int pp=1; pp<13; pp++){
      for (int tt=1; tt<32; tt++){
	int id = 100*pp+tt;
	if (ew==0) id *= -1;
	geo->GetCorners(id,&nCorners,xcorn,ycorn);
	xcorn[nCorners]=xcorn[0];      ycorn[nCorners]=ycorn[0];   // need to close the polylines
	TPolyLine* pline = new TPolyLine(nCorners+1,xcorn,ycorn);
	pline->SetLineWidth(1);
	pline->SetLineColor(1);
	pline->SetFillColor(levelColor[tileLevel[pp-1][tt-1][ew]]);
	thePad = (ew>0)?westPad:eastPad;
	thePad->cd();
	pline->Draw();
	pline->Draw("f");
	TText* ttlabel = new TText(geo->TileCenter(id).X(),geo->TileCenter(id).Y(),Form("%d",tt));
	ttlabel->SetTextAlign(22);
	ttlabel->SetTextSize(0.015);
	ttlabel->Draw();
      }
    }
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
