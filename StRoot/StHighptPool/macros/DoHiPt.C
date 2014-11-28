void DoHiPt(const char* inDir = "links/P02gc.productionCentral.FullField.2001.313.mdst",
	    const char* outFile = "HiPt.Test.hist.root",
	    Int_t cut = 99999999,
	    const char* analysis = "StHiJennsAnalysis",
	    const char* extra="hello",
	    char half = 0,
	    float vertexZSkip=0,
	    float hitAvoid=0,
	    char geomHalf=0)
{

  gSystem->Load("StHiMicroEvent");
  gSystem->Load("StHiMicroAnalysis");
  
  Int_t nevent = 99999;
  Int_t nfile = 99999;

  cout << "inDir : " << inDir << endl;
  cout << "outFile : " << outFile << endl;

  cout << "*********** Using cut type : " << cut << endl;

  cout << "*********** Analysis type : " << analysis << endl;

  // derived classes...

  StHiBaseAnalysis* hi =0 ;

  if(strcmp(analysis,"StHiJennsAnalysis")==0){
    hi = new StHiJennsAnalysis(inDir,outFile);
  }
  else if(strcmp(analysis,"StHiSpectra")==0){
    hi = new StHiSpectra(inDir,outFile,extra);
    StHiSpectra* hiSpectra=dynamic_cast<StHiSpectra*>(hi);
    Cut::mDoSpectraCent=1;

    //    if(extra) {
    //   cout << "Setting eff file : " << extra << endl;
    //  hiSpectra->setEfficiencyFileName(extra);
    //  cout << "Reality check " << hiSpectra->efficiencyFileName() << endl;
    // }
  } 
  else if(strcmp(analysis,"StDcaAnalysis")==0){
    hi = new StDcaAnalysis(inDir,outFile);
  }
  
  // additional stuff

  //This sets all cuts based on the input string
  // a. Centrality (minbias)
  // b. Which Centrality definition for Spectra (flow)
  // c. Which centrality definition for corrections/other (flow)
  // d. Vertex Range (-200,200)
  // e. Use only half of TPC or not (none)
  // f. Numnber of fitpts (20-45)
  // g. 3d dca cut (1 cm)
  // h. eta range (0.5)
  //All 9's is default

  Cut::SetCut(cut);

  if(half){
    if(hitAvoid || Cut::mHitAvoid){
      cout << "Using hit half : " << half << endl;
      Cut::SetHitHalf(half);
    }
    else{
      cout << "Using half : " << half << endl;
      Cut::SetHalf(half); // this overrides the vertex cut
    }
  }
  if(geomHalf){
    cout << "Using geom half : " << geomHalf << endl;
    Cut::SetGeomHalf(geomHalf);
  }

  if(vertexZSkip){
    cout << "Skipping vertex region " << vertexZSkip << endl;
    Cut::SetVertexZSkip(vertexZSkip);
  }
  else if(hitAvoid){
    cout << "Avoiding hits " << hitAvoid << endl;
    Cut::SetHitAvoid(hitAvoid);
  }
  
  hi->setNEvent(nevent);
  hi->setNFile(nfile);


  Int_t stat = hi->Init();
  if(stat){
    cout << "Trouble with Init()" << endl;
    return;
  }

  hi->Run();
  hi->Finish();
  
}
