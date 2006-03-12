int MakeGainT0Y4Copy(const char* gaintxt, const char* timestamp){ 

 
  int tNPadAtRow[45]={
    88,96,104,112,118,126,134,142,150,158,166,174,182,
    98,100,102,104,106,106,108,110,112,112,114,116,118,120,122,122,
    124,126,128,128,130,132,134,136,138,138,140,142,144,144,144,144
  };
  
  cout << " --- Read row/pad to fee conversion table" << endl;
  ifstream InFile("RowPadToFEE.txt");
  Int_t tRowPadToFee[45][182];
  Int_t tRowPadToRDO[45][182];
  Int_t tiRow,tiPad,tiFee,tiRDO;
  InFile >> tiRow >> tiPad >> tiFee >> tiRDO;
  while (!InFile.eof()){
    //  cout << tiRow << " " << tiPad << " " << tiFee << " " << tiRDO << endl;
    tRowPadToFee[tiRow-1][tiPad-1]=tiFee;
    tRowPadToRDO[tiRow-1][tiPad-1]=tiRDO;
    InFile >> tiRow >> tiPad >> tiFee >> tiRDO;
  }
  InFile.close();

  cout << " --- Create tables" << endl;
  gROOT->LoadMacro("Load.C");
  Load();
  St_tpcGain *gain = new St_tpcGain("tpcGain",24);
  St_tpcISTimeOffsets *isT0 = new St_tpcISTimeOffsets("tpcISTimeOffsets",24);
  St_tpcOSTimeOffsets *osT0 = new St_tpcOSTimeOffsets("tpcOSTimeOffsets",24);
  St_tpcBadPad  *bad = new St_tpcBadPad("BadPad",1);
  gain->SetNRows(0);
  isT0->SetNRows(0);
  osT0->SetNRows(0);
  bad->SetNRows(0);
  tpcGain_st grow[24];
  tpcISTimeOffsets_st itrow[24];
  tpcOSTimeOffsets_st otrow[24];
  tpcBadPad_st brow;

  cout << " timestamp: " << timestamp << endl;

  cout << " --- Open input file " << endl;
  ifstream fIn(gaintxt);
  //  ifstream fIn("/RTS/conf/tpc_gains.txt");
  if(!fIn) { cout<<" -----> InputFile "<<gaintxt<<" not found ... aborting"<<endl;
  return 0;
  }
    

  int Sect,lastSect;
  int Row;
  int Pad;
  double Gain;
  double T0;
  int isCounter = 0;
  int osCounter = 0;
    
  
  lastSect = 1;
  fIn >> Sect;
  while(!fIn.eof()){
    fIn >> Row >> Pad >> Gain >> T0;
    if(lastSect<Sect){
      isCounter = 0;
      osCounter = 0;
    }
    if(Gain==0.) { // dead or bad
      brow.sector = Sect;
      brow.row    = Row;
      brow.pad    = Pad;
      brow.fee    = tRowPadToFee[Row-1][Pad-1];
      bad->AddAt(&brow);
    }
    if(Row<13){
      grow[Sect-1].Gain[Row-1][Pad-1] = Gain;
      itrow[Sect-1].offset[isCounter++] = T0;
    }
    else if(Row==13){
      grow[Sect-1].Gain[Row-1][Pad-1] = 0;
      itrow[Sect-1].offset[isCounter++] = 0;
    }
    else{
      grow[Sect-1].Gain[Row-1][Pad-1] = Gain; 
      otrow[Sect-1].offset[osCounter++] = T0;
    }
    lastSect = Sect;
    fIn >> Sect;
  }

  cout << " --- Store gain coeficient in table" << endl;
  int sectorIDs[24];
  for(int ti=0;ti<24;ti++){
    gain->AddAt(&(grow[ti]),ti);
    isT0->AddAt(&(itrow[ti]),ti);
    osT0->AddAt(&(otrow[ti]),ti);
    sectorIDs[ti]=ti+1;
  }
  //
  cout << " --- Save as File " << endl;
  TString fname("tpcGain."); fname+=timestamp; fname+=".root";
  TFile *f = new TFile(fname.Data(),"recreate");
  gain->Write();
  delete f;
  cout << " --- Save IST0 as File " << endl;
  TString fnamei("tpcISTimeOffsets."); fnamei+=timestamp; fnamei+=".root";
  TFile *fi = new TFile(fnamei.Data(),"recreate");
  isT0->Write();
  delete fi;
  cout << " --- Save OST0 as File " << endl;
  TString fnameo("tpcOSTimeOffsets."); fnameo+=timestamp; fnameo+=".root";
  TFile *fo = new TFile(fnameo.Data(),"recreate");
  osT0->Write();
  delete fo;
  //TFile *f = new TFile("tpcBadPad.20011020.000000.root","recreate");
  //bad->Write();
  //delete f;

  cout<< " --- Save to DB "<<endl;

    gSystem->Load("StDbLib");
    StDbManager* mgr=StDbManager::Instance();
    StDbConfigNode* node=mgr->initConfig("Calibrations_tpc");
    StDbTable* dbTable=node->addDbTable("tpcGain");

    dbTable->SetTable((char*)grow,24,sectorIDs);
    mgr->setStoreTime(timestamp);

    if(!mgr->storeDbTable(dbTable)){
           cout<<" ------> error storing in DB"<<endl;
           return 0;
    }

    StDbTable* dbTablei=node->addDbTable("tpcISTimeOffsets");

    dbTablei->SetTable((char*)itrow,24,sectorIDs);
    mgr->setStoreTime(timestamp);

    if(!mgr->storeDbTable(dbTablei)){
           cout<<" ------> error storing IST0 in DB"<<endl;
           return 0;
    }

    StDbTable* dbTableo=node->addDbTable("tpcOSTimeOffsets");

    dbTableo->SetTable((char*)otrow,24,sectorIDs);
    mgr->setStoreTime(timestamp);

    if(!mgr->storeDbTable(dbTableo)){
           cout<<" ------> error storing OST0 in DB"<<endl;
           return 0;
    }

  return 1;  
}
