TFile *fd=0;
TCanvas *c=0;

fitAdc4Ped(char* fName="Rnnnped", char* inDir="") {//
  
  assert( !gSystem->Load("StEEmcPedFit"));
  
  TString fPath=inDir;
  
  TString headName=fPath+fName;
  
  TString fullName=headName+".hist.root";
  
  TString saveHistoName=headName+"fit";
  
  TFile *j=new TFile(fullName);
  
  if (!j->IsOpen()){
    printf("failed to open %s hist.root file\n",fName);
    return;
  }
  
  EEpixPed *pix=new EEpixPed(fullName);
  
  TString fname1=fPath+"ped";
    
  //Towers
  // peak search [x1,x2], peak fit [max-xFit, max+xFit] if integral>minInt
  pix->setLimits(1,250,10,5); //x1,x2,minInt,+/-xFit
  pix->setQA(2000, 1.,35, 0.5,2.); // minInt, pedLow, pedHigh, pedSigMin pedSigMax
  pix->findTowerHisto();   
  pix->fitHisto(fPath);
  pix->saveHisto(saveHistoName);
  
  pix->savePedTable("w", fname1);
  pix->dropHisto();

  //MAPMT
  pix->setLimits(2,400,900,3); //x1,x2,minInt,+/-xFit 
  pix->setQA(2000, 120.,300, 0.4,2.); // minInt, pedLow, pedHigh, pedSigMin pedSigMax
  pix->findMapmtHisto();
  pix->fitHisto(fPath);
  pix->saveHisto(saveHistoName);
  pix->savePedTable("a", fname1);
   
}

