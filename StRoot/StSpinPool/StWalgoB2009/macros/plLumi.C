#include <iostream>
#include <fstream>

// This macro is used to determine the integrated luminosity for 
// the Run 9 st_W triggered dataset from the *.wana.hist.root files
// produced from the St2009WlumiMaker. 

int plLumi(){
  gStyle->SetPalette(1);
  string line; int nRuns; float nTot;
  //input/output files
  ifstream infile("runList584.lis");
  //ifstream infile("zdcRateHigh.csv");
  ofstream outfile("lumi_sl11b.csv");
  outfile<<"runNumber,nSBB,nBHT3,fDet,Lumi"<<endl; 
  
  TString iPath="/star/data01/pwg/stevens4/wAnalysis/xSecPaper/sl11b/7.14.11/data/R";
  float lumiTot=0; float lumiTotErr=0;
  int BHT3prescale=50;
  float BHT3xs=434000; // for HT sum
  
  int BTH3coinBin=5;

  //loop ovr 584 good data runs in analysis
  while(infile.good()){
    getline (infile,line);	
    if(line == ""){ //blank last line ie. end of file
      cout<<"lumiTot="<<lumiTot<<" +/- "<<sqrt(lumiTotErr)<<endl;
      return 0;
    }
    
    string run = line.substr(0,8);
    int runNum = atoi(run.data());
    
    //if(runNum>10094000) {
      //continue;
      //cout<<"lumiTot="<<lumiTot<<endl;
      //return 0;
    //}

    TString fullInpName=iPath; fullInpName+=run;
    fullInpName+=".wana.hist.root";
    fd=new TFile(fullInpName);

    tmp=(TH1F*)fd->Get(Form("lumi_AbortGap1_coinBin%d",BTH3coinBin));
    int nAbortGap1=tmp->Integral();
    tmp=(TH1F*)fd->Get(Form("lumi_AbortGap2_coinBin%d",BTH3coinBin));
    int nAbortGap2=tmp->Integral();
    float nSBB=nAbortGap1*120/11+nAbortGap2*120/9;
    tmp=(TH1F*)fd->Get(Form("lumi_nBTH3coin_coinBin%d",BTH3coinBin));
    float nBHT3=tmp->Integral();
    tmp=(TH1F*)fd->Get("lumi_GoodvsT");
    float fDet=tmp->Integral();

    //compute scaled number of BHT3+coin triggers
    float nTrig=(nBHT3-nAbortGap1*110.0/8.0-nAbortGap2*110.0/8.0)*BHT3prescale/fDet;
    float lumi=nTrig/BHT3xs;
    lumiTot+=lumi;
    float lumiErr=0;
    if(nBHT3>0.) lumiErr=lumi/sqrt(nBHT3);
    else cout<<runNum<<","<<nBHT3<<endl;
    lumiTotErr+=lumiErr*lumiErr;
    
    outfile<<runNum<<","<<nSBB<<","<<nBHT3<<","<<fDet<<","<<lumi<<","<<lumiErr<<endl;
    
  }

  return 0;
  
}

