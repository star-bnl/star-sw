#include <fstream>
#include <map>
using namespace std;

void trighistcombiner(const char* filelistname="trigevent.list")
{
  map<int,int> trigmap;
  const int nTriggers = 12;
  int triggers[nTriggers] = {117001,137221,137222,137822,117300,137571,137575,137585,137586,137611,137501,137622};
  gROOT->Macro("LoadLogger.C");
  gROOT->Macro("loadMuDst.C");
  gSystem->Load("StDaqLib");//for decoder
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StTriggerStudyMaker");

  TFile outfile("trighists.root","RECREATE");

  const char* dbTime = "2006-03-14 15:09:26";
  StBemcTablesWriter* bemctables = new StBemcTablesWriter;
  bemctables->loadTables(dbTime,"ofl");
  StEmcGeom* mEmcGeom = StEmcGeom::instance("bemc");
  StEmcDecoder* mDecoder = new StEmcDecoder();

  ifstream filelist(filelistname);
  TH1F* dumhist;
  TH1F* zvertall;
  TH1F* bbctall;
  TH1F* zverttrig[nTriggers];
  TH1F* bbcttrig[nTriggers];
  TH1F* jp1et;
  TH1F* jp2et;
  TH1F* jp2eff;
  TH1F* jp1eff;
  TH1F* jp1et0;
  TH1F* jp0et0;
  int nFiles = 0;
  while(1){
    char file[100];
    filelist>>file;
    if(!filelist.good())break;
    TFile inputfile(file,"READ");
    if(!inputfile.IsOpen())continue;
    if(nFiles == 0){
      zvertall = (TH1F*)inputfile.Get("zvertall");
      bbctall = (TH1F*)inputfile.Get("bbctall");
      jp1et = (TH1F*)inputfile.Get("jp1et");
      jp2et = (TH1F*)inputfile.Get("jp2et");
      jp1et0 = (TH1F*)inputfile.Get("jp1et0");
      jp0et0 = (TH1F*)inputfile.Get("jp0et0");
      jp1eff = (TH1F*)jp0et0->Clone("jp1eff");
      jp2eff = (TH1F*)jp1et->Clone("jp2eff");
      jp1eff->Reset();
      jp2eff->Reset();
      for(int i = 0; i < nTriggers; i++){
	trigmap[triggers[i]] = i;;
	char namez[100];
	sprintf(namez,"zvert%i",triggers[i]);
	zverttrig[i] = (TH1F*)inputfile.Get(namez);
	char nameb[100];
	sprintf(nameb,"bbct%i",triggers[i]);
	bbcttrig[i] = (TH1F*)inputfile.Get(nameb);
      }

    }else{
      dumhist = (TH1F*)inputfile.Get("zvertall");
      zvertall->Add(dumhist);
      dumhist->Clear();
      dumhist = (TH1F*)inputfile.Get("bbctall");
      bbctall->Add(dumhist);
      dumhist->Clear();
      dumhist = (TH1F*)inputfile.Get("jp1et");
      jp1et->Add(dumhist);
      dumhist->Clear();
      dumhist = (TH1F*)inputfile.Get("jp2et");
      jp2et->Add(dumhist);
      dumhist->Clear();
      dumhist = (TH1F*)inputfile.Get("jp0et0");
      jp0et0->Add(dumhist);
      dumhist->Clear();
      dumhist = (TH1F*)inputfile.Get("jp1et0");
      jp1et0->Add(dumhist);
      dumhist->Clear();
      for(int i = 0; i < nTriggers; i++){
	char namez[100];
	sprintf(namez,"zvert%i",triggers[i]);
	dumhist = (TH1F*)inputfile.Get(namez);
	zverttrig[i]->Add(dumhist);
	dumhist->Clear();
	char nameb[100];
	sprintf(nameb,"bbct%i",triggers[i]);
	dumhist = (TH1F*)inputfile.Get(nameb);
	bbcttrig[i]->Add(dumhist);
	dumhist->Clear();
      }
    }
    inputfile.Clear();
    nFiles++;
  }

  for(int i = 1; i <= jp1eff->GetNbinsX(); i++){
    float jp1et0c = jp1et0->GetBinContent(i);
    float jp0et0c = jp0et0->GetBinContent(i);
    float jp1etc = jp1et->GetBinContent(i);
    float jp2etc = jp2et->GetBinContent(i);
    float jp1efferr = TMath::Sqrt(jp1et0c/(jp0et0c*jp0et0c) + (jp1et0c*jp1et0c)/(jp0et0c*jp0et0c*jp0et0c));
    float jp2efferr = TMath::Sqrt(jp2etc/(jp1etc*jp1etc) + (jp2etc*jp2etc)/(jp1etc*jp1etc*jp1etc));
    jp1eff->SetBinContent(i,jp1et0c/jp0et0c);
    jp1eff->SetBinError(i,jp1efferr);
    jp2eff->SetBinContent(i,jp2etc/jp1etc);
    jp2eff->SetBinError(i,jp2efferr);
  }

  jp1eff->SetTitle("JP1 Efficiency");
  jp2eff->SetTitle("JP2 Efficiency");
  jp1eff->GetYaxis()->SetRangeUser(0,1.2);
  jp2eff->GetYaxis()->SetRangeUser(0,1.2);

  outfile.cd();
  zvertall->Write();
  bbctall->Write();
  jp1et->Write();
  jp2et->Write();
  jp1et0->Write();
  jp0et0->Write();
  jp1eff->Write();
  jp2eff->Write();
  for(int i = 0; i < nTriggers; i++){
    zverttrig[i]->Write();
    bbcttrig[i]->Write();
  }

  outfile.Write();
  outfile.Close();

}
