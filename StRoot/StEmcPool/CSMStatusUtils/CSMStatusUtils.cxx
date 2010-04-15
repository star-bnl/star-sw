/*
Indexing is an issue in this code, so I'm going to be explicit.
When I read from the data, the data are indexed from 1 to N.
When I write the data to the histograms, it's from 1 to N.
When I write text data to the status files, it's from 1 to N.
EVERYTHING is from 1 to N.

except the db ROOT files.
db ROOT files are from 0 to N-1.
I explicitly write them this way.
*/


#include "CSMStatusUtils.h"

#include "TMath.h"
#include "TH2.h"
#include "TAxis.h"
#include "TROOT.h"
#include "TKey.h"
#include "TIterator.h"
#include "TFile.h"
#include "TSystem.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "tables/St_emcStatus_Table.h" 
#include "tables/St_emcPed_Table.h" 
#include "StEmcUtil/database/StEmcDecoder.h"

#include <iostream>
#include <fstream>
#include <iomanip>
#include <map>
#include <set>
#include <string>
#include <stdio.h>

using namespace std;

typedef map<Int_t,vector<Short_t>*>::const_iterator IntToPtrVecShortConstIter ;

//this sets the detector type
void
CSMStatusUtils::setDetectorFlavor(TString flavor) {
  mDetectorFlavor=flavor;
  if(mDetectorFlavor=="bemc") {
    mDetectorSize=4800;
    mDetectorActualSize=4800;
    mRunStatusMapPtr=&mBEMCRunStatusMap;
  } else if(mDetectorFlavor=="eemc") {
    mDetectorSize=720;
    mDetectorActualSize=720;
    mRunStatusMapPtr=&mEEMCRunStatusMap;
    TString tms;
    ifstream ifs("StRoot/StEmcPool/CSMStatusUtils/eemccratemap");
    int tower, crate, channel;
    while(!ifs.eof()) {
      tms.ReadLine(ifs);
      sscanf(tms.Data(),"%d %d %d",&crate,&channel,&tower);
      eemcCrateMap[crate-1][channel] = tower;
    }
    ifs.close();
  }
}

//this takes the files containing the 2D histograms for each run
//and puts their names into mHistFileMap

Int_t 
CSMStatusUtils::initializeHistFileFromDir(TString directory,TString filter) {
  
  void *dir = NULL;
  if ((dir = gSystem->OpenDirectory(directory.Data())) != NULL) {
    const Char_t *dirEntry;
    while ((dirEntry = gSystem->GetDirEntry(dir)) != NULL) {
      Char_t buffer[2048];
      strcpy(buffer,directory.Data());
      if (buffer[strlen(buffer)-1] != '/') strcat(buffer,"/");
      strcat(buffer,dirEntry);
      if (!strstr(buffer,filter.Data())) continue;
      Char_t* needle = strstr(buffer,"run");
      Int_t runNumber = 0;
      if (needle) {
        needle+=3;
        Char_t runString[10];
        strncpy(runString,needle,7);
        runNumber = atoi(runString);
      }
      if (runNumber != 0) mHistFileMap[runNumber] = buffer;
    }
  }
  return mHistFileMap.size(); 
}

//this fills mRunStatusMap with information from status files
//it also fills the time- and date-stamp maps with run specific info

Int_t
CSMStatusUtils::readTablesFromASCII(TString directory,TString filter) {
  void* dir = NULL;
  TString buffert = directory + "/status/";
  TString tmpstr;
  if ((dir = gSystem->OpenDirectory(buffert.Data())) != NULL) {
    const Char_t *dirEntry;
    while ((dirEntry = gSystem->GetDirEntry(dir)) != NULL) {
      Char_t buffer[2048];
      strcpy(buffer,buffert.Data());
      strcat(buffer,dirEntry);
      if (!strstr(buffer,filter.Data())) continue;
      tmpstr = dirEntry;
      if(!tmpstr.Contains(mDetectorFlavor.Data())) continue;
      Char_t* needle = strstr(buffer,"run");
      Int_t runNumber = 0, thetime = 0, thedate = 0;
      if (needle) {
        needle+=3;
        Char_t runString[10];
        Char_t timeString[10];
        Char_t dateString[10];
        strncpy(runString,needle,7);
        runNumber = atoi(runString);
        needle+=13;
        strncpy(dateString,needle,8);
        thedate = atoi(dateString);
        needle+=9;
        strncpy(timeString,needle,6);
        thetime = atoi(timeString); 
      }
      if (runNumber != 0) {
        ifstream in(buffer);
        if(in.good()) {
          Int_t status,itemp;
          vector<Short_t>* vec = new vector<Short_t>(mDetectorSize+1);
          for(int id=1; id<mDetectorSize+1; id++) {
//        cout << buffer << "mrz" << endl;
            in >> itemp >> status;
//        cout << status << "mrz" << itemp << endl;
             (*vec)[id] = status;
          }
cout << buffer << " is status file that was read" << endl;
          (*mRunStatusMapPtr)[runNumber] = vec;
          mRunTimestampMap[runNumber] = thetime;
          mRunDatestampMap[runNumber] = thedate;
//          cout << mRunDatestampMap[runNumber] << "\t" << mRunTimestampMap[runNumber] << "\t" << runNumber << endl;
//          cout << thedate << "aaa" << thetime << "\t" << runNumber << endl;
        }
        in.close();
      }
    }
  }  
  return mRunStatusMapPtr->size();
}

void
CSMStatusUtils::plotAllStatuses(TString rootfiledir,int year,int runstart) {
  void* dir = NULL;
  TString tmpstr;
  vector<Int_t> runlist;
  int runnumber;
  if ((dir = gSystem->OpenDirectory(rootfiledir.Data())) != NULL) {
    const Char_t *dirEntry;
    while ((dirEntry = gSystem->GetDirEntry(dir)) != NULL) {
      Char_t buffer[2048];
      strcpy(buffer,rootfiledir.Data());
      strcat(buffer,dirEntry);
      if (!strstr(buffer,".root")) continue;
      if (strstr(buffer,"run.root")) continue;
      Char_t* needle = strstr(buffer,"run");
      Int_t runNumber = 0;
      if (needle) {
        needle+=3;
        Char_t runString[10];
        strncpy(runString,needle,7);
        runNumber = atoi(runString);
        runlist.push_back(runNumber);
      }
    }
  }
  tm ptm;
  ptm.tm_year = year-1900;
  int firstdate=99999999, firsttime,lastdate=0, lasttime, firstrun=99999999, lastrun=0;
  IntToPtrVecShortConstIter first = mRunStatusMapPtr->begin();
  IntToPtrVecShortConstIter last = mRunStatusMapPtr->end();
  IntToPtrVecShortConstIter iter = first;
  for(iter=first;iter!=last;iter++) {
    runnumber = iter->first;
    if(runnumber < runstart) continue;
    if(firstdate > mRunDatestampMap[runnumber]) {
      firstdate = mRunDatestampMap[runnumber];
      firsttime = mRunTimestampMap[runnumber];
    } else if (firstdate == mRunDatestampMap[runnumber] && firsttime > mRunTimestampMap[runnumber]) {
      firsttime = mRunTimestampMap[runnumber];
    }
    if(lastdate < mRunDatestampMap[runnumber]) {
      lastdate = mRunDatestampMap[runnumber];
      lasttime = mRunTimestampMap[runnumber];
    } else if (lastdate == mRunDatestampMap[runnumber] && lasttime < mRunTimestampMap[runnumber]) {
      lasttime = mRunTimestampMap[runnumber];
    }
    if(firstrun > runnumber && runnumber > runstart) firstrun = runnumber;
    if(lastrun < runnumber) lastrun = runnumber;
  }
//cout << firstdate << "\t" << firsttime <<  "\t" << lastdate <<  "\t" << lasttime << endl;
  int bigyearnumber = year*10000;
//cout << bigyearnumber << endl;
  ptm.tm_mon = ((firstdate-bigyearnumber)/100)+1;
  ptm.tm_mday = (firstdate-bigyearnumber)%100;
  ptm.tm_hour = firsttime/10000;
  ptm.tm_min = (firsttime%10000)/100;
  ptm.tm_sec = firsttime%100;
  time_t firstdatet = mktime(&ptm);
  ptm.tm_mon = ((lastdate-bigyearnumber)/100)+1;
  ptm.tm_mday = (lastdate-bigyearnumber)%100;
  ptm.tm_hour = lasttime/10000;
  ptm.tm_min = (lasttime%10000)/100;
  ptm.tm_sec = lasttime%100;
  time_t lastdatet = mktime(&ptm);
//  int difference = lastrun - firstrun;
  int diffseconds = (int)difftime(lastdatet,firstdatet);
  TCanvas* tc = new TCanvas("peter","Status vs Run Number",800,800);
  tc->cd();
  int timegap = 3600;
//cout << diffseconds << "\t" << timegap << "\t" << diffseconds/timegap << endl;
  TH2F* histogram = new TH2F("stevegeech","Time (Hours) vs BEMC Channels (bad)",4801,-0.5,4800.5,diffseconds/timegap+1,-0.5,diffseconds/timegap+0.5);
//cout << diffseconds/timegap << "is the difference" << endl;
  //  TH2F* histogram = new TH2F("stevegeech","Status vs Run Number",4801,-0.5,4800.5,runlist.size()+1,-0.5,runlist.size()+0.5);
/*  for(int i=0; i<runlist.size(); i++) {
    runnumber = runlist[i];
    cout << i<< runnumber << endl;
    if(runnumber<6100000 || runnumber>6130000) continue;
    vector<Short_t>* statusVector = (*mRunStatusMapPtr)[runnumber];
    if(statusVector) {
      for(int j=1; j<2401; j++) {
//        if((*statusVector)[j]==1) histogram->Fill(j,runnumber);
//      cout << i << endl;
      }
    }
  }*/
  vector<int> binbounds;
  int currentbin;
  for(iter=first;iter!=last;iter++) {
    runnumber = iter->first;
    if(runnumber < runstart) continue;
    vector<Short_t>* statusVector = (*mRunStatusMapPtr)[runnumber];
    if(statusVector) {
      ptm.tm_mon = ((mRunDatestampMap[runnumber]-bigyearnumber)/100)+1;
      ptm.tm_mday = (mRunDatestampMap[runnumber]-bigyearnumber)%100;
      ptm.tm_hour = mRunTimestampMap[runnumber]/10000;
      ptm.tm_min = (mRunTimestampMap[runnumber]%10000)/100;
      ptm.tm_sec = mRunTimestampMap[runnumber]%100;
      time_t timet = mktime(&ptm);
      double timedifference = difftime(timet,firstdatet);
      int ratiodifftogap = (int)timedifference/timegap;
      currentbin = histogram->GetBin(0,ratiodifftogap)/(mDetectorSize+3);
//cout << runnumber << "\trrrr" << currentbin << endl;
      binbounds.push_back(currentbin);
    }
  }
  binbounds.push_back(currentbin);
  int bb=0;
  for(iter=first;iter!=last;iter++) {
    runnumber = iter->first;
    if(runnumber < runstart) continue;
    vector<Short_t>* statusVector = (*mRunStatusMapPtr)[runnumber];
    if(statusVector) {
//      ptm.tm_mon = ((mRunDatestampMap[runnumber]-bigyearnumber)/100)+1;
//      ptm.tm_mday = (mRunDatestampMap[runnumber]-bigyearnumber)%100;
//      ptm.tm_hour = mRunTimestampMap[runnumber]/10000;
//      ptm.tm_min = (mRunTimestampMap[runnumber]%10000)/100;
//      ptm.tm_sec = mRunTimestampMap[runnumber]%100;
//      time_t timet = mktime(&ptm);
//      double timedifference = difftime(timet,firstdatet);
//cout << timedifference/timegap << endl;      
//cout << binbounds[bb] << "\t" << binbounds[bb+1] << endl;      
      for(int i=1; i<4801; i++) {
        if((*statusVector)[i]!=1) {
          for(int bound=binbounds[bb]; bound<binbounds[bb+1]; bound++) {
            histogram->Fill(i,bound);
//cout << "I fill here" << i << endl;
          }
        }
      }
//cout << "ended the filling" << endl;
    }
    bb++;
  }
  histogram->Draw("colz");
}

//this finds out which towers' statuses changed between runs
//and writes only the changes to the abbreviated status files

//however, currently it ALWAYS writes out a root files, regardless
//of the number of changes.  this may change.

Int_t
CSMStatusUtils::saveAbbreviatedStatusTablesToASCII(TString directory) {

  TString tmpstr;
  int runnumber;
  IntToPtrVecShortConstIter first = mRunStatusMapPtr->begin();
  IntToPtrVecShortConstIter last = mRunStatusMapPtr->end();
  IntToPtrVecShortConstIter iter = first;
  IntToPtrVecShortConstIter preiter = first;

//first, we want to catch channels which flip back and forth between
//"good" and "bad" states, ie channels whose pedestals are close to
//the boundaries, or channels who had intermittently stuck bits.
//For all of these channels, if they flip more than 10 percent of 
//the analyzed runs, we flag them as bad permanently.
//also, due to the barrel scheme, I have to introduce a fake zero
//for the bookkeepping.  it's fairly obvious how this works.

  vector<Float_t> statuscounter(mDetectorSize+1);
  vector<Short_t> totalStatusVector(mDetectorSize+1);
  for(int i=0; i<mDetectorSize+1; i++) {
    totalStatusVector[i] = 1;
    statuscounter[i] = 0;
  }
  Short_t fakeZero = 1024;
  Bool_t firstone = kTRUE;
  Short_t oldstatus, status;
  Int_t badChannelsInRun;
  for(iter=first;iter!=last;iter++) {
    runnumber = iter->first;
    vector<Short_t>* statusVector = iter->second;
    vector<Short_t>* oldStatusVector = preiter->second;
    badChannelsInRun = 0;
    for (Int_t i = 1; i < mDetectorSize + 1; i++) {
      oldstatus = (*oldStatusVector)[i];
      status = (*statusVector)[i];
      if(oldstatus == 0) oldstatus = fakeZero;
      if(status == 0) status = fakeZero;

//      if(i == 1) cout << oldstatus << "\t" << status << endl;

//this part sets the "absolute" status vector, namely the vector
//that gives the worst possible state of the channel over all runs

      if(firstone) {  //initialization
        totalStatusVector[i] = status;
      } else if(oldstatus == 1 && status == 1) {
        continue;
      } else if(status != 1) {
        if(totalStatusVector[i] == 1) {
          totalStatusVector[i] = status;
        } else {
          totalStatusVector[i] |= status;
        }
      } else if(oldstatus != 1) {
        if(totalStatusVector[i] == 1) {
          totalStatusVector[i] = oldstatus;
        } else {
          totalStatusVector[i] |= oldstatus;
        }
      } else {  //both != 1
        if(totalStatusVector[i] == 1) {
          totalStatusVector[i] = (oldstatus | status);
        } else {
          totalStatusVector[i] |= (oldstatus | status);
        }
      }
//how many channels changed status for this run?
//the logic is that if you've gotten here, at least one of
//(status,oldstatus) is bad
      if(i < mDetectorActualSize) badChannelsInRun++;
      
//the next line records the number of times the channel changed status      
      if (oldstatus != status) statuscounter[i] += 1;
    } //channels
//for really crappy runs, don't count channel changes toward the maximum number
    for (Int_t i = 1; i < mDetectorSize + 1; i++) {
      oldstatus = (*oldStatusVector)[i];
      status = (*statusVector)[i];
      if(badChannelsInRun > 0.5 * mDetectorActualSize && oldstatus != status)
        statuscounter[i] -= 1;
    }
    preiter = iter;
    firstone = kFALSE;
  } //runs
  
//now we write all status bits to either the root files or text files
  iter = first;
  preiter = first;
  TString datetimestring, runnumberstring;
  firstone = kTRUE;
  for(iter=first; iter!=last; iter++) {
    int numberofchangedchannels=0;
    runnumber = iter->first;
    runnumberstring = "";
    runnumberstring += runnumber;
    tmpstr = directory + "/status/short_status_" + mDetectorFlavor + "_run"
        + runnumberstring + ".status";
    ofstream ofs(tmpstr.Data());

    St_emcStatus *bemc_status=new St_emcStatus("bemcStatus",1);
    emcStatus_st *emcstatus=bemc_status->GetTable();
    for (Int_t i_tow=0; i_tow<4800; i_tow++) emcstatus->Status[i_tow]=1;
//for eemc, I will eventually do unixtime (stop and start)
    vector<Short_t>* statusVector = iter->second;
    vector<Short_t>* oldStatusVector = preiter->second;
    for (UInt_t i = 1; i < statusVector->size(); i++) {
      oldstatus = (*oldStatusVector)[i];
      status = (*statusVector)[i];
//if this is the first run and the channel changes status a lot,
//write down the sum of all bad statuses it has for all runs
      if ( firstone && (statuscounter[i]/mRunStatusMapPtr->size() > 0.1 ) && mRunStatusMapPtr->size() > 3) {
//account for the fake zero
        
	cout << "statcou is " << statuscounter[i] << " for channel " << i << " and size is " << mRunStatusMapPtr->size() << endl;
        if(totalStatusVector[i] & fakeZero) ofs << i << "\t" << "0" << endl;    
        else ofs << i << "\t" << totalStatusVector[i] << endl;        
//otherwise, only write down the status if this is the first run,
//or if the status has changed
      } else if (firstone || 
         (oldstatus != status && (statuscounter[i]/mRunStatusMapPtr->size() <= 0.1 || mRunStatusMapPtr->size() <= 10))) {
        ofs << i << "\t" << status << endl;
        numberofchangedchannels++;
      }
      //Optional if statement to print these flickering status towers as permanently bad in .root files --- D.Staszak 11/06
      /*if ((statuscounter[i]/mRunStatusMapPtr->size() > 0.1 ) && mRunStatusMapPtr->size() > 3) {
	emcstatus->Status[i-1]=totalStatusVector[i];
      } else {
	emcstatus->Status[i-1]=status;
	}*/
      //Original version - line below:
      emcstatus->Status[i-1]=status; //indexed from 0
    }
    ofs << numberofchangedchannels << " channels changed" << endl;
    ofs.close();
    datetimestring = getDateTimeString(runnumber);
    TString statusrootfilename = 
        directory + "/status/" + mDetectorFlavor + "Status" + datetimestring + "root";
    TFile* fout_status = new TFile(statusrootfilename.Data(),"RECREATE");
    fout_status->cd();
    bemc_status->AddAt(emcstatus,0);
    bemc_status->Write();
    fout_status->Close();
    delete bemc_status;
    delete fout_status;

    preiter = iter;
    firstone = kFALSE;
  }
  return 0;
}

//this takes HistFileMap, opens each file in it, creates the
//hot tower histogram, calls analyseStatusHistogram, and if there
//are enough statistics in the run, it will draw the
//histograms, and output to an html file the abbreviated results,
//such as the number of good towers in a run.

//the logic is outlined here:
//"RI" refers to a run's (or set of runs') Run Information:
    //2d histogram, earliest run number, and earliest time and datestamps
//there are three separate RIs: 
//1. the run RI (of the run being analyzed),
//2. the current RI (maybe of multiple runs),
//3. the prior RI (ditto)

//for(all runs)
    //[implied "if(run is last in fill) set Last in Fill Flag (LFF)"]

    //add this run's RI to the current RI
    //analyze the current RI
    
    //if(statistics acceptable)
      //set prior RI = current RI
      //clear current RI (but prior RI is kept!)
    //else if(LFF)
      //add prior RI to the current RI
      //analyze current RI
    //else [implied "go to next run if !LFF and statistics low"]
      //continue

    //save QA info
    
    //if(LFF)
      //clear current RI and prior RI
      
//It then looks to see if the status of a particular channel has
//changed from the last RI to this RI, and if it has, 
//it saves that channel's histograms.
//It will also run to Taco Bell if you are hungry.
//It is *that* versatile.

Int_t
CSMStatusUtils::makeStatusPlots(TString plotDir) {

//set up graphics
  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);
  TString tmpstr;
  tmpstr = mDetectorFlavor + "StatusPlots";
  TCanvas *c1 = new TCanvas(tmpstr.Data(),tmpstr.Data());
  c1->SetLogy();
  c1->Draw();
  TCanvas *c2 = new TCanvas("towerAdcPlots","towerAdcPlots",400,400);
  c2->SetLogy();
  c2->Draw();

//set up output html file
  tmpstr = plotDir + mDetectorFlavor + "Status.html";
  cout << "htmlSummary: " << tmpstr << endl;
  ofstream htmlSummary(tmpstr.Data());   //
  if (!htmlSummary) cout << "htmlSummary isn't defined!" << endl;
  writeHtmlHeaderSummary(htmlSummary);   //
  
  Char_t buffer[2048];

  findFillEnds();

  
  TH2F* priorHist = NULL;
  TH2F* currentHist = NULL;
  TH2F* tmpHist = NULL;
  TString runnumberstring;
  int runnumber, priorRunNumber, currentRunNumber=99999999, savedCurrentRunNumber;
  int priorTimeStamp, currentTimeStamp=99999999;
  int priorDateStamp, currentDateStamp=99999999;
  Bool_t firstGoodRun = kTRUE;
  Float_t averageNumberHitsPerChan;
  Int_t goodTowers;
  
  std::vector<Short_t>* statusVector;
  std::vector<Float_t>* pedestalmean;
  std::vector<Float_t>* pedestalwidth;
  std::vector<Float_t>* pedestalchi;
  TH1F* hHotTower;
  Int_t hottowerPlotNameIter = -1;
  ofstream outputlog("MOSTRECENTLOG.txt");

  for (map<Int_t,string>::const_iterator iter = mHistFileMap.begin();
        iter != mHistFileMap.end(); ++iter) {
//        iter != mHistFileMap.end(); iter = mHistFileMap.end()) {
    TFile* file = new TFile(iter->second.c_str(),"READ");
outputlog << "doing file " << iter->second.c_str() << endl;
cout << "doing file " << iter->second.c_str() << endl;
    if (file && file->IsOpen()) {
outputlog << " it opened" << endl;
cout << " it opened" << endl;
      runnumber = iter->first;
      //      cout << "note - iter->first: " << iter->first;
      runnumberstring = "";
      runnumberstring += runnumber;
      hottowerPlotNameIter++;
      tmpstr = mDetectorFlavor + "StatusAdc_" + runnumberstring;
      TH2F* runHist = dynamic_cast<TH2F*>(file->Get(tmpstr.Data()));
      TTree* myTree = dynamic_cast<TTree*>(file->Get("calinfo"));
      assert(runHist);
      assert(myTree);

//if first good run, initialize currentHist and priorHist
//otherwise, add run histogram to currentHist
      if(firstGoodRun) {
outputlog << "it's the first good run" << endl;
cout << "it's the first good run" << endl;
        currentHist = dynamic_cast<TH2F*>(runHist->Clone("ch1"));
        currentHist->SetDirectory(0);
        priorHist = dynamic_cast<TH2F*>(runHist->Clone("ph1"));
        priorHist->SetDirectory(0);
        priorHist->Reset();
        firstGoodRun = kFALSE;
      } else {
        currentHist->Add(runHist);
      }
      
      setDateTimeInfo(runnumber,myTree);

      //Keep track of the fill number D.Staszak 8/06
      Float_t runFillNum;
      myTree->SetBranchAddress("fillnum",&runFillNum);
      myTree->GetEvent(0);
      //if(runFillNum==0) {
      //cout << iter->second.c_str() << endl;
        cout<<"runfillnumb is " << runFillNum << endl;
	//}
	//assert(runFillNum);

      
      if(runnumber < currentRunNumber) currentRunNumber = runnumber;
      runnumberstring = "";
      runnumberstring += currentRunNumber;
      savedCurrentRunNumber = currentRunNumber;
      if(mRunDatestampMap[runnumber] < currentDateStamp) {
        currentDateStamp = mRunDatestampMap[runnumber];
        currentTimeStamp = mRunTimestampMap[runnumber];
      } else if(mRunDatestampMap[runnumber] == currentDateStamp &&
                  mRunTimestampMap[runnumber] < currentTimeStamp) {
        currentTimeStamp = mRunTimestampMap[runnumber];
      }
        
//analyze this RI
//warning - this code will have a LOT of memory leakage until I clean it
      statusVector = new vector<Short_t>(mDetectorSize+1);
      pedestalmean = new vector<Float_t>(mDetectorSize+1);
      pedestalwidth = new vector<Float_t>(mDetectorSize+1);
      pedestalchi = new vector<Float_t>(mDetectorSize+1);
      
      tmpstr = "hotTower";
      tmpstr += hottowerPlotNameIter;

      hHotTower = new TH1F(tmpstr.Data(),"# of tower hits",mDetectorSize+1,-0.5,mDetectorSize+1-0.5);
      hHotTower->GetXaxis()->SetTitle("Tower Id");
      hHotTower->GetYaxis()->SetTitle("Number of Hits Above Pedestal");


      // analyze
      goodTowers = analyseStatusHistogram(currentHist,plotDir,averageNumberHitsPerChan,
          currentDateStamp,currentTimeStamp,
          *statusVector,*pedestalmean,*pedestalwidth,*pedestalchi,hHotTower);

      if(averageNumberHitsPerChan > 100) {
//set prior RI to current RI; clear current RI
outputlog << " good statistics!" << endl;
 cout << " good statistics!" << endl << endl;
        //------For Single Run, to use html comment out below-------//
         priorHist->Reset();
	 tmpHist = priorHist;
	 priorHist = currentHist;
	 currentHist = tmpHist;
	 priorTimeStamp = currentTimeStamp;
	 currentTimeStamp = 99999999;
	 priorDateStamp = currentDateStamp;
	 currentDateStamp = 99999999;
	 priorRunNumber = currentRunNumber;
	 currentRunNumber = 99999999;
	//----------------------------------------------//
      } else if(mFillEndMap[runnumber]) {
outputlog << "end of fill and poor statistics!    average: " << averageNumberHitsPerChan << endl;
cout << "end of fill and poor statistics!     average: " << averageNumberHitsPerChan << endl << endl;
        currentHist->Add(priorHist);
        if(priorRunNumber < currentRunNumber) currentRunNumber = priorRunNumber;
        runnumberstring = "";
        runnumberstring += currentRunNumber;
        savedCurrentRunNumber = currentRunNumber;
        if(priorDateStamp < currentDateStamp) {
          currentDateStamp = priorDateStamp;
          currentTimeStamp = priorTimeStamp;
        } else if(priorDateStamp == currentDateStamp &&
                    priorTimeStamp < currentTimeStamp) {
          currentTimeStamp = priorTimeStamp;
        }
        delete statusVector;
        delete pedestalmean;
        delete pedestalwidth;
        delete pedestalchi;
        statusVector = new vector<Short_t>(mDetectorSize+1);
        pedestalmean = new vector<Float_t>(mDetectorSize+1);
        pedestalwidth = new vector<Float_t>(mDetectorSize+1);
        pedestalchi = new vector<Float_t>(mDetectorSize+1);
        hHotTower->Reset();
        hHotTower->GetXaxis()->SetTitle("Tower Id");
        hHotTower->GetYaxis()->SetTitle("Number of Hits Above Pedestal");
        goodTowers = analyseStatusHistogram(currentHist,plotDir,averageNumberHitsPerChan,
                  currentDateStamp,currentTimeStamp,
                  *statusVector,*pedestalmean,*pedestalwidth,*pedestalchi,hHotTower);
	  if(averageNumberHitsPerChan < 50) {
outputlog << " fill has ended and stats suck!  Number is " << averageNumberHitsPerChan << endl;
cout << " fill has ended and stats suck!  Number is " << averageNumberHitsPerChan << endl << endl;
          priorHist->Reset();
          currentHist->Reset();
          priorTimeStamp = 99999999;
          currentTimeStamp = 99999999;
          priorDateStamp = 99999999;
          currentDateStamp = 99999999;
          priorRunNumber = 99999999;
          currentRunNumber = 99999999;
          delete statusVector;
          delete pedestalmean;
          delete pedestalwidth;
          delete pedestalchi;
          delete hHotTower;
          file->Close();
          delete file;
          continue;
	  }         
      } else {
outputlog << " poor statistics!    average: " << averageNumberHitsPerChan << endl;
cout << " poor statistics!    average: " << averageNumberHitsPerChan << endl;
        delete statusVector;
        delete pedestalmean;
        delete pedestalwidth;
        delete pedestalchi;
        delete hHotTower;
        file->Close();
        delete file;
        continue;
	}
      
      if(mFillEndMap[runnumber]) {
outputlog << " fill has ended!" << endl;
 cout << " fill has ended!" << endl << endl;
        //-----For Single Run, to use html comment out below---------// 
        //priorHist->Reset();  // Comment this out for multi-run Fill running
        currentHist->Reset();
        priorTimeStamp = 99999999;
	currentTimeStamp = 99999999;
	priorDateStamp = 99999999;
	currentDateStamp = 99999999;
	priorRunNumber = 99999999;
	currentRunNumber = 99999999;
	//-----------------------------------------------//
      }

      cout << "Got here? 1 " << endl;

//if the RI has less than 10% of the towers functioning
//ignore the html file
/*      if (goodTowers < 0.05 * mDetectorSize) {
outputlog<<"special case - everything sucks!" << endl;
 cout<<"special case - everything sucks!" << endl;
 htmlSummary << "<tr> <td>" << runnumber << "</td>"          //
	     << "<td> BAD </td> <td> - </td> <td> - </td>"     //
	     << "<td> - </td> <td> - </td> <td> - </td> </tr><br>"  //
	     << endl;    //
        delete statusVector;
        delete pedestalmean;
        delete pedestalwidth;
        delete pedestalchi;
        delete hHotTower;
        file->Close();
        delete file;
        continue;
	}*/

      (*mRunStatusMapPtr)[savedCurrentRunNumber] = statusVector;
//write out pedestals for this run
      writePedestals(savedCurrentRunNumber,plotDir,*statusVector,*pedestalmean,*pedestalwidth,*pedestalchi);
        
// save tower status bits to a text file
      tmpstr = plotDir + "/status/";
      saveStatusTablesToASCII(tmpstr.Data(), savedCurrentRunNumber);
                
      gStyle->SetOptStat(1111);
      c1->cd();
      hHotTower->Draw();
      c1->Update();
      tmpstr = plotDir + "/run" + runnumberstring + "_" + mDetectorFlavor + "_hotTowers.gif";
      if(gROOT->IsBatch()) {
        tmpstr = plotDir + "/run" + runnumberstring + "_" + mDetectorFlavor + "_hotTowers.eps";
      }
      c1->SaveAs(tmpstr.Data());
      delete hHotTower;

      htmlSummary << "<tr>" << endl    //
		  << "<td>" << "Fill " << runFillNum << "</td>" << ", " << endl // 
                  << "<td> " << "Run " << savedCurrentRunNumber << " </td> " << endl   //
                  << "<td> " << goodTowers << " good towers" << " </td>" << endl   //
                  << "<td> " << getNumberOfChangedTowers(savedCurrentRunNumber) << " towers changed from previous run"//run #  //
                  << " </td>" << endl;   // 

      tmpstr = "./run" + runnumberstring + "_" + mDetectorFlavor + "_badTowers.html";
      htmlSummary << "<td> <a href=\"" << tmpstr.Data() << "\"> list </a></td><br>"   // 
                  << endl;   //

// save tower status bits to the htmlfile

      tmpstr = plotDir + "/run" + runnumberstring + "_" + mDetectorFlavor + "_badTowers.html";
      ofstream htmlout(tmpstr.Data());    //
      writeHtmlHeaderBadTowerList(htmlout,savedCurrentRunNumber);  //
        

// check if first run - if yes plot every bad tower
// if a previous run exists just plot towers which changed status to bad,
// unless there are more than 25 bad towers, in which case, don't,
// since disk space is apparently "important"
      for (Int_t i=1; i<=mDetectorSize; i++) {
	if ((*statusVector)[i] != 1 && (*statusVector)[i] != 18 && (*statusVector)[i] != 0) {

          htmlout << "<tr> <td> " << i << " </td> <td> "           ///
		  << (*statusVector)[i] << " </td> <td> " << endl;  ///
	  
          IntToPtrVecShortConstIter statusIter;
          statusIter = mRunStatusMapPtr->find(savedCurrentRunNumber);
	  
          if (statusIter != mRunStatusMapPtr->begin()) {  //checking to make sure not the first run
            IntToPtrVecShortConstIter preIter = statusIter;
            preIter--;
 
	    if ((*(statusIter->second))[i] == (*(preIter->second))[i] ||
		(*(statusIter->second))[i] == 1 || //good channel
		(*(statusIter->second))[i] == 0) { //no need to plot dead channels
	      htmlout << "- </td> </tr><br>" << endl;  ///
              continue;
	    }
            if(getNumberOfChangedTowers(runnumber) > 25) {
	      //cout << ">25 changed towers --- " << endl;
	      htmlout << "- </td> </tr><br>" << endl;  ///
	      continue;   
	    }
	  }
	  
	  // For single running, use currentHist to ouput plots
	  //Int_t bin = currentHist->GetXaxis()->FindFixBin(i);
	  //TH1D *hTemp = currentHist->ProjectionY("projTemp",bin,bin);
	  
	  // For multi-run fills, use priorHist to output plots, also, comment out reset above
	  Int_t bin = priorHist->GetXaxis()->FindFixBin(i);
	  TH1D *hTemp = priorHist->ProjectionY("projTemp",bin,bin);
	  c2->cd();
          c2->Clear();
	  hTemp->GetXaxis()->SetTitle("adc");   ///
	  hTemp->GetXaxis()->SetRange(0,150);
	  hTemp->Draw();     ///
	  c2->Update();    ///
	  sprintf(buffer,"%s/run%dtower%d_adc.gif",plotDir.Data(),iter->first,i);   ///   // Simple fix plotDir -> plotDir.Data() D.Staszak
	  c2->SaveAs(buffer);   /// 
	  sprintf(buffer,"./run%dtower%d_adc.gif",savedCurrentRunNumber,i);
	    
	  htmlout << "<a href=\"" << buffer << "\" > plot </a>"   ///
		  << "</td> </tr>" << endl;    ///
          delete hTemp;
        }
      }

            
// Now output a default of 5 good towers for comparison (for the first run in group/fill only)  D.Staszak 6.06
      IntToPtrVecShortConstIter statusIter;
      statusIter = mRunStatusMapPtr->find(savedCurrentRunNumber);
      if (statusIter == mRunStatusMapPtr->begin()) {  //checking that it's the first run 
	 
	htmlout << "</tbody>" << endl;
 	htmlout << "</table>" << endl;
	htmlout << "<br><br> <div>Partial Good Tower List for Comparison</div><br>" << endl;
	htmlout << "<table border=\"1\">" << endl;
	htmlout << "<tbody>" << endl;
	htmlout << "<tr> <th width=\"50\"> Tower ID </th> <th width=\"50\"> Status Code </th> <th width=\"100\"> ADC plot </th> </tr>" << endl;	
	
	Int_t good_toplot = 1;
	for (Int_t i=100; i<mDetectorSize && good_toplot<10; i = i+100) {  // Set good_toplot to iterate below to restrict number of good plots printed
	  if ((*statusVector)[i] == 1) {
	    htmlout << "<tr> <td> " << i << " </td> <td> "           ///
		    << (*statusVector)[i] << " </td> <td> " << endl;  ///
	    
	    //Int_t bin = currentHist->GetXaxis()->FindFixBin(i);
	    //TH1D *hTemp = currentHist->ProjectionY("projTemp",bin,bin);
	    Int_t bin = priorHist->GetXaxis()->FindFixBin(i);
	    TH1D *hTemp = priorHist->ProjectionY("projTemp",bin,bin);
	    //hTemp->GetXaxis()->SetRange(0,150);
	    c2->cd();
	    c2->Clear();
	    hTemp->GetXaxis()->SetTitle("adc");   ///
	    hTemp->Draw();     ///
	    c2->Update();    ///
	    
	    sprintf(buffer,"%s/run%dtower%d_adc.gif",plotDir.Data(),iter->first,i);   ///   // Simple fix plotDir -> plotDir.Data() D.Staszak
	    c2->SaveAs(buffer);   /// 
	    sprintf(buffer,"./run%dtower%d_adc.gif",savedCurrentRunNumber,i);
	    htmlout << "<a href=\"" << buffer << "\" > plot </a>"   ///
 		    << "</td> </tr>" << endl;    ///
	    delete hTemp;
	  }
	  good_toplot++;
	  
	}
      }
      writeHtmlFooterSummary(htmlout);   ///
      htmlSummary << "</tr>" << endl;    /// 
      htmlout.close();                   ///
      //        cout << "I got here and there is no problem yet" << endl;
      //        exit(1);
 
    }
    file->Close();
    delete file;
  }
  htmlSummary.close();   ///
  outputlog.close();
  writeHtmlFooterSummary(htmlSummary);   ///
  TH2F* statusHist = makeStatusVersusTimePlot();
  if (statusHist) {
    c1->cd();
    c1->SetLogy(kFALSE);
    c1->Clear();
    //statusHist->GetXaxis()->SetRangeUser(0.5,2400.5);
    statusHist->Draw("colz");
    sprintf(buffer,"%s/bemcStatusPlot.gif",plotDir.Data());  ///     //Same fix as above  D.Staszak
    c1->SaveAs(buffer);  ///
//    sprintf(buffer,"%s/bemcStatusPlot.svg",plotDir);
//    c1->SaveAs(buffer);
    delete statusHist;
  }
  delete c1;  
  delete c2;
  return 0; 
}

//this takes the 2d histograms and performs status checks on each 
//channel.  The output of the status checks is saved in statusVector.
//The tower frequency plot is saved in hHotTower.
//mDetectorFlavor defaults to "bemc", but can be "eemc"

Int_t 
CSMStatusUtils::analyseStatusHistogram(TH2F* hist,
                                          TString directory,
                                          Float_t& averageNumberOfHitsPerChannel,
                                          Int_t dateStamp,
                                          Int_t timeStamp,
                                          std::vector<Short_t>& statusVector,
                                          std::vector<Float_t>& pedestalmean,
                                          std::vector<Float_t>& pedestalwidth,
                                          std::vector<Float_t>& pedestalchi,
                                          TH1F* hHotTower,
                                          TH1F* hPedMean,
                                          TH1F* hPedWidth) {

  TString runnumber = hist->GetName();
  runnumber = runnumber(runnumber.Length()-7,7);

  void* dir;
//create the directory, if it doesn't exist
  if ((dir = gSystem->OpenDirectory(directory)) == NULL)
    gSystem->MakeDirectory(directory);

  TF1* gaus = new TF1("gaus","gaus");

// initialize status vector
  for (vector<Short_t>::iterator iter = statusVector.begin(); iter != statusVector.end(); ++iter)
    *iter = 0;

//loop through all the channels
  for (Int_t chanId = 1; chanId < mDetectorSize+1; chanId++) {

//the next line has the "+1"s because the first bin is underflow
    TH1D* proj = hist->ProjectionY("projTemp",chanId+1,chanId+1);
 
    if (proj) {

// find maximum, which should be the pedestal peak
      Int_t maxBin = 0;
      Float_t maxValue = -1;
      //      for (Int_t j = 1; j < proj->GetXaxis()->GetNbins(); j++) {
      for (Int_t j = 2; j < proj->GetXaxis()->GetNbins(); j++) {  // D.Staszak
        if (proj->GetBinContent(j) > maxValue) {
          maxBin = j;
          maxValue = proj->GetBinContent(j);
        }
      }
      Float_t pedMean = proj->GetXaxis()->GetBinCenter(maxBin);

//pedestal mean test  --- Modified this test to allow some below pedMean of 4, see below --- D.Staszak
//      if (mDetectorFlavor=="bemc" && (pedMean < 4 || pedMean > 145) ||
//          mDetectorFlavor=="eemc" && (pedMean < 3 || pedMean > 145)) {	
//	statusVector[chanId] |= 4;
//	cout << "Before fit - Mean too low --- Id:  " << chanId << "  mean: " << pedMean << endl;
//      }

// D.Staszak - Need to be careful about the influence of 0 bin on mean fit measurement.
// In 2006pp, many runs have multiple towers filled with a lot of 0 ADC counts. Other
// than this, the data is fine...so we just need to worry how it is affecting pedMean
      // --- Include the below line when running over files with '0's --- 
      //Float_t zero_chk = pedMean - 11;

      // fit a gaussian to the pedestal peak
      gaus->SetParameter(0,maxValue);
      gaus->SetParameter(1,pedMean);
      gaus->SetParameter(2,3.5);
      // --- Include this when running over files with '0's --- 
      //if (zero_chk < 1)
      //	gaus->SetRange(pedMean-(10.0+zero_chk),pedMean+(10.0+zero_chk));
      //else       
      // --- End comment ---
      gaus->SetRange(pedMean-10,pedMean+10);
      proj->Fit(gaus,"0RQ");
      
      if(hPedMean) hPedMean->Fill(gaus->GetParameter(1));
      if(hPedWidth) hPedWidth->Fill(gaus->GetParameter(2));
      
      pedestalmean[chanId] = gaus->GetParameter(1);
      pedestalwidth[chanId] = gaus->GetParameter(2);
      pedestalchi[chanId] = gaus->GetChisquare();

//pedestal width test
//SHOULD THIS BE DIFFERENT FOR THE EEMC???

      if (pedestalwidth[chanId] <= 0.5 || pedestalwidth[chanId] > 2.8) {
	//cout << "In wide ped, Id: " << chanId << "   width: " << pedestalwidth[chanId] << "   mean:   " << pedestalmean[chanId] << endl;
	statusVector[chanId] |= 4+32;
      }
  

//pedestal mean test
      if (mDetectorFlavor=="bemc" && (pedestalmean[chanId] < 4 || pedestalmean[chanId] > 145)) {
	if (pedestalmean[chanId] > 2.6 && pedestalwidth[chanId] < 1.5) {  //allow lower mean with thin width
	  //cout << "Saved: " << chanId << "  mean - " << pedestalmean[chanId] << "  width - "  << pedestalwidth[chanId] << endl;
	} else {
	  statusVector[chanId] |= 4;
	  //cout << "After fit - Still didn't make it " << chanId << endl;
	}
      }



//preparation for hot tower/cold tower test
//using a threshold of 10 sigma above pedestal
//FIRST PART OF COLD TOWER TEST DONE HERE NOW
      Int_t minBin = proj->GetXaxis()->FindFixBin(pedestalmean[chanId] + 10*pedestalwidth[chanId]);
      maxBin = proj->GetXaxis()->GetNbins() - 1;
      Int_t hottowerthreshold = minBin;
      if(hHotTower) {

        Float_t nHitsAbovePedestal = proj->Integral(hottowerthreshold,maxBin);

        if(nHitsAbovePedestal==0) {
          nHitsAbovePedestal=1; //(just for log plot sakes)


	  // Quick Fix to cold channels being labelled dead, D.Staszak
	  Float_t deadorno = proj->Integral(2,maxBin);
	  if (deadorno) {
	    nHitsAbovePedestal=2;  // Set at 2 because of a >2 cut above...again, quick fix here
	  }
	  else {
          statusVector[chanId] |= mZerobit;
	  //cout << chanId << "   has zero counts!" << endl;
	  }
	  //if (proj->Integral(1,maxBin) == 0) cout << chanId << "  truly has no counts!" << endl;

        }
        hHotTower->AddAt(nHitsAbovePedestal,chanId);
      }



//stuck bit test (off & on!)
//brief rant here - for checking whether or not bits are stuck on,
//I wanted to just include all bits from 1 to 64
//problem is, sometimes a channel has a high pedestal, and you
//only get hits above 64.  So that eliminates 64.  Also, sometimes
//a channel has a pedestal above 32, but the hit spectrum doesn't
//extend to 64 (this happens with surprising regularity).  That
//eliminates 32.
//I'm adding 16 to the mix because of an obviously stuck bit in the
//barrel.  To do so, I'm doing a separate loop with a separate test
//which searches for the stuck on/off state of the 16 bit, and then
//looks to see whether it can find 3 or more "clumps" of data.
//(in other words, N/16 = j, j must take on at least 3 values)

      int numberofnonzerohits = 0;
      Short_t bitoff = 0;
      Short_t bitcompare = (1+2+4+8);
      Short_t biton = bitcompare;
      Short_t sixteenbiton = 16;
      for(Short_t bin=1; bin<maxBin; bin++) {
        if(proj->GetBinContent(bin) > 0) {
          bitoff |= (bin-1);
          biton &= (bin-1);
          sixteenbiton &= (bin-1);
          numberofnonzerohits++;
        }
      }
      Bool_t typea=kFALSE,typeb=kFALSE,typec=kFALSE;
      if((bitoff & 16) != 16 || (sixteenbiton & 16) != 0) {  //these will not happen together
        for(Short_t bin=1; bin<maxBin; bin++) {
          if(proj->GetBinContent(bin) > 0) {
            if(!typea) {
              typea=kTRUE;
              bin = bin/16*16+15;
            } else if(!typeb) {
              typeb=kTRUE;
              bin = bin/16*16+15;
            } else if(!typec) {
              typec=kTRUE;
              break;
            }
          }
        }
      }
      
      if((bitoff & bitcompare) != bitcompare && numberofnonzerohits > 10) {
	cout << "136: " << chanId << "  bitoff: " << bitoff << endl;
        statusVector[chanId] = statusVector[chanId] | (8+128);
      }
      if(biton != 0 && numberofnonzerohits > 10) {
	cout << "72: " << chanId << "   biton: " << biton << endl;
        statusVector[chanId] = statusVector[chanId] | (8+64);
      }

      if((bitoff & 16) != 16 && typec && numberofnonzerohits > 10) {
	cout << "136: " << chanId << "  16" << endl;
        statusVector[chanId] = statusVector[chanId] | (8+128);
      }
      if((sixteenbiton & 16) != 0 && typec && numberofnonzerohits > 10) {
	cout << "72: " << chanId << "   16" << endl;
        statusVector[chanId] = statusVector[chanId] | (8+64);
      }

      for(Short_t bin=1; bin<maxBin; bin++) {
        if(proj->GetBinContent(bin) > 0) {
          bitoff |= (bin-1);
          biton &= (bin-1);
          numberofnonzerohits++;
        }
      }
      
//total number of hits test
      Float_t entries = proj->Integral(1,proj->GetXaxis()->GetNbins());
      if (entries == 0) {
        statusVector[chanId] |= mZerobit;  //channel has no pedestal?
	if (chanId==50 || chanId==139) cout << "entries=0   " << chanId << "   Status: " << statusVector[chanId] << endl;
      }


// D.Staszak --- Use this test to identify larger problems, as a first check - 
// a lot of entries in the zero bin is an indication of some problems 
// seen with crates, also this finds towers with a lot counts below the
// pedestal peak
    // --- Comment out for running with 0's present: ---
      /*
      if ( (proj->GetBinContent(1) > 10 && (proj->GetBinContent(2)<10 || proj->GetBinContent(3)<10 ) && (proj->Integral(2,proj->GetXaxis()->GetNbins()) > 0)) ||
          (proj->GetBinContent(1) > 50 && (proj->GetBinContent(2)<50 || proj->GetBinContent(3)<50 ) && (proj->Integral(2,proj->GetXaxis()->GetNbins()) > 0))
	   ) {
	if (pedestalmean[chanId] < 15.0) {
	  //cout << "Ignoring - Caught with entries in 0: " << chanId << "    Ped-mean: " << pedestalmean[chanId] << endl;
	} else {
	  //cout << "Caught with entries in 0: " << chanId << "    Ped-mean: " << pedestalmean[chanId] << endl;
	  statusVector[chanId] = statusVector[chanId] | (8+64);
	}
      } 
      */
    //--- End comment ----


      delete proj;
    } else {
//lack of histogram test
      statusVector[chanId] |= mZerobit;
    }
 
  }
  
  unsigned int date = dateStamp;
  unsigned int time = timeStamp;
  StEmcDecoder barry(date,time);
  Int_t towerId,nextTowerId;
  Bool_t histogramsAreSame;
  
//identical channel test

//towerId indexed from 1
//histogram projection is from 2 to N+1 (stupid... but true)

  if(mDetectorFlavor == "bemc") {
//30 crates, indexed from 1
//160 channels per crate, indexed from 0
    
    for(int crate=1; crate<31; crate++) {
      for(int channel=0; channel<160-1; channel++) {  //comparing adjacent channels
        histogramsAreSame = kTRUE;
        barry.GetTowerIdFromCrate(crate, channel, towerId);
        barry.GetTowerIdFromCrate(crate, channel+1, nextTowerId);

        TH1D* projnow = hist->ProjectionY("projTemp2",towerId+1,towerId+1);
        TH1D* projnext = hist->ProjectionY("projTemp3",nextTowerId+1,nextTowerId+1);

        for (Int_t i=1; i<projnow->GetXaxis()->GetNbins() && histogramsAreSame; i++) {
          if( projnow->GetBinContent(i) != projnext->GetBinContent(i))
            histogramsAreSame = kFALSE;
        }
        if(histogramsAreSame) {
          statusVector[towerId] |= 256;
          statusVector[nextTowerId] |= 256;
        }
      }
    }
  } else { //flavor is eemc
//6 crates, indexed from 0
//120 channels per crate, indexed from 0

    for(int crate=0; crate<6; crate++) {
      for(int channel=0; channel<120-1; channel++) {
        histogramsAreSame = kTRUE;
        towerId = eemcCrateMap[crate][channel];
        nextTowerId = eemcCrateMap[crate][channel+1];
//cout << towerId << nextTowerId << "is they!" << endl;
        TH1D* projnow = hist->ProjectionY("projTemp2",towerId+1,towerId+1);
        TH1D* projnext = hist->ProjectionY("projTemp3",nextTowerId+1,nextTowerId+1);

        for (Int_t i=1; i<projnow->GetXaxis()->GetNbins() && histogramsAreSame; i++) {
          if( projnow->GetBinContent(i) != projnext->GetBinContent(i))
            histogramsAreSame = kFALSE;
        }
        if(histogramsAreSame) {
          statusVector[towerId] |= 256;
          statusVector[nextTowerId] |= 256;
        }
      }
    }
  }
        

//hot tower/cold tower tests
  Float_t sumofhits=0, nbinhits=0, ncratehottowers=0;
  Int_t goodTowers = 0;
  for(int i=1; i<mDetectorSize+1; i++) {
    if(hHotTower->GetBinContent(i) > 2) {
      sumofhits += hHotTower->GetBinContent(i);
      nbinhits++;
    }
  }
  if(nbinhits!=0) {
//redo to nullify effects of hot and cold towers on the average
//individual channel tests
    averageNumberOfHitsPerChannel = sumofhits/nbinhits;
    for(int i=1; i<mDetectorSize+1; i++) {
      if ( i==509 || i==533 || i==1306 || i==1397 || i==1503 || i==1892 || i==1893 || i==2074 || i==2075 )  cout << i << "   Avg numberHits/chan= " << averageNumberOfHitsPerChannel << "  for this tower: " << hHotTower->GetBinContent(i) << endl;
      
      //if(hHotTower->GetBinContent(i) > 10*averageNumberOfHitsPerChannel) {
      //if(hHotTower->GetBinContent(i) > 5*averageNumberOfHitsPerChannel) {
      if( (hHotTower->GetBinContent(i) > 8*averageNumberOfHitsPerChannel) 
	  || (i==1503 && (hHotTower->GetBinContent(i) > 5*averageNumberOfHitsPerChannel)) // a couple channels in 06 that were ugly and missed often
	  || (i==1612 && (hHotTower->GetBinContent(i) > 5*averageNumberOfHitsPerChannel)) ) {
	statusVector[i] |= 2;
	cout << "ID: " << i << " average: " << averageNumberOfHitsPerChannel << "  this tower: " << hHotTower->GetBinContent(i) << endl;
      }
      if(hHotTower->GetBinContent(i) < averageNumberOfHitsPerChannel/40) statusVector[i] |= 2+16;
    }

//crate tests
//(tighter cold tower test for whole crates to catch timing problems)
//
//This is not trivial.
//Pretend that some crate is malfunctioning.
//I realized that the malfunction could take *any* form,
//meaning it could include both hot and cold towers.
//Consequently, my crate test now says the following:
//If more than 25% of the towers are hot, it's a bad crate.
//Otherwise, ignore ALL hot towers when averaging the crate channels
//together to see if the crate is malfunctioning.
//I've also set the cut to be 20% of the average, as opposed to 10%
//like individual channels.
//
    if(mDetectorFlavor == "bemc") {
      for(int crate=1; crate<31; crate++) {
        sumofhits = 0;
        nbinhits = 0;
        ncratehottowers=0;
        for(int channel=0; channel<160; channel++) {
          barry.GetTowerIdFromCrate(crate, channel, towerId);
          if(hHotTower->GetBinContent(towerId) > 10*averageNumberOfHitsPerChannel) {
            ncratehottowers++;
          } else if(hHotTower->GetBinContent(towerId) > 2) {
            sumofhits += hHotTower->GetBinContent(towerId);
            nbinhits++;
          }
        }
        if(nbinhits == 0 || (nbinhits != 0 && sumofhits/nbinhits < averageNumberOfHitsPerChannel/5) || ncratehottowers >= 40) {
          for(int channel=0; channel<160; channel++) {
            barry.GetTowerIdFromCrate(crate, channel, towerId);
            statusVector[towerId] |= 2+16;
          }
        }
      }
    } else { //EEMC
      for(int crate=0; crate<6; crate++) {
        sumofhits = 0;
        nbinhits = 0;
        ncratehottowers=0;
        for(int channel=0; channel<120; channel++) {
          towerId = eemcCrateMap[crate][channel];
          if(hHotTower->GetBinContent(towerId) > 10*averageNumberOfHitsPerChannel) {
            ncratehottowers++;
          } else if(hHotTower->GetBinContent(towerId) > 2) {
            sumofhits += hHotTower->GetBinContent(towerId);
            nbinhits++;
          }
        }
        if(nbinhits == 0 || (nbinhits != 0 && sumofhits/nbinhits < averageNumberOfHitsPerChannel/5) || ncratehottowers >= 30) {
          for(int channel=0; channel<120; channel++) {
            towerId = eemcCrateMap[crate][channel];
            statusVector[towerId] |= 2+16;
          }
        }
      }
    }
//rezeroing
    for(int i=1; i<mDetectorSize+1; i++) {
      if(statusVector[i] == 0) {
        statusVector[i]=1;
	if (i==50 || i==139) cout << "status=1   " << i << "   Status: " << statusVector[i] << endl;
        goodTowers++;
      } else if(statusVector[i] & mZerobit) {
        statusVector[i]=0;
	if (i==50 || i==139) cout << "statusVec & mZerobit   " << i << "   Status: " << statusVector[i] << endl;
      }
    }
  } else {
    for(int i=1; i<mDetectorSize+1; i++) {
      statusVector[i]=0;
      if (i==50 || i==139) cout << "nbinhits=0   " << i << "   Status: " << statusVector[i] << endl;
    }
  }
  delete gaus;
  
  return goodTowers;              
}

void
CSMStatusUtils::setDateTimeInfo(int runnumber,TTree* ttree) {
  
  Int_t thedate, thetime;
  TString thedatestring = "", thetimestring = "", tmpstr="";
  if(ttree) {
    ttree->SetBranchAddress("thedate",&thedate);
    ttree->SetBranchAddress("thetime",&thetime);
    ttree->GetEvent(0);
    mRunTimestampMap[runnumber] = thetime;
    mRunDatestampMap[runnumber] = thedate;
  } else {
    assert(ttree);
  }
}

TString
CSMStatusUtils::getDateTimeString(int runnumber,TTree* ttree) {
  
  if(ttree) setDateTimeInfo(runnumber,ttree);
  Int_t thedate, thetime;
  TString thedatestring = "", thetimestring = "", tmpstr="";
  if(mRunTimestampMap.count(runnumber)>0) { 
//    map<Int_t,Int_t>::const_iterator timeiter = mRunTimestampMap.find(runnumber);
//    map<Int_t,Int_t>::const_iterator dateiter = mRunDatestampMap.find(runnumber);
//    thedate = timeiter->second; 
//    thetime = dateiter->second;
    thedate = mRunDatestampMap[runnumber];
    thetime = mRunTimestampMap[runnumber];
  } else {
    thedate = 0;
    thetime = 0;
  }
  thedatestring += thedate;
  thetimestring += thetime;
//the next line makes me want to gag.  Seriously.
  for(int i=0; i<5-TMath::Floor(TMath::Log10(thetime)); i++) tmpstr += "0";
//that, my friends, is how to pad a TString without using sprintf
  
  TString datetimestring = "." + thedatestring + "." + tmpstr + thetimestring + ".";
  cout << datetimestring << endl;
  return datetimestring;
}

//this saves mRunStatusMap to an ASCII file

Int_t
CSMStatusUtils::saveStatusTablesToASCII(TString directory,int runnumber) {
  
  void* dir;
  if ((dir = gSystem->OpenDirectory(directory)) == NULL)
    gSystem->MakeDirectory(directory);

  IntToPtrVecShortConstIter first = mRunStatusMapPtr->begin();
  IntToPtrVecShortConstIter last = mRunStatusMapPtr->end();
  if(runnumber != 0) {
    first = mRunStatusMapPtr->find(runnumber);
    last = first;
    last++;
  }
  TString tmpstr, runnumberstring, datetimestring;
  for (IntToPtrVecShortConstIter iter = first; iter != last; ++iter) {
    int runnumber = iter->first;
    runnumberstring = "";
    runnumberstring += runnumber;
    datetimestring = getDateTimeString(runnumber);
    tmpstr = directory + "/run" + runnumberstring + "_" + mDetectorFlavor
        + datetimestring + "badTowers.txt";
    ofstream txtout(tmpstr.Data());
    for (Int_t i = 1; i <= mDetectorSize; i++) {
      txtout << i << "\t" << (*(iter->second))[i] << endl;
    }
    txtout.close();
  }
  return 0;
}

//this gets the number of towers whose status bits changed 
//between runs

Int_t
CSMStatusUtils::getNumberOfChangedTowers(Int_t runnumber) {

  IntToPtrVecShortConstIter iter = mRunStatusMapPtr->find(runnumber);
// first run - nothing to compare with
  if (iter == mRunStatusMapPtr->begin())
    return -1;
// run not found
  if (iter == mRunStatusMapPtr->end())
    return -1;

  // get previous run
  IntToPtrVecShortConstIter preIter = iter;
  preIter--;
  Int_t changedTowers = 0;
  vector<Short_t>* statusVector = iter->second;
  vector<Short_t>* oldStatusVector = preIter->second;
  for (UInt_t i = 1; i < statusVector->size(); i++)
    if ((((*statusVector)[i] != 1) && ((*oldStatusVector)[i] == 1)) ||
         (((*statusVector)[i] == 1) && ((*oldStatusVector)[i] != 1)))
      changedTowers++;

  return changedTowers;
}

//write pedestals to ROOT db files and to text files

void
CSMStatusUtils::writePedestals(Int_t runNumber, TString directory,
                                std::vector<Short_t>& statusVector,
                                std::vector<Float_t>& pedestalmean,
                                std::vector<Float_t>& pedestalwidth,
                                std::vector<Float_t>& pedestalchi) {

//write out pedestals to the text and root pedestalfiles
  TString pedtxtfilename = directory + "/pedestals/";

//create the pedestal directory, if it doesn't exist
  void* dir = NULL;
  if ((dir = gSystem->OpenDirectory(pedtxtfilename.Data())) == NULL)
    gSystem->MakeDirectory(pedtxtfilename.Data());

  TString runnumber = "";
  runnumber += runNumber;
  pedtxtfilename = directory + "/pedestals/" + mDetectorFlavor
      + "pedestals_for_run_" + runnumber + ".ped";
  ofstream pedestalfile(pedtxtfilename.Data());
  pedestalfile.setf(ios::left);
  pedestalfile << setw(8) << "ID" << 
      setw(8) << "PED" << 
      setw(8) << "RMS" << 
      setw(8) << "STATUS" << endl;

  St_emcPed *bemc_ped=new St_emcPed("bemcPed",1);
  emcPed_st t_ped;
  cout <<  t_ped.Status[367] << endl;
  TString datetimestring = getDateTimeString(runNumber);
  TString pedrootfilename = directory + "/pedestals/" + mDetectorFlavor
      + "Ped" + datetimestring + "root";
  TFile* fout_status = new TFile(pedrootfilename.Data(),"RECREATE");
 
  Short_t shortpedmean,shortpedwidth;
  for (UInt_t i = 1; i < statusVector.size(); i++) {
    shortpedmean = TMath::Nint(100*pedestalmean[i]);
    shortpedwidth = TMath::Nint(100*pedestalwidth[i]);
    t_ped.Status[i-1] = statusVector[i];  //indexed from 0
    t_ped.AdcPedestal[i-1] = shortpedmean;  //indexed from 0
    t_ped.AdcPedestalRMS[i-1] = shortpedwidth;  //indexed from 0
    t_ped.ChiSquare[i-1]=pedestalchi[i];  //indexed from 0
    pedestalfile << setw(8) << i << 
                    setw(8) << setprecision(4) << shortpedmean << 
                    setw(8) << setprecision(3) << shortpedwidth <<
                    setw(8) << setprecision(3) << statusVector[i] << endl;
  }
  pedestalfile.close();
//gzip the text pedestal files
  TString tmpstr = "rm -f " + pedtxtfilename + ".gz";
  gSystem->Exec(tmpstr.Data());
  tmpstr = "gzip " + pedtxtfilename;
  gSystem->Exec(tmpstr.Data());

  fout_status->cd();
  bemc_ped->AddAt(&t_ped,0);
  bemc_ped->Write();
  fout_status->Close();
  delete bemc_ped;
  delete fout_status;
}

//this finds the runs which are at the ends of fills, and writes
//that information into the mFillEndMap

void
CSMStatusUtils::findFillEnds() {

  Float_t runFillNumber, priorRunFillNumber=-1;
  int runnumber, priorRunNumber=0;
  for (map<Int_t,string>::const_iterator iter = mHistFileMap.begin();
        iter != mHistFileMap.end(); ++iter) {
    TFile* file = new TFile(iter->second.c_str(),"READ");
    if (file && file->IsOpen()) {
      runnumber = iter->first;
      assert(runnumber);
      TTree* runTree = dynamic_cast<TTree*>(file->Get("calinfo"));
      assert(runTree);
      runTree->SetBranchAddress("fillnum",&runFillNumber);
      runTree->GetEvent(0);
//if(runFillNumber==0) {
//  cout << iter->second.c_str() << endl;
//  cout<<"runfillnumb is " << runFillNumber << endl;
//}
//      assert(runFillNumber);
      if(priorRunNumber != 0) {
        if(runFillNumber != priorRunFillNumber)
          mFillEndMap[priorRunNumber] = kTRUE;
        else
          mFillEndMap[priorRunNumber] = kFALSE;
      }
      ++iter;
      if(iter == mHistFileMap.end())
        mFillEndMap[runnumber] = kTRUE;
      --iter;
        
      priorRunNumber = runnumber;
      priorRunFillNumber = runFillNumber;
    }
    file->Close();
    delete file;
  }
}
        

//this takes RunStatusMap, creates a brand new 2d histogram 
//of channel vs run, and fills each point with the status of
//the channel

TH2F* 
CSMStatusUtils::makeStatusVersusTimePlot() {
  gStyle->SetPalette(1,0);
  Int_t runs = mRunStatusMapPtr->size();
  TH2F* hist;
  if(mDetectorFlavor=="bemc")
    hist = new TH2F("bemcStatus_run","bemcStatus vs run",
                        4801,-0.5,4800.5,runs+1,-0.5,runs+0.5);
  else
    hist = new TH2F("eemcStatus_run","eemcStatus vs run",
                        721,-0.5,720.5,runs+1,-0.5,runs+0.5);
    
  hist->GetXaxis()->SetTitle("tower id");
  hist->GetYaxis()->SetTitle("relative run number");
  Int_t runNumber = 0;
  for (IntToPtrVecShortConstIter iter = mRunStatusMapPtr->begin();
        iter != mRunStatusMapPtr->end(); ++iter) {
    for (Int_t i=1; i<=mDetectorSize; i++) {
      if ((*(iter->second))[i] != 0) {
        hist->Fill(i,runNumber,(*(iter->second))[i]);
      }
    }
    runNumber++;
  }
  return hist;
}

void CSMStatusUtils::writeHtmlHeaderBadTowerList(ofstream& out,Int_t runnumber) {
  out << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << endl;
  out << "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 STRICT//EN\"" << endl;
  out << "               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" << endl;
  out << "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" >" << endl;
  out << "<head>" << endl;
  out << "<meta content=\"text/html; charset=UTF-8\" />" << endl;
  out << "<title> Bad BEMC Tower List Run " << runnumber << " </title>" << endl;
  out << "<link rel=\"StyleSheet\" href=\"../../myStyle.css\" type=\"text/css\" />" << endl;
  out << "</head>" << endl;
  out << "<body xml:lang=\"en\" lang=\"en\" >" << endl;
  out << "<h1> Bad BEMC Tower List Run " << runnumber << " </h1>" << endl;
  out << "<div class=\"header\">Status Codes</div>" << endl;
  out << "(codes are backward compatible with prior status tables)" << endl;
  out << "<ul>" << endl;
  out << "<li> 0 == channel does not exist </li>" << endl;
  out << "<li> 1 == channel is good </li>" << endl;
  out << "<li> 2 == channel is either hot or cold (see bit 16) </li>" << endl;
  out << "<li> 4 == channel has a weird pedestal (see bit 32)</li>" << endl;
  out << "<li> 8 == channel has a stuck bit (see bits 64 and 128) </li>" << endl;
  out << "<li> 16 == if off, hot tower (10x as many hits as others); if on, " <<
            "cold tower (40x fewer hits than others) </li>" << endl;
  out << "<li> 32 == if off, pedestal mean is out of bounds; if on, " <<
            "pedestal width is too large/small</li>" << endl;
  out << "<li> 64 == bit stuck on</li>" << endl;
  out << "<li> 128 == bit stuck off</li>" << endl;
  out << "</ul>" << endl;
  out << "<div class=\"header\">Bad Tower List</div>" << endl;
  out << "<p> Tower ADC plots are only available if the tower status has changed"
      << " compared to the previous run</p>" << endl;  
  out << "<table border=\"1\">" << endl;
  out << "<tbody>" << endl;
  out << "<tr> <th width=\"50\"> Tower ID </th> <th width=\"50\"> Status Code </th> <th width=\"100\"> ADC plot </th> </tr>" << endl;
}

void CSMStatusUtils::writeHtmlFooterBadTowerList(ofstream& out) {
  out << "</tbody>" << endl;
  out << "</table>" << endl;
  out << "<address> Thorsten Kollegger - last updated";
  out << " 7/30/2004";
  out << "</address>" << endl;
  out << "</body>" << endl;
  out << "</html>" << endl;
}

void CSMStatusUtils::writeHtmlHeaderSummary(ofstream& out) {
  out << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << endl;
  out << "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 STRICT//EN\"" << endl;
  out << "               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" << endl;
  out << "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" >" << endl;
  out << "<head>" << endl;
  out << "<meta content=\"text/html; charset=UTF-8\" />" << endl;
  out << "<title> BEMC Tower Status Analysis </title>" << endl;
  out << "<link rel=\"StyleSheet\" href=\"../../myStyle.css\" type=\"text/css\" />" << endl;
  out << "</head>" << endl;
  out << "<body xml:lang=\"en\" lang=\"en\" >" << endl;
  ifstream in("StRoot/StElectronInvMassAna/CSMStatusUtilsSummary.html");
  Char_t buffer[2048];
  while (in.is_open()) {
    in.getline(buffer,2048);
    if (in.eof()) {
      break;
    }
    out << buffer << endl;
  } 
}

void CSMStatusUtils::writeHtmlFooterSummary(ofstream& out) {
  out << "</tbody>" << endl;
  out << "</table>" << endl;
  out << "<address> David Staszak (taken from David Relyea, Thorsten Kollegger) - last updated";
  out << " 8/20/2006";
  out << "</address>" << endl;
  out << "</body>" << endl;
  out << "</html>" << endl;  
}

ClassImp(CSMStatusUtils)
