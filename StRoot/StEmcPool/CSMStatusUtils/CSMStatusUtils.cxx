#include "CSMStatusUtils.h"

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

#include <iostream>
#include <fstream>
#include <iomanip>
#include <map>
#include <set>
#include <string>

using namespace std;

typedef map<Int_t,vector<Short_t>*>::const_iterator IntToPtrVecShortConstIter ;

//sets the detector type
void
CSMStatusUtils::setDetectorFlavor(TString flavor) {
  mDetectorFlavor=flavor;
  if(mDetectorFlavor=="bemc") mDetectorSize=4800;
  else if(mDetectorFlavor=="eemc") mDetectorSize=720;
  if(mDetectorFlavor=="bemc") mRunStatusMapPtr=&mBEMCRunStatusMap;
  else                        mRunStatusMapPtr=&mEEMCRunStatusMap;
}

//this takes the files containing the 2D histograms for each run
//and puts their names into mHistFileMap

Int_t 
CSMStatusUtils::initializeHistFileFromDir(const Char_t* directory,const Char_t* filter) {
  
  if (!directory || !filter) return 0;
  void *dir = NULL;
  if ((dir = gSystem->OpenDirectory(directory)) != NULL) {
    const Char_t *dirEntry;
    while ((dirEntry = gSystem->GetDirEntry(dir)) != NULL) {
      Char_t buffer[2048];
      strcpy(buffer,directory);
      if (buffer[strlen(buffer)-1] != '/') strcat(buffer,"/");
      strcat(buffer,dirEntry);
      if (!strstr(buffer,filter)) continue;
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

Int_t
CSMStatusUtils::readTablesFromASCII(const Char_t* directory,const Char_t* filter) {
  if (!directory || !filter) return 0;
  void* dir = NULL;
  Char_t buffert[2048];
  strcpy(buffert,directory);
  strcat(buffert,"/status/");
  if ((dir = gSystem->OpenDirectory(buffert)) != NULL) {
    const Char_t *dirEntry;
    while ((dirEntry = gSystem->GetDirEntry(dir)) != NULL) {
      Char_t buffer[2048];
      strcpy(buffer,buffert);
      strcat(buffer,dirEntry);
      if (!strstr(buffer,filter)) continue;
      if (!strstr(buffer,mDetectorFlavor.Data())) continue;
      Char_t* needle = strstr(buffer,"run");
      Int_t runNumber = 0;
      if (needle) {
	      needle+=3;
	      Char_t runString[10];
	      strncpy(runString,needle,7);
	      runNumber = atoi(runString);
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
	        (*mRunStatusMapPtr)[runNumber] = vec;
        }
        in.close();
      } 
    }
  }  
  return mRunStatusMapPtr->size();
}

//this finds out which towers' statuses changed between runs
//and writes only the changes to the abbreviated status files

Int_t
CSMStatusUtils::saveAbbreviatedStatusTablesToASCII(const Char_t* directory) {

  TString tmpstr;
  int tmpint;
  IntToPtrVecShortConstIter first = mRunStatusMapPtr->begin();
  IntToPtrVecShortConstIter last = mRunStatusMapPtr->end();
  IntToPtrVecShortConstIter iter = first;
  IntToPtrVecShortConstIter preiter = first;
  iter++;

  for(;iter!=last;iter++) {
    tmpint = iter->first;
    tmpstr = directory;
    tmpstr += "/status/short_status_";
    tmpstr += mDetectorFlavor;
    tmpstr += "_run";
    tmpstr += tmpint; //runnumber
    tmpstr += ".status";
    ofstream ofs(tmpstr.Data());
    vector<Short_t>* statusVector = iter->second;
    vector<Short_t>* oldStatusVector = preiter->second;
    Short_t oldstatus, status;
    for (UInt_t i = 1; i < statusVector->size(); i++) {
      oldstatus = (*oldStatusVector)[i];
      status = (*statusVector)[i];
      if (oldstatus != status) ofs << i << "\t" << status << endl;
    }
    ofs.close();
    preiter = iter;
  }
}

//this finds out which towers' pedestals changed between runs
//and writes only the changes to the abbreviated pedestal files

/*Int_t
CSMStatusUtils::saveAbbreviatedPedestalTables(const Char_t* directory, const Char_t* filter) {) {

  std::map<Int_t,std::vector<Float_t>*> pedMeanMap;
  std::map<Int_t,std::vector<Float_t>*> pedErrorMap;
  std::map<Int_t,std::vector<Int_t>*> pedStatusMap;
  if (!directory || !filter) return 0;
  void* dir = NULL;
  Char_t buffert[2048];
  strcpy(buffert,directory);
  strcat(buffert,"/pedestals/");
  if ((dir = gSystem->OpenDirectory(buffert)) != NULL) {
    const Char_t *dirEntry;
    while ((dirEntry = gSystem->GetDirEntry(dir)) != NULL) {
      Char_t buffer[2048];
      strcpy(buffer,buffert);
      strcat(buffer,dirEntry);
      if (!strstr(buffer,filter)) continue;
      if (!strstr(buffer,mDetectorFlavor.Data())) continue;
      Char_t* needle = strstr(buffer,"run_");
      Int_t runNumber = 0;
      if (needle) {
	      needle+=3;
	      Char_t runString[10];
	      strncpy(runString,needle,7);
	      runNumber = atoi(runString);
      }
      if (runNumber != 0) {
	      ifstream in(buffer);
        if(in.good()) {
          in.getline(buffert);  //reposition past first line
	        Float_t pedmean, pederr;
          Int_t itemp, istatus;
	        vector<Float_t>* means = new vector<Float_t>(mDetectorSize+1);
	        vector<Float_t>* errs = new vector<Float_t>(mDetectorSize+1);
	        vector<Int_t>* statuses = new vector<Int_t>(mDetectorSize+1);
	        for(int id=1; id<mDetectorSize+1; id++) {
//        cout << buffer << "mrz" << endl;
            in >> itemp >> pedmean >> pederr >> istatus;
//        cout << status << "mrz" << itemp << endl;
 	          (*means)[id] = pedmean;
 	          (*errs)[id] = pederr;
 	          (*statuses)[id] = istatus;
	        }
	        pedMeanMap[runNumber] = means;
	        pedErrorMap[runNumber] = errors;
	        pedStatusMap[runNumber] = statuses;
        }
        in.close();
      } 
    }
  }  
  TString tmpstr;
  int tmpint;
  IntToPtrVecShortConstIter first = pedStatusMap.begin();
  IntToPtrVecShortConstIter last = pedStatusMap.end();
  IntToPtrVecShortConstIter iter = first;
  IntToPtrVecShortConstIter preiter = first;
  iter++;

  for(;iter!=last;iter++) {
    tmpint = iter->first;
    tmpstr = directory;
    tmpstr += "/status/short_status_run";
    tmpstr += tmpint; //runnumber
    tmpstr += ".status";
    ofstream ofs(tmpstr.Data());
    vector<Short_t>* statusVector = iter->second;
    vector<Short_t>* oldStatusVector = preiter->second;
    Short_t oldstatus, status;
    for (UInt_t i = 1; i < statusVector->size(); i++) {
      oldstatus = (*oldStatusVector)[i];
      status = (*statusVector)[i];
      if (oldstatus != status) ofs << i << "\t" << status << endl;
    }
    ofs.close();
    preiter = iter;
  }
}*/

//this takes HistFileMap, opens each file in it, creates the
//hot tower histogram, calls analyseStatusHistogram, draws the
//histograms, and outputs to an html file the abbreviated results,
//such as the number of good towers in a run.
//It then looks to see if the status of a particular channel has
//changed from run to run, and if it has, it saves that channel's
//histograms.
//It can also run to Taco Bell if you are hungry.
//It is *that* versatile.

Int_t
CSMStatusUtils::makeStatusPlots(const Char_t* plotDir) {

  gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);
  TString tmpstr;
  tmpstr = mDetectorFlavor;
  tmpstr += "StatusPlots";
  TCanvas *c1 = new TCanvas(tmpstr.Data(),tmpstr.Data());
  c1->SetLogy();
  c1->Draw();
  TCanvas *c2 = new TCanvas("towerAdcPlots","towerAdcPlots",400,400);
  c2->SetLogy();
  c2->Draw();

  tmpstr = plotDir;
  tmpstr += "/";
  tmpstr += mDetectorFlavor;
  tmpstr += "Status.html";
  ofstream htmlSummary(tmpstr.Data());
  writeHtmlHeaderSummary(htmlSummary);

  for (map<Int_t,string>::const_iterator iter = mHistFileMap.begin();
        iter != mHistFileMap.end(); ++iter) {
    TFile* file = new TFile(iter->second.c_str(),"READ");
    if (file && file->IsOpen()) {
      tmpstr = mDetectorFlavor;
      tmpstr += "StatusAdc_";
      int tmpint = iter->first;
      tmpstr += tmpint;
      TH2F* myHist = dynamic_cast<TH2F*>(file->Get(tmpstr.Data()));
      if (myHist) {
	      vector<Short_t>* statusVector = new vector<Short_t>(mDetectorSize+1);
	      TH1F* hHotTower = new TH1F("hotTower","# of tower hits",mDetectorSize+1,-0.5,mDetectorSize+1-0.5);
	      hHotTower->GetXaxis()->SetTitle("Tower Id");
	      hHotTower->GetYaxis()->SetTitle("Number of Hits Above Pedestal");
        
	      // analyze
	      Int_t goodTowers =
            analyseStatusHistogram(myHist,*statusVector,plotDir,hHotTower);

	      if (goodTowers == 0) {
 	        htmlSummary << "<tr> <td>" << iter->first << "</td>" 
	                    << "<td> - </td> <td> - </td> <td> - </td>"
		            << "<td> - </td> <td> - </td> <td> - </td> </tr>"
		            << endl;
	        continue;
	      }

	      gStyle->SetOptStat(1111);
	      c1->cd();
	      hHotTower->Draw();
	      c1->Update();
        tmpstr = plotDir;
        tmpstr += "/run";
        tmpstr += tmpint;
        tmpstr += "_";
        tmpstr += mDetectorFlavor;
        tmpstr += "_hotTowers.gif";
	      c1->SaveAs(tmpstr.Data());
	      delete hHotTower;

        (*mRunStatusMapPtr)[iter->first] = statusVector;
        
	      htmlSummary << "<tr>" << endl 
	                  << "<td> " << iter->first << " </td> " << endl 
	                  << "<td> " << goodTowers << " </td>" << endl
	                  << "<td> " << getNumberOfChangedTowers(tmpint) //run #
	                  << " </td>" << endl;

        tmpstr = "./run";
        tmpstr += tmpint;
        tmpstr += "_";
        tmpstr += mDetectorFlavor;
        tmpstr += "_badTowers.html";
	      htmlSummary << "<td> <a href=\"" << tmpstr.Data() << "\"> list </a> </td>" 
	                  << endl;

// save bad tower info
        
//first, save it to a regular runfile
        tmpstr = plotDir;
        tmpstr += "/status/";
        int sSTA = saveStatusTablesToASCII(tmpstr.Data(), iter->first);
                
//now save it to a nasty htmlfile

        tmpstr = plotDir;
        tmpstr += "/run";
        tmpstr += tmpint;
        tmpstr += "_";
        tmpstr += mDetectorFlavor;
        tmpstr += "_badTowers.html";
	      ofstream htmlout(tmpstr.Data());
	      writeHtmlHeaderBadTowerList(htmlout,iter->first);
        
        int tmpint2=mDetectorSize;
        if(mDetectorFlavor=="bemc") tmpint2 /= 2;  //last half of channels bad
	      for (Int_t i=1; i<tmpint2+1; i++) {
	        if ((*statusVector)[i] > 0) {
	          htmlout << "<tr> <td> " << i << " </td> <td> "
                    << (*statusVector)[i] << " </td> <td> " << endl;

// check if first run - if yes plot every bad tower
// if a previous run exists just plot towers which changed status
	          IntToPtrVecShortConstIter statusIter;
	          statusIter = mRunStatusMapPtr->find(iter->first);
	          if (statusIter != mRunStatusMapPtr->begin()) {
	            IntToPtrVecShortConstIter preIter = statusIter;
	            preIter--;
	            if ((*(statusIter->second))[i] == (*(preIter->second))[i]) {
		            htmlout << "- </td> </tr>" << endl;
		            continue;
	            }
	          }
	          Int_t bin = myHist->GetXaxis()->FindFixBin(i);
	          TH1D *hTemp = myHist->ProjectionY("projTemp",bin,bin);
	          c2->cd();
	          c2->Clear();
	          hTemp->GetXaxis()->SetTitle("adc");
//FIX	          hTemp->Draw();
	          c2->Update();
//	          sprintf(buffer,"%s/runplots/run%dtower%d_adc.gif",plotDir,iter->first,i);
//FIX	          c2->SaveAs(buffer);
//	          sprintf(buffer,"./runplots/run%dtower%d_adc.gif",iter->first,i);
	          htmlout << "<a href=\"" << "this space for rent" << "\" > plot </a>" 
	              << "</td> </tr>" << endl;
	          //c2->SaveAs(buffer);
	          delete hTemp;
	        }
	      }
	      writeHtmlFooterSummary(htmlout);
	      htmlSummary << "</tr>" << endl;
        htmlout.close();
      }
//        cout << "I got here and there is no problem yet" << endl;
//        exit(1);
    }
    file->Close();
    delete file;
  }
  htmlSummary.close();
  writeHtmlFooterSummary(htmlSummary);
  TH2F* statusHist = makeStatusVersusTimePlot();
  if (statusHist) {
    c1->cd();
    c1->SetLogy(kFALSE);
    c1->Clear();
    statusHist->GetXaxis()->SetRangeUser(0.5,2400.5);
    statusHist->Draw("colz");
//    sprintf(buffer,"%s/bemcStatusPlot.gif",plotDir);
//    c1->SaveAs(buffer);
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
                                          vector<Short_t>& statusVector,
                                          const Char_t* directory,
  		                                    TH1F* hHotTower,
                                          TH1F* hPedMean,
			                                    TH1F* hPedWidth) {
//create the pedestal file
  TString pfilename = directory;
  pfilename += "/pedestals/";
  pfilename += mDetectorFlavor;
  pfilename += "pedestals_for_run_";
  TString runnumber = hist->GetName();
  runnumber = runnumber(runnumber.Length()-7,7);
  pfilename += runnumber;
  pfilename += ".ped";
  ofstream pedestalfile(pfilename.Data());
  pedestalfile.setf(ios::left);
  pedestalfile << setw(8) << "ID" << 
      setw(8) << "PED" << 
      setw(8) << "RMS" << 
      setw(8) << "STATUS" << endl;

  TF1* gaus = new TF1("gaus","gaus");
  TF1* lin = new TF1("lin","[0]+x*[1]");

// initialize status vector
  for (vector<Short_t>::iterator iter = statusVector.begin(); iter != statusVector.end(); ++iter)
    *iter = 0;

//loop through all the channels
  for (Int_t i = 2; i <= hist->GetXaxis()->GetNbins(); i++) {
    Int_t chanId = static_cast<Int_t>(hist->GetXaxis()->GetBinCenter(i));
    if (chanId < 0.5 || chanId > mDetectorSize+0.5) {
      //cout << "bin " << i << " has center " << hist->GetXaxis()->GetBinCenter(i) << " which is not valid" << endl;
      exit(1);
    }

    TH1D* proj = hist->ProjectionY("projTemp",i,i);
 
    if (proj) {

//total number of hits test
      Float_t entries = proj->Integral(1,proj->GetXaxis()->GetNbins());
      if (entries <= 10) statusVector[chanId] += 2;

// find maximum, which should be the pedestal peak
      Int_t maxBin = 0;
      Float_t maxValue = -1;
      for (Int_t j = 1; j < proj->GetXaxis()->GetNbins(); j++) {
	      if (proj->GetBinContent(j) > maxValue) {
	        maxBin = j;
	        maxValue = proj->GetBinContent(j);
	      }
      }
      Float_t pedMean = proj->GetXaxis()->GetBinCenter(maxBin);

//pedestal mean test
      if (mDetectorFlavor=="bemc" && (pedMean < 10 || pedMean > 60) ||
          mDetectorFlavor=="eemc" && (pedMean < 10 || pedMean > 60)) 
        statusVector[chanId] += 4;

      // fit a gaussian to the pedestal peak
      gaus->SetParameter(0,maxValue);
      gaus->SetParameter(1,pedMean);
      gaus->SetParameter(2,3.5);
      gaus->SetRange(pedMean-10,pedMean+10);
      proj->Fit(gaus,"0RQ");

//HERE IS WHERE I CODE UP THE DB STATUS TEST

      int STATUS=1;
      if(chanId>2400) STATUS=0;
      if(hPedMean) hPedMean->Fill(gaus->GetParameter(1));
      if(hPedWidth) hPedWidth->Fill(gaus->GetParameter(2));

//write out pedestal to the pedestalfile      
      pedestalfile << setw(8) << chanId << 
                      setw(8) << setprecision(4) << gaus->GetParameter(1) << 
                      setw(8) << setprecision(3) << gaus->GetParameter(2) <<
                      setw(8) << setprecision(2) << STATUS << endl;
          
//pedestal width test
//SHOULD THIS BE DIFFERENT FOR THE EEMC???
      if (gaus->GetParameter(2) <= 0.5 || gaus->GetParameter(2) > 2.8)
      	statusVector[chanId] += 8;

      
//hot tower/cold tower test
//using a threshold of 6 sigma above pedestal(??)
      Int_t minBin = proj->GetXaxis()->FindFixBin(gaus->GetParameter(1) +
                                                  6*gaus->GetParameter(2));
      maxBin = proj->GetXaxis()->GetNbins() - 1;
      Int_t hottowerthreshold = minBin;
      if(hHotTower) {
        Float_t nHitsAbovePedestal = proj->Integral(hottowerthreshold,maxBin);
        if(nHitsAbovePedestal==0) {
          nHitsAbovePedestal=1; //(just for log plot sakes)
//          statusVector[chanId] += 16;//replaced below
        }
        hHotTower->AddAt(nHitsAbovePedestal,chanId);
      }

//stuck bit test (off & on!)
      Short_t bitoff = 0;
      Short_t biton = (1+2+4+8+16+32);
      for(int i=1; i<maxBin; i++) {
        if(proj->GetBinContent(i) > 0) {
          bitoff = bitoff | i;
          biton = biton & i;
        }
      }
      
      Short_t bitcompare = (1+2+4+8+16);
      bitcompare = 1;
      if((bitoff & bitcompare) != bitcompare)
         statusVector[chanId] += 128;

      bitcompare = 2;
      if((bitoff & bitcompare) != bitcompare)
         statusVector[chanId] += 256;

      bitcompare = 4;
      if((bitoff & bitcompare) != bitcompare)
         statusVector[chanId] += 512;

      bitcompare = 8;
      if((bitoff & bitcompare) != bitcompare)
         statusVector[chanId] += 1024;

      bitcompare = 16;
      if((bitoff & bitcompare) != bitcompare)
         statusVector[chanId] += 2048;

      if(biton != 0)
         statusVector[chanId] += 64;

      delete proj;
    } else {
//lack of histogram test
      statusVector[chanId] += 1;
    }
  }
  pedestalfile.close();
//gzip the pedestal files
  TString tmpstr = "gzip ";
  tmpstr += pfilename;
  gSystem->Exec(tmpstr.Data());
  
//finish the hottower test
  Float_t sumofhits=0, averagehit=0, nbinhits=0;
  for(int i=1; i<mDetectorSize+1; i++) {
    if(hHotTower->GetBinContent(i) > 2) {
      sumofhits += hHotTower->GetBinContent(i);
      nbinhits++;
    }
  }
  
  Int_t goodTowers = 0;
  
  if(nbinhits!=0) {
    averagehit = sumofhits/nbinhits;
    for(int i=1; i<mDetectorSize; i++) {
      if(hHotTower->GetBinContent(i) > 10*averagehit) statusVector[i] += 16;
      if(hHotTower->GetBinContent(i) < averagehit/40) statusVector[i] += 32;
      if (statusVector[i] == 0) goodTowers++;
    }
  }
  delete gaus;
  delete lin;
  return goodTowers;						  
}

//this saves mRunStatusMap to an ASCII file

Int_t
CSMStatusUtils::saveStatusTablesToASCII(const Char_t* directory,int index) {
  
  IntToPtrVecShortConstIter first = mRunStatusMapPtr->begin();
  IntToPtrVecShortConstIter last = mRunStatusMapPtr->end();
  if(index != 0) {
    first = mRunStatusMapPtr->find(index);
    last = first;
    last++;
  }    
  for (IntToPtrVecShortConstIter iter = first; iter != last; ++iter) {
    TString tmpstr = directory;
    tmpstr += "/run";
    int runnumber = iter->first;
    tmpstr += runnumber;
    tmpstr += "_";
    tmpstr += mDetectorFlavor;
    tmpstr += "_badTowers.txt";
	  ofstream txtout(tmpstr.Data());
    int tmpint2=mDetectorSize;
    if(mDetectorFlavor=="bemc") tmpint2 /= 2;  //last half of channels bad
    for (Int_t i = 1; i <= tmpint2; i++) {
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
  if (iter == mRunStatusMapPtr->begin()) {
    cout << "you killed my babyt " << runnumber << endl;
    return -1;
  }
// run not found
  if (iter == mRunStatusMapPtr->end()) {
    cout << "you killed my recturm " << runnumber << endl;
    return -1;
  }

  // get previous run
  IntToPtrVecShortConstIter preIter = iter;
  preIter--;
  Int_t changedTowers = 0;
  vector<Short_t>* statusVector = iter->second;
  vector<Short_t>* oldStatusVector = preIter->second;
  for (UInt_t i = 1; i < statusVector->size(); i++)
    if ((((*statusVector)[i] > 0) && ((*oldStatusVector)[i] == 0)) ||
         (((*statusVector)[i] == 0) && ((*oldStatusVector)[i] > 0)))
      changedTowers++;

  return changedTowers;
}

//this takes RunStatusMap, creates a brand new 2d histogram 
//of channel vs run, and fills each point with the status of
//the channel

TH2F* 
CSMStatusUtils::makeStatusVersusTimePlot() {
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
  out << "<ul>" << endl;
  out << "<li> 1 == ZERO statistics </li>" << endl;
  out << "<li> 2 == very few (<10 hits) statistics </li>" << endl;
  out << "<li> 4 == pedestal mean out of expected range </li>" << endl;
  out << "<li> 8 == pedestal width out of expected range </li>" << endl;
  out << "<li> 16 == hot tower (10x as many hits as others) </li>" << endl;
  out << "<li> 32 == cold tower (10x fewer hits than others) </li>" << endl;
  out << "<li> 64 == bit stuck off</li>" << endl;
  out << "<li> 128 == bit stuck on</li>" << endl;
  out << "</ul>" << endl;
  out << "<div class=\"header\">Bad Tower List</div>" << endl;
  out << "<p> Tower ADC plots are only available if the tower status has changed"
      << " compared to the previous run (and even then they're not, since"
      << " I want to keep working on this until it makes sense. </p>" << endl;  
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
  out << "<address> David Relyea (taken from Thorsten Kollegger) - last updated";
  out << " 10/31/2004";
  out << "</address>" << endl;
  out << "</body>" << endl;
  out << "</html>" << endl;  
}

ClassImp(CSMStatusUtils)
