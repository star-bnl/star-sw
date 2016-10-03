#include "compareRootFiles.hh"

#include "TROOT.h"
#include "TStyle.h"
#include "TLatex.h"
#include "TLegend.h"
#include "TText.h"
#include "TAxis.h"

#include <iostream>
#include <cstdlib>

using std::cout;
using std::endl;

compareRootFiles::compareRootFiles(string decayName, string newFileName, string oldFileName,
				   string description) {

  _decayName = decayName;
  _newFileName = newFileName;
  _oldFileName = oldFileName;
  _description = description;

  _newFile = new TFile(newFileName.c_str(), "read");
  _oldFile = new TFile(oldFileName.c_str(), "read");

  gROOT->SetStyle("Plain");
  gStyle->SetOptStat(0);

  _theCanvas = new TCanvas("theCanvas", "", 900, 700);
  _theCanvas->Clear();
  _theCanvas->UseCurrentStyle();

  _emptyHist = new TH1D("", "", 10, 0.0, 0.0);

  _nGroupMax = 12;

}

compareRootFiles::~compareRootFiles() {

  _newFile->Close();
  _oldFile->Close();

}

void compareRootFiles::getMtmPlots() {

  // Plot mtm plots for different particle groups
  // Leptons+gamma, pions, kaons, charm, light/strange baryons, other

  vector<TH1D*> newHistVect, oldHistVect;

  vector<int> leptonInts;
  leptonInts.push_back(0); leptonInts.push_back(1);
  TH1D* newLeptMtmHist = getMtmHist(_newFile, "newLeptMtmHist", leptonInts);
  TH1D* oldLeptMtmHist = getMtmHist(_oldFile, "oldLeptMtmHist", leptonInts);
  newHistVect.push_back(newLeptMtmHist);
  oldHistVect.push_back(oldLeptMtmHist);

  vector<int> pionInts;
  pionInts.push_back(2); pionInts.push_back(3);
  TH1D* newPionMtmHist = getMtmHist(_newFile, "newPionMtmHist", pionInts);
  TH1D* oldPionMtmHist = getMtmHist(_oldFile, "oldPionMtmHist", pionInts);
  newHistVect.push_back(newPionMtmHist);
  oldHistVect.push_back(oldPionMtmHist);

  vector<int> kaonInts;
  kaonInts.push_back(4); kaonInts.push_back(5);
  TH1D* newKaonMtmHist = getMtmHist(_newFile, "newKaonMtmHist", kaonInts);
  TH1D* oldKaonMtmHist = getMtmHist(_oldFile, "oldKaonMtmHist", kaonInts);
  newHistVect.push_back(newKaonMtmHist);
  oldHistVect.push_back(oldKaonMtmHist);

  vector<int> charmInts;
  charmInts.push_back(6); charmInts.push_back(7);
  TH1D* newCharmMtmHist = getMtmHist(_newFile, "newCharmMtmHist", charmInts);
  TH1D* oldCharmMtmHist = getMtmHist(_oldFile, "oldCharmMtmHist", charmInts);
  newHistVect.push_back(newCharmMtmHist);
  oldHistVect.push_back(oldCharmMtmHist);

  vector<int> baryonInts;
  baryonInts.push_back(8); baryonInts.push_back(9);
  TH1D* newBaryonMtmHist = getMtmHist(_newFile, "newBaryonMtmHist", baryonInts); 
  TH1D* oldBaryonMtmHist = getMtmHist(_oldFile, "oldBaryonMtmHist", baryonInts);
  newHistVect.push_back(newBaryonMtmHist);
  oldHistVect.push_back(oldBaryonMtmHist);

  vector<int> otherInts;
  otherInts.push_back(10);
  TH1D* newOtherMtmHist = getMtmHist(_newFile, "newOtherMtmHist", otherInts);
  TH1D* oldOtherMtmHist = getMtmHist(_oldFile, "oldOtherMtmHist", otherInts);
  newHistVect.push_back(newOtherMtmHist);
  oldHistVect.push_back(oldOtherMtmHist);

  _theCanvas->Clear();
  _theCanvas->UseCurrentStyle();
  _theCanvas->cd(1);
  gPad->SetLogy();

  // Different colours for the particle groups
  int colours[6] = {1, 2, 4, 6, 8, 28};
  string histLabels[6] = {"Leptons,#gamma", "Pions", "Kaons", "D+,D-,D0", 
			  "Light/strange baryons", "Other"};

  TLegend* theLegend = new TLegend(0.7, 0.7, 0.9, 0.9);
  theLegend->SetFillColor(kWhite);

  double newHistMax(0.0), oldHistMax(0.0);
  int newHistMaxInt(0), oldHistMaxInt(0);

  double totalNewFreq(0.0), totalOldFreq(0.0);

  int iHist(0);
  int nHistVect = newHistVect.size();
  for (iHist = 0; iHist < nHistVect; iHist++) {

    TH1D* newHist = newHistVect[iHist];
    string labelName = histLabels[iHist];
    theLegend->AddEntry(newHist, labelName.c_str(), "lpf");

    int colour = colours[iHist];
    newHist->SetLineColor(colour);

    double theNewHistMax = newHist->GetMaximum();
    if (theNewHistMax > newHistMax) {
      newHistMax = theNewHistMax;
      newHistMaxInt = iHist;
    }

    totalNewFreq += newHist->Integral();

    TH1D* oldHist = oldHistVect[iHist];
    oldHist->SetLineStyle(kDashed);
    oldHist->SetLineColor(colour);

    double theOldHistMax = oldHist->GetMaximum();
    if (theOldHistMax > oldHistMax) {
      oldHistMax = theOldHistMax;
      oldHistMaxInt = iHist;
    }    

    totalOldFreq += oldHist->Integral();

  }

  double newScale = 1.0/totalNewFreq;
  double oldScale = 1.0/totalOldFreq;

  if (newHistMax > oldHistMax) {

    TH1D* newFirstHist = newHistVect[newHistMaxInt];

    TAxis* xAxis = newFirstHist->GetXaxis();
    double dx = xAxis->GetBinWidth(1)*1000.0;
    char dxChar[100];
    sprintf(dxChar, "Normalised frequency/%.0f (MeV/c)", dx);

    newFirstHist->SetXTitle("p (GeV/c)");
    newFirstHist->SetYTitle(dxChar);
    newFirstHist->SetTitleOffset(1.25, "Y");
    newFirstHist->Scale(newScale);
    newFirstHist->Draw();

    for (iHist = 0; iHist < nHistVect; iHist++) {

      if (iHist != newHistMaxInt) {
	TH1D* otherNewHist = newHistVect[iHist];
	otherNewHist->Scale(newScale);
	otherNewHist->Draw("same");

      }

      TH1D *otherOldHist = oldHistVect[iHist];
      otherOldHist->Scale(oldScale);
      otherOldHist->Draw("same");

    }

  } else {

    TH1D* oldFirstHist = oldHistVect[oldHistMaxInt];
 
    TAxis* xAxis = oldFirstHist->GetXaxis();
    double dx = xAxis->GetBinWidth(1)*1000.0;
    char dxChar[100];
    sprintf(dxChar, "Normalised frequency/%.0f (MeV/c)", dx);

    oldFirstHist->SetXTitle("p (GeV/c)");
    oldFirstHist->SetYTitle(dxChar);
    oldFirstHist->SetTitleOffset(1.25, "Y");
    oldFirstHist->Scale(oldScale);
    oldFirstHist->Draw();

    for (iHist = 0; iHist < nHistVect; iHist++) {

      if (iHist != oldHistMaxInt) {
	TH1D* otherOldHist = oldHistVect[iHist];
	otherOldHist->Scale(oldScale);
	otherOldHist->Draw("same");
      }

      TH1D* otherNewHist = newHistVect[iHist];
      otherNewHist->Scale(newScale);
      otherNewHist->Draw("same");

    }

  }

  theLegend->Draw();

  TText* text = new TText();
  text->SetTextSize(0.03);
  text->DrawTextNDC(0.1, 0.95, "New version = solid lines");
  text->DrawTextNDC(0.1, 0.915, "Old version = dotted lines");
  TLatex* latexString = new TLatex();
  latexString->SetTextSize(0.03);
  latexString->SetNDC();
  latexString->DrawLatex(0.5, 0.92, _description.c_str());

  string gifFileName("gifFiles/"); 
  gifFileName.append(_decayName); gifFileName.append("_mtmHist.gif");
  _theCanvas->Print(gifFileName.c_str());

  gPad->SetLogy(0);

}

void compareRootFiles::getPartIdPlots() {

  TH1D* newPartIdHist = getPartIdHist(_newFile, "newPartIdHist");
  TH1D* oldPartIdHist = getPartIdHist(_oldFile, "oldPartIdHist");

  _theCanvas->Clear();
  _theCanvas->UseCurrentStyle();
  _theCanvas->cd(1);
  gPad->SetLogy();

  double newPartIdMax = newPartIdHist->GetMaximum();
  double oldPartIdMax = oldPartIdHist->GetMaximum();

  oldPartIdHist->SetLineStyle(kDashed);

  if (newPartIdMax > oldPartIdMax) {

    newPartIdHist->SetXTitle("");
    newPartIdHist->SetYTitle("Normalised frequency");
    newPartIdHist->SetTitleOffset(1.25, "Y");

    newPartIdHist->Draw();
    oldPartIdHist->Draw("same");

  } else {

    oldPartIdHist->SetXTitle("");
    oldPartIdHist->SetYTitle("Normalised frequency");
    oldPartIdHist->SetTitleOffset(1.25, "Y");

    oldPartIdHist->Draw();
    newPartIdHist->Draw("same");

  }

  TLatex* latex = new TLatex();
  latex->SetTextAngle(90.0);

  latex->SetTextSize(0.035);
  latex->DrawLatex(0.5, 0, "Leptons");

  latex->SetTextSize(0.045);
  latex->DrawLatex(1.5, 0, "#gamma");
  latex->DrawLatex(2.5, 0, "#pi^{#pm}");
  latex->DrawLatex(3.5, 0, "#pi^{0}");
  latex->DrawLatex(4.5, 0, "K^{#pm}");
  latex->DrawLatex(5.5, 0, "K^{0}");
  latex->DrawLatex(6.5, 0, "D^{#pm}");
  latex->DrawLatex(7.5, 0, "D^{0}");

  latex->SetTextSize(0.035);
  latex->DrawLatex(8.4, 0, "Light"); latex->DrawLatex(8.75, 0, "baryons");
  latex->DrawLatex(9.4, 0, "Strange"); latex->DrawLatex(9.75, 0, "baryons");

  latex->DrawLatex(10.5, 0, "Other");

  TText* text = new TText();
  text->SetTextSize(0.03);
  text->DrawTextNDC(0.1, 0.95, "New version = solid lines");
  text->DrawTextNDC(0.1, 0.915, "Old version = dotted lines");
  TLatex* latexString = new TLatex();
  latexString->SetTextSize(0.03);
  latexString->SetNDC();
  latexString->DrawLatex(0.5, 0.92, _description.c_str());

  string gifFileName("gifFiles/"); 
  gifFileName.append(_decayName); gifFileName.append("_partIdHist.gif");
  _theCanvas->Print(gifFileName.c_str());

  gPad->SetLogy(0);

}

void compareRootFiles::getAllIdPlots() {

  TH1D* newAllIdHist = getAllIdHist(_newFile, "newAllIdHist");
  TH1D* oldAllIdHist = getAllIdHist(_oldFile, "oldAllIdHist");

  _theCanvas->Clear();
  _theCanvas->UseCurrentStyle();
  _theCanvas->cd(1);
  gPad->SetLogy();

  double newAllIdMax = newAllIdHist->GetMaximum();
  double oldAllIdMax = oldAllIdHist->GetMaximum();

  oldAllIdHist->SetLineStyle(kDashed);

  if (newAllIdMax > oldAllIdMax) {

    newAllIdHist->SetXTitle("PDG Id");
    newAllIdHist->SetYTitle("Normalised frequency");
    newAllIdHist->SetTitleOffset(1.25, "Y");

    newAllIdHist->Draw();
    oldAllIdHist->Draw("same");

  } else {

    oldAllIdHist->SetXTitle("PDG Id");
    oldAllIdHist->SetYTitle("Normalised frequency");
    oldAllIdHist->SetTitleOffset(1.25, "Y");

    oldAllIdHist->Draw();
    newAllIdHist->Draw("same");

  }

  TText* text = new TText();
  text->SetTextSize(0.03);
  text->DrawTextNDC(0.1, 0.95, "New version = solid lines");
  text->DrawTextNDC(0.1, 0.915, "Old version = dotted lines");
  TLatex* latexString = new TLatex();
  latexString->SetTextSize(0.03);
  latexString->SetNDC();
  latexString->DrawLatex(0.5, 0.92, _description.c_str());

  string gifFileName("gifFiles/"); 
  gifFileName.append(_decayName); gifFileName.append("_allIdHist.gif");
  _theCanvas->Print(gifFileName.c_str());

  gPad->SetLogy(0);

}

void compareRootFiles::getNDaugPlots() {

  TH1D* newDaugHist = getDaugHist(_newFile, "newDaugHist");
  TH1D* oldDaugHist = getDaugHist(_oldFile, "oldDaugHist");

  _theCanvas->Clear();
  _theCanvas->UseCurrentStyle();
  _theCanvas->cd(1);

  double newDaugMax = newDaugHist->GetMaximum();
  double oldDaugMax = oldDaugHist->GetMaximum();

  oldDaugHist->SetLineStyle(kDashed);

  if (newDaugMax > oldDaugMax) {

    newDaugHist->SetXTitle("nDaughters");
    newDaugHist->SetYTitle("Normalised frequency");
    newDaugHist->SetTitleOffset(1.25, "Y");

    newDaugHist->Draw();
    oldDaugHist->Draw("same");

  } else {

    oldDaugHist->SetXTitle("nDaughters");
    oldDaugHist->SetYTitle("Normalised frequency");
    oldDaugHist->SetTitleOffset(1.25, "Y");

    oldDaugHist->Draw();
    newDaugHist->Draw("same");

  }

  TText* text = new TText();
  text->SetTextSize(0.03);
  text->DrawTextNDC(0.1, 0.95, "New version = solid lines");
  text->DrawTextNDC(0.1, 0.915, "Old version = dotted lines");
  TLatex* latexString = new TLatex();
  latexString->SetTextSize(0.03);
  latexString->SetNDC();
  latexString->DrawLatex(0.5, 0.92, _description.c_str());

  string gifFileName("gifFiles/"); 
  gifFileName.append(_decayName); gifFileName.append("_daugHist.gif");
  _theCanvas->Print(gifFileName.c_str());

}

TH1D* compareRootFiles::getMtmHist(TFile* theFile, string histName, vector<int> groupInts) {

  if (theFile == 0) {
    // Return empty histogram
    return _emptyHist;
  }

  TTree* theTree = dynamic_cast<TTree*>(theFile->Get("Data"));
  if (theTree == 0) {
    // Return empty histogram
    return _emptyHist;
  }

  int id;
  double p;
  theTree->SetBranchAddress("id", &id);
  theTree->SetBranchAddress("p", &p);

  double pMax = theTree->GetMaximum("p");
  TH1D* mtmHist = new TH1D(histName.c_str(), "", 100, 0.0, pMax*1.1);

  int i;
  int nEntries = theTree->GetEntries();
  for (i = 0; i < nEntries; i++) {

    theTree->GetEntry(i);
    int testInt = this->getPartGroup(id);
    
    // See if the particle id matches any in the group integer vector
    vector<int>::iterator iter;
    for (iter = groupInts.begin(); iter != groupInts.end(); ++iter) {
      int groupInt = *iter;

      if (groupInt == testInt) {

	// We have the right particle group id code
	mtmHist->Fill(p);
      }
    }

  }

  return mtmHist;

}

TH1D* compareRootFiles::getPartIdHist(TFile* theFile, string histName) {

  // Group pi,K,D etc.. into groups and plot multiplicities
  // with bins along the x axis representing each group  
  // leptons  gamma  pi+-  pi0   K+-  K0  D+-  D0  light_baryons  strange_baryons  other

  if (theFile == 0) {
    // Return empty histogram
    return _emptyHist;
  }

  TTree* theTree = dynamic_cast<TTree*>(theFile->Get("Data"));
  if (theTree == 0) {
    // Return empty histogram
    return _emptyHist;
  }

  int id;
  theTree->SetBranchAddress("id", &id);

  int nGroupMax = _nGroupMax;
  vector<double> partNumbers(nGroupMax);
  int iP;
  for (iP = 0; iP < nGroupMax; iP++) {
    partNumbers[iP] = 0.0;
  }

  int i;
  int nEntries = theTree->GetEntries();
  for (i = 0; i < nEntries; i++) {

    theTree->GetEntry(i);

    int groupInt = this->getPartGroup(id);
    if (groupInt >= 0 && groupInt < nGroupMax) {
      partNumbers[groupInt] += 1.0;
    }

  }

  TH1D* idHist = new TH1D(histName.c_str(), "", nGroupMax, 0, nGroupMax);
  idHist->SetMinimum(1);

  for (iP = 0; iP < nGroupMax; iP++) {

    double total = partNumbers[iP];
    idHist->Fill(iP, total);

  }

  idHist->Scale(1.0/(nEntries*1.0));
  idHist->SetMaximum(1.0);

  return idHist;

}

int compareRootFiles::getPartGroup(int PDGId) {

  int group(-1);

  int absPDGId = std::abs(PDGId);
  
  if (absPDGId >= 11 && absPDGId <= 16) {
    group = 0; // leptons
  } else if (absPDGId == 22) {
    group = 1; // photon
  } else if (absPDGId == 211) {
    group = 2; // pi+-
  } else if (absPDGId == 111) {
    group = 3; // pi0
  } else if (absPDGId == 321) {
    group = 4; // K+-
  } else if (absPDGId == 311 || absPDGId == 130 || absPDGId == 310) {
    group = 5; // K0
  } else if (absPDGId == 411) {
    group = 6; // D+-
  } else if (absPDGId == 421) {
    group = 7; // D0
  } else if (absPDGId == 2212 || absPDGId == 2112 || absPDGId == 2224 || 
	     absPDGId == 2214 || absPDGId == 2114 || absPDGId == 1114) {
    group = 8; // light baryons
  } else if (absPDGId >= 3112 && absPDGId <= 3334) {
    group = 9; // strange baryons
  } else if (absPDGId != 0) {
    group = 10; // other particles
  }

  return group;

}

TH1D* compareRootFiles::getAllIdHist(TFile* theFile, string histName) {

  if (theFile == 0) {
    // Return empty histogram
    return _emptyHist;
  }
  
  TTree* theTree = dynamic_cast<TTree*>(theFile->Get("Data"));
  if (theTree == 0) {
    // Return empty histogram
    return _emptyHist;
  }

  int id;
  theTree->SetBranchAddress("id", &id);

  // Just limit the PDG id integer codes up to 4000
  int idMax(4000);
  TH1D* idHist = new TH1D(histName.c_str(), "", 100, -idMax, idMax);

  int nEntries = theTree->GetEntries();
  cout<<"Number of entries = "<<nEntries<<endl;
  int i;
  for (i = 0; i < nEntries; i++) {

    theTree->GetEntry(i);
    idHist->Fill(id);

  }

  double nIdScale = idHist->Integral();
  idHist->Scale(1.0/nIdScale);
  idHist->SetMaximum(1.0);

  return idHist;

}

TH1D* compareRootFiles::getDaugHist(TFile* theFile, string histName) {

  if (theFile == 0) {
    // Return empty histogram
    return _emptyHist;
  }

  TTree* nDaugTree = dynamic_cast<TTree*>(theFile->Get("nDaugTree"));
  if (nDaugTree == 0) {
    // Return empty histogram
    return _emptyHist;
  }

  int nDaug;
  nDaugTree->SetBranchAddress("nDaug", &nDaug);

  int nDaugMax = (int) nDaugTree->GetMaximum("nDaug");
  int nDaugLimit = nDaugMax + 2;
  
  TH1D* nDaugHist = new TH1D(histName.c_str(), "", nDaugLimit, 0, nDaugLimit);

  int nEntries = nDaugTree->GetEntries();
  cout<<"Number of entries = "<<nEntries<<endl;
  int i;
  for (i = 0; i < nEntries; i++) {

    nDaugTree->GetEntry(i);

    if (nDaug > 0) {
      nDaugHist->Fill(nDaug);
    }

  }

  double nDaugScale = nDaugHist->Integral();
  nDaugHist->Scale(1.0/nDaugScale);

  cout<<"nDaugScale = "<<nDaugScale<<endl;

  return nDaugHist;
}


int main(int argc, char** argv) {

  string decayName("UpsilonDecay1");
  if (argc > 1) {decayName = argv[1];} 

  string newRootFile("rootFiles/newUpsilonDecay1.root");
  if (argc > 2) {newRootFile = argv[2];}

  string oldRootFile("rootFiles/oldUpsilonDecay1.root");
  if (argc > 3) {oldRootFile = argv[3];}

  string description("Example description");
  if (argc > 4) {description = argv[4];}

  compareRootFiles myCompareRootFiles(decayName, newRootFile, oldRootFile, description);

  myCompareRootFiles.getNDaugPlots();
  myCompareRootFiles.getAllIdPlots();
  myCompareRootFiles.getPartIdPlots();
  myCompareRootFiles.getMtmPlots();

}
