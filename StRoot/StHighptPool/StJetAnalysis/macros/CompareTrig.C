#include <iostream>
//#include "TFile.h"
//#include "TCanvas.h"
//#include "TH1.h"

//void normalizeHist(TH1* hist);

void CompareTrig()
{
    cout <<"CompareTrig()"<<endl;
    const char* infile1="central_hists.root";
    const char* infile2="minbias_hists.root";

    cout <<"Open read file:\t"<<infile1<<endl;
    TFile* if1 = new TFile(infile1,"READ");
    TH1* pp1 = plusPlus;
    TH1* mm1 = minusMinus;
    TH1* pm1 = plusMinus;
    TH1* likeSign1 = likeSign;
    TH1* subtracted1 = subtracted;

    cout <<"Open read file:\t"<<infile2<<endl;
    TFile* if2 = new TFile(infile2,"READ");
    TH1* pp2 = plusPlus;
    TH1* mm2 = minusMinus;
    TH1* pm2 = plusMinus;
    TH1* likeSign2 = likeSign;
    TH1* subtracted2 = subtracted;

    cout <<"Normalize histograms"<<endl;
    normalizeHist(pp1);
    normalizeHist(mm1);
    normalizeHist(pm1);
    normalizeHist(likeSign1);
    normalizeHist(subtracted1);
    
    normalizeHist(pp2);
    normalizeHist(mm2);
    normalizeHist(pm2);
    normalizeHist(likeSign2);
    normalizeHist(subtracted2);

    TCanvas* c1 = new TCanvas();
    c1->SetLogy();
    likeSign1->Draw();
    likeSign2->SetLineColor(2);
    likeSign2->Draw("same");

    c2 = new TCanvas();
    c2->SetLogy();
    pm1->Draw();
    pm2->SetLineColor(2);
    pm2->Draw("same");
}

void normalizeHist(TH1* hist)
{
    double entries = hist->GetEntries();
    cout <<hist->GetName()<<"\t"<<hist->Integral()<<endl;
    hist->Scale(1./entries);
    cout <<hist->GetName()<<"\t"<<hist->Integral()<<endl;
}
