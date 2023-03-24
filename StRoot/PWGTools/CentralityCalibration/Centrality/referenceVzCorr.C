#include <iostream>
#include "TMath.h"
#include "TRandom3.h"
#include "TCanvas.h"
#include "TPad.h"
#include "TF1.h"
#include "TH1D.h"
#include "TGraph.h"
#include "TAxis.h"
#include "TGraphErrors.h"
#include "TFile.h"
#include "TNtuple.h"
#include <numeric>
#include <sstream>

int referenceVzCorr(){
    
   TFile *f0 = new TFile("referenceData.root");
    
    int numHists = 29;
    double Vz[numHists];
    double VzErr[numHists];
    TH1F *hist[numHists];
    TH1F *clonedhist[numHists];
    double h[numHists];
    double par0[numHists];
    double par1[numHists];
    double hErr[numHists];
    double chi2perNDF[numHists];
    double minbin = 320.0;
    double maxbin = 450.0;

    
    for(int i=0; i<numHists; i++){
        hist[i] = (TH1F *)f0->Get(Form("hRefMultPWVtxZ_%d",i));
        TF1 *f1 = new TF1("f1","([0]/10.0) * (TMath::Erf( -[1]*(x - [2]) ) + 1)", minbin, maxbin);
        f1->SetParameter(0,1.0e4);
        f1->SetParameter(1,0.027);
        f1->SetParameter(2,maxbin-50.0);
        TFitResultPtr r = hist[i]->Fit("f1","RLMS");
        h[i] = r->Parameter(2);
        par0[i] = r->Parameter(0);
        par1[i] = r->Parameter(1);
        hErr[i] = r->ParError(2);
        Vz[i]=10.0*(i-14.0);
        VzErr[i] = 5.0;
        TF1 *fit = hist[i]->GetFunction("f1");
        Double_t chi2 = fit->GetChisquare();
        double numbins = maxbin-minbin+1.0;
        cout<<"CHi2/ndf: "<<chi2<<"/"<<numbins<<endl;
        chi2perNDF[i] = chi2/numbins;
        delete f1;
    }
    
    TCanvas *c_100   = new TCanvas("c_100","Canvas",1200,600);
    c_100->Draw();
    TPad* pad_100 = new TPad("pad_100","Pad",0.,0.,1.,1.);
    pad_100->Divide(4,2);
    pad_100->Draw();
    TCanvas *c_200   = new TCanvas("c_200","Canvas",1200,600);
    c_200->Draw();
    TPad* pad_200 = new TPad("pad_200","Pad",0.,0.,1.,1.);
    pad_200->Divide(4,2);
    pad_200->Draw();
    TCanvas *c_300   = new TCanvas("c_300","Canvas",1200,600);
    c_300->Draw();
    TPad* pad_300 = new TPad("pad_300","Pad",0.,0.,1.,1.);
    pad_300->Divide(4,2);
    pad_300->Draw();
    TCanvas *c_400   = new TCanvas("c_400","Canvas",1200,600);
    c_400->Draw();
    TPad* pad_400 = new TPad("pad_400","Pad",0.,0.,1.,1.);
    pad_400->Divide(4,2);
    pad_400->Draw();

    
    for(int i=0; i<numHists; i++){
        hist[i] = (TH1F *)f0->Get(Form("hRefMultPWVtxZ_%d",i));
        hist[i]->GetXaxis()->SetTitle("Reference Multiplicity");
        
        TF1 *f1 = new TF1("f1","([0]/10.0) * (TMath::Erf( -[1]*(x - [2]) ) + 1)",minbin,maxbin);
        f1->SetParameter(0,par0[i]);
        f1->SetParameter(1,par1[i]);
        f1->SetParameter(2,h[i]);
        f1->SetLineWidth(5);
        if(i<8){
            pad_100->cd(i+1);
            pad_100->cd(i+1)->SetLogy();
            hist[i]->GetXaxis()->SetRangeUser(0,500.0);
            hist[i]->Draw("E");
            f1->Draw("SAME");
        }
        else if(i<16){
            pad_200->cd(i+1-8);
            pad_200->cd(i+1-8)->SetLogy();
            hist[i]->GetXaxis()->SetRangeUser(0,500.0);
            hist[i]->Draw("E");
            f1->Draw("SAME");
        }
        else if(i<24){
            pad_300->cd(i+1-16);
            pad_300->cd(i+1-16)->SetLogy();
            hist[i]->GetXaxis()->SetRangeUser(0,500.0);
            hist[i]->Draw("E");
            f1->Draw("SAME");
        }
        else if(i<32){
            pad_400->cd(i+1-24);
            pad_400->cd(i+1-24)->SetLogy();
            hist[i]->GetXaxis()->SetRangeUser(0,500.0);
            hist[i]->Draw("E");
            f1->Draw("SAME");
        }
    }
    

    TCanvas *allRefMult = new TCanvas("allRefMult","allRefMult",800,600);
    allRefMult->cd();
    TLegend *legP = new TLegend(0.45,0.65,0.9,0.9,"","NDC");
    legP->SetNColumns(3);
    int j[6]={0,0,0,0,0,0};
    bool secondrepeat = false;
    for(int i=0; i<29;i++){
        clonedhist[i] = (TH1F*) hist[i]->Clone();
        if(clonedhist[i]->GetBinContent(10)==0)continue;
        clonedhist[i]->Scale(1.0/clonedhist[i]->Integral(25,50));
        if(j[5]==5){j[0]=-1;j[1]=-1;j[2]=-1;j[3]=-1;j[4]=-1;j[5]=-1;secondrepeat=true;}
        if(secondrepeat){j[0]=-2;j[1]=-2;j[2]=-2;j[3]=-2;j[4]=-2;j[5]=-2;}
        if(i%6==0){clonedhist[i]->SetLineColor(kRed+j[0]);j[0]++;}
        if(i%6==1){clonedhist[i]->SetLineColor(kOrange+j[1]);j[1]++;}
        if(i%6==2){clonedhist[i]->SetLineColor(kGreen+j[2]);j[2]++;}
        if(i%6==3){clonedhist[i]->SetLineColor(kCyan+j[3]);j[3]++;}
        if(i%6==4){clonedhist[i]->SetLineColor(kBlue+j[4]);j[4]++;}
        if(i%6==5){clonedhist[i]->SetLineColor(kMagenta+j[5]);j[5]++;}
        clonedhist[i]->SetStats(0);
        if(i==0)clonedhist[i]->Draw("HIST");
        else clonedhist[i]->Draw("SAME HIST");
        TString name=Form("(%d,%d)cm",(int)(-145.0+10.*(i)),(int)(-145.0+10.*(i+1)));
        legP->AddEntry(clonedhist[i],name,"l");
    }
    legP->Draw();

    auto c1 = new TCanvas("c1","c1");
    TGraphErrors* g = new TGraphErrors(numHists, Vz, h, VzErr, hErr);
    g->SetMarkerStyle(kFullCircle);
    g->Draw("AP");
    
    TGraph* g_chi = new TGraph(numHists, Vz, chi2perNDF);
    auto cchi = new TCanvas("cchi","cchi");
    g_chi->SetMarkerStyle(kFullCircle);
    g_chi->Draw("AP");
    
    double lowedges[numHists];
    for(int i=0; i<numHists+1; i++){
        lowedges[i]=-145.0+10.0*(i);
        cout<<"Bin edges: "<<lowedges[i]<<endl;
    }
    TH1D* upperEdge = new TH1D("upperEdge","h as a function of Vz",numHists, lowedges);
    for(int i=0; i<numHists; i++){
        upperEdge->Fill(Vz[i],h[i]);
        upperEdge->SetBinError(i+1,hErr[i]);
    }
    
    auto c3 = new TCanvas("c3","c3");
    TF1 *f2 = new TF1("f2","[0]+[1]*x+[2]*x*x+[3]*x*x*x+[4]*x*x*x*x+[5]*x*x*x*x*x+[6]*x*x*x*x*x*x",-145.0,145.0);
    
    cout<<"***************************** SIXTH-ORDER VZ CORRECTION ********************************"<<endl;
    TFitResultPtr r2 = upperEdge->Fit("f2","RMS");
    TF1 *Vzfit = upperEdge->GetFunction("f2");
    Double_t Vzchi2 = Vzfit->GetChisquare();
    cout<<"CHI2/NDF: "<<Vzchi2/(29.0-f2->GetNumberFreeParameters())<<endl;
    
    cout<<"****************************** POINTWISE VZ CORRECTION ******************************"<<endl;
    cout<<"double vz_corr[29];"<<endl;
    for(int corrnum=0; corrnum<29; corrnum++){
        cout<<"vz_corr["<<corrnum<<"]="<<h[14]/h[corrnum]<<Form("; //Correction for Vz of (%d,%d)cm",(int)(-145.0+10.*(corrnum)),(int)(-145.0+10.*(corrnum+1)))<<endl;
    }
    
    return 0;
}
