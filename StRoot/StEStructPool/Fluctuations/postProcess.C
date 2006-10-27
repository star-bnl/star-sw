#include <stdio.h>
#include <string.h>
#include "TCanvas.h"
#include "StRoot/StEStructPool/Fluctuations/StEStructSigAnal.h"
#include "TStyle.h"
#include "TSystem.h"
#include "TFile.h"
#include "TH2D.h"

/***********************************************************
 * NOTE: In exportForMatlab I have typed in centrality and pt range names.
 *       This will likely need to be modified sometime somewhere.
 ***********************************************************/

/*
//  gROOT->LoadMacro("load2ptLibs.C");
//  load2ptLibs();
//  .L postProcess.C+
 */
 
 /*
//   postProcess *pp = new postProcess("/common/star/stardata/estruct/prindle/Hijing/auau200/QuenchOff/testForJamie");
//   pp->createSigmas();
//   pp->centralityMults(1200,1);
//   pp->multiplicity(1200,1);
//   pp->deltaSigma2(1);
  */
class postProcess {
    public:
        char    mDir[2048];
        StEStructSigAnal *msigA;
        TFile   *mFile;
        TH2D    *mSig[16*11];
        TCanvas *mCan;
        char    *mCanType;
        int mnEta, mnPhi;
        int mnCents, mnPts, mnPtCents, mnx, mny;
        double iEta, iPhi, sigN, sigPt, error;

        postProcess( char *dir );
        virtual ~postProcess();
        void createSigmas();
        void plotCentrality( int iCent, int ptBin=0, int print=0 );
        void plotType( char *pType, int ptBin=0, int print=0 );
        void centralityMults( int N0, int print=0 );
        void multiplicity( int N0, int print=0 );
        void pt( int P0, int print=0 );
        void deltaSigma2( int print=0 );
        void exportForMatlab();
        void importFromMatlab();
        int  getNCents();
        int  getNPts();
        int  getNPtCents();
        int  getNX( int nCent );
        int  getNY( int nCent );
        // A few methods only useful for Hijing data.
        void plotMCInfo( int print=0 );
        void plotBinaryCollisions( int print=0 );
        void plotParticipants( int print=0 );
        void exportMCInfo();
};

postProcess::postProcess( char *dir ) {
    gStyle->SetPadTopMargin(0.2);
    gStyle->SetPadLeftMargin(0.15);
    gStyle->SetCanvasColor(0);
    gStyle->SetTitleBorderSize(0);
    gStyle->SetTitleBorderSize(0);
    gStyle->SetPadBorderSize(0);
    gStyle->SetTitleColor(0);
    gStyle->SetTitleFillColor(0);
    gStyle->SetOptStat(0);
    gStyle->SetPalette(1);
    gStyle->SetTitleX(0.2);
    gStyle->SetTitleH(0.08);
    gStyle->SetPadBorderMode(0);

    strcpy( mDir, dir );

    mnEta     = 16;
    mnPhi     = 24;
    mnCents   = 0;
    mnPts     = 0;
    mnPtCents = 0;
    mnx       = 0;
    mny       = 0;
    mFile = 0;
    msigA = 0;
    mCan  = 0;
    mCanType = 0;
}
postProcess::~postProcess() {
    free(mDir);
    if (mFile) {
        delete mFile;
    }
    if (msigA) {
        delete msigA;
    }
    if (mCan) {
        delete mCan;
    }
}
void postProcess::createSigmas() {

    char buffer[1024];
    sprintf(buffer,"%s/data/Data.root",mDir);
    if (msigA) {
        delete msigA;
    }
    msigA = new StEStructSigAnal(buffer);
    msigA->fillHistograms();
    sprintf(buffer,"%s/data/Sigmas.root",mDir);
    TFile *out = new TFile(buffer,"RECREATE");
    msigA->writeHistograms(out);
    delete out;
}
void postProcess::plotCentrality( int iCent, int ptBin, int print ) {
    if (iCent < 0) {
        printf("I don't understand the meaning of centrality = %i\n",iCent);
        return;
    }
    char buffer[1024];
    sprintf(buffer,"%s/data/Sigmas.root",mDir);
    if (mFile) {
        delete mFile;
    }
    mFile = new TFile(buffer);

    char hist[1024];

    mnPts = getNPts();
    if (ptBin > mnPts) {
        printf("You asked for pt bin %i. There are only %i available.\n",ptBin, mnPts);
        return;
    }
    int nCents, nx, ny;
    if (0 == ptBin) {
        nCents = getNCents();
        nx = getNX(nCents);
        ny = getNY(nCents);
    } else {
        nCents = getNPtCents();
        nx = getNX(nCents);
        ny = getNY(nCents);
    }
    if (iCent >= nCents) {
        printf("You asked for centrality %i. There are only %i available (and we start counting at 0).\n",iCent, nCents);
        return;
    }

    char *type[] = {"NSig",  "NDel",  "NPlus",  "NMinus",  "NPlusMinus",
                    "PSig",  "PDel",  "PPlus",  "PMinus",  "PPlusMinus",
                    "PNSig", "PNDel", "PNPlus", "PNMinus", "PNPlusMinus",
                    "PNMinusPlus"};
    char *index[] = {"",  "",  "",  "",  "",
                     "1", "1", "1", "1", "1",
                     "1", "1", "1", "1", "1",
                     "1"};
    int order[] = {1, 4, 7, 10, 13,   2, 5, 8, 11, 14,   3, 6, 9, 12, 15};
    TH2D *tmp;

    if (!mCanType) {
        mCan = new TCanvas("c","c",3*250,5*250);
        mCan->Divide(3,5);
    } else if (strcmp(mCanType,"plotCentrality")) {
        mCan->SetWindowSize(3*250,5*250);
        mCan->Clear();
        mCan->Divide(3,5);
    }
    mCanType = "plotCentrality";
    // Omit plotting PNMinusPlus. Should be same as PNPlusMinus.
    // (Hope I don't miss a big surprise.)
    for (int iT=0;iT<15;iT++) {
        mCan->cd(order[iT]);
        if (0 == ptBin) {
            sprintf( hist, "%s%s_%i", type[iT], index[iT], iCent );
        } else {
            sprintf( hist, "%s%s_%i_%i", type[iT], index[iT], iCent, ptBin-1 );
        }
        tmp = (TH2D *) gDirectory->Get(hist);
        double val0 = tmp->GetBinContent(1,1);
        double val1 = tmp->GetBinContent(24,16);
        if (val1 < val0) {
            gPad->SetPhi(-155);
        } else {
            gPad->SetPhi();
        }
        if (0 == ptBin) {
            sprintf(buffer,"%s, centrality %i",type[iT],iCent);
        } else {
            sprintf(buffer,"%s, centrality %i, pt Bin %i",type[iT],iCent,ptBin-1);
        }
        tmp->SetTitle(buffer);
        TAxis *x = tmp->GetXaxis();
        TAxis *y = tmp->GetYaxis();
        TAxis *z = tmp->GetZaxis();
        x->SetTitleSize(0.07);
        x->SetTitleColor(1);
        x->SetTitleOffset(1);
        x->SetNdivisions(505);
        x->SetLabelSize(0.05);
        x->SetTitle("#delta_{#phi}");
        y->SetTitleSize(0.07);
        y->SetTitleOffset(1);
        y->SetTitleColor(1);
        y->SetNdivisions(505);
        y->SetLabelSize(0.05);
        y->SetTitle("#delta_{#eta}");
        z->SetTitleSize(0.07);
        z->SetTitleOffset(0.6);
        z->SetTitleColor(13);
        z->SetNdivisions(505);
        z->SetLabelSize(0.05);
        tmp->Draw("surf1");
    }
    if (print) {
        if (0 == ptBin) {
            sprintf(buffer,"%s/txt/centrality%i.eps",mDir,iCent);
        } else {
            sprintf(buffer,"%s/txt/centrality%i_ptBin%i.eps",mDir,iCent,ptBin-1);
        }
        mCan->Print(buffer);
    }
}
void postProcess::plotType( char *pType, int ptBin, int print ) {
    char *type[] = {"NSig",  "NDel",  "NPlus",  "NMinus",  "NPlusMinus",
                    "PSig",  "PDel",  "PPlus",  "PMinus",  "PPlusMinus",
                    "PNSig", "PNDel", "PNPlus", "PNMinus", "PNPlusMinus",
                    "PNMinusPlus"};
    char *index[] = {"",  "",  "",  "",  "",
                     "1", "1", "1", "1", "1",
                     "1", "1", "1", "1", "1",
                     "1"};
    int iT;
    int found = 0;
    for (iT=0;iT<16;iT++) {
        if (!strcmp(pType,type[iT])) {
            found = 1;
            break;
        }
    }
    if (!found) {
        printf("I didn't understand type of %s.\n",pType);
        return;
    }
    char buffer[1024];
    sprintf(buffer,"%s/data/Sigmas.root",mDir);
    if (mFile) {
        delete mFile;
    }
    mFile = new TFile(buffer);

    mnPts = getNPts();
    if (ptBin > mnPts) {
        printf("I only found %i ptBins. You asked for number %i\n",mnPts,ptBin);
        return;
    }
    // Check for number of centrality bins by trying to read histograms until failure.
    int nCents, nx, ny;
    if (0 == ptBin) {
        nCents = getNCents();
        nx = getNX(nCents);
        ny = getNY(nCents);
    } else {
        nCents = getNPtCents();
        nx = getNX(nCents);
        ny = getNY(nCents);
    }

    TH2D *tmp;
    if (!mCanType) {
        mCan = new TCanvas("c","c",nx*250,ny*250);
        mCan->Divide(mnx,mny);
    } else if (strcmp(mCanType,"plotType") || nx != mnx || ny != mny) {
        mCan->SetWindowSize(nx*250,ny*250);
        mCan->Clear();
        mCan->Divide(nx,ny);
    }
    mnx = nx;
    mny = ny;
    mCanType = "plotType";

    char hist[1024];
    for (int iB=0;iB<nCents;iB++) {
        mCan->cd(iB+1);
        if (0 == ptBin) {
            sprintf( hist, "%s%s_%i", pType, index[iT], iB );
        } else {
            sprintf( hist, "%s%s_%i_%i", pType, index[iT], iB, ptBin-1 );
        }
        tmp = (TH2D *) gDirectory->Get(hist);
        double val0 = tmp->GetBinContent(1,1);
        double val1 = tmp->GetBinContent(24,16);
        if (val1 < val0) {
            gPad->SetPhi(-155);
        } else {
            gPad->SetPhi();
        }
        if (0 == ptBin) {
            sprintf(buffer,"%s, centrality %i",pType,iB);
        } else {
            sprintf(buffer,"%s, centrality %i, pt Bin %i",pType,iB,ptBin-1);
        }
        tmp->SetTitle(buffer);
        TAxis *x = tmp->GetXaxis();
        TAxis *y = tmp->GetYaxis();
        TAxis *z = tmp->GetZaxis();
        x->SetTitleSize(0.07);
        x->SetTitleColor(1);
        x->SetTitleOffset(1);
        x->SetNdivisions(505);
        x->SetLabelSize(0.05);
        x->SetTitle("#delta_{#phi}");
        y->SetTitleSize(0.07);
        y->SetTitleOffset(1);
        y->SetTitleColor(1);
        y->SetNdivisions(505);
        y->SetLabelSize(0.05);
        y->SetTitle("#delta_{#eta}");
        z->SetTitleSize(0.07);
        z->SetTitleOffset(0.6);
        z->SetTitleColor(13);
        z->SetNdivisions(505);
        z->SetLabelSize(0.05);
        tmp->Draw("surf1");
    }
    if (print) {
        if (0 == ptBin) {
            sprintf(buffer,"%s/txt/%s.eps",mDir,pType);
        } else {
            sprintf(buffer,"%s/txt/%s_%i.eps",mDir,pType,ptBin-1);
        }
        mCan->Print(buffer);
    }
}
void postProcess::centralityMults( int N0, int print ) {
    char buffer[1024];
    sprintf(buffer,"%s/QA/QA.root",mDir);
    if (mFile) {
        delete mFile;
    }
    mFile = new TFile(buffer);

    // Check for number of centrality bins by trying to read histograms until failure.
    int mnCents = 0;
    for (int iB=0;iB<100;iB++) {
        sprintf(buffer,"multNSum_%i", iB );
        if (gDirectory->Get(buffer)) {
            mnCents++;
        } else{
            break;
        }
    }
    int mnPtCents = 0;
    for (int iB=0;iB<100;iB++) {
        sprintf(buffer,"multNSum_%i_0", iB );
        if (gDirectory->Get(buffer)) {
            mnPtCents++;
        } else{
            break;
        }
    }
    mnx = getNX(mnCents+1);
    mny = getNY(mnCents+1);

    // Assume all histograms also have low and high pt version.
    // All other routines in this file check not only for number of pt
    // bins but how many centrality bins there are fro pt selected histograms.
    // In this routine I am lazy (partly because I want to super-pose
    // low and high pt on all-pt histogram versions.
    float nBar[mnCents];
    int   num[mnCents];
    TH1F *tmp    = (TH1F *) gDirectory->Get("multNSum_0");
    nBar[0]      = tmp->GetMean();
    num[0]       = (int) tmp->GetEntries();
    TH1F *mult       = (TH1F *) tmp->Clone();
    TH1F *mult34     = (TH1F *) tmp->Clone();
    TH1F *multLow    = (TH1F *) tmp->Clone();
    TH1F *mult34Low  = (TH1F *) tmp->Clone();
    TH1F *multHigh   = (TH1F *) tmp->Clone();
    TH1F *mult34High = (TH1F *) tmp->Clone();

    for (int i=1;i<mnCents;i++) {
        sprintf(buffer,"multNSum_%i",i);
        TH1F *tmp = (TH1F *) gDirectory->Get(buffer);
        nBar[i]   = tmp->GetMean();
        num[i]    = (int) tmp->GetEntries();
        mult->Add(tmp);
    }
    for (int i=1;i<mnPtCents;i++) {
        sprintf(buffer,"multNSum_%i_0",i);
        tmp = (TH1F *) gDirectory->Get(buffer);
        multLow->Add(tmp);
        sprintf(buffer,"multNSum_%i_1",i);
        tmp = (TH1F *) gDirectory->Get(buffer);
        multHigh->Add(tmp);
    }

    for (int i=1;i<=mult->GetNbinsX();i++) {
        float val = pow(i+0.0,0.75) * mult->GetBinContent(i);
        mult34->SetBinContent(i,val);
        val = pow(i+0.0,0.75) * multLow->GetBinContent(i);
        mult34Low->SetBinContent(i,val);
        val = pow(i+0.0,0.75) * multHigh->GetBinContent(i);
        mult34High->SetBinContent(i,val);
    }

    FILE *out;
    char outFileName[1024];
    sprintf( outFileName, "%s/txt/NBar.txt", mDir );
    out = fopen(outFileName,"w");

    for (int i=0;i<mnCents;i++) {
        fprintf(out,"Centrality %i has mean multiplicity of %f with %i events\n",i,nBar[i],num[i]);
    }
    fclose(out);

    if (!mCanType) {
        mCan = new TCanvas("c","c",mnx*250,mny*250);
        mCan->Divide(mnx,mny);
    } else if (strcmp(mCanType,"centralityMults")) {
        mCan->SetWindowSize(mnx*250,mny*250);
        mCan->Clear();
        mCan->Divide(mnx,mny);
    }
    mCanType = "centralityMults";

    for (int i=0;i<mnCents;i++) {
        mCan->cd(i+1);
        sprintf(buffer,"multNSum_%i",i);
        tmp = (TH1F *) gDirectory->Get(buffer);
        tmp->SetAxisRange(0,N0);
        tmp->SetLineColor(1);
        sprintf(buffer,"centrality bin %i",i);
        tmp->SetTitle(buffer);
        tmp->Draw();
        if (mnPtCents == mnCents) {
            sprintf(buffer,"multNSum_%i_0",i);
            tmp = (TH1F *) gDirectory->Get(buffer);
            tmp->SetAxisRange(0,N0);
            tmp->SetLineColor(2);
            tmp->Draw("same");

            sprintf(buffer,"multNSum_%i_1",i);
            TH1F *tmp = (TH1F *) gDirectory->Get(buffer);
            sprintf(buffer,"centrality bin %i",i);
            tmp->SetTitle(buffer);
            tmp->SetAxisRange(0,N0);
            tmp->SetLineColor(3);
            tmp->Draw("same");
        }
    }
    mCan->cd(mnCents+1);
    mult34Low->SetAxisRange(0,N0);
    mult34Low->SetLineColor(3);
    mult34Low->SetTitle("m^3/4*dN/dm");
    mult34Low->Draw();
    mult34High->SetAxisRange(0,N0);
    mult34High->SetLineColor(2);
    mult34High->Draw("same");

    mult34->SetTitle("m^3/4*dN/dm");
    mult34->SetAxisRange(0,N0);
    mult34->SetLineColor(1);
    mult34->Draw("same");

    if (print) {
        sprintf(buffer,"%s/txt/MultBins.eps",mDir);
        mCan->Print(buffer);
    }
}
void postProcess::multiplicity( int N0, int print ) {
    char buffer[1024];
    sprintf(buffer,"%s/data/Data.root",mDir);
    if (mFile) {
        delete mFile;
    }
    mFile = new TFile(buffer);

    TH1F *mult, *mult34;
    mult = (TH1F *) gDirectory->Get("Multiplicity");
    mult34 = (TH1F *) mult->Clone();
    TAxis *x = mult->GetXaxis();
    TAxis *y = mult->GetYaxis();
    x->SetTitle("Mult");
    y->SetTitle("#frac{dN}{dM}");
    x = mult34->GetXaxis();
    y = mult34->GetYaxis();
    x->SetTitle("Mult");
    y->SetTitle("#frac{dN}{dM} * M^{3/4}");
    y->SetTitleOffset(1.75);

    FILE *out;
    char outFileName[1024];
    sprintf( outFileName, "%s/txt/Multiplicities.txt", mDir );
    out = fopen(outFileName,"w");
    for (int i=1;i<=mult->GetNbinsX();i++) {
        float val = pow(i+0.0,0.75) * mult->GetBinContent(i);
        mult34->SetBinContent(i,val);
        fprintf(out,"%i %f\n",i,mult->GetBinContent(i));
    }
    fclose(out);

    if (!mCanType) {
        mCan = new TCanvas("c","c",800,400);
        mCan->Divide(2,1);
    } else if (strcmp(mCanType,"multiplicity")) {
        mCan->SetWindowSize(800,400);
        mCan->Clear();
        mCan->Divide(2,1);
    }
    mCanType = "multiplicity";

    mCan->cd(1);
    gPad->SetLogy(1);
    mult->SetAxisRange(0,N0);
    mult->SetTitle("Multiplicity");
    mult->Draw();
    mCan->cd(2);
    mult34->SetAxisRange(0,N0);
    mult34->SetTitle("Multiplicity");
    mult34->Draw();
    if (print) {
        sprintf(buffer,"%s/txt/Multiplicity.eps",mDir);
        mCan->Print(buffer);
    }
}
void postProcess::pt( int P0, int print ) {
    char buffer[1024];
    sprintf(buffer,"%s/data/Data.root",mDir);
    if (mFile) {
        delete mFile;
    }
    mFile = new TFile(buffer);

    TH1F *pt, *pt34;
    pt = (TH1F *) gDirectory->Get("Pt");
    pt34 = (TH1F *) pt->Clone();
    TAxis *x = pt->GetXaxis();
    TAxis *y = pt->GetYaxis();
    x->SetTitle("P_{#perp}");
    y->SetTitle("#frac{dN}{dP_{#perp}}");
    x = pt34->GetXaxis();
    y = pt34->GetYaxis();
    x->SetTitle("P_{#perp}");
    y->SetTitle("#frac{dN}{dP_{#perp}} * P_{#perp}^{3/4}");
    y->SetTitleOffset(1.75);

    FILE *out;
    char outFileName[1024];
    sprintf( outFileName, "%s/txt/Pt.txt", mDir );
    out = fopen(outFileName,"w");
    for (int i=1;i<=pt->GetNbinsX();i++) {
        float val = pow(i+0.0,0.75) * pt->GetBinContent(i);
        pt34->SetBinContent(i,val);
        fprintf(out,"%i %f\n",i,pt->GetBinContent(i));
    }
    fclose(out);

    if (!mCanType) {
        mCan = new TCanvas("c","c",800,400);
        mCan->Divide(2,1);
    }
    if (!mCanType || strcmp(mCanType,"pt")) {
        mCan->SetWindowSize(800,400);
        mCan->Clear();
        mCan->Divide(2,1);
    }
    mCanType = "pt";

    mCan->cd(1);
    gPad->SetLogy(1);
    pt->SetAxisRange(0,P0);
    pt->SetTitle("Pt");
    pt->Draw();
    mCan->cd(2);
    pt34->SetAxisRange(0,P0);
    pt34->SetTitle("Pt");
    pt34->Draw();
    if (print) {
        sprintf(buffer,"%s/txt/Pt.eps",mDir);
        mCan->Print(buffer);
    }
}
void postProcess::deltaSigma2( int print ) {
    char buffer[1024];
    sprintf(buffer,"%s/data/Sigmas.root",mDir);
    if (mFile) {
        delete mFile;
    }
    mFile = new TFile(buffer);

    // Check for number of centrality bins by trying to read histograms until failure.
    char hist[1024];
    mnCents = getNCents();
    mnx = getNX(mnCents);
    mny = getNY(mnCents);

    FILE *out;
    char outFileName[1024];
    sprintf( outFileName, "%s/txt/NBar.txt", mDir );
    out = fopen(outFileName,"a+");

    // Print out ptHat values for full scale.
    TH2D *pthat;
    fprintf(out,"\n");
    fprintf(out,"\n");
    fprintf(out,"\n");
    for (int iB=0;iB<mnCents;iB++) {
        sprintf(hist,"SptHat_%i", iB );
        pthat = (TH2D *) gDirectory->Get(hist);
        fprintf(out," Centrality %i has ptHat of %f\n",iB,pthat->GetBinContent(24,16));
    }
    fprintf(out,"\n");
    fprintf(out,"\n");
    fprintf(out,"\n");
    for (int iB=0;iB<mnCents;iB++) {
        sprintf(hist,"PptHat_%i", iB );
        pthat = (TH2D *) gDirectory->Get(hist);
        fprintf(out," Centrality %i has ptHat+ of %f\n",iB,pthat->GetBinContent(24,16));
    }
    fprintf(out,"\n");
    fprintf(out,"\n");
    fprintf(out,"\n");
    for (int iB=0;iB<mnCents;iB++) {
        sprintf(hist,"MptHat_%i", iB );
        pthat = (TH2D *) gDirectory->Get(hist);
        fprintf(out," Centrality %i has ptHat- of %f\n",iB,pthat->GetBinContent(24,16));
    }
    // Print out \sigma_{hat_p_perp} values for full scale.
    fprintf(out,"\n");
    fprintf(out,"\n");
    fprintf(out,"\n");
    for (int iB=0;iB<mnCents;iB++) {
        sprintf(hist,"SsigPtHat_%i", iB );
        pthat = (TH2D *) gDirectory->Get(hist);
        fprintf(out," Centrality %i has sigma_{hat_p_perp} of %f\n",iB,sqrt(pthat->GetBinContent(24,16)));
    }
    fclose(out);

    // Plot \Delta\sigma^2 histograms.
    TH2D *psig;
    if (!mCanType || strcmp(mCanType,"deltaSigma2")) {
        mCan = new TCanvas("c","c",mnx*250,mny*250);
        mCan->SetWindowPosition(1,1);
        mCan->Divide(mnx,mny);
        mCanType = "deltaSigma2";
    }
    char title[1024];
    for (int iB=0;iB<mnCents;iB++) {
        sprintf(hist,"PSig1_%i", iB );
        psig = (TH2D *) gDirectory->Get(hist);

        TAxis *x = psig->GetXaxis();
        TAxis *y = psig->GetYaxis();
        TAxis *z = psig->GetZaxis();
        x->SetTitleSize(0.075);
        x->SetTitleColor(1);
        x->SetTitleOffset(1);
        x->SetNdivisions(505);
        x->SetLabelSize(0.05);
        x->SetTitle("#delta_{#phi}");
        y->SetTitleSize(0.075);
        y->SetTitleOffset(1);
        y->SetTitleColor(1);
        y->SetNdivisions(505);
        y->SetLabelSize(0.05);
        y->SetTitle("#delta_{#eta}");
        z->SetTitleSize(0.08);
        z->SetTitleOffset(0.6);
        z->SetTitleColor(13);
        z->SetNdivisions(505);
        z->SetLabelSize(0.05);
        sprintf(title,"#Delta#sigma^{2}, bin %i",iB);
        psig->SetTitle(title);

        mCan->cd(iB+1);
        psig->Draw("surf1");
    }
    if (print) {
        sprintf(buffer,"%s/txt/Deltasigma2.eps",mDir);
        mCan->Print(buffer);
    }
}
void postProcess::exportForMatlab() {
    char buffer[1024];
    sprintf( buffer, "%s/data/Sigmas.root", mDir );
    if (mFile) {
        delete mFile;
    }
    mFile = new TFile(buffer);

    char *measName[] = {"NSig",       "PSig",       "PNSig",
                        "NDel",       "PDel",       "PNDel",
                        "NPlus",      "PPlus",      "PNPlus",
                        "NMinus",     "PMinus",     "PNMinus",
                        "NPlusMinus", "PPlusMinus", "PNPlusMinus", "PNMinusPlus"};
    char *outName[] = {"N_CI",        "P_CI",        "PN_CI",
                       "N_CD",        "P_CD",        "PN_CD",
                       "N_Plus",      "P_Plus",      "PN_Plus",
                       "N_Minus",     "P_Minus",     "PN_Minus",
                       "N_PlusMinus", "P_PlusMinus", "PN_PlusMinus", "PN_MinusPlus"};
    char *index[] = {"", "1", "1",
                     "", "1", "1",
                     "", "1", "1",
                     "", "1", "1",
                     "", "1", "1", "1"};
    char *centName[11];
    char *ptRange[] = {"0.15-0.5", "0.5-2.0"};

    FILE *out;
    char outFileName[1024];
    TH2D *hist;

    int nCents, nPtCents, nPts;
    nCents   = getNCents();
    nPtCents = getNPtCents();
    nPts     = getNPts();

    if (1 == nCents) {
        centName[0] = "b0-3";
    } else if (2 == nCents) {
        centName[0] = "50-100";
        centName[1] = "0-50";
    } else if (4 == nCents) {
        centName[0] = "0-25";
        centName[1] = "25-50";
        centName[2] = "50-75";
        centName[3] = "75-100";
    } else if (5 == nCents) {
        centName[0] = "1-3";
        centName[1] = "4-6";
        centName[2] = "7-9";
        centName[3] = "10-12";
        centName[4] = "13-24";
    } else if (6 == nCents) {
        centName[0] = "1-17";
        centName[1] = "17-33";
        centName[2] = "33-50";
        centName[3] = "50-67";
        centName[4] = "67-83";
        centName[5] = "83-100";
    } else if (7 == nCents) {
        centName[0] = "80-100";
        centName[1] = "64-80";
        centName[2] = "48-64";
        centName[3] = "32-48";
        centName[4] = "16-32";
        centName[5] = "8-16";
        centName[6] = "0-8";
    } else if (11 == nCents) {
        centName[0] = "90-100";
        centName[1] = "80-90";
        centName[2] = "70-80";
        centName[3] = "60-70";
        centName[4] = "50-60";
        centName[5] = "40-50";
        centName[6] = "30-40";
        centName[7] = "20-30";
        centName[8] = "10-20";
        centName[9] = "5-10";
        centName[10] = "0-5";
    }
    for (int iD=0;iD<16;iD++) {
        for (int iC=0;iC<nCents;iC++) {
            sprintf( outFileName, "%s/txt/fluct_%s_%s.txt", mDir, outName[iD], centName[iC] );
            sprintf(buffer,"%s%s_%i", measName[iD], index[iD], iC );
            out = fopen(outFileName,"w");
            hist = (TH2D *) gDirectory->Get(buffer);
            for (int n=1;n<=hist->GetNbinsX();n++) {
                for (int m=1;m<=hist->GetNbinsY();m++) {
                    fprintf(out,"%f\n",hist->GetBinContent(n,m));
                }
            }
            fclose(out);

            sprintf( outFileName, "%s/txt/fluct_%s_%s_err.txt", mDir, outName[iD], centName[iC] );
            sprintf(buffer,"%sErrors_%i", measName[iD], iC );
            out = fopen(outFileName,"w");
            hist = (TH2D *) gDirectory->Get(buffer);
            for (int n=1;n<=hist->GetNbinsX();n++) {
                for (int m=1;m<=hist->GetNbinsY();m++) {
                    fprintf(out,"%f\n",hist->GetBinContent(n,m));
                }
            }
            fclose(out);
        }
    }
    if (1 == nPtCents) {
        centName[0] = "b0-3";
    } else if (2 == nPtCents) {
        centName[0] = "50-100";
        centName[1] = "0-50";
    } else if (4 == nPtCents) {
        centName[0] = "0-25";
        centName[1] = "25-50";
        centName[2] = "50-75";
        centName[3] = "75-100";
    } else if (5 == nPtCents) {
        centName[0] = "1-3";
        centName[1] = "4-6";
        centName[2] = "7-9";
        centName[3] = "10-12";
        centName[4] = "13-24";
    } else if (6 == nCents) {
        centName[0] = "1-17";
        centName[1] = "17-33";
        centName[2] = "33-50";
        centName[3] = "50-67";
        centName[4] = "67-83";
        centName[5] = "83-100";
    } else if (7 == nPtCents) {
        centName[0] = "80-100";
        centName[1] = "64-80";
        centName[2] = "48-64";
        centName[3] = "32-48";
        centName[4] = "16-32";
        centName[5] = "8-16";
        centName[6] = "0-8";
    } else if (11 == nPtCents) {
        centName[0] = "90-100";
        centName[1] = "80-90";
        centName[2] = "70-80";
        centName[3] = "60-70";
        centName[4] = "50-60";
        centName[5] = "40-50";
        centName[6] = "30-40";
        centName[7] = "20-30";
        centName[8] = "10-20";
        centName[9] = "5-10";
        centName[10] = "0-5";
    }
    for (int iD=0;iD<16;iD++) {
        for (int iC=0;iC<nPtCents;iC++) {
            for (int iP=0;iP<nPts;iP++) {
                sprintf( outFileName, "%s/txt/fluct_%s_%s_%s.txt", mDir, outName[iD], centName[iC], ptRange[iP] );
                sprintf(buffer,"%s%s_%i_%i", measName[iD], index[iD], iC, iP );
                out = fopen(outFileName,"w");
                hist = (TH2D *) gDirectory->Get(buffer);
                for (int n=1;n<=hist->GetNbinsX();n++) {
                    for (int m=1;m<=hist->GetNbinsY();m++) {
                        fprintf(out,"%f\n",hist->GetBinContent(n,m));
                    }
                }
                fclose(out);

                sprintf( outFileName, "%s/txt/fluct_%s_%s_%s_err.txt", mDir, outName[iD], centName[iC], ptRange[iP] );
                sprintf(buffer,"%sErrors_%i_%i", measName[iD], iC, iP );
                out = fopen(outFileName,"w");
                hist = (TH2D *) gDirectory->Get(buffer);
                for (int n=1;n<=hist->GetNbinsX();n++) {
                    for (int m=1;m<=hist->GetNbinsY();m++) {
                        fprintf(out,"%f\n",hist->GetBinContent(n,m));
                    }
                }
                fclose(out);
            }
        }
    }
}
void postProcess::importFromMatlab() {
}
int postProcess::getNCents () {
    char hist[1024];
    int nCents = 0;
    for (int iB=0;iB<100;iB++) {
        sprintf(hist,"NSig_%i", iB );
        if (gDirectory->Get(hist)) {
            nCents++;
        } else{
            break;
        }
    }
    return nCents;
}
int postProcess::getNPtCents () {
    char hist[1024];
    int nPtCents = 0;
    for (int iC=0;iC<100;iC++) {
        sprintf(hist,"NSig_%i_0", iC );
        if (gDirectory->Get(hist)) {
            nPtCents++;
        } else{
            break;
        }
    }
    return nPtCents;
}
int postProcess::getNPts () {
    char hist[1024];
    int nPts = 0;
    for (int iP=0;iP<100;iP++) {
        sprintf(hist,"NSig_0_%i", iP );
        if (gDirectory->Get(hist)) {
            nPts++;
        } else{
            break;
        }
    }
    return nPts;
}
int postProcess::getNX( int nCent ) {
    int nx = 0;
    if (nCent < 3) {
        nx = 1;
    } else if (nCent < 7) {
        nx = 2;
    } else if (nCent < 13) {
        nx = 3;
    } else {
        nx = 4;
    }
    return nx;
}
int postProcess::getNY( int nCent ) {
    int ny = 0;
    if (nCent < 2) {
        ny = 1;
    } else if (nCent < 5) {
        ny = 2;
    } else if (nCent < 10) {
        ny = 3;
    } else {
        ny = 4;
    }
    return ny;
}


void postProcess::plotMCInfo( int print ) {
    char buffer[1024];
    sprintf(buffer,"%s/QA/QA.root",mDir);
    if (mFile) {
        delete mFile;
    }
    mFile = new TFile(buffer);

    if (!mCanType || strcmp(mCanType,"plotMCInfo")) {
        mCan = new TCanvas("c","c",2*400,2*400);
        mCan->SetWindowPosition(1,1);
        mCan->Divide(2,2);
        mCanType = "plotMCInfo";
    }


    TH1D *hist;
    TAxis *x, *y;

    hist = (TH1D *) gDirectory->Get("impact");
    x = hist->GetXaxis();
    y = hist->GetYaxis();
    x->SetTitleSize(0.075);
    x->SetTitleColor(1);
    x->SetTitleOffset(1);
    x->SetNdivisions(505);
    x->SetLabelSize(0.05);
    x->SetTitle("fm");
    y->SetTitleSize(0.075);
    y->SetTitleOffset(1);
    y->SetTitleColor(1);
    y->SetNdivisions(505);
    y->SetLabelSize(0.05);
    y->SetTitle("Events");
    hist->SetTitle("Impact Parameter");
    mCan->cd(1);
    hist->Draw();

    hist = (TH1D *) gDirectory->Get("binary");
    x = hist->GetXaxis();
    y = hist->GetYaxis();
    x->SetTitleSize(0.075);
    x->SetTitleColor(1);
    x->SetTitleOffset(1);
    x->SetNdivisions(505);
    x->SetLabelSize(0.05);
    x->SetTitle("Number");
    y->SetTitleSize(0.075);
    y->SetTitleOffset(1);
    y->SetTitleColor(1);
    y->SetNdivisions(505);
    y->SetLabelSize(0.05);
    y->SetTitle("Events");
    hist->SetTitle("Binary collisions");
    mCan->cd(2);
    gPad->SetLogy();
    hist->Draw();

    hist = (TH1D *) gDirectory->Get("participant");
    x = hist->GetXaxis();
    y = hist->GetYaxis();
    x->SetTitleSize(0.075);
    x->SetTitleColor(1);
    x->SetTitleOffset(1);
    x->SetNdivisions(505);
    x->SetLabelSize(0.05);
    x->SetTitle("Number");
    y->SetTitleSize(0.075);
    y->SetTitleOffset(1);
    y->SetTitleColor(1);
    y->SetNdivisions(505);
    y->SetLabelSize(0.05);
    y->SetTitle("Events");
    hist->SetTitle("Participants");
    mCan->cd(3);
    gPad->SetLogy();
    hist->Draw();

    if (print) {
        sprintf(buffer,"%s/txt/MCInfo.eps",mDir);
        mCan->Print(buffer);
    }
}
void postProcess::plotBinaryCollisions( int print ) {
    char buffer[1024];
    sprintf(buffer,"%s/QA/QA.root",mDir);
    if (mFile) {
        delete mFile;
    }
    mFile = new TFile(buffer);

    // Check for number of centrality bins by trying to read histograms until failure.
    mnCents = getNCents();
    mnx = getNX(mnCents+1);
    mny = getNY(mnCents+1);

    if (!mCanType ||
        (strcmp(mCanType,"plotBinaryCollisions") &&
         strcmp(mCanType,"plotParticipants"))) {
        mCan = new TCanvas("c","c",mnx*250,mny*250);
        mCan->SetWindowPosition(1,1);
        mCan->Divide(2,3);
        mCanType = "plotBinaryCollisions";
    }


    TH1D *bin;
    for (int iB=0;iB<mnCents;iB++) {
        sprintf(buffer,"binary_%i", iB );
        bin = (TH1D *) gDirectory->Get(buffer);

        TAxis *x = bin->GetXaxis();
        TAxis *y = bin->GetYaxis();
        x->SetTitleSize(0.075);
        x->SetTitleColor(1);
        x->SetTitleOffset(1);
        x->SetNdivisions(505);
        x->SetLabelSize(0.05);
        x->SetTitle("binary collisions");
        y->SetTitleSize(0.075);
        y->SetTitleOffset(1);
        y->SetTitleColor(1);
        y->SetNdivisions(505);
        y->SetLabelSize(0.05);
        y->SetTitle("Events");
        sprintf(buffer,"impact class %i",iB);
        bin->SetTitle(buffer);

        gPad->SetLogy();
        mCan->cd(iB+1);
        bin->Draw();
    }
    if (print) {
        sprintf(buffer,"%s/txt/binaryCollisions.eps",mDir);
        mCan->Print(buffer);
    }
}
void postProcess::plotParticipants( int print ) {
    char buffer[1024];
    sprintf(buffer,"%s/QA/QA.root",mDir);
    if (mFile) {
        delete mFile;
    }
    mFile = new TFile(buffer);

    // Check for number of centrality bins by trying to read histograms until failure.
    mnCents = getNCents();
    mnx = getNX(mnCents+1);
    mny = getNY(mnCents+1);

    if (!mCanType ||
        (strcmp(mCanType,"plotBinaryCollisions") &&
         strcmp(mCanType,"plotParticipants"))) {
        mCan = new TCanvas("c","c",mnx*250,mny*250);
        mCan->SetWindowPosition(1,1);
        mCan->Divide(2,3);
        mCanType = "plotBinaryCollisions";
    }

    TH1D *par;
    for (int iB=0;iB<mnCents;iB++) {
        sprintf(buffer,"participant_%i", iB );
        par = (TH1D *) gDirectory->Get(buffer);

        TAxis *x = par->GetXaxis();
        TAxis *y = par->GetYaxis();
        x->SetTitleSize(0.075);
        x->SetTitleColor(1);
        x->SetTitleOffset(1);
        x->SetNdivisions(505);
        x->SetLabelSize(0.05);
        x->SetTitle("participants");
        y->SetTitleSize(0.075);
        y->SetTitleOffset(1);
        y->SetTitleColor(1);
        y->SetNdivisions(505);
        y->SetLabelSize(0.05);
        y->SetTitle("Events");
        sprintf(buffer,"impact class %i",iB);
        par->SetTitle(buffer);

        mCan->cd(iB+1);
        gPad->SetLogy();
        par->Draw();
    }
    if (print) {
        sprintf(buffer,"%s/txt/participants.eps",mDir);
        mCan->Print(buffer);
    }
}
void postProcess::exportMCInfo() {
    char buffer[1024];
    sprintf(buffer,"%s/QA/QA.root",mDir);
    if (mFile) {
        delete mFile;
    }
    mFile = new TFile(buffer);

    // Check for number of centrality bins by trying to read histograms until failure.
    mnCents = getNCents();

    // Write binary collisions and participants.
    // Each centrality to its own file.
    FILE *out;
    char outFileName[1024];
    TH1D *hist;
    for (int iC=0;iC<mnCents;iC++) {
        sprintf( outFileName, "%s/txt/binaryCollisions_%i.txt", mDir, iC );
        out = fopen(outFileName,"w");
        sprintf(buffer,"binary_%i", iC );
        hist = (TH1D *) gDirectory->Get(buffer);
        for (int n=1;n<=hist->GetNbinsX();n++) {
            fprintf(out,"%i   %6.f\n", n, hist->GetBinContent(n));
        }
        fclose(out);

        sprintf( outFileName, "%s/txt/participants_%i.txt", mDir, iC );
        out = fopen(outFileName,"w");
        sprintf(buffer,"participant_%i", iC );
        hist = (TH1D *) gDirectory->Get(buffer);
        for (int n=1;n<=hist->GetNbinsX();n++) {
            fprintf(out,"%i   %6.f\n", n, hist->GetBinContent(n));
        }
        fclose(out);
    }

    sprintf( outFileName, "%s/txt/impactParameter.txt", mDir );
    out = fopen(outFileName,"w");
    hist = (TH1D *) gDirectory->Get("impact");
    for (int n=1;n<=hist->GetNbinsX();n++) {
        fprintf(out,"%8.4f    %6.f\n",hist->GetBinCenter(n), hist->GetBinContent(n));
    }
    fclose(out);
}
