//  Set up canvas and load appropriate libraries.
TCanvas* c1=new TCanvas("c1");
.L makePlots.C
.L setupPalette.C
setupPalette();  // set up the colors
c1->Clear();

c1->SetWindowSize(750,500);
c1->Divide(3,2);

gStyle->SetOptTitle(0);
gStyle->SetOptStat(0);

gROOT->LoadMacro("load2ptLibs.C");
load2ptLibs();
gSystem->Load("StEStructPoolSupport.so");


// Use support code to combine charge types.
// I don't think the ytyt handling is up to date, and I don't
// know if PhiPhi is correct.
int nCent = 6;
TFile *tf[nCent];
StEStructSupport *ehelp[nCent];
TH2F **ytyt[nCent];
TH2F **ptdedp[nCent];
TH2F **dedp[nCent];
TH2F **phiphi[nCent];

char fileName[1024];
char *dir = "/common/star/stardata/estruct/prindle/Hijing/auau200/QuenchOff/testMode0";
float sf[2] = {0.98,0.98};
{
for (int ic=0;ic<nCent;ic++) {
    // The Symm part of this name refers to histogram files that have been
    // processed through selectAllM0. If you don't care about DEtaDPhi
    // or SEtaDPhi you can remove all the dedp stuff and then you don't
    // need to process histograms through selectAllM0.
    sprintf(fileName,"%s/data/Data%iSymm.root",dir,ic);
    tf[ic] = new TFile(fileName);
    ehelp[ic] = new StEStructSupport(tf[ic],1);
    ytyt[ic] = (TH2F**) ehelp[ic]->buildNChargeTypes("YtYt");
    // I don't think this sort of normalization is correct anymore.
    for(int j=0;j<4;j++){
        float dx = ytyt[ic][j]->GetXaxis()->GetBinWidth(1);
        float dy = ytyt[ic][j]->GetYaxis()->GetBinWidth(1);
        float sf2 = sqrt(dx*dy);
        ytyt[ic][j]->Scale(1.0/sf2);
    }
    ptdedp[ic] = (TH2F**) ehelp[ic]->buildPtChargeTypes("DEtaDPhi",0,1);
    dedp[ic] = (TH2F**) ehelp[ic]->buildNChargeTypes("DEtaDPhi");
    // I think this is the way we get the PhiPhi histograms.
    phiphi[ic] = (TH2F**) ehelp[ic]->buildNChargeTypes("PhiPhi");
}
}


// Now make some plots, one per centrality.
char *chargeCombo[] = {"LS", "US", "CD", "CI"};
char buffer[1024];
int icharge = 0;
{
      for (int ic=0;ic<nCent;ic++) {
        c1->cd(ic+1);
        gPad->SetPhi();
        gPad->SetTheta();
        phiphi[ic][icharge]->Draw("surf1");
      }
}



{
    for (int i=0;i<nCent;i++) {
        ytyt[i][2]->SetMaximum(0.02);
        ytyt[i][3]->SetMaximum(0.04);
    }
}


int icharge = 1;
{
for (int ic=0;ic<nCent;ic++) {
    c1->cd(ic+1);
    gPad->SetPhi(-70);
    gPad->SetTheta(45);
    ytyt[ic][icharge]->Draw("surf1");
}
}



// Write histograms out in ascii format. (Useful for moving data to
// matlab for example.)
{
for (int ic=0;ic<nCent;ic++) {
    for (int ih=0;ih<4;ih++) {
        sprintf(fileName,"%s/txt/asciiYtYtData%i_%i.txt",dir,ic,ih);
        ehelp[ic][dumpEm[id]]->writeAscii( ytyt[ic], ih, fileName, 0 );
        sprintf(fileName,"%s/txt/asciidedpData%i_%i.txt",dir,ic,ih);
        ehelp[ic][dumpEm[id]]->writeAscii( dedp[ic], ih, fileName, 0 );
        sprintf(fileName,"%s/txt/asciiptdedpData%i_%i.txt",dir,ic,ih);
        ehelp[ic][dumpEm[id]]->writeAscii( ptdedp[ic], ih, fileName, 0 );
    }
}
}
