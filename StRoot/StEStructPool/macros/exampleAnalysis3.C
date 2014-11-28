
TCanvas* c1=new TCanvas("c1");
.L makePlots.C
.L setupPalette.C
setupPalette();  // set up the colors
c1->Clear();

c1->SetWindowSize(500,500);
c1->Divide(2,2);

gStyle->SetOptTitle(0);
gStyle->SetOptStat(0);


gROOT->LoadMacro("load2ptLibs.C");
load2ptLibs();
gSystem->Load("StEStructPoolSupport.so");

int nCent = 3;
TFile *tf[nCent][12];
StEStructSupport *ehelp[nCent][12];
TH2F **ytyt[nCent][12];
TH2F **ptdedp[nCent][12];
TH2F **dedp[nCent][12];

const char* oname[]={"all","awayside","nearside","soft","softAS","softNS","neck","neckAS","neckNS","hard","hardAS","hardNS"};
char fileName[1024];
char *dir = "/common/star/stardata/estruct/prindle/Hijing/auau200/QuenchOff/noSymm_mode3";
char *dir = "/common/star/stardata/estruct/prindle/Data/pp200/pidTest/productionMinBias_year5";
//char *dir = "/star/data01/pwg/estruct/prindle/Data/auau19/2001/2pt/MinBias_FewerBins2";
float sf[2] = {0.98,0.98};
{
for (int ic=0;ic<nCent;ic++) {
    for (int it=0;it<12;it++) {
        sprintf(fileName,"%s/data/Data%i%s.root",dir,ic,oname[it]);
        tf[ic][it] = new TFile(fileName);
        ehelp[ic][it] = new StEStructSupport(tf[ic][it],1);
        ytyt[ic][it] = (TH2F**) ehelp[ic][it]->buildChargeTypes("YtYt",0);
        for(int j=0;j<4;j++){
            float dx = ytyt[ic][it][j]->GetXaxis()->GetBinWidth(1);
            float dy = ytyt[ic][it][j]->GetYaxis()->GetBinWidth(1);
            float sf2 = sqrt(dx*dy);
            ytyt[ic][it][j]->Scale(1.0/sf2);
        }
        ptdedp[ic][it] = (TH2F**) ehelp[ic][it]->buildPtChargeTypes("DEtaDPhi",0,1);
        dedp[ic][it] = (TH2F**) ehelp[ic][it]->buildChargeTypes("DEtaDPhi",0);
    }
}
}

{
    for (int i=0;i<nCent;i++) {
        ytyt[i][2]->SetMaximum(0.02);
        ytyt[i][3]->SetMaximum(0.04);
    }
}


int ipid = 0;
int icharge = 1;
{
for (int ic=0;ic<nCent;ic++) {
    c1->cd(ic+1);
    gPad->SetPhi(-70);
    gPad->SetTheta(45);
    ytyt[ic][ipid][icharge]->Draw("surf1");
}
}

{
const char* oname[]={"all","awayside","nearside","soft","softAS","softNS",
                     "neck","neckAS","neckNS","hard","hardAS","hardNS"};
}
char *chargeCombo[] = {"LS", "US", "CD", "CI"};
char buffer[1024];
int icut = 0;
int icharge = 3;
{
      for (int ic=0;ic<nCent;ic++) {
        c1->cd(ic+1);
        gPad->SetPhi();
        gPad->SetTheta();
        dedp[ic][icut][icharge]->Draw("surf1");
      }
}




// Write histograms out in ascii format.
int dumpEm[] = {0, 3, 9};
{
for (int ic=0;ic<nCent;ic++) {
    for (int id=0;id<3;id++) {
        for (int ih=0;ih<4;ih++) {
            sprintf(fileName,"%s/txt/asciiYtYtData%i%s_%i.txt",dir,ic,oname[dumpEm[id]],ih);
            ehelp[ic][dumpEm[id]]->writeAscii( ytyt[ic][dumpEm[id]], ih, fileName, 0 );
            sprintf(fileName,"%s/txt/asciidedpData%i%s_%i.txt",dir,ic,oname[dumpEm[id]],ih);
            ehelp[ic][dumpEm[id]]->writeAscii( dedp[ic][dumpEm[id]], ih, fileName, 0 );
            sprintf(fileName,"%s/txt/asciiptdedpData%i%s_%i.txt",dir,ic,oname[dumpEm[id]],ih);
            ehelp[ic][dumpEm[id]]->writeAscii( ptdedp[ic][dumpEm[id]], ih, fileName, 0 );
        }
    }
}
}
