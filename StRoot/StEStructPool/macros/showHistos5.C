// This is an example macro reading the \Delta\rho/sqrt(\rho) files and
// putting them into histogram arrays. This makes it easier to plot
// specific histograms on a local computer.

// This particular file grabs a subset of the pid histograms.

TFile *data = new TFile("CuCu200GeV_histos.root");

const char* pidName[] = {"all", "pipi", "piK", "pip", "KK", "KP", "pp", "oo"};
const char* chargeName[] = {"_LS_", "_US_", "_CD_", "_CI_"};
const char* chargeType[] = {"_PP_", "_PM_", "_MP_", "_MM_"};

int nCent = 6;
TH2D *dedp[nCent][8][4];
TH2D *dedpC[nCent][8][4];
TH2D *ptdedp[nCent][8][4];
TH2D *ptdedpC[nCent][8][4];
TH2D *ytyt[nCent][8][4];
TH2D *ytytC[nCent][8][4];
TH2D *etaeta[nCent][8][4];
TH2D *etaetaC[nCent][8][4];
TH2D *ptetaeta[nCent][8][4];
TH2D *ptetaetaC[nCent][8][4];
TH2D *phiphi[nCent][8][4];
TH2D *phiphiC[nCent][8][4];
TH2D *ptphiphi[nCent][8][4];
TH2D *ptphiphiC[nCent][8][4];

{
  for (int ipid=0;ipid<8;ipid++) {
     for (int ic=0;ic<nCent;ic++) {
        for (int icharge=0;icharge<4;icharge++) {
           TString name(pidName[ipid]);
           name += "_NDEtaDPhi"; name += chargeName[icharge];  name += ic;
           dedp[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(pidName[ipid]);
           name += "_PtDEtaDPhi"; name += chargeName[icharge];  name += ic;
           ptdedp[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(pidName[ipid]);
           name += "_YtYt"; name += chargeName[icharge];  name += ic;
           ytyt[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());

           TString name(pidName[ipid]);
           name += "_NDEtaDPhi"; name += chargeType[icharge];  name += ic;
           dedpC[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(pidName[ipid]);
           name += "_PtDEtaDPhi"; name += chargeType[icharge];  name += ic;
           ptdedpC[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(pidName[ipid]);
           name += "_YtYt"; name += chargeType[icharge];  name += ic;
           ytytC[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());


           TString name(pidName[ipid]);
           name += "_NEtaEta"; name += chargeName[icharge];  name += ic;
           etaeta[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(pidName[ipid]);
           name += "_PtEtaEta"; name += chargeName[icharge];  name += ic;
           ptetaeta[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());

           TString name(pidName[ipid]);
           name += "_NEtaEta"; name += chargeType[icharge];  name += ic;
           etaetaC[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(pidName[ipid]);
           name += "_PtEtaEta"; name += chargeType[icharge];  name += ic;
           ptetaetaC[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());


           TString name(pidName[ipid]);
           name += "_NPhiPhi"; name += chargeName[icharge];  name += ic;
           phiphi[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(pidName[ipid]);
           name += "_PtPhiPhi"; name += chargeName[icharge];  name += ic;
           ptphiphi[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());

           TString name(pidName[ipid]);
           name += "_NPhiPhi"; name += chargeType[icharge];  name += ic;
           phiphiC[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(pidName[ipid]);
           name += "_PtPhiPhi"; name += chargeType[icharge];  name += ic;
           ptphiphiC[ic][ipid][icharge] = (TH2D *) gDirectory->Get(name.Data());

        }
     }
  }
}

TCanvas* c1=new TCanvas("c1");
gStyle->SetPalette(1);  // set up the colors
c1->Clear();

c1->SetWindowSize(750,500);
c1->Divide(3,2);

gStyle->SetOptTitle(0);
gStyle->SetOptStat(0);


int ipid = 6;
int icharge = 1;
{
  for (int ic=0;ic<nCent;ic++) {
    c1->cd(ic+1);
    gPad->SetPhi(30);
    gPad->SetTheta(50);
    ytyt[ic][ipid][icharge]->Draw("surf1");
  }
}


// Minimize e+e- peak in all
int ipid = 0;
{
  for (int ic=0;ic<nCent;ic++) {
    dedp[ic][ipid][1]->SetBinContent(13,7,0);
    dedp[ic][ipid][2]->SetBinContent(13,7,0);
    dedp[ic][ipid][3]->SetBinContent(13,7,0);
  }
}

c1->Clear();
c1->SetWindowSize(350,350);
gStyle->SetOptTitle();
gStyle->SetTitleBorderSize(0)

char label[1024];
const char* texPidName[]={"all","#pi#pi","#piK","#piP","KK","KP","PP","noPID",};
const char* pidName[]={"all","pipi","piK","piP","KK","KP","PP","noPID",};
const char* chargeName[] = {"LS", "US", "CD", "CI"};
const char* centName[] = {"0-17", "17-33", "33-50", "50-67", "67-83", "83-100"};
char fileName[1024]
{
  for (int ipid=0;ipid<8;ipid++) {
      for (int icharge=0;icharge<4;icharge++) {
          for (int ic=0;ic<nCent;ic++) {
              sprintf(label,"P: %s, %s, multiplicity %s",texPidName[ipid],chargeName[icharge],centName[ic]);
              ptdedp[ic][ipid][icharge]->SetTitle(label);
              TAxis *x = ptdedp[ic][ipid][icharge]->GetXaxis();
              TAxis *y = ptdedp[ic][ipid][icharge]->GetYaxis();
              x->SetTitleSize(0.07);
              x->SetTitleColor(1);
              x->SetTitleOffset(1.0);
              x->SetNdivisions(505);
              x->SetLabelSize(0.05);
              x->SetTitle("#eta_{#Delta}");
              y->SetTitleSize(0.07);
              y->SetTitleOffset(1.0);
              y->SetTitleColor(1);
              y->SetNdivisions(505);
              y->SetLabelSize(0.05);
              y->SetTitle("#phi_{#Delta}");
              gPad->SetPhi(30);
              gPad->SetTheta(50);
              ptdedp[ic][ipid][icharge]->Draw("surf1");
              sprintf(fileName,"auto_P_%s-%s_%s.gif",pidName[ipid],chargeName[icharge],centName[ic]);
              c1->Print(fileName);
          }
      }
  }
}


char textFileName[1024]
FILE *out;
{
  for (int ipid=0;ipid<8;ipid++) {
      for (int icharge=0;icharge<4;icharge++) {
          for (int ic=0;ic<nCent;ic++) {
              sprintf( textFileName, "auto_N_%s-%s_%s.txt",pidName[ipid],chargeName[icharge],centName[ic]);
              out = fopen(textFileName,"w");
              for (int ix=1;ix<dedp[ic][ipid][icharge]->GetNbinsX();ix++) {
                  for (int iy=1;iy<dedp[ic][ipid][icharge]->GetNbinsY();iy++) {
                      fprintf(out,"%i %i %f\n",ix,iy,dedp[ic][ipid][icharge]->GetBinContent(ix,iy));
                  }
              }
              fclose(out);
          }
      }
  }
}


