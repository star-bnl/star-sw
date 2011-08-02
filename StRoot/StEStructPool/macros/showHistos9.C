// This is an example macro reading the \Delta\rho/sqrt(\rho) files and
// putting them into histogram arrays. This makes it easier to plot
// specific histograms on a local computer.

// This particular file grabs a subset of the pid histograms.

TFile *data = new TFile("/common/star/stardata/estruct/prindle/Hijing/cucu62/mode3_QuenchOff/data/DeltaRhoByRho_1.root");

const char* oname[]={"all","below4","below3","below2","below1","above1","above2","above3","above4","one","onetwo","onethree","two","twothree","three"};
const char* chargeName[] = {"_LS_", "_US_", "_CD_", "_CI_"};
const char* chargeType[] = {"_PP_", "_PM_", "_MP_", "_MM_"};

int nCent = 3;
TH2D *dedp[nCent][12][4];
TH2D *dedpC[nCent][12][4];
TH2D *ptdedp[nCent][12][4];
TH2D *ptdedpC[nCent][12][4];
TH2D *ytyt[nCent][12][4];
TH2D *ytytC[nCent][12][4];
TH2D *etaeta[nCent][12][4];
TH2D *etaetaC[nCent][12][4];
TH2D *ptetaeta[nCent][12][4];
TH2D *ptetaetaC[nCent][12][4];
TH2D *phiphi[nCent][12][4];
TH2D *phiphiC[nCent][12][4];
TH2D *ptphiphi[nCent][12][4];
TH2D *ptphiphiC[nCent][12][4];

{
  for (int ibin=0;ibin<12;ibin++) {
     for (int ic=0;ic<nCent;ic++) {
        for (int icharge=0;icharge<4;icharge++) {
           TString name(binName[ibin]);
           name += "_NDEtaDPhi"; name += chargeName[icharge];  name += ic;
           dedp[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(binName[ibin]);
           name += "_PtDEtaDPhi"; name += chargeName[icharge];  name += ic;
           ptdedp[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(binName[ibin]);
           name += "_YtYt"; name += chargeName[icharge];  name += ic;
           ytyt[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());

           TString name(binName[ibin]);
           name += "_NDEtaDPhi"; name += chargeType[icharge];  name += ic;
           dedpC[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(binName[ibin]);
           name += "_PtDEtaDPhi"; name += chargeType[icharge];  name += ic;
           ptdedpC[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(binName[ibin]);
           name += "_YtYt"; name += chargeType[icharge];  name += ic;
           ytytC[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());


           TString name(binName[ibin]);
           name += "_NEtaEta"; name += chargeName[icharge];  name += ic;
           etaeta[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(binName[ibin]);
           name += "_PtEtaEta"; name += chargeName[icharge];  name += ic;
           ptetaeta[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());

           TString name(binName[ibin]);
           name += "_NEtaEta"; name += chargeType[icharge];  name += ic;
           etaetaC[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(binName[ibin]);
           name += "_PtEtaEta"; name += chargeType[icharge];  name += ic;
           ptetaetaC[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());


           TString name(binName[ibin]);
           name += "_NPhiPhi"; name += chargeName[icharge];  name += ic;
           phiphi[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(binName[ibin]);
           name += "_PtPhiPhi"; name += chargeName[icharge];  name += ic;
           ptphiphi[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());

           TString name(binName[ibin]);
           name += "_NPhiPhi"; name += chargeType[icharge];  name += ic;
           phiphiC[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());
           TString name(binName[ibin]);
           name += "_PtPhiPhi"; name += chargeType[icharge];  name += ic;
           ptphiphiC[ic][ibin][icharge] = (TH2D *) gDirectory->Get(name.Data());

        }
     }
  }
}

TCanvas* c1=new TCanvas("c1");
gStyle->SetPalette(1);  // set up the colors
c1->Clear();

c1->SetWindowSize(1000,750);
c1->Divide(4,3);

gStyle->SetOptTitle(0);
gStyle->SetOptStat(0);


int ic = 0;
int icharge = 3;
{
  for (int ibin=0;ibin<12;ibin++) {
    c1->cd(ibin+1);
    gPad->SetPhi(30);
    gPad->SetTheta(50);
    ptphiphi[ic][ibin][icharge]->Draw("surf1");
  }
}



c1->Clear();
c1->SetWindowSize(350,350);
gStyle->SetOptTitle();
gStyle->SetTitleBorderSize(0)

char label[1024];
const char* binName[]={"all","awayside","nearside","soft","softAS","softNS","neck","neckAS","neckNS","hard","hardAS","hardNS"};
const char* binBinName[]={"all","awayside","nearside","soft","softAS","softNS","neck","neckAS","neckNS","hard","hardAS","hardNS"};
const char* chargeName[] = {"LS", "US", "CD", "CI"};
const char* centName[] = {"0-17", "17-33", "33-50", "50-67", "67-83", "83-100"};
char fileName[1024]
{
  for (int ibin=0;ibin<12;ibin++) {
      for (int icharge=0;icharge<4;icharge++) {
          for (int ic=0;ic<nCent;ic++) {
              sprintf(label,"P: %s, %s, multiplicity %s",texBinName[ibin],chargeName[icharge],centName[ic]);
              ptdedp[ic][ibin][icharge]->SetTitle(label);
              TAxis *x = ptdedp[ic][ibin][icharge]->GetXaxis();
              TAxis *y = ptdedp[ic][ibin][icharge]->GetYaxis();
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
              ptdedp[ic][ibin][icharge]->Draw("surf1");
              sprintf(fileName,"auto_P_%s-%s_%s.gif",binName[ibin],chargeName[icharge],centName[ic]);
              c1->Print(fileName);
          }
      }
  }
}


char textFileName[1024]
FILE *out;
{
  for (int ibin=0;ibin<12;ibin++) {
      for (int icharge=0;icharge<4;icharge++) {
          for (int ic=0;ic<nCent;ic++) {
              sprintf( textFileName, "auto_N_%s-%s_%s.txt",binName[ibin],chargeName[icharge],centName[ic]);
              out = fopen(textFileName,"w");
              for (int ix=1;ix<dedp[ic][ibin][icharge]->GetNbinsX();ix++) {
                  for (int iy=1;iy<dedp[ic][ibin][icharge]->GetNbinsY();iy++) {
                      fprintf(out,"%i %i %f\n",ix,iy,dedp[ic][ibin][icharge]->GetBinContent(ix,iy));
                  }
              }
              fclose(out);
          }
      }
  }
}


