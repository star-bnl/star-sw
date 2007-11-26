
TCanvas* c1=new TCanvas("c1");
.L makePlots.C
.L setupPalette.C
setupPalette();  // set up the colors
c1->Clear();

c1->SetWindowSize(750,500);
c1->Divide(3,2);

gStyle->SetOptTitle(0);
gStyle->SetOptStat(0);

.x load2ptLibs.C();
gSystem->Load("StEStructPoolSupport.so");

int nCent = 6;
TFile *tf[nCent][8];
StEStructSupport *ehelp[nCent][8];
TH2F **ptdedp[nCent][8];
TH2F **ptdedpC[nCent][8];
TH2F **dedp[nCent][8];
TH2F **dedpC[nCent][8];
TH2F **ytyt[nCent][8];
TH2F **ytytC[nCent][8];
TH2F **ptetaeta[nCent][8];
TH2F **ptetaetaC[nCent][8];
TH2F **etaeta[nCent][8];
TH2F **etaetaC[nCent][8];
TH2F **ptphiphi[nCent][8];
TH2F **ptphiphiC[nCent][8];
TH2F **phiphi[nCent][8];
TH2F **phiphiC[nCent][8];

const char* oname[]={"all","pi_pi","pi_K","pi_p","K_K","K_p","p_p","o_o"};
char fileName[1024];
//char *dir = "/common/star/stardata/estruct/prindle/Data/auau200/2004/MinBias/2pt/zBinning/data";
//char *dir = "/common/star/stardata/estruct/prindle/Pythia/pp200GeV/2pt/pid/data";
//char *dir = "/common/star/stardata/estruct/prindle/Hijing/cucu200/QuenchOff/data";
//char *dir = "/common/star/stardata/estruct/prindle/Hijing/cucu200/JetOff/data";
//char *dir = "/star/data01/pwg/estruct/prindle/Data/cucu200/2007ib/cuProdutionMinBias_test3/data";
//char *dir = "/auto/pdsfdv34/estruct/prindle/Data/auau62/2004/MinBias/2pt/data/combineCents";
//char *dir = "/auto/pdsfdv34/estruct/prindle/Data/auau200/2001/MinBiasVertex/2pt/data";
//char *dir = "/auto/pdsfdv34/estruct/prindle/Data/auau200/2004/MinBias/2pt/zBinning/data";
char *dir = "/star/data01/pwg/estruct/prindle/Data/cucu200/2007ib/cuProdutionMinBias/data";
//char *dir = "/star/data01/pwg/estruct/prindle/Data/cucu62/2007ib/cuProductionMinBias/data";
//char *dir = "/common/star/stardata/estruct/prindle/Therminator/standardStats2/data";
//char *dir = "/common/star/stardata/estruct/prindle/Therminator/kaons/data";
//char *dir = "/common/star/stardata/estruct/prindle/Therminator/Delta_phi/data";
//char *dir = "/common/star/stardata/estruct/prindle/Therminator/Lambda_rho/data";
{
  for (int ic=0;ic<nCent;ic++) {
    for (int ipid=0;ipid<8;ipid++) {
      sprintf(fileName,"%s/Data%i%s.root",dir,ic,oname[ipid]);
      tf[ic][ipid]        = new TFile(fileName);
      ehelp[ic][ipid]     = new StEStructSupport(tf[ic][ipid],0);
      ehelp[ic][ipid]->mapplyDEtaFix = false;
      ehelp[ic][ipid]->mPairNormalization = false;
      if ((0 ==ipid) || (1 == ipid) || (4 == ipid) || (6 == ipid) || (7 == ipid)) {
          ehelp[ic][ipid]->mIdenticalPair = true;
      } else {
          ehelp[ic][ipid]->mIdenticalPair = false;
      }
      int subtract = 1;
      ptdedpC[ic][ipid]   = (TH2F**) ehelp[ic][ipid]->buildPtCommon("DEtaDPhi",2,subtract);
      ptetaetaC[ic][ipid] = (TH2F**) ehelp[ic][ipid]->buildPtCommon("EtaEta",2,subtract);
      ptphiphiC[ic][ipid] = (TH2F**) ehelp[ic][ipid]->buildPtCommon("PhiPhi",2,subtract);

      ptdedp[ic][ipid]   = (TH2F**) ehelp[ic][ipid]->buildPtChargeTypes("DEtaDPhi",2,subtract);
      ptetaeta[ic][ipid] = (TH2F**) ehelp[ic][ipid]->buildPtChargeTypes("EtaEta",2,subtract);
      ptphiphi[ic][ipid] = (TH2F**) ehelp[ic][ipid]->buildPtChargeTypes("PhiPhi",2,subtract);

      dedpC[ic][ipid]     = (TH2F**) ehelp[ic][ipid]->buildCommon("DEtaDPhi",2);
      ytytC[ic][ipid]     = (TH2F**) ehelp[ic][ipid]->buildCommon("YtYt",3);
      etaetaC[ic][ipid]   = (TH2F**) ehelp[ic][ipid]->buildCommon("EtaEta",2);
      phiphiC[ic][ipid]   = (TH2F**) ehelp[ic][ipid]->buildCommon("PhiPhi",2);

      dedp[ic][ipid]     = (TH2F**) ehelp[ic][ipid]->buildChargeTypes("DEtaDPhi",2);
      ytyt[ic][ipid]     = (TH2F**) ehelp[ic][ipid]->buildChargeTypes("YtYt",3);
      etaeta[ic][ipid]   = (TH2F**) ehelp[ic][ipid]->buildChargeTypes("EtaEta",2);
      phiphi[ic][ipid]   = (TH2F**) ehelp[ic][ipid]->buildChargeTypes("PhiPhi",2);
    }
  }
}

// The C (as in dedpC) is for common. 0 -> ++, 1 -> -+, 2 -> +-. 3 -> --.
//   The -+ is only populated when we have different particle types.
// Without the C we have 0 -> LS, 1 -> US, 2-> CD, 3 -> CI
int ipid = 1;
int icharge = 0;
{
  for (int ic=0;ic<nCent;ic++) {
    c1->cd(ic+1);
    gPad->SetPhi(30);
    gPad->SetTheta(50);
    ytyt[ic][ipid][icharge]->Draw("surf1");
  }
}

int ic = 0;
int icharge = 1;
{
  for (int ipid=1;ipid<7;ipid++) {
    c1->cd(ipid);
    gPad->SetPhi(30);
    gPad->SetTheta(50);
    dedp[ic][ipid][icharge]->Draw("surf1");
  }
}

const char* pidName[] = {"all", "pipi", "piK", "pip", "KK", "KP", "pp", "oo"};
const char* chargeName[] = {"_LS_", "_US_", "_CD_", "_CI_"};
const char* chargeType[] = {"_PP_", "_PM_", "_MP_", "_MM_"};
TFile *out = new TFile("CuCu200GeV_histos.root","CREATE");
//TFile *out = new TFile("therminatorLambda_rho_histos.root","CREATE");
{
  for (int ipid=0;ipid<8;ipid++) {
     for (int icharge=0;icharge<4;icharge++) {
        for (int ic=0;ic<nCent;ic++) {
           TString name(pidName[ipid]);
           name += "_NDEtaDPhi"; name += chargeName[icharge];  name += ic;
           dedp[ic][ipid][icharge]->SetName(name.Data());
           dedp[ic][ipid][icharge]->SetTitle(name.Data());
           dedp[ic][ipid][icharge]->Write();
           TString name(pidName[ipid]);
           name += "_PtDEtaDPhi"; name += chargeName[icharge];  name += ic;
           ptdedp[ic][ipid][icharge]->SetName(name.Data());
           ptdedp[ic][ipid][icharge]->SetTitle(name.Data());
           ptdedp[ic][ipid][icharge]->Write();
           TString name(pidName[ipid]);
           name += "_YtYt"; name += chargeName[icharge];  name += ic;
           ytyt[ic][ipid][icharge]->SetName(name.Data());
           ytyt[ic][ipid][icharge]->SetTitle(name.Data());
           ytyt[ic][ipid][icharge]->Write();

           TString name(pidName[ipid]);
           name += "_NDEtaDPhi"; name += chargeType[icharge];  name += ic;
           dedpC[ic][ipid][icharge]->SetName(name.Data());
           dedpC[ic][ipid][icharge]->SetTitle(name.Data());
           dedpC[ic][ipid][icharge]->Write();
           TString name(pidName[ipid]);
           name += "_PtDEtaDPhi"; name += chargeType[icharge];  name += ic;
           ptdedpC[ic][ipid][icharge]->SetName(name.Data());
           ptdedpC[ic][ipid][icharge]->SetTitle(name.Data());
           ptdedpC[ic][ipid][icharge]->Write();
           TString name(pidName[ipid]);
           name += "_YtYt"; name += chargeType[icharge];  name += ic;
           ytytC[ic][ipid][icharge]->SetName(name.Data());
           ytytC[ic][ipid][icharge]->SetTitle(name.Data());
           ytytC[ic][ipid][icharge]->Write();


           TString name(pidName[ipid]);
           name += "_NEtaEta"; name += chargeName[icharge];  name += ic;
           etaeta[ic][ipid][icharge]->SetName(name.Data());
           etaeta[ic][ipid][icharge]->SetTitle(name.Data());
           etaeta[ic][ipid][icharge]->Write();
           TString name(pidName[ipid]);
           name += "_PtEtaEta"; name += chargeName[icharge];  name += ic;
           ptetaeta[ic][ipid][icharge]->SetName(name.Data());
           ptetaeta[ic][ipid][icharge]->SetTitle(name.Data());
           ptetaeta[ic][ipid][icharge]->Write();

           TString name(pidName[ipid]);
           name += "_NEtaEta"; name += chargeType[icharge];  name += ic;
           etaetaC[ic][ipid][icharge]->SetName(name.Data());
           etaetaC[ic][ipid][icharge]->SetTitle(name.Data());
           etaetaC[ic][ipid][icharge]->Write();
           TString name(pidName[ipid]);
           name += "_PtEtaEta"; name += chargeType[icharge];  name += ic;
           ptetaetaC[ic][ipid][icharge]->SetName(name.Data());
           ptetaetaC[ic][ipid][icharge]->SetTitle(name.Data());
           ptetaetaC[ic][ipid][icharge]->Write();


           TString name(pidName[ipid]);
           name += "_NPhiPhi"; name += chargeName[icharge];  name += ic;
           phiphi[ic][ipid][icharge]->SetName(name.Data());
           phiphi[ic][ipid][icharge]->SetTitle(name.Data());
           phiphi[ic][ipid][icharge]->Write();
           TString name(pidName[ipid]);
           name += "_PtPhiPhi"; name += chargeName[icharge];  name += ic;
           ptphiphi[ic][ipid][icharge]->SetName(name.Data());
           ptphiphi[ic][ipid][icharge]->SetTitle(name.Data());
           ptphiphi[ic][ipid][icharge]->Write();

           TString name(pidName[ipid]);
           name += "_NPhiPhi"; name += chargeType[icharge];  name += ic;
           phiphiC[ic][ipid][icharge]->SetName(name.Data());
           phiphiC[ic][ipid][icharge]->SetTitle(name.Data());
           phiphiC[ic][ipid][icharge]->Write();
           TString name(pidName[ipid]);
           name += "_PtPhiPhi"; name += chargeType[icharge];  name += ic;
           ptphiphiC[ic][ipid][icharge]->SetName(name.Data());
           ptphiphiC[ic][ipid][icharge]->SetTitle(name.Data());
           ptphiphiC[ic][ipid][icharge]->Write();

        }
     }
  }
}
out->Close();
delete out;

c1->Clear();
c1->SetWindowSize(350,350);
gStyle->SetOptTitle();
gStyle->SetTitleBorderSize(0)

char label[1024];
const char* texPidName[]={"all","#pi#pi","#piK","#piP","KK","KP","PP","noPID",};
const char* pidName[]={"all","pipi","piK","piP","KK","KP","PP","noPID",};
const char* chargeName[] = {"LS", "US", "CD", "CI"};
const char* centName[] = {"2-4", "5-7", "8-10", "11-13", "14-25"};
const char* centName[] = {"0-5", "5-10", "10-20", "20-30", "30-40", "40-50",  "50-60", "60-70", "70-80", "80-90", "90-100"};
const char* centName[] = {"90-100" "80-90", "70-80", "60-70",  "50-60", "40-50", "30-40", "20-30", "10-20", "5-10", "0-5"};
const char* centName[] = {"0-30", "30-60", "60-100"};
{
  for (int ipid=0;ipid<8;ipid++) {
      for (int icharge=0;icharge<4;icharge++) {
          for (int ic=0;ic<nCent;ic++) {
              sprintf(label,"N: %s, %s, multiplicity %s",texPidName[ipid],chargeName[icharge],centName[ic]);
              dedp[ic][ipid][icharge]->SetTitle(label);
              TAxis *x = dedp[ic][ipid][icharge]->GetXaxis();
              TAxis *y = dedp[ic][ipid][icharge]->GetYaxis();
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
              dedp[ic][ipid][icharge]->Draw("surf1");
              sprintf(fileName,"auto_N_%s-%s_%s.gif",pidName[ipid],chargeName[icharge],centName[ic]);
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


