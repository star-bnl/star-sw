
TCanvas* c1=new TCanvas("c1");
.L makePlots.C
.L setupPalette.C
setupPalette();  // set up the colors
c1->Clear();

c1->SetWindowSize(750,750);
c1->Divide(3,3);

gStyle->SetOptTitle(0);
gStyle->SetOptStat(0);

.x load2ptLibs.C();
gSystem->Load("StEStructPoolSupport.so");

int nCent = 7;
TFile *tf[nCent][8];
StEStructSupport *ehelp[nCent][8];
TH2F **ptdedp[nCent][8];
TH2F **ptdedpC[nCent][8];
TH2F **ytyt[nCent][8];
TH2F **dedp[nCent][8];
TH2F **dedpC[nCent][8];
TH2F **ytytC[nCent][8];

const char* oname[]={"all","pi_pi","pi_K","pi_p","K_K","K_p","p_p","o_o",};
char fileName[1024];
char *dir = "/common/star/stardata/estruct/prindle/Data/cucu62/2005/2ptpid/noSymmCode/data";
//char *dir = "/star/data01/pwg/estruct/prindle/Data/pp200/sumPP";
//char *dir = "/auto/pdsfdv34/estruct/prindle/Data/auau62/2004/MinBias/2pt/data/combineCents";
//char *dir = "/auto/pdsfdv34/estruct/prindle/Data/auau200/2001/MinBiasVertex/2pt/data";
{
  for (int ic=0;ic<nCent;ic++) {
    for (int it=0;it<8;it++) {
      sprintf(fileName,"%s/Data%i%s.root",dir,ic,oname[it]);
      tf[ic][it]      = new TFile(fileName);
      ehelp[ic][it]   = new StEStructSupport(tf[ic][it],0);
      ptdedp[ic][it]  = (TH2F**) ehelp[ic][it]->buildPtChargeTypes("DEtaDPhi",0,1);
      dedp[ic][it]    = (TH2F**) ehelp[ic][it]->buildNChargeTypes("DEtaDPhi");
      ytyt[ic][it]    = (TH2F**) ehelp[ic][it]->buildNChargeTypes("YtYt");
      ptdedpC[ic][it] = (TH2F**) ehelp[ic][it]->buildPtCommon("DEtaDPhi",0,1);
      dedpC[ic][it]   = (TH2F**) ehelp[ic][it]->buildNCommon("DEtaDPhi");
      ytytC[ic][it]   = (TH2F**) ehelp[ic][it]->buildNCommon("YtYt");
    }
  }
}

// The C (as in dedpC) is for common. 0 -> ++, 1 -> -+, 2 -> +-. 3 -> --.
//   The +- is only populated when we have different particle types.
// Without the C we have 0 -> LS, 1 -> US, 2-> CD, 3 -> CI
int ipid = 1;
int icharge = 0;
{
  for (int ic=0;ic<nCent;ic++) {
    c1->cd(ic+1);
    gPad->SetPhi(30);
    gPad->SetTheta(50);
    dedp[ic][ipid][icharge]->Draw("surf1");
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


