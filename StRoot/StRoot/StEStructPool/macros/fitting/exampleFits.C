// Note: I cut and paste  parts of this file into a root session.
//       I consider this and associated files as templates which can be modified
//       to suit particular analyses/fitting.

// I normally run the root session in a directory which has soft links to the root files.
// Output written to a subdirectory fitResults.

//Choose dataset to fit.
char *dataset = "AuAu200GeV";
char *dataset = "AuAu62GeV";
char *dataset = "CuCu200GeV";
char *dataset = "CuCu62GeV";

// Choose cutBin and charge to fit as well as setting number of parameters.
int ibin    =  0;
int icharge =  3;
int nParams = 13;

// Open appropriate pair of root files.
// norm scales from the dN/d\eta observed in the EStruct analysis and the corrected
//   dN/d\eta from the bulk working group.
// fPileup is the fraction of pileup not removed by my pileup cut.
//   In principle this can be centrality dependent, but I don't know how to measure that.
{
    if (!strcmp(dataset,"AuAu200GeV")) {
        int nCent   = 11;
        TFile *data1 = new TFile("AuAu200GeV_11c_pair.root");
        TFile *data2 = new TFile("AuAu200GeV_11c_pair_noPileupCuts.root");
        const char* centName[] = {"90-100", "80-90", "70-80", "60-70", "50-60", "40-50", "30-40", "20-30", "10-20", "5-10", "0-5"};
        double norm[] = {1.235, 1.177, 1.157, 1.167, 1.178, 1.193, 1.230, 1.292, 1.366, 1.379, 1.451};
        // Normalization implied by taking difference between my noPileup analysis and Michael's analysis
        // and minimizing structure. 10% sort of differences.
        double normN[] = {1.23,  1.177, 1.11,  1.13,  1.122, 1.142, 1.18,  1.258, 1.364, 1.5,   1.46};
        double fPileup = 0.25;
        // Next are vaules I used when I first sent data to Lanny.
        // double fPileup[] = {0.0, 0.0, 0.2,  0.3, 0.3, 0.2, 0.1, 0.05, 0.0, 0.0, 0.0};
    } else if (!strcmp(dataset,"AuAu62GeV")) {
        int nCent   = 11;
        TFile *data1 = new TFile("AuAu62GeV_11c_pair.root");
        TFile *data2 = new TFile("AuAu62GeV_11c_pair_noPileupCuts.root");
        const char* centName[] = {"90-100", "80-90", "70-80", "60-70", "50-60", "40-50", "30-40", "20-30", "10-20", "5-10", "0-5"};
        double norm[] = {1.0298, 1.1747, 1.1758, 1.1937, 1.2174, 1.2416, 1.2702, 1.3082, 1.3668, 1.4016, 1.4426};
        double fPileup = 0.25;
    } else if (!strcmp(dataset,"CuCu200GeV")) {
        int nCent   = 10;
        TFile *data1 = new TFile("CuCu200GeV_10c_pair.root");
        TFile *data2 = new TFile("CuCu200GeV_10c_pair_noPileupCuts.root");
        const char* centName[] = {"90-100", "80-90", "70-80", "60-70", "50-60", "40-50", "30-40", "20-30", "10-20", "5-10", "0-5"};
        double fPileup = 0.25;
        double norm[]    = {1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0};
    } else if (!strcmp(dataset,"CuCu62GeV")) {
        int nCent   = 10;
        TFile *data1 = new TFile("CuCu62GeV_10c_pair.root");
        TFile *data2 = new TFile("CuCu62GeV_10c_pair_noPileupCuts.root");
        const char* centName[] = {"90-100", "80-90", "70-80", "60-70", "50-60", "40-50", "30-40", "20-30", "10-20", "5-10", "0-5"};
        double fPileup = 0.25;
        double norm[]    = {1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0};
    }
}

// The details of getting pointers to histograms depends on the cutBin mode.
// Here we assume mode 3.

const char* binName[]    = {"all","soft","neck","hard","softHard"};
const char* chargeName[] = {"LS", "US", "CD", "CI"};
const char* chargeType[] = {"_PP_", "_PM_", "_MP_", "_MM_"};

TH2D *dedpCut[nCent];
TH2D *dedpNoCut[nCent];
TH2D *dedpNoPileup[nCent];
TH2D *dedpPileup[nCent];
TH2D *ptdedp[nCent];

    // Get pointers to histograms.
{
    for (int ic=0;ic<nCent;ic++) {
        data1->cd();
        TString name(binName[ibin]);
        name += "_NDEtaDPhi_"; name += chargeName[icharge];  name += "_";  name += ic;
        dedpCut[ic] = (TH2D *) gDirectory->Get(name.Data());
        dedpCut[ic]->Scale(norm[ic]);
        TString name(binName[ibin]);
        name += "_PtDEtaDPhi_"; name += chargeName[icharge];  name += "_";  name += ic;
        ptdedp[ic] = (TH2D *) gDirectory->Get(name.Data());
        ptdedp[ic]->Scale(norm[ic]);

        data2->cd();
        TString name(binName[ibin]);
        name += "_NDEtaDPhi_"; name += chargeName[icharge];  name += "_";  name += ic;
        dedpNoCut[ic] = (TH2D *) gDirectory->Get(name.Data());
        dedpNoCut[ic]->Scale(norm[ic]);

        dedpNoPileup[ic] = NULL;
        dedpPileup[ic]   = NULL;
    }
}

    // Extrapolate to no pileup
    // Since Cut and noCut are mostly the same events they don't have independent errors.
    // Root propagates errors to noPileup as if they are independent. We need to reset those.
{
    for (int ic=0;ic<nCent;ic++) {
        if (dedpNoPileup[ic]) {
            delete dedpNoPileup[ic];
            dedpNoPileup[ic] = NULL;
        }
        if (dedpPileup[ic]) {
            delete dedpPileup[ic];
            dedpPileup[ic] = NULL;
        }
        dedpNoPileup[ic] = (TH2D *) dedpCut[ic]->Clone();
        dedpPileup[ic] = (TH2D *) dedpCut[ic]->Clone();
        dedpNoPileup[ic]->Reset();
        dedpPileup[ic]->Reset();

        dedpNoPileup[ic]->Add(dedpCut[ic],dedpNoCut[ic],1,-fPileup);
        dedpPileup[ic]->Add(dedpNoCut[ic],dedpCut[ic],1,-1);
        dedpNoPileup[ic]->Scale(1/(1-fPileup));
        dedpPileup[ic]->Scale(1/(1-fPileup));
        for (int ix=1;ix<=dedpNoPileup[ic]->GetNbinsX();ix++) {
            for (int iy=1;iy<=dedpNoPileup[ic]->GetNbinsY();iy++) {
                double err = dedpCut[ic]->GetBinError(ix,iy);
                dedpNoPileup[ic]->SetBinError(ix,iy,err);
                dedpPileup[ic]->SetBinError(ix,iy,err);
            }
        }
    }
}

// For CuCu the bin at (0,0) is screwy, LS histogram is just missing here.
// Set error to large number (or could set it to 0 to remove from fit.
{
    if (!strcmp(dataset,"CuCu200GeV") || !strcmp(dataset,"CuCu62GeV")) {
        for (int ic=0;ic<nCent;ic++) {
            dedpNoPileup[ic]->SetBinError(13,7,1);
            dedpPileup[ic]->SetBinError(13,7,1);
        }
    }
}

//-------------------------------------------------
// Data has been read.
// Create fit functions.
.L fitCos.C++

TAxis *x = dedpCut[3][0]->GetXaxis();
TAxis *y = dedpCut[3][0]->GetYaxis();
int    nx   = x->GetNbins();
double xmin = x->GetXmin();
double xmax = x->GetXmax();
int    ny   = y->GetNbins();
double ymin = y->GetXmin();
double ymax = y->GetXmax();
double pi = 3.1415926;

TF2 *fitFunc[nCent];
{
    char buffer[1024];
    for (int ic=0;ic<nCent;ic++) {
        sprintf(buffer,"fit_bin%i_charge%i_Cent%i",ibin,icharge,ic);
        fitFunc[ic] = new TF2(buffer,fitCos,xmin,xmax,ymin,ymax,nParams);
    }
}

// For covariance matrix.
double matrix[nCent][nParams][nParams];
{
    for (int ic=0;ic<nCent;ic++) {
        for (int ip=0;ip<nParams;ip++) {
            for (int jp=0;jp<nParams;jp++) {
                matrix[ic][ip][jp] = 0;
            }
        }
    }
}

//-------------------------------------------------
// Read parameters from previous fit. Set lastFits to NULL if we don't have previous
//   fit (or the format for the previous fit is different.)
//char *lastFits = NULL;

char lastFits[1024];
sprintf(lastFits,"fitResults/cosEta_fitParams_%s_%ic.txt",dataset,nCent);
.L readParams.C++
{
    if (lastFits) {
        float lastParams[nCent][nParams], lastErrors[nCent][nParams];
        readParams(lastFits,&lastParams[0][0],&lastErrors[0][0],nCent,nParams);
    }
}

// Set initial values and limits on parameters.
{
    char *parName[] = {"offset", "cos(phi)", "cos(2phi)", "jetAmp", "jetSigX", "jetSigY", "expAmp", "expX", "expY", "cos(etaD)", "cos(2etaD)", "cos(3etaD)", "cos(4etaD)"};
    double parVal[] = {0.0, -0.03, 0.03, 0.15,0.6,0.6, 0.1,0.2,0.2,  -0.1, 0.05, -0.02, 0.01};
    for (int ic=0;ic<nCent;ic++) {
        for (int ip=0;ip<nParams;ip++) {
            fitFunc[ic]->SetParName(ip,parName[ip]);
            if (lastFits) {
                fitFunc[ic]->SetParameter(ip,lastParams[ic][ip]);
            } else {
                fitFunc[ic]->SetParameter(ip,parVal[ip]);
            }
        }
    }
}
{
    for (int ic=0;ic<nCent;ic++) {
        fitFunc[ic]->SetParLimits( 0,-2.0,2.0);
        fitFunc[ic]->SetParLimits( 1,-1.6,1.0);
        fitFunc[ic]->SetParLimits( 2,-1.0,1.0);
        fitFunc[ic]->SetParLimits( 3,0.0,2.0);
        fitFunc[ic]->SetParLimits( 4,0.1,5.0);
        fitFunc[ic]->SetParLimits( 5,0.1,5.0);
        fitFunc[ic]->SetParLimits( 6,0.0,25.0);
        fitFunc[ic]->SetParLimits( 7,1e-9,0.95);
        fitFunc[ic]->SetParLimits( 8,1e-9,0.95);
        fitFunc[ic]->SetParLimits( 9,-1.0,1.0);
        fitFunc[ic]->SetParLimits(10,-1.0,1.0);
        fitFunc[ic]->SetParLimits(11,-1.0,1.0);
        fitFunc[ic]->SetParLimits(12,-1.0,1.0);
    }
}

// Initial fits. No MINOS errors so fit should be fairly quick.
{
    for (int ic=0;ic<nCent;ic++) {
        reject = kTRUE;
        printf(">>Starting fit to centrality %i, bin %i, charge %i\n",ic,ibin,icharge);
        sprintf(buffer,"fit_bin%i_charge%i_Cent%i",ibin,icharge,ic);
        dedpNoPileup[ic]->Fit(buffer,"O");
    }
}

// Free selected paramters (may make a difference in errors).
{
    for (int ic=0;ic<nCent;ic++) {
        fitFunc[ic]->ReleaseParameter( 0);
        fitFunc[ic]->ReleaseParameter( 1);
        fitFunc[ic]->ReleaseParameter( 2);
        fitFunc[ic]->ReleaseParameter( 3);
        fitFunc[ic]->ReleaseParameter( 4);
        fitFunc[ic]->ReleaseParameter( 5);
        fitFunc[ic]->ReleaseParameter( 6);
        fitFunc[ic]->ReleaseParameter( 7);
        fitFunc[ic]->ReleaseParameter( 8);
        fitFunc[ic]->ReleaseParameter( 9);
        fitFunc[ic]->ReleaseParameter(10);
    }
}


// Redo fit asking for MINOS errors.
{
    for (int ic=0;ic<nCent;ic++) {
        reject = kTRUE;
        printf(">>Starting fit to centrality %i, bin %i, charge %i\n",ic,ibin,icharge);
        sprintf(buffer,"fit_bin%i_charge%i_Cent%i",ibin,icharge,ic);
        dedpNoPileup[ic]->Fit(buffer,"EO");
        gMinuit->mnemat(&matrix[ic][0][0],nParams);
    }
}

//-------------------------------------------------
// Extract fit parameters, write to file.
reject = kFALSE;
double param[nCent][nParams];
double error[nCent][nParams];
double chi2[nCent];
{
    for (int ic=0;ic<nCent;ic++) {
        fitFunc[ic]->GetParameters(param[ic]);
        chi2[ic] = fitFunc[ic]->GetChisquare();
        for (int ip=0;ip<nParams;ip++) {
            error[ic][ip] = fitFunc[ic]->GetParError(ip);
        }
    }
}
char *paramNames[] = {"   chi2   ", "  offset", "      cos(phi)", "    cos(2phi)", "   jetAmp", "      jetSigX", "     jetSigY", "      expAmp", "     expSigX", "     expSigY", "     cos(etaD)", "  cos(2etaD)", "     cos(3etaD)", "  cos(4etaD)"};
.L writeParams.C++
char newFits[1024];
sprintf(newFits,"fitResults/cosEta_fitParams_%s_%ic.txt",dataset,nCent);
FILE *fPar = fopen(newFits,"w");
writeParams(fPar,ibin,icharge,paramNames,chi2,&param[0][0],&error[0][0],nCent,nParams);
fclose(fPar);

// Write parameters in format appropriate for creating root plots.
char *pNames[] = {"offset", "cosPhi", "cos2Phi", "MjAmp", "MjEta", "MjPhi", "expAmp", "expEta", "expPhi", "cos_etaD", "cos_2etaD", "cos_3etaD", "cos_4etaD"};
.L writePlotCode.C++
char plotCode[1024];
sprintf(plotCode,"fitResults/cos_plotCode_%s_%ic.C",dataset,nCent);
char tag[1024];
sprintf(tag,"cos%s_%i_",dataset,nCent);
writePlotCode(plotCode,tag,pNames,chi2,&param[0][0],&error[0][0],nCent,nParams);

// Write normalized covariance matrices.
// Print covariance matrices.
// Seems that Minuit omits fixed parameters.
char *cNames[] = {"  offset", "  cosPhi", " cos2Phi", "   mjAmp", "   mjEta", "   mjPhi", "    eAmp", "    eEta", "    ePhi", "    cEta", "   c2Eta", "   c3Eta", "   c4Eta"};
.L writeCovariance.C++
char covariance[1024];
sprintf(covariance,"fitResults/cos_covariance_%s_%ic.txt",dataset,nCent);
FILE *fPar = fopen(covariance,"w");
writeCovariance(fPar,ibin,icharge,cNames,error[0],matrix[0][0],nCent,nParams);
fclose(fPar);



//==================================================================================================
// Most of rest of this code is for drawing histograms, fits, residuals, etc.
// Basically checking quality of fits.

TCanvas* c1=new TCanvas("c1");
gStyle->SetPalette(1);  // set up the colors
c1->Clear();
//gStyle->SetOptTitle(0);
gStyle->SetOptStat(0);
c1->SetWindowSize(800,600);
c1->Divide(4,3);

//-------------------------------------------------
// Drawing fitFunction often does not get the sharp exponential.
// Copy to histograms.
TH2D *fitHist[nCent];
reject = kFALSE;
{
    for (int ic=0;ic<nCent;ic++) {
        fitHist[ic] = (TH2D *) dedpNoPileup[ic]->Clone();
        fitHist[ic]->Reset();
        fitHist[ic]->Add(fitFunc[ic]);
    }
}


reject = kFALSE;
{
    for (int ic=0;ic<nCent;ic++) {
        c1->cd(1+ic);
        gPad->SetPhi(30);
        gPad->SetTheta(30);
        fitHist[ic]->Draw("surf1,hist");
    }
}

//-------------------------------------------------
{
    for (int ic=0;ic<nCent;ic++) {
        c1->cd(ic+1);
        gPad->SetPhi(30);
        gPad->SetTheta(30);
        dedpNoPileup[ic]->Draw("surf1,hist");
    }
}

//-------------------------------------------------
// Subtract fit from data to get residual.
TH2D *residual[nCent];
{
    for (int ic=0;ic<nCent;ic++) {
        residual[ic] = (TH2D *) dedpNoPileup[ic]->Clone();
        residual[ic]->Add(fitHist[ic],-1);
        double max = dedpNoPileup[ic]->GetMaximum();
        double min = dedpNoPileup[ic]->GetMinimum();
        residual[ic]->SetMaximum(0.05*(max-min));
        residual[ic]->SetMinimum(-0.05*(max-min));
    }
}

{
    for (int ic=0;ic<nCent;ic++) {
        c1->cd(ic+1);
        gPad->SetPhi(30);
        gPad->SetTheta(50);
        residual[ic]->Draw("surf1,hist");
    }
}

//-------------------------------------------------
// Divide residual by error to get chi squared map.
TH2D *chiSqMap[nCent];
reject = kFALSE;
{
    for (int ic=0;ic<nCent;ic++) {
        chiSqMap[ic] = NULL;
    }
}
{
    reject = kFALSE;
    for (int ic=0;ic<nCent;ic++) {
        if (chiSqMap[ic]) {
            delete chiSqMap[ic];
            chiSqMap[ic] = NULL;
        }
        chiSqMap[ic] = (TH2D *) dedpNoPileup[ic]->Clone();
        chiSqMap[ic]->Add(fitFunc[ic],-1);
        for (int ix=1;ix<=chiSqMap[ic]->GetNbinsX();ix++) {
            for (int iy=1;iy<=chiSqMap[ic]->GetNbinsY();iy++) {
                double den = dedpNoPileup[ic]->GetBinError(ix,iy);
                if (den != 0) {
                    double res = chiSqMap[ic]->GetBinContent(ix,iy)/den;
                    chiSqMap[ic]->SetBinContent(ix,iy,res);
                }
            }
        }
    }
}

{
  for (int ic=0;ic<nCent;ic++) {
    c1->cd(1+ic);
    gPad->SetPhi(30);
    gPad->SetTheta(50);
//    chiSqMap[ic][ibin]->SetMaximum(max[icharge]);
    chiSqMap[ic]->Draw("colz,hist");
  }
}

//-------------------------------------------------
// Plot errors
TH2D *hErr[nCent];
{
    for (int ic=0;ic<nCent;ic++) {
        hErr[ic] = (TH2D *) dedpNoPileup[ic]->Clone();
        for (int ix=1;ix<=hErr[ic]->GetNbinsX();ix++) {
            for (int iy=1;iy<=hErr[ic]->GetNbinsY();iy++) {
                double e = dedpNoPileup[ic]->GetBinError(ix,iy);
                hErr[ic]->SetBinContent(ix,iy,e);
            }
        }
    }
}

{
    for (int ic=0;ic<nCent;ic++) {
        c1->cd(ic+1);
        gPad->SetPhi(30);
        gPad->SetTheta(30);
        hErr[ic]->Draw("surf1,hist");
    }
}



//-------------------------------------------------
// Write data and errors as text files.
{
    char buffer[1024];
    for (int ic=0;ic<nCent;ic++) {
        sprintf(buffer,"bin%i_CIDEtaDPhi_softHard.txt",ic);
        FILE *fAmp = fopen(buffer,"w");
        sprintf(buffer,"bin%i_CIDEtaDPhi_Errors_softHard.txt",ic);
        FILE *fErr = fopen(buffer,"w");
        double eta, phi, a, e;
        for (int ix=1;ix<=dedpNoPileup[ic]->GetNbinsX();ix++) {
            for (int iy=1;iy<=dedpNoPileup[ic]->GetNbinsY();iy++) {
                eta = dedpNoPileup[ic]->GetXaxis()->GetBinCenter(ix);
                phi = dedpNoPileup[ic]->GetYaxis()->GetBinCenter(iy);
                a   = dedpNoPileup[ic]->GetBinContent(ix,iy);
                e   = dedpNoPileup[ic]->GetBinError(ix,iy);
                fprintf(fAmp," %7.4f  %7.4f  %7.4f\n",eta,phi,a);
                fprintf(fErr," %7.4f  %7.4f  %9.6f\n",eta,phi,e);
            }
        }
        fclose(fAmp);
        fclose(fErr);
    }
}

//-------------------------------------------------
// Sometimes we want to look at particular components.
TF2 *fitComponent[nCent][9];
char buffer[1024];
{
    for (int ic=0;ic<nCent;ic++) {
        sprintf(buffer,"fit_Cent%i_Component0",ic);
        fitComponent[ic][0] = new TF2(buffer,"[0]",xmin,xmax,ymin,ymax);
        fitComponent[ic][0]->SetNpx(nx);
        fitComponent[ic][0]->SetNpy(ny);
        sprintf(buffer,"fit_Cent%i_Component1",ic);
        fitComponent[ic][1] = new TF2(buffer,"[0]*cos(y)",xmin,xmax,ymin,ymax);
        fitComponent[ic][1]->SetNpx(nx);
        fitComponent[ic][1]->SetNpy(ny);
        sprintf(buffer,"fit_Cent%i_Component2",ic);
        fitComponent[ic][2] = new TF2(buffer,"[0]*cos(2*y)",xmin,xmax,ymin,ymax);
        fitComponent[ic][2]->SetNpx(nx);
        fitComponent[ic][2]->SetNpy(ny);
        sprintf(buffer,"fit_Cent%i_Component3",ic);
        fitComponent[ic][3] = new TF2(buffer,"[0]*exp(-0.5*((x/[1])^2+(y/[2])^2))+[0]*exp(-0.5*((x/[1])^2+((y-6.283)/[2])^2))",xmin,xmax,ymin,ymax);
        fitComponent[ic][3]->SetNpx(nx);
        fitComponent[ic][3]->SetNpy(ny);
        sprintf(buffer,"fit_Cent%i_Component4",ic);
        fitComponent[ic][4] = new TF2(buffer,"[0]*exp(-sqrt((x/[1])^2+(y/[2])^2))",xmin,xmax,ymin,ymax);
        fitComponent[ic][4]->SetNpx(nx);
        fitComponent[ic][4]->SetNpy(ny);
        sprintf(buffer,"fit_Cent%i_Component5",ic);
        fitComponent[ic][5] = new TF2(buffer,"[0]*cos(x)",xmin,xmax,ymin,ymax);
        fitComponent[ic][5]->SetNpx(nx);
        fitComponent[ic][5]->SetNpy(ny);
        sprintf(buffer,"fit_Cent%i_Component6",ic);
        fitComponent[ic][6] = new TF2(buffer,"[0]*cos(2*x)",xmin,xmax,ymin,ymax);
        fitComponent[ic][6]->SetNpx(nx);
        fitComponent[ic][6]->SetNpy(ny);
        sprintf(buffer,"fit_Cent%i_Component7",ic);
        fitComponent[ic][7] = new TF2(buffer,"[0]*cos(3*x)",xmin,xmax,ymin,ymax);
        fitComponent[ic][7]->SetNpx(nx);
        fitComponent[ic][7]->SetNpy(ny);
        sprintf(buffer,"fit_Cent%i_Component8",ic);
        fitComponent[ic][8] = new TF2(buffer,"[0]*cos(4*x)",xmin,xmax,ymin,ymax);
        fitComponent[ic][8]->SetNpx(nx);
        fitComponent[ic][8]->SetNpy(ny);
    }
}
{
    char buffer[1024];
    for (int ic=0;ic<nCent;ic++) {
        fitComponent[ic][0]->SetParameter(0,param[ic][0]);

        fitComponent[ic][1]->SetParameter(0,param[ic][1]);

        fitComponent[ic][2]->SetParameter(0,param[ic][2]);

        fitComponent[ic][3]->SetParameter(0,param[ic][3]);
        fitComponent[ic][3]->SetParameter(1,param[ic][4]);
        fitComponent[ic][3]->SetParameter(2,param[ic][5]);

        fitComponent[ic][4]->SetParameter(0,param[ic][6]);
        fitComponent[ic][4]->SetParameter(1,param[ic][7]);
        fitComponent[ic][4]->SetParameter(2,param[ic][8]);

        fitComponent[ic][5]->SetParameter(0,param[ic][9]);
        fitComponent[ic][6]->SetParameter(0,param[ic][10]);
        fitComponent[ic][7]->SetParameter(0,param[ic][11]);
        fitComponent[ic][8]->SetParameter(0,param[ic][12]);
    }
}
{
    for (int ic=0;ic<nCent;ic++) {
        c1->cd(ic+1);
        gPad->SetPhi(30);
        gPad->SetTheta(30);
        fitComponent[ic][4]->Draw("surf1,hist");
    }
}

TH2D *hSub[nCent];
{
    for (int ic=0;ic<nCent;ic++) {
        hSub[ic] = (TH2D *) dedpNoPileup[ic]->Clone();
        hSub[ic]->Reset();
        hSub[ic]->Add(fitComponent[ic][5],1);
        hSub[ic]->Add(fitComponent[ic][6],1);
        hSub[ic]->Add(fitComponent[ic][7],1);
        hSub[ic]->Add(fitComponent[ic][8],1);
    }
}
{
    for (int ic=0;ic<nCent;ic++) {
        hSub[ic]->Add(fitComponent[ic][3],-1);
    }
}
{
    for (int ic=0;ic<nCent;ic++) {
        c1->cd(ic+1);
        gPad->SetPhi(30);
        gPad->SetTheta(30);
        hSub[ic]->Draw("surf1,hist");
    }
}


//-------------------------------------------------
gStyle->SetTitleBorderSize(0);
gPad->SetLeftMargin(0.16);
gROOT->SetStyle("Plain");
gStyle->SetOptStat(0);

hSub[6]->SetTitle("Same-side peak, 28-38% centrality")
TAxis *x = hSub[6]->GetXaxis();
x->SetTitleSize(0.07);
x->SetTitleOffset(1.0);
x->SetNdivisions(505);
x->SetLabelSize(0.05);
x->SetTitle("#eta_{#Delta}");
TAxis *y = hSub[6]->GetYaxis();
y->SetTitleSize(0.07);
y->SetTitleOffset(1.0);
y->SetNdivisions(505);
y->SetLabelSize(0.05);
y->SetTitle("#phi_{#Delta}");
TAxis *z = hSub[6]->GetZaxis();
z->SetTitleSize(0.07);
z->SetTitleOffset(1.0);
z->SetNdivisions(505);
z->SetLabelSize(0.05);
z->SetTitle("#Delta#rho/#sqrt{#rho_{ref}}");

hSub[4]->SetTitle("Same-side peak, 46-55% centrality")
TAxis *x = hSub[4]->GetXaxis();
x->SetTitleSize(0.07);
x->SetTitleOffset(1.0);
x->SetNdivisions(505);
x->SetLabelSize(0.05);
x->SetTitle("#eta_{#Delta}");
TAxis *y = hSub[4]->GetYaxis();
y->SetTitleSize(0.07);
y->SetTitleOffset(1.0);
y->SetNdivisions(505);
y->SetLabelSize(0.05);
y->SetTitle("#phi_{#Delta}");
TAxis *z = hSub[4]->GetZaxis();
z->SetTitleSize(0.07);
z->SetTitleOffset(1.0);
z->SetNdivisions(505);
z->SetLabelSize(0.05);
z->SetTitle("#Delta#rho/#sqrt{#rho_{ref}}");
