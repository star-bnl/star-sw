#include "StEStructAutoFit.h"
#include "TMath.h"
#include "TH2D.h"
#include "TVector.h"
#include "TMatrixD.h"
#include "TDecompSVD.h"
#include "TF1.h"
#include "TF2.h"
#include "TRandom2.h"
#include "Stiostream.h"


using namespace TMath;


const int numIterations=500;

// Electron peak, dipole
Double_t fun1(Double_t *x, Double_t *par) {
        //Double_t v1 = par[0] * Cos(x[1]);
        Double_t v1 = par[0] * (1- Cos(x[1]))/2.;
        Double_t v2 = par[1] * Cos(2.*x[1]);
        Double_t geta = par[2] * Exp(-pow((x[0]/par[3]), 2)/2.);
        Double_t d1 = Exp(-pow(x[1]/par[4], 2)/2.);
        Double_t d2 = Exp(-pow((x[1]-2.*Pi())/par[4], 2)/2.);
        Double_t g1 = geta*(d1+d2);
        Double_t g2 = par[5] * Exp(-pow((x[0]/par[6]), 2)/2.);
	Double_t eg = par[8] * Exp(-Sqrt(pow(x[0]/par[9], 2)+pow(x[1]/par[10], 2))/2.);

        return v1+v2+g1+g2+par[7]+eg;
}

// No electron peak, dipole
Double_t fun2(Double_t *x, Double_t *par) {
        //Double_t v1 = par[0] * Cos(x[1]);
        Double_t v1 = par[0] * (1- Cos(x[1]))/2.;
        Double_t v2 = par[1] * Cos(2.*x[1]);
        Double_t geta = par[2] * Exp(-pow((x[0]/par[3]), 2)/2.);
        Double_t d1 = Exp(-pow(x[1]/par[4], 2)/2.);
        Double_t d2 = Exp(-pow((x[1]-2.*Pi())/par[4], 2)/2.);
        Double_t g1 = geta*(d1+d2);
        Double_t g2 = par[5] * Exp(-pow((x[0]/par[6]), 2)/2.);

        return v1+v2+g1+g2+par[7];
}
// Same as above but with v3 term
Double_t fun2v3(Double_t *x, Double_t *par) {
        //Double_t v1 = par[0] * Cos(x[1]);
        Double_t v1 = par[0] * (1- Cos(x[1]))/2.;
        Double_t v2 = par[1] * Cos(2.*x[1]);
        Double_t v3 = par[8] * Cos(3.*x[1]);
        Double_t geta = par[2] * Exp(-pow((x[0]/par[3]), 2)/2.);
        Double_t d1 = Exp(-pow(x[1]/par[4], 2)/2.);
        Double_t d2 = Exp(-pow((x[1]-2.*Pi())/par[4], 2)/2.);
        Double_t g1 = geta*(d1+d2);
        Double_t g2 = par[5] * Exp(-pow((x[0]/par[6]), 2)/2.);

        return v1+v2+g1+g2+par[7]+v3;
}
Double_t fun3(Double_t *x, Double_t *par) {
        //Double_t v1 = par[0] * Cos(x[1]);
        Double_t v1 = par[0] * (1- Cos(x[1]))/2.;
        Double_t v2 = par[1] * Cos(2.*x[1]);
        Double_t geta = par[2] * Exp(-pow((x[0]/par[3]), 2)/2.);
        Double_t d1 = Exp(-pow(x[1]/par[4], 2)/2.);
        Double_t d2 = Exp(-pow((x[1]-2.*Pi())/par[4], 2)/2.);
        Double_t g1 = geta*(d1+d2);
        Double_t g2a = par[5] * Exp(-pow((x[0]/par[6]), 2)/2.);
        Double_t g2b = par[7] * Exp(-pow((x[0]/par[8]), 2)/2.);

        return v1+v2+g1+g2a+g2b+par[9];
}
Double_t fun4(Double_t *x, Double_t *par) {
        //Double_t v1 = par[0] * Cos(x[1]);
        Double_t v1 = par[0] * (1- Cos(x[1]))/2.;
        Double_t v2 = par[1] * Cos(2.*x[1]);
        Double_t geta = par[2] * Exp(-pow((x[0]/par[3]), 2)/2.);
        Double_t d1 = Exp(-pow(x[1]/par[4], 2)/2.);
        Double_t d2 = Exp(-pow((x[1]-2.*Pi())/par[4], 2)/2.);
        Double_t g1 = geta*(d1+d2);
        Double_t g2 = par[5] * Exp(-pow((x[0]/par[6]), 10)/10.);

        return v1+v2+g1+g2+par[7];
}

// No electron peak, AS Gauss
Double_t fun5(Double_t *x, Double_t *par) {
        Double_t v1 = par[0] * (Exp(-pow((x[1]-Pi())/par[8], 2)/2.)+Exp(-pow((x[1]+Pi())/par[8], 2)/2.));
        Double_t v2 = par[1] * Cos(2.*x[1]);
        Double_t geta = par[2] * Exp(-pow((x[0]/par[3]), 2)/2.);
        Double_t d1 = Exp(-pow(x[1]/par[4], 2)/2.);
        Double_t d2 = Exp(-pow((x[1]-2.*Pi())/par[4], 2)/2.);
        Double_t g1 = geta*(d1+d2);
        Double_t g2 = par[5] * Exp(-pow((x[0]/par[6]), 2)/2.);

        return v1+v2+g1+g2+par[7];
}

// Electron peak, AS Gauss
Double_t fun6(Double_t *x, Double_t *par) {
        Double_t v1 = par[0] * (Exp(-pow((x[1]-Pi())/par[8], 2)/2.)+Exp(-pow((x[1]+Pi())/par[8], 2)/2.));
        Double_t v2 = par[1] * Cos(2.*x[1]);
        Double_t geta = par[2] * Exp(-pow((x[0]/par[3]), 2)/2.);
        Double_t d1 = Exp(-pow(x[1]/par[4], 2)/2.);
        Double_t d2 = Exp(-pow((x[1]-2.*Pi())/par[4], 2)/2.);
        Double_t g1 = geta*(d1+d2);
        Double_t g2 = par[5] * Exp(-pow((x[0]/par[6]), 2)/2.);
	Double_t eg = par[9] * Exp(-Sqrt(pow(x[0]/par[10], 2)+pow(x[1]/par[11], 2))/2.);

        return v1+v2+g1+g2+par[7]+eg;
}
// No electron peak, dipole, two SS Gaussians
Double_t fun7(Double_t *x, Double_t *par) {
        //Double_t v1 = par[0] * Cos(x[1]);
        Double_t v1 = par[0] * (1- Cos(x[1]))/2.;
        Double_t v2 = par[1] * Cos(2.*x[1]);
        Double_t geta = par[2] * Exp(-pow((x[0]/par[3]), 2)/2.);
        Double_t d1 = Exp(-pow(x[1]/par[4], 2)/2.);
        Double_t d2 = Exp(-pow((x[1]-2.*Pi())/par[4], 2)/2.);
        Double_t g1 = geta*(d1+d2);
        Double_t g2 = par[5] * Exp(-pow((x[0]/par[6]), 2)/2.);
        Double_t getab = par[8] * Exp(-pow((x[0]/par[9]), 2)/2.);
        Double_t d1b = Exp(-pow(x[1]/par[10], 2)/2.);
        Double_t d2b = Exp(-pow((x[1]-2.*Pi())/par[10], 2)/2.);
        Double_t g1b = getab*(d1b+d2b);

        return v1+v2+g1+g2+par[7]+g1b;
}

// Electron peak, dipole
Double_t fun8(Double_t *x, Double_t *par) {
        //Double_t v1 = par[0] * Cos(x[1]);
        Double_t v1 = par[0] * (1- Cos(x[1]))/2.;
        Double_t v2 = par[1] * Cos(2.*x[1]);
        Double_t v3 = par[11] * Cos(3.*x[1]);
        Double_t geta = par[2] * Exp(-pow((x[0]/par[3]), 2)/2.);
        Double_t d1 = Exp(-pow(x[1]/par[4], 2)/2.);
        Double_t d2 = Exp(-pow((x[1]-2.*Pi())/par[4], 2)/2.);
        Double_t g1 = geta*(d1+d2);
        Double_t g2 = par[5] * Exp(-pow((x[0]/par[6]), 2)/2.);
	Double_t eg = par[8] * Exp(-Sqrt(pow(x[0]/par[9], 2)+pow(x[1]/par[10], 2))/2.);

        return v1+v2+g1+g2+par[7]+eg+v3;
}


// New algorithm just based on random sampling
// Less sophisticated, but perhaps more reliable

double* StEStructAutoFit::autofit8Par(double* best, TH2D* plot, int type, double* allchisq) {
        // First set the error in the (0,0) bin high so it isn't part of the fit
        plot->SetBinError(13, 7, 1000.);
        plot->SetBinError(12, 7, 1000.);
        plot->SetBinError(11, 7, 1000.);
        plot->SetBinError(14, 7, 1000.);
        plot->SetBinError(15, 7, 1000.);
        plot->SetBinError(13, 6, 1000.);
        plot->SetBinError(13, 8, 1000.);

	double amprange = plot->GetMaximum() - plot->GetMinimum();

	// The range of values will just be based on RMS and means of the histogramsa
	double mean=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			mean += plot->GetBinContent(i, k);
		}
	}
	mean *= 1./(25.*24.);

	double rms2=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			rms2 += pow(plot->GetBinContent(i, k)-mean, 2);
			//rms += (plot->GetBinContent(i, k)-mean) * (plot->GetBinContent(i, k)-mean);
			//rms2 += plot->GetBinContent(i, k);
		}
	}
	rms2 *= 1./(25.*24.);
	rms2 = sqrt(rms2);

	cout << "\nmean=" << mean << "\trms=" << rms2 << endl;

	double minphi=.35;
	double maxphi=.9;

	double mineta=.4;
	double maxeta=3.0;

	double mincos=0.;
	//double maxcos=2.0*rms2;
	double maxcos=amprange;

	double minv2=0.;
	//double maxv2=2.0*rms2;
	double maxv2=amprange;

	double ming2=0.;
	//double maxg2=5.*rms2;
	double maxg2=amprange;

	double ming1=-2.*rms2;
	if(type == 1)
		ming1=0.;
	double maxg1=2.*rms2;

	double mingw=.1;
	double maxgw=5;

	double minoffset=mean-3.*rms2;
	double maxoffset=mean+rms2;

	TRandom2 rand2;
	rand2.SetSeed(0);

        double chisqsum=0;
        double chisqsqsum=0;
        double bestchisq=999999;
	double worstchisq=0;
	int numConverge=0;
	for(int i=0; i<numIterations; i++) {
		double phi = rand2.Rndm()*(maxphi-minphi)+minphi;
		double eta = rand2.Rndm()*(maxeta-mineta)+mineta;
		double cosv = rand2.Rndm()*(maxcos-mincos)+mincos;
		double v2 = rand2.Rndm()*(maxv2-minv2)+minv2;
		double g2 = rand2.Rndm()*(maxg2-ming2)+ming2;
		double g1 = rand2.Rndm()*(maxg1-ming1)+ming1;
		double gw = rand2.Rndm()*(maxgw-mingw)+mingw;
		double offset = rand2.Rndm()*(maxoffset-minoffset)+minoffset;

		cout << "\nphi=" << phi << "\teta=" << eta << "\tcos=" << cosv << "\tv2=" << v2 << "\tg2=" << g2 << "\tg1=" << g1 << "\tgw=" << gw << "\toffset=" << offset << endl;


		const Int_t npar = 8;
	
                Double_t f2params[npar] = {cosv, v2, g2, eta, phi, g1, gw, offset};

                TF2 *f2 = new TF2("f2",fun2,-2,2,-Pi()/2.,3*Pi()/2, npar);
                f2->SetParameters(f2params);
                //f2->SetParLimits(0, -5, 0);
		//f2->FixParameter(1, 0.011704);
		//f2->FixParameter(1, 0.00364);
		if(type == 2)
                	f2->SetParLimits(1, 0, 5);
                f2->SetParLimits(2, 0, 5);
                f2->SetParLimits(3, 0.1, 5);
                f2->SetParLimits(4, 0.1, 2);
		if(type == 1 || type == 2)
                	f2->SetParLimits(5, 0, 3);
                f2->SetParLimits(6, 0.12, 7);

                int fitStatus=plot->Fit("f2","N");
                fitStatus=plot->Fit("f2","EN");

                if(fitStatus==0) {
                        double chisq=f2->GetChisquare();
                        cout << "\n" << f2->GetParameter(2) << endl;
                        chisqsum+=chisq;
                        chisqsqsum+=chisq*chisq;
                        if(chisq<bestchisq) {
                                for(int i=0; i<npar; i++) {
                                        best[i*2]=f2->GetParameter(i);
					best[i*2+1]=f2->GetParError(i);
                                }
                                bestchisq=chisq;
                        }
			if(chisq>worstchisq) {
				worstchisq=chisq;
			}
			numConverge++;
			if(allchisq!=NULL) {allchisq[i]=chisq;}
		} else {
			if(allchisq!=NULL) {allchisq[i]=0.;}
		}
                delete f2;
	}

	cout << "\nOut of " << numIterations << " attempts, " << numConverge << " actually converged." << endl;
        cout << "\nThe best chisq is: " << bestchisq << endl;
        cout << "\nThe worst chisq is: " << worstchisq << endl;

//	if(storeChi) {
//		chiPlot = new TH1D("chiPlot","Chi Squared",100,bestchisq,worstchisq);
//		//chiPlot->SetBins(100,bestchisq,worstchisq);
//		for(int i=0; i<numIterations; i++) {
//			cerr << "\tIteration: " << i;
//			if(allchisq[i]!=0.) {
//				chiPlot->Fill(allchisq[i]);
//			}
//		}
//	}
//	cerr << "\nIn function plot is: " << chiPlot;
//	cerr << "\nIn function RMS is: " << chiPlot->GetRMS();
	
	
	return best;
}

// Now with v3

double* StEStructAutoFit::autofit8Parv3(double* best, TH2D* plot, int type, double* allchisq) {
        // First set the error in the (0,0) bin high so it isn't part of the fit
        plot->SetBinError(13, 7, 1000.);
        plot->SetBinError(12, 7, 1000.);
        plot->SetBinError(11, 7, 1000.);
        plot->SetBinError(14, 7, 1000.);
        plot->SetBinError(15, 7, 1000.);
        plot->SetBinError(13, 6, 1000.);
        plot->SetBinError(13, 8, 1000.);

	double amprange = plot->GetMaximum() - plot->GetMinimum();

	// The range of values will just be based on RMS and means of the histogramsa
	double mean=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			mean += plot->GetBinContent(i, k);
		}
	}
	mean *= 1./(25.*24.);

	double rms2=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			rms2 += pow(plot->GetBinContent(i, k)-mean, 2);
			//rms += (plot->GetBinContent(i, k)-mean) * (plot->GetBinContent(i, k)-mean);
			//rms2 += plot->GetBinContent(i, k);
		}
	}
	rms2 *= 1./(25.*24.);
	rms2 = sqrt(rms2);

	cout << "\nmean=" << mean << "\trms=" << rms2 << endl;

	double minphi=.35;
	double maxphi=.9;

	double mineta=.4;
	double maxeta=3.0;

	double mincos=0.;
	//double maxcos=2.0*rms2;
	double maxcos=amprange;

	double minv2=0.;
	//double maxv2=2.0*rms2;
	double maxv2=amprange;

	double minv3=0.;
	//double maxv3=2.0*rms2;
	double maxv3=amprange;

	double ming2=0.;
	//double maxg2=5.*rms2;
	double maxg2=amprange;

	double ming1=-2.*rms2;
	if(type == 1)
		ming1=0.;
	double maxg1=2.*rms2;

	double mingw=.1;
	double maxgw=5;

	double minoffset=mean-3.*rms2;
	double maxoffset=mean+rms2;

	TRandom2 rand2;
	rand2.SetSeed(0);

        double chisqsum=0;
        double chisqsqsum=0;
        double bestchisq=999999;
	double worstchisq=0;
	int numConverge=0;
	for(int i=0; i<numIterations; i++) {
		double phi = rand2.Rndm()*(maxphi-minphi)+minphi;
		double eta = rand2.Rndm()*(maxeta-mineta)+mineta;
		double cosv = rand2.Rndm()*(maxcos-mincos)+mincos;
		double v2 = rand2.Rndm()*(maxv2-minv2)+minv2;
		double v3 = rand2.Rndm()*(maxv3-minv3)+minv3;
		double g2 = rand2.Rndm()*(maxg2-ming2)+ming2;
		double g1 = rand2.Rndm()*(maxg1-ming1)+ming1;
		double gw = rand2.Rndm()*(maxgw-mingw)+mingw;
		double offset = rand2.Rndm()*(maxoffset-minoffset)+minoffset;

		cout << "\nphi=" << phi << "\teta=" << eta << "\tcos=" << cosv << "\tv2=" << v2 << "\tg2=" << g2 << "\tg1=" << g1 << "\tgw=" << gw << "\toffset=" << offset << endl;


		const Int_t npar = 9;
	
                Double_t f2params[npar] = {cosv, v2, g2, eta, phi, g1, gw, offset, v3};

                TF2 *f2 = new TF2("f2",fun2v3,-2,2,-Pi()/2.,3*Pi()/2, npar);
                f2->SetParameters(f2params);
                //f2->SetParLimits(0, -5, 0);
		//f2->FixParameter(1, 0.011704);
		//f2->FixParameter(1, 0.00364);
		if(type == 2)
                	f2->SetParLimits(1, 0, 5);
                f2->SetParLimits(2, 0, 5);
                f2->SetParLimits(3, 0.1, 5);
                f2->SetParLimits(4, 0.1, 2);
		if(type == 1 || type == 2)
                	f2->SetParLimits(5, 0, 3);
                f2->SetParLimits(6, 0.01, 7);

                int fitStatus=plot->Fit("f2","N");
                fitStatus=plot->Fit("f2","EN");

                if(fitStatus==0) {
                        double chisq=f2->GetChisquare();
                        cout << "\n" << f2->GetParameter(2) << endl;
                        chisqsum+=chisq;
                        chisqsqsum+=chisq*chisq;
                        if(chisq<bestchisq) {
                                for(int i=0; i<npar; i++) {
                                        best[i*2]=f2->GetParameter(i);
					best[i*2+1]=f2->GetParError(i);
                                }
                                bestchisq=chisq;
                        }
			if(chisq>worstchisq) {
				worstchisq=chisq;
			}
			numConverge++;
			if(allchisq!=NULL) {allchisq[i]=chisq;}
		} else {
			if(allchisq!=NULL) {allchisq[i]=0.;}
		}
                delete f2;
	}

	cout << "\nOut of " << numIterations << " attempts, " << numConverge << " actually converged." << endl;
        cout << "\nThe best chisq is: " << bestchisq << endl;
        cout << "\nThe worst chisq is: " << worstchisq << endl;

//	if(storeChi) {
//		chiPlot = new TH1D("chiPlot","Chi Squared",100,bestchisq,worstchisq);
//		//chiPlot->SetBins(100,bestchisq,worstchisq);
//		for(int i=0; i<numIterations; i++) {
//			cerr << "\tIteration: " << i;
//			if(allchisq[i]!=0.) {
//				chiPlot->Fill(allchisq[i]);
//			}
//		}
//	}
//	cerr << "\nIn function plot is: " << chiPlot;
//	cerr << "\nIn function RMS is: " << chiPlot->GetRMS();
	
	
	return best;
}

// Exponential electron peak
double* StEStructAutoFit::autofit11Par(double* best, TH2D* plot, int type, double* allchisq) {

	double amprange = plot->GetMaximum() - plot->GetMinimum();

	// The range of values will just be based on RMS and means of the histogramsa
	double mean=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			mean += plot->GetBinContent(i, k);
		}
	}
	mean *= 1./(25.*24.);

	double rms2=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			rms2 += pow(plot->GetBinContent(i, k)-mean, 2);
			//rms += (plot->GetBinContent(i, k)-mean) * (plot->GetBinContent(i, k)-mean);
			//rms2 += plot->GetBinContent(i, k);
		}
	}
	rms2 *= 1./(25.*24.);
	rms2 = sqrt(rms2);

	cout << "\nmean=" << mean << "\trms=" << rms2 << endl;

	double minphi=.35;
	double maxphi=.9;

	double mineta=.4;
	double maxeta=2.8;

	double mincos=0.;
	//double maxcos=2.0*rms2;
	double maxcos=amprange;

	double minv2=0.;
	//double maxv2=2.0*rms2;
	double maxv2=amprange;

	double ming2=0.;
	//double maxg2=5.*rms2;
	double maxg2=amprange;

	double ming1=-2.*rms2;
	if(type == 1)
		ming1=0.;
	double maxg1=2.*rms2;

	double mingw=.1;
	double maxgw=5;

	double minoffset=mean-3.*rms2;
	double maxoffset=mean+rms2;

	double minexp=0.;
	double maxexp=10.*rms2;

	double minew=.01;
	double maxew=.8;

	TRandom2 rand2;
	rand2.SetSeed(0);

        double chisqsum=0;
        double chisqsqsum=0;
        double bestchisq=999999;
	double worstchisq=0;
	int numConverge=0;
	for(int i=0; i<numIterations; i++) {
		double phi = rand2.Rndm()*(maxphi-minphi)+minphi;
		double eta = rand2.Rndm()*(maxeta-mineta)+mineta;
		double cosv = rand2.Rndm()*(maxcos-mincos)+mincos;
		double v2 = rand2.Rndm()*(maxv2-minv2)+minv2;
		double g2 = rand2.Rndm()*(maxg2-ming2)+ming2;
		double g1 = rand2.Rndm()*(maxg1-ming1)+ming1;
		double gw = rand2.Rndm()*(maxgw-mingw)+mingw;
		double offset = rand2.Rndm()*(maxoffset-minoffset)+minoffset;
		double exp = rand2.Rndm()*(maxexp-minexp)+minexp;
		double expwe = rand2.Rndm()*(maxew-minew)+minew;
		double expwp = rand2.Rndm()*(maxew-minew)+minew;

		cout << "\nphi=" << phi << "\teta=" << eta << "\tcos=" << cosv << "\tv2=" << v2 << "\tg2=" << g2 << "\tg1=" << g1 << "\tgw=" << gw << "\toffset=" << offset << endl;


		const Int_t npar = 11;
	
                Double_t f2params[npar] = {cosv, v2, g2, eta, phi, g1, gw, offset, exp, expwe, expwp};

                TF2 *f2 = new TF2("f2",fun1,-2,2,-Pi()/2.,3*Pi()/2, npar);
                f2->SetParameters(f2params);
                //f2->SetParLimits(0, -5, 0);
		//f2->FixParameter(1, 0.011704);
		//f2->FixParameter(1, 0.00364);
		if(type == 2)
                	f2->SetParLimits(1, 0, 5);
                f2->SetParLimits(2, 0, 5);
                f2->SetParLimits(3, 0.1, 5);
                f2->SetParLimits(4, 0.1, 1.2);
		if(type == 1 || type == 2)
                	f2->SetParLimits(5, 0, 3);
                f2->SetParLimits(6, 0.01, 7);
                f2->SetParLimits(8, 0., 5);
		f2->SetParLimits(9, 0.005, .9);
		f2->SetParLimits(10, 0.005, .9);

                int fitStatus=plot->Fit("f2","N");
                fitStatus=plot->Fit("f2","EN");

                if(fitStatus==0) {
                        double chisq=f2->GetChisquare();
                        cout << "\n" << f2->GetParameter(2) << endl;
                        chisqsum+=chisq;
                        chisqsqsum+=chisq*chisq;
                        if(chisq<bestchisq) {
                                for(int i=0; i<npar; i++) {
                                        best[i*2]=f2->GetParameter(i);
					best[i*2+1]=f2->GetParError(i);
                                }
                                bestchisq=chisq;
                        }
			if(chisq>worstchisq) {
				worstchisq=chisq;
			}
			numConverge++;
			if(allchisq!=NULL) {allchisq[i]=chisq;}
		} else {
			if(allchisq!=NULL) {allchisq[i]=0.;}
		}
                delete f2;
	}

	cout << "\nOut of " << numIterations << " attempts, " << numConverge << " actually converged." << endl;
        cout << "\nThe best chisq is: " << bestchisq << endl;
        cout << "\nThe worst chisq is: " << worstchisq << endl;
	
	return best;
}


// Exponential electron peak + v3
double* StEStructAutoFit::autofit11Parv3(double* best, TH2D* plot, int type, double* allchisq) {

	double amprange = plot->GetMaximum() - plot->GetMinimum();

	// The range of values will just be based on RMS and means of the histogramsa
	double mean=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			mean += plot->GetBinContent(i, k);
		}
	}
	mean *= 1./(25.*24.);

	double rms2=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			rms2 += pow(plot->GetBinContent(i, k)-mean, 2);
			//rms += (plot->GetBinContent(i, k)-mean) * (plot->GetBinContent(i, k)-mean);
			//rms2 += plot->GetBinContent(i, k);
		}
	}
	rms2 *= 1./(25.*24.);
	rms2 = sqrt(rms2);

	cout << "\nmean=" << mean << "\trms=" << rms2 << endl;

	double minphi=.35;
	double maxphi=.9;

	double mineta=.4;
	double maxeta=2.8;

	double mincos=0.;
	//double maxcos=2.0*rms2;
	double maxcos=amprange;

	double minv2=0.;
	//double maxv2=2.0*rms2;
	double maxv2=amprange;

	double minv3=0.;
	//double maxv3=2.0*rms2;
	double maxv3=amprange;

	double ming2=0.;
	//double maxg2=5.*rms2;
	double maxg2=amprange;

	double ming1=-2.*rms2;
	if(type == 1)
		ming1=0.;
	double maxg1=2.*rms2;

	double mingw=.1;
	double maxgw=5;

	double minoffset=mean-3.*rms2;
	double maxoffset=mean+rms2;

	double minexp=0.;
	double maxexp=10.*rms2;

	double minew=.01;
	double maxew=1.;

	TRandom2 rand2;
	rand2.SetSeed(0);

        double chisqsum=0;
        double chisqsqsum=0;
        double bestchisq=999999;
	double worstchisq=0;
	int numConverge=0;
	for(int i=0; i<numIterations; i++) {
		double phi = rand2.Rndm()*(maxphi-minphi)+minphi;
		double eta = rand2.Rndm()*(maxeta-mineta)+mineta;
		double cosv = rand2.Rndm()*(maxcos-mincos)+mincos;
		double v2 = rand2.Rndm()*(maxv2-minv2)+minv2;
		double v3 = rand2.Rndm()*(maxv3-minv3)+minv3;
		double g2 = rand2.Rndm()*(maxg2-ming2)+ming2;
		double g1 = rand2.Rndm()*(maxg1-ming1)+ming1;
		double gw = rand2.Rndm()*(maxgw-mingw)+mingw;
		double offset = rand2.Rndm()*(maxoffset-minoffset)+minoffset;
		double exp = rand2.Rndm()*(maxexp-minexp)+minexp;
		double expwe = rand2.Rndm()*(maxew-minew)+minew;
		double expwp = rand2.Rndm()*(maxew-minew)+minew;

		cout << "\nphi=" << phi << "\teta=" << eta << "\tcos=" << cosv << "\tv2=" << v2 << "\tg2=" << g2 << "\tg1=" << g1 << "\tgw=" << gw << "\toffset=" << offset << endl;


		const Int_t npar = 12;
	
                Double_t f2params[npar] = {cosv, v2, g2, eta, phi, g1, gw, offset, exp, expwe, expwp, v3};

                TF2 *f2 = new TF2("f2",fun1,-2,2,-Pi()/2.,3*Pi()/2, npar);
                f2->SetParameters(f2params);
                //f2->SetParLimits(0, -5, 0);
		//f2->FixParameter(1, 0.011704);
		//f2->FixParameter(1, 0.00364);
		if(type == 2)
                	f2->SetParLimits(1, 0, 5);
                f2->SetParLimits(2, 0, 5);
                f2->SetParLimits(3, 0.1, 5);
                f2->SetParLimits(4, 0.1, 2);
		if(type == 1 || type == 2)
                	f2->SetParLimits(5, 0, 3);
                f2->SetParLimits(6, 0.01, 7);
                f2->SetParLimits(8, 0., 5);
		f2->SetParLimits(9, 0.005, 2);
		f2->SetParLimits(10, 0.005, 2);

                int fitStatus=plot->Fit("f2","N");
                fitStatus=plot->Fit("f2","EN");

                if(fitStatus==0) {
                        double chisq=f2->GetChisquare();
                        cout << "\n" << f2->GetParameter(2) << endl;
                        chisqsum+=chisq;
                        chisqsqsum+=chisq*chisq;
                        if(chisq<bestchisq) {
                                for(int i=0; i<npar; i++) {
                                        best[i*2]=f2->GetParameter(i);
					best[i*2+1]=f2->GetParError(i);
                                }
                                bestchisq=chisq;
                        }
			if(chisq>worstchisq) {
				worstchisq=chisq;
			}
			numConverge++;
			if(allchisq!=NULL) {allchisq[i]=chisq;}
		} else {
			if(allchisq!=NULL) {allchisq[i]=0.;}
		}
                delete f2;
	}

	cout << "\nOut of " << numIterations << " attempts, " << numConverge << " actually converged." << endl;
        cout << "\nThe best chisq is: " << bestchisq << endl;
        cout << "\nThe worst chisq is: " << worstchisq << endl;
	
	return best;
}

// New algorithm just based on random sampling
// Less sophisticated, but perhaps more reliable
// Away-side Gaussian verion

double* StEStructAutoFit::autofit9Par(double* best, TH2D* plot, int type, double* allchisq) {
        // First set the error in the (0,0) bin high so it isn't part of the fit
        plot->SetBinError(13, 7, 1000.);
        plot->SetBinError(12, 7, 1000.);
        plot->SetBinError(11, 7, 1000.);
        plot->SetBinError(14, 7, 1000.);
        plot->SetBinError(15, 7, 1000.);
        plot->SetBinError(13, 6, 1000.);
        plot->SetBinError(13, 8, 1000.);

	double amprange = plot->GetMaximum() - plot->GetMinimum();

	// The range of values will just be based on RMS and means of the histogramsa
	double mean=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			mean += plot->GetBinContent(i, k);
		}
	}
	mean *= 1./(25.*24.);

	double rms2=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			rms2 += pow(plot->GetBinContent(i, k)-mean, 2);
			//rms += (plot->GetBinContent(i, k)-mean) * (plot->GetBinContent(i, k)-mean);
			//rms2 += plot->GetBinContent(i, k);
		}
	}
	rms2 *= 1./(25.*24.);
	rms2 = sqrt(rms2);

	cout << "\nmean=" << mean << "\trms=" << rms2 << endl;

	double minphi=.35;
	double maxphi=.9;

	double mineta=.4;
	double maxeta=2.8;

	double mincos=0.;
	//double maxcos=2.0*rms2;
	double maxcos=amprange;

	double minv2=0.;
	//double maxv2=2.0*rms2;
	double maxv2=amprange;

	double ming2=0.;
	//double maxg2=5.*rms2;
	double maxg2=amprange;

	double ming1=-2.*rms2;
	if(type == 1)
		ming1=0.;
	double maxg1=2.*rms2;

	double mingw=.1;
	double maxgw=5;

	double minoffset=mean-3.*rms2;
	double maxoffset=mean+rms2;

	double minagw=.5;
	double maxagw=2.5;

	TRandom2 rand2;
	rand2.SetSeed(0);

        double chisqsum=0;
        double chisqsqsum=0;
        double bestchisq=999999;
	double worstchisq=0;
	int numConverge=0;
	for(int i=0; i<numIterations; i++) {
		double phi = rand2.Rndm()*(maxphi-minphi)+minphi;
		double eta = rand2.Rndm()*(maxeta-mineta)+mineta;
		double cosv = rand2.Rndm()*(maxcos-mincos)+mincos;
		double v2 = rand2.Rndm()*(maxv2-minv2)+minv2;
		double g2 = rand2.Rndm()*(maxg2-ming2)+ming2;
		double g1 = rand2.Rndm()*(maxg1-ming1)+ming1;
		double gw = rand2.Rndm()*(maxgw-mingw)+mingw;
		double offset = rand2.Rndm()*(maxoffset-minoffset)+minoffset;
		double agw = rand2.Rndm()*(maxagw-minagw)+minagw;

		cout << "\nphi=" << phi << "\teta=" << eta << "\tcos=" << cosv << "\tv2=" << v2 << "\tg2=" << g2 << "\tg1=" << g1 << "\tgw=" << gw << "\toffset=" << offset << endl;


		const Int_t npar = 9;
	
                Double_t f2params[npar] = {cosv, v2, g2, eta, phi, g1, gw, offset, agw};

                TF2 *f2 = new TF2("f2",fun5,-2,2,-Pi()/2.,3*Pi()/2, npar);
                f2->SetParameters(f2params);
                //f2->SetParLimits(0, -5, 0);
		//f2->FixParameter(1, 0.011704);
		//f2->FixParameter(1, 0.00364);
		if(type == 2)
                	f2->SetParLimits(1, 0, 5);
                f2->SetParLimits(2, 0, 5);
                f2->SetParLimits(3, 0.1, 5);
                f2->SetParLimits(4, 0.1, 2);
		if(type == 1 || type == 2)
                	f2->SetParLimits(5, 0, 3);
                f2->SetParLimits(6, 0.01, 7);
		f2->SetParLimits(8, 0.01, 5);

                int fitStatus=plot->Fit("f2","N");
                fitStatus=plot->Fit("f2","EN");

                if(fitStatus==0) {
                        double chisq=f2->GetChisquare();
                        cout << "\n" << f2->GetParameter(2) << endl;
                        chisqsum+=chisq;
                        chisqsqsum+=chisq*chisq;
                        if(chisq<bestchisq) {
                                for(int i=0; i<npar; i++) {
                                        best[i*2]=f2->GetParameter(i);
					best[i*2+1]=f2->GetParError(i);
                                }
                                bestchisq=chisq;
                        }
			if(chisq>worstchisq) {
				worstchisq=chisq;
			}
			numConverge++;
			if(allchisq!=NULL) {allchisq[i]=chisq;}
		} else {
			if(allchisq!=NULL) {allchisq[i]=0.;}
		}
                delete f2;
	}

	cout << "\nOut of " << numIterations << " attempts, " << numConverge << " actually converged." << endl;
        cout << "\nThe best chisq is: " << bestchisq << endl;
        cout << "\nThe worst chisq is: " << worstchisq << endl;
	
	return best;
}

// Away-side Gaussian verion
// Exponential peak

double* StEStructAutoFit::autofit12Par(double* best, TH2D* plot, int type, double* allchisq) {

	double amprange = plot->GetMaximum() - plot->GetMinimum();

	// The range of values will just be based on RMS and means of the histogramsa
	double mean=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			mean += plot->GetBinContent(i, k);
		}
	}
	mean *= 1./(25.*24.);

	double rms2=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			rms2 += pow(plot->GetBinContent(i, k)-mean, 2);
			//rms += (plot->GetBinContent(i, k)-mean) * (plot->GetBinContent(i, k)-mean);
			//rms2 += plot->GetBinContent(i, k);
		}
	}
	rms2 *= 1./(25.*24.);
	rms2 = sqrt(rms2);

	cout << "\nmean=" << mean << "\trms=" << rms2 << endl;

	double minphi=.35;
	double maxphi=.9;

	double mineta=.4;
	double maxeta=2.8;

	double mincos=0.;
	//double maxcos=2.0*rms2;
	double maxcos=amprange;

	double minv2=0.;
	//double maxv2=2.0*rms2;
	double maxv2=amprange;

	double ming2=0.;
	//double maxg2=5.*rms2;
	double maxg2=amprange;

	double ming1=-2.*rms2;
	if(type == 1)
		ming1=0.;
	double maxg1=2.*rms2;

	double mingw=.1;
	double maxgw=5;

	double minoffset=mean-3.*rms2;
	double maxoffset=mean+rms2;

	double minagw=.5;
	double maxagw=2.5;

	double minexp=0.;
	double maxexp=10.*rms2;

	double minew=.01;
	double maxew=1.;

	TRandom2 rand2;
	rand2.SetSeed(0);

        double chisqsum=0;
        double chisqsqsum=0;
        double bestchisq=999999;
	double worstchisq=0;
	int numConverge=0;
	for(int i=0; i<numIterations; i++) {
		double phi = rand2.Rndm()*(maxphi-minphi)+minphi;
		double eta = rand2.Rndm()*(maxeta-mineta)+mineta;
		double cosv = rand2.Rndm()*(maxcos-mincos)+mincos;
		double v2 = rand2.Rndm()*(maxv2-minv2)+minv2;
		double g2 = rand2.Rndm()*(maxg2-ming2)+ming2;
		double g1 = rand2.Rndm()*(maxg1-ming1)+ming1;
		double gw = rand2.Rndm()*(maxgw-mingw)+mingw;
		double offset = rand2.Rndm()*(maxoffset-minoffset)+minoffset;
		double agw = rand2.Rndm()*(maxagw-minagw)+minagw;
		double exp = rand2.Rndm()*(maxexp-minexp)+minexp;
		double expwe = rand2.Rndm()*(maxew-minew)+minew;
		double expwp = rand2.Rndm()*(maxew-minew)+minew;

		cout << "\nphi=" << phi << "\teta=" << eta << "\tcos=" << cosv << "\tv2=" << v2 << "\tg2=" << g2 << "\tg1=" << g1 << "\tgw=" << gw << "\toffset=" << offset << endl;


		const Int_t npar = 12;
	
                Double_t f2params[npar] = {cosv, v2, g2, eta, phi, g1, gw, offset, agw, exp, expwe, expwp};

                TF2 *f2 = new TF2("f2",fun6,-2,2,-Pi()/2.,3*Pi()/2, npar);
                f2->SetParameters(f2params);
                //f2->SetParLimits(0, -5, 0);
		//f2->FixParameter(1, 0.011704);
		//f2->FixParameter(1, 0.00364);
		if(type == 2)
                	f2->SetParLimits(1, 0, 5);
                f2->SetParLimits(2, 0, 5);
                f2->SetParLimits(3, 0.1, 5);
                f2->SetParLimits(4, 0.1, 2);
		if(type == 1 || type == 2)
                	f2->SetParLimits(5, 0, 3);
                f2->SetParLimits(6, 0.01, 7);
		f2->SetParLimits(8, 0.01, 5);
                f2->SetParLimits(9, 0., 5);
		f2->SetParLimits(10, 0.005, 2);
		f2->SetParLimits(11, 0.005, 2);

                int fitStatus=plot->Fit("f2","N");
                fitStatus=plot->Fit("f2","EN");

                if(fitStatus==0) {
                        double chisq=f2->GetChisquare();
                        cout << "\n" << f2->GetParameter(2) << endl;
                        chisqsum+=chisq;
                        chisqsqsum+=chisq*chisq;
                        if(chisq<bestchisq) {
                                for(int i=0; i<npar; i++) {
                                        best[i*2]=f2->GetParameter(i);
					best[i*2+1]=f2->GetParError(i);
                                }
                                bestchisq=chisq;
                        }
			if(chisq>worstchisq) {
				worstchisq=chisq;
			}
			numConverge++;
			if(allchisq!=NULL) {allchisq[i]=chisq;}
		} else {
			if(allchisq!=NULL) {allchisq[i]=0.;}
		}
                delete f2;
	}

	cout << "\nOut of " << numIterations << " attempts, " << numConverge << " actually converged." << endl;
        cout << "\nThe best chisq is: " << bestchisq << endl;
        cout << "\nThe worst chisq is: " << worstchisq << endl;
	
	return best;
}


double* StEStructAutoFit::autofit11Par2G(double* best, TH2D* plot, int type, double yt, double* allchisq) {
        // First set the error in the (0,0) bin high so it isn't part of the fit
        plot->SetBinError(13, 7, 1000.);
        plot->SetBinError(12, 7, 1000.);
        plot->SetBinError(11, 7, 1000.);
        plot->SetBinError(14, 7, 1000.);
        plot->SetBinError(15, 7, 1000.);
        plot->SetBinError(13, 6, 1000.);
        plot->SetBinError(13, 8, 1000.);

	double amprange = plot->GetMaximum() - plot->GetMinimum();

	// The range of values will just be based on RMS and means of the histogramsa
	double mean=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			mean += plot->GetBinContent(i, k);
		}
	}
	mean *= 1./(25.*24.);

	double rms2=0.;
	for(int i=1; i<=25; i++) {
		for(int k=1; k<=24; k++) {
			rms2 += pow(plot->GetBinContent(i, k)-mean, 2);
			//rms += (plot->GetBinContent(i, k)-mean) * (plot->GetBinContent(i, k)-mean);
			//rms2 += plot->GetBinContent(i, k);
		}
	}
	rms2 *= 1./(25.*24.);
	rms2 = sqrt(rms2);

	cout << "\nmean=" << mean << "\trms=" << rms2 << endl;

	double minphi=.35;
	double maxphi=.9;

	double mineta=.4;
	double maxeta=2.8;

	double mincos=0.;
	//double maxcos=2.0*rms2;
	double maxcos=amprange;

	double minv2=0.;
	//double maxv2=2.0*rms2;
	double maxv2=amprange;

	double ming2=0.;
	//double maxg2=5.*rms2;
	double maxg2=amprange;

	double ming2b=0.;
	//double maxg2b=5.*rms2;
	double maxg2b=amprange;

	double ming1=-2.*rms2;
	if(type == 1)
		ming1=0.;
	double maxg1=2.*rms2;

	double mingw=.1;
	double maxgw=5;

	double minoffset=mean-3.*rms2;
	double maxoffset=mean+rms2;

	double eta2 = .9/sqrt(yt);
	double phi2 = 1.8/sqrt(3.*yt-2.);

	TRandom2 rand2;
	rand2.SetSeed(0);

        double chisqsum=0;
        double chisqsqsum=0;
        double bestchisq=999999;
	double worstchisq=0;
	int numConverge=0;
	for(int i=0; i<numIterations; i++) {
		double phi = rand2.Rndm()*(maxphi-minphi)+minphi;
		double eta = rand2.Rndm()*(maxeta-mineta)+mineta;
		double cosv = rand2.Rndm()*(maxcos-mincos)+mincos;
		double v2 = rand2.Rndm()*(maxv2-minv2)+minv2;
		double g2 = rand2.Rndm()*(maxg2-ming2)+ming2;
		double g1 = rand2.Rndm()*(maxg1-ming1)+ming1;
		double gw = rand2.Rndm()*(maxgw-mingw)+mingw;
		double offset = rand2.Rndm()*(maxoffset-minoffset)+minoffset;
		double g2b = rand2.Rndm()*(maxg2-ming2)+ming2;

		cout << "\nphi=" << phi << "\teta=" << eta << "\tcos=" << cosv << "\tv2=" << v2 << "\tg2=" << g2 << "\tg1=" << g1 << "\tgw=" << gw << "\toffset=" << offset << endl;


		const Int_t npar = 11;
	
                Double_t f2params[npar] = {cosv, v2, g2, eta, phi, g1, gw, offset, g2b, eta2, phi2};

                TF2 *f2 = new TF2("f2",fun7,-2,2,-Pi()/2.,3*Pi()/2, npar);
                f2->SetParameters(f2params);
                //f2->SetParLimits(0, -5, 0);
		//f2->FixParameter(1, 0.011704);
		//f2->FixParameter(1, 0.00364);
		if(type == 2) {
                	f2->SetParLimits(1, 0, 5);
                	f2->SetParLimits(2, 0, 5);
		}
                f2->SetParLimits(3, 0.1, 5);
                f2->SetParLimits(4, 0.1, 2);
		if(type == 1 || type == 2)
                	f2->SetParLimits(5, 0, 3);
                f2->SetParLimits(6, 0.01, 7);
		if(type == 1 || type == 2)
                	f2->SetParLimits(8, 0, 3);
		f2->FixParameter(9, eta2);
		f2->FixParameter(10, phi2);

                int fitStatus=plot->Fit("f2","N");
                fitStatus=plot->Fit("f2","EN");

                if(fitStatus==0) {
                        double chisq=f2->GetChisquare();
                        cout << "\n" << f2->GetParameter(2) << endl;
                        chisqsum+=chisq;
                        chisqsqsum+=chisq*chisq;
                        if(chisq<bestchisq) {
                                for(int i=0; i<npar; i++) {
                                        best[i*2]=f2->GetParameter(i);
					best[i*2+1]=f2->GetParError(i);
                                }
                                bestchisq=chisq;
                        }
			if(chisq>worstchisq) {
				worstchisq=chisq;
			}
			numConverge++;
			if(allchisq!=NULL) {allchisq[i]=chisq;}
		} else {
			if(allchisq!=NULL) {allchisq[i]=0.;}
		}
                delete f2;
	}

	cout << "\nOut of " << numIterations << " attempts, " << numConverge << " actually converged." << endl;
        cout << "\nThe best chisq is: " << bestchisq << endl;
        cout << "\nThe worst chisq is: " << worstchisq << endl;
	
	return best;
}
