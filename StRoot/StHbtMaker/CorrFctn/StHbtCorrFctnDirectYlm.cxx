/****************************************************************************
 * $Id: StHbtCorrFctnDirectYlm.cxx,v 1.2 2015/11/02 20:11:06 perev Exp $
 *
 * Author: Yan Yang, hce137@gmail.com
 * **************************************************************************
 * Description: Correlation function that is binned in Ylms directly
 *				Provides a way to store the numerator and denominator
 *				in Ylms directly and correctly calculate the correlation
 *				function from them.
 *				The original author is not known, I guess Kisiel would be.
 * *************************************************************************/

#include "StHbtCorrFctnDirectYlm.h"

#include <TMath.h>
#include "gsl/gsl_blas.h"
#include "gsl/gsl_linalg.h"
#include "gsl/gsl_complex.h"
#include <iostream>
#include <cmath>
#include "TMatrixD.h"

using namespace std;

//#define YY_DEBUG
#define MAXJM 49

#ifdef __ROOT__
ClassImp(StHbtCorrFctnDirectYlm)
#endif

StHbtCorrFctnDirectYlm::StHbtCorrFctnDirectYlm(const char* name, int maxl, int ibin = 30, double vmin = 0.0, double vmax = 0.3)
	: cftnreal(0), cftnimag(0), covnum(0), covden(0), fR2factor(0),
	mDoEMCIC(false), emcicP1P2T(NULL), emcicP1P2Z(NULL), emcicE1E2(NULL), emcicE1plusE2(NULL),
	mName(name), mNbins(ibin), mkmin(vmin), mkmax(vmax),
	mNormRadius(0.0), mNormBohr(0.0), mNormBinMin(0), mNormBinMax(0)
{
	fMaxL = maxl;
	maxjm = (maxl + 1) * (maxl + 1);

	//Fill in factorials table
	factorials = new double[( 4 * (maxl + 1) )];
	int fac = 1;
	factorials[0] = 1;
	for (int iter = 1; iter < 4 * (maxl + 1); iter++) {
		fac *= iter;
		factorials[iter] = fac;
	}

	//Fill in els and ems table
	int el = 0;
	int em = 0;
	int il = 0;
	els	 = new double[maxjm];
	ems	 = new double[maxjm];
	elsi = new int[maxjm];
	emsi = new int[maxjm];
	do {
		els[il]	 = el;
		ems[il]	 = em;
		elsi[il] = (int) el;
		emsi[il] = (int) em;

		//cout << "il el em " << il << " " << elsi[il] << " " << emsi[il] << endl;
		em++;
		il++;
		if (em > el) {
			el++;
			em = -el;
		}
	} while (el <= maxl);

	//Create numerator and denominator histograms
	numsreal = new TH1D*[maxjm];
	numsimag = new TH1D*[maxjm];
	densreal = new TH1D*[maxjm];
	densimag = new TH1D*[maxjm];

	char bufname[200];
	for (int ihist = 0; ihist < maxjm; ihist++) {
		int em = emsi[ihist] < 0 ? elsi[ihist] - emsi[ihist] : emsi[ihist];

		sprintf(bufname, "NumReYlm%i%i%s", elsi[ihist], em, name);
		numsreal[ihist] = new TH1D(bufname, bufname, ibin, vmin, vmax);
		sprintf(bufname, "NumImYlm%i%i%s", elsi[ihist], em, name);
		numsimag[ihist] = new TH1D(bufname, bufname, ibin, vmin, vmax);
		sprintf(bufname, "DenReYlm%i%i%s", elsi[ihist], em, name);
		densreal[ihist] = new TH1D(bufname, bufname, ibin, vmin, vmax);
		sprintf(bufname, "DenImYlm%i%i%s", elsi[ihist], em, name);
		densimag[ihist] = new TH1D(bufname, bufname, ibin, vmin, vmax);

		numsreal[ihist]->Sumw2();
		numsimag[ihist]->Sumw2();
		densreal[ihist]->Sumw2();
		densimag[ihist]->Sumw2();
	}

	sprintf(bufname, "BinCountNum%s", name);
	binctn = new TH1D(bufname, bufname, ibin, vmin, vmax);

	sprintf(bufname, "BinCountDen%s", name);
	binctd = new TH1D(bufname, bufname, ibin, vmin, vmax);

	ylmbuffer = new complex<double>[maxjm];

	//Covariance matrices
	covmnum = new double[maxjm * maxjm * 4 * ibin];
	covmden = new double[maxjm * maxjm * 4 * ibin];
}

//_________________________________________________________________________
void StHbtCorrFctnDirectYlm::DoEMCIC()
{
	mDoEMCIC = true;
	emcicP1P2T = new TH1D*[maxjm];
	emcicP1P2Z = new TH1D*[maxjm];
	emcicE1E2  = new TH1D*[maxjm];
	emcicE1plusE2 = new TH1D*[maxjm];
	char bufname[200];
	for (int ihist = 0; ihist < maxjm; ihist++) {
		int em = emsi[ihist] < 0 ? elsi[ihist] - emsi[ihist] : emsi[ihist];
		sprintf( bufname, "emcicP1P2T%i%i%s", elsi[ihist], em, mName.c_str() );
		emcicP1P2T[ihist] = new TH1D(bufname, bufname, mNbins, mkmin, mkmax);
		emcicP1P2T[ihist]->Sumw2();
		sprintf( bufname, "emcicP1P2Z%i%i%s", elsi[ihist], em, mName.c_str() );
		emcicP1P2Z[ihist] = new TH1D(bufname, bufname, mNbins, mkmin, mkmax);
		emcicP1P2Z[ihist]->Sumw2();
		sprintf( bufname, "emcicE1E2%i%i%s", elsi[ihist], em, mName.c_str() );
		emcicE1E2[ihist] = new TH1D(bufname, bufname, mNbins, mkmin, mkmax);
		emcicE1E2[ihist]->Sumw2();
		sprintf( bufname, "emcicE1plusE2%i%i%s", elsi[ihist], em, mName.c_str() );
		emcicE1plusE2[ihist] = new TH1D(bufname, bufname, mNbins, mkmin, mkmax);
		emcicE1plusE2[ihist]->Sumw2();
	}
}

//__________________________________________________________________________
StHbtCorrFctnDirectYlm::StHbtCorrFctnDirectYlm()
{
	StHbtCorrFctnDirectYlm("StHbtCorrFctnDirectYlm", 2);
}

//___________________________________________________________________________
StHbtCorrFctnDirectYlm::~StHbtCorrFctnDirectYlm()
{
//cout << "StHbtCorrFctnDirectYlm::~StHbtCorrFctnDirectYlm()\n";
	for (int ihist = 0; ihist < maxjm; ihist++) {
		if (numsreal[ihist]) delete numsreal[ihist];
		if (numsimag[ihist]) delete numsimag[ihist];
		if (densreal[ihist]) delete densreal[ihist];
		if (densimag[ihist]) delete densimag[ihist];
		if (cftnreal && cftnreal[ihist]) delete cftnreal[ihist];
		if (cftnimag && cftnimag[ihist]) delete cftnimag[ihist];
		if (mDoEMCIC) {
			delete emcicP1P2T[ihist];
			delete emcicP1P2Z[ihist];
			delete emcicE1E2[ihist];
			delete emcicE1plusE2[ihist];
		}
	}

	if (mDoEMCIC) {
		delete []emcicP1P2T;
		delete []emcicP1P2Z;
		delete []emcicE1E2;
		delete []emcicE1plusE2;
	}

	delete binctn;
	delete binctd;

	delete []numsreal;
	delete []numsimag;
	delete []densreal;
	delete []densimag;
	if (cftnreal) delete []cftnreal;
	if (cftnimag) delete []cftnimag;

	delete []factorials;
	delete []els;
	delete []ems;
	delete []elsi;
	delete []emsi;
	delete []ylmbuffer;

	delete []covmnum;
	delete []covmden;

	delete covnum;
	delete covden;

}

//____________________________________________________________________________
double StHbtCorrFctnDirectYlm::ClebschGordan(double aJot1, double aEm1, double aJot2, double aEm2, double aJot, double aEm)
{
	int	   mint, maxt;
	double cgc = 0.0;
	int	   titer;
	double coef;

	maxt = lrint(aJot1 + aJot2 - aJot);
	mint = 0;
	if (lrint(aJot1 - aEm1) < maxt) maxt = lrint(aJot1 - aEm1);
	if (lrint(aJot2 + aEm2) < maxt) maxt = lrint(aJot2 + aEm2);
	if (lrint( -(aJot - aJot2 + aEm1) ) > mint) mint = lrint( -(aJot - aJot2 + aEm1) );
	if (lrint( -(aJot - aJot1 - aEm2) ) > mint) mint = lrint( -(aJot - aJot1 - aEm2) );

	for (titer = mint; titer <= maxt; titer++) {
		coef  = TMath::Power(-1, titer);
		coef *= TMath::Sqrt( (2 * aJot + 1) *
							 factorials[lrint(aJot1 + aEm1)] *
							 factorials[lrint(aJot1 - aEm1)] *
							 factorials[lrint(aJot2 + aEm2)] *
							 factorials[lrint(aJot2 - aEm2)] *
							 factorials[lrint(aJot + aEm)] *
							 factorials[lrint(aJot - aEm)] );
		coef /= (factorials[titer] *
				 factorials[lrint(aJot1 + aJot2 - aJot - titer)] *
				 factorials[lrint(aJot1 - aEm1 - titer)] *
				 factorials[lrint(aJot2 + aEm2 - titer)] *
				 factorials[lrint(aJot - aJot2 + aEm1 + titer)] *
				 factorials[lrint(aJot - aJot1 - aEm2 + titer)]);

		cgc += coef;
	}

	cgc *= DeltaJ(aJot1, aJot2, aJot);

	return (cgc);
}

//____________________________________________________________________________
double StHbtCorrFctnDirectYlm::DeltaJ(double aJot1, double aJot2, double aJot)
{
	if ( (aJot1 + aJot2 - aJot) < 0 ) {
		//cout << "J1+J2-J3 < 0 !!!" << " " << aJot1 << " " << aJot2 << " " << aJot << endl;
		return (0);
	}
	if ( (aJot1 - aJot2 + aJot) < 0 ) {
		//cout << "J1-J2+J3 < 0 !!!" << " " << aJot1 << " " << aJot2 << " " << aJot << endl;
		return (0);
	}
	if ( (-aJot1 + aJot2 + aJot) < 0 ) {
		//cout << "-J1+J2+J3 < 0 !!!" << " " << aJot1 << " " << aJot2 << " " << aJot << endl;
		return (0);
	}
	if ( (aJot1 + aJot2 + aJot + 1) < 0 ) {
		//cout << "J1+J2+J3+1 < 0 !!!" << " " << aJot1 << " " << aJot2 << " " << aJot << endl;
		return (0);
	}
	double res = TMath::Sqrt(1.0 *
							 factorials[lrint(aJot1 + aJot2 - aJot)] *
							 factorials[lrint(aJot1 - aJot2 + aJot)] *
							 factorials[lrint(-aJot1 + aJot2 + aJot)] /
							 factorials[lrint(aJot1 + aJot2 + aJot + 1)]);

	return (res);
}

//___________________________________________________________________________
double StHbtCorrFctnDirectYlm::WignerSymbol(double aJot1, double aEm1, double aJot2, double aEm2, double aJot, double aEm)
{
	if (lrint(aEm1 + aEm2 + aEm) != 0.0)
		return (0.0);
	double cge = ClebschGordan(aJot1, aEm1, aJot2, aEm2, aJot, -aEm);
	if (lrint( abs(aJot1 - aJot2 - aEm) ) % 2)
		cge *= -1.0;
	cge /= sqrt(2 * aJot + 1);

	if (cge == -0.0) cge = 0.0;

	return (cge);
}

//____________________________________________________________________________
void StHbtCorrFctnDirectYlm::GetMtilde(complex<double>* aMat, double* aMTilde)
{
	//Create the Mtilde for a given q bin
	double lzero, mzero;
	double lprim, mprim;
	double lbis, mbis;

	int lzeroi, mzeroi;
	int lprimi, mprimi;
	int lbisi, mbisi;

	for (int iz = 0; iz < GetMaxJM() * 2; iz++)
		for (int ip = 0; ip < GetMaxJM() * 2; ip++)
			aMTilde[iz * GetMaxJM() * 2 + ip] = 0.0;

	for (int izero = 0; izero < GetMaxJM(); izero++) {
		GetElEmForIndex(izero, &lzero, &mzero);
		GetElEmForIndex(izero, &lzeroi, &mzeroi);
		for (int ibis = 0; ibis < GetMaxJM(); ibis++) {
			GetElEmForIndex(ibis, &lbis, &mbis);
			GetElEmForIndex(ibis, &lbisi, &mbisi);
			complex<double> val = complex<double>(0.0, 0.0);
			complex<double> mcomp[MAXJM];
			for (int iprim = 0; iprim < GetMaxJM(); iprim++) {
				GetElEmForIndex(iprim, &lprim, &mprim);
				GetElEmForIndex(iprim, &lprimi, &mprimi);
				//(-1)^m
				if (abs(mzeroi) % 2) mcomp[iprim] = complex<double>(-1.0, 0.0);
				else mcomp[iprim] = complex<double>(1.0, 0.0);

				//P1
				mcomp[iprim] *= sqrt( (2 * lzero + 1) * (2 * lprim + 1) * (2 * lbis + 1) );
				//W1
				mcomp[iprim] *= WignerSymbol(lzero, 0, lprim, 0, lbis, 0);
				//W2
				mcomp[iprim] *= WignerSymbol(lzero, -mzero, lprim, mprim, lbis, mbis);
				mcomp[iprim] *= aMat[iprim];
				val += mcomp[iprim];
			}

			aMTilde[(izero * 2) * ( 2 * GetMaxJM() ) + (ibis * 2)] = real(val);
			aMTilde[(izero * 2 + 1) * ( 2 * GetMaxJM() ) + (ibis * 2)] = imag(val);
			if (imag(val) != 0.0)
				aMTilde[(izero * 2) * ( 2 * GetMaxJM() ) + (ibis * 2 + 1)] = -imag(val);
			else
				aMTilde[(izero * 2) * ( 2 * GetMaxJM() ) + (ibis * 2 + 1)] = 0.0;
			aMTilde[(izero * 2 + 1) * ( 2 * GetMaxJM() ) + (ibis * 2 + 1)] = real(val);
		}
	}
}

int StHbtCorrFctnDirectYlm::GetMaxJM()
{ return (maxjm); }

void StHbtCorrFctnDirectYlm::GetElEmForIndex(int aIndex, double* aEl, double* aEm)
{
	*aEl = els[aIndex];
	*aEm = ems[aIndex];
}

void StHbtCorrFctnDirectYlm::GetElEmForIndex(int aIndex, int* aEl, int* aEm)
{
	*aEl = elsi[aIndex];
	*aEm = emsi[aIndex];
}

int StHbtCorrFctnDirectYlm::GetBin(int qbin, int ilmzero, int zeroimag, int ilmprim, int primimag)
{
	return (qbin * GetMaxJM() * GetMaxJM() * 4 +
			(ilmprim * 2 + primimag) * GetMaxJM() * 2 +
			ilmzero * 2 + zeroimag);
}

//_____________________________________________________________________________
void StHbtCorrFctnDirectYlm::AddRealPair(const StHbtPair* aPair, double weight)
{
	double qout	 = aPair->dKOut();
	double qside = aPair->dKSide();
	double qlong = aPair->dKLong();
	double kv = sqrt(qout * qout + qside * qside + qlong * qlong);
	if (kv >= mkmax || kv < mkmin) return; //yyang
	int nqbin = binctn->GetXaxis()->FindFixBin(kv) - 1;
	if (fR2factor) weight *= ( 1.0 / (kv * kv) );

	StHbtYlm::YlmUpToL(elsi[GetMaxJM() - 1], qout, qside, qlong, ylmbuffer);
	//check is it not a number
	for (int ilm = 0; ilm < GetMaxJM(); ilm++) {
		if ( TMath::IsNaN(real(ylmbuffer[ilm]) * weight) ) return;
		if ( TMath::IsNaN(imag(ylmbuffer[ilm]) * weight) ) return;
		if ( ::isinf(real(ylmbuffer[ilm]) * weight) ) return;
		if ( ::isinf(imag(ylmbuffer[ilm]) * weight) ) return;
	}
	if ( nqbin < binctn->GetNbinsX() )
		for (int ilmzero = 0; ilmzero < GetMaxJM(); ilmzero++)
			for (int ilmprim = 0; ilmprim < GetMaxJM(); ilmprim++) {
				if ( TMath::IsNaN(real(ylmbuffer[ilmzero]) * real(ylmbuffer[ilmprim]) * weight * weight) ) return;
				if ( TMath::IsNaN(real(ylmbuffer[ilmzero]) * -imag(ylmbuffer[ilmprim]) * weight * weight) ) return;
				if ( TMath::IsNaN(imag(ylmbuffer[ilmzero]) * real(ylmbuffer[ilmprim]) * weight * weight) ) return;
				if ( TMath::IsNaN(imag(ylmbuffer[ilmzero]) * -imag(ylmbuffer[ilmprim]) * weight * weight) ) return;
				if ( ::isinf(real(ylmbuffer[ilmzero]) * real(ylmbuffer[ilmprim]) * weight * weight) ) return;
				if ( ::isinf(real(ylmbuffer[ilmzero]) * -imag(ylmbuffer[ilmprim]) * weight * weight) ) return;
				if ( ::isinf(imag(ylmbuffer[ilmzero]) * real(ylmbuffer[ilmprim]) * weight * weight) ) return;
				if ( ::isinf(imag(ylmbuffer[ilmzero]) * -imag(ylmbuffer[ilmprim]) * weight * weight) ) return;
			}

	for (int ilm = 0; ilm < GetMaxJM(); ilm++) {
		numsreal[ilm]->Fill(kv, real(ylmbuffer[ilm]) * weight);
		numsimag[ilm]->Fill(kv, -imag(ylmbuffer[ilm]) * weight);
		binctn->Fill(kv, 1.0);
	}
	if (mDoEMCIC) {
		double p1p2t = sqrt( aPair->track1()->FourMomentum().px() * aPair->track1()->FourMomentum().py() + aPair->track2()->FourMomentum().px() * aPair->track2(
								 )->FourMomentum().py() );
		double p1p2z = aPair->track1()->FourMomentum().pz() * aPair->track2()->FourMomentum().pz();
		double e1e2	 = aPair->track1()->FourMomentum().e() * aPair->track2()->FourMomentum().e();
		double e1pluse2 = aPair->track1()->FourMomentum().e() + aPair->track2()->FourMomentum().e();
		if ( !( ::isnan(p1p2t) || ::isnan(p1p2z) || ::isnan(e1e2) || ::isnan(e1pluse2) ||
				::isinf(p1p2t) || ::isinf(p1p2z) || ::isinf(e1e2) || ::isinf(e1pluse2) ) )
		{
			for (int ilm = 0; ilm < GetMaxJM(); ilm++) {
				emcicP1P2T[ilm]->Fill(kv, p1p2t);
				emcicP1P2Z[ilm]->Fill(kv, p1p2z);
				emcicE1plusE2[ilm]->Fill(kv, e1pluse2);
				emcicE1E2[ilm]->Fill(kv, e1e2);
			}
		}
	}   //if mDoEMCIC

	//Fill in the error matrix
	//int tabshift = nqbin*GetMaxJM()*GetMaxJM()*4;
	//the following code needs to be revised!! // yyang
	if ( nqbin < binctn->GetNbinsX() )
		for (int ilmzero = 0; ilmzero < GetMaxJM(); ilmzero++)
			for (int ilmprim = 0; ilmprim < GetMaxJM(); ilmprim++) {
				covmnum[GetBin(nqbin, ilmzero, 0, ilmprim, 0)] += real(ylmbuffer[ilmzero])  * real(ylmbuffer[ilmprim])  * weight * weight;
				covmnum[GetBin(nqbin, ilmzero, 0, ilmprim, 1)] += real(ylmbuffer[ilmzero])  * -imag(ylmbuffer[ilmprim]) * weight * weight;
				covmnum[GetBin(nqbin, ilmzero, 1, ilmprim, 0)] += -imag(ylmbuffer[ilmzero]) * real(ylmbuffer[ilmprim])  * weight * weight;
				covmnum[GetBin(nqbin, ilmzero, 1, ilmprim, 1)] += -imag(ylmbuffer[ilmzero]) * -imag(ylmbuffer[ilmprim]) * weight * weight;
			}
}

//____________________________________________________________________________
void StHbtCorrFctnDirectYlm::AddMixedPair(const StHbtPair* aPair, double weight)
{
	double qout	 = aPair->dKOut();
	double qside = aPair->dKSide();
	double qlong = aPair->dKLong();
	double kv = sqrt(qout * qout + qside * qside + qlong * qlong);
	if (kv >= mkmax || kv < mkmin) return; //yyang

	if (fR2factor) weight *= ( 1.0 / (kv * kv) );

	StHbtYlm::YlmUpToL(elsi[GetMaxJM() - 1], qout, qside, qlong, ylmbuffer);
	//check is not a number
	for (int ilm = 0; ilm < GetMaxJM(); ilm++) {
		if ( TMath::IsNaN(real(ylmbuffer[ilm]) * weight) ) return;
		if ( TMath::IsNaN(imag(ylmbuffer[ilm]) * weight) ) return;
	}
	for (int ilmzero = 0; ilmzero < GetMaxJM(); ilmzero++)
		for (int ilmprim = 0; ilmprim < GetMaxJM(); ilmprim++) {
			if ( TMath::IsNaN( real(ylmbuffer[ilmzero]) * real(ylmbuffer[ilmprim])  ) ) return;
			if ( TMath::IsNaN( real(ylmbuffer[ilmzero]) * -imag(ylmbuffer[ilmprim]) ) ) return;
			if ( TMath::IsNaN( imag(ylmbuffer[ilmzero]) * real(ylmbuffer[ilmprim])  ) ) return;
			if ( TMath::IsNaN( imag(ylmbuffer[ilmzero]) * -imag(ylmbuffer[ilmprim]) ) ) return;
		}
	for (int ilm = 0; ilm < GetMaxJM(); ilm++) {
		densreal[ilm]->Fill(kv, real(ylmbuffer[ilm])  * weight);
		densimag[ilm]->Fill(kv, -imag(ylmbuffer[ilm]) * weight);
		binctd->Fill(kv, 1.0);
	}

	//Fill in the error matrix
	int nqbin = binctn->GetXaxis()->FindFixBin(kv) - 1;
	//int tabshift = nqbin*GetMaxJM()*GetMaxJM()*4;
	if ( nqbin < binctn->GetNbinsX() )
		for (int ilmzero = 0; ilmzero < GetMaxJM(); ilmzero++)
			for (int ilmprim = 0; ilmprim < GetMaxJM(); ilmprim++) {
				covmden[GetBin(nqbin, ilmzero, 0, ilmprim, 0)] += real(ylmbuffer[ilmzero])  * real(ylmbuffer[ilmprim]);
				covmden[GetBin(nqbin, ilmzero, 0, ilmprim, 1)] += real(ylmbuffer[ilmzero])  * -imag(ylmbuffer[ilmprim]);
				covmden[GetBin(nqbin, ilmzero, 1, ilmprim, 0)] += -imag(ylmbuffer[ilmzero]) * real(ylmbuffer[ilmprim]);
				covmden[GetBin(nqbin, ilmzero, 1, ilmprim, 1)] += -imag(ylmbuffer[ilmzero]) * -imag(ylmbuffer[ilmprim]);
			}
}

//________________________________________________________
void StHbtCorrFctnDirectYlm::Finish()
{
	PackCovariances();
//CalcCorrFctn();

	for (int ilm = 0; ilm < maxjm; ilm++) {
		numsreal[ilm]->Write();
		numsimag[ilm]->Write();
		densreal[ilm]->Write();
		densimag[ilm]->Write();
		if (cftnreal && cftnreal[ilm]) cftnreal[ilm]->Write();
		if (cftnimag && cftnimag[ilm]) cftnimag[ilm]->Write();
		if (mDoEMCIC) {
			emcicP1P2T[ilm]->Write();
			emcicP1P2Z[ilm]->Write();
			emcicE1E2[ilm]->Write();
			emcicE1plusE2[ilm]->Write();
		}
	}
	if (covnum) covnum->Write();
	if (covden) covden->Write();
}

//_________________________________________________________________
void StHbtCorrFctnDirectYlm::Write(TFile* rfile)
{
	rfile->cd();
	for (int ilm = 0; ilm < maxjm; ilm++) {
		numsreal[ilm]->Write();
		numsimag[ilm]->Write();
		densreal[ilm]->Write();
		densimag[ilm]->Write();
		if (cftnreal && cftnreal[ilm]) cftnreal[ilm]->Write();
		if (cftnimag && cftnimag[ilm]) cftnimag[ilm]->Write();
		if (mDoEMCIC) {
			emcicP1P2T[ilm]->Write();
			emcicP1P2Z[ilm]->Write();
			emcicE1E2[ilm]->Write();
			emcicE1plusE2[ilm]->Write();
		}
	}
	if (covnum) covnum->Write();
	if (covden) covden->Write();
	rfile->Write();
}

void StHbtCorrFctnDirectYlm::ReadFromFile(TFile* ifile)
{
	/* yyang
	 * this function is used for read hadded */

	//cout << "Reading in numerators and denominators" << endl;
	char bufname[200];
	for (int ihist = 0; ihist < maxjm; ihist++) {
		int em = emsi[ihist] < 0 ? elsi[ihist] - emsi[ihist] : emsi[ihist];
		sprintf(bufname, "NumReYlm%i%i%s", elsi[ihist], em, mName.c_str());
		if (numsreal[ihist]) delete numsreal[ihist];
		numsreal[ihist] = new TH1D( *( (TH1D*) ifile->Get(bufname) ) );

		sprintf(bufname, "NumImYlm%i%i%s", elsi[ihist], em, mName.c_str());
		if (numsimag[ihist]) delete numsimag[ihist];
		numsimag[ihist] = new TH1D( *( (TH1D*) ifile->Get(bufname) ) );

		sprintf(bufname, "DenReYlm%i%i%s", elsi[ihist], em, mName.c_str());
		if (densreal[ihist]) delete densreal[ihist];
		densreal[ihist] = new TH1D( *( (TH1D*) ifile->Get(bufname) ) );

		sprintf(bufname, "DenImYlm%i%i%s", elsi[ihist], em, mName.c_str());
		if (densimag[ihist]) delete densimag[ihist];
		densimag[ihist] = new TH1D( *( (TH1D*) ifile->Get(bufname) ) );

		sprintf(bufname, "CfnReYlm%i%i%s", elsi[ihist], em, mName.c_str());
		string keyname(bufname);
		if (ifile->FindKey( (keyname + ";*").c_str() ) ) ifile->Delete( (keyname + ";*").c_str() );

		sprintf(bufname, "CfnImYlm%i%i%s", elsi[ihist], em, mName.c_str());
		keyname = string(bufname);
		if (ifile->FindKey( (keyname + ";*").c_str() ) ) ifile->Delete( (keyname + ";*").c_str() );
	}

	if (covnum) delete covnum;
	sprintf(bufname, "CovNum%s", mName.c_str());
	covnum = new TH3D ( *( (TH3D*) ifile->Get(bufname) ) );

	if (covden) delete covden;
	sprintf(bufname, "CovDen%s", mName.c_str() );
	covden = new TH3D ( *( (TH3D*) ifile->Get(bufname) ) );

	if ( (covnum) && (covden) ) {
		//cout << "Unpacking covariance matrices from file " << endl;
		UnpackCovariances();
	}
	else {
		cout << "Creating fake covariance matrices" << endl;

		for (int ibin = 1; ibin <= numsreal[0]->GetNbinsX(); ibin++) {
		   double nent	= numsreal[0]->GetEntries();
		   double nentd = densreal[0]->GetEntries();
		   for (int ilmx = 0; ilmx < GetMaxJM(); ilmx++) {
			   for (int ilmy = 0; ilmy < GetMaxJM(); ilmy++) {
				   double t1t2rr = numsreal[ilmx]->GetBinContent(ibin) * numsreal[ilmy]->GetBinContent(ibin) / nent / nent;
				   double t1t2ri = numsreal[ilmx]->GetBinContent(ibin) * numsimag[ilmy]->GetBinContent(ibin) / nent / nent;
				   double t1t2ir = numsimag[ilmx]->GetBinContent(ibin) * numsreal[ilmy]->GetBinContent(ibin) / nent / nent;
				   double t1t2ii = numsimag[ilmx]->GetBinContent(ibin) * numsimag[ilmy]->GetBinContent(ibin) / nent / nent;
				   if (ilmx == ilmy) {
					   covmnum[GetBin(ibin - 1, ilmx, 0, ilmy, 0)] = nent * (TMath::Power(numsreal[ilmx]->GetBinError(ibin) / nent, 2) * (nent - 1) + t1t2rr);
					   covmnum[GetBin(ibin - 1, ilmx, 0, ilmy, 1)] = nent * t1t2ri;
					   covmnum[GetBin(ibin - 1, ilmx, 1, ilmy, 0)] = nent * t1t2ir;
					   covmnum[GetBin(ibin - 1, ilmx, 1, ilmy, 1)] = nent * (TMath::Power(numsimag[ilmx]->GetBinError(ibin) / nent, 2) * (nent - 1) + t1t2rr);
				   }
				   else {
					   covmnum[GetBin(ibin - 1, ilmx, 0, ilmy, 0)] = nent * t1t2rr;
					   covmnum[GetBin(ibin - 1, ilmx, 0, ilmy, 1)] = nent * t1t2ri;
					   covmnum[GetBin(ibin - 1, ilmx, 1, ilmy, 0)] = nent * t1t2ir;
					   covmnum[GetBin(ibin - 1, ilmx, 1, ilmy, 1)] = nent * t1t2ii;
				   }
				   t1t2rr = densreal[ilmx]->GetBinContent(ibin) * densreal[ilmy]->GetBinContent(ibin) / nentd / nentd;
				   t1t2ri = densreal[ilmx]->GetBinContent(ibin) * densimag[ilmy]->GetBinContent(ibin) / nentd / nentd;
				   t1t2ir = densimag[ilmx]->GetBinContent(ibin) * densreal[ilmy]->GetBinContent(ibin) / nentd / nentd;
				   t1t2ii = densimag[ilmx]->GetBinContent(ibin) * densimag[ilmy]->GetBinContent(ibin) / nentd / nentd;

				   covmden[GetBin(ibin - 1, ilmx, 0, ilmy, 0)] = nentd * t1t2rr;
				   covmden[GetBin(ibin - 1, ilmx, 0, ilmy, 1)] = nentd * t1t2ri;
				   covmden[GetBin(ibin - 1, ilmx, 1, ilmy, 0)] = nentd * t1t2ir;
				   covmden[GetBin(ibin - 1, ilmx, 1, ilmy, 1)] = nentd * t1t2ii;
			   }
		   }
	   }
	}

	//Recalculating the correlation functions
	CalcCorrFctn();

	//save cf
	ifile->cd();
	for (int ilm = 0; ilm < maxjm; ilm++) {
		cftnreal[ilm]->Write();
		cftnimag[ilm]->Write();
	}
}

int StHbtCorrFctnDirectYlm::PackYlmVector(double* invec, double* outvec)
{
	int ioutcount = 0;
	int em, el;
	for (int ilm = 0; ilm < GetMaxJM(); ilm++) {
		GetElEmForIndex(ilm, &el, &em);
		outvec[ioutcount++] = invec[ilm * 2];
		if (em == 0) continue;
		outvec[ioutcount++] = invec[ilm * 2 + 1];
	}

	return (ioutcount);
}

int StHbtCorrFctnDirectYlm::PackYlmMatrix(double* inmat, double* outmat)
{
	int ioutcountz = 0;
	int ioutcountp = 0;
	int emz, elz;
	int emp, elp;
	int finalsize = 0;

	for (int ilm = 0; ilm < GetMaxJM(); ilm++) {
		GetElEmForIndex(ilm, &elz, &emz);
		finalsize++;
		if (emz == 0) continue;
		finalsize++;
	}

	for (int ilmz = 0; ilmz < GetMaxJM(); ilmz++) {
		GetElEmForIndex(ilmz, &elz, &emz);
		ioutcountp = 0;
		for (int ilmp = 0; ilmp < GetMaxJM(); ilmp++) {
			GetElEmForIndex(ilmp, &elp, &emp);
			outmat[ioutcountz * finalsize + ioutcountp] = inmat[GetBin(0, ilmz, 0, ilmp, 0)];
			ioutcountp++;
			if (emp == 0) continue;
			outmat[ioutcountz * finalsize + ioutcountp] = inmat[GetBin(0, ilmz, 0, ilmp, 1)];
			ioutcountp++;
		}
		ioutcountz++;

		if (emz == 0) continue;
		ioutcountp = 0;
		for (int ilmp = 0; ilmp < GetMaxJM(); ilmp++) {
			GetElEmForIndex(ilmp, &elp, &emp);
			outmat[ioutcountz * finalsize + ioutcountp] = inmat[GetBin(0, ilmz, 1, ilmp, 0)];
			ioutcountp++;
			if (emp == 0) continue;
			outmat[ioutcountz * finalsize + ioutcountp] = inmat[GetBin(0, ilmz, 1, ilmp, 1)];
			ioutcountp++;
		}
		ioutcountz++;
	}

	return (ioutcountz);
}

void StHbtCorrFctnDirectYlm::PackCovariances()
{
	char bufname[200];

	sprintf(bufname, "CovNum%s", numsreal[0]->GetName() + 10);
	if (covnum) delete covnum;
	covnum = new TH3D(  bufname, bufname,
						numsreal[0]->GetNbinsX(), numsreal[0]->GetXaxis()->GetXmin(), numsreal[0]->GetXaxis()->GetXmax(),
						GetMaxJM() * 2, -0.5, GetMaxJM() * 2 - 0.5,
						GetMaxJM() * 2, -0.5, GetMaxJM() * 2 - 0.5);

	for (int ibin = 1; ibin <= covnum->GetNbinsX(); ibin++)
		for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++)
			for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++)
				covnum->SetBinContent(ibin, ilmz + 1, ilmp + 1, covmnum[GetBin(ibin - 1, ilmz / 2, ilmz % 2, ilmp / 2, ilmp % 2)]);

	sprintf(bufname, "CovDen%s", numsreal[0]->GetName() + 10);
	if (covden) delete covden;
	covden = new TH3D( bufname, bufname,
					   densreal[0]->GetNbinsX(), densreal[0]->GetXaxis()->GetXmin(), densreal[0]->GetXaxis()->GetXmax(),
					   GetMaxJM() * 2, -0.5, GetMaxJM() * 2 - 0.5,
					   GetMaxJM() * 2, -0.5, GetMaxJM() * 2 - 0.5);

	for (int ibin = 1; ibin <= covden->GetNbinsX(); ibin++)
		for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++)
			for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++)
				covden->SetBinContent(ibin, ilmz + 1, ilmp + 1, covmden[GetBin(ibin - 1, ilmz / 2, ilmz % 2, ilmp / 2, ilmp % 2)]);

}

void StHbtCorrFctnDirectYlm::UnpackCovariances()
{
	if (covnum) {
		for (int ibin = 1; ibin <= covnum->GetNbinsX(); ibin++)
			for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++)
				for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++)
					covmnum[GetBin(ibin - 1, ilmz / 2, ilmz % 2, ilmp / 2, ilmp % 2)] = covnum->GetBinContent(ibin, ilmz + 1, ilmp + 1);
	}
	if (covden) {
		for (int ibin = 1; ibin <= covden->GetNbinsX(); ibin++)
			for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++)
				for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++)
					covmden[GetBin(ibin - 1, ilmz / 2, ilmz % 2, ilmp / 2, ilmp % 2)] = covden->GetBinContent(ibin, ilmz + 1, ilmp + 1);
	}
}

int StHbtCorrFctnDirectYlm::GetIndexForLM(int el, int em)
{
	for (int iter = 0; iter < maxjm; iter++)
		if ( (el == elsi[iter]) && (em == emsi[iter]) )
			return (iter);
	return (-1);
}

TH1D* StHbtCorrFctnDirectYlm::GetNumRealHist(int el, int em)
{
	if (GetIndexForLM(el, em) >= 0)
		return (numsreal[GetIndexForLM(el, em)]);
	else
		return (0);
}

TH1D* StHbtCorrFctnDirectYlm::GetNumImagHist(int el, int em)
{
	if (GetIndexForLM(el, em) >= 0)
		return (numsimag[GetIndexForLM(el, em)]);
	else
		return (0);
}

TH1D* StHbtCorrFctnDirectYlm::GetDenRealHist(int el, int em)
{
	if (GetIndexForLM(el, em) >= 0)
		return (densreal[GetIndexForLM(el, em)]);
	else
		return (0);
}

TH1D* StHbtCorrFctnDirectYlm::GetDenImagHist(int el, int em)
{
	if (GetIndexForLM(el, em) >= 0)
		return (densimag[GetIndexForLM(el, em)]);
	else
		return (0);
}

StHbtString StHbtCorrFctnDirectYlm::Report()
{
	return ("StHbtCorrFctnDirectYlm::Finish");
}

void StHbtCorrFctnDirectYlm::SetR2Factor(bool aFactor)
{
	fR2factor = aFactor;
}

//__________________________________________________________________________
void StHbtCorrFctnDirectYlm::CalcCorrFctn()
{
	cftnreal = new TH1D*[maxjm];
	cftnimag = new TH1D*[maxjm];

	char bufname[200];
	for (int ihist = 0; ihist < maxjm; ihist++) {
		int em = emsi[ihist] < 0 ? elsi[ihist] - emsi[ihist] : emsi[ihist];

		sprintf( bufname, "CfnReYlm%i%i%s", elsi[ihist], em, mName.c_str() );
		cftnreal[ihist] = new TH1D(bufname, bufname, mNbins, mkmin, mkmax);
		sprintf( bufname, "CfnImYlm%i%i%s", elsi[ihist], em, mName.c_str() );
		cftnimag[ihist] = new TH1D(bufname, bufname, mNbins, mkmin, mkmax);

		cftnreal[ihist]->Sumw2();
		cftnimag[ihist]->Sumw2();
	}

	covmcfc = new double[maxjm * maxjm * 4 * mNbins];

	complex<double> tMq0[maxjm];
	complex<double> tTq0[maxjm];
	double			tMTilde[maxjm * maxjm * 4];
	complex<double> tCq0[maxjm];

	int recalccov = 1;
	if ( (covnum) && (covnum->GetBinContent(0, 0, 0) > 0.0) ) {
		cout << "Detected calculated covariance matrix. Do not recalculate !!!\n";
		recalccov = 0;
	}

	//If requested calculate the advanced normalization factor
	//*** WARNING !!! ***
	//This is the factor that assumes that the correlation function
	//calculated is the non-identical particle correlation function
	//dominated by the asymptotic behaviour of the Coulmb interaction
	//in the normalization region
	//For any other function use normal normalization !!!
	//*** WARNING !!! ***

	double normfactor = 1.0;
	if (mNormBinMax > 0) {
		double sksum = 0.0;
		double wksum = 0.0;

		double sk, wk, ks;
		if (mNormBinMin < 1) mNormBinMin = 1;
		if ( mNormBinMax > densreal[0]->GetNbinsX() )
			mNormBinMax = densreal[0]->GetNbinsX();
		
		for (int ib = mNormBinMin; ib <= mNormBinMax; ib++) {
			ks = densreal[0]->GetXaxis()->GetBinCenter(ib);
			sk = numsreal[0]->GetBinContent(ib) / ( densreal[0]->GetBinContent(ib) * ( 1.0 - mNormPurity / (mNormRadius * mNormBohr * ks * ks) ) );
			wk = numsreal[0]->GetBinContent(ib);
			sksum += sk * wk;
			wksum += wk;
		}
		normfactor *= sksum / wksum;
		normfactor /= numsreal[0]->GetEntries() / densreal[0]->GetEntries();
	}

	for (int ibin = 1; ibin <= numsreal[0]->GetNbinsX(); ibin++) {
		for (int ilm = 0; ilm < maxjm; ilm++) {
			if (recalccov) {
				tMq0[ilm] = complex<double>( densreal[ilm]->GetBinContent(ibin) / (densreal[0]->GetEntries() / normfactor),
											 densimag[ilm]->GetBinContent(ibin) / (densreal[0]->GetEntries() / normfactor) );
				tTq0[ilm] = complex<double>( numsreal[ilm]->GetBinContent(ibin) / numsreal[0]->GetEntries(),
											 numsimag[ilm]->GetBinContent(ibin) / numsreal[0]->GetEntries() );
			}
			else {
				tMq0[ilm] = complex<double>(densreal[ilm]->GetBinContent(ibin) / normfactor,
											densimag[ilm]->GetBinContent(ibin) / normfactor);
				tTq0[ilm] = complex<double>( numsreal[ilm]->GetBinContent(ibin),
											 numsimag[ilm]->GetBinContent(ibin) );
			}
		}

		//Calculate the proper error matrix for T
		//from the temporary covariance matrices
		if (recalccov) {
			for (int ilmzero = 0; ilmzero < GetMaxJM(); ilmzero++) {
				for (int ilmprim = 0; ilmprim < GetMaxJM(); ilmprim++) {
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)]) ) {
						cout << "NaN !!!! RR " << ilmzero << " " << ilmprim << endl;
					}
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)]) ) {
						cout << "NaN !!!! RI " << ilmzero << " " << ilmprim << endl;
					}
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)]) ) {
						cout << "NaN !!!! IR " << ilmzero << " " << ilmprim << endl;
					}
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)]) ) {
						cout << "NaN !!!! II " << ilmzero << " " << ilmprim << endl;
					}

					covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)] /= numsreal[0]->GetEntries();
					covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)] /= numsreal[0]->GetEntries();
					covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)] /= numsreal[0]->GetEntries();
					covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)] /= numsreal[0]->GetEntries();

					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)]) ) {
						cout << "NaN !!!! RR" << endl;
					}
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)]) ) {
						cout << "NaN !!!! RI" << endl;
					}
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)]) ) {
						cout << "NaN !!!! IR" << endl;
					}
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)]) ) {
						cout << "NaN !!!! II" << endl;
					}

					covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)] -= real(tTq0[ilmzero]) * real(tTq0[ilmprim]);
					covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)] -= real(tTq0[ilmzero]) * imag(tTq0[ilmprim]);
					covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)] -= imag(tTq0[ilmzero]) * real(tTq0[ilmprim]);
					covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)] -= imag(tTq0[ilmzero]) * imag(tTq0[ilmprim]);

					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)]) ) {
						cout << "NaN !!!! RR" << endl;
					}
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)]) ) {
						cout << "NaN !!!! RI" << endl;
					}
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)]) ) {
						cout << "NaN !!!! IR" << endl;
					}
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)]) ) {
						cout << "NaN !!!! II" << endl;
					}

					covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)] /= (numsreal[0]->GetEntries() - 1);
					covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)] /= (numsreal[0]->GetEntries() - 1);
					covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)] /= (numsreal[0]->GetEntries() - 1);
					covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)] /= (numsreal[0]->GetEntries() - 1);

					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 0)]) ) {
						cout << "NaN !!!! RR" << endl;
					}
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 0, ilmprim, 1)]) ) {
						cout << "NaN !!!! RI" << endl;
					}
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 0)]) ) {
						cout << "NaN !!!! IR" << endl;
					}
					if ( ::isnan(covmnum[GetBin(ibin - 1, ilmzero, 1, ilmprim, 1)]) ) {
						cout << "NaN !!!! II" << endl;
					}
				}
			}
		}
		GetMtilde(tMq0, tMTilde);

		//Perform the solution for the correlation function itself and the errors
		double mDeltaT[maxjm * maxjm * 4];
		for (int ilmzero = 0; ilmzero < GetMaxJM() * 2; ilmzero++)
			for (int ilmprim = 0; ilmprim < GetMaxJM() * 2; ilmprim++)
				mDeltaT[(ilmzero * maxjm * 2) + ilmprim] = (covmnum[GetBin(ibin - 1, ilmzero / 2, ilmzero % 2, ilmprim / 2, ilmprim % 2)]);

#ifdef YY_DEBUG
		cout << "Delta T matrix " << endl;
		for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++) {
			for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++) {
				cout.precision(3);
				cout.width(10);
				cout << mDeltaT[ilmz * GetMaxJM() * 2 + ilmp];
			}
			cout << endl;
		}
#endif

		double mDeltaTPacked[maxjm * maxjm * 4];
		int	   msize = PackYlmMatrixIndependentOnly(mDeltaT, mDeltaTPacked);

#ifdef YY_DEBUG
		cout << "Delta T matrix packed " << endl;
		for (int ilmz = 0; ilmz < msize; ilmz++) {
			for (int ilmp = 0; ilmp < msize; ilmp++) {
				cout.precision(3);
				cout.width(10);
				cout << mDeltaTPacked[ilmz * msize + ilmp];
			}
			cout << endl;
		}
#endif

		//(1) Solve (DeltaT)^1 Mtilde = Q
		//Prepare halper matrices
		double mM[maxjm * maxjm * 4];
		double mMPacked[maxjm * maxjm * 4];
		for (int iter = 0; iter < maxjm * maxjm * 4; iter++)
			mM[iter] = tMTilde[iter];
		PackYlmMatrixIndependentOnly(mM, mMPacked);

		gsl_matrix_view matM = gsl_matrix_view_array(mMPacked, msize, msize);

#ifdef YY_DEBUG
		cout << "Mtilde matrix " << endl;
		for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++) {
			for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++) {
				cout.precision(3);
				cout.width(10);
				cout << mM[ilmz * GetMaxJM() * 2 + ilmp];
			}
			cout << endl;
		}
		cout << "Mtilde matrix packed " << endl;
		for (int ilmz = 0; ilmz < msize; ilmz++) {
			for (int ilmp = 0; ilmp < msize; ilmp++) {
				cout.precision(3);
				cout.width(10);
				cout << mMPacked[ilmz * msize + ilmp];
			}
			cout << endl;
		}
#endif

		//Inverting matrix DeltaT.
		double mU[maxjm * maxjm * 4];
		InvertYlmIndependentMatrix(mDeltaT, mU);

		double mDTInvertedPacked[maxjm * maxjm * 4];
		PackYlmMatrixIndependentOnly(mU, mDTInvertedPacked);

		gsl_matrix_view matDTI = gsl_matrix_view_array(mDTInvertedPacked, msize, msize);
#ifdef YY_DEBUG
		cout << "Delta T matrix inverted packed " << endl;
		for (int ilmz = 0; ilmz < msize; ilmz++) {
			for (int ilmp = 0; ilmp < msize; ilmp++) {
				cout.precision(3);
				cout.width(10);
				cout << mDTInvertedPacked[ilmz * msize + ilmp];
			}
			cout << endl;
		}
#endif

		//(2) Multiply DeltaT^1 M = Q
		double mQ[maxjm * maxjm * 4];
		for (int iter = 0; iter < msize * msize; iter++)
			mQ[iter] = 0.0;
		gsl_matrix_view matQ = gsl_matrix_view_array(mQ, msize, msize);
		gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, &matDTI.matrix, &matM.matrix, 0.0, &matQ.matrix);

		double			mTest[maxjm * maxjm * 4];
		gsl_matrix_view matTest = gsl_matrix_view_array(mTest, msize, msize);

		double mF[maxjm * maxjm * 4];
		for (int iter = 0; iter < maxjm * maxjm * 4; iter++)
			mF[iter] = mDeltaTPacked[iter];
		gsl_matrix_view matF = gsl_matrix_view_array(mF, msize, msize);
		gsl_blas_dgemm(CblasTrans, CblasNoTrans, 1.0, &matF.matrix, &matQ.matrix, 0.0, &matTest.matrix);
#ifdef YY_DEBUG
		cout << "Test matrix packed - compare to Mtilde" << endl;
		for (int ilmz = 0; ilmz < msize; ilmz++) {
			for (int ilmp = 0; ilmp < msize; ilmp++) {
				cout.precision(3);
				cout.width(10);
				cout << mTest[ilmz * msize + ilmp];
			}
			cout << endl;
		}
#endif
		//(2) Multiply Mtilde^T Q = P
		double mP[maxjm * maxjm * 4];
		for (int iter = 0; iter < maxjm * maxjm * 4; iter++)
			mP[iter] = 0;

		gsl_matrix_view matP = gsl_matrix_view_array(mP, msize, msize);
		gsl_blas_dgemm(CblasTrans, CblasNoTrans, 1.0, &matM.matrix, &matQ.matrix, 0.0, &matP.matrix);

#ifdef YY_DEBUG
		cout << "P matrix packed" << endl;
		for (int ilmz = 0; ilmz < msize; ilmz++) {
			for (int ilmp = 0; ilmp < msize; ilmp++) {
				cout.precision(3);
				cout.width(10);
				cout << mP[ilmz * msize + ilmp];
			}
			cout << endl;
		}
#endif

		//(3) Solve P^-1 Mtilde^T = R
		double mPUnpacked[maxjm * maxjm * 4];
		UnPackYlmMatrixIndependentOnly(mP, mPUnpacked, msize);

#ifdef YY_DEBUG
		cout << "P matrix unpacked" << endl;
		for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++) {
			for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++) {
				cout.precision(3);
				cout.width(10);
				cout << mPUnpacked[ilmz * GetMaxJM() * 2 + ilmp];
			}
			cout << endl;
		}
#endif

		//Invert the P matrix
		double mPInverted[maxjm * maxjm * 4];
		InvertYlmIndependentMatrix(mPUnpacked, mPInverted);

		double mPInvertedPacked[maxjm * maxjm * 4];
		PackYlmMatrixIndependentOnly(mPInverted, mPInvertedPacked);

		gsl_matrix_view matPI = gsl_matrix_view_array(mPInvertedPacked, msize, msize);

#ifdef YY_DEBUG
		cout << "P matrix inverted packed" << endl;
		for (int ilmz = 0; ilmz < msize; ilmz++) {
			for (int ilmp = 0; ilmp < msize; ilmp++) {
				cout.precision(3);
				cout.width(10);
				cout << mPInvertedPacked[ilmz * msize + ilmp];
			}
			cout << endl;
		}
#endif

		double mR[maxjm * maxjm * 4];
		for (int ir = 0; ir < maxjm * maxjm * 4; ir++)
			mR[ir] = 0.0;
		gsl_matrix_view matR = gsl_matrix_view_array(mR, msize, msize);


		//(2) Multiply P^-1 M (Trans) = R
#ifdef YY_DEBUG
		cout << "Matrix M Packed " << endl;
		for (int ilmz = 0; ilmz < msize; ilmz++) {
			for (int ilmp = 0; ilmp < msize; ilmp++) {
				cout.precision(3);
				cout.width(10);
				cout << mMPacked[ilmz * msize + ilmp];
			}
			cout << endl;
		}
#endif

		gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, &matPI.matrix, &matM.matrix, 1.0, &matR.matrix);

#ifdef YY_DEBUG
		cout << "R matrix packed " << endl;
		for (int ilmz = 0; ilmz < msize; ilmz++) {
			for (int ilmp = 0; ilmp < msize; ilmp++) {
				cout.precision(3);
				cout.width(10);
				cout << mR[ilmz * msize + ilmp];
			}
			cout << endl;
		}
#endif

		//(4) Solve DeltaT^-1 T = L
		double			vL[maxjm * 2];
		gsl_vector_view vecL = gsl_vector_view_array(vL, msize);

		//Decomposing the M matrix
		//gsl_linalg_SV_decomp(&matF.matrix, &matS.matrix, &vecST.vector, &vecWT.vector);

		double vB[maxjm * 2];
		for (int iter = 0; iter < GetMaxJM(); iter++) {
			vB[iter * 2] = real(tTq0[iter]);
			vB[iter * 2 + 1] = imag(tTq0[iter]);
		}

		double vBPacked[maxjm * 2];
		PackYlmVectorIndependentOnly(vB, vBPacked);

		gsl_vector_view vecB = gsl_vector_view_array(vBPacked, msize);


		//Solving the problem
		//gsl_linalg_SV_solve(&matF.matrix, &matS.matrix, &vecST.vector, &vecB.vector, &vecL.vector);

#ifdef YY_DEBUG
		cout << "L vector packed " << endl;
		for (int ilmp = 0; ilmp < msize; ilmp++) {
			cout.precision(3);
			cout.width(10);
			cout << vL[ilmp];
		}
		cout << endl;
#endif

		//Multiply DeltaT^-1 T = L
		gsl_blas_dgemv(CblasNoTrans, 1.0, &matDTI.matrix, &vecB.vector, 0.0, &vecL.vector);

		//(5) Multiply R L = C
		double vY[maxjm * 2];
		for (int iter = 0; iter < GetMaxJM() * 2; iter++) {
			vY[iter] = 0.0;
		}

		//Prepare inputs for solving the problem
		gsl_vector_view vecY = gsl_vector_view_array(vY, msize);
		gsl_blas_dgemv (CblasNoTrans, 1.0, &matR.matrix, &vecL.vector, 0.0, &vecY.vector);

#ifdef YY_DEBUG
		cout << "C vector packed" << endl;
		for (int ilmp = 0; ilmp < msize; ilmp++) {
			cout.precision(3);
			cout.width(10);
			cout << vY[ilmp];
		}
		cout << endl;
#endif


		int mpack = 0;
		int el, em;
		for (int ilm = 0; ilm < maxjm; ilm++) {
			//cftnreal[ilm]->SetBinContent(ibin, vC[mpack++]);
			GetElEmForIndex(ilm, &el, &em);
			if (em < 0) {
				cftnreal[ilm]->SetBinContent(ibin, 0.0);
				cftnimag[ilm]->SetBinContent(ibin, 0.0);
			}
			else {
				cftnreal[ilm]->SetBinContent(ibin, vY[mpack++]);
				if (em == 0)
					cftnimag[ilm]->SetBinContent(ibin, 0);
				else
					//cftnimag[ilm]->SetBinContent(ibin, vC[mpack++]);
					cftnimag[ilm]->SetBinContent(ibin, vY[mpack++]);
			}
		}

		//Invert V
		mpack = 0;
		for (int ilm = 0; ilm < maxjm; ilm++) {
			GetElEmForIndex(ilm, &el, &em);
			if (em < 0 ) {
				cftnreal[ilm]->SetBinError(ibin, 0);
				cftnimag[ilm]->SetBinError(ibin, 0);
			}
			else {
				cftnreal[ilm]->SetBinError( ibin, sqrt( fabs(mPInvertedPacked[mpack * msize + mpack]) ) );
				mpack++;
				if (em == 0)
					cftnimag[ilm]->SetBinError(ibin, 0);
				else {
					cftnimag[ilm]->SetBinError( ibin, sqrt( fabs(mPInvertedPacked[mpack * msize + mpack]) ) );
					mpack++;
				}
			}
		}

		for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++) {
			for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++) {
				if (ilmp > ilmz)
					covmcfc[GetBin(ibin - 1, ilmz / 2, ilmz % 2, ilmp / 2, ilmp % 2)] = mPInverted[ilmz * GetMaxJM() * 2 + ilmp];
				else
					covmcfc[GetBin(ibin - 1, ilmz / 2, ilmz % 2, ilmp / 2, ilmp % 2)] = mPInverted[ilmp * GetMaxJM() * 2 + ilmz];
			}
		}
	} //for (int ibin=1; ibin<=numsreal[0]->GetNbinsX(); ibin++)
	

//PackCfcCovariance()
}

//______________________________________________________________________
int StHbtCorrFctnDirectYlm::PackYlmMatrixIndependentOnly(double* inmat, double* outmat)
{
	int ioutcountz = 0;
	int ioutcountp = 0;
	int emz, elz;
	int emp, elp;
	int finalsize = 0;

	for (int ilm = 0; ilm < GetMaxJM(); ilm++) {
		GetElEmForIndex(ilm, &elz, &emz);
		if (emz < 0) continue;
		finalsize++;
		if (emz == 0) continue;
		finalsize++;
	}
	//cout << "Final size " << finalsize << endl;

	for (int ilmz = 0; ilmz < GetMaxJM(); ilmz++) {
		GetElEmForIndex(ilmz, &elz, &emz);
		ioutcountp = 0;

		if (emz < 0) continue;
		for (int ilmp = 0; ilmp < GetMaxJM(); ilmp++) {
			GetElEmForIndex(ilmp, &elp, &emp);
			if (emp < 0) continue;
			outmat[ioutcountz * finalsize + ioutcountp] = inmat[GetBin(0, ilmz, 0, ilmp, 0)];
			ioutcountp++;
			if (emp == 0) continue;
			outmat[ioutcountz * finalsize + ioutcountp] = inmat[GetBin(0, ilmz, 0, ilmp, 1)];
			ioutcountp++;
		}
		ioutcountz++;

		if (emz == 0) continue;
		ioutcountp = 0;
		for (int ilmp = 0; ilmp < GetMaxJM(); ilmp++) {
			GetElEmForIndex(ilmp, &elp, &emp);
			if (emp < 0) continue;
			outmat[ioutcountz * finalsize + ioutcountp] = inmat[GetBin(0, ilmz, 1, ilmp, 0)];
			ioutcountp++;
			if (emp == 0) continue;
			outmat[ioutcountz * finalsize + ioutcountp] = inmat[GetBin(0, ilmz, 1, ilmp, 1)];
			ioutcountp++;
		}
		ioutcountz++;
	}
	return (ioutcountz);
}

//_____________________________________________________________________
void StHbtCorrFctnDirectYlm::InvertYlmIndependentMatrix(double* inmat, double* outmat)
{
	//Invert the Ylm matrix by inverting only the matrix
	//with independent elements and filling in the rest
	//according to sign rules

	double mU[maxjm * maxjm * 4];
	int	   isize = PackYlmMatrixIndependentOnly(inmat, mU);
	//cout << "Independent count " << isize << endl;

	gsl_matrix_view matU = gsl_matrix_view_array(mU, isize, isize);

#ifdef YY_DEBUG
	cout << "Input matrix independent only " << endl;
	for (int ilmz = 0; ilmz < isize; ilmz++) {
		for (int ilmp = 0; ilmp < isize; ilmp++) {
			cout.precision(3);
			cout.width(10);
			cout << mU[ilmz * isize + ilmp];
		}
		cout << endl;
	}
#endif

	//Identity matrix helper for inversion
	double mI[maxjm * maxjm * 4];
	for (int iterm = 0; iterm < isize; iterm++)
		for (int iterp = 0; iterp < isize; iterp++)
			if (iterm == iterp)
				mI[iterm * isize + iterp] = 1.0;
			else
				mI[iterm * isize + iterp] = 0.0;

	gsl_matrix_view matI = gsl_matrix_view_array(mI, isize, isize);

	//Invert the matrix
	gsl_blas_dtrsm(CblasLeft, CblasUpper, CblasNoTrans, CblasNonUnit, 1.0, &matU.matrix, &matI.matrix);

	UnPackYlmMatrixIndependentOnly(mI, outmat, isize);
}

//_____________________________________________________________________
void StHbtCorrFctnDirectYlm::UnPackYlmMatrixIndependentOnly(double* inmat, double* outmat, int insize)
{
	int lmax = sqrt(insize) - 1;
	//cout << "lmax is  " << lmax << endl;
	int tmax = (lmax + 1) * (lmax + 1) * 2;
	int indexfrom[tmax];
	int multfrom[tmax];

	int el, em;
	for (int iter = 0; iter < tmax; iter++) {
		int im = iter % 2;
		GetElEmForIndex(iter / 2, &el, &em);
		if (em == 0) {
			if (im == 1) {
				indexfrom[iter] = 0;
				multfrom[iter]	= 0;
			}
			else {
				indexfrom[iter] = el * el;
				multfrom[iter]	= 1;
			}
		}
		else if (em < 0) {
			indexfrom[iter] = (el * el) + (-em) * 2 - 1;
			if (im) indexfrom[iter]++;
			if ( (-em) % 2 )
				if (im) multfrom[iter] = 1;
				else multfrom[iter] = -1;
			else
			if (im) multfrom[iter] = -1;
			else multfrom[iter] = 1;
		}
		else if (em > 0) {
			indexfrom[iter] = (el * el) + (em) * 2 - 1;
			if (im) indexfrom[iter]++;
			multfrom[iter] = 1;
		}
	}

	for (int ilmz = 0; ilmz < GetMaxJM() * 2; ilmz++)
		for (int ilmp = 0; ilmp < GetMaxJM() * 2; ilmp++)
			outmat[ilmz * GetMaxJM() * 2 + ilmp] = inmat[(indexfrom[ilmz] * insize) + indexfrom[ilmp]] * multfrom[ilmz] * multfrom[ilmp];
}

//____________________________________________________________________
int StHbtCorrFctnDirectYlm::PackYlmVectorIndependentOnly(double* invec, double* outvec)
{
	int ioutcount = 0;
	int em, el;
	for (int ilm = 0; ilm < GetMaxJM(); ilm++) {
		GetElEmForIndex(ilm, &el, &em);
		if (em < 0) continue;
		outvec[ioutcount++] = invec[ilm * 2];
		if (em == 0)
			continue;
		outvec[ioutcount++] = invec[ilm * 2 + 1];
	}
	return (ioutcount);
}

/***************************************************************************
 * $Log: StHbtCorrFctnDirectYlm.cxx,v $
 * Revision 1.2  2015/11/02 20:11:06  perev
 * isnan for new compiler
 *
 * Revision 1.1  2013/01/18 14:46:02  yyang
 * Add ultilities for SHD of CF
 *
 * ***********************************************************************/
