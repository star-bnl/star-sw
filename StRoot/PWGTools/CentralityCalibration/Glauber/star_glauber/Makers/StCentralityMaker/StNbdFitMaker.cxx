
#include <assert.h>

#include "TCanvas.h"
#include "TError.h"
#include "TFile.h"
#include "TGraph.h"
#include "TH2.h"
#include "TH3.h"
#include "TLine.h"
#include "TStyle.h"
#include "TMath.h"

#include "StMessMgr.h"
#include "StNegativeBinomial.h"
#include "StNbdFitMaker.h"

ClassImp(StNbdFitMaker)

	//____________________________________________________________________________________________________
	// Default constructor
StNbdFitMaker::StNbdFitMaker()
{
	mNBinomial = 0 ;
	mhRefMult = 0 ;
	mhRefMultSim = 0 ;

	mNData = 0 ;

	mMinimumMultiplicityCut = 100.0 ; // >50 by default. Can be changed by setMinimumMultiplicityCut(const Double_t)

	//mDoCentralityDetermination = kFALSE ;
	mDoCentralityDetermination = kTRUE;

	mCanvas = 0 ;
	mCutOff[0] = 0;
	mCutOff[1] = 0;
	mOneLine = 0 ;
	mChi2Graph = 0;
}

//____________________________________________________________________________________________________
// Default destructor
StNbdFitMaker::~StNbdFitMaker()
{
	if(mNBinomial) delete mNBinomial ;
	if(mCanvas) delete mCanvas ;

	for(Int_t i=0;i<2;i++)
	{
		if(mCutOff[i]) delete mCutOff[i] ;
	}
	if(mOneLine) delete mOneLine ;

	if(mChi2Graph) delete mChi2Graph ;
}

//____________________________________________________________________________________________________
void StNbdFitMaker::DoCentralityDetermination()
{
	LOG_INFO << "StNbdFitMaker::DoCentralityDetermination  Centrality determination is ON" << endm;
	mDoCentralityDetermination = kTRUE ;
}

//____________________________________________________________________________________________________
Double_t StNbdFitMaker::GetNormalization(const TH1& h1, const TH1& h2, const Double_t min, const Double_t max) const
{
	// Get normalization factor in (min, max)
	Double_t numerator   = 0.0;
	Double_t denominator = 0.0;
	const Int_t mulminbin = h1.GetXaxis()->FindBin(min);
	const Int_t mulmaxbin = h1.GetXaxis()->FindBin(max);

	for(Int_t mul=mulminbin; mul<mulmaxbin; mul++){
		const Double_t n1      = h1.GetBinContent(mul+1);
		const Double_t n1Error = h1.GetBinError(mul+1);

		if( n1 == 0.0 || n1Error == 0.0 ) continue ;

		const Double_t n2 = h2.GetBinContent(mul+1);

		numerator   += n1 * n2 / (n1Error*n1Error) ;
		denominator += n2 * n2 / (n1Error*n1Error) ;
	}

	const Double_t norm = (denominator!=0.0) ? numerator / denominator : 1.0 ;

	LOG_INFO << Form("StNbdFitMaker::GetNormalization  (min, max, norm) = (%5d, %5d, %1.7f)", 
			static_cast<Int_t>(min), static_cast<Int_t>(max), norm)
		<< endm;

	return norm ;
}

//____________________________________________________________________________________________________
Double_t StNbdFitMaker::CalculateChi2(const TH1& hdata, const TH1& hfunc, const Double_t minimumMultiplicityCut)
{
	/// Calculate chi2 from data and func
	mNData = 0 ;

	Double_t chi2 = 0.0 ;
	for(Int_t ix=0; ix<hdata.GetNbinsX(); ix++){
		// Lower multiplicity cut off
		const Double_t mult      = hdata.GetBinCenter(ix+1);
		if( mult < minimumMultiplicityCut ) continue ;

		// Check data points
		const Double_t dataError = hdata.GetBinError(ix+1);
		if( dataError == 0.0 ) continue ;
		// Check sim points
		const Double_t simError = hfunc.GetBinError(ix+1);
                if( simError == 0.0 ) continue ;
		// Combine errors in quadrature
		const Double_t totError = TMath::Power((dataError*dataError + simError*simError),0.5);

		// Calculate chi2
		const Double_t data  = hdata.GetBinContent(ix+1);
		const Double_t func  = hfunc.GetBinContent(ix+1);
		const Double_t delta = (data - func)/totError ;
		chi2 += delta*delta ;
		mNData++;
	}

	LOG_INFO << Form("StNbdFitMaker::calculateChi2  M > %5d: (npp, k) = (%1.5f, %1.5f)  chi2/ndata = %1.3f/%5d = %1.3f",
			static_cast<Int_t>(minimumMultiplicityCut), mNBinomial->GetNpp(), mNBinomial->GetK(), chi2, mNData, chi2/static_cast<Double_t>(mNData))
		<< endm;

	return chi2 ;
}

//____________________________________________________________________________________________________
void StNbdFitMaker::CalculateCentrality(const TH1& hdata, const TH1& hmc) const
{
	// - Calculate centrality from the MC multiplicity distribution
	// - Also calculate the re-weighting correction = MC/Data, important for peripheral (typically 60-80%)
	//
	// NOTE: Assume MC multiplicity has been normalized to the real data

	const UInt_t ncent = 16 ; // 0-80% (5% increment)
	Int_t centBin[2][3][ncent]; // Final centrality bins

	for(UInt_t i=0; i<2; i++) {
		// For cross check, do centrality determination
		// 1) from peripheral to central
		// 2) from central to peripheral
		if ( i == 0 ) LOG_INFO << "StNbdFitMaker::CalculateCentrality  (1) Centrality determination from peripheral to central" << endm;
		if ( i == 1 ) LOG_INFO << "StNbdFitMaker::CalculateCentrality  (2) Centrality determination from central to peripheral" << endm;

		// Centrality cut
		Double_t centralityCut[ncent] ;
		Double_t centralityMin[ncent] ;
		Double_t centralityMax[ncent] ;

		for(UInt_t ic=0; ic<ncent; ic++) {
			const Double_t sign         = (i==0) ? -1.0 : 1.0 ;
			const Double_t centStep     = 0.05 ;
			const Double_t centCutStart = (i==0) ? 0.80 : 0.05 ;
			const Double_t centMinStart = (i==0) ? 75.0 : 0.0 ;

			centralityCut[ic] = centCutStart + sign*centStep * ic ;
			centralityMin[ic] = centMinStart + sign*centStep * 100.0 * ic ;
			centralityMax[ic] = (centMinStart + centStep*100.0) + sign*centStep * 100.0 * ic ;

			// Print centrality definitions
			LOG_INFO << Form("StNbdFitMaker::CalculateCentrality  Centrality: %1.0f-%1.0f %%, cut = %1.2f",
					TMath::Abs(centralityMin[ic]), TMath::Abs(centralityMax[ic]), centralityCut[ic])
				<< endm;
		}

		// Start calculation for three different cases, for systematic error study
		// 0: default, 1:-5% total cross section, 2:+5% total cross section
		const Double_t nevent = hmc.Integral() ;
		for(Int_t it=0; it<3; it++){ // vary total cross section by 5 % in it=1,2
			Double_t scale = 1.0 ;
			if( it == 1 ) scale = 0.95 ;
			if( it == 2 ) scale = 1.05 ;

			UInt_t bin = 0 ;
			const Int_t nbin = hmc.GetNbinsX() ;
			Double_t sum = 0.0;
			// The following commented code is the original centrality-cut calculator. The updated method is below which 
			//  integrates the data from 0-20% and the glauber for centralities >20%
			/*for(Int_t im=0; im<nbin; im++){
				const Double_t M      = (i==0) ? hmc.GetBinCenter(im+1) : hmc.GetBinCenter(nbin-im) ;
				const Int_t Mint      = (i==0) ? im : nbin-im-1 ;
				const Double_t count  = (i==0) ? hmc.GetBinContent(im+1) : hmc.GetBinContent(nbin-im) ;
				if( count == 0.0 ) continue ;

				sum += count ;
				const Double_t fraction    = sum / nevent ;
				const Double_t fCentBinCut = centralityCut[bin] * scale;
				const Double_t R           = (i==0) ? (1.0 - fraction) : fraction ;
				const Bool_t isCentOk      = (i==0) ? R <= fCentBinCut : R > fCentBinCut ;

				if( isCentOk && bin < ncent ){
					cout << Form("%2.2f - %2.2f (%%) :  M > %4d (im=%3d, M=%1.1f, bin=%4d) (sum, total, fraction>cut) = (%1.3f, %1.3f, %1.3f>%1.3f)",
							TMath::Abs(centralityMin[bin]*scale), TMath::Abs(centralityMax[bin]*scale), Mint, im, M, bin, sum, nevent, R, fCentBinCut) << endl;

					centBin[i][it][bin] = (Double_t)Mint ;
					bin++;
				}
			}// multiplicity loop */
			
			// The following is the updated centrality calculation which integrates the data from 0-20% and the glauber for centralities >20%
			Double_t distance = 1000.0;
                        for(Int_t im=0; im<nbin; im++){
                                const Int_t Mint      = (i==0) ? im : nbin-im ;
                                Double_t count = 0.0;
                                if( (i==0 && bin>11) || (i!=0 && bin<4) ) count = (i==0) ? hdata.GetBinContent(im+1) : hdata.GetBinContent(nbin-im);
                                else count = (i==0) ? hmc.GetBinContent(im+1) : hmc.GetBinContent(nbin-im) ;
                                if( count == 0.0 ) continue ;

                                sum += count ;
                                const Double_t fraction    = sum / nevent ;
                                const Double_t fCentBinCut = centralityCut[bin] * scale;
                                const Double_t R           = (i==0) ? (1.0 - fraction) : fraction ;
                                Double_t thisdistance = TMath::Abs(R - fCentBinCut );
                                //const Bool_t isCentOk      = (i==0) ? R <= fCentBinCut : R > fCentBinCut ;
                                const Bool_t isCentOk      = (thisdistance > distance) ;
                                distance=thisdistance;

                                if( isCentOk && bin < ncent ){
                                        cout << Form("%2.2f - %2.2f (%%) :  M > %4d (im=%3d, M=%1.1f, bin=%4d) (sum, total, fraction>cut) = (%1.3f, %1.3f, %1.3f>%1.3f)",
                                                        TMath::Abs(centralityMin[bin]*scale), TMath::Abs(centralityMax[bin]*scale), Mint, im, (Double_t)Mint, bin, sum, nevent, R, fCentBinCut) << endl;
                                        centBin[i][it][bin] = (Double_t)Mint ;
                                        bin++;
                                        distance=1000.0;
                                }
                        }// multiplicity loop   
		}// different total cross section
	}// from peripheral or central

	// Print centrality bins in the array format for implementation in StCentrality.cxx
	// - Use central to peripheral
	for(UInt_t ic=0; ic<ncent; ic++) {
		LOG_INFO << Form("  mMultiplicityCut[0].push_back( %3d ); mCentralityMin[0].push_back( %2.1f ); mCentralityMax[0].push_back( %2.1f );",
				(Int_t)centBin[1][0][ic], 5.0*ic, 5.0*(ic+1)
				) << endm;
	}

	// For +/- 5% bins
	for(UInt_t ic=0; ic<ncent; ic++) {
		LOG_INFO << Form("  mMultiplicityCut[1].push_back( %3d );  mMultiplicityCut[2].push_back( %3d );",
				(Int_t)centBin[1][1][ic], (Int_t)centBin[1][2][ic]
				) << endm;
	}

}

//____________________________________________________________________________________________________
void StNbdFitMaker::SetParameters(const Double_t npp, const Double_t k, const Double_t x,
		const Double_t efficiency, const Double_t triggerbias, const Bool_t isConstEfficiency)
{
	if(mNBinomial) delete mNBinomial ;

	mNBinomial = new StNegativeBinomial(npp, k, x, efficiency, triggerbias, isConstEfficiency);
}

//____________________________________________________________________________________________________
void StNbdFitMaker::SetMinimumMultiplicityCut(const Double_t cut)
{
	mMinimumMultiplicityCut = cut ;
	LOG_INFO << "StNbdFitMaker::setMinimumMultiplicityCut  Set low multiplicity cut off : M > " <<  cut << endm;
}

//____________________________________________________________________________________________________
void StNbdFitMaker::ReadData(const Char_t* data, const Char_t* glauber, const Char_t* dataHistogramName)
{
	// Read real data file
	TFile* inputData = TFile::Open(data);
	if(!inputData || !inputData->IsOpen()){
		Error("StNbdFitMaker::readData", "can't open %s", data);
		assert(0);
	}
	LOG_INFO << "StNbdFitMaker::readData  open Data file: " <<  inputData->GetName() << endm;

	mhRefMult = 0;
	//  if( mNBinomial->useTpc() ){
	/// TPC refMult
	mhRefMult = (TH1D*) inputData->Get(dataHistogramName);
	//    mhRefMult = (TH1D*) inputData->Get("hRefMultTpc");
	//    mhRefMult = (TH1D*) inputData->Get("hRefMult");
	//  }
	//  else{
	//    /// FTPC refMult
	//    mhRefMult = (TH1D*) inputData->Get("hRefMultFTpc");
	//  }

	if(!mhRefMult){
		Error("StNbdFitMaker::readData", "hRefMult doesn't exist");
		assert(mhRefMult);
	}

	mhRefMult->SetLineColor(1);

	// Define simulated refmult
	const Int_t nbinsx  = mhRefMult->GetNbinsX() ;
	const Double_t xmin = mhRefMult->GetXaxis()->GetXmin() ;
	const Double_t xmax = mhRefMult->GetXaxis()->GetXmax() ;
	mhRefMultSim = new TH1D("hRefMultSim", "", nbinsx, xmin, xmax);
	mhRefMultSim->SetLineColor(2);

	// Sumw2 to calculate error properly
	mhRefMult->Sumw2();
	mhRefMultSim->Sumw2();

	// Read glauber file
	TFile* inputGlauber = TFile::Open(glauber);
	if(!inputGlauber || !inputGlauber->IsOpen())
	{
		Error("StNbdFitMaker::readData", "can't open %s", glauber);
		assert(0);
	}
	LOG_INFO << "StNbdFitMaker::readData  open Glauber file: " << inputGlauber->GetName() << endm;

	mhNcoll_Npart = (TH2D*) inputGlauber->Get("hNcoll_Npart");
	if(!mhNcoll_Npart)
	{
		Error("StNbdFitMaker::readData", "hNcoll_Npart doesn't exist");
		assert(mhNcoll_Npart);
	}
}

//____________________________________________________________________________________________________
//TGraph* StNbdFitMaker::Fit(const Int_t nevents, const Char_t* outputFileName)//old
TGraph* StNbdFitMaker::Fit(const Int_t nevents, TString outputFileName)//zaochen
{
	gStyle->SetOptStat(0);

	/// Fit real data by simulated multiplicity distribution

	/// Make sure the refmult and Ncoll_Npart histograms have benn opened
	if(!mhRefMult)
	{
		Error("StNbdFitMaker::Fit", "hRefMult doesn't exist");
		assert(mhRefMult);
	}

	if(!mhNcoll_Npart)
	{
		Error("StNbdFitMaker::Fit", "hNcoll_Npart doesn't exist");
		assert(mhNcoll_Npart);
	}

	mhRefMultSim->Reset();

	Int_t ievent = 0 ;
	while( ievent < nevents ) 
	{
		Double_t npart, ncoll;
		mhNcoll_Npart->GetRandom2(npart, ncoll);
		const Bool_t isNpartNcollOk = (npart>=2 && ncoll>=1) ;
		if ( !isNpartNcollOk ) continue ;

		const Int_t multiplicity = mNBinomial->GetMultiplicity(npart, static_cast<Int_t>(ncoll));
		mhRefMultSim->Fill(multiplicity);

		if( ievent % (nevents/10) == 0 )
		{
			LOG_INFO << Form("StNbdFitMaker::Fit  ievent=%10d  (npart, ncoll, mult) = (%10d, %10d, %10d)",
					ievent, (Int_t)npart, (Int_t)ncoll, multiplicity)
				<< endm;
		}

		ievent++;
	}

	// Normalization
	const Double_t norm = GetNormalization(*mhRefMult, *mhRefMultSim, mMinimumMultiplicityCut, mhRefMult->GetXaxis()->GetXmax());
	//const Double_t norm = GetNormalization(*mhRefMult, *mhRefMultSim, mMinimumMultiplicityCut, 400);
	mhRefMultSim->Scale(norm);

	// Get chi2
	const Double_t chi2 = CalculateChi2(*mhRefMult, *mhRefMultSim, mMinimumMultiplicityCut);

	//----------------------------------------------------------------------------------------------------
	// Set chi2
	//----------------------------------------------------------------------------------------------------
	if(mChi2Graph) delete mChi2Graph ;
	mChi2Graph = new TGraph();
	mChi2Graph->SetPoint(0, 0, chi2);
	mChi2Graph->SetPoint(1, 1, mNData - 2); // 2 fitting parameters (npp, k)
	mChi2Graph->SetPoint(2, 2, chi2/(mNData-2.0));

	//----------------------------------------------------------------------------------------------------
	// Draw
	//----------------------------------------------------------------------------------------------------
	mhRefMult->SetMinimum(0.1);
	mhRefMult->SetMaximum(mhRefMult->GetMaximum()*10.0);

	mhRefMultSim->SetXTitle("Refmult (MC)");
	mhRefMultSim->SetYTitle("Count");
	mhRefMultSim->SetTitle(Form("npp=%1.2f, k=%1.2f, x=%1.2f",
				mNBinomial->GetNpp(), mNBinomial->GetK(), mNBinomial->GetX()));

	if(mCanvas) delete mCanvas ;
	mCanvas = new TCanvas("c1", "c1", 1200, 500);
	mCanvas->Divide(2, 1);
	mCanvas->cd(1);
	mCanvas->GetPad(1)->SetLogy(1);

	mhRefMult->Draw("h");
	mhRefMultSim->Draw("hsame");

	// Normalization line
	if(mCutOff[0]) delete mCutOff[0] ;
	mCutOff[0] = new TLine( mMinimumMultiplicityCut, mhRefMult->GetMinimum(), mMinimumMultiplicityCut, mhRefMult->GetMaximum());
	mCutOff[0]->SetLineColor(4);
	mCutOff[0]->SetLineStyle(2);
	mCutOff[0]->Draw();

	// Draw ratio of MC to data
	mCanvas->cd(2);
	TH1* hRatio = (TH1D*) mhRefMultSim->Clone();
	hRatio->SetName("hRatio");
	hRatio->Divide(mhRefMult);
	hRatio->SetYTitle("MC/data");

	hRatio->SetMinimum(0);
	hRatio->SetMaximum(2);
	hRatio->Draw();

	if(mOneLine) delete mOneLine ;
	mOneLine = new TLine(hRatio->GetXaxis()->GetXmin(), 1.0, hRatio->GetXaxis()->GetXmax(), 1.0);
	mOneLine->SetLineColor(4);
	mOneLine->SetLineStyle(2);
	mOneLine->Draw();

	if(mCutOff[1]) delete mCutOff[1] ;
	mCutOff[1] = new TLine( mMinimumMultiplicityCut, hRatio->GetMinimum(), mMinimumMultiplicityCut, hRatio->GetMaximum());
	mCutOff[1]->SetLineColor(4);
	mCutOff[1]->SetLineStyle(2);
	mCutOff[1]->Draw();

	mCanvas->cd();
	mCanvas->Update();

	//----------------------------------------------------------------------------------------------------
	// Centrality
	//----------------------------------------------------------------------------------------------------
	if ( mDoCentralityDetermination ) 
	{
		CalculateCentrality(*mhRefMult, *mhRefMultSim) ;
	}

	//----------------------------------------------------------------------------------------------------
	// Write only if outputFileName is given
	//----------------------------------------------------------------------------------------------------
	//const TString fileName(outputFileName);//zaochen
	//if(!fileName.IsWhitespace())//zaochen
	if(!outputFileName.IsWhitespace())//zaochen
	{
		LOG_INFO << "StNbdFitMaker::Fit  Write output ROOT file : " << outputFileName << endm;
		TFile* output = TFile::Open(outputFileName, "recreate");
		mhRefMult->Write();
		mhRefMultSim->Write();
		hRatio->Write();
		output->Close();
	}

	return mChi2Graph;
}

//____________________________________________________________________________________________________
Int_t StNbdFitMaker::Scan(const Int_t nevents,
		const Int_t nppbin, const Double_t nppmin, const Double_t nppmax,
		const Int_t kbin, const Double_t kmin, const Double_t kmax,
		const Int_t xbin, const Double_t xmin, const Double_t xmax,
		//const Double_t x,
		//const Int_t effbin, const Double_t effmin, const Double_t effmax,
		const Double_t efficiency,
		const Double_t triggerbias, const Bool_t isConstEfficiency
		){
	/// Loop over all (npp, k, x) to find out minimum chi2
	TH3* hChi2 = new TH3D("hChi2", "#chi^{2}/NDF in (npp, k, x) space",
			nppbin, nppmin, nppmax, kbin, kmin, kmax, xbin, xmin, xmax);
	hChi2->SetXTitle("n_{pp}");
	hChi2->SetYTitle("k");
	hChi2->SetZTitle("x");

	const Double_t nppstep = (nppbin==1) ? 0 : (nppmax-nppmin)/static_cast<Double_t>(nppbin-1) ;
	const Double_t kstep   = (kbin==1)   ? 0 : (kmax-kmin)/static_cast<Double_t>(kbin-1) ;
	const Double_t xstep   = (xbin==1)   ? 0 : (xmax-xmin)/static_cast<Double_t>(xbin-1) ;

	for(Int_t ix=0; ix<nppbin; ix++){
		const Double_t npp = nppmin + nppstep*ix ;

		for(Int_t iy=0; iy<kbin; iy++){
			const Double_t k = kmin + kstep*iy ;
			LOG_INFO << Form("StNbdFitMaker::Scan  Fitting for (npp, k) = (%1.3f, %1.3f) ...", npp, k) << endm;

			for(Int_t iz=0; iz<xbin; iz++){
				const Double_t x = xmin + xstep*iz ;
				// Set parameters
				SetParameters(npp, k, x, efficiency, triggerbias, isConstEfficiency);

				//add by Lizhu
				//const Char_t* fileforratio(Form("Ratio_npp%1.3f_k%1.3f_x%1.3f_eff%1.3f.root", npp, k, x, efficiency));
				const TString fileforratio = Form("RatioChi2Files/Ratio_npp%1.3f_k%1.3f_x%1.3f_eff%1.3f.root", npp, k, x, efficiency);//zaochen

				// Fitting,  write ROOT file
				Fit(nevents, fileforratio);

				// Fitting, do not write ROOT file
				//Fit(nevents, "");

				// Get chi2
				hChi2->SetBinContent(ix+1, iy+1, iz+1, mChi2Graph->GetY()[2]);

				LOG_INFO << Form("StNbdFitMaker::Scan  Fitting for (npp, k, x, d, chi2/ndf) = (%1.3f, %1.3f, %1.3f, %1.3f, %1.3f/%3d=%1.3f) ...", 
						npp, k, x, efficiency, mChi2Graph->GetY()[0], (Int_t)mChi2Graph->GetY()[1], mChi2Graph->GetY()[2])
					<< endm;
			}// x loop
		}// k loop
	}// npp loop

	//----------------------------------------------------------------------------------------------------
	// Write chi2 graph
	// Filename is determined by the range of npp, k and x
	//----------------------------------------------------------------------------------------------------
	//const Char_t* fileName(Form("chi2_nevents%d_npp%1.3f-%1.3f_k%1.3f-%1.3f_x%1.3f_%1.3f_eff%1.3f.root",
	const Char_t* fileName(Form("RatioChi2Files/chi2_nevents%d_npp%1.3f-%1.3f_k%1.3f-%1.3f_x%1.3f_%1.3f_eff%1.3f.root",
				nevents, nppmin, nppmax, kmin, kmax, xmin, xmax, efficiency));
	TFile* outputFile = TFile::Open(fileName, "recreate");
	hChi2->Write();
	outputFile->Close();

	return 1;
}


