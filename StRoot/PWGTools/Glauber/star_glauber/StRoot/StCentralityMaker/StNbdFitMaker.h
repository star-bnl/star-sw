/******************************************************************************
 * $Id: StNbdFitMaker.h,v 1.2 2012/04/25 05:15:30 hmasui Exp $
 * $Log: StNbdFitMaker.h,v $
 * Revision 1.2  2012/04/25 05:15:30  hmasui
 * Added centrality calculation. Real data histogram name can be now specified in ReadData() function
 *
 ******************************************************************************/

#ifndef __StNbdFitMaker_h__
#define __StNbdFitMaker_h__

class TCanvas ;
class TGraph ;
class TH1 ;
class TH2 ;
class TLine ;
class StNegativeBinomial ;

//____________________________________________________________________________________________________
// Class StNbdFitMaker: Determine the NBD parameters from the data
class StNbdFitMaker {
	public:
		StNbdFitMaker();
		virtual ~StNbdFitMaker(); /// Default destructor

		/// Switch on centrality calculation (default is OFF)
		void DoCentralityDetermination() ;

		/// Read real data and glauber ROOT files
		void ReadData(const Char_t* data, const Char_t* glauber,
				const Char_t* dataHistogramName = "hRefMultTpc") ;

		/// Draw multiplicity distribution sampled by nevents (default is 1000 events)
		/// Do not write output ROOT file if outputFileName is blank
		/// Return TGraph, contains
		/// [0] = chi2
		/// [1] = NDF
		/// [2] = chi2/NDF
		//TGraph* Fit(const Int_t nevents = 1000, const Char_t* outputFileName = "");//zaochen comment
		TGraph* Fit(const Int_t nevents = 1000, const TString outputFileName = "");

		/// Find minimum chi2/NDF in (npp, k, efficiency) space
		Int_t Scan(const Int_t nevents,
				const Int_t nppbin, const Double_t nppmin, const Double_t nppmax,
				const Int_t kbin, const Double_t kmin, const Double_t kmax,
				const Int_t xbin, const Double_t xmin, const Double_t xmax,
				//        const Double_t x,
				const Double_t efficiency=1.0,
				//        const Int_t effbin, const Double_t effmin, const Double_t effmax,
				const Double_t triggerbias=1.0, const Bool_t isConstEfficiency=kTRUE
				);

		/// Set parameters
		void SetParameters(const Double_t npp, const Double_t k, const Double_t x,
				const Double_t efficiency, const Double_t triggerbias, const Bool_t isConstEfficiency) ;

		/// Set minimum multiplicity cuts to avoid inefficiency (default is M>50)
		void SetMinimumMultiplicityCut(const Double_t cut) ;

	private:
		// Functions
		Double_t GetNormalization(const TH1& h1, const TH1& h2,
				const Double_t min, const Double_t max) const ; // Get normalization factor in (min, max)
		Double_t CalculateChi2(const TH1& hdata, const TH1& hfunc,
				const Double_t minimumMultiplicityCut) ; // Get chi2 from data and func

		void CalculateCentrality(const TH1& hdata, const TH1& hmc) const ;

		// Data members
		StNegativeBinomial* mNBinomial ; /// Negative binomial distribution
		TH1* mhRefMult     ; /// Reference multiplicity
		TH1* mhRefMultSim  ; /// Simulated refmult
		TH2* mhNcoll_Npart ; /// Ncoll vs Npart histograms for fit
		Int_t mNData ; /// Number of data points used in fit
		Double_t mMinimumMultiplicityCut ; /// Minimum multiplicity cut for fitting
		Bool_t mDoCentralityDetermination ; /// Centrality determination flag (default is false)

		TCanvas* mCanvas   ; /// Canvas to draw multiplicity
		TLine* mCutOff[2]  ; /// Multiplicity cut off
		TLine* mOneLine    ; /// Line at 1
		TGraph* mChi2Graph ; /// Container of chi2 and NDF

		ClassDef(StNbdFitMaker, 0)
};
#endif

