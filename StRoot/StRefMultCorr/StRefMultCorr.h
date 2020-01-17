//------------------------------------------------------------------------------
// $Id: StRefMultCorr.h,v 1.5 2020/01/16 23:53:31 tnonaka Exp $
// $Log: StRefMultCorr.h,v $
// Revision 1.5  2020/01/16 23:53:31  tnonaka
// gRefmult for Run14 and Run16 added
//
// Revision 1.4  2019/10/03 15:44:21  tnonaka
// Some functions for pile-up rejection and trigger inefficiency corrections are added
//
// Revision 1.3  2019/07/11 03:31:49  tnonaka
// Some functions are added/replaced to read header files and to implement Vz dependent centrality definitions for 54.4 GeV RefMult.
//
//
//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
// StRefMultCor package has been moved to StRoot/StRefMultCorr 2019/02
//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
//
// Revision 1.9  2015/05/22 06:52:07  hmasui
// Add grefmult for Run14 Au+Au 200 GeV
//
// Revision 1.8  2013/05/10 18:33:33  hmasui
// Add TOF tray mult, preliminary update for Run12 U+U
//
// Revision 1.7  2012/05/08 03:19:51  hmasui
// Move parameters to Centrality_def_refmult.txt
//
// Revision 1.6  2012/04/23 21:29:33  hmasui
// Added isBadRun() function for outlier rejection, getBeginRun() and getEndRun() to obtain the run range for a given (energy,year)
//
// Revision 1.5  2011/11/08 19:11:03  hmasui
// Add luminosity corrections for 200 GeV
//
// Revision 1.4  2011/10/11 19:35:18  hmasui
// Fix typo. Add z-vertex check in getWeight() function
//
// Revision 1.3  2011/10/10 21:30:34  hmasui
// Replaced hard coded parameters for z-vertex and weight corrections by input parameters from text file
//
// Revision 1.2  2011/08/12 20:28:04  hmasui
// Avoid varying corrected refmult in the same event by random number
//
// Revision 1.1  2011/08/11 18:38:36  hmasui
// First version of Refmult correction class
//
//------------------------------------------------------------------------------
//  StRefMultCorr class
//   - Provide centrality bins based on multiplicity (refmult, refmult2, tof tray mulitplicity etc)
//     * 5% increment centrality bins (16 bins)
//     * 5% increment in 0-10%, and 10% increment in 10-80% (9 bins)
//   - Provide corrected multiplicity (z-vertex dependence)
//   - Provide "re-weighting" correction, only relevant to the peripheral bins
//
//  Centrality binning:
//     Bin       Centrality (16)   Centrality (9)
//     0            75-80%            70-80%
//     1            70-75%            60-70%
//     2            65-70%            50-60%
//     3            60-65%            40-50%
//     4            55-60%            30-40%
//     5            50-55%            20-30%
//     6            45-50%            10-20%
//     7            40-45%             5-10%
//     8            35-40%             0- 5%
//     9            30-35%
//    10            25-30%
//    11            20-25%
//    12            15-20%
//    13            10-15%
//    14             5-10%
//    15             0- 5%
//
//  See how to use this class in StRefMultCorr/macros/getCentralityBins.C
//
//  authors: Alexander Schmah, Hiroshi Masui
//------------------------------------------------------------------------------

#ifndef __StRefMultCorr_h__
#define __StRefMultCorr_h__

#include <vector>
#include <map>
#include "TString.h"

#include "BadRun.h"

//______________________________________________________________________________
// Class to correct z-vertex dependence, luminosity dependence of multiplicity
class StRefMultCorr 
{
	public:
		// Specify the type of multiplicity (default is refmult)
		// "refmult"   - reference multiplicity defined in |eta|<0.5
		// "refmult2"  - reference multiplicity defined in 0.5<|eta|<1.0
		// "refmult3"  - reference multiplicity defined in |eta|<0.5 without protons
		// "toftray"   - TOF tray multiplicity
		// "grefmult"  - global reference multiplicity defined in |eta|<0.5,dca<3,nHitsFit>10
		// Specify the type of data sets (in case there are multiple prameters/definitions in the same runs)
		// "Def"
		// "VpdMB5"
		// "VpdMB30"
		// "VpdMBnoVtx"
		StRefMultCorr(const TString name="refmult", const TString subname="Def", const TString libname="Def");
		virtual ~StRefMultCorr(); /// Default destructor

		// Bad run rejection
		Bool_t isBadRun(const Int_t RunId) ;

		// Event-by-event initialization. Call this function event-by-event
		// * Default ZDC coincidence rate = 0 to make the function backward compatible 
		// --> i.e. no correction will be applied unless users set the values for 200 GeV
		void initEvent(const UShort_t RefMult, const Double_t z, const Double_t zdcCoincidenceRate=0.0) ; // Set multiplicity, vz and zdc coincidence rate

		Bool_t passnTofMatchRefmultCut(Double_t refmult, Double_t ntofmatch); //remove the pile up event 

		/// Get corrected multiplicity, correction as a function of primary z-vertex
		Double_t getRefMultCorr() const;
		// Corrected multiplity, flag=0:  Luminosity only, flag=1:  z-vertex only, flag=2:  full correction (default)
		Double_t getRefMultCorr(const UShort_t RefMult, const Double_t z, const Double_t zdcCoincidenceRate, const UInt_t flag=2) const ;

		/// Get 16 centrality bins (5% increment, 0-5, 5-10, ..., 75-80)
		Int_t getCentralityBin16() const;

		/// Get 9 centrality bins (10% increment except for 0-5 and 5-10)
		Int_t getCentralityBin9() const;
		
		/// Re-weighting correction, correction is only applied up to mNormalize_step (energy dependent)
		//Double_t getWeight() const;
		Double_t getWeight();

		// Initialization of centrality bins etc
		void init(const Int_t RunId);

		// Read scale factor from text file
		void setVzForWeight(const Int_t nbin, const Double_t min, const Double_t max) ;
		void readScaleForWeight(const Char_t* input) ;

		// Return begin/end run from energy and year
		Int_t getBeginRun(const Double_t energy, const Int_t year) ;
		Int_t getEndRun(  const Double_t energy, const Int_t year) ;

		// Print all parameters
		void print(const Option_t* option="") const ;

	private:
		const TString mName ; // refmult, refmult2, refmult3 or toftray (case insensitive)
		const TString mSubName ; // specify triggers, in case there are multiple parameters/definitions in the same runs 
		const TString mLibName ; // specify library, in case the centrality definition depends on productions 

		// Functions
		void clear() ; /// Clear all arrays
		Bool_t isIndexOk() const ; /// 0 <= mParameterIndex < maxArraySize
		Bool_t isZvertexOk() const ; /// mStart_zvertex < z < mStop_zvertex
		Bool_t isRefMultOk() const ; /// 0-80%, (corrected multiplicity) > mCentrality_bins[0]
		Bool_t isCentralityOk(const Int_t icent) const ; /// centrality bin check
		Int_t setParameterIndex(const Int_t RunId) ; /// Parameter index from run id (return mParameterIndex)

		// Special scale factor for Run14 to take into account the weight
		// between different triggers
		//  - return 1 for all the other runs
		Double_t getScaleForWeight() const ;

		// Data members
		enum 
		{
			mNCentrality   = 16, /// 16 centrality bins, starting from 75-80% with 5% bin width
			mNPar_z_vertex = 8,
			mNPar_weight   = 8,
			mNPar_luminosity = 2
		};

		// Use these variables to avoid varying the corrected multiplicity
		// in the same event by random numbers
		UShort_t mRefMult ;     /// Current multiplicity
		Double_t mVz ;          /// Current primary z-vertex
		Double_t mZdcCoincidenceRate ; /// Current ZDC coincidence rate
		Double_t mRefMult_corr; /// Corrected refmult

		std::vector<Int_t> mYear              ; /// Year
		std::vector<Int_t> mStart_runId       ; /// Start run id
		std::vector<Int_t> mStop_runId        ; /// Stop run id
		std::vector<Double_t> mStart_zvertex  ; /// Start z-vertex (cm)
		std::vector<Double_t> mStop_zvertex   ; /// Stop z-vertex (cm)
		std::vector<Double_t> mNormalize_stop ; /// Normalization between MC and data (normalized in refmult>mNormalize_stop)
		std::vector<Int_t>    mCentrality_bins[mNCentrality+1] ;  /// Centrality bins (last value is set to 5000)
		std::vector<Double_t> mPar_z_vertex[mNPar_z_vertex] ;     /// parameters for z-vertex correction
		std::vector<Double_t> mPar_weight[mNPar_weight] ;         /// parameters for weight correction
		std::vector<Double_t> mPar_luminosity[mNPar_luminosity] ; /// parameters for luminosity correction (valid only for 200 GeV)
		Int_t mParameterIndex; /// Index of correction parameters

		std::multimap<std::pair<Double_t, Int_t>, Int_t> mBeginRun ; /// Begin run number for a given (energy, year)
		std::multimap<std::pair<Double_t, Int_t>, Int_t> mEndRun   ; /// End run number for a given (energy, year)
		std::vector<Int_t> mBadRun ; /// Bad run number list

		// [6][680];
		Int_t mnVzBinForWeight ; /// vz bin size for scale factor
		std::vector<Double_t> mVzEdgeForWeight ; /// vz edge value
		std::vector<Double_t> mgRefMultTriggerCorrDiffVzScaleRatio ; /// Scale factor for global refmult

		/////// Added from official package in StRoot/StRefMultCorr 
		const Int_t getRefX() const;  /// X= 1 (normal RefMult), 2, 3, 4
		const Int_t getNumberOfDatasets() const; /// Number of definitions for each X
		void readHeaderFile();   //// alternative of read() in rev<=1.9 
		void readBadRunsFromHeaderFile();  /// alternative of readBadRuns() in rev<=1.9 
		//// Multiplicity dependent centrality definitions
		Int_t getVzWindowForVzDepCentDef() const;
		Int_t getCentralityBin9VzDep() const;
		Int_t getCentralityBin16VzDep() const;
		std::vector<std::string> StringSplit( const std::string str, const char sep );

		//---------------------------------------------------------
		//weight refmult shape in each Vz bin to what in the center (|Vz|<10cm), add by Zaochen
		Double_t getShapeWeight_SubVz2Center() ;
		//---------------------------------------------------------
		//
		// Read scale factor from header file
		void readScaleForWeight(const Int_t nRefmultBin, const Double_t *weight) ;

		ClassDef(StRefMultCorr, 0)
};
#endif

