/******************************************************************************
 * $Id: StGlauberAnalysisMaker.h,v 1.3 2012/04/25 05:05:08 hmasui Exp $
 * $Log: StGlauberAnalysisMaker.h,v $
 * Revision 1.3  2012/04/25 05:05:08  hmasui
 * Fix the multiplicity calculation in the analysis maker, use multiplicity from tree. Added higher harmonics. Unit weight for multiplicity related quantities. Use STAR logger.
 *
 * Revision 1.2  2010/11/21 02:27:56  hmasui
 * Add re-weighting. Calculate multiplicity from Npart and Ncoll. Use STAR logger
 *
 ******************************************************************************/
//====================================================================================================
//  StGlauberAnalysisMaker class 
//    Analyze outputs from Fast MC Glauber simulation by StFastGlauberMcMaker
//
//  In order to make histograms from the output tree, you can do
//  root4star -b
//  [0] .L doAnalysisMaker.C
//  [1] doAnalysisMaker("AuAu_200GeV", "file.list", "ana.root", "default")
//  where first argument is the system+energy string,
//  second argument is the input file list,
//  third argument is the output file name
//  and fourth argument is type. The available type can be found by
//    StFastGlauberMcMaker::Print("type")
//
//  The system+energy string should be like "%2s%2s_%dGeV",
//  where the system name should contain 2+2 characters from
//  each nucleus and energy should be integer (e.x. 7.7 -> 7)
//
//  Reweighting correction can be applied by
//    StGlauberAnalysisMaker::ReweightingOn()
//  
//   You can see the debugging messages by
//     StGlauberAnalysisMaker::DebugOn()
//   in case you need further checks of outputs
//
//====================================================================================================


#ifndef __StGlauberAnalysisMaker_h__
#define __StGlauberAnalysisMaker_h__

class TBranch ;
class TFile ;
class TTree ;
class StCentralityMaker ;
class StGlauberHistogramMaker ;
class StGlauberCumulantHistogramMaker ;
class StGlauberTree ;
//#include <map>
#include "TString.h"

//____________________________________________________________________________________________________
// Class StGlauberAnalysisMaker: Analyze initial conditions
class StGlauberAnalysisMaker {
	public:
		StGlauberAnalysisMaker(const TString type="default",
				const TString system = "AuAu_200GeV", const TString outputFileName = "ana.root",
				const TString tableDir = "./"); /// Default constructor
		virtual ~StGlauberAnalysisMaker(); /// Default destructor

		Bool_t Make()  ; /// Make one event
		Bool_t RunFile(const TString inputFileName = "icmaker.root") ; /// Loop over all events in one file
		Bool_t Run(const TString inputFileList)                      ; /// Loop over all file lists
		Bool_t Finish() ; /// Finish analysis

		void UnitWeightOn() ; /// Use unit weight instead of multiplicity weight
		void ReweightingOn() ; /// Apply re-weighting correction
		void DebugOn() ; /// Print debugging messages

	private:
		// Functions
		Bool_t Init(const TString tableDir); /// Initialization of histograms

		// Data members
		enum {
			mNMaxType = 12  // Maximum number of types
		};

		static const TString mTypeName[mNMaxType]    ; /// Available types
		static const TString mDescription[mNMaxType] ; /// Description of types
		//    static const UInt_t mCentralityId[mNMaxType] ; /// Centrality id for corresponding type

		const TString mType         ; /// Type (see above)
		TFile* mOutputFile          ; /// Output ROOT file
		TString mOutputFileName     ; /// Output ROOT filename
		Bool_t mUnitWeight          ; /// Unit weight flag
		Bool_t mReweighting         ; /// Reweighting flag
		UInt_t mDebug ; /// Debug flag
		UInt_t mNevents ; /// Number of accepted events

		//    std::map<const TString, const UInt_t> mCentralityIdMap ; /// Centrality id for each node

		StGlauberTree* mGlauberTree         ; /// Input ROOT tree (read mode)
		StCentralityMaker* mCentralityMaker ; /// Centrality maker

		// Output histograms
		StGlauberHistogramMaker* mImpactParameter   ; /// Impact parameter
		StGlauberHistogramMaker* mNpart             ; /// Npart
		StGlauberHistogramMaker* mNcoll             ; /// Ncoll
		StGlauberHistogramMaker* mMultiplicity      ; /// Multiplicity (uncorrected)
		StGlauberHistogramMaker* mAreaRP            ; /// Reaction plane area
		StGlauberHistogramMaker* mAreaPP            ; /// Participant plane area
		StGlauberCumulantHistogramMaker* mEccRP     ; /// Reaction plane eccentricity (2nd order) Npart weight
		StGlauberCumulantHistogramMaker* mEccPP[3]  ; /// Participant plane eccentricity (2nd, 3rd and 4th) Npart weight
		StGlauberCumulantHistogramMaker* mEccRPM    ; /// Reaction plane eccentricity (2nd order) multiplicity weight
		StGlauberCumulantHistogramMaker* mEccPPM[3] ; /// Participant plane eccentricity (2nd, 3rd and 4th) multiplicity weight

		ClassDef(StGlauberAnalysisMaker, 0)
};

#endif

