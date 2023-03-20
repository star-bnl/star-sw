
#ifndef __StCentrality_h__
#define __StCentrality_h__

#include <vector>
#include "TString.h"

//____________________________________________________________________________________________________
// Class StCentrality: Centrality class
//   - Centrality: multiplicity cut, and corresponding centrality range
//   - NBD: npp, k and x parameters
class StCentrality {
	public:
		/// Type         description
		/// default      default parameters for a given system
		/// low          low npp, high x (-5% npp)
		/// high         high npp, low x (+5% npp)
		StCentrality(const TString system = "AuAu_200GeV", const TString type="default"); /// Default constructor
		virtual ~StCentrality(); /// Default destructor

		static void help() ; /// Print help messages

		// Getters

		// mode
		//  0    default
		//  1    -5% total cross section
		//  2    +5% total cross section
		Double_t GetCentrality(const UInt_t multiplicity, const UInt_t mode=0) const ; // Get centrality

		Double_t GetNpp()         const ; /// Get Npp
		Double_t GetK()           const ; /// Get K
		Double_t GetX()           const ; /// Get X
		Double_t GetEfficiency()  const ; /// Get efficiency
		Double_t GetTriggerBias() const ; /// Get trigger bias

		Double_t GetReweighting(const UInt_t multiplicity) const ; /// Re-weighting correction

	private:
		// Functions
		Double_t GetNppError(const Double_t npp) const ; /// +/- 5% error if type is low or high

		// Initialization
		void Init_AuAu200GeV() ; /// Initialization of Au + Au 200 GeV
		void Init_AuAu62GeV()  ; /// Initialization of Au + Au 62.4 GeV
		void Init_AuAu39GeV()  ; /// Initialization of Au + Au 39 GeV
		void Init_AuAu27GeV()  ; /// Initialization of Au + Au 27 GeV
		void Init_AuAu19GeV()  ; /// Initialization of Au + Au 19.6 GeV
		void Init_AuAu11GeV()  ; /// Initialization of Au + Au 11.5 GeV
		void Init_AuAu7GeV()   ; /// Initialization of Au + Au 7.7 GeV
		//    void Init_SmSm200GeV() ; /// Initialization of Sm + Sm 200 GeV
		void Init_dAu200GeV()  ; /// Initialization of d + Au 200 GeV

		// Need actual implementation
		void Init_CuCu200GeV() ;
		void Init_ZrZr200GeV() ;
		void Init_UU200GeV() ;
		void Init_PbPb2760GeV() ;

		// Data members
		enum {
			mNMode = 3
		};

		const TString mType   ; /// Type
		Double_t mNpp         ; /// Average multiplicity in p + p
		Double_t mK           ; /// NBD k parameters
		Double_t mX           ; /// Fraction of hard component
		Double_t mEfficiency  ; /// Efficiency
		Double_t mTriggerBias ; /// Trigger bias
		Double_t mParReweighting[2]; /// Parameters for re-weighting correction 1-exp(-p0*x^{p1})
		std::vector<UInt_t> mMultiplicityCut[mNMode] ; /// Multiplicity cut for each centrality bin
		std::vector<Double_t> mCentralityMin[mNMode] ; /// Minimum centrality
		std::vector<Double_t> mCentralityMax[mNMode] ; /// Maximum centrality

		ClassDef(StCentrality, 0)
};

inline Double_t StCentrality::GetNpp()          const { return mNpp ; }
inline Double_t StCentrality::GetK()            const { return mK ; }
inline Double_t StCentrality::GetX()            const { return mX ; }
inline Double_t StCentrality::GetEfficiency()   const { return mEfficiency ; }
inline Double_t StCentrality::GetTriggerBias()  const { return mTriggerBias ; }

#endif

