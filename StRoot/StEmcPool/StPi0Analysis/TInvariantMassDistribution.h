#ifndef StPi0Analysis_TInvariantMassDistribution_H
#define StPi0Analysis_TInvariantMassDistribution_H

#include <TNamed.h>
#include <TH1F.h>
#include <TH2F.h>

#include <list>
using namespace std;

#include "StPi0AnalysisVersion.h"
#include "StPi0AnalysisUtil.h"
#include "TBinParameters.h"

class TInvariantMassDistribution : public TNamed {
public:
		typedef TInvariantMassDistribution this_type;
		typedef TNamed inherited;
		typedef TBinParameters parameters_type;
		typedef TH1F distribution_type;
		typedef TH2F distribution_2d_type;

		TInvariantMassDistribution(const Char_t *name = 0, const Char_t *title = 0);
		TInvariantMassDistribution(const this_type &inv);
		TInvariantMassDistribution(const parameters_type &par);
		virtual ~TInvariantMassDistribution();

		this_type &operator=(const this_type &inv);

		Bool_t operator<(const this_type &inv) const;
		Bool_t operator==(const this_type &inv) const;
		Bool_t operator==(const parameters_type &par) const;
		Bool_t operator!=(const this_type &inv) const;

		Bool_t add(const this_type &inv, Bool_t check = true);

		virtual void Print(Option_t* option) const;

		Bool_t debug;

		MEMBER_DEF(parameters_type, BinParameters)
		HISTO_DEF(distribution_type, Distribution, 0)
		HISTO_DEF(distribution_2d_type, DistributionTower, 0)
		HISTO_DEF(distribution_2d_type, DistributionEtaCoord, 0)
		distribution_type *getDistribution() {return histDistribution;}
		distribution_2d_type *getDistributionTower() {return histDistributionTower;}
		distribution_2d_type *getDistributionEtaCoord() {return histDistributionEtaCoord;}

		Bool_t fill(const TCandidateParameters &candidate, Float_t w);
		Bool_t fill(const TPointParameters &point, Float_t val, Float_t w);

		ClassDef(TInvariantMassDistribution, STPI0ANALYSIS_VERSION);

protected:
		Bool_t fill(Float_t binValue, Float_t value, Int_t towerId, Float_t etaCoord, Float_t w);

};

typedef TInvariantMassDistribution distribution_type;
typedef list<distribution_type> distribution_list_type;

#endif
