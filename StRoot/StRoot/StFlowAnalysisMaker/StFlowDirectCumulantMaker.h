///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowDirectCumulantMaker.h,v 1.2 2014/08/06 11:43:14 jeromel Exp $
//
// Authors: Dhevan Gangadharan, UCLA, Dec 2009
//
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using direct cumulants
//
///////////////////////////////////////////////////////////////////////////////

#ifndef StFlowDirectCumulantMaker_H
#define StFlowDirectCumulantMaker_H
#include "StMaker.h"
#include "StFlowMaker/StFlowConstants.h"
#include "TString.h"
class StFlowEvent;
class StFlowSelection;
class TH2D;

class StFlowDirectCumulantMaker : public StMaker {

  
 public:
    
  struct EventEntries{
    TH2D *cent_yields;
  };
  struct EventEntries Entries[Flow::SPECIES];

  struct  structure4{
    TH2D *Sum_Singles; // correlations
    TH2D *Sum_Squares; // for errors
  };

  struct structure3{
    struct structure4 Species[Flow::SPECIES];
  };
  struct structure2{
    struct structure3 Phase[Flow::PHASES];
  };

  struct structure1{
    struct structure2 Type[Flow::TYPES];
  };

  struct structure1 Term[Flow::TERMS];  

  TH1D *Event_counter;
  TH1D *Event_counterWeighted;

  /// Default constructor
           StFlowDirectCumulantMaker(const Char_t* name="FlowDirectCumulant");
  /// Constructor with selection object
          StFlowDirectCumulantMaker(const Char_t* name,
			       const StFlowSelection& pFlowSelect);
           StFlowDirectCumulantMaker(const StFlowDirectCumulantMaker &from) {};
  virtual  ~StFlowDirectCumulantMaker();

  Int_t    Init();
  Int_t    Make();
  Int_t    Finish();
  virtual  const char *GetCVS() const {static const char cvs[]=
    "Tag $Name:  $ $Id: StFlowDirectCumulantMaker.h,v 1.2 2014/08/06 11:43:14 jeromel Exp $ built " __DATE__ " " __TIME__ ;
    return cvs;}

private:

  void GetMweights();
  void FillParticleArrays();
  void CalculateTerms();
  void CalculateSumTerms(Bool_t);
  void Combinations(Bool_t, int, int);
  void CalculateCorrelationsIntegrated();
  void CalculateCorrelationsDifferential();
  void Fill_Histograms();
  void ClearTerms();

  int TERM1,TERM2,TERM3,TERM4,TERM5;
  int TERM6,TERM7,TERM8,TERM9,TERM10;
  int DIF,INT;
  int COS,SIN;
  
  double Mweights[30][1000];

  int    grefmult, zbin;
  int    id_p[Flow::MAXMULT]; // for primaries
  double phi_p[Flow::MAXMULT];
  double pt_p[Flow::MAXMULT];
  int    ptbin_p[Flow::MAXMULT];
  double weights_p[Flow::MAXMULT];
  int    p_count;
  double ref_combos_4, ref_combos_3, ref_combos_2, ref_combos_1, ref_combos_0; // no. of comb.

  double pt_bins[Flow::PTBINS];
  int    pt_bin_counter_p[Flow::PTBINS];
  
  double sum_1_cos_diff[Flow::SPECIES][Flow::PTBINS], sum_1_sin_diff[Flow::SPECIES][Flow::PTBINS];
  double sum_2_cos_diff[Flow::SPECIES][Flow::PTBINS], sum_2_sin_diff[Flow::SPECIES][Flow::PTBINS];
  double sum_3_cos_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS], sum_3_sin_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS];
  double sum_4_cos_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS], sum_4_sin_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS];
  double sum_5_cos_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS], sum_5_sin_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS];
  double sum_6_cos_sin_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS], sum_6_sin_cos_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS];
  double sum_7_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS], sum_8_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS];
  double sum_9_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS], sum_10_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS];
  double sum_11_cos_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS], sum_11_sin_diff[Flow::SPECIES][Flow::PTBINS][Flow::PTBINS];

  double sum_1_cos_full[Flow::SPECIES], sum_1_sin_full[Flow::SPECIES];
  double sum_2_cos_full[Flow::SPECIES], sum_2_sin_full[Flow::SPECIES];
  
  double sum_1_cos[Flow::SPECIES][Flow::PTBINS], sum_1_sin[Flow::SPECIES][Flow::PTBINS];
  double sum_2_cos[Flow::SPECIES][Flow::PTBINS], sum_2_sin[Flow::SPECIES][Flow::PTBINS];
  double sum_3_cos[Flow::SPECIES][Flow::PTBINS], sum_3_sin[Flow::SPECIES][Flow::PTBINS];
  double sum_4_cos[Flow::SPECIES][Flow::PTBINS], sum_4_sin[Flow::SPECIES][Flow::PTBINS];
  double sum_5_cos[Flow::SPECIES][Flow::PTBINS], sum_5_sin[Flow::SPECIES][Flow::PTBINS];
  double sum_6_cos_sin[Flow::SPECIES][Flow::PTBINS], sum_6_sin_cos[Flow::SPECIES][Flow::PTBINS];
  double sum_7[Flow::SPECIES][Flow::PTBINS], sum_8[Flow::SPECIES][Flow::PTBINS];
  double sum_9[Flow::SPECIES][Flow::PTBINS], sum_10[Flow::SPECIES][Flow::PTBINS];
  double sum_11_cos[Flow::SPECIES][Flow::PTBINS], sum_11_sin[Flow::SPECIES][Flow::PTBINS];

  double sum_1_cos_reference[Flow::SPECIES][Flow::PTBINS], sum_1_sin_reference[Flow::SPECIES][Flow::PTBINS];
  double sum_2_cos_reference[Flow::SPECIES][Flow::PTBINS], sum_2_sin_reference[Flow::SPECIES][Flow::PTBINS];
  double sum_3_cos_reference[Flow::SPECIES][Flow::PTBINS], sum_3_sin_reference[Flow::SPECIES][Flow::PTBINS];
  double sum_11_cos_reference[Flow::SPECIES][Flow::PTBINS], sum_11_sin_reference[Flow::SPECIES][Flow::PTBINS];
  double Cumulant_Terms[Flow::TERMS][Flow::TYPES][Flow::PHASES][Flow::SPECIES][Flow::PTBINS];

  StFlowEvent*     pFlowEvent;  //! pointer to StFlowEvent
  StFlowSelection* pFlowSelect; //! selection object

  TString      MakerName;

  ClassDef(StFlowDirectCumulantMaker,0)              // macro for rootcint
};

#endif

