#ifndef EfficiencyPlots_HH
#define EfficiencyPlots_HH
#include "TObject.h"
#include "StiEvaluator/StiEvaluatorHistograms.h"

class StMiniMcEvent;
class StMiniMcPair;

class EfficiencyPlots : public StiEvaluatorHistograms
{
public:
  EfficiencyPlots(const char * name, 
		  const char * description, 
		  double minMult,
		  double maxMult,
		  double minZ,
		  double maxZ,
		  int    id,
		  double  minNHits,
		  double  minNFitHits,
		  double  maxDca,
		  double  minEta,
		  double  maxEta,
		  int     trackType);
  virtual ~EfficiencyPlots();
  virtual void initialize();
  virtual void fill(StMiniMcEvent*);
  virtual void finish();

 protected:
  int    _eventCount;
  TH1D * _h_pt_mc;
  TH1D * _h_eta_mc;
  TH1D * _h_phi_mc;
  TH2D * _h_ptVsEta_mc;
  TH2D * _h_phiVsEta_mc;
  
  TH1D * _h_pt_rec;
  TH1D * _h_eta_rec;
  TH1D * _h_phi_rec;
  TH2D * _h_ptVsEta_rec;
  TH2D * _h_phiVsEta_rec;

  TH1D * _h_pt_ratio;
  TH1D * _h_eta_ratio;
  TH1D * _h_phi_ratio;
  TH2D * _h_ptVsEta_ratio;
  TH2D * _h_phiVsEta_ratio;

  TProfile * _p_pt;
  TProfile * _p_eta;
  TProfile * _p_phi;
  TProfile2D * _p_ptVsEta_mc;
  TProfile2D * _p_phiVsEta_mc;

  TH1D * _h_dca10;
  TH1D * _h_dca20;
  TH1D * _h_dca30;
  TH1D * _h_dca40;

  ClassDef(EfficiencyPlots,1)
};

#endif
