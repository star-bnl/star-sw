#ifndef ResolutionPlots_HH
#define ResolutionPlots_HH
#include "TObject.h"
#include "StiEvaluator/StiEvaluatorHistograms.h"

class StMiniMcEvent;
class StMiniMcPair;

class ResolutionPlots : public StiEvaluatorHistograms
{
public:
  ResolutionPlots(const char* name, 
		  const char* description, 
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
		  int     trackType,
		  int     pullType);
  virtual ~ResolutionPlots();
  virtual void initialize();
  virtual void fill(StMiniMcEvent*);
  void finish();

  
  double _minMult;
  double _maxMult;
  int    _id;
  double _minNHits;
  double _minNFitHits;
  double _maxDca;
  double _minEta;
  double _maxEta;
  
  int    _trackType;
  int    _pullType;

  TH1D * _h_p_mc;
  TH1D * _h_pt_mc;
  TH1D * _h_eta_mc;
  TH1D * _h_phi_mc;
  
  TH1D * _h_p_rec;
  TH1D * _h_pt_rec;
  TH1D * _h_eta_rec;
  TH1D * _h_phi_rec;
  
  TH2D * _h_dpVsP;
  TH2D * _h_dptVsP;
  TH2D * _h_dptVsPt;
  TH2D * _h_detaVsEta;
  TH2D * _h_dphiVsPt;

  TH2D * _h_dpByPVsP;
  TH2D * _h_dptByPtVsPt;
  TH2D * _h_dptByPtVsP;

  TProfile * _p_dpVsP;
  TProfile * _p_dptVsP;
  TProfile * _p_dptVsPt;
  TProfile * _p_detaVsEta;
  TProfile * _p_dphiVsPt;

  TProfile * _p_dpByPVsP;
  TProfile * _p_dptByPtVsPt;
  TProfile * _p_dptByPtVsP;

  TProfile * _p_dpByP2VsP;
  TProfile * _p_dptByPt2VsPt;
  TProfile * _p_dptByPt2VsP;
  TProfile * _p_deta2VsEta;
  TProfile * _p_dphi2VsPt;

  TH1D * _h_sigdpByPVsP;
  TH1D * _h_sigdptByPtVsPt;
  TH1D * _h_sigdptByPtVsP;
  TH1D * _h_sigdetaVsEta;
  TH1D * _h_sigdphiVsPt;

  TH1D * _h_dpVsP_1;
  TH1D * _h_dpVsP_2;
  TH1D * _h_dptVsP_1;
  TH1D * _h_dptVsP_2;

  TH1D * _h_dpByPVsP_1;
  TH1D * _h_dpByPVsP_2;
  TH1D * _h_dptByPtVsP_1;
  TH1D * _h_dptByPtVsP_2;

  TH1D * _h_curvPull;
  TH1D * _h_ptPull;
  TH1D * _h_tanPull;
  TH2D * _h_curvPullVsPt;
  TH2D * _h_ptPullVsPt;
  TH2D * _h_tanPullVsPt;

  TH1D * _h_chi2;
  TH1D * _h_rChi2;
  TH2D * _h_rChi2VsPt;
  TH2D * _h_rChi2VsEta;
  TH2D * _h_rChi2VsNpts;

  ClassDef(ResolutionPlots,1)
};

#endif
