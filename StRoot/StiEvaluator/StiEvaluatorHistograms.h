#ifndef StiEvaluatorHistograms_H_INCLUDED
#define StiEvaluatorHistograms_H_INCLUDED
#include "Sti/Base/HistogramGroup.h"
class StMiniMcEvent;
class StMiniMcPair;

class StiEvaluatorHistograms : public HistogramGroup
{
 public:
  StiEvaluatorHistograms(const string & name, 
			 const string & description, 
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
  virtual ~StiEvaluatorHistograms();
  virtual void initialize();
  virtual void finish();
  virtual void fill(StMiniMcEvent * event); 
  void calculateSTD(TProfile * h1, TProfile * h2, TH1D * h);
  bool acceptEtaCut(double);
  bool acceptPair(StMiniMcPair*);
  bool acceptGeantId(int);
  void slice(TH2D*h, double meanMin=-1.,double meanMax=1., double sigmaMin=0., double sigmaMax=1.);
  void divide(TH2D * h1, TH2D* h2, TH2D* h3);

 protected:
  double _minMult;
  double _maxMult;
  double _minZ;
  double _maxZ;
  int    _id;
  double _minNHits;
  double _minNFitHits;
  double _maxDca;
  double _minEta;
  double _maxEta;

  int    _trackType;

  StMiniMcEvent* minimcevent; 
};



#endif
