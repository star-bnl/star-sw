#include "Stiostream.h"
#include "StiEvaluatorHistograms.h"
#include "StMiniMcEvent/StMiniMcEvent.h"
#include "StMiniMcEvent/StMiniMcPair.h"

StiEvaluatorHistograms::StiEvaluatorHistograms(const string & name, 
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
					       int     trackType)
  : HistogramGroup(name,
		   description), 
    _minMult(minMult),
    _maxMult(maxMult),
    _minZ(minZ),
    _maxZ(maxZ),
    _id(id),
    _minNHits(_minNHits),
    _minNFitHits(minNFitHits),
    _maxDca(maxDca),
    _minEta(minEta),
    _maxEta(maxEta), 
    _trackType(trackType)
{}

StiEvaluatorHistograms::~StiEvaluatorHistograms()
{}

//Dummy version does nothing
void StiEvaluatorHistograms::fill(StMiniMcEvent * event)
{}

//Dummy version does nothing
void StiEvaluatorHistograms::initialize()
{}

//Dummy version does nothing
void StiEvaluatorHistograms::finish()
{}

bool StiEvaluatorHistograms::acceptEtaCut(double eta)
{
  if ( (eta >= _minEta) && (eta <= _maxEta) ) return true;
  return false;
}

bool StiEvaluatorHistograms::acceptPair(StMiniMcPair* pair) 
{
  if (!(pair->nHitMc() >= _minNHits)) return false;
  //if (!acceptEtaCut(pair->etaMc())) return false;
  if (!(pair->fitPts() >= _minNFitHits)) return false; // thesis >= 24, BigBad >=25
  if (!(pair->dcaGl()  <  _maxDca)) return false; // thesis primary dca (only for P00hm), Big Bad global dca
  // Now check that the charge matches the specified ID
  // pi-, k-, pbar = 9, 12, 15
  // pi+, k+, prot = 8, 11, 14
  if ((_id==8 || _id==11 || _id==14) && pair->charge()<0) return false;
  if ((_id==9 || _id==12 || _id==15) && pair->charge()>0) return false;
  return true;
}

bool StiEvaluatorHistograms::acceptGeantId(int id) 
{
  if (_id == 9999) // _id is not set, use background for all Nch (pi, k, p)
    return	(id == 8  || id ==  9 ||
		 id == 11 || id == 12 ||
		 id == 14 || id == 15); 
  else return (_id==id); 
  
}

/// FitSlicesY the given histogram and
/// add the results to the group
void StiEvaluatorHistograms::slice(TH2D*h,double meanMin,double meanMax,double sigmaMin,double sigmaMax)
{
  h->FitSlicesY();  
  string name;
  string title;
  name = h->GetName();
  name += "_1";

  //cout << "StiEvaluatorHistograms::slice() -I- histoName:"<< name<<endl;
  TH1D * hh;
  hh = (TH1D*)gDirectory->Get(name.c_str());
  if (hh)
    {
      hh->SetMinimum(meanMin);
      hh->SetMaximum(meanMax); 
      name = h->GetName();
      name += "_Mean";
      title = h->GetName();
      title += " - Mean";
      hh->SetName(name.c_str());
      hh->SetTitle(title.c_str());
      add(hh);
    }
  else
    {
      cout << "StiEvaluatorHistograms::slice() -E- no object of that name found!"<<endl;
    }
  name = h->GetName();
  name += "_2";
  hh = (TH1D*)gDirectory->Get(name.c_str());
  if (hh)
    {
      hh->SetMinimum(sigmaMin);
      hh->SetMaximum(sigmaMax);
      name  = h->GetName();
      name  += "_Sigma";
      title = h->GetName();
      title += " - Sigma";
      hh->SetName(name.c_str());
      hh->SetTitle(title.c_str());
      add(hh);
    }
}

/// Calculate the standard deviation on the basis of the mean (first moment),
/// and second moment distribution.
void StiEvaluatorHistograms::calculateSTD(TProfile * h1, TProfile * h2, TH1D * h)
{
  int n1 = h1->GetNbinsX();
  int n2 = h2->GetNbinsX();
  int n  = h->GetNbinsX();
  if (!h1 || !h2 || !h)
    {
      cout << "StiEvaluatorHistograms::calculateSTD( ) -E- Arguments are null pointers" <<endl;
      return;
    }
  if (n1!=n2 || n2!=n)
    {
      cout << "StiEvaluatorHistograms::calculateSTD( ) -E- Histos with different bin counts" << endl;
      cout << h1->GetName() << " n:" << n1 << endl;
      cout << h2->GetName() << " n:" << n2 << endl;
      cout << h->GetName() << " n:" << n << endl;
      return;
    }
  double v1,v2,s;
  for (int i=1; i<=n; ++i)
    {
      v1=h1->GetBinContent(i);
      v2=h2->GetBinContent(i);
      double var = v2-v1*v1;
      if (var>0)
	s = ::sqrt(var);
      else
	s = -1;
      h->SetBinContent(i,s);
    }
}

///Calculate ratio of 2D histogram h1 by 2d histo h2 into h3.
void StiEvaluatorHistograms::divide(TH2D * h1, TH2D* h2, TH2D* h3)
{
  if (!h1 || !h2 || !h3)
    {
      cout << "StiEvaluatorHistograms::divide( ) -E- Arguments are null pointers" <<endl;
      return;
    }
  int n1x = h1->GetNbinsX();
  int n2x = h2->GetNbinsX();
  int n3x = h3->GetNbinsX();
  int n1y = h1->GetNbinsY();
  int n2y = h2->GetNbinsY();
  int n3y = h3->GetNbinsY();
  if (n1x!=n2x || n2x!=n3x || n1y!=n2y || n2y!=n3y)
    {
      cout << "StiEvaluatorHistograms::divide( ) -E- Histos with different bin counts" << endl;
      cout << h1->GetName() << " nx:" << n1x <<  " ny:" << n1x << endl;
      cout << h2->GetName() << " nx:" << n2x <<  " ny:" << n2x << endl;
      cout << h3->GetName() << " nx:" << n3x <<  " ny:" << n3x << endl;
      return;
    }  
  double v1,v2,ratio;
  for (int i=1; i<=n1x; ++i)
    { 
      for (int j=1; j<=n1y; ++j)
	{
	  v1=h1->GetBinContent(i,j);
	  v2=h2->GetBinContent(i,j);
	  if (v2<=0) 
	    ratio = 0.;
	  else
	    ratio = v1/v2;
	  h3->SetBinContent(i,j,ratio);
	}
    }
}
