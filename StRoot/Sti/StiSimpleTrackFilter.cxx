#include "StiTrack.h"
#include "StiSimpleTrackFilter.h"

StiSimpleTrackFilter::StiSimpleTrackFilter()
{
  setDefaults();
}

StiSimpleTrackFilter::~StiSimpleTrackFilter()
{
  for (int j=0;j<100;j++)
    {
      delete names[j];
    }	
}

void StiSimpleTrackFilter::set(int id, const char * name, double minimum, double maximum, bool useIt)
{
  if (id<0||id>=100)
    {
      cout << "StiSimpleTrackFilter::set() - Warning - Invalid id:" << id
	   << " - Filter not set"<<endl;
      return;
    }
  used[id]  = useIt;
  low[id]   = minimum;
  hi[id]    = maximum;
  //*names[id] = name;
}

void StiSimpleTrackFilter::set(int id, double minimum, double maximum, bool use=true)
{  
  if (id<0||id>=100)
    {
      cout << "StiSimpleTrackFilter::set() - Warning - Invalid id:" << id
	   << " - Filter not set"<<endl;
      return;
    }
  used[id]  = use;
  low[id]   = minimum;
  hi[id]    = maximum;
}



void StiSimpleTrackFilter::setDefaults()
{
  cout << "StiSimpleTrackFilter::setDefaults() - Starting" << endl;
 for (int i=0;i<100;i++)
    {
      used[i] = false;
      low[i]  = 0.;
      hi[i]   = -1.;
      //*names[i] = new string("");
    }
  cout << "StiSimpleTrackFilter::setDefaults() - Setting usual parameters" << endl;
 
  // set defaults - with false
  set(kChi2,      "chi2",       0.,  50.);
  set(kPhi,       "PHI",        0.,   1.);
  set(kPt,        "Pt",         0.,   1.);
  set(kP,         "P",          0.,   1.);
  
  set(kPseudoRap, "PseudoRap",  -2.,   2.);
  set(kNPts,      "NPts",       8., 100.);
  set(kNGaps,     "NGaps",      0.,   1.);
  set(kNToNmaxPts,"NToNmaxPts", 0.,   1.);
  
  set(kNTpcPts,   "NTpcPts",    0.,   1.);
  set(kNSvtPts,   "NSvtPts",    0.,   1.);
  set(kTpcDedx,   "TpcDedx",    0.,   1.);
  set(kSvtDedx,   "SvtDedx",    0.,   1.);
  cout << "StiSimpleTrackFilter::setDefaults() - Done" << endl;

}

bool StiSimpleTrackFilter::accept(StiTrack * t) const
{
  int j=0;	double v;
  j=kChi2;       if (used[j]) { v = t->getChi2();            if (v<low[j]||v>hi[j]) return false;}
  j=kPseudoRap;  if (used[j]) { v = t->getPseudoRapidity();  if (v<low[j]||v>hi[j]) return false;}
  j=kNPts;       if (used[j]) { v = t->getPointCount();      if (v<low[j]||v>hi[j]) return false;}
  j=kNGaps;      if (used[j]) { v = t->getGapCount();        if (v<low[j]||v>hi[j]) return false;}
  j=kPhi;        if (used[j]) { v = t->getPhi();             if (v<low[j]||v>hi[j]) return false;}
  j=kPt;         if (used[j]) { v = t->getPt();              if (v<low[j]||v>hi[j]) return false;}
  j=kP;          if (used[j]) { v = t->getP();               if (v<low[j]||v>hi[j]) return false;}
  
  //j=kNToNmaxPts; if (used[j]) { v = t->getNToNmaxPts();         if (v<low[j]||v>hi[j]) return false;}
  
  //j=kNTpcPts;    if (used[j]) { v = t->getNTpcPts();         if (v<low[j]||v>hi[j]) return false;}
  //j=kNSvtPts;    if (used[j]) { v = t->getNSvtPts();         if (v<low[j]||v>hi[j]) return false;}
  //j=kTpcDedx;    if (used[j]) { v = t->getTpcDedx();         if (v<low[j]||v>hi[j]) return false;}
  //j=kSvtDedx;    if (used[j]) { v = t->getSvtDedx();         if (v<low[j]||v>hi[j]) return false;}
  return true;
}


