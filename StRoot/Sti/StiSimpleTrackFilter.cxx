#include "StiTrack.h"
#include "StiSimpleTrackFilter.h"

StiSimpleTrackFilter::StiSimpleTrackFilter()
{
	reset();
	setDefaults();
}

StiSimpleTrackFilter::~StiSimpleTrackFilter()
{
	for (int j=0;j<n;j++)
		{
			delete names[j];
		}	
}

void StiSimpleTrackFilter::reset()
{
	n=25;
	for (int j=0;j<n;j++)
		{
			used[j]  = false;
			low[j]   = 0.;
			hi[j]    = -1.;
			names[j] = new string("null");
		}
}

void StiSimpleTrackFilter::set(int id, const char * name, bool useIt, double minimum, double maximum)
{
	if (id<0||id>=n)
		{
			cout << "StiSimpleTrackFilter::set() - Warning - Invalid id - Filter not set"<<endl;
			return;
		}
	used[id]  = useIt;
	low[id]   = minimum;
	hi[id]    = maximum;
	*names[id] = name;
}


void StiSimpleTrackFilter::setDefaults()
{
	set(kChi2,          "chi2",          true,   0.,  50.);
	set(kPhi,           "PHI",           false,  0.,   1.);
	set(kPt,            "Pt",            false,  0.,   1.);
	set(kP,             "P",             false,  0.,   1.);

	set(kPseudoRap,     "PseudoRap",     false, -2.,   2.);
	set(kNPts,          "NPts",          true,   8., 100.);
	set(kNGaps,         "NGaps",         false,  0.,   1.);
	set(kNToNmaxPts,    "NToNmaxPts",    false,  0.,   1.);

	set(kNTpcPts,       "NTpcPts",       false,  0.,   1.);
	set(kNSvtPts,       "NSvtPts",       false,  0.,   1.);
	set(kTpcDedx,       "TpcDedx",       false,  0.,   1.);
	set(kSvtDedx,       "SvtDedx",       false,  0.,   1.);
}

bool StiSimpleTrackFilter::accept(StiTrack * t)
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


