#include <stdexcept>
#include "Parameter.h"
#include "StiTrack.h"
#include "StiSimpleTrackFilter.h"

StiSimpleTrackFilter::StiSimpleTrackFilter()
  : Parameters("SimpleTrackFilter","SimpleTrackFilter")
{
  initialize();
}

StiSimpleTrackFilter::StiSimpleTrackFilter(const string & name, const string & description)
  : Parameters(name,description)
{
  initialize();
}

StiSimpleTrackFilter::~StiSimpleTrackFilter()
{}

void StiSimpleTrackFilter::initialize()
{
  parameterVector.clear();
  add("Chi2Used", "Use Chi2",     false, Parameter::Boolean, StiTrack::kChi2);
  add("Chi2Min",  "Minimum Chi2", 0.,    Parameter::Double, StiTrack::kChi2);
  add("Chi2Max",  "Maximum Chi2", 20.,   Parameter::Double, StiTrack::kChi2);
  
  add("PtUsed",   "Use Pt",     false,   Parameter::Boolean, StiTrack::kPt);
  add("PtMin",    "Minimum Pt", 0.1,     Parameter::Double, StiTrack::kPt);
  add("PtMax",    "Maximum Pt", 10.,     Parameter::Double, StiTrack::kPt);
  
  add("PUsed",    "Use P",     false,    Parameter::Boolean, StiTrack::kP);
  add("PMin",     "Minimum P", 0.,       Parameter::Double, StiTrack::kP);
  add("PMax",     "Maximum P", 10.,      Parameter::Double, StiTrack::kP);
  
  add("EtaUsed",  "Use Eta",     false,  Parameter::Boolean, StiTrack::kPseudoRapidity);
  add("EtaMin",   "Minimum Eta", -1.5,   Parameter::Double, StiTrack::kPseudoRapidity);
  add("EtaMax",   "Maximum Eta",  1.5,   Parameter::Double, StiTrack::kPseudoRapidity);
  
  add("nPtsUsed", "Use nPts",     false, Parameter::Boolean, StiTrack::kPointCount);
  add("nPtsMin",  "Minimum nPts", 0.,    Parameter::Integer, StiTrack::kPointCount);
  add("nPtsMax",  "Maximum nPts", 60.,   Parameter::Integer, StiTrack::kPointCount);

  add("nGapsUsed","Use nGaps",     false, Parameter::Boolean, StiTrack::kGapCount);
  add("nGapsMin", "Minimum nGaps", 0.,    Parameter::Integer, StiTrack::kGapCount);
  add("nGapsMax", "Maximum nGaps", 60.,   Parameter::Integer, StiTrack::kGapCount);

  //add("NToNmaxPtsUsed", "Use NToNmaxPts",false, Parameter::Boolean, StiTrack::);
  //add("NToNmaxPtsMin","Minimum NToNmaxPts",0.25, Parameter::Double, StiTrack::);
  //add("NToNmaxPtsMax","Maximum NToNmaxPts",1.0 , Parameter::Double, StiTrack::);

  add("PhiUsed",  "Use Phi",     false,  Parameter::Boolean, StiTrack::kPhi);
  add("PhiMin",   "Minimum Phi", 0.,     Parameter::Double, StiTrack::kPhi);
  add("PhiMax",   "Maximum Phi", 6.3,   Parameter::Double, StiTrack::kPhi);
}

bool StiSimpleTrackFilter::accept(StiTrack * t) const
{
  cout << "StiSimpleTrackFilter::accept(StiTrack * t)  - INFO - Starting" << endl;
  double v,low,high;
  ParameterConstIterator iter = begin();
  Parameter * parUse;
  Parameter * parLow;
  Parameter * parHi;
  while (iter!=end())
    {
      parUse = *iter; iter++;
      parLow = *iter; iter++;
      parHi  = *iter; iter++;
      if (parUse&&parLow&&parHi)
	{
	  if (parUse->getValue())
	    {
	      v = t->getValue(parUse->getKey());
	      low = parLow->getValue();
	      high = parHi->getValue();
	      if (v<low || v>high)
		return false;
	    }
	}
      else
	{
	  cout << "StiSimpleTrackFilter::accept(StiTrack * t) - INFO - Internal Error" << endl;
	}
    }
  return true;
}


StiSimpleTrackFilterFactory::StiSimpleTrackFilterFactory(const string& newName,
							 int original,
							 int incremental, 
							 int maxInc)
  : StiObjectFactoryInterface<StiTrackFilter>(newName, 
					      original, 
					      incremental, 
					      maxInc)
{
  initialize();
}

StiSimpleTrackFilterFactory::~StiSimpleTrackFilterFactory()
{
  // cout <<"StiSimpleTrackFilterFactory::~StiSimpleTrackFilterFactory()"<<endl;
}

void * StiSimpleTrackFilterFactory::makeNewObject() const
{
  cout << "StiSimpleTrackFilterFactory::makeNewObject() - Instantiating StiSimpleTrackFilter()." << endl;
  return new StiSimpleTrackFilter();
}
  
