#include <stdexcept>
#include <string>
using std::string;
#include "Sti/Parameter.h"
#include "Sti/EditableParameter.h"
#include "Sti/StiTrack.h"
#include "StiRootSimpleTrackFilter.h"

StiRootSimpleTrackFilter::StiRootSimpleTrackFilter()
  : StiTrackFilter(),
    EditableParameters()
{
  initialize();
}


StiRootSimpleTrackFilter::~StiRootSimpleTrackFilter()
{}

void StiRootSimpleTrackFilter::initialize()
{
  cout << "StiRootSimpleTrackFilter::initialize() - INFO - Starting" << endl;
  parameterVector.clear();
  add("Chi2Used", "Use Chi2",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kChi2);
  add("Chi2Min",  "Minimum Chi2", 0., 0., 0., 100.,2,Parameter::Double, StiTrack::kChi2);
  add("Chi2Max",  "Maximum Chi2", 20., 20., 0., 100.,2,Parameter::Double, StiTrack::kChi2);
  
  add("PhiUsed",  "Use Phi",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPhi);
  add("PhiMin",   "Minimum Phi", 0.,   0.,  0., 6.3,2,Parameter::Double, StiTrack::kPhi);
  add("PhiMax",   "Maximum Phi", 6.3, 6.3, 0., 6.3,2,Parameter::Double, StiTrack::kPhi);
  
  add("PtUsed",   "Use Pt",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPt);
  add("PtMin",    "Minimum Pt", 0., 0., 0., 100.,2,Parameter::Double, StiTrack::kPt);
  add("PtMax",    "Maximum Pt", 10., 10., 0., 100.,2,Parameter::Double, StiTrack::kPt);
  
  add("PUsed",    "Use P",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kP);
  add("PMin",     "Minimum P", 0., 0., 0., 100.,2,Parameter::Double, StiTrack::kP);
  add("PMax",     "Maximum P", 10., 10., 0., 100.,2,Parameter::Double, StiTrack::kP);
  
  add("EtaUsed",  "Use Eta",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPseudoRapidity);
  add("EtaMin",   "Minimum Eta", -1.5, -1.5, -10., 10.,2,Parameter::Double, StiTrack::kPseudoRapidity);
  add("EtaMax",   "Maximum Eta",  1.5,  1.5, -10., 10.,2,Parameter::Double, StiTrack::kPseudoRapidity);
  
  add("nPtsUsed", "Use nPts",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPointCount);
  add("nPtsMin",  "Minimum nPts", 0., 0., 0., 100.,1,Parameter::Integer, StiTrack::kPointCount);
  add("nPtsMax",  "Maximum nPts", 60., 60., 0., 100.,1,Parameter::Integer, StiTrack::kPointCount);

  add("nGapsUsed","Use nGaps",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kGapCount);
  add("nGapsMin", "Minimum nGaps", 0., 0., 0., 100.,1,Parameter::Integer, StiTrack::kGapCount);
  add("nGapsMax", "Maximum nGaps", 60., 60., 0., 100.,1,Parameter::Integer, StiTrack::kGapCount);

  //add("NToNmaxPtsUsed", "Use NToNmaxPts",false,false,0,1,1,Parameter::Boolean);
  //add("NToNmaxPtsMin","Minimum NToNmaxPts",0.25, 0.25, 0.,1.,1.,Parameter::Double);
  //add("NToNmaxPtsMax","Maximum NToNmaxPts",1.00, 1.00, 0.,1.,1.,Parameter::Double);
}

void StiRootSimpleTrackFilter::setDefaults()
{
  ParameterConstIterator iter;
  for (iter=begin(); iter!=end(); iter++)
    {
      EditableParameter * par = static_cast<EditableParameter *>(*iter);
      par->reset();
    }
}

bool StiRootSimpleTrackFilter::accept(StiTrack * t) const
{
  //cout << "StiRootSimpleTrackFilter::accept(t) - INFO - Starting" << endl;
  double v,low,high;
  ParameterConstIterator iter = begin();
  Parameter * parUse;
  Parameter * parLow;
  Parameter * parHi;
  int count=0;
  while (iter!=end())
    {
      parUse = *iter; iter++;
      parLow = *iter; iter++;
      parHi  = *iter; iter++;
      if (parUse&&parLow&&parHi)
	{
	  if (parUse->getValue())
	    {
	      //cout << "/"<<count++;
	      v = t->getValue(parUse->getKey());
	      low = parLow->getValue();
	      high = parHi->getValue();
	      if (v<low || v>high)
		{
		  //cout<<"/false"<<endl;
		  return false;
		}
	    }
	}
      else
	{
	  //cout << "StiSimpleTrackFilter::accept(StiTrack * t) - INFO - Internal Error" << endl;
	}
    }
  //cout<<"/true"<<endl;
  return true;
}


StiRootSimpleTrackFilterFactory::StiRootSimpleTrackFilterFactory(const string & newName, 
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

StiRootSimpleTrackFilterFactory::~StiRootSimpleTrackFilterFactory()
{
  // cout <<"StiRootSimpleTrackFilterFactory::~StiRootSimpleTrackFilterFactory()"<<endl;
}

void * StiRootSimpleTrackFilterFactory::makeNewObject() const
{
  cout << "StiRootSimpleTrackFilterFactory::makeNewObject - INFO - instantiating StiRootSimpleTrackFilter" << endl;
  
  return new StiRootSimpleTrackFilter();
}
