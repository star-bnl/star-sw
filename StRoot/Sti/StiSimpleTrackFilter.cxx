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
  add("Chi2Used", "Use Chi2",     false, Parameter::Boolean);
  add("Chi2Min",  "Minimum Chi2", 0.,    Parameter::Double);
  add("Chi2Max",  "Maximum Chi2", 20.,   Parameter::Double);
  
  add("PtUsed",   "Use Pt",     false,   Parameter::Boolean);
  add("PtMin",    "Minimum Pt", 0.1,     Parameter::Double);
  add("PtMax",    "Maximum Pt", 10.,     Parameter::Double);
  
  add("PUsed",    "Use P",     false,    Parameter::Boolean);
  add("PMin",     "Minimum P", 0.,       Parameter::Double);
  add("PMax",     "Maximum P", 10.,      Parameter::Double);
  
  add("EtaUsed",  "Use Eta",     false,  Parameter::Boolean);
  add("EtaMin",   "Minimum Eta", -1.5,   Parameter::Double);
  add("EtaMax",   "Maximum Eta",  1.5,   Parameter::Double);
  
  add("nPtsUsed", "Use nPts",     false, Parameter::Boolean);
  add("nPtsMin",  "Minimum nPts", 0.,    Parameter::Integer);
  add("nPtsMax",  "Maximum nPts", 60.,   Parameter::Integer);

  add("nGapsUsed","Use nGaps",     false, Parameter::Boolean);
  add("nGapsMin", "Minimum nGaps", 0.,    Parameter::Integer);
  add("nGapsMax", "Maximum nGaps", 60.,   Parameter::Integer);

  add("NToNmaxPtsUsed", "Use NToNmaxPts",false, Parameter::Boolean);
  add("NToNmaxPtsMin","Minimum NToNmaxPts",0.25, Parameter::Double);
  add("NToNmaxPtsMax","Maximum NToNmaxPts",1.0 , Parameter::Double);

  add("PhiUsed",  "Use Phi",     false,  Parameter::Boolean);
  add("PhiMin",   "Minimum Phi", 0.,     Parameter::Double);
  add("PhiMax",   "Maximum Phi", 6.3,   Parameter::Double);
}

bool StiSimpleTrackFilter::accept(StiTrack * t) const
{

  int j=0;	double v;
  if (parameterVector[j++]->getValue()) 
    { 
      v = t->getChi2();
      if (v<parameterVector[j++]->getValue() || v>parameterVector[j++]->getValue())
	return false;
    }
  if (parameterVector[j]->getValue()) 
    {
      v = t->getPseudoRapidity();  
      if (v<parameterVector[j++]->getValue() || v>parameterVector[j++]->getValue())
	return false;
    }
  if (parameterVector[j]->getValue()) 
    { 
      v = t->getPointCount();      
      if (v<parameterVector[j++]->getValue()||v>parameterVector[j++]->getValue())
	return false;
    } 
  if (parameterVector[j]->getValue()) 
    {
      v = t->getGapCount();
      if (v<parameterVector[j++]->getValue()||v>parameterVector[j++]->getValue())
	return false;
    }
  if (parameterVector[j]->getValue()) 
    {
      v = t->getPhi();     
      if (v<parameterVector[j++]->getValue()||v>parameterVector[j++]->getValue())
	return false;
    }
  if (parameterVector[j]->getValue()) 
    { 
      v = t->getPt();     
      if (v<parameterVector[j++]->getValue()||v>parameterVector[j++]->getValue())
	return false;
    }
  if (parameterVector[j]->getValue()) 
    {
      v = t->getP(); 
      if (v<parameterVector[j++]->getValue()||v>parameterVector[j++]->getValue())
	return false;
    }
  /*
  if (parameterVector[j]->getValue()) 
    { 
      v = t->getNToNmaxPts();  
      if (v<parameterVector[j++]->getValue()||v>parameterVector[j++]->getValue()) 
	return false;
    }
  */
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
  
