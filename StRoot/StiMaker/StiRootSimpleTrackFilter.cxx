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

/*
void StiRootSimpleTrackFilter::add(const string & name, 
				   const string & description,
				   double value, 
				   double defaultValue, 
				   double min, 
				   double max,
				   double increment,
				   int    type)
{
EditableParameter * par = static_cast<EditableParameter*>(StiToolkit::instance()->getParameterFactory()->getObject());
  if (par)
    {
      par->set(name,description,value,defaultValue,min,max,increment,type);
      add(par);
    }
  else
    throw runtime_error("StiRootSimpleTrackFilter::add() - ERROR - static_cast returned null pointer");
}
*/

void StiRootSimpleTrackFilter::initialize()
{
  parameterVector.clear();
  add("Chi2Used", "Use Chi2",     false, false, 0,1,1,Parameter::Boolean);
  add("Chi2Min",  "Minimum Chi2", 0., 0., 0., 100.,2,Parameter::Double);
  add("Chi2Max",  "Maximum Chi2", 20., 20., 0., 100.,2,Parameter::Double);
  
  add("PhiUsed",  "Use Phi",     false, false, 0,1,1,Parameter::Boolean);
  add("PhiMin",   "Minimum Phi", 0.,   0.,  0., 6.3,2,Parameter::Double);
  add("PhiMax",   "Maximum Phi", 6.3, 6.3, 0., 6.3,2,Parameter::Double);
  
  add("PtUsed",   "Use Pt",     false, false, 0,1,1,Parameter::Boolean);
  add("PtMin",    "Minimum Pt", 0., 0., 0., 100.,2,Parameter::Double);
  add("PtMax",    "Maximum Pt", 10., 10., 0., 100.,2,Parameter::Double);
  
  add("PUsed",    "Use P",     false, false, 0,1,1,Parameter::Boolean);
  add("PMin",     "Minimum P", 0., 0., 0., 100.,2,Parameter::Double);
  add("PMax",     "Maximum P", 10., 10., 0., 100.,2,Parameter::Double);
  
  add("EtaUsed",  "Use Eta",     false, false, 0,1,1,Parameter::Boolean);
  add("EtaMin",   "Minimum Eta", -1.5, -1.5, -10., 10.,2,Parameter::Double);
  add("EtaMax",   "Maximum Eta",  1.5,  1.5, -10., 10.,2,Parameter::Double);
  
  add("nPtsUsed", "Use nPts",     false, false, 0,1,1,Parameter::Boolean);
  add("nPtsMin",  "Minimum nPts", 0., 0., 0., 100.,1,Parameter::Integer);
  add("nPtsMax",  "Maximum nPts", 60., 60., 0., 100.,1,Parameter::Integer);

  add("nGapsUsed","Use nGaps",     false, false, 0,1,1,Parameter::Boolean);
  add("nGapsMin", "Minimum nGaps", 0., 0., 0., 100.,1,Parameter::Integer);
  add("nGapsMax", "Maximum nGaps", 60., 60., 0., 100.,1,Parameter::Integer);

  add("NToNmaxPtsUsed", "Use NToNmaxPts",false,false,0,1,1,Parameter::Boolean);
  add("NToNmaxPtsMin","Minimum NToNmaxPts",0.25, 0.25, 0.,1.,1.,Parameter::Double);
  add("NToNmaxPtsMax","Maximum NToNmaxPts",1.00, 1.00, 0.,1.,1.,Parameter::Double);
}

void StiRootSimpleTrackFilter::setDefaults()
{
  ParameterIterator iter;
  for (iter=begin(); iter!=end(); iter++)
    {
      EditableParameter * par = static_cast<EditableParameter *>(*iter);
      par->reset();
    }
}

bool StiRootSimpleTrackFilter::accept(StiTrack * t) const
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


StiRootSimpleTrackFilterFactory::StiRootSimpleTrackFilterFactory(const string & newName, 
								 int original,
								 int incremental,
								 int maxInc)
  : StiTrackFilterFactory(newName, 
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
