#include <cassert>
#include "StiDefaultTrackFilter.h"

#include <stdexcept>
#include <string>
using std::string;
#include "Sti/Base/Parameter.h"
#include "Sti/Base/EditableParameter.h"
#include "Sti/StiTrack.h"
#include "StiDefaultTrackFilter.h"

StiDefaultTrackFilter::StiDefaultTrackFilter()
  : EditableFilter<StiTrack>()
{}

StiDefaultTrackFilter::StiDefaultTrackFilter(const string & name, const string & description)
  : EditableFilter<StiTrack>(name,description)
{}


StiDefaultTrackFilter::~StiDefaultTrackFilter()
{}

void StiDefaultTrackFilter::initialize()
{
  cout << "StiDefaultTrackFilter::initialize() - INFO - Starting" << endl;
  parameterVector.clear();
  add(new EditableParameter("Chi2Used", "Use Chi2",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kChi2));
  add(new EditableParameter("Chi2Min",  "Minimum Chi2", 0., 0., 0., 100.,1,Parameter::Double, StiTrack::kChi2));
  add(new EditableParameter("Chi2Max",  "Maximum Chi2", 20., 20., 0., 100.,1,Parameter::Double, StiTrack::kChi2));
  
  add(new EditableParameter("PhiUsed",  "Use Phi",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kPhi));
  add(new EditableParameter("PhiMin",   "Minimum Phi", -3.2,    -3.2, -3.2, 3.2, 2,Parameter::Double, StiTrack::kPhi));
  add(new EditableParameter("PhiMax",   "Maximum Phi",  3.2,     3.2, -3.2, 3.2, 2,Parameter::Double, StiTrack::kPhi));
  
  add(new EditableParameter("PtUsed",   "Use Pt",     true, true, 0,1,1,Parameter::Boolean, StiTrack::kPt));
  add(new EditableParameter("PtMin",    "Minimum Pt", 0.1, 0.1, 0., 100.,2,Parameter::Double, StiTrack::kPt));
  add(new EditableParameter("PtMax",    "Maximum Pt", 10., 10., 0., 100.,2,Parameter::Double, StiTrack::kPt));
  
  add(new EditableParameter("PUsed",    "Use P",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kP));
  add(new EditableParameter("PMin",     "Minimum P", 0., 0., 0., 100.,2,Parameter::Double, StiTrack::kP));
  add(new EditableParameter("PMax",     "Maximum P", 10., 10., 0., 100.,2,Parameter::Double, StiTrack::kP));
  
  add(new EditableParameter("EtaUsed",  "Use Eta",     true, true, 0,1,1,Parameter::Boolean, StiTrack::kPseudoRapidity));
  add(new EditableParameter("EtaMin",   "Minimum Eta", -0.5, -0.5, -10., 10.,2,Parameter::Double, StiTrack::kPseudoRapidity));
  add(new EditableParameter("EtaMax",   "Maximum Eta",  0.5,  0.5, -10., 10.,2,Parameter::Double, StiTrack::kPseudoRapidity));
  
  add(new EditableParameter("nPtsUsed", "Use nPts",     true, true, 0,1,1,Parameter::Boolean, StiTrack::kPointCount));
  add(new EditableParameter("nPtsMin",  "Minimum nPts", 10., 10., 0., 100.,1,Parameter::Integer, StiTrack::kPointCount));
  add(new EditableParameter("nPtsMax",  "Maximum nPts", 60., 60., 0., 100.,1,Parameter::Integer, StiTrack::kPointCount));

  add(new EditableParameter("nGapsUsed","Use nGaps",     true, true, 0,1,1,Parameter::Boolean, StiTrack::kGapCount));
  add(new EditableParameter("nGapsMin", "Minimum nGaps", 0., 0., 0., 100.,1,Parameter::Integer, StiTrack::kGapCount));
  add(new EditableParameter("nGapsMax", "Maximum nGaps", 3., 3., 0., 100.,1,Parameter::Integer, StiTrack::kGapCount));

  add(new EditableParameter("chargeUsed","Use Charge",     false, false, 0,1,1,Parameter::Boolean, StiTrack::kCharge));
  add(new EditableParameter("chargeMin", "Minimum Charge", -1., -1., -100.,   100.,1,Parameter::Integer, StiTrack::kCharge));
  add(new EditableParameter("chargeMax", "Maximum Charge",  1.,  1., -100.,   100.,1,Parameter::Integer, StiTrack::kCharge));
}


bool StiDefaultTrackFilter::accept(const StiTrack * t) const
{
  //cout << "StiDefaultTrackFilter::accept(t) - INFO - Starting" << endl;
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
	    if (parUse->getBoolValue())
		    {
		    //cout << "/"<<count++;
		    v = t->getValue(parUse->getKey());
		    low = parLow->getDoubleValue();
		    high = parHi->getDoubleValue();
		    if (v<low || v>high)
			    {
			    //cout<<"/false-done"<<endl;
			    return false;
			    }
		    }
	    }
      else
        assert(0 && "StiDefaultTrackFilter::accept Internal error");
    }
  //cout<<"/true-done"<<endl;
  return true;
}


void StiDefaultTrackFilter::setDefaults()
{}

