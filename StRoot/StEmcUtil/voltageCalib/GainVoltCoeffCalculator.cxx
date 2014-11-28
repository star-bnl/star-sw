///GainVoltCoeffCalculator.cxx
///Calculator used parametrize PMT gains vs HV
///with a power law.
///The calculation is done by the process method
/// by iterating over each PMT. The GainVoltPmtParameters
/// class is used to hold PMT data and for the actual power 
/// law fit. 
///
#include "GainVoltCoeffCalculator.h"
#include "GainVoltPmtParameters.h"
using namespace std;
//ClassImp(GainVoltCoeffCalculator)

GainVoltCoeffCalculator::GainVoltCoeffCalculator()
{}

GainVoltCoeffCalculator::GainVoltCoeffCalculator(const GainVoltCoeffCalculator&calculator)
  : _pmts(calculator._pmts)
{}

GainVoltCoeffCalculator::~GainVoltCoeffCalculator()
{}

GainVoltCoeffCalculator & GainVoltCoeffCalculator::operator=(const GainVoltCoeffCalculator&calculator)
{
  _pmts = calculator._pmts;
  return *this;
}

void GainVoltCoeffCalculator::setIoMode(int mode)
{
  GainVoltPmtParameters::ioMode = mode;
}

vector<GainVoltPmtParameters*>::iterator GainVoltCoeffCalculator::begin()
{
  return _pmts.begin();
}
   
vector<GainVoltPmtParameters*>::iterator GainVoltCoeffCalculator::end()
{
  return _pmts.end();
}
    
vector<GainVoltPmtParameters*>::const_iterator GainVoltCoeffCalculator::begin() const
{
  return _pmts.begin();
}

vector<GainVoltPmtParameters*>::const_iterator GainVoltCoeffCalculator::end() const
{
  return _pmts.end();
}


void GainVoltCoeffCalculator::process()
{
  setIoMode(3);
  try 
    {
      // loop over all PMTs, and perform a fit.
      GVP_iterator i;
      //VPunused int count=0;
      for (i=_pmts.begin();i!=_pmts.end();i++)
	{
	  (*i)->fit();
	  //cout << count++ << "\t"<< *(*i);
	}
    }
  catch (runtime_error & err)
    {
      cout << "GainVoltCoeffCalculator::process() - RunTimeError -"<< err.what() << endl;
    }
}

///Read the given object from the given stream
istream& operator>>(istream& is, GainVoltCoeffCalculator& object)
{
  int n;
  is >> n;
  if (n<=0)
    {
      cout << "GainVoltCoeffCalculator::operator>>() - FATAL - nPmt<=0";
      return is;
    }
  GainVoltPmtParameters * pmt;
  //Loop n times
  //For each, instantiate, read, and add to container
  for (int i=0;i<n;i++)
    {
      pmt = new GainVoltPmtParameters();
      is >> *pmt;
      object._pmts.push_back(pmt);
      //cout << *pmt;
    }
  return is;
}

ostream& operator<<(ostream& os, GainVoltCoeffCalculator& object)
{
  vector<GainVoltPmtParameters*>::iterator i;
  for (i=object._pmts.begin();i!=object._pmts.end();i++)
    os << *(*i);
  return os;
}

