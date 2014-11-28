#include "GainVoltPmtParameters.h"
#include <math.h>

int GainVoltPmtParameters::ioMode=2;

/// The following values were obtained from an average of 2000 STAR EMC PMTs.
/// To change these values, use the "void setDefaults(double, double)" method.
double GainVoltPmtParameters::_defaultA = 2.56e-29;
double GainVoltPmtParameters::_defaultB = 10.71;

GainVoltPmtParameters::GainVoltPmtParameters()
{}

GainVoltPmtParameters::GainVoltPmtParameters(double refVoltage, double gains[5])
{
  set(refVoltage,gains);
}

GainVoltPmtParameters::GainVoltPmtParameters(int n, double volts[], double gains[])
{
  set(n,volts,gains);
}

GainVoltPmtParameters::GainVoltPmtParameters(const GainVoltPmtParameters& p)
{}

GainVoltPmtParameters::~GainVoltPmtParameters()
{}

GainVoltPmtParameters & GainVoltPmtParameters::operator=(const GainVoltPmtParameters&p)
{
  _id = p._id;
  _a  = p._a;
  _b  = p._b;
  _data = p._data;
  return *this;
}

/// Set the voltages and gains
///\param refVolt reference voltage
///\param gains array of five gain values obtained with voltages ref-100,ref-50,ref,ref+50,ref+100
void GainVoltPmtParameters::set(double refVolt, double gains[5])
{
  if (refVolt<=100)
    throw runtime_error("GainVoltPmtParameters::set(double,int,double[]) - F - invalid reference voltage");
  _data[refVolt-100] = gains[0];
  _data[refVolt- 50] = gains[1];
  _data[refVolt    ] = gains[2];
  _data[refVolt- 50] = gains[3];
  _data[refVolt+100] = gains[4];
}

/// Set the voltages and gains
///\param n number of data points
///\param volts[] voltage settings
///\param gains[] relative gains obtained with the given voltage setttings
void GainVoltPmtParameters::set(int n, double volts[], double gains[])
{
  if (n<=0)
    throw runtime_error("GainVoltPmtParameters::set() - F - invalid number of points");
  for (int i=0;i<n;i++)
  {
    //_data.push_back(pair<double,double>(volts[i],gains[i]));
    _data[volts[i]] = gains[i];
  }
}

/// Determine whether the input data are valid.
bool GainVoltPmtParameters::isValid() const
{
  
  map<double,double>::const_iterator i;
  for (i=_data.begin();i!=_data.end();i++)
    {
      if (i->first<200 || i->second<1)
	return false;
    }
  return true;
}

void GainVoltPmtParameters::fit()
{
  //cout << "GainVoltPmtParameters::fit() - INFO - N points:"<< _data.size()<<endl; 
  if (isValid())
    {
      _fit.fit(&_data);
      _a = _fit.getCoefficient();
      _b = _fit.getExponent();
    }
  else
    {
      cout <<"GainVoltPmtParameters::fit() - Default Parameters used for " << *this<<endl;
      _a = _defaultA;
      _b = _defaultB;
    }
}

ostream& operator<<(ostream& os, GainVoltPmtParameters& object)
{
  os << object._id << "\t";
  if (object.ioMode&1) os << object._a <<"\t" << object._b<< "\t";
  if (object.ioMode&2)
  {
     map<double,double>::const_iterator i;
     if (object.ioMode&4)
	   {
	    os << object.getNPoints()<<"\t";
	    for (i=object._data.begin();i!=object._data.end();i++) os << i->first << "\t" << i->second<<"\t";
	   }
     else
	   {
	     i=object._data.begin(); i++;i++;
	     os <<  i->first <<"\t";
	     for (i=object._data.begin();i!=object._data.end();i++) os << i->second << "\t";
	   }
  }
  os << endl;
  return os;
}

istream& operator>>(istream& is, GainVoltPmtParameters& object)
{
  double x,y;
  is >> object._id;
  if (object.ioMode&1)
    {
      is >> x >> y;
      object._a = x;
      object._b = y;
    }
  if (object.ioMode&2)
    {
      int n;
      if (object.ioMode&4)
	{
	  is >> n;
	  for (int i=0;i<n;i++)
	    {
	      is >> x >> y;
	      object._data[x] = y;
	    }
	}
      else
	{
	  double refVolt,gain;
	  is >> refVolt; 
	  is >> gain; object._data[refVolt-100] = gain; 
	  is >> gain; object._data[refVolt- 50] = gain; 
	  is >> gain; object._data[refVolt    ] = gain; 
	  is >> gain; object._data[refVolt+ 50] = gain; 
	  is >> gain; object._data[refVolt+100] = gain; 
	}
    }
  else
    cout << "no data input "<<endl;
  return is;
}

///Get multiplicative constant
double GainVoltPmtParameters::getMultConstant() const
{
  return _a;
}

///Get exponent of the power law
double GainVoltPmtParameters::getExponent() const
{
  return _b;
}

///Get the gain obtained if the given voltage is applied
double GainVoltPmtParameters::getGain(double voltage) const
{
  if (voltage<=0)
    throw runtime_error("GainVoltPmtParameters::getGain(double) - F - given voltage<0");
  return exp(_a+_b*::log(voltage));
}

///Get the voltage need to obtain the given relative gain
double GainVoltPmtParameters::getVoltage(double gain) const
{
  if (gain<=0) return 0;
	return exp(::log(gain)/_b-_a);
}

//Get the ChiSquare of the fit 
//double GainVoltPmtParameters::getChiSquare() const
//{
//  return _chisq;
//}

///Get the number of data points available and used for this PMT
int GainVoltPmtParameters::getNPoints() const
{
  return _data.size();
}

///Print the information of this PMT
void GainVoltPmtParameters::print()
{
  cout << "N Points:" << getNPoints() << endl
    << "Voltage(V)  Gain(Relative)"<<endl
    << "============================="<<endl;
  map<double,double>::const_iterator i;
  for (i=_data.begin();i!=_data.end();i++)
  {
    cout << i->first << "\t" << i->second<<endl;
  }
  cout <<"============================="<<endl
       << "    a : " << _a<< endl
       << "    b : " << _b<< endl;
       
}

PmtIdentifier & GainVoltPmtParameters::getPmtIdentifier()
{
  return _id;
}

void GainVoltPmtParameters::setDefaults(double multCoefficient, double exponent)
{
  _defaultA = multCoefficient;
  _defaultB = exponent;
}
