#include "LinearFit.h"

template<typename Number>
class PowerLawFit : public LinearFit<Number>
{
 public:
  typedef map<Number, Number> Points;
  
  PowerLawFit();
  PowerLawFit(Points * points);
  PowerLawFit(const PowerLawFit & fit);
  virtual ~PowerLawFit();
  const PowerLawFit & operator=(const PowerLawFit & fit); 
  virtual void fit();
  virtual void fit(Points * points);
  const Number getExponent() const;
  const Number getCoefficient() const;

 protected:
  /// Points provided on input
  Points * _data;
  /// Transient (for internal use) array used to hold the log 0f _data
  Points _logData;
  /// Exponent of the power law
  Number _exponent;
  /// Multiplicative Coefficient of the power law
  Number _coeff;
  
};

template<typename Number>
PowerLawFit<Number>::PowerLawFit()
  : LinearFit<Number>(),_data(0),_exponent(0),_coeff(0)
{}

template<typename Number>
PowerLawFit<Number>::PowerLawFit(Points * points)
  : LinearFit<Number>(),_data(points),_exponent(0),_coeff(0)
{
}

template<typename Number>
PowerLawFit<Number>::PowerLawFit(const PowerLawFit & fit)
  : LinearFit<Number>(fit), _data(fit._data),_exponent(fit._exponent),_coeff(fit._coeff)
{}

template<typename Number>
PowerLawFit<Number>::~PowerLawFit()
{}

template<typename Number>
const PowerLawFit<Number> & PowerLawFit<Number>::operator=(const PowerLawFit<Number> & fit)
{
  LinearFit<Number>::operator=(fit);
  _data = fit._data;
  _exponent = fit._exponent;
  _coeff = fit._coeff;
  return *this;
}

template<typename Number>
void PowerLawFit<Number>::fit()
{
  typename Points::const_iterator i;
  _logData.clear();
  double x,y;
  for (i=_data->begin();i!=_data->end();i++)
    {
      x = ::log(i->first);
      y = ::log(i->second);
      _logData[x]=y;
    }
  this->_points = &_logData;
  LinearFit<Number>::fit();
  _coeff = exp(this->_b);
  _exponent = this->_a;
}

template<typename Number>
void PowerLawFit<Number>::fit(Points * points)
{
  _data = points;
  fit();
}

template<typename Number>
const Number PowerLawFit<Number>::getExponent() const
{
  return _exponent;
}

template<typename Number>
const Number PowerLawFit<Number>::getCoefficient() const
{
  return _coeff;
}
