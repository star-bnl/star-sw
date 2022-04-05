#include <map>
#include <stdexcept>
#include <string>
#include <math.h>
using namespace std;
template<typename Number>
class LinearFit
{
public:
  typedef std::map<Number, Number> Points;
  LinearFit();
  LinearFit(Points * points);
  LinearFit(const LinearFit & fit);
  virtual ~LinearFit();
  const LinearFit & operator=(const LinearFit & fit); 
  virtual void fit();
  virtual void fit(Points * points);
  const Number getSlope() const;
  const Number getIntercept() const;
  const Number getRegressionCoefficient() const; 
protected:
  Points * _points;
  ///Slope
  Number _a;
  ///Intercept
  Number _b;
  ///Regression
  Number _r;
};

template<typename Number>
LinearFit<Number>::LinearFit()
: _points(0), _a(0), _b(0), _r(0)
{}

template<typename Number>
LinearFit<Number>::LinearFit(Points * points)
: _points(points), _a(0), _b(0), _r(0)
{}

template<typename Number>
LinearFit<Number>::LinearFit(const LinearFit<Number> & fit)
: _points(fit._points), _a(fit._a), _b(fit._b), _r(fit._r)
{}

template<typename Number>
const LinearFit<Number> & LinearFit<Number>::operator=(const LinearFit<Number> & fit)
{
  _points = fit._points;
  _a = fit._a;
  _b = fit._b;
  _r = fit._r;
  
  return *this;
}

///Perform linear fit on given points.
///Store reference to given points and
///call "fit()" to actually perform the fit.
template<typename Number>
void LinearFit<Number>::fit(Points * points)
{
  _points = points;
  _a = _b = _r = 0;
  fit();
}

template<typename Number>
void LinearFit<Number>::fit()
{
  int n = _points->size();
  if (n < 2)
    throw runtime_error("LinearFit<Number>::fit() - FATAL - n<2");
  Number sumx=0,sumy=0,sumx2=0,sumy2=0,sumxy=0;
  Number x, y,sxx,syy,sxy;
  // Compute error matrix
  typename std::map<Number, Number>::const_iterator i;
  for (i = _points->begin(); i != _points->end(); i++)
    {
      x = i->first;
      y = i->second;
      sumx += x;
      sumy += y;
      sumx2 += (x*x);
      sumy2 += (y*y);
      sumxy += (x*y);
    }
  sxx = sumx2 - (sumx * sumx / n);
  syy = sumy2 - (sumy * sumy / n);
  sxy = sumxy - (sumx * sumy / n);
  // Infinite slope_, non existant yIntercept
  if (fabs(sxx) == 0)
    {
      _a = -99999.;
      _b = -99999.;
      _r = -99999.;
      return;
    }
  // Calculate the slope_ and yIntercept
  _a = sxy/sxx;
  _b = sumy/n - _a*sumx/n;
  // Compute the regression coefficient
  if (fabs(syy) == 0)
    _r = 1;
  else
    _r = sxy/::sqrt(sxx*syy);
}

template<typename Number>
LinearFit<Number>::~LinearFit()
{}

template<typename Number>
inline const Number LinearFit<Number>::getSlope() const
{
  return _a;
}

template<typename Number>
inline const Number LinearFit<Number>::getIntercept() const
{
  return _b;
}

template<typename Number>
inline const Number LinearFit<Number>::getRegressionCoefficient() const
{
  return _r;
}

