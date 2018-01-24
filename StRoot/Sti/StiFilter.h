#ifndef StiFilter_H
#define StiFilter_H 1
#include <string>
#include <vector>
using std::string;
#include <Stiostream.h>
#include <stdlib.h>

class StiFilter
{
 public: 

	enum FilterMode {Null=0,Bool,ByValue,ByRange,ByExcludedRange};

  /// Instantiation of a null filter.
  /**
   */
  StiFilter():
		name("noname"), used(true), minimum(0.), maximum(1.), mode(Null)
		{}
	
	/// Instantiation of a boolean filter
  StiFilter(bool v, const string& n="noname", bool u=true):
		name(n), used(u), minimum(v), maximum(0.), mode(Bool)
		{}

	/// Instantiation of a filter by value using integer as input
  StiFilter(int v, const string& n="noname", bool u=true):
		name(n), used(u), minimum(v), maximum(0.), mode(ByValue)
		{}

	/// Instantiation of a filter by value using double as input
  StiFilter(double v, const string& n="noname", bool u=true):
		name(n), used(u), minimum(v), maximum(0.), mode(ByValue)
		{}

	/// Instantiation of a filter by range using double as input
  StiFilter(double min, double max, const string& n="noname", bool u=true):
		name(n), used(u), minimum(min), maximum(max), mode(ByRange)
		{}

	/// Instantiation of a filter by range using double as input and with selective normal/reverse logic
  StiFilter(double min, double max, bool reverse, const string& n="noname", bool u=true):
		name(n), used(u), minimum(min), maximum(max), mode(reverse?ByExcludedRange:ByRange)
		{}

	/// Copy Ctr
  StiFilter(const StiFilter &sp):
		name(sp.name),used(sp.used),minimum(sp.minimum), maximum(sp.maximum), mode(sp.mode)
		{}

  virtual ~StiFilter()
		{}

  void setName(const string& n)
		{
			name = n;
		}

  void setUsed(bool u)
		{
			used = u;
		}

  void setValue(bool value)
		{
			minimum = value?1:0;
		}

  void setValue(int value)
		{
			minimum = value;
		}

  void setValue(double value)
		{
			minimum = value;
		}

  void setRange(double min, double max)
		{
			minimum = min;
			maximum = max;
		}

  void set(double min, double max, bool reverse, const string& n, bool u)
		{
			name = n;
			used = u;
			minimum = min;
			maximum = max;
			mode = reverse?ByExcludedRange:ByRange;
		}

  string getName()
		{
			return string(name);
		}

  bool   isUsed()
		{
			return used;
		}

  double getValue()
		{
			return minimum;
		}

  double getMinimum()
		{
			return minimum;
		}

  double getMaximum()
		{
			return maximum;
		}

  friend   ostream& operator<<(ostream& os, const StiFilter &);

  virtual bool accept(bool value);
  virtual bool accept(int value);
  virtual bool accept(double value);

 protected:
  string  name;
  bool    used;
  double  minimum;
  double  maximum;
  int     mode;
};

/// Filter by value with Bool as input.
inline bool StiFilter::accept(bool value)
{
  return value? minimum>0 : minimum==0 ;
}

/// Filter by value with int as input.
inline bool StiFilter::accept(int value)
{
	switch (mode)
		{
		case Null: return false; // null filter always return false
		case Bool: return minimum?value>0:value==0;
		case ByValue: return minimum==value;
		case ByRange: return value>minimum&&value<=maximum;
		case ByExcludedRange: return value<=minimum || value>maximum;
		}
	return false;
}

/// Filter by value, range or excluded range with double as input.
inline bool StiFilter::accept(double value)
{
	switch (mode)
		{
		case Null: return false; // null filter always return false
		case Bool: return minimum?value>0:value==0;
		case ByValue: return minimum==value;
		case ByRange: return value>minimum&&value<=maximum;
		case ByExcludedRange: return value<=minimum || value>maximum;
		}
	return false;
}

typedef vector<StiFilter*> StiFilterVec;
#endif
