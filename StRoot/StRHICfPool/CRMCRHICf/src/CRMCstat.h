#ifndef _Stat_h_
#define _Stat_h_
#include <iostream>
#include <cmath>

// --------------  test observables
template <typename T>
struct Stat {
  T   _sum  = 0;
  T   _sum2 = 0;
  int _cnt  = 0;
  void fill(T x)
  {
    _sum  += x;
    _sum2 += x*x;
    _cnt++;
  }
  double mean() const { return double(_sum) / _cnt; }
  double var() const { return double(_sum2) / _cnt - mean() * mean(); }
  double std() const { return std::sqrt(var()); }
  double sem() const { return std::sqrt(var()/_cnt); }
};

//--------------------------------------------------------------------
template <typename T>
std::ostream& operator<<(std::ostream& o, const Stat<T>& s)
{
  return o << s.mean() << " +/- " << s.sem();
}

#endif
// Local Variables:
//   mode: C++
// End:
