//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#ifndef KFMCCounter_H
#define KFMCCounter_H

#include <iostream>
#include <fstream>
#include <vector>

template <typename T>
struct KFMCCounter // counters for different tracks categories
{
  int NCounters;
  
  std::vector<T> counters;

  KFMCCounter():NCounters(0),counters(0) { };
  KFMCCounter(int nCounters):NCounters(nCounters), counters(nCounters,T(0)) { };

  void AddCounter(){ NCounters++; counters.push_back(T(0)); };
  void AddCounters(int nCounters){ NCounters += nCounters; counters.resize( NCounters, T(0)); };
  
  KFMCCounter& operator+=(KFMCCounter& a){
    if (NCounters != a.NCounters){
      std::cout << " KFMCCounter: Error. Addition of counters of different sizes: " << NCounters << " " << a.NCounters << std::endl;
    }
    else{
      for (int iC = 0; iC < NCounters; iC++){
        counters[iC] += a.counters[iC];
      }
    }
    return *this;
  };

  KFMCCounter operator+(KFMCCounter& a){
    KFMCCounter res = *this;
    res += a;
    return res;
  };

  template <typename T2>
  KFMCCounter<double> operator/(KFMCCounter<T2>& a){
    KFMCCounter<double> b(NCounters);
    if (NCounters != a.NCounters){
      std::cout << " KFMCCounter: Error. Addition of counters of different sizes: " << NCounters << " " << a.NCounters << std::endl;
    }
    else{
      for (int iC = 0; iC < NCounters; iC++){
        b.counters[iC] = Div(counters[iC],a.counters[iC]);
      }
    }
    return b;
  }

  template <typename T2>
  KFMCCounter<T2> operator/(double a){
    KFMCCounter<T2> b(NCounters);
    for (int iC = 0; iC < NCounters; iC++){
      b.counters[iC] = (T2)Div(counters[iC],a);
    }
    return b;
  }

  friend std::fstream & operator<<(std::fstream &strm, const KFMCCounter<T> &a ){
    strm << a.NCounters << " " << a.counters.size() << " ";
    for(unsigned int iV=0; iV<a.counters.size(); iV++)
      strm << a.counters[iV] << " ";
    strm << std::endl;
    return strm;
  }

  friend std::ostream & operator<<(std::ostream &strm, const KFMCCounter<T> &a ){
    strm << a.NCounters << " " << a.counters.size() << " ";
    for(unsigned int iV=0; iV<a.counters.size(); iV++)
      strm << a.counters[iV] << " ";
    strm << std::endl;
    return strm;
  }

  friend std::fstream & operator>>(std::fstream &strm, KFMCCounter<T> &a ){
    int tmp;
    strm >> tmp;
    a.NCounters = tmp;
    strm >> tmp;
    a.counters.resize(tmp,T(0));
    for(int iV=0; iV<tmp; iV++)
    {
      T tmp1;
      strm >> tmp1;
      a.counters[iV] = tmp1;
    }
    return strm;
  }

  private:
    double Div(double a, double b){return (b > 0) ? a/b : -1.;};
};

#endif
