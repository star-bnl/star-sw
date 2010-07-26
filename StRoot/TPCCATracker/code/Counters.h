/**
*       Counters for efficiency calculation
**/


#ifndef Counters_H
#define Counters_H

#include <iostream>
using std::cout;
using std::endl;
using std::ios;

#include <string>
using std::string;
         
#include <vector>
using std::vector;
         
#include <map>
using std::map;

template <typename T>
struct TTracksCatCounters // counters for different tracks categories
{
  int NCounters;
  
  vector<T> counters;

  TTracksCatCounters():NCounters(0){ counters.clear(); };
  TTracksCatCounters(int nCounters):NCounters(nCounters){ counters.resize( NCounters, T(0)); };

  void AddCounter(){ NCounters++; counters.push_back(T(0)); };
  void AddCounters(int nCounters){ NCounters += nCounters; counters.resize( NCounters, T(0)); };
  
  TTracksCatCounters& operator+=(TTracksCatCounters& a){
    if (NCounters != a.NCounters){
      cout << " TTracksCatCounters: Error. Addition of counters of different sizes: " << NCounters << " " << a.NCounters << endl;
    }
    else{
      for (int iC = 0; iC < NCounters; iC++){
        counters[iC] += a.counters[iC];
      }
    }
    return *this;
  };

  template <typename T2>
  TTracksCatCounters<double> operator/(TTracksCatCounters<T2>& a){
    TTracksCatCounters<double> b(NCounters);
    if (NCounters != a.NCounters){
      cout << " TTracksCatCounters: Error. Addition of counters of different sizes: " << NCounters << " " << a.NCounters << endl;
    }
    else{
      for (int iC = 0; iC < NCounters; iC++){
        b.counters[iC] = Div(counters[iC],a.counters[iC]);
      }
    }
    return b;
  }

  template <typename T2>
  TTracksCatCounters<T2> operator/(double a){
    TTracksCatCounters<T2> b(NCounters);
    for (int iC = 0; iC < NCounters; iC++){
      b.counters[iC] = (T2)Div(counters[iC],a);
    }
    return b;
  }

  private:
    double Div(double a, double b){return (b > 0) ? a/b : -1.;};
};

struct TEfficiencies
{
  TEfficiencies():ratio_ghosts(0),ratio_clones(0),ghosts(0),clones(0){
    // you should add counter with shortname="total" !!
  };

  virtual void AddCounter(string shortname, string name);
  
  TEfficiencies& operator+=(TEfficiencies& a);
  void CalcEff();
  void Inc(bool isReco, string name); // increment counters according to parameters

  void Print();

  
  vector<string> names; // names counters indexed by index of counter
  map<string, int> indices; // indices of counters indexed by a counter shortname
  
  TTracksCatCounters<double> ratio_reco;
  double ratio_ghosts;
  double ratio_clones;
  
  TTracksCatCounters<int> mc;
  TTracksCatCounters<int> reco;
  int ghosts;
  int clones;
};

inline void TEfficiencies::AddCounter(string shortname, string name)
{
  indices[shortname] = names.size();
  names.push_back(name);
    
  ratio_reco.AddCounter();
  mc.AddCounter();
  reco.AddCounter();
}

inline void TEfficiencies::CalcEff()
{
  ratio_reco = reco/mc;
  if (mc.counters[0] > 0){
    ratio_clones = clones/double(mc.counters[indices["total"]]);
  }
  else{
    ratio_clones = -1;
  }
  if (reco.counters[0] > 0){
    ratio_ghosts = ghosts/double(reco.counters[indices["total"]]);
  }
  else{
    ratio_ghosts = -1;
  }
}

inline TEfficiencies& TEfficiencies::operator+=(TEfficiencies& a)
{
  mc += a.mc; reco += a.reco;
  ghosts += a.ghosts; clones += a.clones;

  return *this;
}

inline void TEfficiencies::Inc(bool isReco, string name)
{
  const int index = indices[name];
    
  mc.counters[index]++;
  if (isReco) reco.counters[index]++;
}

inline void TEfficiencies::Print(){
  cout.setf(ios::fixed);
  cout.setf(ios::showpoint);
  cout.precision(3);
  cout << "Track category         : " << " Eff "        <<" | "<< "All MC"  << endl;

  int NCounters = mc.NCounters;
  for (int iC = 0; iC < NCounters; iC++){
    cout << names[iC]  << "   : "
        << ratio_reco.counters[iC]
        << "  | " << mc.counters[iC]  << endl;
  }

  cout << "Clone     probability  : " << ratio_clones <<" | "<< clones << endl;
  cout << "Ghost     probability  : " << ratio_ghosts <<" | "<< ghosts << endl;
}

#endif
