/*
 * This file is part of KFParticle package
 * Copyright (C) 2007-2019 FIAS Frankfurt Institute for Advanced Studies
 *               2007-2019 Goethe University of Frankfurt
 *               2007-2019 Ivan Kisel <I.Kisel@compeng.uni-frankfurt.de>
 *               2007-2019 Maksym Zyzak
 *
 * KFParticle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * KFParticle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef KFMCCounter_H
#define KFMCCounter_H

#include <iostream>
#include <fstream>
#include <vector>

/** @class KFMCCounter
 ** @brief A helper structure to store information on the number of reconstructed and Monte Carlo particles for efficiency calculation.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class is used to calculate reconstruction efficiency and ratios of a given set of particles.
 **/

template <typename T>
struct KFMCCounter
{
  int NCounters; ///< Number of counters in the current object.
  
  std::vector<T> counters; ///< Counters of different set of particles.

  KFMCCounter():NCounters(0),counters(0) { }
  KFMCCounter(int nCounters):NCounters(nCounters), counters(nCounters,T(0)) { } ///< Constructs the object with the set of counters "nCounters".

  void AddCounter(){ NCounters++; counters.push_back(T(0)); } ///< Adds a counter to the existing list.
  void AddCounters(int nCounters){ NCounters += nCounters; counters.resize( NCounters, T(0)); } ///< Adds several counters to the existing list.
  
  /** Operator adds all counters from object "a" to the current object. Returns the current object. */
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
  /** Operator adds all counters from object "a" to the current object, result is stored to the temporary object. Returns the temporary object. */
  KFMCCounter operator+(KFMCCounter& a){
    KFMCCounter res = *this;
    res += a;
    return res;
  };
  /** Operator divides all counters from the current object to the counters from object "a", result is stored to the temporary object. Returns the temporary object. */
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
  /** Operator divides all counters from the current object to the value "a", result is stored to the temporary object. Returns the temporary object. */
  template <typename T2>
  KFMCCounter<T2> operator/(double a){
    KFMCCounter<T2> b(NCounters);
    for (int iC = 0; iC < NCounters; iC++){
      b.counters[iC] = (T2)Div(counters[iC],a);
    }
    return b;
  }
  /** Operator to write the object "a" to the file "strm".*/
  friend std::fstream & operator<<(std::fstream &strm, const KFMCCounter<T> &a ){
    strm << a.NCounters << " " << a.counters.size() << " ";
    for(unsigned int iV=0; iV<a.counters.size(); iV++)
      strm << a.counters[iV] << " ";
    strm << std::endl;
    return strm;
  }
  /** Operator to write the object "a" to the stream "strm".*/
  friend std::ostream & operator<<(std::ostream &strm, const KFMCCounter<T> &a ){
    strm << a.NCounters << " " << a.counters.size() << " ";
    for(unsigned int iV=0; iV<a.counters.size(); iV++)
      strm << a.counters[iV] << " ";
    strm << std::endl;
    return strm;
  }
  /** Operator to read the object "a" from the file "strm".*/
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
    /** Divides value "a" on value "b" if b is not zero, otherwise returns "-1". */
    double Div(double a, double b){return (b > 0) ? a/b : -1.;}
};

#endif
