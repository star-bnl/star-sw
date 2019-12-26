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

#ifndef KFPVEfficiencies_H
#define KFPVEfficiencies_H

#ifndef HLTCA_STANDALONE
#include "TNamed.h"
#endif

#include <map>
#include <iomanip>
#include "KFMCCounter.h"

/** @class KFPVEfficiencies
 ** @brief Class to calculate efficiency of KF Particle Finder.
 ** @author  M.Zyzak, I.Kisel
 ** @date 05.02.2019
 ** @version 1.0
 **
 ** The class calculates reconstruction efficiency of the primary vertices.\n
 ** Definitions:\n
 ** background - physics background, when daughter particle come from the secondary vertex;\n
 ** ghost - combinatorial background, tracks do not form a real vertex;\n
 ** clone - a vertex is reconstructed several times, for example, half of tracks form one group and
 ** another half form second group.
 **/

class KFPVEfficiencies: public TNamed
{
 public:

  KFPVEfficiencies():
    names(),
    indices(),
    ratio_reco(),
    mc(),
    reco(),
    ratio_ghost(),
    ratio_bg(),
    ratio_clone(),
    ghost(),
    bg(),
    clone()
  {
    AddCounter(Form("%s","PV"), Form("%-*s",12,"PV"));
    AddCounter(Form("%s","PVtrigger"), Form("%-*s",12,"PV trigger"));
    AddCounter(Form("%s","PVpileup"), Form("%-*s",12,"PV pileup "));
  }

  virtual ~KFPVEfficiencies(){};

  virtual void AddCounter(TString shortname, TString name)
  {
    /** Adds a counter with the name defined by "name" to all counter
     ** objects. For easiness of operation with counters, a shortname is assigned
     ** to each of them and the corresponding entry in the map indices is done.
     ** \param[in] shortname - a short name of the counter for fast and easy access to its index
     ** \param[in] name - name of the counter which is added to each counter object.
     **/
    indices[shortname] = names.size();
    names.push_back(name);

    ratio_reco.AddCounter();
    mc.AddCounter();
    reco.AddCounter();

    ratio_ghost.AddCounter();
    ratio_bg.AddCounter();
    ratio_clone.AddCounter();
    ghost.AddCounter();
    bg.AddCounter();
    clone.AddCounter();
  };

  /** \brief Operator to add efficiency table from object "a" to the current object. Returns the current object after addition. */
  KFPVEfficiencies& operator+=(KFPVEfficiencies& a){
    mc += a.mc; reco += a.reco;
    ghost += a.ghost; bg += a.bg; clone += a.clone;
    return *this;
  };
  
  /** \brief Function to calculate efficiency after all counters are set. If the counters are modified the function should be called again. */
  void CalcEff(){
    ratio_reco = reco/mc;

    KFMCCounter<int> allReco = reco + ghost + bg;
    ratio_ghost = ghost/allReco;
    ratio_bg  = bg/allReco;
    ratio_clone  = clone/allReco;
  };
  

  void Inc(bool isReco, int nClones, TString name)
  {
    /** Increases counters by one, if the corresponding boolean variable is "true".
     ** MC counter is increased in any case.
     ** \param[in] isReco - "true" if vertex is reconstructed
     ** \param[in] nClones - number of double reconstructed vertices for the given MC vertex,
     ** will be added to the "clone" counters
     ** \param[in] name  - "shortname" of the set of counters, which should be increased
     **/
    const int index = indices[name];
    
    mc.counters[index]++;
    if (isReco) reco.counters[index]++;
    if(nClones > 0)
      clone.counters[index] += nClones;
  };

  void IncReco(bool isGhost, bool isBg, TString name)
  {
    /** Increases counters by one, if the corresponding boolean variable is "true".
     ** \param[in] isGhost - "true" if ghost is added
     ** \param[in] isBg - "true" if physics background is added
     ** \param[in] name  - "shortname" of the set of counters, which should be increased
     **/
    const int index = indices[name];

    if (isGhost) ghost.counters[index]++;
    if (isBg)    bg.counters[index]++;
  };

  /** \brief Prints the efficiency table on the screen. */
  void PrintEff(){
    std::ios_base::fmtflags original_flags = std::cout.flags();
    std::cout.setf(std::ios::fixed);
    std::cout.setf(std::ios::showpoint);
    std::cout.precision(3);
    std::cout << "              : "
         << "   Eff "
         <<" / "<< " Ghost "
         <<" / "<< "BackGr "
         <<" / "<< "Clone  "
         <<" / "<< "N Ghost"
         <<" / "<< "N BackGr"
         <<" / "<< "N Reco "
         <<" / "<< "N Clone "
         <<" | "<< "  N MC "  << std::endl;
    
    int NCounters = mc.NCounters;
    for (int iC = 0; iC < NCounters; iC++){
        std::cout << names[iC]
             << "  : " << std::setw(6) << ratio_reco.counters[iC]              
             << "  / " << std::setw(6) << ratio_ghost.counters[iC]  // particles w\o MCParticle
             << "  / " << std::setw(6) << ratio_bg.counters[iC]     // particles with incorrect MCParticle
             << "  / " << std::setw(6) << ratio_clone.counters[iC]     // particles with incorrect MCParticle
             << "  / " << std::setw(6) << ghost.counters[iC]
             << "  / " << std::setw(7) << bg.counters[iC]
             << "  / " << std::setw(6) << reco.counters[iC]
             << "  / " << std::setw(7) << clone.counters[iC]
             << "  | " << std::setw(6) << mc.counters[iC]  << std::endl;
    }
    std::cout.flags(original_flags); 
  };
  
  /** \brief Operator to write efficiencies to file. */
  friend std::fstream & operator<<(std::fstream &strm, KFPVEfficiencies &a) {

    strm << a.ratio_reco;
    strm << a.mc;
    strm << a.reco;
    strm << a.ratio_ghost;
    strm << a.ratio_bg;
    strm << a.ratio_clone;
    strm << a.ghost;
    strm << a.bg;
    strm << a.clone;

    return strm;
  }
  /** \brief Operator to read efficiencies from file. */
  friend std::fstream & operator>>(std::fstream &strm, KFPVEfficiencies &a){

    strm >> a.ratio_reco;
    strm >> a.mc;
    strm >> a.reco;
    strm >> a.ratio_ghost;
    strm >> a.ratio_bg;
    strm >> a.ratio_clone;
    strm >> a.ghost;
    strm >> a.bg;
    strm >> a.clone;

    return strm;
  }
  /** \brief Adds efficiency from the file with the name defined by "fileName" to the current objects. */
  void AddFromFile(TString fileName)
  {
    std::fstream file(fileName.Data(),std::fstream::in);
    file >> *this;
  }

 private:
  std::vector<TString> names;      ///< Names of the counters. The same for all counters objects.
  std::map<TString, int> indices;  ///< Map between the counter index and its short name.

  KFMCCounter<double> ratio_reco;  ///< Efficiency.

  KFMCCounter<int> mc;             ///< Counters of the Monte Carlo vertices.
  KFMCCounter<int> reco;           ///< Counters of the reconstructed vertices.

  KFMCCounter<double> ratio_ghost; ///< Ratio of the ghost candidates to the total number of candidates.
  KFMCCounter<double> ratio_bg;    ///< Ratio of the physics background candidates to the total number of candidates.
  KFMCCounter<double> ratio_clone; ///< Ratio of double reconstructed vertices to the total number of signal candidates.

  KFMCCounter<int> ghost;          ///< Counters of the ghost candidates.
  KFMCCounter<int> bg;             ///< Counters of the physics background candidates.
  KFMCCounter<int> clone;          ///< Counters of the double reconstructed vertices.
};

#endif
