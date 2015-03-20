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

#ifndef KFPVEfficiencies_H
#define KFPVEfficiencies_H

#ifndef HLTCA_STANDALONE
#include "TNamed.h"
#endif

#include <map>
#include <iomanip>
#include "KFMCCounter.h"

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

  virtual void AddCounter(TString shortname, TString name){
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

  KFPVEfficiencies& operator+=(KFPVEfficiencies& a){
    mc += a.mc; reco += a.reco;
    ghost += a.ghost; bg += a.bg; clone += a.clone;
    return *this;
  };
  
  void CalcEff(){
    ratio_reco = reco/mc;

    KFMCCounter<int> allReco = reco + ghost + bg;
    ratio_ghost = ghost/allReco;
    ratio_bg  = bg/allReco;
    ratio_clone  = clone/allReco;
  };
  

  void Inc(bool isReco, int nClones, TString name)
  {
    const int index = indices[name];
    
    mc.counters[index]++;
    if (isReco) reco.counters[index]++;
    if(nClones > 0)
      clone.counters[index] += nClones;
  };

  void IncReco(bool isGhost, bool isBg, TString name){
    const int index = indices[name];

    if (isGhost) ghost.counters[index]++;
    if (isBg)    bg.counters[index]++;
  };

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

  void AddFromFile(TString fileName)
  {
    std::fstream file(fileName.Data(),std::fstream::in);
    file >> *this;
  }

//   ClassDef(KFPVEfficiencies,1);

 private:
  std::vector<TString> names; // names counters indexed by index of counter
  std::map<TString, int> indices; // indices of counters indexed by a counter shortname

  KFMCCounter<double> ratio_reco;

  KFMCCounter<int> mc;
  KFMCCounter<int> reco;

  KFMCCounter<double> ratio_ghost;
  KFMCCounter<double> ratio_bg;
  KFMCCounter<double> ratio_clone;

  KFMCCounter<int> ghost;
  KFMCCounter<int> bg; // background
  KFMCCounter<int> clone; // background
};

#endif
