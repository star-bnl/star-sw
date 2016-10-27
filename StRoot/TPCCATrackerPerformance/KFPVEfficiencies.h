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

#include "TNamed.h"
#include "Counters.h"

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

    TTracksCatCounters<int> allReco = reco + ghost + bg;
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
    std::cout.setf(ios::fixed);
    std::cout.setf(ios::showpoint);
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
             << "  : " << setw(6) << ratio_reco.counters[iC]              
             << "  / " << setw(6) << ratio_ghost.counters[iC]  // particles w\o MCParticle
             << "  / " << setw(6) << ratio_bg.counters[iC]     // particles with incorrect MCParticle
             << "  / " << setw(6) << ratio_clone.counters[iC]     // particles with incorrect MCParticle
             << "  / " << setw(6) << ghost.counters[iC]
             << "  / " << setw(7) << bg.counters[iC]
             << "  / " << setw(6) << reco.counters[iC]
             << "  / " << setw(7) << clone.counters[iC]
             << "  | " << setw(6) << mc.counters[iC]  << std::endl;
    }
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
    std::fstream file(fileName.Data(),fstream::in);
    file >> *this;
  }

//   ClassDef(KFPVEfficiencies,1);

 private:
  vector<TString> names; // names counters indexed by index of counter
  map<TString, int> indices; // indices of counters indexed by a counter shortname

  TTracksCatCounters<double> ratio_reco;

  TTracksCatCounters<int> mc;
  TTracksCatCounters<int> reco;

  TTracksCatCounters<double> ratio_ghost;
  TTracksCatCounters<double> ratio_bg;
  TTracksCatCounters<double> ratio_clone;

  TTracksCatCounters<int> ghost;
  TTracksCatCounters<int> bg; // background
  TTracksCatCounters<int> clone; // background
};

#endif
