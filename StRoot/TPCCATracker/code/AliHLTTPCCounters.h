/**
 *       Alice HTL TPC counters for efficiency calculation
 **/


#ifndef AliHLTTPCCounters_H
#define AliHLTTPCCounters_H

#include <Counters.h>
#include <AliHLTTPCCAParameters.h>

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

struct AliHLTTPCEfficiencies: public TEfficiencies
{
  AliHLTTPCEfficiencies():TEfficiencies(){

    AddCounter("total"          ,"Allset    efficiency");
    AddCounter("ref"           ,"Refset    efficiency");
    AddCounter("extra"           ,"Extra     efficiency");
    AddCounter("rest"           ,"Rest      efficiency");
//     AddCounter("ref_prim"      ,"RefPrim   efficiency");
//     AddCounter("ref_sec"       ,"RefSec    efficiency");
//     AddCounter("extra_prim"      ,"ExtraPrim efficiency");
//     AddCounter("extra_sec"       ,"ExtraSec  efficiency");

  }

  virtual void AddCounter(string shortname, string name){
    TEfficiencies::AddCounter(shortname, name);
    ratio_clone.AddCounter();
    clone.AddCounter();
  };
  
  AliHLTTPCEfficiencies& operator+=(AliHLTTPCEfficiencies& a){
    TEfficiencies::operator+=(a);
    clone += a.clone;

    return *this;
  };
  
  void CalcEff(){
    TEfficiencies::CalcEff();
    ratio_clone = clone/mc;
  };
  
  void Inc(bool isReco, int _nclones, string name){
    TEfficiencies::Inc(isReco, name);
    clone.counters[indices[name]] += _nclones;
  };

  void Print(){
    cout.setf(ios::fixed);
    cout.setf(ios::showpoint);
    cout.precision(3);
    cout << "Track category         : " << " Eff  "       <<" / "<< "Clones" <<" | "<< "All MC"  << endl;
    
    int NCounters = mc.NCounters;
    for (int iC = 0; iC < NCounters; iC++){
      cout << names[iC]  << "   : "
          << ratio_reco.counters[iC]
          << "  / " << ratio_clone.counters[iC]
          << "  | " << mc.counters[iC]  << endl;
    }
    cout << "Ghost     probability  : " << ratio_ghosts <<" | "<< ghosts << endl;
  };

  TTracksCatCounters<double> ratio_clone;
  TTracksCatCounters<int> clone;
};

  /// Information about reconstruction of MCTrack
class AliHLTTPCCAPerformanceMCTrackData{
  public:
    AliHLTTPCCAPerformanceMCTrackData(){
      set = 0;
      isReconstructable = 0;
      nReconstructed = 0;
    };

    void SetSet(int set_){ set = set_; }
    void SetAsReconstructable(){ isReconstructable = true; }
    void AddReconstructed(){ nReconstructed++; }

    int  GetSet(){ return set;}
    bool IsReconstructable(){ return isReconstructable; }
    bool IsReconstructed(){ return nReconstructed >= 1; }
    bool GetNClones(){ return (nReconstructed > 1) ? nReconstructed - 1 : 0; }

    void Print(){ cout << "Set: " << set << " RecoAble: " << isReconstructable << " NReco: " << nReconstructed << endl; }
  private:
    int  set;                // set of tracks 0-OutSet, 1-ExtraSet, 2-RefSet
    bool isReconstructable;
    int  nReconstructed;
};

  /// Information about reconstruction of Reconstructed Track
class AliHLTTPCCAPerformanceRecoTrackData{
  public:
    AliHLTTPCCAPerformanceRecoTrackData(){
      mcTrackId = -1;
    };

    void SetMCTrack(int mcTrackId_, float purity_){
      mcTrackId = mcTrackId_;
      purity = purity_;
    }

    int GetMCTrackId(){ return mcTrackId; }
    int GetPurity(){ return purity; }
    bool  IsGhost( float minPurity = 0)  { return (mcTrackId == -1) || (purity < minPurity); }

    void Print(){ cout << "Track: " << mcTrackId << " Purity: " << purity << endl; }
  private:
    int mcTrackId;
    float purity;
};


#endif
