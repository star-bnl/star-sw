/**
 *       Alice HTL TPC counters for efficiency calculation
 **/


#ifndef AliHLTTPCCounters_H
#define AliHLTTPCCounters_H

#include "Counters.h"

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

    AddCounter("long_ref"      ,"LongRef mc10r10   efficiency");
    AddCounter("ref"           ,"Refset  mc10r10   efficiency");
    AddCounter("total"         ,"Allset  mc10r10   efficiency");
    AddCounter("total_4_15"    ,"Allset  mc4-15r4  efficiency");
    AddCounter("total_4"       ,"Allset  mc4  r4   efficiency");
    AddCounter("extra"         ,"Extra   mc10r10   efficiency");
    AddCounter("rest"          ,"Rest    mc10r10   efficiency");
//     AddCounter("ref_prim"      ,"RefPrim   efficiency");
//     AddCounter("ref_sec"       ,"RefSec    efficiency");
//     AddCounter("extra_prim"      ,"ExtraPrim efficiency");
//     AddCounter("extra_sec"       ,"ExtraSec  efficiency");
    AddCounter("nhits"         ,"NHits   per    track");
    AddCounter("purity"        ,"Purity   per   track");
    AddCounter("all_"        ,  "All           tracks");
    AddCounter("all_15_20"         ,"All mc15-20 r15   efficiency");
    AddCounter("all_20"         ,"All mc>20   r15   efficiency");
    AddCounter("all_mc15_r10"         ,"All mc15    r10   efficiency");
    //
    AddCounter("long_ref30"      ,"LongRef 30-72 r15 efficiency");
    AddCounter("ref30"           ,"Refset  30-72 r15 efficiency");
    AddCounter("total30"         ,"Allset  30-72 r15 efficiency");
    AddCounter("extra30"         ,"Extra   30-72 r15 efficiency");
    //
    AddCounter("long_ref_l30"      ,"LongRef <30 r15   efficiency");
    AddCounter("ref_l30"           ,"Refset  <30 r15   efficiency");
    AddCounter("total_l30"         ,"Allset  <30 r15   efficiency");
    AddCounter("extra_l30"         ,"Extra   <30 r15   efficiency");
    //
    AddCounter("long_ref_l30_r10"      ,"LongRef <30 r10   efficiency");
    AddCounter("ref_l30_r10"           ,"Refset  <30 r10   efficiency");
    AddCounter("total_l30_r10"         ,"Allset  <30 r10   efficiency");
    AddCounter("extra_l30_r10"         ,"Extra   <30 r10   efficiency");

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
    // ---
//    ratio_reco = reco/mc;
//    if (mc.counters[0] > 0){
//      ratio_clones = clones/double(mc.counters[indices["total"]]);
//    }
//    else{
//      ratio_clones = -1;
//    }
    // ---
  };
  
  void Inc(bool isReco, int _nclones, string name){
    TEfficiencies::Inc(isReco, name);
    clone.counters[indices[name]] += _nclones;
  };

//  void Inc1(bool isReco, int _nclones, string name){
//    if(isReco) reco.counters[indices["all_mc15_r10"]]++;
//    if(isReco) clone.counters[indices["all_mc15_r10"]] += _nclones;
//  };

  void IncA( int _nhits, int _purity ) {
    reco.counters[indices["nhits"]] += _nhits;
    reco.counters[indices["purity"]] += _purity;
    reco.counters[indices["all_"]]++;
  }

  void Print(){
    if (nEvents == 0) {
      cout << "Warning: nEvents = 0. " << endl;
      return;
    }
    
    cout.setf(ios::fixed);
    cout.setf(ios::showpoint);
    cout.precision(3);
    cout << "Track category                 : " << " Eff  "       <<" / "<< "Clones" <<" | "<< "All Reco" <<" | "<< "All MC"  << endl;
    
    int NCounters = mc.NCounters;
    for (int iC = 0; iC < NCounters; iC++){
	if( iC > 5 && iC < 10 ) continue;
      cout << names[iC]  << "   : "
          << ratio_reco.counters[iC]
          << "  / " << ratio_clone.counters[iC]
          << "  | " << reco.counters[iC]
          << "\t  | " << mc.counters[iC]  << endl;
    }
    cout << "Ghost     probability  : " << ratio_ghosts <<" | "<< ghosts << endl;
    cout << "All reco tracks/ev : " << double(reco.counters[indices["total"]])/double(nEvents)  << endl;

//    cout << ">NHits  per track : " << double(reco.counters[indices["nhits"]])/double(reco.counters[indices["total"]] + ghosts + clone.counters[indices["total"]]) << endl;
//    cout << ">Purity per track : " << double(reco.counters[indices["purity"]])/double(reco.counters[indices["total"]] + ghosts + clone.counters[indices["total"]]) << endl;
    cout << ">NHits  per track : " << double(reco.counters[indices["nhits"]])/double(reco.counters[indices["all_"]]) << endl;
    cout << ">Purity per track : " << double(reco.counters[indices["purity"]])/double(reco.counters[indices["all_"]]) << endl;
//    cout<<" ___ total tracks calculated: "<<reco.counters[indices["total"]] + ghosts + clone.counters[indices["total"]]<<"\n";
//    cout<<" ... nRecoTracks: "<<nRecoTracks<<"\n";
//#ifdef MAIN_DRAW
//    getchar();
//#endif
  };

  void SetNRecoTracks( int n ) { nRecoTracks += n; }

  TTracksCatCounters<double> ratio_clone;
  TTracksCatCounters<int> clone;
  int nRecoTracks;
};

  /// Information about reconstruction of MCTrack
class AliHLTTPCCAPerformanceMCTrackData{
  public:
    AliHLTTPCCAPerformanceMCTrackData(){
      set = 0;
      isReconstructable = 0;
      nReconstructed = 0;
      // ---
      set1 = 0;
      nReconstructed1 = 0;

      isReconstructable30 = 0;
      nReconstructed30 = 0;
      set30 = 0;

      isReconstructable_l30 = 0;
      nReconstructed_l30 = 0;
      set_l30 = 0;

      isReconstructable_l30_r10 = 0;
      nReconstructed_l30_r10 = 0;
      set_l30_r10 = 0;

      isReconstructable_mc4_15_r4 = 0;
      nReconstructed_mc4_15_r4 = 0;
      set_mc4_15_r4 = 0;

      isReconstructable_mc4_r4 = 0;
      nReconstructed_mc4_r4 = 0;
      set_mc4_r4 = 0;
      // ---
    };

    void SetSet(int set_){ set = set_; }
    void SetAsReconstructable(){ isReconstructable = true; }
    void AddReconstructed(){ nReconstructed++; }

    int  NReconstructed(){ return nReconstructed; }
    int  GetSet(){ return set;}
    bool IsReconstructable(){ return isReconstructable; }
    bool IsReconstructed(){ return nReconstructed >= 1; }
    int GetNClones(){ return (nReconstructed > 1) ? nReconstructed - 1 : 0; }

    // ---
    void SetFirstTrackID( int id ) { tFirstTrackID = id; }
    int GetFirstTrackID() { return tFirstTrackID; }
    void AddReconstructed1(){ nReconstructed1++; }
    bool IsReconstructed1(){ return nReconstructed1 >= 1; }
    int GetNClones1(){ return (nReconstructed1 > 1) ? nReconstructed1 - 1 : 0; }

    void SetSet1(int set_){ set1 = set_; }
    int  GetSet1(){ return set1;}

    bool IsReconstructable30() const { return isReconstructable30; }
    bool IsReconstructable30() { return isReconstructable30; }
    bool IsReconstructed30() { return nReconstructed30 >= 1; }
    int  NReconstructed30() { return nReconstructed30; }
    void SetAsReconstructable30() { isReconstructable30 = true; }
    void SetAsNonReconstructable30() { isReconstructable30 = false; }
    void AddReconstructed30() { nReconstructed30++; }
    int GetNClones30() { return (nReconstructed30 > 1) ? nReconstructed30 - 1 : 0; }
    void SetSet30(int set_) { set30 = set_; }
    int  GetSet30() { return set30;}

    void SetAsReconstructable_l30() { isReconstructable_l30 = true; }
    void SetAsNonReconstructable_l30() { isReconstructable_l30 = false; }
    bool IsReconstructable_l30() { return isReconstructable_l30; }
    bool IsReconstructed_l30() { return nReconstructed_l30 >= 1; }
    void AddReconstructed_l30() { nReconstructed_l30++; }
    void SetSet_l30(int set_) { set_l30 = set_; }
    int  GetSet_l30() { return set_l30;}
    int GetNClones_l30() { return (nReconstructed_l30 > 1) ? nReconstructed_l30 - 1 : 0; }

    void SetAsReconstructable_l30_r10() { isReconstructable_l30_r10 = true; }
    void SetAsNonReconstructable_l30_r10() { isReconstructable_l30_r10 = false; }
    bool IsReconstructable_l30_r10() { return isReconstructable_l30_r10; }
    bool IsReconstructed_l30_r10() { return nReconstructed_l30_r10 >= 1; }
    void AddReconstructed_l30_r10() { nReconstructed_l30_r10++; }
    void SetSet_l30_r10(int set_) { set_l30_r10 = set_; }
    int  GetSet_l30_r10() { return set_l30_r10;}
    int GetNClones_l30_r10() { return (nReconstructed_l30_r10 > 1) ? nReconstructed_l30_r10 - 1 : 0; }

    void SetAsReconstructable_mc4_15_r4() { isReconstructable_mc4_15_r4 = true; }
    void SetAsNonReconstructable_mc4_15_r4() { isReconstructable_mc4_15_r4 = false; }
    bool IsReconstructable_mc4_15_r4() { return isReconstructable_mc4_15_r4; }
    bool IsReconstructed_mc4_15_r4() { return nReconstructed_mc4_15_r4 >= 1; }
    void AddReconstructed_mc4_15_r4() { nReconstructed_mc4_15_r4++; }
    void SetSet_mc4_15_r4(int set_) { set_mc4_15_r4 = set_; }
    int  GetSet_mc4_15_r4() { return set_mc4_15_r4;}
    int GetNClones_mc4_15_r4() { return (nReconstructed_mc4_15_r4 > 1) ? nReconstructed_mc4_15_r4 - 1 : 0; }

    void SetAsReconstructable_mc4_r4() { isReconstructable_mc4_r4 = true; }
    void SetAsNonReconstructable_mc4_r4() { isReconstructable_mc4_r4 = false; }
    bool IsReconstructable_mc4_r4() { return isReconstructable_mc4_r4; }
    bool IsReconstructed_mc4_r4() { return nReconstructed_mc4_r4 >= 1; }
    void AddReconstructed_mc4_r4() { nReconstructed_mc4_r4++; }
    void SetSet_mc4_r4(int set_) { set_mc4_r4 = set_; }
    int  GetSet_mc4_r4() { return set_mc4_r4;}
    int GetNClones_mc4_r4() { return (nReconstructed_mc4_r4 > 1) ? nReconstructed_mc4_r4 - 1 : 0; }

    int GetNClones_all4() { return ((nReconstructed_mc4_15_r4+nReconstructed) > 1) ? (nReconstructed_mc4_15_r4+nReconstructed) - 1 : 0; }

    void SetAsNonReconstructable(){ isReconstructable = false; }
    // ---

    void Print(){ cout << "Set: " << set << " RecoAble: " << isReconstructable << " NReco: " << nReconstructed << endl; }
  private:
    int  set;                // set of tracks 0-OutSet, 1-ExtraSet, 2-RefSet
    bool isReconstructable;
    int  nReconstructed;
    // ---
    int tFirstTrackID;
    int  nReconstructed1;
    int set1;

    bool isReconstructable30;
    int  nReconstructed30;
    int set30;

    bool isReconstructable_l30;
    int  nReconstructed_l30;
    int set_l30;

    bool isReconstructable_l30_r10;
    int  nReconstructed_l30_r10;
    int set_l30_r10;

    bool isReconstructable_mc4_15_r4;
    int  nReconstructed_mc4_15_r4;
    int set_mc4_15_r4;

    bool isReconstructable_mc4_r4;
    int  nReconstructed_mc4_r4;
    int set_mc4_r4;
    // ---
};

  /// Information about reconstruction of Reconstructed Track
class AliHLTTPCCAPerformanceRecoTrackData{
  public:
    AliHLTTPCCAPerformanceRecoTrackData(){
      mcTrackId = -1;

      recnstructed = 0;
      clone = 0;
    };

    void SetMCTrack(int mcTrackId_, float purity_, int nHits_){
      mcTrackId = mcTrackId_;
      purity = purity_;
      nHits = nHits_;
    }

    int GetMCTrackId() const { return mcTrackId; }
    float GetPurity() const { return purity; }
    bool  IsGhost( float minPurity = 0) const  { return (mcTrackId == -1) || (purity < minPurity); }
    bool  IsReco( float minPurity = 0, int minNHits = 0) const { return (mcTrackId != -1) && (purity >= minPurity) && (nHits >= minNHits); }
    int GetNHits() const { return nHits; }

    void SetReconstructed( bool r = true ) { recnstructed = r; }
    bool IsReconstructed() const { return recnstructed; }
    bool IsReconstructed() { return recnstructed; }
    void SetClone( bool c = true ) { clone = c; }
    bool IsClone() const { return clone; }
    bool IsClone() { return clone; }

    void Print(){ cout << " ___ Track: " << mcTrackId << " Purity: " << purity << endl; }
  private:
    int mcTrackId;
    float purity;
    int nHits;

    bool recnstructed;
    bool clone;
};


#endif
