#ifndef __StEEmcMixQAMaker_h__
#define __StEEmcMixQAMaker_h__

#include "StMaker.h"

class StEEmcMixMaker;
class StEEmcPointMaker;
class TH1F;
class TH2F;

#include <vector>
#include "StEEmcPair.h"

class StEEmcMixQAMaker : public StMaker
{

 public:
  /// constructor
  StEEmcMixQAMaker(const Char_t *name);
  /// destructor
  ~StEEmcMixQAMaker(){ /* nada */ };

  /// initializes the maker
  Int_t Init();
  /// processes a single event
  Int_t Make();
  /// clears the maker
  void  Clear(Option_t *opts=""){ /* nada */ };
  /// specifies the name of the mixer and the mass range for 
  /// gated histograms.
  /// \param name: the name of the StEEmcMixMaker (or derivative class)
  /// \param min: minimum mass cut applied to all histograms save for the mass spectrum
  /// \param max: maximum mass cut ...
  void mixer(const Char_t *name, Float_t min=0., Float_t max=999.);
  /// specifies the name of the point maker
  void points(const Char_t *name);

  /// use combinatoric background points instead of real points
  void background(){ mBackground=true; }
 
  
  Int_t maxPerSector;  /**<-- maximum number of pairs matched to sector (default 200)*/
  Int_t maxPerEvent;   /**<-- maximum number of pairs per event (default 200)*/
  Int_t maxPerCluster; /**<-- maximum number of points matched to the 6-18 tower cluster (default 2)*/

  Float_t minTowerFrac; /**<-- minimum energy fraction (E1 + E2)/Etowers (default 0.0) */

  Float_t zVertexMin;/**<-- minimum z vertex */
  Float_t zVertexMax;/**<-- maximum z vertex */

  Float_t minZgg; /**<-- minimum zgg */
  Float_t maxZgg; /**<-- maximum zgg */

  /// add a pT bin from the last boundary added to this value 
  void addBin(Float_t b){ assert(b>mBins.back()); mBins.push_back(b); }

 private:
 protected:

  Float_t mMin; /**<-- min mass for gated quantities */
  Float_t mMax; /**<-- max mass for gated quantities */

  Bool_t mBackground; /**<-- specifies whether or not we are histograming a mixed event background */

  TH1F *hNcandidates; /**<-- number of reconstructed pi0 candidates per event */
  TH1F *hNcandidatesR; /**<-- number of reconstructed pi0 candidtaes per sector [0.1,0.18] */ 

  /// Y vs X of pi0 pairs
  std::vector<TH2F *> hYXpair;  //! Y vs X of pi0 pair
  /// Y vs X of higher energy gamma
  std::vector<TH2F *> hYXhigh;  //! Y vs X of higher E gamma
  /// Y vs X of lower energy gamma
  std::vector<TH2F *> hYXlow;   //! Y vs X of lower E gamma
  /// Energy of first gamma vs energy of second
  std::vector<TH2F *> hE1E2;    //! E1 vs E2
  
  /// PT bin boundaries
  std::vector<Float_t>               mBins;     //! Bin boundaries in pT

  //
  // The following are 2D vectors of histograms.  We 
  // bin the data in pT and sector.  Sector 13 integrates
  // over all 12 eemc sectors.  One final pT bin integrates
  // over all pT

  /// Invariant mass spectrum 
  std::vector< std::vector<TH1F *> > hMassR;    //! Inv mass of real pairs
  /// Energy sharing [mMin,mMax]
  std::vector< std::vector<TH1F *> > hZggR;     //!
  /// Opening angle [mMin,mMax]
  std::vector< std::vector<TH1F *> > hPhiggR;   //!
  /// Pair energy [mMin,mMax]
  std::vector< std::vector<TH1F *> > hEnergyR;  //!
  /// Event vertex [mMin,mMax]
  std::vector< std::vector<TH1F *> > hZvertexR; //!

  /// Mass spectrum for all events
  TH1F *hMassRall;
  /// vertex for all events
  TH1F *hZvertexRall;

  /// returns the ptbin the pair is in
  Int_t ptbin( StEEmcPair p );

  /// Verify that the pi0 candidate is the only pair of reconstructed
  /// points matching the contiguous group of towers. 
  Bool_t twoBodyCut( StEEmcPair p );

  /// Pointer to the pi0 mixer
  StEEmcMixMaker   *mEEmixer;  //!
  /// pointer to the point maker
  StEEmcPointMaker *mEEpoints; //!

  /// Makes class availabel to root
  ClassDef(StEEmcMixQAMaker,1);  

};

#endif
