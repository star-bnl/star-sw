#ifndef __StEEmcDisplay_h__
#define __StEEmcDisplay_h__

#include "TNamed.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcTower.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcCluster.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcSmdCluster.h"
#include "StEEmcPool/StEEmcPointMaker/StEEmcPointMaker.h"

class TH1F;
class TH2F;
class TMarker;
class TCanvas;
class TPaveText;

class EEmcGeomSimple;

class StEEmcDisplay : public TNamed
{

 public:
  StEEmcDisplay( const Char_t *name="aname", const Char_t *atitle="atitle" );
  ~StEEmcDisplay(){ /* nada */ };

  /// Add a tower to the display
  void add ( StEEmcTower      tower );
  /// Add an SMD strip to the display
  void add ( StEEmcStrip      strip );
  
  /// Add an smd cluster to the display with the specified color and fill (TAttFill) type
  void add ( StEEmcSmdCluster cluster, Int_t color=1, Int_t fill=1001 );
  /// Add a point to the display with the specified color an style
  void add ( StEEmcPoint      point,   Int_t color=1, Int_t style=20 );

  /// clears all storage arrays, deletes all canvases and histograms
  void clear();

  TH2F *DrawTowers(Option_t *opts="box,text");/**< function to display tower energy distribution */
  TH2F *DrawPoints(Option_t *opts="box");/**< draws tower energy distribution and overlays reconstructed points */
  TH2F *DrawLayer(Int_t layer, Option_t *opts="box,text");/**< draws one of the preshower or postshower layers */
  TH1F *DrawSmd( Int_t sector, Int_t plane, Option_t *opts="" );/**< draws an smd plane */
  TH1F *DrawClusters( Int_t sector, Int_t plane, Option_t *opts="");/**< draws smd clusters on top of smd plane */

  TH1F *DrawCluster( Int_t cluster, Option_t *opts="" ); /**< -- not a user function -- */

  TCanvas *getEEmc(){ return eemc; }/**< returns pointer to the canvas showing tower energy distribution */
  TCanvas *getSmdu(){ return smdu; }/**< returns pointer to the canvas showing smdu energy distribution */
  TCanvas *getSmdv(){ return smdv; }/**< returns pointer to the canvas showing smdv energy distribution */
  TCanvas *getPre1(){ return pre1; }/**< returns pointer to the canvas showing pre1 energy distribution */
  TCanvas *getPre2(){ return pre2; }/**< returns pointer to the canvas showing pre2 energy distribution */
  TCanvas *getPost(){ return post; }/**< returns pointer to the canvas showing post energy distribution */

  /// Returns the energy summed over the specified layer.  Useful for making
  /// plots of total energy distributions, e.g.
  ///
  /// mTree->Draw("sumEnergy(3)","<cuts>")
  /// 
  /// to draw the total energy deposited in the postshower
  Float_t sumEnergy( Int_t layer ); 

  /// Returns the number of hit elements in the spcified layer above threshold
  Int_t   hitMultiplicity( Int_t layer, Float_t threshold = 0.0 ); 

  
  Int_t   mHighTowerPhibin; /**< phibin of the highest energy tower */
  Int_t   mHighTowerEtabin; /**< etabin of the highest energy tower */
  Float_t mHighTowerEnergy; /**< energy of the highest energy tower */

 private:
 protected:

  TCanvas *eemc;//!
  TCanvas *smdu;//!
  TCanvas *smdv;//!
  TCanvas *pre1;//!
  TCanvas *pre2;//!
  TCanvas *post;//!

  EEmcGeomSimple *geom; //||

  Float_t mEnergy2Mip;  //||

  Float_t  mTowerEnergy[ kEEmcNumSectors ][ kEEmcNumSubSectors ][ kEEmcNumEtas ];
  Int_t    mTowerStat  [ kEEmcNumSectors ][ kEEmcNumSubSectors ][ kEEmcNumEtas ];
  Int_t    mTowerFail  [ kEEmcNumSectors ][ kEEmcNumSubSectors ][ kEEmcNumEtas ];
  TH2F    *hTowers; //!

  Float_t  mStripEnergy[ kEEmcNumSectors ][ kEEmcNumSmdPlanes ][ kEEmcNumStrips ];
  Int_t    mStripStat  [ kEEmcNumSectors ][ kEEmcNumSmdPlanes ][ kEEmcNumStrips ];
  Int_t    mStripFail  [ kEEmcNumSectors ][ kEEmcNumSmdPlanes ][ kEEmcNumStrips ];
  TH1F    *hSmds[ kEEmcNumSectors ][ kEEmcNumSmdPlanes ]; //!

  Float_t  mPrepostEnergy[ kEEmcNumSectors ][ kEEmcNumSubSectors ][ kEEmcNumEtas ][3];
  Int_t    mPrepostStat  [ kEEmcNumSectors ][ kEEmcNumSubSectors ][ kEEmcNumEtas ][3];
  Int_t    mPrepostFail  [ kEEmcNumSectors ][ kEEmcNumSubSectors ][ kEEmcNumEtas ][3];
  TH2F    *hPre1; //!
  TH2F    *hPre2; //!
  TH2F    *hPost; //!  

  std::vector< Int_t >                 mClusterKey;
  std::vector< Float_t >               mClusterEnergy;
  std::vector< Int_t >                 mClusterSector;
  std::vector< Int_t >                 mClusterPlane;
  std::vector< std::vector < Int_t > > mClusterStrips;
  std::vector< Float_t >               mClusterMean;
  std::vector< Float_t >               mClusterSigma;
  std::vector< TPaveText * >           mClusterStats;
  std::vector< Bool_t >                mClusterSplit;

  std::vector< TH1F* >                 mClusterHisto; //||
  std::vector< Int_t >                 mClusterColor; //||
  std::vector< Int_t >                 mClusterFill;  //||

  std::vector< Int_t >                 mPointKey;
  std::vector< Float_t >               mPointEnergy; 
  std::vector< Float_t >               mPointX;
  std::vector< Float_t >               mPointY;

  std::vector< TMarker* >              mPointMarker; //||
  std::vector< Int_t >                 mPointColor;  //||
  std::vector< Int_t >                 mPointStyle;  //||

  std::vector< Int_t >                 mPointUkey;
  std::vector< Int_t >                 mPointVkey;

  void failTower( Int_t sec, Int_t sub, Int_t eta, TH2F *histo );

  Int_t nClustersU;
  Int_t nClustersV;
  Int_t nPoints;
  Int_t nPi0;
  Int_t nEta;

  ClassDef(StEEmcDisplay,1);

};

#endif
