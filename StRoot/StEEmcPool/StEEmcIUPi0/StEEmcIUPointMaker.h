#ifndef __StEEmcIUPointMaker_h__
#define __StEEmcIUPointMaker_h__

#include "StMaker.h"
#include "TString.h"
#include "StEEmcIUCluster.h"
#include "StEEmcIUSmdCluster.h"
#include "StEEmcIUPoint.h" 


#include <map>

class StEEmcA2EMaker;
class StEEmcIUClusterMaker;
class TH1F;
class TH2F;
class EEmcGeomSimple;
class EEmcSmdMap;
class EEmcSmdGeom; 

class StEEmcIUPointMaker : public StMaker {

 public:

  StEEmcIUPointMaker( const Char_t *name );
  ~StEEmcIUPointMaker(){ /* nada */ };

  /// Initialize
  Int_t Init();
  /// Build points for this event
  Int_t Make();
  /// Clear old points
  void  Clear(Option_t *opts="");
  /// Set adc to energy maker 
  void analysis(const Char_t *name);
  /// Set cluster maker
  void clusters(const Char_t *name);

  /// Sets the miminimum energy for matching a pair of smd
  /// clusters to a tower.  SMD pairs which cross beneath
  /// a tower which falls below the threshold will not form
  /// a point.  Default is zero.
  void towerThreshold( Float_t t) { mTowerThreshold=t; }

  /// Sets an smd matching requirement, Eu>s*Ev&&Ev>2*Eu
  void smdMatch( Float_t s ){ mSmdMatch = s; }
  /// Return a unique key assigned by the cluster maker
  //Int_t key(){ return mKey; }
  /// Return a unique key assigned by the cluster maker
  //Int_t key()const { return mKey; }

  /// Set a unique id for this cluster... k>=0 is intended to
  /// be assigned within the cluster maker.  k<0 intended for
  /// error flags to kill off clusters.
  //void  key(Int_t k){ mKey=k; }

    /// Return vector of all points found in endcap 
  StEEmcIUPointVec_t points();
  /// Return number of points
  Int_t numberOfPoints(); 
  /// Return a specified point
  StEEmcIUPoint point(Int_t ipoint); 

  /// Return vector of smd only points found in endcap (all u,v crossings
  /// beneath an active tower or a "failed" tower with signal in one 
  /// other layer.)  
  StEEmcIUPointVec_t smdPoints();
  /// Return the total number of smd points
  Int_t numberOfSmdPoints();
  /// Return a specified smd point 
  StEEmcIUPoint smdPoint(Int_t ip); 

  /// Total energy seen by the algorithm
  Float_t energySeen(){ return mEseen; }

  /// If called, will look for the presence of StEvent and
  /// fill the StEmcCollection.
  void setFillStEvent(){ mFillStEvent=true; }

  /// Energy sharing mode.
  /// \param mode: 0=smd, 1=tower-shape (iterated).
  void setEnergyMode(Int_t mode){ mEnergyMode=mode; }
  /// Number of iterations for tower-shape mode
  void setLimit(Int_t l){ mLimit=l; }


  /// Given an StEmcPoint, return the StEEmcIUPoint from
  /// whence it came
  StEEmcIUPoint point( StEmcPoint *p ){ return mEtoEE[p]; }


 private:
 protected:
  Int_t ukey;
  Int_t vkey;
 
  TString mNameAnalysis; /**<- name of the adc to energy maker */
  TString mNameClusters; /**<- name of the cluster make */

  /// ADC2E
  StEEmcA2EMaker     *mEEanalysis; //!
  /// Clusters
  StEEmcIUClusterMaker *mEEclusters; //! 

  /// Tower geometry
  EEmcGeomSimple *mEEtow; //!
  /// Smd geometry
  EEmcSmdGeom    *mEEsmd; //!
  /// Tower to smd map
  EEmcSmdMap     *mEEmap; //! 
  TH1F       *hZEratio;
  TH1F       *pointet;
  TH2F       *pointgeo;
  /// Fills the StEmcPoint collection
  void fillStEvent();
  /// Checks that StEvent is properly saved
  void verifyStEvent();

  /// All fully reconstructed points
  StEEmcIUPointVec_t mPoints;  //!

  /// SMD only points
  StEEmcIUPointVec_t mSmdPoints;  //!

  /// maps smd clusters to points
  std::map< StEEmcIUSmdCluster, StEEmcIUPointVec_t > mUclusters2points; //!
  /// maps smd clusters to points
  std::map< StEEmcIUSmdCluster, StEEmcIUPointVec_t > mVclusters2points; //!
  
  /// build smd points and associations between smd points and clusters
  StEEmcIUPointVec_t buildSmdPoints( Int_t sector, StEEmcIUSmdClusterVec_t &u, StEEmcIUSmdClusterVec_t &v );
  /// find points in the endcap
  Bool_t findPoints ( Int_t sector, 
		      StEEmcIUSmdClusterVec_t u, 
		      StEEmcIUSmdClusterVec_t v, 
		      StEEmcIUPointVec_t &p );

  /// Divide energy of eemc towers between identified smd points using fit (doesn't work)
  void shareEnergy();
  /// Divide energy of eemc towers between identified smd points (doesn't work as well as smd algo)
  void shareEnergySimple();
  /// Divide energy of eemc towers between identified smd points in proportion to the smd energy
  void shareEnergySmd();
  /// Determine the number of points which share tower energy with another point
  void countRelatives();
  /// Remove a cluster from the list of clusters
  void removeCluster( StEEmcIUSmdClusterVec_t &clusters, Int_t key );

  /// return the fraction of energy expected in tower 
  /// due to the presence of a point, based on tower
  /// energy response function
  Float_t  fracp2t( StEEmcIUPoint &p, StEEmcTower &t );

  /// Minimum tower threshold to consider a U,V pair of clusters a
  /// valid smd point.  Cut removes U,V pairs from being considered
  /// for association in findPoints().
  Float_t mTowerThreshold;
  /// Maximum allowed value for |Eu-Ev|/2(Eu+Ev) to form a candidate smd
  /// point in buildSmdPoints().  Cut is applied before U,V clusters are
  /// matched in findPoints().
  Float_t mSmdMatch;
  /// Energy seen by the algorithm
  Float_t mEseen;
  /// Option to fill StEvent
  Bool_t mFillStEvent;
  /// Option for dividing energy
  Int_t mEnergyMode;
  /// How many iterations for the tower energy sharing mode
  Int_t mLimit;

  /// Map connecting StEEmcIUPoint to StEmcPoint
  std::map<StEmcPoint *, StEEmcIUPoint> mEtoEE;    //!

  /// Makes class available to root
  ClassDef(StEEmcIUPointMaker,1);

};

inline void StEEmcIUPointMaker::analysis(const Char_t *n){ mNameAnalysis=n; }
inline void StEEmcIUPointMaker::clusters(const Char_t *n){ mNameClusters=n; }

inline StEEmcIUPointVec_t StEEmcIUPointMaker::points(){ return mPoints ; }
inline Int_t StEEmcIUPointMaker::numberOfPoints(){ return mPoints.size(); } 
inline StEEmcIUPoint StEEmcIUPointMaker::point(Int_t ip){ return mPoints[ip]; } 

inline StEEmcIUPointVec_t StEEmcIUPointMaker::smdPoints(){ return mSmdPoints; }
inline Int_t StEEmcIUPointMaker::numberOfSmdPoints(){ return mSmdPoints.size(); } 
inline StEEmcIUPoint StEEmcIUPointMaker::smdPoint(Int_t ip){ return mSmdPoints[ip]; } 
#endif
