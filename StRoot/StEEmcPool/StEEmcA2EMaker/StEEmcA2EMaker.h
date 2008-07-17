#ifndef __StEEmcA2EMaker_h__
#define __StEEmcA2EMaker_h__

#include <StMaker.h>
#include <TString.h>
#include <vector>

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#include "StEEmcTower.h"
#include "StEEmcStrip.h"

class StEEmcDbMaker;
class StMuDstMaker;
class StEventMaker;

class StMuEmcCollection;
class StEmcCollection;

class StEEmcA2EMaker : public StMaker {

 public:

  /// Constructor
  StEEmcA2EMaker( const Char_t *name="mEEanalysis" );
  /// Destructor
  ~StEEmcA2EMaker(){ /* nada */ };

  /// Initialize
  Int_t Init();
  /// Read and process one event
  Int_t Make();
  /// Clear the maker for next event
  void  Clear(Option_t *opts="");

  /// Set the number of sigma above pedestal required 
  /// to consider the detector "hit".  May be negative 
  /// value.  Layer [0-5]=[TPQRUV].
  void threshold( Float_t cut, Int_t layer );

  /// Set the name of the EEMC database, init obtains pointer.
  void database( const Char_t *dbname );

  /// Set the name and type of the input data maker.
  /// \param name: name of the maker
  /// \param type: 1=muDst, 2=StEvent, 3=muEzt (unsupported)
  void source( const Char_t *name, Int_t type=0 ); 

  /// Returns a vector of hit tower, preshower and postshower
  /// elements.  Check hitTowers[i].layer() to deterimine if
  /// it is a tower(0), pre1(1), pre2(2) or postshower(3)
  /// element.  
  StEEmcTowerVec_t towers(Int_t layer=0){ return mHitTowers[layer]; }

  /// Returns a vector of hit strips, given the sector and plane
  StEEmcStripVec_t strips(Int_t sec, Int_t pln ){ return mHitStrips[sec][pln]; }

  /// Return number of hit tower elements for specified layer.
  /// \param layer: 0=T, 1=P, 2=Q, 3=R
  Int_t numberOfHitTowers(Int_t layer) { return (Int_t)mHitTowers[layer].size(); }

  /// Return a specified hit tower
  /// \param hit: index of the hit, 0 <= hit < numberOfHitTowers()
  /// \param layer: 0=T, 1=P, 2=Q, 3=R
  StEEmcTower hittower( Int_t hit, Int_t layer ){ return mHitTowers[layer][hit]; } 

  /// Return a specified tower element
  /// \param index: tower index ranging from 0-719
  /// \param layer: layer index, 0=T, 1=P, 2=Q, 3=R
  StEEmcTower tower(Int_t index, Int_t layer=0) { return mTowers[index][layer]; }

  /// Return a specified tower element
  /// \param sector: tower sector, counting from 0 [0,12)
  /// \param subsector: tower subsector A-E, counting from 0 [0,5)
  /// \param etabin: tower etabin, counting from 0 [0,12) 
  /// \param layer: layer index, 0=T, 1=P, 2=Q, 3=R
  StEEmcTower tower(Int_t sector, Int_t subsector, Int_t etabin, Int_t layer=0){return tower(index(sector,subsector,etabin),layer);}

  /// Return a pointer to the tower element which the specified
  /// vector (origin at 0,0,0) points to.  A NULL is returned if
  /// no valid tower exists at that position.
  StEEmcTower *tower( TVector3 &r, Int_t layer );
  
  /// Given tower sector, subsector, translate to phibin 
  Int_t phibin( Int_t sector, Int_t subsector ) { return sector * 5 + subsector; } 
  /// Given tower sector, subsector, etabin, translate into index
  Int_t index( Int_t sector, Int_t subsector, Int_t etabin ) { return 12 * phibin(sector, subsector) + etabin; }  
  

  /// Returns the tower with the largest ADC response
  /// \param layer: TPQR=0123 
  StEEmcTower hightower(Int_t layer=0);
  
  /// Return number of hit SMD strips for the specified sector, plane
  /// \param sector: 0-11 for 12 EEMC sectors
  /// \param plane: 0=U, 1=V
  Int_t numberOfHitStrips(Int_t sector, Int_t plane ) { return (Int_t)mHitStrips[sector][plane].size(); }

  /// Returns the specified hit strip
  /// \param sec: sector containing the hit
  /// \param pl: plane containing the hit (0=U, 1=V)
  /// \param hit: [0, numberOfHitStrips())
  StEEmcStrip hitstrip(Int_t sec,Int_t pl, Int_t hit){ return mHitStrips[sec][pl][hit]; } 
  
  /// Return a specifed hit SMD strip
  /// \param sector: sector index, [0,12)
  /// \param plane: plane index, 0=U, 1=V
  /// \param strip: strip index, runs [0,288)
  StEEmcStrip strip(Int_t sector, Int_t plane, Int_t strip) 
    { return mStrips[sector][plane][strip]; }
  
  /// Get the energy (towers) or energy deposit (pre,post,smd) in
  ///  the specified sector
  /// \param sector: 0..11 the 12 EEMC sectors
  /// \param layer: 0=T, 1=P, 2=Q, 3=R, 4=U, 5=V
  Float_t energy(Int_t sector, Int_t layer) 
    { return mEnergy[sector][layer]; }
  /// Return energy summed over full endcap
  Float_t energy(Int_t layer); 

  /// Return number of hits (number of elements above threshold)
  /// for the specified sector and layer. 
  /// \param sector: 0..11 the 12 EEMC sectors
  /// \param layer: 0=T, 1=P, 2=Q, 3=R, 4=U, 5=V
  Int_t numberOfHits(Int_t sector, Int_t layer){ return mHits[sector][layer]; } 

  /// Set a "scale" parameter for reconstructing MC.  Energies
  /// will be multiplied by scale.
  void scale(Float_t s){ mScale=s; }
  
  private:
  protected:
  
  Float_t mScale;         /**<- Scales tower energy for MC  */
  Float_t mSigmaPed[6];   /**<- ADC > nSigmaPed + ped       */
  TString mDbName;        /**<- Name of the DB maker        */
  TString mInputName;     /**<- Name of the input maker     */
  Int_t mInputType;       /**<- Type of input maker         */

  StEEmcDbMaker *mDbMaker;      /**<- Pointer to DB maker   */
  StMuDstMaker  *mMuDstMaker;   /**<- Pointer to MuDst      */
  StEventMaker  *mEventMaker;   /**<- Pointer to StEvent    */

  EEmcGeomSimple *mEEgeom;      /**<- Pointer to EEMC geometry class */

  /// Read data from muDst, StEvent or muEzt branches, 
  /// depending on input type

  Bool_t readData();     /**<- Main routine for reading data */
  Bool_t readMuDst();    /**<- Read from MuDst (type=1)      */
  Bool_t readStEvent();  /**<- Read from StEvent (type=2)    */
  Bool_t readEzt();      /**<- Read from EZtrees, not implemented */

  /// Main "filling" method.  StEvent and Ezt will be converted
  /// to mudst collection prior to call
  Bool_t fillFromMuDst( StMuEmcCollection *emc );
  /// If StEvent is used, we will fill additional parts of StEEmcElement
  Bool_t fillFromSt( StEmcCollection *emc );



  /// Add a tower hit
  /// \param sec: sector [0,12)
  /// \param sub: subsector [0,5)
  /// \param eta: etabin [0,12)
  /// \param adc: adc value
  /// \param layer: layer [0,4)
  void addTowerHit(Int_t sec, Int_t sub, Int_t eta, Float_t adc, Int_t layer);
  /// Add an smd hit
  /// \param sec: sector [0,12)
  /// \param plane: plane [0,2) = U,V
  /// \param str: strip number [0,288)
  /// \param adc: adc value
  void addSmdHit(Int_t sec, Int_t plane, Int_t str, Float_t adc );

  /// Array of 720 x 4 tower objects 
  StEEmcTower mTowers[kEEmcNumSectors*kEEmcNumSubSectors*kEEmcNumEtas][4];
  /// Array of 12x2x288 smd strip objects
  StEEmcStrip mStrips[kEEmcNumSectors][kEEmcNumSmdUVs][kEEmcNumStrips];

  /// Each element in this vector contains a vector of hit 
  /// tower elements.  The dimension of this vector will be
  /// 4, corresponding to:
  ///
  /// mHitTowers[0] = vector of hit towers
  /// mHitTowers[1] = vector of hit preshower1 elements
  /// mhitTowers[2] = vector of hit preshower2 elements
  /// mHitTowers[3] = vector of hit postshower elements
  std::vector< StEEmcTowerVec_t > mHitTowers;

  StEEmcTower *mHighTower[4]; /**<- Holds pointer to highest responding tower element in layer */

  /// Same concept as with the hit towers, but this time
  /// applied to the SMD planes.  mHitStrips[sec][plane][n]
  /// will, for instance, return the nth hit strip in the
  /// specified sector (sec=0..11) for the specified plane.
  std::vector< std::vector< StEEmcStripVec_t > > mHitStrips;


  /// Summed energy (towers) or energy deposit (pre,post,smd)
  /// in each sector.  0=T,1=P,2=Q,3=R,4=U,5=V
  Float_t mEnergy[kEEmcNumSectors][6];
  /// Number of hits in layer
  Int_t   mHits[kEEmcNumSectors][6]; 

  /// Makes class available to root
  ClassDef(StEEmcA2EMaker,1);

};

inline void StEEmcA2EMaker::threshold(Float_t c, Int_t l){ mSigmaPed[l]=c; }
inline void StEEmcA2EMaker::database(const Char_t *name){ mDbName=name; }
inline void StEEmcA2EMaker::source(const Char_t *name, Int_t type){ mInputName=name; mInputType=type; }

inline StEEmcTower StEEmcA2EMaker::hightower(Int_t layer){ return *mHighTower[layer]; }


#endif
