// Hey Emacs this is -*-c++-*-
// $Id: EEmcTTDisplay.h,v 1.11 2004/05/06 16:02:49 zolnie Exp $
#ifndef STAR_EEmcTTDisplay
#define STAR_EEmcTTDisplay

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
class TString;
class TGeoVolume;
class TGeoMatrix;
class TList;
//
class StMuTrack;
class EEmcTower;
class EEmcTTMatch;

class EEmcTTDisplay : public EEmcGeomSimple {
public: 
  //! the constructor
  /// \param name  the top EEMC volume (TGeoVolume) label
  EEmcTTDisplay(const char *name="eemc");

  //! the destructor
  ~EEmcTTDisplay();
  
  /// returns top EEMC TGeoVolume 
  TGeoVolume* operator() () { return mEEmc; }; 
  /// returns top TGeoVolume 
  TGeoVolume* GetVolume()   { return mEEmc; }; 



  /// adds a tower to the list of displayed towers
  /// \param tile name (in the form of 05TC11) 
  /// \return      kTRUE on success and kFALSE on failure
  Bool_t       AddTower(const char *tile);

  //! adds tower to the list
  /// \param sec     sector index    [0,mNumSec)
  /// \param sub     subsector index [0,mNumSSec)
  /// \param eta     eta index       [0,mNumEta)
  /// \return      kTRUE on success and kFALSE on failure
  Bool_t       AddTower(int sec, int sub, int eta) 
  {   
    return AddTower(volumeName(sec,sub,eta)); 
  }  

  /// adds a tower to the list of displayed towers
  /// \param tower a reference to EEmcTower 
  /// \return      kTRUE on success and kFALSE on failure
  Bool_t       AddTower(const EEmcTower& tower)       ;

  /// adds a track to the list of displayed tracks
  /// \param x     x-component of the track origin
  /// \param y     y-component of the track origin
  /// \param z     z-component of the track origin
  /// \param px    x-component of the track momentum
  /// \param py    y-component of the track momentum
  /// \param pz    z-component of the track momentum
  /// \param qB    sign sensitive product of the particle charge and magnetic field 
  /// \param zMin  to BE DOCUMENTED
  /// \param zMax  to BE DOCUMENTED
  /// \return      kTRUE on success and kFALSE on failure
  Bool_t       AddTrack(Double_t x, Double_t y, Double_t z, 
			Double_t px, Double_t py, Double_t pz, 
			Double_t qB, Double_t zMin=0.0, Double_t zMax=0.0);

  /// adds a track to the list of displayed tracks
  /// \param track a refence to StMuTrack
  /// \return      kTRUE on success and kFALSE on failure
  Bool_t       AddTrack(const StMuTrack& track);


  //! adds  a tower with matched tracks to the list of displayed tower/tracks
  /// \param tmatch a refence to EEmcTTMatchp
  /// \return      kTRUE on success and kFALSE on failure
  Bool_t       AddMatch(EEmcTTMatch& tmatch);

  /// draws towers/tracks 
  /// \param option - not uset at the moment
  void         Draw ( const Option_t* option = "");

  /// clears tower/track lists
  /// \param option - not uset at the moment
  void         Clear( const Option_t* option = ""); 


  /// prints EEmcTTMatch information to an ostream
  /// \param out    a reference to std::ostream
  /// \param tmatch a reference to struct EEmcTTMatch
  void         Out(ostream &out, EEmcTTMatch &tmatch);

  /// prints EEmcTTMatch information to a TString
  /// \param out    a reference to TString
  /// \param tmatch a reference to struct EEmcTTMatch
  void         Out(TString &out, EEmcTTMatch &tmatch);
  


  /// sets STAR magnetic field 
  /// \param B magnetic field (sign sensitive) in Tesla
  void   SetMagneticField(double B) { mBField = B;    }
  //! returns STAR magnetic fiels in Tesla
  double GetMagneticField()         { return mBField; }

  //! sets a flag that controls how TPC trackcs are displayed, 
  /// if f is true  - tracks are plotted from vertex to endcap 
  /// if f is false - tracks are plotted from first to last point in the track 
  /// \param f flag true/false
  void   SetShowExtrapolatedTracks(bool f) { mShowExtrapolatedTracks = f; }
  //! gets a flag that controls how TPC trackcs are displayed, 
  /// if f is true  - tracks are plotted from vertex to endcap 
  /// if f is false - tracks are plotted from first to last point in the track 
  /// \return true or false
  bool   GetShowExtrapolatedTracks() { return mShowExtrapolatedTracks; }


private:
  //! initializes EEMC geometry: sector,subsectors and towers
  /// \param topName a label of a top most volume
  void  initGeometry(const char *topName);

  /// volume name based on sec, subsector and eta
  /// \param sec     sector index    [0,mNumSec)
  /// \param sub     subsector index [0,mNumSSec)
  /// \param eta     eta index       [0,mNumEta)
  /// \return a pointer to a static string (fixit)
  char *volumeName(int sec, int sub=-1, int eta=-1);
  /// volume name based on tower structure
  /// \param tower  a reference to tower structure
  /// \return a pointer to a static string (fixit)
  char *volumeName(const EEmcTower& tower);

  TGeoVolume* mEEmc; /**<- top TGeoVolume        */
  TList *mTowerHits; /**<- list of towers        */
  TList *mTrackHits; /**<- list of tracks        */
  double mBField   ; /**<- magnetic field (in T) */ 
  bool   mShowExtrapolatedTracks; /**<- flag to control how tracks are plotted */
  
public:
  ClassDef(EEmcTTDisplay, 1)   // 
};


#endif



// $Log: EEmcTTDisplay.h,v $
// Revision 1.11  2004/05/06 16:02:49  zolnie
// more docs
//
// Revision 1.10  2004/05/05 21:37:37  zolnie
// ver 2.0 released
//
// Revision 1.9  2004/05/04 18:28:55  zolnie
// version after split
//
// Revision 1.8  2004/04/14 16:40:34  zolnie
// *** empty log message ***
//
// Revision 1.7  2004/04/13 14:53:38  zolnie
// *** empty log message ***
//
// Revision 1.6  2004/01/27 20:38:41  zolnie
// more docs
//
// Revision 1.5  2004/01/27 16:26:14  zolnie
// polished doxygen documentation
//
// Revision 1.4  2004/01/26 22:54:15  zolnie
// after name cleanup
//
// Revision 1.3  2004/01/26 21:51:53  zolnie
// shorter names
//
// Revision 1.2  2004/01/26 21:08:31  zolnie
// working track/tower display (before big farewell cleanup)
//
// Revision 1.1  2004/01/19 22:07:50  zolnie
// toward track/tower display
//

