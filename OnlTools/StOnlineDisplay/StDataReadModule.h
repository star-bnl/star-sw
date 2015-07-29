// $Id: StDataReadModule.h,v 1.9 2015/07/29 16:53:18 smirnovd Exp $

#ifndef STAR_StDataReadModule
#define STAR_StDataReadModule

/*!
 *                                                                     
 * \class  StDataReadModule
 * \author  Fine
 * \date   2004/11/17
 * \brief  Online display module
 *
 * This commented block at the top of the header file is considered as
 * the class description to be present on the this class Web page. 
 *
 * 
 * StDataReadModule virtual base class for Maker                        
 * Template Maker doing nothing. See README file in StRoot/StDataReadModule
 *
 *
 */                                                                      

#ifndef TModule_H
#include "TModule.h"
#endif


// You may forward declare other classes if you have data-members
// used in pointer-only context by using declaration like
// class St_SomeExternClass;
//
// You do need in such simple case to add the include file
// (and compilation is much faster).

class  StEvpReader;
struct global_track;
class  TColoredAxis;
struct DATAP;
struct L3_P;
class  EventTracker;
class  TDataProvider;
class  St_emcOnlineStatus;
class  Gl3Data;
struct tpc_t;
class  daqReader;
struct btow_t;
class  daq_dta;
class  QObject;
class  StuDraw3DEvent;

class StDataReadModule : public TModule {
   friend class StSteeringModule;
 private:
  // Private method declaration if any

 protected:
    // Protected method if any
    void        DeletePool();
    void        Distribution(global_track *tracks,UInt_t nTracks);
    void        Distribution(Gl3Data *tracks,UInt_t nTracks);
    int         EmcReader();
    int         L3Reader();
    Int_t       MakeEvent();
    Int_t       MakeColor(Double_t energy);

    // Protected data if any
    StEvpReader *fEventPoolReader;
    Bool_t      fNeedRecreate;
    TString      fDaqFileName;
    TString     fLastGoodDaqFileName;
    TString     fMountPoint;      // mount point for network data
    TString     fMountDirectory;  // mount directory 
    int         fEventType;
    int         fEventNumber;  // event number of ZERO for next event
    int         fBadEventCounter;
    int         fGoodEventCounter;
    
    int         fNbins;
    double      fDeLookUp[100];
    double      flookFactor;
    TColoredAxis *fColorAxis; // the axise to present the track coloring schema
    daqReader  *fDataP;
    DATAP      *fDataBuffer;
    float       fBField;   // The magnetic field
    L3_P       *fL3p;      // Buffer to keep the reconstructed tracks
    EventTracker  *fEventTracker; // L3 EVENT TRACKER 
    Gl3Data      *fL3DataProvider; // L3 EVENT builder from EvenmtDisplay packageTRACKER 
    TDataProvider *fSizeProvider;
    TDataProvider *fColorProvider;
    Int_t         fL3TracksOn; //! should we draw tpc track
    Int_t         fEmcHitsOn;  //! should we draw Emc towers
    Int_t         fL3HitsOn;   //! should we draw tpc hits
    St_emcOnlineStatus *fBemcOnlineStatus; // Emc barrel status
    Bool_t        fRecordReady; // The next record has been read
    Int_t         fEmcDataLength; // the lenght of the emc data record;
    TObjectSet   *fTracks;
    TObjectSet   *fHits;
    QObject      *fGuiObject;
    tpc_t  *fTpc;
    btow_t *fBtow;
    daq_dta *fEmc_in;
    Int_t   fLengthBtow;
    StuDraw3DEvent  *fEventDisplay; 
    std::vector<float> fHittxyz;
    std::vector<float> fTrackXYZ;
    Bool_t   fDemo; //< Demo mode: re-open the file after end-of-file
    Bool_t   fRecording; //< The status of the record mode
    Bool_t   fSuspendRecording; //< The suspend recording temporary
    //! time to process the entire Event
    TStopwatch  *fMakeEventTimer; 
    //! Time  to update the image. 
    TStopwatch  *fUpdateTimer; 
    TStopwatch  *fMakeEmcHitsTimer; 
    TStopwatch  *fMakeTracksTimer; 
    TStopwatch  *fMakeTpcHitsTimer; 
    
    void ClearTracks(Option_t *);
    void ClearHits(Option_t *);
    int tpcReader(daqReader *m, int sector );
public: 
    StDataReadModule(const char *name="DataReadModule");
    virtual       ~StDataReadModule();
    virtual void   Clear(Option_t *option="");
    virtual Int_t  Init();
    virtual Int_t  Make();
    StuDraw3DEvent   *Display();

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty TModule::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty TModule::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StDataReadModule.h,v 1.9 2015/07/29 16:53:18 smirnovd Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
public:
  TDataProvider *GetColorProvider() const { return  fColorProvider;}
  TDataProvider *GetSizeProvider() const  { return  fSizeProvider;}
  void  SetDaqFileName(const char *fileName);
  void  SetRunNumber(int runNumber2beOpen);
  void  SetEventNumber(int eventNumber2beRead);
  void  SetMountPoint(const char *mountPoint);
  void  SetDemo(Bool_t on = kTRUE);
  Bool_t Demo() const;
  Bool_t Recording() const;
  const TString &MountPoint() const;
  void  SetMagneticField(float field=0.005);
  Int_t MakeTpcHits();
  Int_t MakeTracks();
  Int_t MakeEmcHits();
  Int_t NextEvent();
  Int_t NextFile();
  Int_t Remake();
  void  SetL3TracksOn(Int_t on = 1)  { fL3TracksOn = on; }
  void  SetL3HitsOn  (Int_t on = 1)  { fL3HitsOn   = on; }
  void  SetEmcHitsOn (Int_t on = 1)  { fEmcHitsOn  = on; }
  void  SetRecording(bool on=true);

  virtual void   SetGuiObject(QObject *gui);
  virtual void   NextEventsSlot(int interval);
  virtual void   StopEvents();
  virtual void   MakeTitle(int ok);
  // ClassDef(StDataReadModule,0)   // 
};

//____________________________________________________________________
inline   void  StDataReadModule::SetDemo(Bool_t on) { fDemo = on; }
//____________________________________________________________________
inline   Bool_t  StDataReadModule::Demo() const { return fDemo; }
//____________________________________________________________________
inline const TString &StDataReadModule::MountPoint() const { return fMountPoint; }
//____________________________________________________________________
inline   void  StDataReadModule::SetMagneticField(float field) {fBField = field;}
//____________________________________________________________________
inline   Bool_t StDataReadModule::Recording() const { return fRecording ;}

#endif
