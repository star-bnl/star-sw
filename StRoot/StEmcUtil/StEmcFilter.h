/*!\class StEmcFilter
\author Alexandre A. P. Suaide
\author Marcia M. de Moura

This class makes general event selection for EMC data. It also gives
acceptance correction factors because of limited EMC coverage.
This class also can be used to perform StMcEvent and StMcTrack (simulation)
selection in a limited way.

To use this class, just do:

  StEmcFilter *filter = new StEmcFilter();<br>
  filter->FILTERPARAM = value; <br>
  if(filter->Accept(StEvent pointer)) .... event is accepted <br>
  if(filter->Accept(StTrack pointer)) .... track is accepted <br>
  
It is also possible to use this filter to make track identification based on
dE/dX information. To do this just write:

  Float_t mass;
  Int_t geant_id;
  filter->GetTrackId(StTrack pointer, mass, geant_id);
  
The geant_id is obtained based on the dE/dX cuts set in the filter.

The cuts available now are:
  - Event selection
    - EmcPresent = requires EMC data on event (kTRUE, kFALSE). Default value is kTRUE.
    - HaveVertex = requires a valid vertex (kTRUE, kFALSE). Default is kTRUE.
    - HavePrimaries = requires at least one primary track (kTRUE, kFALSE). Default is kTRUE.
    - ZVertexCut = Z Vertex threshold in cm. Default is 20 cm.
    - EmcETotalMin = minimum energy on EMC in GeV. Default is 0.
    - EmcETotalMax = maximum energy on EMC in GeV. Default is 100000.
    - MinMult = minimum multiplicity. Default is 0.
    - MaxMult = maximum multiplicity. Default is 100000.
    - BField = just set the BField value for track projection in tesla. Default is 0.5 tesla.
    - AddTriggeMask(trigger_mask) = adds one trigger mask for trigger selection. Can add up to 20 masks. Default is all trigger masks.
  - Track selection
    - DCACut = DCA cut on track in cm. Default is 300000.
    - PtCut = transverse momentm cut (GeV/c). Default is 0.
    - EtaMin = minimum eta of the track (momentum). Default is -10000.
    - EtaMax = maximum eta of the track (momentum). Default is +10000.
    - FitPoinsCut = number of fit points in the track. Default is 0.
    - MustProjectEmc = requires that the track is projected on a valid EMC tower (kTRUE, kFALSE). Default is kTRUE.
  - dE/dX cuts
    - dEdXScale = scale factor to BetheBlock function to maker proper dE/dX selection. Default is 1.
    - PointsdEdX = minimum number of points to make dE/dX id. Default is 0.
    - dEdXPMax = maximum momentum to make dE/dX id. Default is 1 GeV/c.
    - dEdXCut = minimum dE/dX value to make dE/dX id. Default is 0.
    - dEdXNSigma = maximum number of sigmas to get a valid dE/dX id.
  - StMcTracks (those cuts are applied only for Monte Carlo tracks from StMcEvent)
    - OnlyHadrons = select only hadronic tracks (kTRUE, kFALSE). pi0 is excluded from the list. Default is kTRUE
    - McChargeType = select the McTrack by charge type (kNEGATIVE, kNEUTRAL, kPOSITIVE, kCHARGED, kALL). Default is kALL.
    - McMustProjectEmc = requires that the McTrack is projected on a valid EMC tower (kTRUE, kFALSE). Default is kTRUE.
    - McPtCut = transverse momentm cut (GeV/c). Default is 0.
    - McEtaMin = minimum eta of the McTrack (momentum). Default is -10000.
    - McEtaMax = maximum eta of the McTrack (momentum). Default is +10000.

*/
#ifndef StEmcFilter_HH
#define StEmcFilter_HH
#include "BetheBloch.h"
#include "tables/St_emcRunning_Table.h"
#include "tables/St_smdRunning_Table.h"
#include <iostream.h>

#define NTOWER 4800

class StEvent;
class StTrack;
class StV0Vertex;
class StMcTrack;
class StMcEvent;
class StEmcGeom;
class StEmcPosition;
class StPionPlus;
class StPionMinus;
class StProton;
class StAntiProton;
class StKaonPlus;
class StKaonMinus;
class StElectron;
class StPositron;

enum McCharge   { kNEGATIVE = -1, kNEUTRAL = 0, kPOSITIVE = 1, kCHARGED = 2, kALL = 3 };
enum EmcStatus  { kBAD = 0, kGOOD = 1, kOTHER = 2};

class StEmcFilter: public TObject
{
  private:
    StEmcGeom*      mGeo[4]; //!
    StEmcPosition*  mEmcPosition; //!
    StPionPlus*     mPion; 
    StProton*       mProton;
    StKaonPlus*     mKaon;
    StElectron*     mElectron;
    BetheBloch      mBB;
    
    Float_t         mPtTower[NTOWER];
    Float_t         mETower[NTOWER];
    Int_t           mNTracksTower[NTOWER];


    // status tables
    St_emcRunning*  mBemcRunning;
    St_emcRunning*  mBprsRunning;
    St_smdRunning*  mBsmdeRunning;
    St_smdRunning*  mBsmdpRunning;
    
    // event cuts (only ZVertex is applied to McEvent for a while)
    Bool_t          mPrintLog;       
    Bool_t          mEmcPresent;     
    Bool_t          mHaveVertex;     
    Bool_t          mHavePrimaries;  
    Float_t         mZVertexCut;     
    Float_t         mEmcETotalMin;   
    Float_t         mEmcETotalMax;   
    Float_t         mMinMult;        
    Float_t         mMaxMult;        
    Float_t         mBField;         
    UInt_t          mTriggerMask[30];
    UInt_t          mNTriggerMask;
    
    // tracks cuts
    Float_t         mDCACut;         
    Float_t         mPtCut;          
    Float_t         mEtaMin;         
    Float_t         mEtaMax;         
    Int_t           mFitPointsCut;   
    Bool_t          mMustProjectEmc; 
    
    // dE/dX cuts
    Float_t         mdEdXScale;      
    Int_t           mPointsdEdX;     
    Float_t         mdEdXPMax;       
    Float_t         mdEdXCut;        
    Float_t         mdEdXNSigma;  
    
    // V0 Vertex cuts
    Float_t         mV0Pt;
    Bool_t          mV0TrackProjectOnEmc;
    
    // EMC tower cuts
    Int_t           mMaxTracksPerTower;
    Float_t         mEMin;
    Float_t         mEMax;
    Float_t         mPtMin;
    Float_t         mPtMax;
    Bool_t          mPNeighbor;
    Bool_t          mSNeighbor;
    Bool_t          mTNeighbor;  
    
    // MC Tracks cuts 
    Bool_t          mOnlyHadrons;        
    McCharge        mMcChargeType;       
    Bool_t          mMcMustProjectEmc;   
    Float_t         mMcPtCut;            
    Float_t         mMcEtaMin;           
    Float_t         mMcEtaMax;           
    
    void            msg(char* a,char* b) {if(mPrintLog) cout <<"StEmcFiletr:: "<<a<<" rejected: "<<b<<endl; }
    
  public:
                StEmcFilter();                        ///< StEmcFilter constructor
                ~StEmcFilter();                       ///< StEmcFilter destructor

    void        initEmcTowers(StEvent*);              ///< Use this function before using accept() and getNTracksTower() methods for towers. This initializes all vectors necessary to these methods.
                
    Bool_t      accept(StEvent*);                     ///< Returns kTRUE/kFALSE if StEvent is accepted
    Bool_t      accept(StTrack*);                     ///< Returns kTRUE/kFALSE if StTrack is accepted
    Bool_t      accept(StMcEvent*);                   ///< Returns kTRUE/kFALSE if StMcEvent is accepted
    Bool_t      accept(StMcTrack*);                   ///< Returns kTRUE/kFALSE if StMcTrack is accepted
    Bool_t      accept(StV0Vertex*);                  ///< Returns kTRUE/kFALSE if StV0vertex is accepted
    Bool_t      accept(Int_t);                        ///< Returns kTRUE/kFALSE if tower with correspondent id is accepted;
    Bool_t      accept(Int_t,Int_t,Int_t);            ///< Returns kTRUE/kFALSE if tower with correspondent module, eta and sub is accepted;
    
    Int_t       getNTracksTower(Int_t);               ///< Returns number of tracks pointing to the tower with given id
    Int_t       getNTracksTower(Int_t,Int_t,Int_t);   ///< Returns number of tracks pointing to the tower with given module, eta, sub
    
    EmcStatus   getEmcStatus(Int_t,Int_t);            ///< Return EMC status (kGOOD, kBAD, kOTHER) for a given detector and bin.
        
    Bool_t      getTrackId(StTrack*,Float_t&,Int_t&); ///< Return track id based on dE/dX
    Float_t     getEmcETotal(StEvent*);               ///< Return total energy on EMC
    
    Float_t     getFraction(Int_t,Int_t,Int_t=0);     ///< Return fraction of EMC live on a given detector and eta bin
    Float_t     getWestFraction(Int_t=1);             ///< Return fraction of EMC live on west side of STAR
    Float_t     getEastFraction(Int_t=1);             ///< Return fraction of EMC live on east side of STAR
    Float_t     getTotalFraction(Int_t=1);            ///< Return fraction of EMC live on STAR
    
    void        printCuts();                          ///< Print filter parameters
    
    void        setBemcStatus(St_emcRunning* st)  { mBemcRunning = st; }   ///< Set tower status (need to get the table from database)
    void        setBprsStatus(St_emcRunning* st)  { mBprsRunning = st; }   ///< Set pre-shower status (need to get the table from database)
    void        setBsmdeStatus(St_smdRunning* st) { mBsmdeRunning = st; }  ///< Set SMD-eta status (need to get the table from database)
    void        setBsmdpStatus(St_smdRunning* st) { mBsmdpRunning = st; }  ///< Set SMD-phi status (need to get the table from database)
    
    // event cuts (only ZVertex is applied to McEvent for a while)
    void        setPrintLog(Bool_t a)             { mPrintLog = a; }       ///< Print log if event is rejected.
    void        setEmcPresent(Bool_t a)           { mEmcPresent = a; }     ///< requires EMC data on event (kTRUE, kFALSE). Default value is kTRUE.
    void        setHaveVertex(Bool_t a)           { mHaveVertex = a; }     ///< requires a valid vertex (kTRUE, kFALSE). Default is kTRUE.
    void        setHavePrimaries(Bool_t a)        { mHavePrimaries = a; }  ///< requires at least one primary track (kTRUE, kFALSE). Default is kTRUE.
    void        setZVertexCut(Float_t a)          { mZVertexCut = a; }     ///< Z Vertex threshold in cm. Default is 20 cm.
    void        setEmcETotalMin(Float_t a)        { mEmcETotalMin = a; }   ///< minimum energy on EMC in GeV. Default is 0.
    void        setEmcETotalMax(Float_t a)        { mEmcETotalMax = a; }   ///< maximum energy on EMC in GeV. Default is 100000.
    void        setMinMult(Float_t a)             { mMinMult = a; }        ///< minimum multiplicity. Default is 0.
    void        setMaxMult(Float_t a)             { mMaxMult = a; }        ///< maximum multiplicity. Default is 100000.
    void        setBField(Float_t a)              { mBField = a; }         ///< just set the BField value for track projection in tesla. Default is 0.5 tesla.
    void        addTriggerMask(UInt_t mask)       { mTriggerMask[mNTriggerMask++] = mask; } ///< adds one trigger mask for trigger selection. Can add up to 20 masks. Default is all trigger masks.
    void        clearTriggerMask()                { mNTriggerMask = 0; }   ///< Resets trigger masks.
    
    // tracks cuts
    void        setDCACut(Float_t a)              { mDCACut = a; }         ///< DCA cut on track in cm. Default is 300000.
    void        setPtCut(Float_t a)               { mPtCut = a; }          ///< transverse momentm cut (GeV/c). Default is 0.
    void        setEtaMin(Float_t a)              { mEtaMin = a; }         ///< minimum eta of the track (momentum). Default is -10000.
    void        setEtaMax(Float_t a)              { mEtaMax = a; }         ///< maximum eta of the track (momentum). Default is +10000.
    void        setFitPointsCut(Int_t a)          { mFitPointsCut = a; }   ///< number of fit points in the track. Default is 0.
    void        setMustProjectEmc(Bool_t a)       { mMustProjectEmc = a; } ///< requires that the track is projected on a valid EMC tower (kTRUE, kFALSE). Default is kTRUE.
    
    // dE/dX cuts
    void        setdEdXScale(Float_t a)           { mdEdXScale = a; }      ///< scale factor to BetheBlock function to maker proper dE/dX selection. Default is 1.
    void        setPointsdEdX(Int_t a)            { mPointsdEdX = a; }     ///< minimum number of points to make dE/dX id. Default is 0.
    void        setdEdXPMax(Float_t a)            { mdEdXPMax = a; }       ///< maximum momentum to make dE/dX id. Default is 1 GeV/c.
    void        setdEdXCut(Float_t a)             { mdEdXCut = a; }        ///< minimum dE/dX value to make dE/dX id. Default is 0.
    void        setdEdXNSigma(Float_t a)          { mdEdXNSigma = a; }     ///< maximum number of sigmas to get a valid dE/dX id.
    
    // V0 Vertex cuts
    void        setV0Pt(Float_t a)                { mV0Pt = a; }                ///< Set minimum pt for V0
    void        setV0TrackProjectOnEmc(Bool_t a)  { mV0TrackProjectOnEmc = a; } ///< At least one of the tracks should project on EMC patch. Default is kTRUE.

    // EMC tower cuts
    void        setMaxTracksPerTower(Int_t a)     { mMaxTracksPerTower = a; }   ///< maximum number of tracks projected in tower
    void        setEMin(Float_t a)                { mEMin = a; }                ///< minimum energy in the tower
    void        setEMax(Float_t a)                { mEMax = a; }                ///< maximum energy in the tower
    void        setPtMin(Float_t a)               { mPtMin = a; }               ///< minimum pt sum from tracks in the tower
    void        setPtMax(Float_t a)               { mPtMax = a; }               ///< maximum pt sum from tracks in the tower
    void        setPNeighbor(Bool_t a)            { mPNeighbor = a; }           ///< Primary neighbors clear (3x3). Default is kTRUE
    void        setSNeighbor(Bool_t a)            { mSNeighbor = a; }           ///< Seconday neighbors clear (5x5). Default is kFALSE
    void        setTNeighbor(Bool_t a)            { mTNeighbor = a; }           ///< Terciary neighbors clear (7x7). Default is kFALSE
    
    // MC Tracks cuts 
    void        setOnlyHadrons(Bool_t a)          { mOnlyHadrons = a; }        ///< select only hadronic tracks (kTRUE, kFALSE). pi0 is excluded from the list. Default is kTRUE
    void        setMcChargeType(McCharge a)       { mMcChargeType = a; }       ///< select the McTrack by charge type (kNEGATIVE, kNEUTRAL, kPOSITIVE, kCHARGED, kALL). Default is kALL.
    void        setMcMustProjectEmc(Bool_t a)     { mMcMustProjectEmc = a; }   ///< requires that the McTrack is projected on a valid EMC tower (kTRUE, kFALSE). Default is kTRUE.
    void        setMcPtCut(Float_t a)             { mMcPtCut = a; }            ///< transverse momentm cut (GeV/c). Default is 0.
    void        setMcEtaMin(Float_t a)            { mMcEtaMin = a; }           ///< minimum eta of the McTrack (momentum). Default is -10000.
    void        setMcEtaMax(Float_t a)            { mMcEtaMax = a; }           ///< maximum eta of the McTrack (momentum). Default is +10000.
    

    ClassDef(StEmcFilter,1)

};
#endif
