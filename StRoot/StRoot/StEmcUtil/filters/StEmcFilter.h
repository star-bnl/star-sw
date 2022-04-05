/*!\class StEmcFilter
\author Alexandre A. P. Suaide
\author Marcia M. de Moura

This class makes general event selection for EMC data. It also gives
acceptance correction factors because of limited EMC coverage.
This class also can be used to perform StMcEvent and StMcTrack (simulation)
selection in a limited way.

To use this class, just do:

  StEmcFilter *filter = new StEmcFilter();<br>
  filter->setFILTERPARAM(value); <br>
  if(filter->accept(StEvent*)) .... event is accepted <br>
  if(filter->accept(StTrack*)) .... track is accepted <br>
  filter->initEmcTowers(StEvent*)
  if(filter->accept(module, eta, sub)) .... tower is accepted <br>
  
It is also possible to use this filter to make track identification based on
dE/dX information. To do this just write:

  Float_t mass;
  Int_t geant_id;
  filter->GetTrackId(StTrack pointer, mass, geant_id);
  
The geant_id is obtained based on the dE/dX cuts set in the filter.
*/
#ifndef StEmcFilter_HH
#define StEmcFilter_HH
#include "BetheBloch.h"
#include "tables/St_emcStatus_Table.h"
#include "tables/St_smdStatus_Table.h"
#include <Stiostream.h>

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
    Int_t           mCentralityBin;
    Int_t           mCentrality[10];

    // status tables
    St_emcStatus*   mBemcRunning;
    St_emcStatus*   mBemcRunningOrig;
    St_emcStatus*   mBprsRunning;
    St_smdStatus*   mBsmdeRunning;
    St_smdStatus*   mBsmdpRunning;

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
		Float_t         mPtCutMax;    
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

    void            calcCentrality(StEvent*); 

  public:
                		StEmcFilter(Int_t = 0);               ///< StEmcFilter constructor
                		~StEmcFilter();                       ///< StEmcFilter destructor

    void        		initEmcTowers(StEvent*,Int_t=0);      ///< Use this function before using accept(), getNTracksTower() and getPtTower() methods for towers. This initializes all vectors necessary to these methods.
 
    Bool_t      		accept(StEvent*);                     ///< Returns kTRUE/kFALSE if StEvent is accepted
    Bool_t      		accept(StTrack*);                     ///< Returns kTRUE/kFALSE if StTrack is accepted
    Bool_t      		accept(StMcEvent*);                   ///< Returns kTRUE/kFALSE if StMcEvent is accepted
    Bool_t      		accept(StMcTrack*);                   ///< Returns kTRUE/kFALSE if StMcTrack is accepted
    Bool_t      		accept(StV0Vertex*);                  ///< Returns kTRUE/kFALSE if StV0vertex is accepted
    Bool_t      		accept(Int_t);                        ///< Returns kTRUE/kFALSE if tower with correspondent id is accepted;
    Bool_t      		accept(Int_t,Int_t,Int_t);            ///< Returns kTRUE/kFALSE if tower with correspondent module, eta and sub is accepted;

    Int_t       		getNTracksTower(Int_t);               ///< Returns number of tracks pointing to the tower with given id
    Int_t       		getNTracksTower(Int_t,Int_t,Int_t);   ///< Returns number of tracks pointing to the tower with given module, eta, sub

    Float_t     		getPtTower(Int_t);                    ///< Returns the total pt from projected tracks in tower with id    
    Float_t     		getPtTower(Int_t,Int_t,Int_t);        ///< Returns the total pt from projected tracks in tower with with module, eta, sub    

    EmcStatus   		getEmcStatus(Int_t,Int_t);            ///< Return EMC status (kGOOD, kBAD, kOTHER) for a given detector and bin.

    Int_t           getCentralityBin() { return mCentralityBin; } ///< Return centrality bin
    Int_t           getCentrality(Int_t i) { return mCentrality[i]; } ///< Return centrality definition for a given bin

    Bool_t      		getTrackId(StTrack*,Float_t&,Int_t&); ///< Return track id based on dE/dX
    Bool_t      		getTrackId(StTrack*,Int_t&,Float_t&,Float_t&,Int_t&,Int_t*,Float_t*); ///< Return track id based on dE/dX and nsigma for each particle
    Float_t     		getEmcETotal(StEvent*);               ///< Return total energy on EMC

    Float_t     		getFraction(Int_t,Int_t,Int_t=0);     ///< Return fraction of EMC live on a given detector and eta bin
    Float_t     		getWestFraction(Int_t=1);             ///< Return fraction of EMC live on west side of STAR
    Float_t     		getEastFraction(Int_t=1);             ///< Return fraction of EMC live on east side of STAR
    Float_t     		getTotalFraction(Int_t=1);            ///< Return fraction of EMC live on STAR

    St_emcStatus*   getBemcStatus()          { return mBemcRunning; }   ///< get tower status (need to get the table from database)
    St_emcStatus*   getBprsStatus()          { return mBprsRunning; }   ///< get pre-shower status (need to get the table from database)
    St_smdStatus*   getBsmdeStatus()         { return mBsmdeRunning; }  ///< get SMD-eta status (need to get the table from database)
    St_smdStatus*   getBsmdpStatus()         { return mBsmdpRunning; }  ///< get SMD-phi status (need to get the table from database)

    // get event cuts 
    Bool_t        	getEmcPresent()          { return mEmcPresent; }     ///< requires EMC data on event (kTRUE, kFALSE). Default value is kTRUE.
    Bool_t        	getHaveVertex()          { return mHaveVertex; }     ///< requires a valid vertex (kTRUE, kFALSE). Default is kTRUE.
    Bool_t        	getHavePrimaries()       { return mHavePrimaries; }  ///< requires at least one primary track (kTRUE, kFALSE). Default is kTRUE.
    Float_t       	getZVertexCut()          { return mZVertexCut; }     ///< Z Vertex threshold in cm. Default is 20 cm.
    Float_t       	getEmcETotalMin()        { return mEmcETotalMin; }   ///< minimum energy on EMC in GeV. Default is 0.
    Float_t       	getEmcETotalMax()        { return mEmcETotalMax; }   ///< maximum energy on EMC in GeV. Default is 100000.
    Float_t       	getMinMult()             { return mMinMult; }        ///< minimum multiplicity. Default is 0.
    Float_t       	getMaxMult()             { return mMaxMult; }        ///< maximum multiplicity. Default is 100000.
    Float_t       	getBField()              { return mBField; }         ///< just get the BField value for track projection in tesla. Default is 0.5 tesla.

    // get tracks cuts
    Float_t       	getDCACut()              { return mDCACut; }         ///< DCA cut on track in cm. Default is 300000.
    Float_t       	getPtCut()               { return mPtCut; }          ///< transverse momentm cut (GeV/c). Default is 0.
    Float_t       	getPtCutMax()            { return mPtCutMax; }       ///< maximumum transverse momentm cut (GeV/c). Default is 1000.
    Float_t       	getEtaMin()              { return mEtaMin; }         ///< minimum eta of the track (momentum). Default is -10000.
    Float_t       	getEtaMax()              { return mEtaMax; }         ///< maximum eta of the track (momentum). Default is +10000.
    Int_t         	getFitPointsCut()        { return mFitPointsCut; }   ///< number of fit points in the track. Default is 0.
    Bool_t        	getMustProjectEmc()      { return mMustProjectEmc; } ///< requires that the track is projected on a valid EMC tower (kTRUE, kFALSE). Default is kTRUE.

    // get dE/dX cuts
    Float_t       	getdEdXScale()           { return mdEdXScale; }      ///< scale factor to BetheBlock function to maker proper dE/dX selection. Default is 1.
    Int_t       		getPointsdEdX()          { return mPointsdEdX; }     ///< minimum number of points to make dE/dX id. Default is 0.
    Float_t       	getdEdXPMax()            { return mdEdXPMax; }       ///< maximum momentum to make dE/dX id. Default is 1 GeV/c.
    Float_t       	getdEdXCut()             { return mdEdXCut; }        ///< minimum dE/dX value to make dE/dX id. Default is 0.
    Float_t       	getdEdXNSigma()          { return mdEdXNSigma; }     ///< maximum number of sigmas to get a valid dE/dX id.

    // get V0 Vertex cuts
    Float_t       	getV0Pt()                { return mV0Pt; }                ///< get minimum pt for V0
    Bool_t      		getV0TrackProjectOnEmc() { return mV0TrackProjectOnEmc; } ///< At least one of the tracks should project on EMC patch. Default is kTRUE.

    // get EMC tower cuts
    Int_t       		getMaxTracksPerTower()   { return mMaxTracksPerTower; }   ///< maximum number of tracks projected in tower
    Float_t       	getEMin()                { return mEMin; }                ///< minimum energy in the tower
    Float_t       	getEMax()                { return mEMax; }                ///< maximum energy in the tower
    Float_t       	getPtMin()               { return mPtMin; }               ///< minimum pt sum from tracks in the tower
    Float_t       	getPtMax()               { return mPtMax; }               ///< maximum pt sum from tracks in the tower
    Bool_t        	getPNeighbor()           { return mPNeighbor; }           ///< Primary neighbors clear (3x3). Default is kTRUE
    Bool_t        	getSNeighbor()           { return mSNeighbor; }           ///< Seconday neighbors clear (5x5). Default is kFALSE
    Bool_t        	getTNeighbor()           { return mTNeighbor; }           ///< Terciary neighbors clear (7x7). Default is kFALSE

    // get MC Tracks cuts 
    Bool_t        	getOnlyHadrons()         { return mOnlyHadrons; }        ///< select only hadronic tracks (kTRUE, kFALSE). pi0 is excluded from the list. Default is kTRUE
    McCharge        getMcChargeType()        { return mMcChargeType; }       ///< select the McTrack by charge type (kNEGATIVE, kNEUTRAL, kPOSITIVE, kCHARGED, kALL). Default is kALL.
    Bool_t        	getMcMustProjectEmc()    { return mMcMustProjectEmc; }   ///< requires that the McTrack is projected on a valid EMC tower (kTRUE, kFALSE). Default is kTRUE.
    Float_t       	getMcPtCut()             { return mMcPtCut; }            ///< transverse momentm cut (GeV/c). Default is 0.
    Float_t       	getMcEtaMin()            { return mMcEtaMin; }           ///< minimum eta of the McTrack (momentum). Default is -10000.
    Float_t       	getMcEtaMax()            { return mMcEtaMax; }           ///< maximum eta of the McTrack (momentum). Default is +10000.

    void        		printCuts();                          ///< Print filter parameters

    void        		setBemcStatus(St_emcStatus* st)   { mBemcRunning = st; }   ///< Set tower status (need to get the table from database)
    void        		setBprsStatus(St_emcStatus* st)   { mBprsRunning = st; }   ///< Set pre-shower status (need to get the table from database)
    void        		setBsmdeStatus(St_smdStatus* st)  { mBsmdeRunning = st; }  ///< Set SMD-eta status (need to get the table from database)
    void        		setBsmdpStatus(St_smdStatus* st)  { mBsmdpRunning = st; }  ///< Set SMD-phi status (need to get the table from database)

    // set event cuts (only ZVertex is applied to McEvent for a while)
    void        		setPrintLog(Bool_t a)             { mPrintLog = a; }       ///< Print log if event is rejected.
    void        		setEmcPresent(Bool_t a)           { mEmcPresent = a; }     ///< requires EMC data on event (kTRUE, kFALSE). Default value is kTRUE.
    void        		setHaveVertex(Bool_t a)           { mHaveVertex = a; }     ///< requires a valid vertex (kTRUE, kFALSE). Default is kTRUE.
    void        		setHavePrimaries(Bool_t a)        { mHavePrimaries = a; }  ///< requires at least one primary track (kTRUE, kFALSE). Default is kTRUE.
    void        		setZVertexCut(Float_t a)          { mZVertexCut = a; }     ///< Z Vertex threshold in cm. Default is 20 cm.
    void        		setEmcETotalMin(Float_t a)        { mEmcETotalMin = a; }   ///< minimum energy on EMC in GeV. Default is 0.
    void        		setEmcETotalMax(Float_t a)        { mEmcETotalMax = a; }   ///< maximum energy on EMC in GeV. Default is 100000.
    void        		setMinMult(Float_t a)             { mMinMult = a; }        ///< minimum multiplicity. Default is 0.
    void        		setMaxMult(Float_t a)             { mMaxMult = a; }        ///< maximum multiplicity. Default is 100000.
    void        		setBField(Float_t a)              { mBField = a; }         ///< just set the BField value for track projection in tesla. Default is 0.5 tesla.
    void        		addTriggerMask(UInt_t mask)       { mTriggerMask[mNTriggerMask++] = mask; } ///< adds one trigger mask for trigger selection. Can add up to 20 masks. Default is all trigger masks.
    void        		clearTriggerMask()                { mNTriggerMask = 0; }   ///< Resets trigger masks.

    // set tracks cuts
    void        		setDCACut(Float_t a)              { mDCACut = a; }         ///< DCA cut on track in cm. Default is 300000.
    void        		setPtCut(Float_t a)               { mPtCut = a; }          ///< transverse momentm cut (GeV/c). Default is 0.
    void        		setPtCutMax(Float_t a)            { mPtCutMax = a; }       ///< maximumum transverse momentm cut (GeV/c). Default is 1000.
    void        		setEtaMin(Float_t a)              { mEtaMin = a; }         ///< minimum eta of the track (momentum). Default is -10000.
    void        		setEtaMax(Float_t a)              { mEtaMax = a; }         ///< maximum eta of the track (momentum). Default is +10000.
    void        		setFitPointsCut(Int_t a)          { mFitPointsCut = a; }   ///< number of fit points in the track. Default is 0.
    void        		setMustProjectEmc(Bool_t a)       { mMustProjectEmc = a; } ///< requires that the track is projected on a valid EMC tower (kTRUE, kFALSE). Default is kTRUE.

    // set dE/dX cuts
    void        		setdEdXScale(Float_t a)           { mdEdXScale = a; }      ///< scale factor to BetheBlock function to maker proper dE/dX selection. Default is 1.
    void        		setPointsdEdX(Int_t a)            { mPointsdEdX = a; }     ///< minimum number of points to make dE/dX id. Default is 0.
    void        		setdEdXPMax(Float_t a)            { mdEdXPMax = a; }       ///< maximum momentum to make dE/dX id. Default is 1 GeV/c.
    void        		setdEdXCut(Float_t a)             { mdEdXCut = a; }        ///< minimum dE/dX value to make dE/dX id. Default is 0.
    void        		setdEdXNSigma(Float_t a)          { mdEdXNSigma = a; }     ///< maximum number of sigmas to get a valid dE/dX id.

    // set V0 Vertex cuts
    void        		setV0Pt(Float_t a)                { mV0Pt = a; }                ///< Set minimum pt for V0
    void        		setV0TrackProjectOnEmc(Bool_t a)  { mV0TrackProjectOnEmc = a; } ///< At least one of the tracks should project on EMC patch. Default is kTRUE.

    // set EMC tower cuts
    void        		setMaxTracksPerTower(Int_t a)     { mMaxTracksPerTower = a; }   ///< maximum number of tracks projected in tower
    void        		setEMin(Float_t a)                { mEMin = a; }                ///< minimum energy in the tower
    void        		setEMax(Float_t a)                { mEMax = a; }                ///< maximum energy in the tower
    void        		setPtMin(Float_t a)               { mPtMin = a; }               ///< minimum pt sum from tracks in the tower
    void        		setPtMax(Float_t a)               { mPtMax = a; }               ///< maximum pt sum from tracks in the tower
    void        		setPNeighbor(Bool_t a)            { mPNeighbor = a; }           ///< Primary neighbors clear (3x3). Default is kTRUE
    void        		setSNeighbor(Bool_t a)            { mSNeighbor = a; }           ///< Seconday neighbors clear (5x5). Default is kFALSE
    void        		setTNeighbor(Bool_t a)            { mTNeighbor = a; }           ///< Terciary neighbors clear (7x7). Default is kFALSE

    // set MC Tracks cuts 
    void        		setOnlyHadrons(Bool_t a)          { mOnlyHadrons = a; }        ///< select only hadronic tracks (kTRUE, kFALSE). pi0 is excluded from the list. Default is kTRUE
    void        		setMcChargeType(McCharge a)       { mMcChargeType = a; }       ///< select the McTrack by charge type (kNEGATIVE, kNEUTRAL, kPOSITIVE, kCHARGED, kALL). Default is kALL.
    void        		setMcMustProjectEmc(Bool_t a)     { mMcMustProjectEmc = a; }   ///< requires that the McTrack is projected on a valid EMC tower (kTRUE, kFALSE). Default is kTRUE.
    void        		setMcPtCut(Float_t a)             { mMcPtCut = a; }            ///< transverse momentm cut (GeV/c). Default is 0.
    void        		setMcEtaMin(Float_t a)            { mMcEtaMin = a; }           ///< minimum eta of the McTrack (momentum). Default is -10000.
    void        		setMcEtaMax(Float_t a)            { mMcEtaMax = a; }           ///< maximum eta of the McTrack (momentum). Default is +10000.


    ClassDef(StEmcFilter,0)

};
#endif
