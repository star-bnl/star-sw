/*!\class StEmcMicroEvent
\author Alexandre A. P. Suaide

This is the EMC micro Event main structure.
*/
#ifndef StEmcMicroEvent__h
#define StEmcMicroEvent__h
 
#include "TObject.h"
#include "StEmcMicroCollection.h"
#include "StFpdMicroCollection.h"
#include "StEmcMicroTrack.h"
#include "StEmcMicroV0.h"
#include "TObjArray.h" 
 
class StEmcMicroEvent : public TObject 
{
 
 public:
 
                StEmcMicroEvent();
  virtual       ~StEmcMicroEvent();
     
  Int_t         getVersion()                 const { return mVersion; }                            ///< Return EMC Micro Event version

  StEmcMicroCollection* getEmc()             const { return fEmc; }                                ///< Return micro EMC collection
  StFpdMicroCollection* getFpd()             const { return fFpd; }                                ///< Return FPD Information
  
  StEmcMicroTrack* getPrimaryTrack(Int_t i)  const { return (StEmcMicroTrack*)fpTracks->At(i); }   ///< Return Primary track
  Int_t            getNPrimaryTrack()        const { return fpTracks->GetEntries(); }              ///< Return Number of Primary track
  StEmcMicroTrack* getGlobalTrack(Int_t i)   const { return (StEmcMicroTrack*)fgTracks->At(i); }   ///< Return Global track
  Int_t            getNGlobalTrack()         const { return fgTracks->GetEntries(); }              ///< Return Number of Global track
   
  StEmcMicroV0*    getV0(Int_t i)            const { return (StEmcMicroV0*)fV0->At(i); }           ///< Return V0
  Int_t            getNV0()                  const { return fV0->GetEntries(); }                   ///< Return Number of V0
  
  UInt_t        getOrigMult()                const { return mOrigMult; }                           ///< Return Multiplicity (number of primaries before cut) 
  UInt_t        getCentrality()              const { return mCentrality; }                         ///< Return centrality (not defined yet)
  Float_t       getVertexX()                 const { return mVertexX; }                            ///< Return Vertex X
  Float_t       getVertexY()                 const { return mVertexY; }                            ///< Return Vertex Y
  Float_t       getVertexZ()                 const { return mVertexZ; }                            ///< Return Vertex Z
  
  UInt_t        getL0TriggerWord()           const { return mL0TriggerWord; }                      ///< Return L0 trigger word

  Int_t         getEventID()                 const { return mEventID; }                            ///< Return Event Id
  Int_t         getRunID()                   const { return mRunID; }                              ///< Return run number
  Int_t         getEventTime()               const { return mEventTime; }                          ///< Return event time (unix time)
  Int_t         getToken()                   const { return mToken; }                              ///< Return event Token

  Int_t         getCTB()                     const { return mCTB; }                                ///< Return CTB sum

  Int_t         getZDCe()                    const { return mZDCe; }                               ///< Return ZDC east
  Int_t         getZDCw()                    const { return mZDCw; }                               ///< Return ZDC west
  Float_t       getZVertexZDC()              const { return mZVertexZDC; }                         ///< Return Z vertex from ZDC

  Int_t         getBBCe()                    const { return mBBCe; }                               ///< Return BBC east
  Int_t         getBBCw()                    const { return mBBCw; }                               ///< Return BBC west
  Int_t         getBBCSum()                  const { return mBBCe+mBBCw; }                         ///< Return BBC sum
  Int_t         getBBCNHits()                const { return mBBCNHits; }                           ///< Return BBC number of hits
  Float_t       getZVertexBBC()              const { return mZVertexBBC; }                         ///< Return BBC Z Vertex

  Int_t         getHighTower(Int_t i)        const { return (Int_t)mHT[i]; }                       ///< Return EMC High tower ADC
  Int_t         getPatch(Int_t i)            const { return (Int_t)mPATCH[i]; }                    ///< Return EMC Patch ADC
  
  UInt_t        getBunchCrossing7bit()       const { return mBX7bit; }                             ///< Return Bunch Crossing Id 7 bits
  UInt_t        getBunchCrossing()           const { return mBX; }                                 ///< Return Bunch Crossing 
  Int_t         getSpinBits()                const { return mSpinBits; }                           ///< Return Spin bits
  Int_t         getSpinBitYellowUp()         const { return (mSpinBits >> 0) & 0x1; }              ///< Return Spin bit yellow up
  Int_t         getSpinBitYellowDown()       const { return (mSpinBits >> 1) & 0x1; }              ///< Return Spin bit yellow down
  Int_t         getSpinBitBlueUp()           const { return (mSpinBits >> 2) & 0x1; }              ///< Return Spin bit blue up
  Int_t         getSpinBitBlueDown()         const { return (mSpinBits >> 3) & 0x1; }              ///< Return Spin bit blue down
    
  Float_t       getBField()                  const { return mBField; }                             ///< Return Magnetic field (tesla)
      
  void setEmc(StEmcMicroCollection* emc)            { fEmc = emc; }  
  void setFpd(StFpdMicroCollection* fpd)            { fFpd = fpd; }
  void addPrimaryTrack(StEmcMicroTrack *t)          { fpTracks->AddLast(t); }
  void addGlobalTrack(StEmcMicroTrack *t)           { fgTracks->AddLast(t); }
  void addV0(StEmcMicroV0 *v)                       { fV0->AddLast(v); }
   
  void setVersion(const Int_t ver)                  { mVersion = ver; }
    
  void setOrigMult(const UInt_t mult)               { mOrigMult = mult; }
  void setCentrality(const UInt_t cent)             { mCentrality = cent; }
  void setVertexPos(const Float_t x, const Float_t y, const Float_t z) { mVertexX=x; mVertexY=y; mVertexZ=z; }

  void setL0TriggerWord(const UInt_t trigger)       { mL0TriggerWord = trigger; }
  void setEventID(const Int_t id)                   { mEventID = id; }
  void setRunID(const Int_t id)                     { mRunID = id; }
  void setEventTime(const Int_t time)               { mEventTime = time; }
  void setToken(const Int_t token)                  { mToken = token; }

  void setCTB(const Int_t ctb)                      { mCTB  = ctb; }

  void setZDCe(const Int_t zdce)                    { mZDCe = zdce; }
  void setZDCw(const Int_t zdcw)                    { mZDCw = zdcw; }
  void setZVertexZDC(const Float_t ZVZDC)           { mZVertexZDC = ZVZDC; }

  void setBBCe(const Int_t bbce)                    { mBBCe = bbce; }
  void setBBCw(const Int_t bbcw)                    { mBBCw = bbcw; }
  void setBBCNHits(const Int_t BBCNH)               { mBBCNHits = BBCNH; }
  void setZVertexBBC(const Float_t ZVBBC)           { mZVertexBBC = ZVBBC; }
  
  void setHighTower(const Int_t i, const Int_t HT)  { mHT[i] = (Char_t)HT; }
  void setPatch(const Int_t i, const Int_t PATCH)   { mPATCH[i] = (Char_t)PATCH; }
  
  void setBunchCrossing7bit(const UInt_t BX7)       { mBX7bit = BX7; }
  void setBunchCrossing(const UInt_t BX)            { mBX = BX; }
  void setSpinBits(const Int_t Spin)                { mSpinBits = Spin; }
   
  void setBField(const Float_t bf)                  { mBField = bf; }
 
 private:

  void clear(Option_t *option="");
 
  Int_t          mVersion;              ///< Micro EMC Event version

  UInt_t         mOrigMult;             ///< number of tracks
  UInt_t         mCentrality;           ///< centrality bin
  Float_t        mVertexX;              ///< primary vertex position
  Float_t        mVertexY;              ///< primary vertex position
  Float_t        mVertexZ;              ///< primary vertex position
    
  UInt_t         mL0TriggerWord;        ///< L0 Trigger Word
  Int_t          mEventID;              ///< event ID
  Int_t          mRunID;                ///< run ID
  Int_t          mEventTime;            ///< event time
  Int_t          mToken;                ///< event token
  Int_t          mCTB;                  ///< CTB value sum
  Int_t          mZDCe;                 ///< ZDC east
  Int_t          mZDCw;                 ///< ZDC west
  Float_t        mZVertexZDC;           ///< ZDC Z vertex
  Int_t          mBBCe;                 ///< BBC east
  Int_t          mBBCw;                 ///< BBC west
  Int_t          mBBCNHits;             ///< BBC Number of hits
  Float_t        mZVertexBBC;           ///< BBC Z Vertex
  
  Char_t         mHT[300];              ///< EMC High Tower
  Char_t         mPATCH[300];           ///< EMC Patch
    
  UInt_t         mBX7bit;               ///< BunchCrossing 7 bit
  UInt_t         mBX;                   ///< BunchCrossing 
  Int_t          mSpinBits;             ///< SpinBits

  Float_t        mBField;               ///< BField (tesla)
 
  StEmcMicroCollection* fEmc;
  StFpdMicroCollection* fFpd;
  TObjArray*            fpTracks;
  TObjArray*            fgTracks;
  TObjArray*            fV0;
                   
  ClassDef(StEmcMicroEvent,1)
};
 
#endif 
