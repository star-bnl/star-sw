//******************************************************************************
//                                                                            
// StEmcEnergy.h
//
// Authors: Marcia Maria de Moura
//
// Initial version: 2001/12/11
//
//******************************************************************************

/*! \class StEmcEnergy
\author Marcia M. de Moura

This class gets EMC energy from StEvent/StMcEvent and caluclates, for
all valid towers, the hadronic background, neutral hadronic correction,
and electromagnetic transverse energy on EMC

*/
#ifndef StEmcEnergy_H
#define StEmcEnergy_H

#include <TH1.h>
#include <TH2.h>

#include "TObject.h"
#include "SystemOfUnits.h"

enum  evalMetd {evalOff, metd1, metd2};

class StEvent;
class StTrack;
class StMcEvent;
class StMcCalorimeterHit;
class StEmcGeom;
class StEmcFilter;

class StEmcEnergy : public TObject
{
  public:                 
              	StEmcEnergy();    
    	virtual   	~StEmcEnergy();

    	Float_t   getBemcEnergy();                  ///< Return full EMC energy
    	Float_t   getChHadEnergy();                 ///< Return full charged hadronic energy on EMC
    	Float_t   getQ0HadEnergy();                 ///< Return full neutral hadronic energy on EMC
    	Float_t   getEmEnergy();                    ///< Return full electromagnetic energy on EMC
    	Float_t   getBemcEt();                      ///< Return full EMC transverse energy
    	Float_t   getChHadDepEt();                  ///< Return full charged hadronic transverse energy on EMC
    	Float_t   getQ0HadDepEt();                  ///< Return full neutral hadronic transverse energy on EMC
    	Float_t   getEmEt();                        ///< Return full eletromagnetic transverse energy on EMC
        Int_t   getNTracks();                     ///< Return the number of tracks used to subtract hadronic background
 StEmcFilter*   getFilter();                      ///< Return EmcFilter being used
            
    	Float_t   getChHadEnergyInBtow(UInt_t);     ///< Return Charged hadronic energy in one tower               
    	Float_t   getEnergyInBtow(UInt_t);          ///< Return EMC energy in one tower       
      Float_t   getEmEnergyInBtow(UInt_t);        ///< Return electromagnetic energy in one tower  
               
         void   setEval(evalMetd);                ///< Set evaluation method if using geant
         void   setEvent(StEvent*);               ///< Set StEvent pointer
         void   setMcEvent(StMcEvent*);           ///< Set McEvent pointer
         void   setEmcFilter(StEmcFilter*);       ///< Set event filter
         void   setTPCEff(Bool_t);                ///< Apply or don't TPC efficiency correction
         void   setBfield(Float_t);               ///< Set magnetic field
         void   setQ0Factor(Float_t);             ///< Set neutral correction factor
         void   processEvent();                   ///< Process StEvent/StMcEvent information
  
  protected:
            
       Float_t   mBfield;
       Float_t   mQ0Factor;
        Bool_t   mTPCEff;
      evalMetd   mEvalMetd;  
             
      StEvent*   mEvent;
    StMcEvent*   mMcEvent;
  StEmcFilter*   mEmcFilter;
	      Bool_t   mInternalFilter;
            
       Float_t   mEnergyInBtow[4800];
       Float_t   mChHadEnergyInBtow[4800];
       Float_t   mQ0HadEnergyInBtow[4800];
       Float_t   mEmEnergyInBtow[4800];
       Float_t   mEtInBtow[4800];
       Float_t   mChHadEtInBtow[4800];
       Float_t   mQ0EtInBtow[4800];
       Float_t   mEmEtInBtow[4800];
       Float_t   mBemcEnergy;
       Float_t   mChHadEnergy;
       Float_t   mQ0HadEnergy;
       Float_t   mEmEnergy;
       Float_t   mBemcEt;
       Float_t   mChHadEt;
       Float_t   mQ0HadEt;
       Float_t   mEmEt;   
			   Int_t   mNTracks;         
            
       Float_t   mMcChHadEnergyInBtow[4800];
       Float_t   mGeantEmEtInBtow[4800];
       StEmcGeom *mBemcGeom; //!
            
       Float_t   trackEff(StTrack*, UInt_t);
          void   energyInBtow();
          void   chHadEnergyInBtow();
          void   q0HadEnergyInBtow();
          void   emEnergyInBtow();
          void   geantEmEtInBtow(Int_t=0);
                             
          void   mcEnergyInBtow();
               
          void   hadDepFromMcTrackInBtow();
                                       
          void   processEnergy();
       Float_t   dEToTotaldE(StMcCalorimeterHit* , const char* );
  
  ClassDef(StEmcEnergy,1)

};
//------------------------------------------------------------------------------
inline Float_t StEmcEnergy::getBemcEnergy(){ return mBemcEnergy; }
inline Float_t StEmcEnergy::getChHadEnergy(){ return mChHadEnergy; }
inline Float_t StEmcEnergy::getQ0HadEnergy(){ return mQ0HadEnergy; }
inline Float_t StEmcEnergy::getEmEnergy(){ return mEmEnergy; }
inline Float_t StEmcEnergy::getBemcEt(){ return mBemcEt; }
inline Float_t StEmcEnergy::getChHadDepEt(){ return mChHadEt; }
inline Float_t StEmcEnergy::getQ0HadDepEt(){ return mQ0HadEt; }
inline Float_t StEmcEnergy::getEmEt(){ return mEmEt; }
inline   Int_t StEmcEnergy::getNTracks(){ return mNTracks; }

inline void    StEmcEnergy::setEval( evalMetd metd) { mEvalMetd = metd; }
inline void    StEmcEnergy::setEvent(StEvent* event){ mEvent = event; }
inline void    StEmcEnergy::setMcEvent(StMcEvent* mcEvent){ mMcEvent = mcEvent; }
inline void    StEmcEnergy::setTPCEff(Bool_t option)  { mTPCEff = option; }
inline void    StEmcEnergy::setQ0Factor(Float_t factor)  { mQ0Factor = factor; }
inline Float_t StEmcEnergy::getEnergyInBtow(UInt_t towerNdx) { return mEnergyInBtow[towerNdx]; }
inline Float_t StEmcEnergy::getChHadEnergyInBtow(UInt_t towerNdx) { return mChHadEnergyInBtow[towerNdx]; }
inline Float_t StEmcEnergy::getEmEnergyInBtow(UInt_t towerNdx) { return mEmEnergyInBtow[towerNdx]; }
inline StEmcFilter* StEmcEnergy::getFilter() { return mEmcFilter; }

//------------------------------------------------------------------------------
#endif
