#ifndef __StEEmcElement_h__
#define __StEEmcElement_h__

#include <TObject.h>
#include <TString.h>

class StEmcRawHit;

class StEEmcElement : public TObject {

 public:

  StEEmcElement();
  ~StEEmcElement(){ /* nada */ };

  /// Set the raw ADC for this element
  void raw(Float_t r);
  /// Set the pedestal-subtracted ADC for this element
  void adc(Float_t a);
  /// Set the energy (adc-ped+0.5)/gain for this element
  void energy(Float_t e);
  /// Set a status bit for this element
  void stat(unsigned s);
  /// Set a fail bit for this element
  void fail(unsigned f);
  /// Set the name for this element
  void name(const Char_t *n);

  /// Return raw ADC for this element
  Float_t raw();
  /// Return raw ADC for this element
  Float_t raw() const;

  /// Return the pedestal-subtracted ADC for this element
  Float_t adc();
  /// Return the pedestal-subtracted ADC for this element
  Float_t adc() const;

  /// Return the EM energy (towers) or energy deposit for this element (SMD,pre,post).  If a fail bit is set, will return 0.
  Float_t energy();
  /// Return the EM energy (towers) or energy deposit for this element (SMD,pre,post).  If a fail bit is set, will return 0.
  Float_t energy() const;

  /// Return the status bit for this element
  unsigned stat();
  /// Return the fail bit for this element
  unsigned fail();
  /// Return the name of this element
  const Char_t *name();

  void stemc( StEmcRawHit *h );
  StEmcRawHit *stemc();

  void Clear(Option_t *opts="");

 private:
 protected:

  Float_t mRaw;    // Raw ADC
  Float_t mAdc;    // Ped subtracted ADC
  Float_t mEnergy; // EM energy/energy deposit
  unsigned mStat;  // detector problems
  unsigned mFail;  // fatal detector problems
  TString  mName;  // Name of this element

  StEmcRawHit *mstRawHit; //! raw hit from StEvent, used for providing StEmcClusters 
  
  ClassDef(StEEmcElement,1);

};

inline void StEEmcElement::raw(Float_t r){ mRaw=r; }
inline void StEEmcElement::adc(Float_t a){ mAdc=a; }
inline void StEEmcElement::energy(Float_t e){ mEnergy=e; }
inline void StEEmcElement::stat(unsigned s){ mStat=s; }
inline void StEEmcElement::fail(unsigned f){ mFail=f; }
inline void StEEmcElement::name(const Char_t *n){ mName=n; }

inline Float_t StEEmcElement::raw(){ return mRaw; }
inline Float_t StEEmcElement::raw() const { return mRaw; }

inline Float_t StEEmcElement::adc(){ return mAdc; }
inline Float_t StEEmcElement::adc() const { return mAdc; }

inline Float_t StEEmcElement::energy(){ return (mFail)?0.:mEnergy; }
inline Float_t StEEmcElement::energy() const { return (mFail)?0.:mEnergy; }

inline unsigned StEEmcElement::stat(){ return mStat; }
inline unsigned StEEmcElement::fail(){ return mFail; }
inline const Char_t *StEEmcElement::name() { return mName.Data(); }

inline void StEEmcElement::stemc( StEmcRawHit *h ){ mstRawHit=h; }
inline StEmcRawHit *StEEmcElement::stemc(){ return mstRawHit; }

#endif
