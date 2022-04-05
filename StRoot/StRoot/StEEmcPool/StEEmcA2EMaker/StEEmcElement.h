#ifndef __StEEmcElement_h__
#define __StEEmcElement_h__

#include <TObject.h>
#include <TString.h>

class StEmcRawHit;

class StEEmcElement : public TObject {

public:

  StEEmcElement();
  virtual ~StEEmcElement(){ /* nada */ };

  /// Set the raw ADC for this element
  void raw(Float_t r) { mRaw=r; }
  /// Set the pedestal-subtracted ADC for this element
  void adc(Float_t a) { mAdc=a; }
  /// Set the energy (adc-ped+0.5)/gain for this element
  void energy(Float_t e) { mEnergy=e; }
  /// Set a status bit for this element
  void stat(unsigned s) { mStat=s; }
  /// Set a fail bit for this element
  void fail(unsigned f) { mFail=f; }
  /// Set the name for this element
  void name(const Char_t *n) { mName=n; }

  /// Return raw ADC for this element
  Float_t raw() const { return mRaw; }
  /// Return the pedestal-subtracted ADC for this element
  Float_t adc() const { return mAdc; }
  /// Return the EM energy (towers) or energy deposit for this element (SMD,pre,post).  If a fail bit is set, will return 0.
  Float_t energy() const { return (mFail)?0.:mEnergy; }
  /// Return the status bit for this element
  unsigned stat() const { return mStat; }
  /// Return the fail bit for this element
  unsigned fail() const { return mFail; }
  /// Return the name of this element
  const Char_t *name() const { return mName.Data(); }

  /// Sets pointer to the StEmcRawHit when processing an StEvent file
  void stemc( StEmcRawHit *h ) { mstRawHit=h; }
  /// Returns pointer to StEmcRawHit
  StEmcRawHit *stemc() { return mstRawHit; }
  const StEmcRawHit *stemc() const { return mstRawHit; }

  /// Clears the element
  virtual void Clear(Option_t *opts="");

protected:
  Float_t mRaw;    /**<- Raw ADC */
  Float_t mAdc;    /**<- Ped subtracted ADC */
  Float_t mEnergy; /**<- EM energy/energy deposit */
  unsigned mStat;  /**<- detector problems */ 
  unsigned mFail;  /**<- fatal detector problems */
  TString  mName;  /**<- Name of this element */
  StEmcRawHit *mstRawHit; //! Raw hit from StEvent
  
  ClassDef(StEEmcElement,1);
};

#endif
