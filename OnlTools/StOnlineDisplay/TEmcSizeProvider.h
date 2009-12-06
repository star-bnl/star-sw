#ifndef STAR_TEmcSizeProvider
#define STAR_TEmcSizeProvider

#include "TDataProvider.h"
#include "TString.h"

class EmcChecker;
class St_emcOnlineStatus;
struct btow_t;

//!  TEmcSizeProvider is class to prpvide the dimensions to visualize the EMC towers
//_____________________________________________________________________________
class TEmcSizeProvider : public TDataProvider {
protected :
      UInt_t   fScale;     /*!  The normalization factor;  */
      Int_t    fIndex;     /*!   Current index;                    */
      UShort_t fThreshold; /*!  Min reported value             */
      EmcChecker   *fEmcChecker;
      unsigned short *fHeader;
      TString   fObjectInfo; /*!  string to be used to label the clicked tower */
      St_emcOnlineStatus * fBemcOnlineStatus;
      Bool_t   fUsePedestals;
    
     //! ReportValue returns the  "size" calculated from the /a val
     /*!
                  ReportValue returns the tower height calcuate  fron the tpwer ADC value
            */
     Int_t ReportValue(UShort_t val);
     
public:
      TEmcSizeProvider(btow_t **src=0, void **available=0,Int_t *len=0);
      virtual char   *GetObjectInfo(Int_t towerId, Int_t py ) const;
      virtual ~TEmcSizeProvider();
      virtual void ComputerScale();
      virtual Int_t NextAttribute();
      void ResetEmcDecoder(int date, int time);
      virtual void ResetCounter();
      inline  void SetScale( UInt_t  scale)  { fScale=scale;    }
      inline  void SetThreshold( UInt_t  cut){ fThreshold =cut; }
      inline  UInt_t GetScale() const        { return fScale;   }
      //!  SetOnlineStatus the pointer to the onlist BEMC  status table
      void SetOnlineStatus( St_emcOnlineStatus *table) {  fBemcOnlineStatus = table; }
      Bool_t UsePedestals() const { return fUsePedestals; }
      void SetUsePedestals(Bool_t on=kTRUE) { fUsePedestals = on; }
};

#endif
