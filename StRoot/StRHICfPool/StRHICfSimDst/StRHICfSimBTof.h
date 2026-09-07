#ifndef StRHICfSimBTof_HH
#define StRHICfSimBTof_HH

#include "TObject.h"

class StRHICfSimBTof : public TObject
{
    public: 
        StRHICfSimBTof();
        ~StRHICfSimBTof();

        void Clear(Option_t *option = "");

        void SetSimBTof(bool isVPD, int tray, int module, int cell, int simTrkId);

        void SetIsVPD();
        void SetTray(int idx);
        void SetModule(int idx);
        void SetCell(int idx);
        void SetSimTrkId(int idx);

        Bool_t IsVPD(); // is VPD cell
        Int_t GetTray(); 
        Int_t GetModule(); 
        Int_t GetCell();
        Int_t GetGlobalCell(); // global cell index for all B-TOF tower
        Int_t GetSimTrkId(); // Hitted in BTOF SimTrack id

    private:
        Bool_t mIsVPD;
        UChar_t mTray; // StMuBTofHit tray()
        UChar_t mModule; // StMuBTofHit module()
        UChar_t mCell;  // StMuBTofHit cell()
        Int_t mSimTrkId;  // StMuBTofHit idTruth()


    ClassDef(StRHICfSimBTof,1)
};

#endif
