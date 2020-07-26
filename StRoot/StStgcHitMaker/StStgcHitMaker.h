#ifndef STSTGCHITMAKER
#define STSTGCHITMAKER

#include <stdint.h>
#include "StRTSBaseMaker.h"
#include "TNtuple.h"

class StEvent;
class StStgcCollection;



class StStgcHitMaker: public StRTSBaseMaker {
public:
    StStgcHitMaker( const char* name = "stgc" );
    ~StStgcHitMaker();

        Int_t  Init();
        Int_t  InitRun( Int_t );
        Int_t  FinishRun( Int_t );
        Int_t  Finish();
        Int_t  Make();

        void   processEvent(  );
        void   checkEvent();
        void setDebug( const bool );

		void setWriteStgcTree( bool w ) { write_stgc_tree = w; }
private:
    StEvent*             mEvent;
    Int_t                mRunYear;
    Bool_t               mDebug;
    // TNtuple *ntuple;
    // TTree and data
    bool write_stgc_tree = false;
	TFile * fTree;
    TTree* treeStgc;
    unsigned short rdo, sec, altro, ch, n;
    unsigned short adc[100], tb[100];


    StStgcCollection* GetStgcCollection();




    ClassDef( StStgcHitMaker, 0 )

};

inline void StStgcHitMaker::setDebug( const bool debug )  { mDebug = debug; }


#endif // STSTGCHITMAKER
