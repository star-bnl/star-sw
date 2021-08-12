/*
 *
 * \class StFcsEpdQaMaker
 *
 */

#ifndef STAR_StFcsEpdQaMaker_HH
#define STAR_StFcsEpdQaMaker_HH

#include "StRoot/St_base/Stypes.h"
#include "StRoot/StEvent/StEnumerations.h"
#include "StMaker.h"

class StFcsDb;
class StFcsCollection;
class StEpdCollection;
class TH1F;
class TH2F;

class StFcsEpdQaMaker : public StMaker {
 public: 
   StFcsEpdQaMaker(const Char_t* name = "FcsEpdQa");
   virtual ~StFcsEpdQaMaker();
   virtual Int_t Init();
   virtual Int_t Make();
   virtual Int_t Finish();

   void setRun(int v) {mRun = v;}
   void setFilename(char* v) {sprintf(mFilename,"%s",v);}

 protected:

 private:
   StFcsDb *mFcsDb=0;
   StFcsCollection *mFcsCollection=0;
   StEpdCollection *mEpdCollection=0;
   int mRun=0;
   TFile* mFile;
   char mFilename[100];

   TH2F* mQtDepA[15];
   TH2F* mQtDepT[15];
   TH2F* mQtDepR[15];

   ClassDef(StFcsEpdQaMaker,1);
};

#endif

/*
 * $Id: StFcsEpdQaMaker.h,v 1.1 2021/05/30 21:33:05 akio Exp $
 * $Log: StFcsEpdQaMaker.h,v $
 * Revision 1.1  2021/05/30 21:33:05  akio
 * QA for EPD West from DEP and QT comparison
 *
 */
