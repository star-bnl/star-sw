/***************************************************************************
 *
 * $Id: StSvtEmbeddingMaker.h,v 1.2 2003/09/10 19:47:37 perev Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Svt Embedding Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtEmbeddingMaker.h,v $
 * Revision 1.2  2003/09/10 19:47:37  perev
 * ansi corrs
 *
 * Revision 1.1  2003/07/31 19:18:09  caines
 * Petrs improved simulation code
 *
 **************************************************************************/
#ifndef STAR_StSvtEmbeddingMaker
#define STAR_StSvtEmbeddingMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif


class TObjectSet;
class StSvtHybridCollection;
class StSvtHybridPixelsC;
class StSvtHybridData;
class StSvtHybridSimData;
class TH2F;
class TFile;

class StSvtEmbeddingMaker : public StMaker
{

  public:

      StSvtEmbeddingMaker(const char* name = "SvtEmbedding");
      virtual ~StSvtEmbeddingMaker();

     virtual Int_t Init();
     virtual Int_t Make();
     virtual Int_t Finish();
 
     void mixData();

     void bookHistograms();
     void fillHistograms();

  private:

    StSvtHybridCollection*               mSimPixelDataColl;   //!
    StSvtHybridCollection*               mRealDataColl;       //!
    StSvtHybridPixelsC*                   mSimPixelData;       //!
    StSvtHybridData*                     mRealData;           //!
    StSvtHybridSimData*                  mSvtEmbeddedData;    //!
    
    TH2F** mDataHist;      //!
    TFile* mFile;          //!

  ClassDef(StSvtEmbeddingMaker,0)

};

#endif


