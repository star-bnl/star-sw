#include <iostream>
#include "StMessMgr.h"

#include "StEnumerations.h"
#include "StEvent.h"
#include "StEvent/StTriggerData.h"
#include "StMtdCollection.h"
#include "StMtdHeader.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuMtdHeader.h"
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoEvent/StPicoDst.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoMtdTrigger.h"
#include "tables/St_mtdModuleToQTmap_Table.h"
#include "tables/St_mtdQTSlewingCorr_Table.h"
#include "tables/St_mtdQTSlewingCorrPart2_Table.h"

#include "StMtdTrigUtil.h"

ClassImp(StMtdTrigUtil)

//_____________________________________________________________________________
StMtdTrigUtil::StMtdTrigUtil(const Char_t *name) : StMaker(name)
{
  mRunYear        = -1;
  mStEvent        = NULL;
  mMuDst          = NULL;
  mPicoDst        = NULL;
  mPosCorrToQTtac = kTRUE;
  reset();
}

//_____________________________________________________________________________
void StMtdTrigUtil::reset()
{
  mVpdTacSum      = 0;
  mTF201TriggerBit = 0;

  memset(mTHUBtime,0,sizeof(mTHUBtime));
  memset(mQtTacSum,0,sizeof(mQtTacSum));
  memset(mMT101Tac,0,sizeof(mMT101Tac));
  memset(mMT101Id,0,sizeof(mMT101Id));
  memset(mQtPosTrig,-1,sizeof(mQtPosTrig));
  memset(mQtPosHighestTwo,-1,sizeof(mQtPosHighestTwo));
  memset(mQtTacSumHighestTwo,0,sizeof(mQtTacSumHighestTwo));
}

//_____________________________________________________________________________
StMtdTrigUtil::~StMtdTrigUtil()
{
}

//_____________________________________________________________________________
Int_t StMtdTrigUtil::Init()
{
  return kStOK;
}

//_____________________________________________________________________________
Int_t StMtdTrigUtil::Make()
{
  Int_t iret = kStOK;

  // initilization
  reset();

  // Check the availability of input data
  if(GetInputDS("StEvent"))
    {
      LOG_DEBUG << "Running on StEvent ..." << endm;
      mStEvent = (StEvent*) GetInputDS("StEvent");
      extractTrigInfo((StTriggerData*)mStEvent->triggerData());
    }
  else if(GetMaker("MuDst"))
    {
      LOG_DEBUG << "Running on MuDst ..." << endm;
      StMuDstMaker *muDstMaker = (StMuDstMaker*) GetMaker("MuDst");
      mMuDst = muDstMaker->muDst();
      extractTrigInfo(const_cast<StTriggerData*>(mMuDst->event()->triggerData()));
    }
  else if(GetMaker("picoDst"))
    {
      LOG_DEBUG << "Running on PicoDst ..." << endm;
      StPicoDstMaker *picoDstMaker = (StPicoDstMaker*) GetMaker("picoDst");
      mPicoDst = picoDstMaker->picoDst();
      extractTrigInfo(mPicoDst);
    } 
  else 
    {
      LOG_ERROR << "Input file format unrecongnized ..." << endm;
      return kStErr;
    }
  findFireQT();
  return iret;
}

//_____________________________________________________________________________
void StMtdTrigUtil::extractTrigInfo(StTriggerData *trigData)
{
  // check availability of input data
  if(!trigData) 
    {
      LOG_ERROR << "No trigger data available ..." << endm;
      return;
    }

  int runnumber = -1;
  if(mStEvent)     runnumber = mStEvent->runId();
  else if(mMuDst)  runnumber = mMuDst->event()->runNumber();
  else
    {
      LOG_ERROR << "Invalid input data format ..." << endm;
      return;
    }
  int year = runnumber / 1e6 + 1999;
  if ((runnumber % 1000000) / 1000 >= 273) year += 1;

  // THUB time
  if(mStEvent)
    {
      StMtdCollection *mtdCollection = mStEvent->mtdCollection();
      if(mtdCollection) 
	{
	  StMtdHeader *mtdHeader = mtdCollection->mtdHeader();
	  if(mtdHeader)
	    {
	      mTHUBtime[0] = 25 * (mtdHeader->triggerTime(0)&0xfff);
	      mTHUBtime[1] = 25 * (mtdHeader->triggerTime(1)&0xfff);
	    }
	}
    }
  else
    {
      StMuMtdHeader *muMtdHeader = mMuDst->mtdHeader();
      if(muMtdHeader) 
	{
	  mTHUBtime[0] = 25 * ( muMtdHeader->triggerTime(1) & 0xfff );
	  mTHUBtime[1] = 25 * ( muMtdHeader->triggerTime(2) & 0xfff );
	}
    }

  // VPD TacSum
  mVpdTacSum = trigData->vpdEarliestTDCHighThr(east,0) + trigData->vpdEarliestTDCHighThr(west,0);

  // QT
  Int_t mtd_qt_tac_max = 4095;
  Int_t mtd_qt_tac_min = 100;
  if (runnumber >= 16045067) mtd_qt_tac_min = 80;
  if (runnumber >= 18070005) mtd_qt_tac_min = 200; // change due to new boards
  Int_t mtd_qt_tac_diff_range_abs = 600;
  if (year == 2015) mtd_qt_tac_diff_range_abs = 1023;

  Int_t mtdQTtac[kNQTboard][16];
  Int_t mtdQTadc[kNQTboard][16];
  for(Int_t i=0; i<32; i++)
    {
      Int_t type = (i/4)%2;
      if(year<=2015)
	{
	  if(type==1)
	    {
	      mtdQTtac[0][i-i/4*2-2] = trigData->mtdAtAddress(i,0);
	      mtdQTtac[1][i-i/4*2-2] = trigData->mtdgemAtAddress(i,0);
	      mtdQTtac[2][i-i/4*2-2] = trigData->mtd3AtAddress(i,0);
	      mtdQTtac[3][i-i/4*2-2] = trigData->mtd4AtAddress(i,0);
	    }
	  else
	    {
	      mtdQTadc[0][i-i/4*2] = trigData->mtdAtAddress(i,0);
	      mtdQTadc[1][i-i/4*2] = trigData->mtdgemAtAddress(i,0);
	      mtdQTadc[2][i-i/4*2] = trigData->mtd3AtAddress(i,0);
	      mtdQTadc[3][i-i/4*2] = trigData->mtd4AtAddress(i,0);
	    }
	}
      else
	{
	  for(int im=0; im<kNQTboard; im++)
	    {
	      if(type==0) mtdQTadc[im][i-i/4*2] = trigData->mtdQtAtCh(im+1,i,0);
	      else        mtdQTtac[im][i-i/4*2-2] = trigData->mtdQtAtCh(im+1,i,0);
	    }
	}
    }

  Int_t j[2] = {0, 0};
  Int_t a[2] = {0, 0};
  for(Int_t im=0; im<kNQTboard; im++)
    {
      if(year<=2015 && im>3)     continue;
      for(int i=0; i<8; i++)
	{
	  if(year==2016 && i%2 == 0)  // monitor channel
	    {
	      mQtTacSum[im][i] = 0;
	      continue;
	    }


	  // Apply slewing correction
	  for (Int_t k=0; k<2; k++) 
	    {
	      j[k] = mtdQTtac[im][i*2+k];
	      a[k] = mtdQTadc[im][i*2+k];
	      
	      Int_t slew_bin = -1;
	      if (a[k]>0 && a[k]<=mQTSlewBinEdge[im][i*2+k][0]) 
		{
		  slew_bin = 0;
		}
	      else 
		{
		  for (Int_t l=1; l<8; l++) 
		    {
		      if (a[k] > mQTSlewBinEdge[im][i*2+k][l-1] && a[k]<=mQTSlewBinEdge[im][i*2+k][l]) 
			{
			  slew_bin = l;
			  break;
			}
		    }
		}
	    if (slew_bin >= 0) 
	      {
		j[k] += mQTSlewCorr[im][i * 2 + k][slew_bin];
	      }
	    }
      
	  // no "equal" in online algorithm
	  if (j[0] <= mtd_qt_tac_min || j[0] >= mtd_qt_tac_max ||
	      j[1] <= mtd_qt_tac_min || j[1] >= mtd_qt_tac_max ||
	      fabs(j[0] - j[1]) >= mtd_qt_tac_diff_range_abs) 
	    {
	      mQtTacSum[im][i] = 0;
	      continue;
	    }

	  // Apply position correction
	  Int_t sumTac = j[0] + j[1];
	  if(mPosCorrToQTtac)
	    {
	      Int_t module = mQTtoModule[im][i];
	      if(module<0) sumTac = 0;
	      else sumTac = Int_t( j[0] + j[1] + abs(module-3)*1./8 * (j[0]-j[1]) );
	    }
	  mQtTacSum[im][i] = sumTac;
	}
    }

  // MT101
  for(Int_t i = 0; i < kNQTboard; i++)
    {
      //if(year<=2015 && i>3)     continue;
      int idx = 0;
      if(year==2016) idx = i / 2 * 3 + i % 2 * 16;
      else idx = i * 3;
      mMT101Tac[i][0] = (trigData->mtdDsmAtCh(idx, 0)) + ((trigData->mtdDsmAtCh(idx + 1, 0) & 0x3) << 8);
      mMT101Id[i][0]  = (trigData->mtdDsmAtCh(idx + 1, 0) & 0xc) >> 2;
      mMT101Tac[i][1] = (trigData->mtdDsmAtCh(idx + 1, 0) >> 4) + ((trigData->mtdDsmAtCh(idx + 2, 0) & 0x3f) << 4);
      mMT101Id[i][1]  = (trigData->mtdDsmAtCh(idx + 2, 0) & 0xc0) >> 6;
    }
  
  // TF201
  Int_t dsmBit1 = trigData->dsmTF201Ch(0);
  Int_t dsmBit2 = 0;
  if (year == 2016) dsmBit2 = trigData->dsmTF201Ch(6);
  for (Int_t i = 0; i < 4; i++) 
    {
      for (Int_t j = 0; j < 2; j++) 
	{
	  if (year == 2016)
	    {
	      int qt = i * 2;
	      mTF201TriggerBit |= ((dsmBit1 >> (i * 2 + j + 4)) & 0x1) << (qt * 2 + j);
	      qt = i * 2 + 1;
	      mTF201TriggerBit |= ((dsmBit2 >> (i * 2 + j + 4)) & 0x1) << (qt * 2 + j);
	    }
	  else 
	    {
	      int qt = i;
	      mTF201TriggerBit |= ((dsmBit1 >> (i * 2 + j + 4)) & 0x1) << (qt * 2 + j);
	    }
	} 
    } 
}

//_____________________________________________________________________________
void StMtdTrigUtil::extractTrigInfo(StPicoDst *picoDst)
{
  if(!picoDst)
    {
      LOG_ERROR << "No picoDst files available ... " << endm;
      return;
    }

  StPicoMtdTrigger *mtdTrig = picoDst->mtdTrigger(0);
  if(!mtdTrig) 
    {
      LOG_ERROR << "No StPicoMtdTrigger available ... " << endm;
      return;
    }

  // VPD tac
  mVpdTacSum = mtdTrig->getVpdTacSum();

  // QT
  for(Int_t im=0; im<kNQTboard; im++)
    {
      for(Int_t i=0; i<8; i++)
	{
	  mQtTacSum[im][i] = mtdTrig->getQTtacSum(im+1,i+1);
	}
    }

  // MT101
  for(Int_t i = 0; i < kNQTboard; i++)
    {
      for(Int_t j=0; j<2; j++)
	{
	  mMT101Tac[i][j] = mtdTrig->getMT101Tac(i+1,j);
	  mMT101Id[i][j]  = mtdTrig->getMT101Id(i+1,j);
	}
    }

   // TF201
  mTF201TriggerBit = mtdTrig->getTF201TriggerBit();
}



//_____________________________________________________________________________
void StMtdTrigUtil::findFireQT()
{
  // QT
  Int_t mxq_tacsum[kNQTboard][2];
  Int_t mxq_tacsum_pos[kNQTboard][2];
  for(Int_t i=0; i<kNQTboard; i++)
    {
      for(Int_t j=0; j<2; j++)
	{
	  mxq_tacsum[i][j] = 0; // to reject all zero values
	  mxq_tacsum_pos[i][j] = -1;
	}
    }

  for(Int_t im=0; im<kNQTboard; im++)
    {
      for(Int_t i=0; i<8; i++)
	{
	  Int_t sumTac = mQtTacSum[im][i];

	  if(mxq_tacsum[im][0] < sumTac)
	    {
	      mxq_tacsum[im][1] = mxq_tacsum[im][0];
	      mxq_tacsum[im][0] = sumTac;

	      mxq_tacsum_pos[im][1] = mxq_tacsum_pos[im][0];
	      mxq_tacsum_pos[im][0] = i+1;
	    }
	  else if (mxq_tacsum[im][1] < sumTac)
	    {
	      mxq_tacsum[im][1]  = sumTac;
	      mxq_tacsum_pos[im][1] = i+1;
	    }
	}
      mQtPosHighestTwo[im][0] = mxq_tacsum_pos[im][0];
      mQtPosHighestTwo[im][1] = mxq_tacsum_pos[im][1];
      mQtTacSumHighestTwo[im][0] = mxq_tacsum[im][0];
      mQtTacSumHighestTwo[im][1] = mxq_tacsum[im][1];
    }

  for(Int_t im = 0; im < kNQTboard; im++)
    {
      for(Int_t j=0; j<2; j++)
	{
	  if((mTF201TriggerBit>>(im*2+j))&0x1)
	    {
	      mQtPosTrig[im][j] = mQtPosHighestTwo[im][j];
	    }
	  else
	    {
	      mQtPosTrig[im][j] = -1;
	    }
	}
    }
}

//_____________________________________________________________________________
Bool_t StMtdTrigUtil::isQtFireTrigger(const Int_t qt, const Int_t pos)
{
  if(qt<1) return kFALSE;
  return (pos==mQtPosTrig[qt-1][0] || pos==mQtPosTrig[qt-1][1]);
}

//_____________________________________________________________________________
Bool_t StMtdTrigUtil::isQtHighestTwo(const int qt, const int pos)
{
  if(qt<1) return kFALSE;
  return (pos==mQtPosHighestTwo[qt-1][0] || pos==mQtPosHighestTwo[qt-1][1]);
}

//_____________________________________________________________________________
Int_t StMtdTrigUtil::getHitTimeInQT(const int backleg, const int module)
{
  if(backleg<1 || backleg>30 || module<1 || module>5) return kFALSE;
  return mQtTacSum[mModuleToQT[backleg-1][module-1]-1][mModuleToQTPos[backleg-1][module-1]-1];
}

//_____________________________________________________________________________
Int_t StMtdTrigUtil::getHitTimeDiffToVPDInQT(const int backleg, const int module)
{
  return getHitTimeInQT(backleg,module)/8 - mVpdTacSum/8 + 1024;
}

//_____________________________________________________________________________
Int_t StMtdTrigUtil::getHitTimeDiffToVPDInMT101(const int backleg, const int module)
{
  Int_t qt  = mModuleToQT[backleg-1][module-1];
  Int_t pos =  mModuleToQTPos[backleg-1][module-1];
  Int_t index = -1;
  if(pos==mQtPosHighestTwo[qt-1][0]) index = 0;
  if(pos==mQtPosHighestTwo[qt-1][1]) index = 1;
  if(index==-1) return 0;
  else return (getMT101Tac(qt, index) - mVpdTacSum/8 + 1024);
}

//_____________________________________________________________________________
Bool_t StMtdTrigUtil::isHitFireTrigger(const Int_t backleg, const Int_t module)
{
  if(backleg<1 || backleg>30 || module<1 || module>5) return kFALSE;
  return isQtFireTrigger( mModuleToQT[backleg-1][module-1], mModuleToQTPos[backleg-1][module-1] );
}

//_____________________________________________________________________________
Bool_t StMtdTrigUtil::isHitHighestTwo(const Int_t backleg, const Int_t module)
{
  if(backleg<1 || backleg>30 || module<1 || module>5) return kFALSE;
  return isQtHighestTwo( mModuleToQT[backleg-1][module-1], mModuleToQTPos[backleg-1][module-1]);
}

//_____________________________________________________________________________
Int_t StMtdTrigUtil::InitRun(const Int_t runNumber)
{
  // initialize maps
  memset(mModuleToQT,-1,sizeof(mModuleToQT));
  memset(mModuleToQTPos,-1,sizeof(mModuleToQTPos));
  memset(mQTtoModule,-1,sizeof(mQTtoModule));

  // obtain maps from DB
  LOG_INFO << "Retrieving mtdModuleToQTmap table from database ..." << endm;
  TDataSet *dataset = GetDataBase("Geometry/mtd/mtdModuleToQTmap");
  St_mtdModuleToQTmap *mtdModuleToQTmap = static_cast<St_mtdModuleToQTmap*>(dataset->Find("mtdModuleToQTmap"));
  if(!mtdModuleToQTmap)
    {
      LOG_ERROR << "No mtdModuleToQTmap table found in database" << endm;
      return kStErr;
    }
  mtdModuleToQTmap_st *mtdModuleToQTtable = static_cast<mtdModuleToQTmap_st*>(mtdModuleToQTmap->GetTable());

  for(Int_t i=0; i<gMtdNBacklegs; i++)
    {
      for(Int_t j=0; j<gMtdNModules; j++)
	{
	  Int_t index = i*5 + j;
	  Int_t qt = mtdModuleToQTtable->qtBoardId[index];
	  Int_t channel = mtdModuleToQTtable->qtChannelId[index];
	  mModuleToQT[i][j]    = qt;
	  if(channel<0)
	    {
	      mModuleToQTPos[i][j] = channel;
	    }
	  else
	    {
	      if(channel%8==1) mModuleToQTPos[i][j] = 1 + channel/8 * 2;
	      else             mModuleToQTPos[i][j] = 2 + channel/8 * 2;
	    }
	  if(mModuleToQT[i][j]>0 && mModuleToQTPos[i][j]>0)
	    mQTtoModule[mModuleToQT[i][j]-1][mModuleToQTPos[i][j]-1] = j + 1;
	}
    }

  // online slewing correction for QT board
  memset(mQTSlewBinEdge,-1,sizeof(mQTSlewBinEdge));
  memset(mQTSlewCorr,-1,sizeof(mQTSlewCorr));
  LOG_INFO << "Retrieving mtdQTSlewingCorr table from database ..." << endm;

  dataset = GetDataBase("Calibrations/mtd/mtdQTSlewingCorr");
  if(!dataset)
    {
      LOG_ERROR << "No dataset for mtdQTSlewingCorr found in database" << endm;
      return kStErr;
    }
  St_mtdQTSlewingCorr *mtdQTSlewingCorr = static_cast<St_mtdQTSlewingCorr*>(dataset->Find("mtdQTSlewingCorr"));
  if(!mtdQTSlewingCorr)
    {
      LOG_ERROR << "No mtdQTSlewingCorr table found in database" << endm;
      return kStErr;
    }
  mtdQTSlewingCorr_st *mtdQTSlewingCorrtable = static_cast<mtdQTSlewingCorr_st*>(mtdQTSlewingCorr->GetTable());
  for(int j=0; j<4; j++)
    {
      for(int i=0; i<16; i++)
        {
          for(Int_t k=0; k<8; k++)
            {
              Int_t index = j*16*8 + i*8 + k;
              mQTSlewBinEdge[j][i][k] = (int) mtdQTSlewingCorrtable->slewingBinEdge[index];
              mQTSlewCorr[j][i][k] = (int) mtdQTSlewingCorrtable->slewingCorr[index];
            }
        }
    }

  LOG_INFO << "Retrieving mtdQTSlewingCorrPart2 table from database ..." << endm;
  dataset = GetDataBase("Calibrations/mtd/mtdQTSlewingCorrPart2");
  if(dataset)
    {
      St_mtdQTSlewingCorrPart2 *mtdQTSlewingCorr2 = static_cast<St_mtdQTSlewingCorrPart2*>(dataset->Find("mtdQTSlewingCorrPart2"));
      mtdQTSlewingCorrPart2_st *mtdQTSlewingCorrtable2 = static_cast<mtdQTSlewingCorrPart2_st*>(mtdQTSlewingCorr2->GetTable());
      for(int j=0; j<4; j++)
	{
	  for(int i=0; i<16; i++)
	    {
	      for(Int_t k=0; k<8; k++)
		{
		  Int_t index = j*16*8 + i*8 + k;
		  mQTSlewBinEdge[j+4][i][k] = (int) mtdQTSlewingCorrtable2->slewingBinEdge[index];
		  mQTSlewCorr[j+4][i][k] = (int) mtdQTSlewingCorrtable2->slewingCorr[index];
		}
	    }
	}
    }

  LOG_INFO << "===== End retrieving mtdQTSlewingCorr =====" << endm;

  return kStOK;
}

//
//// $Id: StMtdTrigUtil.cxx,v 1.1 2018/11/29 20:27:27 marr Exp $
//// $Log: StMtdTrigUtil.cxx,v $
//// Revision 1.1  2018/11/29 20:27:27  marr
//// Mtd trigger utility class for analyzing MTD trigger information
////

