/***************************************************************************
 *
 * $Id: StTpcHitMaker.cxx,v 1.15 2009/03/16 13:41:45 fisyak Exp $
 *
 * Author: Valeri Fine, BNL Feb 2007
 ***************************************************************************
 *
 * Description:  Fill the StEvent from the DAQ clusters
 *               
 * Input:  DAQReader
 * Output: StTpcHit collection added to StEvent
 *
 ***************************************************************************
 *
 * $Log: StTpcHitMaker.cxx,v $
 * Revision 1.15  2009/03/16 13:41:45  fisyak
 * Switch to new scheme (avoid legacy) for TPX cluster reading
 *
 * Revision 1.14  2009/03/11 18:38:20  fisyak
 * Add 22 time bins to account subtracted by Tonko, clean up
 *
 * Revision 1.13  2009/02/20 22:06:15  fisyak
 * Restore access to TPX
 *
 * Revision 1.12  2008/12/29 23:58:06  fine
 * Optimize the DAQ data access
 *
 * Revision 1.11  2008/12/29 21:14:41  fine
 * Sort out  the tps/tpc data handling
 *
 * Revision 1.10  2008/12/29 18:23:48  fine
 * avoid using the dummy data
 *
 * Revision 1.9  2008/12/18 20:20:25  fine
 * access two different detectors tpx/tpc
 *
 * Revision 1.8  2008/12/17 23:27:04  fine
 * Clean up
 *
 * Revision 1.7  2008/12/17 23:26:00  fine
 * Adjust the sector number
 *
 * Revision 1.6  2008/12/17 02:04:28  fine
 * fix the sector number to make the new interface happy
 *
 * Revision 1.5  2008/12/16 20:43:25  fine
 * add the DAQ_READER compliant access to the tpx sector
 *
 * Revision 1.4  2008/12/15 21:04:01  fine
 * For for the NEW_DAQ_READER
 *
 * Revision 1.3  2008/07/31 20:45:26  fisyak
 * Add TpcMixer
 *
 * Revision 1.2  2008/06/23 20:13:53  fisyak
 * Add real data pixel annotation
 *
 * Revision 1.1.1.1  2008/05/27 14:22:41  fisyak
 * Maker to access TPC DAQ information via EVP_READER
 *
 * Revision 1.3  2008/05/27 14:18:18  fisyak
 * Freeze before moving to STAR repository
 *
 * Revision 1.2  2008/04/28 14:37:15  fisyak
 * Rearrage TpcHitMaker to make it run for parallel taks, add the first version of online clustering
 *
 * Revision 1.1.1.1  2008/04/03 20:16:41  fisyak
 * Initial version
 *
 * Revision 1.9  2008/01/29 02:44:38  fine
 * INFO
 *
 * Revision 1.8  2008/01/29 02:42:31  fine
 * remove 16th sector constarin. EVP_READER can read all of them alone now.
 *
 * Revision 1.7  2008/01/28 23:48:39  fine
 * use the new base class
 *
 * Revision 1.6  2008/01/10 01:12:49  fine
 *  makr to use the full TPC + TPX
 *
 * Revision 1.5  2008/01/09 15:16:48  fine
 * Correct the sector number
 *
 * Revision 1.4  2008/01/09 00:43:29  fine
 * Working version. It can be used as the protopty for anither maker that calles RTS_READER to fill the 16-th TPX sector
 *
 * Revision 1.3  2008/01/07 19:04:07  fine
 * Add the  interface to access the DAQ clusters
 *
 * Revision 1.2  2008/01/07 17:37:39  fine
 * check for tpcHitCollection and new StTpcHit object
 *
 * Revision 1.1  2008/01/04 17:52:32  fine
 * New maker to populate the StEvent from the tpc structure filled by the new EVP_READER package
 *
 *
 * StTpcHitMaker - class to fille the StEvewnt from DAQ reader
 *
 **************************************************************************/
#include <assert.h>
#include "StTpcHitMaker.h"

#include "TDataSetIter.h"
#include "StDAQMaker/StDAQReader.h"
#include "TError.h"
#include "string.h"
#include "StEvent.h"
#include "StEvent/StTpcHitCollection.h"
#include "StEvent/StTpcHit.h"
#include "StTpcRawData.h"
#include "StThreeVectorF.hh"

#include "StDaqLib/TPC/trans_table.hh"
#include "StRtsTable.h"
#include "StDbUtilities/StCoordinates.hh"
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TH2.h"
static TNtuple *pulserP = 0;
Float_t StTpcHitMaker::fgDp    = .1;             // hardcoded errors
Float_t StTpcHitMaker::fgDt    = .2;
Float_t StTpcHitMaker::fgDperp = .1;
#include "St_tpc_cl.h"
TableClassImpl(St_tpc_cl,tcl_cl);
#include "St_daq_cld.h"
TableClassImpl(St_daq_cld,tcl_cl);
#include "St_daq_sim_cld.h"
TableClassImpl(St_daq_sim_cld,tcl_cl);
#include "St_daq_adc_tb.h"
TableClassImpl(St_daq_adc_tb,daq_adc_tb);
#include "St_daq_sim_adc_tb.h"
TableClassImpl(St_daq_sim_adc_tb,daq_sim_adc_tb);
ClassImp(StTpcHitMaker);
//_____________________________________________________________
Int_t StTpcHitMaker::Init() {
  LOG_INFO << "StTpcHitMaker::Init as"  << GetName() << endm;
  const Char_t *Names[kAll] = {"undef",
			       "tpc_hits","tpx_hits",
			       "TpcPulser","TpxPulser",
			       "TpcPadMonitor","TpxPadMonitor",
			       "TpcDumpPxls2Nt","TpxDumpPxls2Nt",
			       "TpxRaw","TpxRaw"};
  TString MkName(GetName());
  for (Int_t k = 1; k < kAll; k++) {
    if (MkName.CompareTo(Names[k],TString::kIgnoreCase) == 0) {kMode = (EMode) k; break;}
  }
  assert(kMode);
  return kStOK ;
}
//_____________________________________________________________
Int_t StTpcHitMaker::Make() {
  Int_t minSector = 1;
  Int_t maxSector = 24;
  if (IAttr("minSector")) minSector = IAttr("minSector");
  if (IAttr("maxSector")) maxSector = IAttr("maxSector");
  for (Int_t sector = minSector; sector <= maxSector; sector++) {
    // invoke tpcReader to fill the TPC DAQ sector structure
    TString cldadc("cld");
    if ( kMode==kTpxRaw ) cldadc = "adc";
    mQuery = Form("tpx/%s[%i]",cldadc.Data(),sector);
    StRtsTable *daqTpcTable = GetNextDaqElement(mQuery);
    if (daqTpcTable) {
      kReaderType = kStandardTpx;
    } else {
      mQuery = Form("tpc/legacy[%i]",sector);
      daqTpcTable = GetNextDaqElement(mQuery);
      if (daqTpcTable) {
	kReaderType = kLegacyTpc;
      } else {
	mQuery = Form("tpx/legacy[%i]",sector);
	daqTpcTable = GetNextDaqElement(mQuery);
	if (daqTpcTable) {
	  kReaderType = kLegacyTpx;
	}
      }
    }
    assert(Sector() == sector);
    while (daqTpcTable) {
      Int_t row = daqTpcTable->Row();
      if (row > 0 && row <= 45) {
	fTpc = 0;
	if (kReaderType == kLegacyTpx || kReaderType == kLegacyTpx) fTpc = (tpc_t*)*DaqDta()->begin();
	switch (kMode) {
	case kTpc: 
	case kTpx:             UpdateHitCollection(sector); break;
	case kTpcPulser:       
	case kTpxPulser:       
	  if (! fTpc)                  break;
	  DoPulser(sector);            break;
	case kTpcPadMonitor:   
	case kTpxPadMonitor:   
	  if (! fTpc)                  break;
	  PadMonitor(sector);          break;
	case kTpcDumpPxls2Nt:  
	case kTpxDumpPxls2Nt:  
	  if (! fTpc)                  break;
	  DumpPixels2Ntuple(sector);   break;
	case kTpcRaw: 
	  if (! fTpc)                  break;
	  RawTpcData(sector);          break;
	case kTpxRaw: 
	  RawTpxData(sector);          break;
	default:
	  break;
	}
      }
      daqTpcTable = GetNextDaqElement(mQuery);
    }
  }
  return kStOK;
}
//_____________________________________________________________
void StTpcHitMaker::UpdateHitCollection(Int_t sector) {
  // Populate StEvent with StTpcHit collection
  StEvent *pEvent = dynamic_cast<StEvent *> (GetInputDS("StEvent"));
  if (Debug()) {LOG_INFO << "StTpcHitMaker::Make : StEvent has been retrieved " <<pEvent<< endm;}
  if (! pEvent) {LOG_INFO << "StTpcHitMaker::Make : StEvent has not been found " << endm; return;}
  StTpcHitCollection *hitCollection = pEvent->tpcHitCollection();
  if ( !hitCollection )  {
    // Save the hit collection to StEvent...if needed
    hitCollection = new StTpcHitCollection();
    pEvent->setTpcHitCollection(hitCollection);
  }
  Int_t NRows = DaqDta()->GetNRows();
  if (NRows <= 0) return;
  Int_t nhitsB = hitCollection->numberOfHits();
  Int_t sec = DaqDta()->Sector();
  Int_t row = DaqDta()->Row();
  if (kReaderType == kLegacyTpc || kReaderType == kLegacyTpx) {
    tpc_t *tpc = (tpc_t *) DaqDta()->GetTable();
    for (Int_t l = 0; l < NRows; tpc++) {
      if ( !tpc->has_clusters )  return;
      for(Int_t row=0;row<45;row++) {
	tpc_cl *c = &tpc->cl[row][0];
	Int_t ncounts = tpc->cl_counts[row];
	for(Int_t j=0;j<ncounts;j++,c++) {
	  if (! c) continue;
	  Int_t iok = hitCollection->addHit(CreateTpcHit(*c,sector,row+1));
	  assert(iok);
	}
      }
    }
  } else {// kReaderType == kStandardTpx
    daq_cld *cld = (daq_cld *) DaqDta()->GetTable();
    if (Debug() > 1) {
      LOG_INFO << Form("CLD sec %2d: row %2d: clusters: %3d",sec, row, NRows) << endm;
    }
    for (Int_t l = 0; l < NRows; l++, cld++) {
      if (Debug() > 1) {
	LOG_INFO << Form("    pad %f[%d:%d], tb %f[%d:%d], cha %d, fla 0x%X, Id %d, Q %d ",
			 cld->pad,
			 cld->p1,
			 cld->p2,
			 cld->tb,
			 cld->t1,
			 cld->t2,
			 cld->charge,
			 cld->flags
			 ) << endm;
      }
      if (! cld->pad) continue;
      if (cld->tb >= __MaxNumberOfTimeBins__) continue;
      Int_t iok = hitCollection->addHit(CreateTpcHit(*cld,sector,row));
      assert(iok);
    }
  }
  Int_t nhits = hitCollection->numberOfHits();
  if (Debug()) {
    LOG_INFO << " Total hits in Sector : row " << sector << " : " << row << " = " << nhits - nhitsB << endm;
  }
}
//_____________________________________________________________
StTpcHit *StTpcHitMaker::CreateTpcHit(const tpc_cl &cluster, Int_t sector, Int_t row) {
  // Create  an instance of the StTpcHit from the tpcReader data

  Float_t pad  = cluster.p;
  Float_t time = cluster.t;
  if (kReaderType == kLegacyTpx) time += 22; // remove Tonko's offset
  static StTpcCoordinateTransform transform(gStTpcDb);
  static StTpcLocalSectorCoordinate local;
  static StTpcLocalCoordinate global;
  StTpcPadCoordinate padcoord(sector, row, pad, time);
  transform(padcoord,local,kFALSE);
  transform(local,global);
    
  UInt_t hw = 1;   // detid_tpc
  hw += sector << 4;     // (row/100 << 4);   // sector
  hw += row    << 9;     // (row%100 << 9);   // row
  
  Int_t npads = TMath::Abs(cluster.p2 - cluster.p1) + 1;
  hw += (npads   << 15);  // npads
  
  Int_t ntmbk = TMath::Abs(cluster.t2 - cluster.t1) + 1;
  hw += (ntmbk << 22);  // ntmbks...

  static StThreeVector<double> hard_coded_errors(fgDp,fgDt,fgDperp);

  Double_t gain = (row<=13) ? St_tss_tssparC::instance()->gain_in() : St_tss_tssparC::instance()->gain_out();
  Double_t wire_coupling = (row<=13) ? St_tss_tssparC::instance()->wire_coupling_in() : St_tss_tssparC::instance()->wire_coupling_out();
  Double_t q = cluster.charge * ((Double_t)St_tss_tssparC::instance()->ave_ion_pot() * 
				   (Double_t)St_tss_tssparC::instance()->scale())/(gain*wire_coupling) ;

  StTpcHit *hit = new StTpcHit(global.position(),hard_coded_errors,hw,q
            , (unsigned char ) 0  // c
            , (unsigned short) 0  // idTruth=0
            , (unsigned short) 0  // quality=0,
            , (unsigned short) 0  // id =0,
            , cluster.p1 //  mnpad
            , cluster.p2 //  mxpad
            , cluster.t1 //  mntmbk
            , cluster.t2 //  mxtmbk
            , pad
            , time );
//  LOG_INFO << p << " sector " << sector << " row " << row << endm;
  return hit;
}
//_____________________________________________________________
StTpcHit *StTpcHitMaker::CreateTpcHit(const daq_cld &cluster, Int_t sector, Int_t row) {
  // Create  an instance of the StTpcHit from the tpcReader data

  Float_t pad  = cluster.pad;
  Float_t time = cluster.tb;
  static StTpcCoordinateTransform transform(gStTpcDb);
  static StTpcLocalSectorCoordinate local;
  static StTpcLocalCoordinate global;
  StTpcPadCoordinate padcoord(sector, row, pad, time);
  transform(padcoord,local,kFALSE);
  transform(local,global);
    
  UInt_t hw = 1;   // detid_tpc
  hw += sector << 4;     // (row/100 << 4);   // sector
  hw += row    << 9;     // (row%100 << 9);   // row
  
  Int_t npads = TMath::Abs(cluster.p2 - cluster.p1) + 1;
  hw += (npads   << 15);  // npads
  
  Int_t ntmbk = TMath::Abs(cluster.t2 - cluster.t1) + 1;
  hw += (ntmbk << 22);  // ntmbks...

  static StThreeVector<double> hard_coded_errors(fgDp,fgDt,fgDperp);

  Double_t gain = (row<=13) ? St_tss_tssparC::instance()->gain_in() : St_tss_tssparC::instance()->gain_out();
  Double_t wire_coupling = (row<=13) ? St_tss_tssparC::instance()->wire_coupling_in() : St_tss_tssparC::instance()->wire_coupling_out();
  Double_t q = cluster.charge * ((Double_t)St_tss_tssparC::instance()->ave_ion_pot() * 
				   (Double_t)St_tss_tssparC::instance()->scale())/(gain*wire_coupling) ;

  StTpcHit *hit = new StTpcHit(global.position(),hard_coded_errors,hw,q
            , (unsigned char ) 0  // c
            , (unsigned short) 0  // idTruth=0
            , (unsigned short) 0  // quality=0,
            , (unsigned short) 0  // id =0,
            , cluster.p1 //  mnpad
            , cluster.p2 //  mxpad
            , cluster.t1 //  mntmbk
            , cluster.t2 //  mxtmbk
            , pad
            , time );
//  LOG_INFO << p << " sector " << sector << " row " << row << endm;
  return hit;
}
//________________________________________________________________________________
void StTpcHitMaker::DoPulser(Int_t sector) {
  struct Pulser_t {Float_t sector, row, pad, gain, t0, nnoise, noise, npeak;};
  static Char_t *names = "sector:row:pad:gain:t0:nnoise:noise:npeak";
  static Pulser_t Pulser;
  if (! pulserP) {
    TFile *f = GetTFile();
    assert(f);
    f->cd();
    pulserP = new TNtuple("pulserP","Pulser analysis",names);
  }
  Int_t r, p, tb, tbmax;
  Int_t npeak, nnoise;
  if (! fTpc) return;
  if (! fTpc->channels_sector) return;
  for(Int_t row = 1; row <= 45; row++) {
    r = row - 1;
    if (! fTpc->cl_counts[r]) continue;
    for (Int_t pad = 1; pad <= 182; pad++) {
      p = pad - 1;
      Int_t ncounts = fTpc->counts[r][p];
      if (! ncounts) continue;
      static UShort_t adc[512];
      memset (adc, 0, sizeof(adc));
      tbmax = 513;
      UShort_t adcmax = 0;
      for (Int_t i = 0; i < ncounts; i++) {
	tb = fTpc->timebin[r][p][i];
	adc[tb] = log8to10_table[fTpc->adc[r][p][i]]; 
	if (adc[tb] > adcmax) {
	  tbmax = tb;
	  adcmax = adc[tb];
	}
      }
      if (tbmax < 2 || tbmax > 504) continue;
      npeak =  nnoise = 0;
      Int_t i1s = TMath::Max(  0, tbmax - 2);
      Int_t i2s = TMath::Min(511, tbmax + 7);
      Int_t i1  = TMath::Max(0  ,i1s - 20);
      Int_t i2  = TMath::Min(511,i2s + 20);
      Double_t peak = 0;
      Double_t noise = 0;
      Double_t t0 = 0;
      for (Int_t i = i1; i <= i2; i++) {
	if (i >= i1s && i <= i2s) continue;
	nnoise++;
	noise += adc[i];
      }
      if (nnoise) noise /= nnoise;
      for (Int_t i = i1s; i <= i2s; i++) {
	npeak++;
	peak += adc[i] - noise;
	t0   += i*(adc[i] - noise);
      }
      if (peak <= 0) continue;
      t0    /= peak;
      Pulser.sector = sector;
      Pulser.row    = row;
      Pulser.pad    = pad;
      Pulser.gain   = peak;
      Pulser.t0     = t0;
      Pulser.nnoise = nnoise;
      Pulser.noise  = noise;
      Pulser.npeak  = npeak;
      pulserP->Fill(&Pulser.sector);
    }
  }
}
//________________________________________________________________________________
void StTpcHitMaker::PadMonitor(Int_t sector) {
  static TH2F *padMon[24][45];
  static TH2F *pcl[24][45];
  static Bool_t first = kTRUE;
  if (! fTpc) return;
  if (first) {
    first = kFALSE;
    for (Int_t s = 1; s <= 24; s++) 
      for (Int_t r = 1; r <= 45; r++) {
	padMon[s-1][r-1] = new TH2F(Form("padS%02iR%02i",s,r),Form("Pad monitor for sector = %i and row = %i",s,r),
				 512,0,512,182,1,183);
	pcl[s-1][r-1] = new TH2F(Form("clS%02iR%02i",s,r),Form("Cluster monitor for sector = %i and row = %i",s,r),
				 512,0,512,182,1,183);
      }
  } else {
    for (Int_t s = 1; s <= 24; s++) 
      for (Int_t r = 1; r <= 45; r++) {
	padMon[s-1][r-1]->Reset();
	pcl[s-1][r-1]->Reset();
      }
  }
  Int_t nhits = 0;
  Int_t npixels = 0;
  
  Int_t s = sector - 1;
  if (Debug()) PrintSpecial(sector);
  if ( fTpc->has_clusters ) {
    for(Int_t r=0;r<45;r++)        {
      tpc_cl *c = &fTpc->cl[r][0];
      Int_t ncounts = fTpc->cl_counts[r];
      for(Int_t j=0;j<ncounts;j++,c++) {
	Float_t pad  = c->p;
	Float_t time = c->t;
	Float_t q =  c->charge;
	pcl[s][r]->Fill(time,pad,q);
	nhits++;
      }
    }
    LOG_INFO << " Total hits in Sector : " << sector << " = " << nhits << endm;
  }
  //    if (! fTpc->channels_sector) continue;
  for(Int_t r = 0; r < 45; r++) {
    for (Int_t pad = 1; pad <= 182; pad++) {
      Int_t p = pad - 1;
      Int_t ncounts = fTpc->counts[r][p];
      if (! ncounts) continue;
      for (Int_t i = 0; i < ncounts; i++) {
	Int_t tb = fTpc->timebin[r][p][i];
	Float_t adc = log8to10_table[fTpc->adc[r][p][i]]; 
	padMon[s][r]->Fill(tb,pad,adc);
	npixels++;
      }
    }
  }
  LOG_INFO << " Total pixels in Sector : " << sector << " = " << npixels << endm;
}
//________________________________________________________________________________
void StTpcHitMaker::DumpPixels2Ntuple(Int_t sector) {
  struct BPoint_t {
    Float_t sector, row, pad, tb, adc, ped, t0, peak;
  };
  static const Char_t *BName = "sector:row:pad:tb:adc:ped:t0:peak";
  static TNtuple *adcP = 0;
  if (! adcP) {
    assert(GetTFile());
    GetTFile()->cd();
    adcP = new TNtuple("adcP","Pulser ADC",BName);
  }
  static BPoint_t P;
  if (! fTpc) return;
  Int_t r, p, tb, tbmax;
  //  if (! fTpc->channels_sector) return;
  for(Int_t row = 1; row <= 45; row++) {
    r = row - 1;
    for (Int_t pad = 1; pad <= 182; pad++) {
      p = pad - 1;
      Int_t ncounts = fTpc->counts[r][p];
      if (! ncounts) continue;
      static UShort_t adc[512];
      memset (adc, 0, sizeof(adc));
      tbmax = 513;
      UShort_t adcmax = 0;
      for (Int_t i = 0; i < ncounts; i++) {
	tb = fTpc->timebin[r][p][i];
	adc[tb] = log8to10_table[fTpc->adc[r][p][i]]; 
	if (adc[tb] > adcmax) {
	  tbmax = tb;
	  adcmax = adc[tb];
	}
      }
      if (tbmax < 2 || tbmax > 504) continue;
      Int_t npeak = 0, nped = 0;
      Int_t i1s = TMath::Max(  0, tbmax - 2);
      Int_t i2s = TMath::Min(511, tbmax + 7);
      Int_t i1  = TMath::Max(0  ,i1s - 20);
      Int_t i2  = TMath::Min(511,i2s + 20);
      Double_t peak = 0;
      Double_t ped = 0;
      Double_t t0 = 0;
      for (Int_t i = i1; i <= i2; i++) {
	if (i >= i1s && i <= i2s) continue;
	nped++;
	ped += adc[i];
      }
      if (nped) ped /= nped;
      for (Int_t i = i1s; i <= i2s; i++) {
	npeak++;
	peak += adc[i] - ped;
	t0   += i*(adc[i] - ped);
      }
      if (peak <= 0) continue;
      t0    /= peak;
      i1 = (Int_t) TMath::Max(0.,t0 - 20);
      i2 = (Int_t) TMath::Min(511., t0 + 80);
      for (Int_t i = i1; i <= i2; i++) {
	P.sector = sector;
	P.row    = row;
	P.pad    = pad;
	P.tb     = i - t0;
	P.adc    = adc[i];
	P.ped    = ped;
	P.t0     = t0;
	P.peak   = peak;
	adcP->Fill(&P.sector);
      }
    }
  }
}
//________________________________________________________________________________
void StTpcHitMaker::PrintSpecial(Int_t sector) {
  // example usage: calculate total charge and 
  // print occupancy
  Int_t r,p,t ;
  UInt_t adc = 0;
  UChar_t val ;
  if (! fTpc) return;
  if(fTpc->mode==0) {	// normal event
    UInt_t tot_pix = 0 ;
    UInt_t cl_count = 0 ;
    Int_t i ;
    
    for(r=0;r<45;r++) {	// padrow
      for(p=0;p<182;p++) {	// pad
	for(t=0;t<fTpc->counts[r][p];t++) {	
	  val = fTpc->adc[r][p][t] ;										
	  Int_t vali = log8to10_table[val];
	  adc += val ;
	  if(val) tot_pix++ ;
	  if (Debug() > 1) {
	    Int_t timebin = fTpc->timebin[r][p][t] ;
	    printf("%d %d %d %d %d\n",sector,r+1,p+1,timebin,vali) ;
	  }
	}
      }
      
      if(fTpc->has_clusters) {
	cl_count += fTpc->cl_counts[r] ;
      }
      if (Debug() > 1) {
	if(fTpc->has_clusters) {
	  for(i=0;i<fTpc->cl_counts[r];i++) {
	    tpc_cl *c = &fTpc->cl[r][i] ;
	    
	    printf("%d %d %f %f %d %d %d %d %d %d\n",
		   sector,r+1,c->p,c->t,c->charge,c->flags,c->p1,c->p2,c->t1,c->t2) ;
	  }
	}
      }
    }
    LOG_INFO << Form("TPC: Sector %d: occupancy %3d %%, charge %d, pixels %u, clusters %d",sector,
		     (int)(100.0 *((double)fTpc->channels_sector/(double)fTpc->max_channels_sector)),
		     adc,tot_pix,cl_count) << endm;
  }
}
//________________________________________________________________________________
StTpcDigitalSector *StTpcHitMaker::GetDigitalSector(Int_t sector) {
  TDataSet *event = GetData("Event");
  StTpcRawData *data = 0;
  if (! event) {
    data = new StTpcRawData(24);
    event = new TObjectSet("Event", data);
    AddData(event);
  } else data = (StTpcRawData *) event->GetObject();
  assert(data);
  StTpcDigitalSector *digitalSector = data->GetSector(sector);
  if (! digitalSector) {
    digitalSector = new StTpcDigitalSector();
    data->setSector(sector,digitalSector);
  }
  return digitalSector;
}
//________________________________________________________________________________
Int_t StTpcHitMaker::RawTpxData(Int_t sector) {
  memset(ADCs, 0, sizeof(ADCs));
  memset(IDTs, 0, sizeof(IDTs));
  StTpcDigitalSector *digitalSector = 0;
  Int_t some_data = 0;
  Int_t r_old = -1;
  Int_t p_old = -1;
  Int_t Total_data = 0;
  int r=Row() ;	// I count from 1
  if(r==0) return 0 ;	// TPC does not support unphy. rows so we skip em
  r-- ;			// TPC wants from 0
  int p = Pad() - 1 ;	// ibid.
  if (p < 0 || p >= StTpcDigitalSector::numberOfPadsAtRow(r+1)) return 0;
  if (r_old != r || p_old != p) {
    if (some_data) {
      Total_data += some_data;
      some_data = 0;
      if (! digitalSector) digitalSector = GetDigitalSector(sector);
      Int_t ntbold = digitalSector->numberOfTimeBins(r_old+1,p_old+1);
      if (ntbold) {
	LOG_INFO << "digitalSector " << sector 
		 << " already has " << ntbold << " at row/pad " << r_old+1 <<  "/" << p_old+1 << endm;
      }
      digitalSector->putTimeAdc(r_old+1,p_old+1,ADCs,IDTs);
      memset(ADCs, 0, sizeof(ADCs));
      memset(IDTs, 0, sizeof(IDTs));
    }
    r_old = r;
    p_old = p;
  }
  TGenericTable::iterator iword = DaqDta()->begin();
  for (;iword != DaqDta()->end();++iword) {
    daq_adc_tb &daqadc = (*(daq_adc_tb *)*iword);
    int tb   = daqadc.tb;
    int adc  = daqadc.adc;
    ADCs[tb] = adc;
    IDTs[tb] = 65535;
    some_data++ ;	// I don't know the bytecount but I'll return something...
  }
  
  if (some_data) {
    Total_data += some_data;
    some_data = 0;
    if (! digitalSector) digitalSector = GetDigitalSector(sector);
    Int_t ntbold = digitalSector->numberOfTimeBins(r_old+1,p_old+1);
    if (ntbold) {
      LOG_INFO << "digitalSector " << sector 
	       << " already has " << ntbold << " at row/pad " << r_old+1 <<  "/" << p_old+1 << endm;
    }
    digitalSector->putTimeAdc(r_old+1,p_old+1,ADCs,IDTs);
    memset(ADCs, 0, sizeof(ADCs));
    memset(IDTs, 0, sizeof(IDTs));
  }
  return Total_data;
}
//________________________________________________________________________________
Int_t StTpcHitMaker::RawTpcData(Int_t sector) {
  if (! fTpc) return 0;
  memset(ADCs, 0, sizeof(ADCs));
  memset(IDTs, 0, sizeof(IDTs));
  StTpcDigitalSector *digitalSector = 0;
  Int_t Total_data = 0;
  for (Int_t row = 1;  row <= __NumberOfRows__; row++) {
      Int_t r = row - 1;
      for (Int_t pad = 1; pad <= StTpcDigitalSector::numberOfPadsAtRow(row); pad++) {
         Int_t p = pad - 1;
         memset(ADCs, 0, sizeof(ADCs));
         memset(IDTs, 0, sizeof(IDTs));
         Int_t ncounts = fTpc->counts[r][p];
         if (! ncounts) continue;
         for (Int_t i = 0; i < ncounts; i++) {
            Int_t tb = fTpc->timebin[r][p][i];
            ADCs[tb] = log8to10_table[fTpc->adc[r][p][i]]; 
            IDTs[tb] = 65535;
            Total_data++;
         }
         if (! digitalSector) digitalSector = GetDigitalSector(sector);
         Int_t ntbold = digitalSector->numberOfTimeBins(row,pad);
         if (ntbold) {
            LOG_INFO << "digitalSector " << sector 
                     << " already has " << ntbold << " at row/pad " << row <<  "/" << pad << endm;
         }
         digitalSector->putTimeAdc(row,pad,ADCs,IDTs);
      }
    }									
    if (Total_data) {
      LOG_INFO << "Read " << Total_data << " pixels from Sector " << sector << endm;
    }
    return Total_data;
}
