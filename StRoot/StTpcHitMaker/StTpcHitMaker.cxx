/***************************************************************************
 *
 * $Id: StTpcHitMaker.cxx,v 1.6 2008/12/17 02:04:28 fine Exp $
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
#if 0 
#include <sys/types.h>
#include <stdio.h>
#endif
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
#ifndef NEW_DAQ_READER
#  include "RTS/src/EVP_READER/tpcReader.h"
#  include "RTS/src/RTS_READER/daq_dta.h"
#  include "RTS/src/EVP_READER/evpReaderClass.h"
#  include "RTS/src/RTS_READER/rts_reader.h"
#  include "RTS/src/RTS_READER/daq_det.h"
#else /* NEW_DAQ_READER */
#  include "StRtsTable.h"
#  include "DAQ_TPC/daq_tpc.h"
#  include "DAQ_READER/daq_dta_structs.h"
#endif /* NEW_DAQ_READER */

#if 0
#  include "RTS/src/EVP_READER/evpReader.hh"
#  include "RTS/include/rtsLog.h"
#  include "RTS/src/DAQ_TPX/daq_tpx.h"
#endif

#include "StDbUtilities/StCoordinates.hh"
#include "StEVPTpcCluser.h"
#include "StDetectorDbMaker/St_tss_tssparC.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TH2.h"

#ifdef NEW_DAQ_READER
#  ifdef tpc
#    error TPC if defined elsewhere
#  else
#    define tpc (*fTpc)
#  endif
#endif /* NEW_DAQ_READER */

static TNtuple *pulserP = 0;
Float_t StTpcHitMaker::fgDp    = .1;             // hardcoded errors
Float_t StTpcHitMaker::fgDt    = .2;
Float_t StTpcHitMaker::fgDperp = .1;
#ifndef NEW_DAQ_READER
evpReader  *StTpcHitMaker::fDaqReader = 0;
rts_reader *StTpcHitMaker::fRtsReader = 0;
#endif /* ! NEW_DAQ_READER */
ClassImp(StTpcHitMaker);
//_____________________________________________________________
Int_t StTpcHitMaker::Init() {
  LOG_INFO << "StTpcHitMaker::Init as"  << GetName() << endm;
  const Char_t *Names[kAll] = {"undef","tpc_hits","TpxPulser","TpxPadMonitor","TpxDumpPxls2Nt","TpxRaw"};
  TString MkName(GetName());
  for (Int_t k = 1; k < kAll; k++) {
    if (MkName.CompareTo(Names[k],TString::kIgnoreCase) == 0) {kMode = (EMode) k; break;}
  }
  assert(kMode);
  return kStOK ;
}
#ifndef NEW_DAQ_READER
//_____________________________________________________________
evpReader *StTpcHitMaker::InitReader() {
  // Init EVP_READER 
  if (!fDaqReader) { 
    StDAQReader *daqReader = 0;
    St_DataSet *dr = GetDataSet("StDAQReader");
    if(dr) daqReader = (StDAQReader *)(dr->GetObject());
    if(daqReader == NULL) {
      LOG_INFO << "StTpcHitMaker::InitReader No daqReader available..." << endm;
    } else {
      fDaqReader = daqReader->getFileReader();
      if(fDaqReader == NULL) {
	LOG_INFO << "StTpcHitMaker::InitRun No DaqReader available..." << endm;
      } else {
	if (Debug()) {
	  LOG_INFO << "StTpcHitMaker::InitReader: "  << fDaqReader << endm;
	}
	fRtsReader = fDaqReader->rts_rr;
      }
    }
  }
  return fDaqReader;
}
#else /* NEW_DAQ_READER */

#endif /* NEW_DAQ_READER */
//_____________________________________________________________
Int_t StTpcHitMaker::MakeSector(Int_t sector) {
  // invoke tpcReader to fill the TPC DAQ sector structure
#ifndef NEW_DAQ_READER
  evpReader *evp = InitReader();
  return  evp ? tpcReader((char *)evp,sector) : 0;
#else /* NEW_DAQ_READER */
  TString sec = Form("legacy[%i]",sector+1); // with this verskon the first sector ==1 !!!
  StRtsTable *daqTpcTable = GetNext(sec);
  if (daqTpcTable) {
     fTpc = (tpc_t*)*DaqDta()->begin();
     assert(Sector() == sector);
  }
  return (Int_t)daqTpcTable;
#endif /* NEW_DAQ_READER */
}
//_____________________________________________________________
Int_t StTpcHitMaker::Make() {
  Int_t minSector = 1;
  Int_t maxSector = 24;
  if (IAttr("TPXOnly")) minSector = maxSector = 16;
  else {
    if (IAttr("minSector")) minSector = IAttr("minSector");
    if (IAttr("maxSector")) maxSector = IAttr("maxSector");
  }
  mStEvent = dynamic_cast<StEvent *> (GetInputDS("StEvent"));
  LOG_INFO << "StTpcHitMaker::Make : StEvent has been retrieved " <<mStEvent<< endm;
  for (Int_t sector = minSector; sector <= maxSector; sector++) {
    if ( MakeSector(sector-1) <= 0) continue;
    switch (kMode) {
    case kTpx:             UpdateHitCollection(sector); break;
    case kTpxPulser:       DoPulser(sector);            break;
    case kTpxPadMonitor:   PadMonitor(sector);          break;
    case kTpxDumpPxls2Nt:  DumpPixels2Ntuple(sector);   break;
    case kTpxRaw:          RawData(sector);             break;
    default:
      break;
    }
  }
  return kStOK;
}
//_____________________________________________________________
void StTpcHitMaker::UpdateHitCollection(Int_t sector) {
  // Populate StEvent with StTpcHit collection
  StTpcHitCollection *hitCollection = GetHitCollection();
  if (hitCollection) {
    Int_t nhitsB = hitCollection->numberOfHits();
    if ( !tpc.has_clusters )  return;
    for(Int_t row=0;row<45;row++) {
      tpc_cl *c = &tpc.cl[row][0];
      Int_t ncounts = tpc.cl_counts[row];
      for(Int_t j=0;j<ncounts;j++,c++) {
	static StEVPTpcCluser daqCluster;
	if( (c->t < 0.1) || (c->p < 0.1)) continue;
	daqCluster.setTpcCl(c);
	Int_t iok = hitCollection->addHit(CreateTpcHit(daqCluster,sector,row+1));
	assert(iok);
      }
    }
    Int_t nhits = hitCollection->numberOfHits();
    LOG_INFO << " Total hits in Sector : " << sector << " = " << nhits - nhitsB << endm;
  }
}
//_____________________________________________________________
StTpcHit *StTpcHitMaker::CreateTpcHit(const StDaqTpcClusterInterface &cluster, Int_t sector, Int_t row) {
  // Create  an instance of the StTpcHit from the tpcReader data

  Float_t pad  = cluster.pad();
  Float_t time = cluster.time();
#if 0 // in EVP_READER now
  if (sector != 16) {
    pad  -= 0.5;
    time -= 0.5;
    
  }
#endif
  static StTpcCoordinateTransform transform(gStTpcDb);
  static StTpcLocalSectorCoordinate local;
  static StTpcLocalCoordinate global;
  StTpcPadCoordinate padcoord(sector, row, pad, time);
  transform(padcoord,local,kFALSE);
  transform(local,global);
    
  UInt_t hw = 1;   // detid_tpc
  hw += sector << 4;     // (row/100 << 4);   // sector
  hw += row    << 9;     // (row%100 << 9);   // row
  
  Int_t npads = TMath::Abs(cluster.maxPad() - cluster.minPad()) + 1;
  hw += (npads   << 15);  // npads
  
  Int_t ntmbk = TMath::Abs(cluster.maxTimeBucket() - cluster.minTimeBucket()) + 1;
  hw += (ntmbk << 22);  // ntmbks...

  static StThreeVector<double> hard_coded_errors(fgDp,fgDt,fgDperp);

  Double_t gain = (row<=13) ? St_tss_tssparC::instance()->gain_in() : St_tss_tssparC::instance()->gain_out();
  Double_t wire_coupling = (row<=13) ? St_tss_tssparC::instance()->wire_coupling_in() : St_tss_tssparC::instance()->wire_coupling_out();
  Double_t q = cluster.charge() * ((Double_t)St_tss_tssparC::instance()->ave_ion_pot() * 
				   (Double_t)St_tss_tssparC::instance()->scale())/(gain*wire_coupling) ;

  StTpcHit *hit = new StTpcHit(global.position(),hard_coded_errors,hw,q
            , (unsigned char ) 0  // c
            , (unsigned short) 0  // idTruth=0
            , (unsigned short) 0  // quality=0,
            , (unsigned short) 0  // id =0,
            , cluster.minPad() //  mnpad
            , cluster.maxPad() //  mxpad
            , cluster.minTimeBucket() //  mntmbk
            , cluster.maxTimeBucket() //  mxtmbk
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
  if (! tpc.channels_sector) return;
  for(Int_t row = 1; row <= 45; row++) {
    r = row - 1;
    if (! tpc.cl_counts[r]) continue;
    for (Int_t pad = 1; pad <= 182; pad++) {
      p = pad - 1;
      Int_t ncounts = tpc.counts[r][p];
      if (! ncounts) continue;
      static UShort_t adc[512];
      memset (adc, 0, sizeof(adc));
      tbmax = 513;
      UShort_t adcmax = 0;
      for (Int_t i = 0; i < ncounts; i++) {
	tb = tpc.timebin[r][p][i];
	adc[tb] = log8to10_table[tpc.adc[r][p][i]]; 
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
  if ( tpc.has_clusters ) {
    for(Int_t r=0;r<45;r++)        {
      tpc_cl *c = &tpc.cl[r][0];
      Int_t ncounts = tpc.cl_counts[r];
      for(Int_t j=0;j<ncounts;j++,c++) {
	static StEVPTpcCluser cluster;
	if( (c->t < 0.1) || (c->p < 0.1)) continue;
	cluster.setTpcCl(c);
	Float_t pad  = cluster.pad() - 0.5;
	Float_t time = cluster.time() - 0.5;
	Float_t q =  cluster.charge();
	pcl[s][r]->Fill(time,pad,q);
	nhits++;
      }
    }
    LOG_INFO << " Total hits in Sector : " << sector << " = " << nhits << endm;
  }
  //    if (! tpc.channels_sector) continue;
  for(Int_t r = 0; r < 45; r++) {
    for (Int_t pad = 1; pad <= 182; pad++) {
      Int_t p = pad - 1;
      Int_t ncounts = tpc.counts[r][p];
      if (! ncounts) continue;
      for (Int_t i = 0; i < ncounts; i++) {
	Int_t tb = tpc.timebin[r][p][i];
	Float_t adc = log8to10_table[tpc.adc[r][p][i]]; 
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
  Int_t r, p, tb, tbmax;
  //  if (! tpc.channels_sector) return;
  for(Int_t row = 1; row <= 45; row++) {
    r = row - 1;
    for (Int_t pad = 1; pad <= 182; pad++) {
      p = pad - 1;
      Int_t ncounts = tpc.counts[r][p];
      if (! ncounts) continue;
      static UShort_t adc[512];
      memset (adc, 0, sizeof(adc));
      tbmax = 513;
      UShort_t adcmax = 0;
      for (Int_t i = 0; i < ncounts; i++) {
	tb = tpc.timebin[r][p][i];
	adc[tb] = log8to10_table[tpc.adc[r][p][i]]; 
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
  if(tpc.mode==0) {	// normal event
    UInt_t tot_pix = 0 ;
    UInt_t cl_count = 0 ;
    Int_t i ;
    
    for(r=0;r<45;r++) {	// padrow
      for(p=0;p<182;p++) {	// pad
	for(t=0;t<tpc.counts[r][p];t++) {	
	  val = tpc.adc[r][p][t] ;										
	  Int_t vali = log8to10_table[val];
	  adc += val ;
	  if(val) tot_pix++ ;
	  if (Debug() > 1) {
	    Int_t timebin = tpc.timebin[r][p][t] ;
	    printf("%d %d %d %d %d\n",sector,r+1,p+1,timebin,vali) ;
	  }
	}
      }
      
      if(tpc.has_clusters) {
	cl_count += tpc.cl_counts[r] ;
      }
      if (Debug() > 1) {
	if(tpc.has_clusters) {
	  for(i=0;i<tpc.cl_counts[r];i++) {
	    tpc_cl *c = &tpc.cl[r][i] ;
	    
	    printf("%d %d %f %f %d %d %d %d %d %d\n",
		   sector,r+1,c->p,c->t,c->charge,c->flags,c->p1,c->p2,c->t1,c->t2) ;
	  }
	}
      }
    }
    LOG_INFO << Form("TPC: Sector %d: occupancy %3d %%, charge %d, pixels %u, clusters %d",sector,
		     (int)(100.0 *((double)tpc.channels_sector/(double)tpc.max_channels_sector)),adc,tot_pix,cl_count) << endm;
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
void StTpcHitMaker::RawData(Int_t sector) {
  static Short_t ADCs[512];
  static UShort_t IDTs[512];
  memset(ADCs, 0, sizeof(ADCs));
  memset(IDTs, 0, sizeof(IDTs));
  StTpcDigitalSector *digitalSector = 0;
  Int_t some_data = 0;
  Int_t r_old = -1;
  Int_t p_old = -1;
  Int_t Total_data = 0;
#ifndef NEW_DAQ_READER
  daq_dta *dta = fRtsReader->det("tpx")->get("adc",sector) ;
  while(dta->iterate()) {
    int r = dta->row ;	// I count from 1
#else /* NEW_DAQ_READER */
  TString query = Form("tpx/adc[%i]",sector);
  while (GetNextDaqElement(query))
  {
    int r=Row() ;	// I count from 1
#endif /* NEW_DAQ_READER */
    if(r==0) continue ;	// TPC does not support unphy. rows so we skip em
    r-- ;			// TPC wants from 0
#ifndef NEW_DAQ_READER
    int p = dta->pad - 1 ;	// ibid.
#else /* NEW_DAQ_READER */
    int p = Pad() - 1 ;	// ibid.
#endif /* NEW_DAQ_READER */
    if (p < 0 || p >= StTpcDigitalSector::numberOfPadsAtRow(r+1)) continue;
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
#ifndef NEW_DAQ_READER
    for(u_int i=0;i<dta->ncontent;i++) {
      int tb = dta->adc[i].tb ;
      int adc = dta->adc[i].adc ;
      ADCs[tb] = adc;
      IDTs[tb] = 65535;
      some_data++ ;	// I don't know the bytecount but I'll return something...
    }
#else
    TGenericTable::iterator iword = DaqDta()->begin();
    for (;iword != DaqDta()->end();++iword) {
        daq_adc_tb &daqadc = (*(daq_adc_tb *)*iword);
        int tb = daqadc .tb ;
        int adc = daqadc .adc ;
        ADCs[tb] = adc;
        IDTs[tb] = 65535;
        some_data++ ;	// I don't know the bytecount but I'll return something...
    }
#endif /* NEW_DAQ_READER */
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
  if (Total_data) {
    LOG_INFO << "Read " << Total_data << " pixels from Sector " << sector << endm;
  }
  if (Total_data) return;
  for (Int_t row = 1;  row <= __NumberOfRows__; row++) {
    Int_t r = row - 1;
    for (Int_t pad = 1; pad <= StTpcDigitalSector::numberOfPadsAtRow(row); pad++) {
      Int_t p = pad - 1;
      memset(ADCs, 0, sizeof(ADCs));
      memset(IDTs, 0, sizeof(IDTs));
      Int_t ncounts = tpc.counts[r][p];
      if (! ncounts) continue;
      for (Int_t i = 0; i < ncounts; i++) {
	Int_t tb = tpc.timebin[r][p][i];
	ADCs[tb] = log8to10_table[tpc.adc[r][p][i]]; 
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
}
//________________________________________________________________________________
StTpcHitCollection *StTpcHitMaker::GetHitCollection() {
  // Get StEvent if any at once
  StTpcHitCollection *hitCollection = 0;
  if (mStEvent) {
    hitCollection = mStEvent->tpcHitCollection();
    // Need to create the hit collection
    if ( !hitCollection )  {
      // Save the hit collection to StEvent...if needed
      hitCollection = new StTpcHitCollection();
      mStEvent->setTpcHitCollection(hitCollection);
    }
  }
  return hitCollection;
}
