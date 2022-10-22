/*
  The example shows how to rerun the cluster finder
  on the TPX (or TPC) cluster data...
*/

#include <assert.h>
#include <map>
#include <vector>
#include <stdio.h>
#if !defined(__CINT__) 
#include <sys/types.h>
#include <rtsLog.h>
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>
#include <DAQ_TPX/daq_tpx.h>
#include <DAQ_TPX/tpxGain.h>
#include "RTS/src/DAQ_TPX/tpxFCF_flags.h" // for FCF flag definition
#endif

#if 0
int main(int argc, char *argv[])
#else
/*
  root.exe lRTS.C tpc_rerun.C+
*/
#include "Riostream.h"
#include "TFile.h"
#include "TTree.h"
#include "TString.h"
  using namespace std;
class HitParameters : public TObject {
public:
  HitParameters() {set();}
  HitParameters(Int_t s, Int_t r, Int_t charge, Float_t p, Int_t pmn, Int_t pmx, Float_t t, Int_t tmn, Int_t tmx, Int_t flag) {
    set();
    sector = s; row = r; adc = charge; pad = p; timebucket = t; pmin = pmn; pmax = pmx;  tmin = tmn; tmax = tmx; fl = flag;
  };
  virtual ~HitParameters() {}
  void set() {memset(&begin, 0, &end-&begin);}
  Char_t begin;
  Int_t    sector, row;
  Float_t  x,y,z,q;
  Int_t    adc;
  Float_t pad,timebucket;
  Int_t    npads, ntbks, IdTruth;
  Int_t    pmin, pmax, tmin, tmax;
  Float_t  xL,yL,zL,dX;
  Int_t    trigId, us,fl;
  Float_t  time, timeb;
  Char_t end;
  virtual void Print(Option_t *option="") const {
    cout << Form("HitP s/r %3i/%3i ",sector,row);
    cout << Form(" fl%3i us %1i", fl, us)
      //	 << Form(" xyz:%10.3f%10.3f%10.3f xyzL:%10.3f%10.3f%10.3f",x,y,z,xL,yL,zL)
      //	 << Form(" dX %5.2f",dX)
	 << Form(" tm %6.2f pad %6.2f adc %5i",timebucket,pad, adc)
	 << Form(" IdT%6i,npad%3i,ntbks%3i",IdTruth,npads,ntbks)
	 << endl;
  }
  ClassDef(HitParameters,3)
};
class HitMatch : public TObject {
public:
  HitMatch() {}
  virtual ~HitMatch() {}
  HitParameters newP;
  HitParameters oldP;
  ClassDef(HitMatch,3)
};
typedef map<const HitParameters*,const HitParameters*>     Hit2HitMap;
vector<HitParameters> OnlineClusters;
vector<HitParameters> OfflineClusters;
TTree *hitTree = 0;
HitMatch *fHitMatch = 0;
//________________________________________________________________________________
void FillMatch(const HitParameters* hit1,const  HitParameters* hit2) {
  if (hit1) fHitMatch->oldP = *hit1;
  else      fHitMatch->oldP = HitParameters();
  if (hit2) fHitMatch->newP = *hit2;
  else      fHitMatch->newP = HitParameters();
  hitTree->Fill();
}
Int_t tpc_rerun(const Char_t *file = "/hlt/cephfs/daq/2018/166/19166011/st_physics_adc_19166011_raw_4000003.daq")
#endif
{
  rtsLogOutput(RTS_LOG_STDERR) ;
  //	rtsLogLevel(NOTE) ;
#if 0
  daqReader *dr = new daqReader(argv[1]) ;	// open file
#else
  TFile *fOut = new TFile("trackMateFilest_physics_adc.root","recreate");
  hitTree = new TTree("hitMateComp","hitMateComp");
  fHitMatch = new HitMatch;
  TBranch *hitBr = hitTree->Branch("HitMatch",&fHitMatch);
  daqReader *dr = new daqReader(file) ;	// open file
  Int_t iPrint = 0;
#endif
  // need this specific construct so that the cluster finder can initialized!
  daq_tpx *tpx = new daq_tpx(dr) ;	// insert the TPX detector explicitly in the DAQ_READER
  // set compatibility flsgs
  tpx->fcf_run_compatibility = 10 ;
  tpx->fcf_do_cuts = 1 ;
  
  // we'll skip this so that we can load our own gaons
  //	tpx->InitRun(123) ;			// initialize the run with some dummy run number...
  
  // load our own gain file here
  //	tpx->gain_algo->from_file((char *)"tpx_gains.txt.20191216.060513",0) ;
  tpx->gain_algo->from_file((char *)"/net/l402/data/fisyak/STAR/packages/.DEV2/StarDb/Calibrations/tpc/tpx/tpx_gains.txt.23Oct18.1",0) ;
  Int_t ev = 0;
  while(dr->get(0,0)) {			// zip through the input files...
    
    
    int got_adc_data = 0 ;
    daq_dta *dd, *sim_dta ;
    
#define DUMP_CLD_IN_FILE
#ifdef DUMP_CLD_IN_FILE
    // if you care, you can dump the in-file clusters here
    for (Int_t s = 1; s <= 24; s++) 
      for (Int_t r = 1; r <= 45; r++) {
	OnlineClusters.clear();
	OfflineClusters.clear();
	dd = dr->det("tpx")->get("cld") ;
	if (! dd) continue;
	while(dd && dd->iterate()) {
	  if (dd->sec != s) continue;
	  if (dd->row != r) continue;
	  for(u_int i=0;i<dd->ncontent;i++) {
	    
	    // Here we can blow off clusters we don't care about
	    //			switch(dd->cld[i].flags) {
	    //			case 0 :	// normal
	    //			case 1 :	// normal
	    //			case 2 :	// normal
	    //			case 3 :	// should not really use
	    //				continue ;
	    //			default :
	    //				break ;
	    //			}
	    if (iPrint < 10) {
	    printf("in file: row %2d: pad %f [%d:%d], tb %f [%d:%d], charge %d, flags 0x%X\n",dd->row,
		   dd->cld[i].pad,
		   dd->cld[i].p1,
		   dd->cld[i].p2,
		   dd->cld[i].tb,
		   dd->cld[i].t1,
		   dd->cld[i].t2,
		   dd->cld[i].charge,
		   dd->cld[i].flags) ;
	    }
	    if (dd->cld[i].flags & ~(FCF_ONEPAD | FCF_MERGED | FCF_BIG_CHARGE)) {
	    } else {
	      OnlineClusters.push_back(HitParameters(s,r,dd->cld[i].charge,dd->cld[i].pad, dd->cld[i].p1, dd->cld[i].p2, dd->cld[i].tb,  dd->cld[i].t1, dd->cld[i].t2, dd->cld[i].flags));
	    }	    
	  }
	}
#endif
	
#define DO_CLD
#ifdef DO_CLD
	dd = dr->det("tpx")->get("adc") ;	// get the ADC data
	if(dd == 0) {
	  LOG(WARN,"No adc data in this event...") ;
	  continue ;			// not there, skip...
	}
	
	
	sim_dta = dr->det("tpx")->put("adc_sim") ;	// create the ADC data for simulation
	
	
	// read input data and fill in the adc_sim
	while(dd->iterate()) {
	  got_adc_data = 1 ;
	  daq_sim_adc_tb *sim_d = (daq_sim_adc_tb *) sim_dta->request(512) ;	// ask for space
	  
	  // copy over
	  for(u_int i=0;i<dd->ncontent;i++) {
	    sim_d[i].adc = dd->adc[i].adc ;
	    sim_d[i].tb = dd->adc[i].tb ;
	    sim_d[i].track_id = -1; //0xFFFF ;	// should be 0xFFFF if you don;t have tracking...
	  }
	  
	  // wrap up this pad with the correct length & coordinates
	  sim_dta->finalize(dd->ncontent,dd->sec,dd->row,dd->pad) ;
	  
	}
	
	if(!got_adc_data) {
	  LOG(WARN,"No ADC data in this event...") ;
	  continue ;
	}
	LOG(NOTE,"Doing adc data...") ;
	
	// OK, now we have the ADC data ready for re-clusterfinding so let's do it
	
	
	dd = dr->det("tpx")->get("cld_sim") ;	// this will rerun the cluster finder on the "adc_sim" data
	if(dd == 0) continue ;	// error
	
	// dump the newly found data out...
	while(dd->iterate()) {
	  //printf("sec %2d, row %3d: %d clusters\n",dd->sec,dd->row,dd->ncontent) ;
	  
	  if (dd->sec != s) continue;
	  if (dd->row != r) continue;
	  for(u_int i=0;i<dd->ncontent;i++) {
#if 0
	    if(dd->sim_cld[i].cld.flags != 0) continue ;
	    if((dd->sim_cld[i].cld.p2 - dd->sim_cld[i].cld.p1) != 2) continue ;
	    
	    int tid = dd->sim_cld[i].track_id ;
	    
	    daq_dta *dta = dr->det("tpx")->get("adc_sim") ;
	    while(dta && dta->iterate()) {
	      daq_sim_adc_tb *sim_d = (daq_sim_adc_tb *) dta->Void ;
	      
	      for(int i=0;i<dta->ncontent;i++) {
		if(sim_d[i].track_id == tid) {
		  printf("%5d %d %d %d %d\n",tid,dta->row,dta->pad,sim_d[i].tb,sim_d[i].adc) ;
		}
	      }
	    }
#endif
	    if (iPrint < 10) {	    
#if 1
	    // Here we can blow off clusters we don't care about
	    //			if(dd->sim_cld[i].cld.flags != 0) continue ;
	    //			if((dd->sim_cld[i].cld.p2 - dd->sim_cld[i].cld.p1) != 2) continue ;
	    //			switch(dd->sim_cld[i].cld.flags) {
	    //			case 0 :
	    //			case 2 :
	    //				continue ;
	    //			default:
	    //				break ;
	    //			}
	    //
	    //			if(dd->sim_cld[i].cld.tb < 15.0) 
	    printf("rerun: row %2d: pad %f [%d:%d], tb %f [%d:%d], charge %d, flags 0x%X: track %d, Q %d\n",dd->row,
		   dd->sim_cld[i].cld.pad,
		   dd->sim_cld[i].cld.p1,
		   dd->sim_cld[i].cld.p2,
		   dd->sim_cld[i].cld.tb,
		   dd->sim_cld[i].cld.t1,
		   dd->sim_cld[i].cld.t2,
		   dd->sim_cld[i].cld.charge,
		   dd->sim_cld[i].cld.flags,
		   dd->sim_cld[i].track_id,
		   dd->sim_cld[i].quality) ;
#endif
	    iPrint++;
	    }
	    if (dd->sim_cld[i].cld.flags & ~(FCF_ONEPAD | FCF_MERGED | FCF_BIG_CHARGE)) {
	    } else {
	      OfflineClusters.push_back(HitParameters(s,r,dd->sim_cld[i].cld.charge,
						     dd->sim_cld[i].cld.pad, dd->sim_cld[i].cld.p1, dd->sim_cld[i].cld.p2, 
						     dd->sim_cld[i].cld.tb,  dd->sim_cld[i].cld.t1, dd->sim_cld[i].cld.t2, 
						     dd->sim_cld[i].cld.flags));
	    }	    
	  }
	}
	
#endif
#if 1
	Hit2HitMap Hit2ToHit1;
	Hit2HitMap Hit1ToHit2;
	static Int_t _debug = 0;
	Int_t N1 = OnlineClusters.size();
	Int_t N2 = OfflineClusters.size();
	for (Int_t i1 = 0; i1 < N1; i1++) {
	  HitParameters &hit1 = OnlineClusters[i1];
	  for (Int_t i2 = 0; i2 < N2; i2++) {
	    HitParameters &hit2 = OfflineClusters[i2];
	    assert(hit1.sector == hit2.sector &&
		   hit1.row == hit2.row);
	    Double_t R = 
	      (hit1.pad        - hit2.pad       )*(hit1.pad        - hit2.pad      ) +
	      (hit1.timebucket - hit2.timebucket)*(hit1.timebucket - hit2.timebucket);
	    if (R < 4) {
	      if (! Hit1ToHit2[&hit1] && ! Hit2ToHit1[&hit2]) {
	    if (_debug) {
	      cout << "matched hit1\t"; hit1.Print();
	      cout << "matched hit2\t"; hit2.Print();
	    }
		Hit1ToHit2[&hit1] = &hit2;
		Hit2ToHit1[&hit2] = &hit1;
	      } else {
		const HitParameters *hit1o = Hit2ToHit1[&hit2];
		const HitParameters *hit2o = Hit1ToHit2[&hit1];
		if (hit1o) {
		  Double_t Ro = 
		    (hit1o->pad        - hit2.pad       )*(hit1o->pad        - hit2.pad      ) +
		    (hit1o->timebucket - hit2.timebucket)*(hit1o->timebucket - hit2.timebucket);
		  if (R < Ro) Hit2ToHit1[&hit2] = &hit1;
		} else { 
	    if (_debug) {
	      cout << "rematched1 hit1\t"; hit1.Print();
	      cout << "rematched1 hit2\t"; hit2.Print();
	    }
		  Hit2ToHit1[&hit2] = &hit1;
		}
		if (hit2o) {
		  Double_t Ro = 
		    (hit1.pad        - hit2o->pad       )*(hit1.pad        - hit2o->pad      ) +
		    (hit1.timebucket - hit2o->timebucket)*(hit1.timebucket - hit2o->timebucket);
		  if (R < Ro) Hit1ToHit2[&hit1] = &hit2;
		} else {
	    if (_debug) {
	      cout << "rematched2 hit1\t"; hit1.Print();
	      cout << "rematched2 hit2\t"; hit2.Print();
	    }
		  Hit1ToHit2[&hit1] = &hit2;
		}
	      }
	    }
	  } // hit2 loop
	} // hit1 loop
	for (Int_t i1 = 0; i1 < N1; i1++) {
	  HitParameters &hit1 = OnlineClusters[i1];
	  const HitParameters *hit2 = Hit1ToHit2[&hit1];
	    if (_debug) {
	      cout << "Add hit1\t"; hit1.Print();
	      if (hit2) {cout << "Add hit2\t"; hit2->Print();}
	    }
	  FillMatch(&hit1, hit2);
	}
	for (Int_t i2 = 0; i2 < N2; i2++) {
	  HitParameters &hit2 = OfflineClusters[i2];
	  const HitParameters *hit1 = Hit2ToHit1[&hit2];
	  if (hit1) continue;
	    if (_debug) {
	      cout << "Add hit2\t"; hit2.Print();
	    }
	  FillMatch(hit1, &hit2);
	}
#endif
      } // sector &&  row loops
    ev++;
    if (ev%100 == 1) cout << "process event\t" << ev << endl;
  }	// end of ebent
  fOut->Write();
  return 0 ;
}
