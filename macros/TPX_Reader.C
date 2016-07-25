struct BPoint_t {
  Float_t sector, rdo, row, pad, tb, adc, ped, pedrms, t0, t0rms;
};
const Char_t *BName = "sector:rdo:row:pad:tb:adc:ped:pedrms:t0:t0rms";
//________________________________________________________________________________
Int_t TPX_Reader(const Char_t *fname="/star/data03/daq/2007/tonko/tpx_puls8302002_dbg.sfs") {
  int sector, rdo ;
  struct daq_tpx_adc *tpx ;
  BPoint_t P;
  TString fName(gSystem->BaseName(fname));
  //  fName.ReplaceAll(".data",".root");
  fName.ReplaceAll(".sfs",".root");
  f = new TFile(fName.Data(),"RECREATE");
  FitP = new TNtuple("FitP","Pulser",BName);
  
  gSystem->Load("SFS.so") ;
  gSystem->Load("DAQ_TPX.so") ;
  gSystem->Load("RTS_READER.so") ;
  
  
  cint_rts_reader c ;
  
  c.enable("tpx") ;	// tpx is currently the only one enabled...
  
  c.add_input(fname) ;
  // you can repeat the add_input to add up to 1024 files...
  
  sector = 16 ;	// for FY08...
  Int_t ieve = 0;
  while(c.Make() >= 0) {	// loop through all events of all added file...
    printf("******* event %d ***********\n",c.get_cur_event()) ;
    for(rdo=1;rdo<=6;rdo++) {	// loop through all 6 RDOs
      int pads ;
      tpx = (struct daq_tpx_adc *) c.get("tpx","adc",&pads, sector, rdo) ;
      if(tpx == 0) {	// specific detector, bank, sector & rdo not found in this event
	continue ;
      }
      //      printf("rdo %d: %d pads.\n",rdo,pads) ;
      while(pads--) {
	int i ;
	/*
	  NOTE: nonphysical pads are asigned row 0 so row 0 is valid but
	  not physically connected. Skip it.
	*/
	if(tpx->row==0) {
	  tpx++ ;
	  continue ;
	}
	// rows count [1..45], pads [1..182max]
#if 0
	if(tpx->pad == 10) {	// print occasionally
	  printf("\trow %2d: pad %3d: %d pixels\n",
		 tpx->row,tpx->pad,
		 tpx->count) ;
	} ;
#endif
	Int_t   N  = 0;
	Double_t ped = 0; // ped in tb range = [50,180]
	Double_t ped1 = 0;
	Double_t pedrms = 0;
	Int_t tmax = -1;
	Double_t amax = -99;
	for(i=0;i<tpx->count;i++) {
	  if (tpx->adc[i] > amax) {
	    tmax = tpx->tb[i];
	    amax = tpx->adc[i];
	  }
	  if (tpx->tb[i] > 50 && tpx->tb[i] < 180) {
	    N++;
	    ped += tpx->adc[i];
	    pedrms += tpx->adc[i]*tpx->adc[i];
	  }
	}
	if (tmax < 180 || tmax > 220) {tpx++; continue;}
	if (N > 10) {
	  ped /= N;
	  pedrms /= N;
	  pedrms = TMath::Sqrt(pedrms - ped*ped);
	} else {
	  ped = pedrms = 0;
	}
	Double_t A   = 0;
	Double_t t0  = 0; // signal in tb range [180,260]
	Double_t t0rms = 0;
	for(i=0;i<tpx->count;i++) {
	  if (tpx->tb[i] >= tmax - 5 && tpx->tb[i] <= tmax + 5) {
	    Double_t sig = tpx->adc[i] - ped;
	    A  += sig;
	    t0 += tpx->tb[i]*sig;
	  }
	}	
	if (A > 10) {
	  t0 /= A;
	  t0rms = 1./TMath::Sqrt(A);
	} else {
	  t0 = t0rms = 0;
	}
	for(i=0;i<tpx->count;i++) {
	  // timebin & corresponding ADC:
	  //	  printf("\t\t tb %4d = %4d\n",tpx->tb[i],tpx->adc[i]) ;
	  if (tpx->tb[i] > tmax - 20  && tpx->tb[i] < tmax + 80) {
	    P.sector = sector;
	    P.rdo   = rdo;
	    P.row   = tpx->row;
	    P.pad   = tpx->pad;
	    P.tb    = tpx->tb[i];
	    P.adc   = tpx->adc[i];
	    P.ped   = ped;
	    P.pedrms= pedrms;
	    P.t0   = t0;
	    P.t0rms= t0rms;
	    FitP->Fill(&P.sector);
	  }
	}
	tpx++ ;	// move to next pad...
      }
    }
    ieve++;
    //    if (ieve) break;
  }
  printf("Done with %d events...\n",c.get_cur_event()) ;
  f->Write();
  return c.get_cur_event() ;
}
//________________________________________________________________________________
void makePlots(const Char_t *files="tpx_puls8302002_dbg.root") {
  Int_t Npads[45] = {   88,  96, 104, 112, 118, 126, 134, 142, 150, 158, // Inner
			166, 174, 182,
		        98, 100, 102, 104, 106, 106, 108, 110, 112, 112, // Outer
		       114, 116, 118, 120, 122, 122, 124, 126, 128, 128, 
		       130, 132, 134, 136, 138, 138, 140, 142, 144, 144, 
		       144, 144};
  TDirIter Dir(files);
  TTreeIter iter("FitP");
  Char_t *file = 0, *file1 = 0;
  Int_t NFiles = 0;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++; file1 = file;}
  cout << "file1 " << file1 << endl;
  const Float_t&     sector                                   = iter("sector");
  const Float_t&     rdo                                      = iter("rdo");
  const Float_t&     row                                      = iter("row");
  const Float_t&     pad                                      = iter("pad");
  const Float_t&     tb                                       = iter("tb");
  const Float_t&     adc                                      = iter("adc");
  const Float_t&     ped                                      = iter("ped");
  const Float_t&     pedrms                                   = iter("pedrms");
  const Float_t&     t0                                       = iter("t0");
  const Float_t&     t0rms                                    = iter("t0rms");
  TString Out(gSystem->BaseName(file1));
  Out.ReplaceAll(".root","Shape3.root");
  TFile *fOut = new TFile(Out,"recreate");
  TProfile *rp[45];
  TProfile *rx[45];
  TProfile *Rp[45];
  TProfile *Rx[45];
  for (Int_t r = 1; r <= 45; r++) {
    rp[r-1] = new TProfile(Form("row%02i",r),Form("Signal versus tb-t0 for row = %i",r),250,-5.,20.);
    rx[r-1] = new TProfile(Form("r%02i",r),Form("Signal versus tb for row = %i",r),100,150,250);
    Rp[r-1] = new TProfile(Form("Row%02i",r),Form("Signal+ped versus tb-t0 for row = %i",r),250,-5.,20.);
    Rx[r-1] = new TProfile(Form("R%02i",r),Form("Signal+ped versus tb for row = %i",r),100,150,250);
  }
  Int_t n = 0;
  while (iter.Next()) {
    if (n%100000 == 0) {
      cout << n << " sector " << sector << " rdo " << rdo << " row " << row << " pad " << pad 
	   << " tb " << tb << " adc " << adc << " ped " << ped << " t0 " << t0 << endl;
    }
    Int_t r = row - 1;
    if (pad < 5 || pad > Npads[r]-5) continue;
    rp[r]->Fill(tb-t0,adc-ped);
    rx[r]->Fill(tb,adc-ped);
    Rp[r]->Fill(tb-t0,adc);
    Rx[r]->Fill(tb,adc);
    n++;
#if 0
    if (n > 10) break;
#endif
  }
  fOut->Write();
  //  delete fOut;
}
