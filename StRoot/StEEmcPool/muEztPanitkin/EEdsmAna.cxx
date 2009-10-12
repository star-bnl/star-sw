//#include <iostream>

#include <TObjArray.h>
#include <TH2.h>
#include <TFile.h>

#include <StMessMgr.h>

#include "EEdsmAna.h"

#define EEmapTP_USE // trick instattiates data only in the cxx
#include "StTriggerUtilities/Eemc/EEmapTP.h" 
#undef EEmapTP_USE

#include "StTriggerUtilities/Eemc/EEdsm0.h"
#include "StTriggerUtilities/Eemc/EEdsm1.h"
#include "StTriggerUtilities/Eemc/EEdsm2.h"
#include "StTriggerUtilities/Eemc/EEdsm3.h"

#include "StTriggerUtilities/StDSMUtilities/DSM.hh"
#include "StTriggerUtilities/StDSMUtilities/DSMAlgo_EE101_2009.hh"
#include "StTriggerUtilities/StDSMUtilities/DSMAlgo_EE102_2009.hh"


ClassImp(EEdsmAna)

//--------------------------------------------------
EEdsmAna::EEdsmAna(const Char_t *name, int year)
    : myName(name)
    , mYear(year)
    , EE101(*new DSM("EE101"))
    , EE102(*new DSM("EE102"))
{
  nTot=0;

    mJPthr[0]=16;
    mJPthr[1]=24;
    mJPthr[2]=30;
    mJPthr[3]=40;
  int TPthrSelc = 2;
  int HTTPthrSelc = 2;

  //=====================  DSM 0 ===============
  Nee0 = 9; 
  ee0 = new EEdsm0[Nee0];
  Nee0out = 12;
  ee0outTPadc = new int [Nee0out];
  ee0outHTadc = new int [Nee0out];
  
  // set pedestals initially to 0
  ped0 = new int *[Nee0]; // WARN, may be old from 2005 ???, probably not used in the code
  for (int ibr = 0;ibr < Nee0; ibr++) {
    ped0[ibr] = new int [ee0[ibr].getNc()];
    for(int ch = 0;ch < ee0[ibr].getNc();ch++) 
      ped0[ibr][ch]=0;
  }

  //=====================  DSM 1 ===============
  Nee1 = 2;
  ee1 = new EEdsm1[Nee1];
  for(int i = 0;i < Nee1;i++) ee1[i].setYear(mYear, mJPthr, TPthrSelc, HTTPthrSelc);
  ee1[0].setType(1);
  ee1[1].setType(2);
  ee1out3JPadc = new int [Nee1];
  
  //=====================  DSM 2 ===============
  ee2 = new EEdsm2;  // Endcap
  ee2->setYear(mYear);
  
  be2 = new EEdsm2[Nbe2];
  for(int i = 0;i < Nbe2;i++)   // Barrel
    be2[i].setYear(mYear);

  //=====================  DSM 3 ===============
  ee3 = new EEdsm3;
  ee3->setYear(mYear);

  //===================== 2009 L1 DSM =====================

  // JP E_T = (JP ADC - 5) * 0.236 GeV

  EE101.registers[0] = EE102.registers[0] = 32;	// 6.4 GeV
  EE101.registers[1] = EE102.registers[1] = 40;	// 8.3 GeV
  EE101.registers[2] = EE102.registers[2] = 60;	// 13  GeV

  clear();
}

//--------------------------------------------------
EEdsmAna::~EEdsmAna() {
  delete& EE101;
  delete& EE102;
}

//--------------------------------------------------
void EEdsmAna::clear() {

  for (int i=0;i<Nee0;i++) if (ee0) ee0[i].clear();
  for (int i=0;i<Nee0out;i++) { //# boards !=  # outputs
    if (ee0outTPadc) ee0outTPadc[i]=0;
    if (ee0outHTadc) ee0outHTadc[i]=0;
  }

  for (int i=0;i<Nee1;i++) { // # boards ==# outputs
    if (ee1) ee1[i].clear();
    if (ee1out3JPadc) ee1out3JPadc[i]=0;
  }

  memset(ee1outTPthrMax, 0, sizeof(ee1outTPthrMax));
  memset(ee1outHTTPthrMax, 0, sizeof(ee1outHTTPthrMax));

  for (int i=0;i<EEnJetPatch;i++) { 
    ee1outJPadc[i]=0;
    ee1outHT[i]=0;
    AdjJPsum[i]=0;
  }

  for (int i = 0;i < EEnHalf;i++) {
    ee1outTPthrMax[i] = 0;
    ee1outHTTPthrMax[i] = 0;
  }

  if (ee2) ee2->clear();
  ee2outHT=0;

  for (int i=0;i<Nbe2;i++) 
    if (be2) be2[i].clear();

  if (ee3) ee3->clear();
}

//--------------------------------------------------
void EEdsmAna::initHisto(TObjArray *HList){
    // .................level-0 input, 2D, HT ..............
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	H0inHT[iJ] = new TH2F(Form("dsm0inJP%d_HT", iJ + 1),
    	    Form("HT input, DSM layer-0, Jet Patch %d (Steve), trig=%s;input 0-DSM ADC value;Trig Patch  ID", iJ + 1, myName),
	    64, -0.5, 63.5, EEnTPinJP, 0.5, EEnTPinJP + 0.5);
	if (HList) HList->Add(H0inHT[iJ]); 
    }

    H0inHTall = new TH2F("dsm0inJPall_HT", 
	Form("HT input, DSM layer-0, all trig Patches, trig=%s; Hanks ID=10*brd+inpID", myName),
	90, -0.5, 89.5, 64, -0.5, 63.5);
    if (HList) HList->Add(H0inHTall);

    // ........level-0 input, 2D, TP sum
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	H0inTP[iJ] = new TH2F(Form("dsm0inJP%d_TP", iJ + 1),
	Form("TP input, DSM layer-0, Jet Patch %d (Steve), trig=%s;input to level 0;Trig Patch  ID", iJ + 1,myName),
	64, -0.5, 63.5, EEnTPinJP, 0.5, EEnTPinJP + 0.5);
	if (HList) HList->Add(H0inTP[iJ]);
    }

    H0inTPall = new TH2F("dsm0inJPall_TP", Form("TP input, DSM layer-0, all trig Patches, trig=%s", myName),
	90, -0.5, 89.5, 64, -0.5, 63.5);
    if (HList) HList->Add( H0inTPall);

    // .................level-1 input, 2D
    //================================ TP 

    for(int iJ = 0;iJ < EEnHalfJetPatch;iJ++) {
	H1inTPvEmu[iJ] = new TH2F(Form("dsm1HJP%d_TP", iJ + 1),
	    Form("TP input vs. emulated, DSM layer-1, brd=%d  ch=%d, trig=%s;emulated TP from level-0;input to level-1", iJ/6, iJ%6, myName),
	    40, 0., 400, 40, 0, 400);
	if (HList) HList->Add(H1inTPvEmu[iJ]);
    }

    //================================ HT 
    for(int iJ = 0;iJ < EEnHalfJetPatch;iJ++) {
	H1inHTvEmu[iJ] = new TH2F(Form("dsm1HJP%d_HT", iJ + 1), 
	    Form("HT input vs.emu, DSM-1, Half Patch %d (Falk), trig=%s;emulated HT from level-0;input to level-1", iJ + 1, myName),
	    64, -0.5, 63.5, 4, -0.5, 3.5);
	if (HList) HList->Add(H1inHTvEmu[iJ]);
    }

    // .................level-2 input

    //================================ HT 
    for(int iJ = 0;iJ < EEnHalf;iJ++) {
	H2inHTTP[iJ] = new TH2F(Form("dsm2Half%d_HTTP", iJ + 1),
	    Form("TP (+) HTTP thres, JP_Falk(%d+%d+%d), trig=%s;emu layer1 out: maxTPthr*4 + maxHTTPthr;layer2 inp: TPthr*2 + HTTP", 
		3*iJ, (3*iJ) + 1, (3*iJ) + 2, myName),
	    16, -0.5, 15.5, 4, -0.5, 3.5);
	if (HList) HList->Add(H2inHTTP[iJ]);
    }

    //================================ half Etot sum
    for(int iJ = 0;iJ < EEnHalf;iJ++) {
	H1inEtot[iJ] = new TH1F(Form("dsm2Half%d_Etot", iJ + 1),
	    Form("Etot for JP_Falk(%d+%d+%d), trig=%s; level2 input", 3*iJ, (3*iJ) + 1, (3*iJ) + 2, myName),
	    32, -0.5, 31.5);
	if (HList) HList->Add(H1inEtot[iJ]); 
    }

    // Total energy histos
    for(int iJ = 0;iJ < mxEtotBit;iJ++) {
        HEetot[iJ] = new TH1F(Form("dsm2E_etot%d", iJ),
	    Form("EEMC ETOT BIT=%d, trig=%s;  EEMC ETOT emul sum over 1 brd @ level2", iJ, myName),
	    48, 15.5, 63.5);
        HEetot[iJ]->SetLineColor((iJ == 1) ? kRed : kBlue);
        if (HList) HList->Add(HEetot[iJ]);
    
        HBetot[iJ] = new TH1F(Form("dsm2B_etot%d", iJ),
	    Form("BEMC ETOT BIT=%d, trig=%s;  BEMC ETOT emul sum over 3 brds @ level2", iJ, myName),
	    190, 65.5, 255.5);
        HBetot[iJ]->SetLineColor((iJ == 1) ? kRed : kBlue);
        if (HList) HList->Add(HBetot[iJ]);

        HBEetot[iJ] = new TH1F(Form("dsm2BE_etot%d", iJ),
	    Form("B+EEMC ETOT BIT=%d, trig=%s;  B+EEMC ETOT emul sum over 4 brds @ level2", iJ, myName),
	    190, 65.5, 255.5);
	HBEetot[iJ]->SetLineColor((iJ == 1) ? kRed : kBlue);
        if (HList) HList->Add(HBEetot[iJ]);
    }

    // .................level-3 input
    //================================ HT 
    H3inHTTP = new TH2F("dsm3_HTTP", Form("HTTP  layer=3, trig=%s;emu layer2 out:  orTPbit*2 + orHTTPbit;layer3 inp: TPbit*2 + HTTPbit", myName),
	4, -0.5, 3.5, 4, -0.5, 3.5);
    if (HList) HList->Add(H3inHTTP); 

    //..................Jet Patch sums
    //=================================summed patch spectra
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	int steve_jp = ((iJ + 2) % EEnJetPatch) + 1;  //stored by dsm input number 0-5 Want by Steves jp #
	H4jpSums[iJ] = new TH1F(Form("JP%d_sum", steve_jp),
	    Form("Emulated Sum Jet Patch %d (DSMin %d);Jet Patch Emu Sum;Freq", ((iJ + 2) % EEnJetPatch) + 1, iJ),
	    300, -0.5, 299.5);
	if (HList) HList->Add(H4jpSums[iJ]);
    }

    //=================================Jet patch over threshold
    static const char* JetPatchSumThresholdTitles[] = {
      "Jet Patch Sum <= JP0",
      "JP0 < Jet Patch Sum <= JP1",
      "JP1 < Jet Patch Sum <= JP2",
      "Jet Patch Sum > JP2"
    };

    for (int iJ = 0;iJ < EEnThresh;iJ++) {
	TH1F *h = new TH1F(Form("JPsumTh%d", iJ), JetPatchSumThresholdTitles[iJ], 6, 0.5, 6.5);
	h->GetXaxis()->SetTitle("Steve's Jet Patch ID");
	h->GetYaxis()->SetTitle("Frequency"); 
	h->SetFillColor(kBlue);
	H4jpFreq[iJ]=h;
	if (HList) HList->Add(h);
    }

    //=================================summed adjacent patch spectra
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	int steve_jp = ((iJ + 2) % EEnJetPatch) + 1;  //stored by dsm input number 0-5 Want by Steves jp #
	int steve_jp2 = ((iJ + 3) % EEnJetPatch) + 1;
	H4adjpSums[iJ] = new TH1F(Form("JP%d%d_sum", steve_jp, steve_jp2),
	    Form("Emulated Sum Jet Patches %d+%d;Jet Patch Emu Sum;Freq", steve_jp, steve_jp2),
	    150, -0.5, 149.5);
	if (HList) HList->Add(H4adjpSums[iJ]);
    }

    //=================================adjacent patch correlation
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	int steve_jp = ((iJ + 2) % EEnJetPatch) + 1;  //stored by dsm input number 0-5 Want by Steves jp #
	int steve_jp2 = ((iJ + 3) % EEnJetPatch) + 1;
	H4adjPcor[iJ] = new TH2F(Form("JP%d%d_cor", steve_jp, steve_jp2),
	    Form("Emulated Sum Jet Patches %d vs %d;Jet Patch %d Sum;Jet Patch %d Sum", steve_jp, steve_jp2, steve_jp, steve_jp2),
	    60, 9.5, 69.5, 60, 9.5,69.5);
	if (HList) HList->Add(H4adjPcor[iJ]);
    }

    { //====================freq summed adjacent patch spectra over thresh
	H4adjpFreq = new TH1F("JPadjTh", "Adjacent Jet patches both over thr0;DSM Adjacent Jet Patch Emu Sum (FEE crate add 2);Freq", 
	    8, -0.5, 7.5);
	if (HList) HList->Add(H4adjpFreq);
    }

    {
	// added in 2005 
	H5jpPed = new TH2F("JPpedZoom", "Zoom in of 1x1 JP pedestals;Steve's JP ID",
	    EEnJetPatch, 0.5, EEnJetPatch + 0.5, 46, -0.5, 45.5); //2005: Y:40,34.5,74.5
	if (HList) HList->Add(H5jpPed); 
    }
    
    {
	H5jpFreq = new TH1F("JPtotFreq", Form("JP DSM sum > %d;Steve's JP ID", mJPthr[1]), 
	    EEnJetPatch, 0.5, EEnJetPatch + 0.5); 
	H5jpFreq->SetFillColor(kGreen);
	if (HList) HList->Add(H5jpFreq);

	H5jpHot = new TH1F("JPpedHot", "Hot towers;Steve's JP ID",
	    EEnJetPatch, 0.5, EEnJetPatch + 0.5); 
	H5jpHot->SetFillColor(kRed); // this histo is updated in presenter - a bit tricky
	if (HList) HList->Add(H5jpHot); // is _not_ filled by sorter
    }
}

//--------------------------------------------------
void EEdsmAna::resetHisto(){
    // .................level-0 input, 2D, HT ..............
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	if (H0inHT[iJ]) H0inHT[iJ]->Reset();
    }
    if (H0inHTall) H0inHTall->Reset();
    // ........level-0 input, 2D, TP sum
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	if (H0inTP[iJ]) H0inTP[iJ]->Reset();
    }
    if (H0inTPall) H0inTPall->Reset();

    // .................level-1 input, 2D
    //================================ TP 
    for(int iJ = 0;iJ < EEnHalfJetPatch;iJ++) {
	if (H1inTPvEmu[iJ]) H1inTPvEmu[iJ]->Reset();
    }
    //================================ HT 
    for(int iJ = 0;iJ < EEnHalfJetPatch;iJ++) {
	if (H1inHTvEmu[iJ]) H1inHTvEmu[iJ]->Reset();
    }

    // .................level-2 input
    //================================ HT 
    for(int iJ = 0;iJ < EEnHalf;iJ++) {
	if (H2inHTTP[iJ]) H2inHTTP[iJ]->Reset();
    }
    //================================ half Etot sum
    for(int iJ = 0;iJ < EEnHalf;iJ++) {
	if (H1inEtot[iJ]) H1inEtot[iJ]->Reset();
    }

    // Total energy histos
    for(int iJ = 0;iJ < mxEtotBit;iJ++) {
        if (HEetot[iJ]) HEetot[iJ]->Reset();
        if (HBetot[iJ]) HBetot[iJ]->Reset();
        if (HBEetot[iJ]) HBEetot[iJ]->Reset();
    }

    // .................level-3 input
    //================================ HT 
    if (H3inHTTP) H3inHTTP->Reset();

    //..................Jet Patch sums
    //=================================summed patch spectra
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	if (H4jpSums[iJ]) H4jpSums[iJ]->Reset();
    }
    //=================================Jet patch over threshold
    for (int iJ = 0;iJ < EEnThresh;iJ++) {
	if (H4jpFreq[iJ]) H4jpFreq[iJ]->Reset();
    }
    //=================================summed adjacent patch spectra
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	if (H4adjpSums[iJ]) H4adjpSums[iJ]->Reset();
    }
    //=================================adjacent patch correlation
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	if (H4adjPcor[iJ]) H4adjPcor[iJ]->Reset();
    }
    { //====================freq summed adjacent patch spectra over thresh
	if (H4adjpFreq) H4adjpFreq->Reset();
    }
    {
	// added in 2005 
	if (H5jpPed) H5jpPed->Reset();
    }    
    {
	if (H5jpFreq) H5jpFreq->Reset();
	if (H5jpHot) H5jpHot->Reset();
    }
}


//--------------------------------------------------
void EEdsmAna::sort( const unsigned char * dsm0inp, 
		      const unsigned short int  * dsm1inp,
		      const unsigned short int  * dsm2inp,   
		      const unsigned short int  * dsm3inp) {
    nTot++;

    if (dsm0inp) readDsm0(dsm0inp);
    if (dsm1inp) readDsm1(dsm1inp);
    if(dsm2inp) readDsm2(dsm2inp);
    if(dsm3inp) readDsm3(dsm3inp);
    
    emulDsm0();    
    emulDsm1();    
    emulDsm2();    

    histoDsm0();
    histoDsm1();
    histoDsm2();
    histoDsm3();

    return;

    LOG_DEBUG << "==================================" << endm;
    printAllEndcap();
    printAllBarrel();

#if 0
  // test for Steve:
  int iJ;
  for(iJ=0;iJ<EEnHalf;iJ++) {
    if(ee1outTPthrMax[iJ]>=ee1outHTTPthrMax[iJ]) continue;
    LOG_DEBUG << Form("NEW_EVE==================================\n ee1outTPthrMax[%d]=%d <ee1outHTTPthrMax[%d]=%d\n",iJ,ee1outTPthrMax[iJ],iJ,ee1outHTTPthrMax[iJ]) << endm;
    goto dump;
  }
#endif

}


//--------------------------------------------------
void EEdsmAna::emulDsm0() {
    if (ee0) {
	for (int i = 0;i < EEnTPphi;i++) {
	    for(int j = 0;j < EEnTPeta;j++) {
		const EEmapTP &y = eeMapTP[i][j];
		//if (y) {
		    int ibr = y.brdIn - 1;
		    int ch = y.chIn;
		    int chOut = y.chOut;
		    const EEdsm0 &br = ee0[ibr];
		    // ........ TP ..........
		    ee0outTPadc[chOut] += br.getInpTP6bit(ch);
		    //.......... HT ..............
		    int val= br.getInpHT6bit(ch);
    		    if(ee0outHTadc[chOut]< val)ee0outHTadc[chOut]=val;
		//}
    	    }
	}
    }
}

//--------------------------------------------------
void EEdsmAna::emulDsm1() {
  switch (mYear) {
  case 2006:
    if (ee1) {
      for (int i=0;i<Nee1;i++) {
	const EEdsm1 &br = ee1[i];           //Select data for board 1 or 2
	int maxTPthr=0;
	int maxHTTPthr=0;
	for(int j=0;j<br.getNc(); j++) {
	  int ix=j+i*br.getNc();
	  int ijp=ix/2;
	  ee1outJPadc[ijp]+=br.getInpTPsum(j);  
	  // Note jet patches are numbered in DSM input order.  So ijp=0 is FEE crate 3.
	  ee1out3JPadc[i]+=br.getInpTPsum(j);
	  //int val= br.getHTthr(j);
	  //if(ee1outHT[ijp]< val)ee1outHT[ijp]=val;
	  //if(maxTPthr< br.getTPthr(j)) maxTPthr= br.getTPthr(j);
	  //if(maxHTTPthr< br.getHTTPthr(j)) maxHTTPthr= br.getHTTPthr(j);
	}
	ee1outTPthrMax[i]  =maxTPthr;
	ee1outHTTPthrMax[i]=maxHTTPthr;
      }
    }
    //Add some further processing to check jet patch sums for passing thresholds
    //and form adjacent patch sums.
    for (int i=0;i<EEnJetPatch;i++) {
      AdjJPsum[i]=ee1outJPadc[i]+ee1outJPadc[(i+1)%EEnJetPatch]; //sum adj patches
    }
    break;

  case 2009:
    DSMAlgo_EE101_2009()(EE101);
    DSMAlgo_EE102_2009()(EE102);

    // Get jet patch sums

    ee1outJPadc[0] = EE102.info[0];
    ee1outJPadc[1] = EE102.info[1];
    ee1outJPadc[2] = EE102.info[2];

    ee1outJPadc[3] = EE101.info[0];
    ee1outJPadc[4] = EE101.info[1];
    ee1outJPadc[5] = EE101.info[2];

    // Calculate adjacent jet patch sums

    for (int jp = 0; jp < 6; ++jp)
      AdjJPsum[jp] = ee1outJPadc[jp] + ee1outJPadc[(jp+1)%6];

    break;
  }
}

//--------------------------------------------------
void EEdsmAna::emulDsm2() {
    if (ee2) {
	ee2outHT=ee2->get3JPHTthr(0);
	if(ee2outHT<ee2->get3JPHTthr(1))ee2outHT=ee2->get3JPHTthr(1);
    }
}

//--------------------------------------------------
void EEdsmAna::readDsm0(const unsigned char *EEMC) {
  /*Gets level 0 DSM inputs and reorders them.  They
    are stored 7 -> 0 followed by 15 -> 8.  Output is
    ordered as physically input to boards, 0 -> 15.
  */
    if (EEMC && ee0) {
        //--------------------- initialize input to DSM level-0
	for(int ibr = 0;ibr < Nee0;ibr++) { // index, boards ID= ibr+1
	    int k = 16 * ibr; // begin of data for given board
	    // fill in lower 8 words [ 0,1...,7] 
	    for(int i = 0;i < 8;i++) ee0[ibr].setBite(7 - i, EEMC[k + i]);
	    // fill in higher 8 words [8,9,....,15]
	    for(int i = 8;i < 16;i++) ee0[ibr].setBite(23 - i, EEMC[k + i]);
	    ee0[ibr].unpack();
	}
    }
}

//--------------------------------------------------
void EEdsmAna::readDsm1(const unsigned short *EEMC_l1) {
  /* Gets level 1 DSM inputs (output of level 0) and
     reorders them.  They are stored 3 ->0 followed by
     7 -> 4 followed by 11 -> 8 and 15 -> 12.  Only 0-5
     and 8-13 are used, the first group for the first 
     board.  Output is ordered as physically input to
     the board.  For first board the order is crate 3,
     4 then 5, and for second board crate 6, 1 then 2.
  */

  switch (mYear) {
  case 20006:
    if (EEMC_l1 && ee1) {
      for(int i = 0;i < Nee1;i++) {
	ee1[i].setWord(0, EEMC_l1[3 + i*8]);
	ee1[i].setWord(1, EEMC_l1[2 + i*8]);
	ee1[i].setWord(2, EEMC_l1[1 + i*8]);
	ee1[i].setWord(3, EEMC_l1[0 + i*8]);
	ee1[i].setWord(4, EEMC_l1[7 + i*8]);
	ee1[i].setWord(5, EEMC_l1[6 + i*8]);
      }
    }
    break;

  case 2009:
    if (EEMC_l1) {
      EE101.channels[0] = EEMC_l1[3];
      EE101.channels[1] = EEMC_l1[2];
      EE101.channels[2] = EEMC_l1[1];
      EE101.channels[3] = EEMC_l1[0];
      EE101.channels[4] = EEMC_l1[7];
      EE101.channels[5] = EEMC_l1[6];
      EE101.channels[6] = EEMC_l1[5];
      EE101.channels[7] = EEMC_l1[4];

      EE102.channels[0] = EEMC_l1[11];
      EE102.channels[1] = EEMC_l1[10];
      EE102.channels[2] = EEMC_l1[ 9];
      EE102.channels[3] = EEMC_l1[ 8];
      EE102.channels[4] = EEMC_l1[15];
      EE102.channels[5] = EEMC_l1[14];
      EE102.channels[6] = EEMC_l1[13];
      EE102.channels[7] = EEMC_l1[12];
    }
    break;
  }
}

//--------------------------------------------------
void EEdsmAna::readDsm2(const unsigned short *EMC) {
    if (EMC && ee2 && be2) {
	//Endcap
	ee2->setWord(0, EMC[5]);
	ee2->setWord(1, EMC[4]);
	//Barrel
	be2[0].setWord(0, EMC[3]);
	be2[0].setWord(1, EMC[2]);
	be2[1].setWord(0, EMC[1]);
	be2[1].setWord(1, EMC[0]);
	be2[2].setWord(0, EMC[7]);
	be2[2].setWord(1, EMC[6]);
    }
}

//--------------------------------------------------
void EEdsmAna::readDsm3( const unsigned short *lastDSM) {
    if (lastDSM && ee3) {
	ee3->setWord(0, lastDSM[0]);
    }
}

//--------------------------------------------------
void EEdsmAna::histoDsm0() {
  if (ee0) {
    for(int i = 0;i < EEnTPphi;i++) {
      for(int j = 0;j < EEnTPeta;j++) {
	const EEmapTP &y = eeMapTP[i][j];
	//if (y) {
	int ibr = y.brdIn - 1;
	int ch = y.chIn;
	int iJP = y.JPid - 1;
	if ((iJP >= 0) && (iJP < EEnJetPatch)) {
	  H0inHT[iJP]->Fill(ee0[ibr].getInpHT6bit(ch), y.TPid);
	  H0inTP[iJP]->Fill(ee0[ibr].getInpTP6bit(ch), y.TPid);
	  H0inHTall->Fill((10 * ibr) + ch, ee0[ibr].getInpHT6bit(ch));
	  H0inTPall->Fill((10 * ibr) + ch, ee0[ibr].getInpTP6bit(ch));
	}
	//}
      }
    }
  }
}

//--------------------------------------------------
void EEdsmAna::histoDsm1(){
  switch (mYear) {
  case 2006:
    if (ee1) {
      for (int i = 0;i < Nee1;i++) {
	const EEdsm1 &br = ee1[i];
	//if (br) {
	for(int j = 0;j < br.getNc();j++) {
	  int ix = j + (i * br.getNc());
	  // .... TP ..................
	  H1inTPvEmu[ix]->Fill(ee0outTPadc[ix], br.getInpTPsum(j));
	  //..... HT ............
	  //H1inHTvEmu[ix]->Fill(ee0outHTadc[ix], br.getHTthr(j));
	}
	//}
      }
    }

    /* 2006
       6) For the upper 4 plots ( H4jpFreq[i]), change the JP sum cuts to:
       JP sum > 16
       JP sum in range [24,29]
       JP sum in range [30,39]
       JP sum > 40
    */

    for (int i = 0;i < EEnJetPatch;i++) {     //histos for jet patch sums
      int steve_jp = ((i + 2) % EEnJetPatch) + 1;  //stored by dsm input number 0-5 Want by Steves jp #
      H4jpSums[i]->Fill(ee1outJPadc[i]);
      int JPene = ee1outJPadc[i];
      if(JPene > mJPthr[0]) H4jpFreq[0]->Fill(steve_jp);
      if(JPene > mJPthr[3]) H4jpFreq[3]->Fill(steve_jp);
      else if(JPene > mJPthr[2]) H4jpFreq[2]->Fill(steve_jp);
      else if(JPene > mJPthr[1]) H4jpFreq[1]->Fill(steve_jp);
    
      H5jpPed->Fill(steve_jp, ee1outJPadc[i]);
      if (ee1outJPadc[i] > mJPthr[1]) H5jpFreq->Fill(steve_jp); 
      int j = (i + 1) % EEnJetPatch;
      H4adjPcor[i]->Fill(ee1outJPadc[i], ee1outJPadc[j]);
      H4adjpSums[i]->Fill(AdjJPsum[i]);

      if ((ee1outJPadc[i] > mJPthr[1]) && (ee1outJPadc[(i + 1) % EEnJetPatch] > mJPthr[1])) H4adjpFreq->Fill(steve_jp);
    }
    break;

  case 2009:
    // The jet patch numbering scheme (0-5) follows that in the West Barrel:
    // JP0:  150 degrees (Steve's Jet Patch 6/FEE Crate 6)
    // JP1:   90 degrees (Steve's Jet Patch 1/FEE Crate 1)
    // JP2:   30 degrees (Steve's Jet Patch 2/FEE Crate 2)
    // JP3:  -30 degrees (Steve's Jet Patch 3/FEE Crate 3)
    // JP4:  -90 degrees (Steve's Jet Patch 4/FEE Crate 4)
    // JP5: -150 degrees (Steve's Jet Patch 5/FEE Crate 5)
    // See http://www.iucf.indiana.edu/U/STAR/eemc_proj/electr_trig_daq/eemc_trigger_patches_annotated.pdf

    static const int steve_jp[] = { 6, 1, 2, 3, 4, 5 };

    for (int jp = 0; jp < 6; ++jp) {
      H4jpSums[jp]->Fill(ee1outJPadc[jp]);

      // JP threshold histogram indexes:
      // 0 =       ADC <= JP0
      // 1 = JP0 < ADC <= JP1
      // 2 = JP1 < ADC <= JP2
      // 3 = JP2 < ADC

      if (ee1outJPadc[jp] <= EE101.registers[0])
	H4jpFreq[0]->Fill(steve_jp[jp]);
      else if (ee1outJPadc[jp] > EE101.registers[0] && ee1outJPadc[jp] <= EE101.registers[1])
	H4jpFreq[1]->Fill(steve_jp[jp]);
      else if (ee1outJPadc[jp] > EE101.registers[1] && ee1outJPadc[jp] <= EE101.registers[2])
	H4jpFreq[2]->Fill(steve_jp[jp]);
      else if (ee1outJPadc[jp] > EE101.registers[2])
	H4jpFreq[3]->Fill(steve_jp[jp]);

      H5jpPed->Fill(steve_jp[jp], ee1outJPadc[jp]);
      if (ee1outJPadc[jp] > EE101.registers[1]) H5jpFreq->Fill(steve_jp[jp]); // JP1 threshold

      H4adjPcor[jp]->Fill(ee1outJPadc[jp], ee1outJPadc[(jp+1)%6]);
      H4adjpSums[jp]->Fill(AdjJPsum[jp]);
    }
    break;
  }
}

//--------------------------------------------------
void EEdsmAna::histoDsm2() {
    if (ee2) {
	//------------ HT ---------------
	/*for (int iJ = 0;iJ < EEnHalf;iJ++) {
	    int max = 0;
	    for(int i = 3 * iJ;i < ((3 * iJ) + 3);i++) {
		if (max < ee1outHT[i]) max = ee1outHT[i];
	    }
	}*/

  //------------ TP & HTTP since 2006 ---------------
  /*
    4) The upper 2 plots need to be completely redefined. The result will be interpretable by me 
    and probably by few others until I train them, but it's not easy to do better unless we have 
    access to the register values that tell DSM exactly which thresholds we are choosing from TP
    and HT x TP bits. We want to change both histograms to be 16 channels in x by 4 channels in y. 
    The left-hand plot should still deal with the layer 1 DSM that Falk labels for JP
    0+1+2, and the right-hand plot for JP3+4+5.

    To form the 4-bit word for x in each of the above histograms, do the following:
    a) loop over the 6 inputs to the layer 1 DSM and extract bits 12-13; place the highest of 
    these 6 2-bit values into the upper 2 bits of the address for x.
    b) repeat (a), but now for bits 14-15 of the 6 inputs; place the highest of these 6 2-bit 
    values into the lower 2 bits of the 4-bit address for x.
    x thus measures: (TP bits for highest threshold passed)*4 + (HTxTP bits for highest threshold 
    passed)

    For the y-value in these histograms, form a 2-bit word with bit 1=bit 9 of the corresponding
    layer 2 DSM input word bit 0=bit 7 of the corresponding layer 2 DSM input word (all bits 
    numbered starting from 0).
    y thus measures: (selected TP threshold passed?Y or N)*2
    +(selected HT x TP threshold passed?Y or N)
    
  */

	for(int iJ = 0;iJ < EEnHalf;iJ++) {
	    int x = (ee1outTPthrMax[iJ] * 4) + ee1outHTTPthrMax[iJ];
	    int y = (ee2->getTPthr(iJ) * 2) + ee2->getHTTPthr(iJ);
	    H2inHTTP[iJ]->Fill(x,y);
	}

  /* in 2006+
    2 1D plots, one for each layer 1 DSM (i.e., Falk JP0+1+2 and Falk JP3+4+5).
    The x-axis should be 32 channels and represent the value extracted from bits 0-4 of
    the corresponding EEMC input to the layer 2 DSM. The plots should be labeled:
    ETOT FOR JP0+1+2 and ETOT FOR JP3+4+5.
  */

	for (int iJ = 0;iJ < EEnHalf;iJ++) H1inEtot[iJ]->Fill(ee2->get3JPsum(iJ));
    }

    if (ee3) {
  //Total energy histos
  /*
    64 channels x by 2 channels y :
    x-axis (label EEMC ETOT emulated) ; value = sum over
    2 EEMC inputs to layer 2 DSM the values in bits 0-4 for each input.
    
    y-axis (label EEMC ETOT BIT): value = bit 11 of layer
    2 DSM output (or of TCU input).
  */
	unsigned short sumE = ee2->get3JPsum(0) + ee2->get3JPsum(1);
	HEetot[ee3->getEndcapEsumthr1bit()]->Fill(sumE);
  
  //BARREL histos 2006
  /*
    256 channels x by 2 channels y:
    x-axis (label BEMC ETOT emulated) ; value = sum over
    6 BEMC inputs to layer 2 DSM the values in bits 0-4 for each input.
    
    y-axis (label BEMC ETOT BIT): value = bit 4 of layer
    2 DSM output (or of TCU input).
  */
	unsigned short sumB = 0;
	for(int i = 0;i < Nbe2;i++) {
	    sumB += be2[i].get3JPsum(0);
	    sumB += be2[i].get3JPsum(1);
	}
	HBetot[ee3->getBarreEsumThr1bit()]->Fill(sumB);

  /*
    256 channels x by 2 channels y:
    x-axis (label B+EEMC ETOT emulated) ; value = sum over
    6 BEMC+2 EEMC inputs to layer 2 DSM the values in bits 0-4 for each input.
    
    y-axis (label B+EEMC ETOT BIT): value = bit 15 of layer
    2 DSM output (or of TCU input).
  */

	HBEetot[ee3->getEtotThr1bit()]->Fill(sumB + sumE);  
    }
}

//--------------------------------------------------
void EEdsmAna::histoDsm3() {
    if (ee3) {
/*
 The plot should be replaced with a 4 x 4 histo:
x-axis = 2-bit word constructed from:
bit 0=OR of bit 7 on two EEMC inputs to layer 2 DSM bit 1=OR of bit 9 on two EEMC inputs to layer 2 DSM y-axis = 2-bit word constructed from:
bit 0=bit 12 of layer 2 DSM output (or TCU input) bit 1=bit 14 of layer 2 DSM output (or TCU input)

This histogram should have the "normal" appearance of a diagonal band; the x-axis is the emulated TP and HT x TP inputs to layer 2, and the y-axis is the actual output from layer 2.

 */
	bool x_b0 = ee1outTPthrMax[0] || ee1outTPthrMax[1];
        bool x_b1 = ee1outHTTPthrMax[0] || ee1outHTTPthrMax[1];
	int x = (x_b1 * 2) + x_b0; 
	int y = (ee3->getEndcapHTTPthr1bit() * 2) +ee3->getEndcapTPthr1bit();
	H3inHTTP->Fill(x, y);
    }
}

//--------------------------------------------------
void EEdsmAna::printDsm0map() const {
  printf("TP Map level-0 \n      ");
  for(int j=0;j< EEnTPeta;j++)
    printf("        iEta=%d        ",j);
  printf("\n              ");
  for(int j=0;j< EEnTPeta;j++)
    printf("JP/TP brd:ch->outCh   ");
  for(int i=0;i<EEnTPphi; i++) {
    int phi=111-i*12;
    if(phi<0) phi+=360;
    printf("\nPhi/deg=%3d   ",phi);
    for(int j=0;j< EEnTPeta;j++) {
       EEmapTP *y=&eeMapTP[i][j];
      printf("%d/%2d %2d:%2d -> %2d      ",y->JPid,y->TPid,y->brdIn,y->chIn,y->chOut);
    }
  }
  printf("\n");
}

//--------------------------------------------------
void EEdsmAna::printAllEndcap(int k) const {
  printf("\n\nEEdsmAna(%s)::print(eve=%d) , year=%d \n",myName,nTot,mYear);
  for(int i=0;i<Nee0;i++) {
    printf("\n----------- level-0 Board %2d ",i+1); 
    ee0[i].print();
  }
  printf("\n----------- level-0 emulated output \n ch =");
  for(int i=Nee0out-1;i>=0; i--) printf("  %4d ",i);
  printf("\n TP =");
  for(int i=Nee0out-1;i>=0; i--) printf("  %4d ",ee0outTPadc[i]);
  printf("\nHTadc =");
  for(int i=Nee0out-1;i>=0; i--) printf("  %4d ",ee0outHTadc[i]);
  printf("\n\n----------- level-1 -----------------\n "); 
  for(int i=0;i<Nee1;i++) {
    printf("\n----------- level-1 Board %2d , ",i+1); 
    ee1[i].print();
    printf("emul out 3x.9 JP_Falk(%d+%d+%d) energy/dec: 13bit=%d  8bit=%d  mxTPthr=%d mxHTTPthr=%d\n",3*i,3*i+1,3*i+2,ee1out3JPadc[i],ee1out3JPadc[i]>>5, ee1outTPthrMax[i], ee1outHTTPthrMax[i]);
  }
  printf("\n----------- level-1 emulated output \n JP_Falk =");
  int Njp=EEnJetPatch;
  for(int i=Njp-1;i>=0; i--) printf("  %4d ",i);
  printf("\n JP_Steve=");
  for(int i=Njp-1;i>=0; i--) printf("  %4d ",(i+2)%6+1);
  printf("\n JPsum   =");
  for(int i=Njp-1;i>=0; i--) printf("  %4d ",ee1outJPadc[i]);
  printf("\n HTthr   =");
  for(int i=Njp-1;i>=0; i--) printf("  %4d ",ee1outHT[i]);
  if(mYear<2006) {
    printf("\n AdjJPsum=");
    for(int i=Njp-1;i>=0; i--) printf("  %4d ",AdjJPsum[i]);
  }
  printf("\n");
  printf("emul: orTPbit=%d  orHTTPbit=%d \n",ee1outTPthrMax[0]||ee1outTPthrMax[1], ee1outHTTPthrMax[0] ||ee1outHTTPthrMax[1]);
  printf("\n\n----------- level-2 ----------------- \n"); 
  ee2->print();
  printf("\n\n----------- level-3 ( aka TCU) ----------- \n"); 
  ee3->print();
}

//--------------------------------------------------
void EEdsmAna::printAllBarrel(int k) const {
  printf("\n\nBEdsmAna(%s)::print(eve=%d) , year=%d \n",myName,nTot,mYear);
  for(int i=0;i<Nbe2;i++) {
    printf("\n----------- level-2 BARREL Board %2d ",i+1); 
    be2[i].print();
  }
  printf("\n\n");
}

//--------------------------------------------------
void EEdsmAna::usePed(const Char_t *fName){
    //......................... load ped for L-0
    LOG_INFO << "EEdsmAna(" << myName << ") reads pedestals from " << fName << endm;
    FILE *fd = fopen(fName, "r");
    if (fd) {
	for (Int_t ibr = 0;ibr < Nee0;ibr++) {
	    for (Int_t ch = 0;ch < ee0[ibr].getNc();ch++) {
		int a = 0, b = 0, c = 0;
		int ret = fscanf(fd, "%d %d %d", &a, &b, &c);
	        if ((ret == 3) && (a == (ibr + 1)) && (b == ch)) {
		    ped0[ibr][ch] = c;
		} else {
		    LOG_ERROR << "Bad file format " << fName << ": got ret=" << ret << ", a=" << a << ", b=" << b << ", c=" << c << endm;
		}
	    }
	}
	fclose(fd);
	fd = 0;
    } else {
	LOG_ERROR << "Cannot read file " << fName << endm;
    }
}

void EEdsmAna::saveHisto(TFile *f) const {
    if (f) f->cd();


    // .................level-0 input, 2D, HT ..............
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	if (H0inHT[iJ]) H0inHT[iJ]->Write();
    }

    if (H0inHTall) H0inHTall->Write();

    // ........level-0 input, 2D, TP sum
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	if (H0inTP[iJ]) H0inTP[iJ]->Write();
    }

    if (H0inTPall) H0inTPall->Write();

    // .................level-1 input, 2D
    //================================ TP 

    for(int iJ = 0;iJ < EEnHalfJetPatch;iJ++) {
	if (H1inTPvEmu[iJ]) H1inTPvEmu[iJ]->Write();
    }

    //================================ HT 
    for(int iJ = 0;iJ < EEnHalfJetPatch;iJ++) {
	if (H1inHTvEmu[iJ]) H1inHTvEmu[iJ]->Write();
    }

    // .................level-2 input

    //================================ HT 
    for(int iJ = 0;iJ < EEnHalf;iJ++) {
	if (H2inHTTP[iJ]) H2inHTTP[iJ]->Write();
    }

    //================================ half Etot sum
    for(int iJ = 0;iJ < EEnHalf;iJ++) {
	if (H1inEtot[iJ]) H1inEtot[iJ]->Write();
    }

    // Total energy histos
    for(int iJ = 0;iJ < mxEtotBit;iJ++) {
        if (HEetot[iJ]) HEetot[iJ]->Write();
    
        if (HBetot[iJ]) HBetot[iJ]->Write();

        if (HBEetot[iJ]) HBEetot[iJ]->Write();
    }

    // .................level-3 input
    //================================ HT 
    if (H3inHTTP) H3inHTTP->Write(); 

    //..................Jet Patch sums
    //=================================summed patch spectra
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	if (H4jpSums[iJ]) H4jpSums[iJ]->Write();
    }

    //=================================Jet patch over threshold
    for (int iJ = 0;iJ < EEnThresh;iJ++) {
	if (H4jpFreq[iJ]) H4jpFreq[iJ]->Write();
    }

    //=================================summed adjacent patch spectra
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	if (H4adjpSums[iJ]) H4adjpSums[iJ]->Write();
    }

    //=================================adjacent patch correlation
    for(int iJ = 0;iJ < EEnJetPatch;iJ++) {
	if (H4adjPcor[iJ]) H4adjPcor[iJ]->Write();
    }

    { //====================freq summed adjacent patch spectra over thresh
	if (H4adjpFreq) H4adjpFreq->Write();
    }

    {
	// added in 2005 
	if (H5jpPed) H5jpPed->Write(); 
    }
    
    {
	if (H5jpFreq) H5jpFreq->Write();

	if (H5jpHot) H5jpHot->Write(); // is _not_ filled by sorter
    }
}

