#include <TH2.h>
#include "StEemcTriggerSimu.h"
#include "TObjArray.h"
#include <cassert>

ClassImp(StEemcTriggerSimu);

//==================================================
//==================================================
void  
StEemcTriggerSimu::initHisto() {
  assert(mHList);
  memset(hA,0,sizeof(hA));

  hA[0]=0;
  hA[1]=new TH1F("trgID", "trigger ID for input; trigger ID", 10,0,5);


  //................. DSM0 inputs  .........................  
  hA[10]=new TH2F("inpHTtrg0", "TRG input HT-6bit, DSM0; Hank's TP index", 90,-0.5,89.5, 65,-0.5,64.5);
  hA[11]=new TH2F("emuHTadc0", "ADC-emulated HT-6bit, DSM0; Hank's TP index", 90,-0.5,89.5, 65,-0.5,64.5);
  hA[12]=new TH2F("HTdiff0", "HT-6bit diff, ADC-emulated vs DSM0-input; Hank's TP index", 90,-0.5,89.5, 141,-70.5,70.5);
  hA[13]=new TH1F("HTerr0", "HT-6bit error frequency, ADC-emulated vs DSM0-input; Hank's TP index", 90,-0.5,89.5);
 

  hA[20]=new TH2F("inpTPtrg0", "TRG input TP-6bit, DSM0; Hank's TP index", 90,-0.5,89.5, 65,-0.5,64.5);
  hA[21]=new TH2F("emuTPadc0", "ADC-emulated TP-6bit, DSM0; Hank's TP index", 90,-0.5,89.5, 65,-0.5,64.5);
  hA[22]=new TH2F("TPdiff0", "TP-6bit diff, ADC-emulated vs DSM0-input; Hank's TP index", 90,-0.5,89.5, 141,-70.5,70.5);
  hA[23]=new TH1F("TPerr0", "TP-6bit error frequency, ADC-emulated vs DSM0-input; Hank's TP index", 90,-0.5,89.5);




  //................. DSM1 inputs  .........................
  hA[150]=new TH2F("inpTPsumTrg1", "DSM1 input TPsum; DSM1 input channel", 24,-0.25,11.75, 1024,-0.5,1023.5);
  hA[151]=new TH2F("emuTPsumtrg1", "DSM0 emulated TPsum; DSM0 output channel", 24,-0.25,11.75, 1024,-0.5,1023.5);
  hA[152]=new TH2F("TPsumDiffDSM0", "TPsum diff, DSM0-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75, 2101,-1050.5,1050.5);
  hA[153]=new TH1F("TPsumErrFreqDSM0", "TPsum error frequency, DSM0-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75);
  hA[154]=new TH2F("adcEmuTPsumtrg1", "ADC-emulated TPsum; DSM0 output channel", 24,-0.25,11.75, 1024,-0.5,1023.5);
  hA[155]=new TH2F("adcTPsumDiffDSM0", "TPsum diff, ADC-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75, 2101,-1050.5,1050.5);
  hA[156]=new TH1F("adcTPsumErrFreqDSM0", "TPsum error frequency, ADC-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75);
  hA[157]=new TH2F("TPsumDiffDSM0in", "TPsum diff zoom in, DSM0-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75, 201,-100.5,100.5);
  hA[158]=new TH2F("adcTPsumDiffDSM0in", "TPsum diff zoom in, ADC-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75, 201,-100.5,100.5);


  hA[160]=new TH2F("inpHTtrg1", "DSM1 input HT2bit; DSM1 input channel", 24,-0.25,11.75, 4,-0.5,3.5);
  hA[161]=new TH2F("emuHTtrg1", "DSM0 emulated HT2bit; DSM0 output channel", 24,-0.25,11.75, 4,-0.5,3.5);
  hA[162]=new TH2F("HTdiffDSM0", "HT2bit diff, DSM0-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75, 17,-8.5,8.5);
  hA[163]=new TH1F("HTerrFreqDSM0", "HT2bit error frequency, DSM0-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75);
  hA[164]=new TH2F("adcEmuHTtrg1", "ADC-emulated HT2bit; DSM0 output channel", 24,-0.25,11.75, 4,-0.5,3.5);
  hA[165]=new TH2F("adcHTdiffDSM0", "HT2bit diff, ADC-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75, 17,-8.5,8.5);
  hA[166]=new TH1F("adcHTerrFreqDSM0", "HT2bit error frequency, ADC-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75);


  hA[170]=new TH2F("inpHTTPtrg1", "DSM1 input HTTP2bit; DSM1 input channel", 24,-0.25,11.75, 4,-0.5,3.5);
  hA[171]=new TH2F("emuHTTPtrg1", "DSM0 emulated HTTP2bit; DSM0 output channel", 24,-0.25,11.75, 4,-0.5,3.5);
  hA[172]=new TH2F("HTTPdiffDSM0", "HTTP2bit diff, DSM0-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75, 17,-8.5,8.5);
  hA[173]=new TH1F("HTTPerrFreqDSM0", "HTTP2bit error frequency, DSM0-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75);
  hA[174]=new TH2F("adcEmuHTTPtrg1", "ADC-emulated HTTP2bit; DSM0 output channel", 24,-0.25,11.75, 4,-0.5,3.5);
  hA[175]=new TH2F("adcHTTPdiffDSM0", "HTTP2bit diff, ADC-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75, 17,-8.5,8.5);
  hA[176]=new TH1F("adcHTTPerrFreqDSM0", "HTTP2bit error frequency, ADC-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75);


  hA[180]=new TH2F("inpTPtrg1", "DSM1 input TP2bit; DSM1 input channel", 24,-0.25,11.75, 4,-0.5,3.5);
  hA[181]=new TH2F("emuTPtrg1", "DSM0 emulated TP2bit; DSM0 output channel", 24,-0.25,11.75, 4,-0.5,3.5);
  hA[182]=new TH2F("TPdiffDSM0", "TP2bit difference, DSM0-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75, 17,-8.5,8.5);
  hA[183]=new TH1F("TPerrFreqDSM0", "TP2bit error frequency, DSM0-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75);
  hA[184]=new TH2F("adcEmuTPtrg1", "ADC-emulated TP2bit; DSM0 output channel", 24,-0.25,11.75, 4,-0.5,3.5);
  hA[185]=new TH2F("adcTPdiffDSM0", "TP2bit diff, ADC-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75, 17,-8.5,8.5);
  hA[186]=new TH1F("adcTPerrFreqDSM0", "TP2bit error frequency, ADC-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75);


  hA[190]=new TH2F("inp16bitTrg1", "DSM1 input 16bit; DSM1 input channel", 24,-0.25,11.75, 4096,-0.5,4095.5);
  hA[191]=new TH2F("emu16bitTrg1", "DSM0 emulated 16bit; DSM0 output channel", 24,-0.25,11.75, 4096,-0.5,4095.5);
  hA[192]=new TH2F("16bitdiffDSM0", "16bit diff, DSM0-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75, 8191,-4095.5,4095.5);
  hA[193]=new TH1F("16bitErrFreqDSM0", "16bit error frequency, DSM0-emu DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75);
  hA[194]=new TH2F("adcEmu16bitTrg1", "ADC-emulated 16bit; DSM0 output channel", 24,-0.25,11.75, 4096,-0.5,4095.5);
  hA[195]=new TH2F("adc16bitDiffDSM0", "16bit diff, ADC-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75, 8191,-4095.5,4095.5);
  hA[196]=new TH1F("adc16bitErrFreqDSM0", "16bit error frequency, ADC-emu vs DSM1-inp; DSM0 output (DSM1 input) channel", 24,-0.25,11.75);




  //................. DSM2 inputs  .........................
  int n1=16;
  float x1=-0.25,x2=7.75;
  hA[500]=new TH2F("inpTPsum5bitTrg2", "DSM2 input TPsum5bit; X-axis:0,1=E-EMC, 2..7=B-EMC", n1,x1,x2, 32,-0.5,31.5);
  hA[501]=new TH2F("emuTPsum5bitTrg2", "DSM1 emulated TPsum5bit; X-axis:0,1=E-EMC, 2..7=B-EMC", n1,x1,x2, 32,-0.5,31.5);
  hA[502]=new TH2F("TPsum5bitDiffDSM1", "TPsum5bit diff, DSM1-emu vs DSM2-inp;  X-axis:0,1=E-EMC, 2..7=B-EMC",n1,x1,x2, 71,-35.5,35.5); 
  hA[503]=new TH1F("TPerrFreqDSM1", "TPsum5bit error freq, DSM1-emu vs DSM2-inp;X-axis:0,1=E-EMC, 2..7=B-EMC ", n1,x1,x2);
  hA[504]=new TH2F("adcEmuTPsum5bitTrg2", "ADC-emulated TPsum5bit; X-axis:0,1=E-EMC, 2..7=B-EMC", n1,x1,x2, 32,-0.5,31.5);
  hA[505]=new TH2F("adcTPsum5bitDiffDSM1", "TPsum5bit diff, ADC-emu vs DSM2-inp; X-axis:0,1=E-EMC, 2..7=B-EMC",n1,x1,x2, 71,-35.5,35.5); 
  hA[506]=new TH1F("adcTPerrFreqDSM1", "TPsum5bit error freq, ADC-emu vs DSM2-inp;X-axis:0,1=E-EMC, 2..7=B-EMC ", n1,x1,x2);


  hA[510]=new TH2F("inpJP_HT_HTTPthr_TPthrTrg2", "DSM2 input JP2bit, HT2bit, HTTPthr1bit, TPthr1bit; DSM2 Board Number", 24,-0.25,11.75, 4,-0.5,3.5);
  hA[511]=new TH2F("emuJP_HT_HTTPthr_TPthrTrg2", "DSM1 emulated JP2bit, HT2bit, HTTPthr1bit, TPthr1bit; DSM2 Board Number", 24,-0.25,11.75, 4,-0.5,3.5); 
  hA[512]=new TH2F("DSM1diff_JP_HT_HTTPthr_TPthr", "Diff, DSM1-emulated vs DSM2-input JP2bit, HT2bit, HTTPthr1bit, TPthr1bit; DSM2 Board Number", 24,-0.25,11.75, 17,-8.5,8.5); 
  hA[513]=new TH1F("JP_HT_HTTPthr_TPthr_DSM1errFreq", "Error frequency, DSM1-emu vs DSM2-inp of JP2bit, HT2bit, HTTPthr1bit, TPthr1bit; DSM2 Board Number", 24,-0.25,11.75); 
  hA[514]=new TH2F("adcEmuJP_HT_HTTPthr_TPthrTrg2", "ADC-emulated JP2bit, HT2bit, HTTPthr1bit, TPthr1bit; DSM2 Board Number", 24,-0.25,11.75, 4,-0.5,3.5); 
  hA[515]=new TH2F("adcDSM1diff_JP_HT_HTTPthr_TPthr", "Diff, ADC-emulated vs DSM2-input JP2bit, HT2bit, HTTPthr1bit, TPthr1bit; DSM2 Board Number", 24,-0.25,11.75, 17,-8.5,8.5); 
  hA[516]=new TH1F("adcJP_HT_HTTPthr_TPthr_DSM1errFreq", "Error frequency, ADC-emu vs DSM2-inp of JP2bit, HT2bit, HTTPthr1bit, TPthr1bit; DSM2 Board Number", 24,-0.25,11.75); 



  
  hA[520]=new TH2F("DSM2BoardDiff-inpJP_HT_HTTPthr_TPthrTrg2", "DSM2 board diff, DSM2-input JP2bit, HT2bit, HTTPthr1bit, TPthr1bit; DSM2 Board Number", 12,-0.5,11.5, 13,-6.5,6.5);
  hA[521]=new TH2F("DSM2BoardDiff-emuJP_HT_HTTPthr_TPthrTrg2", "DSM2 board diff, DSM1-emu JP2bit, HT2bit, HTTPthr1bit, TPthr1bit; DSM2 Board Number", 12,-0.5,11.5, 13,-6.5,6.5); 
  
  


  hA[550]=new TH1F("BarreEsum", "DSM2 Barrel 5bit sum, TRG RED, EMU BLUE", 101,49.5,150.5);
  hA[551]=new TH1F("adcBarreEsum", "ADC-emu DSM2 Barrel 5bit sum", 101,49.5,150.5);
  hA[560]=new TH1F("BarreEsum1", "DSM2 inp Barrel 5bit sum", 101,49.5,150.5);


  hA[552]=new TH1F("EndcapEsum", "DSM2 Endcap 5bit sum, TRG RED, EMU BLUE", 51,9.5,60.5);
  hA[553]=new TH1F("adcEndcapEsum", "ADC-emu DSM2 Endcap 5bit sum", 51,9.5,60.5);
  hA[562]=new TH1F("EndcapEsum1", "DSM2 inp Endcap 5bit sum", 51,9.5,60.5);


  hA[554]=new TH1F("Etotal", "DSM2 Barrel + Endcap 5bit sum, TRG RED, EMU BLUE", 141,59.5,200.5);
  hA[555]=new TH1F("adcEtotal", "ADC-emu DSM2 Barrel + Endcap 5bit sum", 141,59.5,200.5);
  hA[564]=new TH1F("Etotal1", "DSM2 inp Barrel + Endcap 5bit sum", 141,59.5,200.5);


  hA[556]=new TH1F("adcEsumErrFreq", "Barrel, Endcap and Etotal difference frequency", 11,-0.5,10.5);

  for(int n=550; n<556; n++){
    int flag=n%2;
    if(flag==0) hA[n]->SetLineColor(kRed);
    else hA[n]->SetLineColor(kBlue);
    //hA[n]->SetListStyle(2);
  }

  for(int k=560; k<565; k=k+2){
    hA[k]->SetLineColor(kRed);
  }


  //................. DSM3 inputs  ...........................
  
  hA[600]=new TH2F("inpEndcap", "Endcap DSM3 input: JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit; Endcap DSM3 input: JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit", 12,-0.25,5.75, 4,-0.5,3.5);
  hA[601]=new TH2F("emuEndcap", "Endcap DSM2 emulated: JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit; Endcap DSM2 emu: JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit", 12,-0.25,5.75, 4,-0.5,3.5);
  hA[602]=new TH2F("EndcapDiff", "Diff, DSM2-emu vs DSM3-inp Endcap JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit; Endcap: JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit", 12,-0.25,5.75, 17,-8.5,8.5);
  hA[603]=new TH1F("EndcapErrFreq","Error frequency, DSM2-emu vs DSM3-inp Endcap JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit; Endcap: JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit", 12,-0.25,5.75);


  hA[610]=new TH2F("inpBarrel", "Barrel DSM3 input: JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit; Barrel DSM3 input: JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit", 12,-0.25,5.75, 4,-0.5,3.5);
  hA[611]=new TH2F("emuBarrel", "Barrel DSM2 emulated: JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit; Barrel DSM2 emu: JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit", 12,-0.25,5.75, 4,-0.5,3.5);
  hA[612]=new TH2F("BarrelDiff", "Diff, DSM2-emu vs DSM3-inp Barrel JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit; Barrel:JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit", 12,-0.25,5.75, 17,-8.5,8.5);
  hA[613]=new TH1F("BarrelErrFreq","Error frequency, DSM2-emu vs DSM3-inp Barrel JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit; Barrel:JP2bit, HT2bit, EsumThr1bit, HTTP1bit, TP1bit", 12,-0.25,5.75);


  hA[620]=new TH2F("inpEtot", "DSM3 input Etot", 6,-0.5,5.5, 2,-0.5,1.5);
  hA[621]=new TH2F("emuEtot", "DSM2-emu Etot", 6,-0.5,5.5, 2,-0.5,1.5);
  hA[622]=new TH2F("EtotDiff", "Diff, DSM2-emu vs DSM3-inp Etot", 6,-0.5,5.5, 5,-2.5,2.5);
  hA[623]=new TH1F("EtotErrFreq","Error frequency, DSM2-emu vs DSM3-inp Etot", 6,-0.5,5.5);
  hA[624]=new TH2F("adcEmuEtot", "ADC-emulated Etot", 6,-0.5,5.5, 2,-0.5,1.5);
  hA[625]=new TH2F("adcEtotDiff", "Diff, ADC-emu vs DSM3-inp Etot", 6,-0.5,5.5, 5,-2.5,2.5);
  hA[626]=new TH1F("adcEtotErrFreq","Error frequency, ADC-emu vs DSM3-inp Etot", 6,-0.5,5.5);


  hA[690]=new TH2F("inpBarreSum", "DSM2 input barrel energy sum", 6,-0.5,5.5, 11,64.5,75.5); 
  
  //.... add histos to the list
  int i;
  for(i=0;i<mxAH;i++) {
    if( hA[i]==0) continue;
    mHList->Add( hA[i]);
  }

}

//
// $Log: StEemcTriggerHisto.cxx,v $
// Revision 1.6  2010/04/16 01:47:46  pibero
// Oops, forgot to include triggers before 2009. Thanks, Liaoyuan.
//
// Revision 1.5  2009/08/26 19:33:56  fine
// fix the compilation issues under SL5_64_bits  gcc 4.3.2
//
// Revision 1.4  2009/01/26 15:09:07  fisyak
// Add missing (in ROOT 5.22) includes
//
// Revision 1.3  2007/07/23 02:59:59  balewski
// cleanup, bbc for M-C still not working
//

