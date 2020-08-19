/*
  Results of measurment cpu per event for different compilers :
  4  gcc (GCC) 4.8.5 20150623 (Red Hat 4.8.5-28) is default compiler for Scientific Linux release 7.3 (Nitrogen)
  5  gcc (GCC) 5.3.1 20160406 (Red Hat 5.3.1-6)     devtoolset-4
  6  gcc (GCC) 6.3.1 20170216 (Red Hat 6.3.1-3)     devtoolset-6
  7  gcc (GCC) 7.3.1 20180303 (Red Hat 7.3.1-5)     devtoolset-7
  8  gcc (GCC) 8.3.1 20190311 (Red Hat 8.3.1-3)     devtoolset-8
  9  gcc (GCC) 9.1.1 20190605 (Red Hat 9.1.1-2)     devtoolset-9
  6  gcc (GCC) 6.3.0                                /cvmfs/sft.cern.ch/lcg/contrib/gcc/6.3.0/x86_64-centos7-gcc63-opt/bin/gcc
 10  gcc (GCC) 10.1.0                               /cvmfs/sft.cern.ch/lcg/releases/gcc/10.1.0-6f386/x86_64-centos7/bin/gcc

Processed 973 events from 2019AuAu200 on HLT machine l409.l4.bnl.local (xeon-phi-dev.starp.bnl.gov) on 0/15/2020 in .DEV2 with chain
root.exe -q -b bfc.C(2000,"P2019a,mtd,btof,BEmcChkStat,CorrY,OSpaceZ2,OGridLeakFull,evout,NoHistos,noTags,noRunco,StiCA,picoWrite,PicoVtxVpdOrDefault","/hlt/cephfs/daq/2019B/192/20192001/hlt_20192001_11_01_000.daq")'

rc.DEV2.sl73_gcc485_debug/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_gcc485/lib/libStiCA.so)      is loaded
rc.DEV2.sl73_gcc485_opt/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_gcc485/LIB/libStiCA.so)        is loaded
rc.DEV2.sl73_x8664_gcc1010_debug/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc1010/lib/libStiCA.so)        is loaded
rc.DEV2.sl73_x8664_gcc1010_opt/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc1010/LIB/libStiCA.so)  is loaded
rc.DEV2.sl73_x8664_gcc485_debug/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc485/lib/libStiCA.so)  is loaded
rc.DEV2.sl73_x8664_gcc485_opt/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc485/LIB/libStiCA.so)    is loaded
rc.DEV2.sl73_x8664_gcc531_debug/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc531/lib/libStiCA.so)  is loaded
rc.DEV2.sl73_x8664_gcc531_opt/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc531/LIB/libStiCA.so)    is loaded
rc.DEV2.sl73_x8664_gcc630_opt/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc630/LIB/libStiCA.so)    is loaded
rc.DEV2.sl73_x8664_gcc631_debug/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc631/lib/libStiCA.so)  is loaded
rc.DEV2.sl73_x8664_gcc631_opt/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc631/LIB/libStiCA.so)    is loaded
rc.DEV2.sl73_x8664_gcc7_debug/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc7/lib/libStiCA.so)      is loaded
rc.DEV2.sl73_x8664_gcc7_opt/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc7/LIB/libStiCA.so)        is loaded
rc.DEV2.sl73_x8664_gcc8_debug/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc8/lib/libStiCA.so)      is loaded
rc.DEV2.sl73_x8664_gcc8_opt/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc8/LIB/libStiCA.so)        is loaded
rc.DEV2.sl73_x8664_gcc9_debug/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc9/lib/libStiCA.so)      is loaded
rc.DEV2.sl73_x8664_gcc9_opt/hlt_20192001_11_01_000.log:QA :INFO  - Library libStiCA               [       StiCALib] (/net/l402/data/fisyak/STAR/packages/.DEV2/.sl73_x8664_gcc9/LIB/libStiCA.so)        is loaded
[l



[l409] ~/work/reco/2020/Timing/AuAu200_2019B $ 
foreach d ( rc.DEV2.sl73_gcc485_debug rc.DEV2.sl73_x8664_gcc1010_debug rc.DEV2.sl73_x8664_gcc485_debug rc.DEV2.sl73_x8664_gcc531_debug rc.DEV2.sl73_x8664_gcc630_opt \
rc.DEV2.sl73_x8664_gcc7_opt rc.DEV2.sl73_x8664_gcc8_opt rc.DEV2.sl73_x8664_gcc9_opt rc.DEV2.sl73_gcc485_opt    rc.DEV2.sl73_x8664_gcc1010_opt    rc.DEV2.sl73_x8664_gcc485_opt \
rc.DEV2.sl73_x8664_gcc531_opt    rc.DEV2.sl73_x8664_gcc631_debug  rc.DEV2.sl73_x8664_gcc7_debug  rc.DEV2.sl73_x8664_gcc8_debug  rc.DEV2.sl73_x8664_gcc9_debug)
mkdir TbyT_${d}; 
cd TbyT_${d}
ln -s ../rc.DEV2.sl73_x8664_gcc531_opt old
ln -s ../${d} new
MakeTbyT.pl 
lsf63 ~/xml/jobs.xml 
cd -
end
*/
#include "TAxis.h"
#include "TH1.h"
	      //________________________________________________________________________________
void cpu_compiler() {
  struct cpu_t {				
    Int_t         bin;
    const Char_t *opt;
    Float_t       cpu;
  };
  enum {NBins = 17};
  cpu_t CPU[NBins] = {
    { 1, "32b/485-g",          33.8727},
    { 2, "64b/485-g",          32.3714},
    { 3, "64b/531-g",          31.6395},
    { 4, "64b/631-g",          31.8687},
    { 5, "64b/7-g",            31.5965},
    { 6, "64b/8-g",            31.4382},
    { 7, "64b/9-g",            31.8512},
    { 8, "64b/1010-g",         32.4381},
    
    { 9, "32b/485-O",          15.8916},
    {10, "64b/485-O",          13.2109},
    {11, "64b/531-O",          12.6573},
    {12, "64b/630-O",          12.4409},
    {13, "64b/631-O",          12.8086},
    {14, "64b/7-O",            13.1371},
    {15, "64b/8-O",            12.7786},
    {16, "64b/9-O",            12.6349},
    {17, "64b/1010-O",         12.6213}
  };
  TH1F *cpuH = new TH1F("cpuH","cpu per event (seconds) versus compiler",NBins,0.5,NBins+.5);
  TH1F *ratH = new TH1F("ratH","cpu per event relative to 64b/gcc631-opt  versus compiler",NBins,0.5,NBins+.5);
  for (Int_t bin = 1; bin <= NBins; bin++) {
    Int_t i = bin - 1;
    cpuH->GetXaxis()->SetBinLabel(bin,CPU[i].opt);
    cpuH->SetBinContent(bin,CPU[i].cpu);
    ratH->GetXaxis()->SetBinLabel(bin,CPU[i].opt);
    ratH->SetBinContent(bin,CPU[i].cpu/CPU[12].cpu);
  }
	      }
