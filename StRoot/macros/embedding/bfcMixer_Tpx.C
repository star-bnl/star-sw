//////////////////////////////////////////////////////////////////////////
//
// Macro for running chain with different inputs
//
// Owner:  Yuri Fisyak
//
// $Id: bfcMixer_Tpx.C,v 1.53 2020/01/22 15:06:31 starembd Exp $
//
// $Log: bfcMixer_Tpx.C,v $
// Revision 1.53  2020/01/22 15:06:31  starembd
// added options for Run16 dAu39 and dAu20
//
// Revision 1.52  2018/03/26 02:43:13  zhux
// added P17id Run16 dAu200 chain.
//
// Revision 1.51  2018/03/08 08:41:59  zhux
// default is flat in rapidity, not pseudo-rapidity.
//
// Revision 1.50  2018/03/08 08:40:32  zhux
// added P17id Run14 AuAu200 chain.
//
// Revision 1.49  2018/02/08 02:02:38  zhux
// added Run16 AuAu200 HFT/MTD chain.
//
// Revision 1.48  2018/01/11 19:06:26  genevb
// Update B/EEMC simulation chain options and maker ordering
//
// Revision 1.47  2017/11/04 03:12:53  zhux
// added P16ig Run14 AuAu200 st_WB chain
//
// Revision 1.46  2017/10/13 05:36:18  zhux
// added lines for pvranking/primemode cut switches. the default is off.
//
// Revision 1.45  2017/10/12 13:31:06  zhux
// add "Run9 pp500 P16ib" chain setting.
//
// Revision 1.44  2017/10/12 13:02:27  zhux
// added run15 pp200 pAu200 chains
//
// Revision 1.43  2017/10/02 09:28:09  zhux
// added chain for Run15 FXT AuAu4.5/AuAl4.9
//
// Revision 1.42  2017/10/02 04:22:05  zhux
// updated run14 auau200 HFT chain. removed 'NoPXLIT,NoSstIT,NoIstIT' from the chain, removed pxlhit,isthit from the chain.
//
// Revision 1.41  2017/08/09 09:13:24  zhux
// added run14 auau200 (p16id) chain
//
// Revision 1.40  2016/04/05 13:32:45  zhux
// added chain for : Run13 pp500; Run14 AuAu200 HFT & MTD; Run12 CuAu200.
//
// Revision 1.39  2015/07/11 02:04:02  zhux
// chain for Run14 Au+Au 14.5 added.
//
// Revision 1.38  2014/01/24 16:30:45  zhux
// added chain for run12 p+p 200GeV
//
// Revision 1.37  2013/04/02 00:14:43  zhux
// added chain for run12 U+U 193GeV
//
// Revision 1.35  2012/09/28 16:02:42  zhux
// Chain for Run 9 p+p 200 GeV (P11id) added; 'Embedding' option removed from Chain3 (see ticket #2419).
//
// Revision 1.34  2012/04/18 03:47:34  zhux
// Corrected string name for Run 11 Au+Au 19.6 GeV chain
//
// Revision 1.33  2012/04/13 17:39:11  cpowell
// Added chain options for P10ikAuAu62
//
// Revision 1.32  2012/03/27 15:50:44  cpowell
// Added chain options for P10ikAuAu39 (same as P10ihAuAu39)
//
// Revision 1.31  2012/02/17 20:47:36  fisyak
// Remove nodefault option from chain3
//
// Revision 1.30  2012/02/17 15:01:24  didenko
// add run 2011 chains
//
// Revision 1.29  2012/01/14 02:15:28  zhux
// Geometry tag addition in Chain3 removed, Chain2 geometry tags are all updated to the latest version (on 2011.1.14).
//
// Revision 1.28  2011/12/05 16:06:34  zhux
// latest geometry (y2010c) used in simulation chain (chain2) for P10ik
//
// Revision 1.27  2011/09/23 02:47:40  cpowell
// Chain for p+p 200 P10ic production added. Setup for W embedding included.
//
// Revision 1.26  2011/08/04 19:50:01  cpowell
// Flag included to embed Pythia events. This excludes StPrepEmbedmaker from the chain and runs starsim before reconstruction.
//
// Revision 1.25  2011/07/18 06:27:39  zhux
// The chain for p+p 500 P09ig production added
//
// Revision 1.24  2011/03/03 08:32:07  hmasui
// Put P10ic chain back for p+p, deleted in 1.21 by accident
//
//////////////////////////////////////////////////////////////////////////

class StChain;
StChain  *Chain=0;
class StBFChain;
StBFChain *chain1, *chain2, *chain3;
//_____________________________________________________________________
void bfcMixer_Tpx(Int_t Nevents=100,
		  const Char_t *daqfile="/star/rcf/test/daq/2009/embed/st_physics_adc_10128048_raw_1320001.daq",
		  const Char_t *tagfile="/star/rcf/test/daq/2009/embed/st_physics_adc_10128048_raw_1320001.tags.root",
		  Double_t pt_low=0.1,
		  Double_t pt_high=5.0,
                  Double_t eta_low=-1.5,
                  Double_t eta_high=1.5,
                  Double_t vzlow = -150.0,
                  Double_t vzhigh = 150.0,
                  Double_t vr = 100.0,
		  Int_t pid=9,
		  Double_t mult=100,
                  std::vector<Int_t> triggers = 0,
                  const Char_t *prodName = "P08iepp",
                  const Char_t* type = "FlatPt",
									const bool bPythia = false,
									const Char_t *fzdfile="test.fzd"
									){
  // production chains for P08ic - p+p, Au+Au 9 GeV and d+Au
  TString prodP08iepp("DbV20081117 B2008a ITTF IAna ppOpt l3onl emcDY2 fpd ftpc trgd ZDCvtx NosvtIT NossdIT Corr4 OSpaceZ2 OGridLeak3D VFMCE -hitfilt");
//  TString prodP08icpp("DbV20080712,pp2008,ITTF,OSpaceZ2,OGridLeak3D,beamLine,VFMCE,TpxClu -VFPPV -hitfilt");
//  TString prodP08icAuAu9("DbV20080709 P2008 ITTF VFMCE -hitfilt");
//  TString prodP08icAuAu200("DbV20070101 P2008 ITTF VFMCE -hitfilt");  
//  TString prodP08icdAu("DbV20080712 P2008 ITTF OSpaceZ2 OGridLeak3D beamLine, VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP08iedAu("DbV20090213 P2008 ITTF OSpaceZ2 OGridLeak3D beamLine VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP10iapp("DbV20091001 pp2009c TpcRS ITTF OSpaceZ2 OGridLeak3D beamLine, VFMCE TpcRS -VFMinuit -hitfilt");

   // production chain for P10ic p+p RFF & FF
   TString prodP10icpp200("DbV20100301 pp2009c ITTF BEmcChkStat btof Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -hitfilt");
   // production chain for run 9 p+p 200 (P11id) RFF & FF
   TString prodP11idpp200("DbV20120908,pp2009d,ITTF,BEmcChkStat,btof,fmsdat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-hitfilt");
   // production chain for P09ig p+p 500 GeV RFF & FF
   TString prodP09igpp500("DbV20091225 pp2009c ITTF BEmcChkStat btof Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -hitfilt");
   // production chain for P11b p+p 500 GeV run 2009  st_W reproduction with fixed bug for Pt >= 20GeV (not using VFMCE)
   TString prodP11ibpp500("DbV20110310 OGGVoltErr pp2009c ITTF VFPPVnoCTB BEmcChkStat beamLine Corr4 OSpaceZ2 OGridLeak3D");
	 prodP11ibpp500 += " VFPPVnoCTB beamLine TpxClu -VFMinuit -hitfilt";

  // BES Run10 chains
  TString prodP10ikAuAu62("DbV20110413 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP10ihAuAu39("DbV20100909 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP10ikAuAu39("DbV20100909 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP10ihAuAu11("DbV20100821 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");
  TString prodP10ihAuAu7("DbV20100821 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");

  // Run10 Au+Au 200 GeV chain
  TString prodP10ikAuAu200("DbV20101213 P2010a pmdReco btof BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -VFMinuit -hitfilt");

   // Run11 Au+Au 200 GeV chain
  TString prodP11idAuAu200("DbV20111124 P2011a pmdReco btof mtdDat BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -VFMinuit -hitfilt");
  
   // Run11 Au+Au 27 GeV chain  
  TString prodP11idAuAu27("DbV20110911 P2011a btof mtddat pmdReco BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -VFMinuit -hitfilt");

  // Run11 Au+Au 19.6 GeV chain  
  TString prodP11idAuAu19("DbV20110820 P2011a btof mtddat pmdReco BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -VFMinuit -hitfilt");

   // Run11 pp 500 GeV chain  
  TString prodP11idpp500("DbV20110923 pp2011a btof mtddat fmsdat BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D VFMCE TpxClu -hitfilt");

  // Run12 U+U 193 GeV chain
  TString prodP12idUU193("DbV20120921,P2012b,AgML,mtdDat,btof,fmsDat,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu -VFMinuit -hitfilt");

  // Run12 pp200 chain
  TString prodP12idpp200("DbV20130212,pp2012b,AgML,mtdDat,btof,fmsDat,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-hitfilt");

  // Run13 pp500 chain
  TString prodP14igpp500("DbV20140905,pp2013b,StiHftP,mtd,btof,fmsDat,fgt,fgtPoint,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-hitfilt");

  // Run14 AuAu15 chain
  TString prodP14iiAuAu15("DbV20150110,P2014a,btof,mtd,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt");

  // Run14 AuAu200 chain (P15ic) (NO PXL/IST hits)
  TString prodP15icAuAu200("DbV20150316,P2014a,btof,mtd,mtdCalib,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt");

  // Run14 AuAu200 MTD chain
  TString prodP15ieAuAu200("DbV20150504,P2014a,btof,mtd,mtdCalib,pxlHit,istHit,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt");

  // Run12 CuAu200 chain
  TString prodP15ieCuAu200("DbV20150529,P2012b,AgML,mtd,btof,fmsDat,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt");

  // Run14 AuAu200 chain (P16id) (NO PXL/IST hits)
  TString prodP16idAuAu200("DbV20160418,P2014a,btof,mtd,mtdCalib,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt");

  // Run15 AuAu4.5 fxt chain
  TString prodP16iaAuAu5("DbV20160126,DbV20150920_tpc_Calibrations,P2015b,mtd,btof,BEmcChkStat,CorrX,VFMCE,TpxClu,-VFMinuit,-hitfilt");

  // Run15 pp200 chains
  TString prodP16idpp200("DbV20160418,DbV20161117_TPC_Calibrations,pp2015c,btof,mtd,mtdCalib,pp2pp,fmsDat,fmsPoint,fpsDat,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFPPVnoCTB,-VFPPV,-beamline,-hitfilt");

  // Run15 pAu200 chains
  TString prodP16idpAu200("DbV20160710,DbV20161117_TPC_Calibrations,pp2015c,btof,mtd,mtdCalib,pp2pp,fmsDat,fmsPoint,fpsDat,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFPPVnoCTB,-VFPPV,-beamline,-hitfilt");

  // Run9 pp500 P16ib chain
  TString prodP16ibpp500("DbV20110311,OGGVoltErr,pp2009c,ITTF,BEmcChkStat,btof,Corr4,OSpaceZ2,OGridLeak3D,DbV20151021_TOF_Calibrations,DbV20161021_tpc_Calibrations,VFMCE,TpxClu,-hitfilt");

  // Run14 AuAu200 st_WB chain (P16ig)
  TString prodP16igAuAu200("DbV20160406,P2014a,StiCA,btof,mtd,mtdCalib,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt");

  // Run16 AuAu200 st_physics & st_mtd chain (P16ij)
  TString prodP16ijAuAu200("DbV20161018,P2016a,StiCA,mtd,mtdCalib,btof,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt");

  // Run14 AuAu200 min. bias (P17id)
  TString prodP17idAuAu200("DbV20150316 DbV20170712_trg_Calibrations,FiltTrg_JetCorrTrgs,P2014a,btof,mtd,mtdCalib,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt");

  // Run16 dAu200 chain (P17id)
  TString prodP17iddAu200("DbV20161216,P2016a,StiCA,mtd,mtdCalib,btof,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt");
  
  // Run16 dAu39 chain (P17id)
  TString prodP17iddAu39("DbV20170426,P2016a,StiCA,mtd,mtdCalib,btof,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt");
  
  // Run16 dAu20 chain (P17id)
  TString prodP17iddAu20("DbV20170426,P2016a,StiCA,mtd,mtdCalib,btof,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,VFMCE,TpxClu,-VFMinuit,-hitfilt");

  TString geomP08ic("ry2008e");
  TString geomP10ic("ry2009d");
  TString geomP10ih("ry2010c");
  TString geomP10ik(geomP10ih); // Same chain as P10ih
  TString geomP11id("ry2011");
  TString geomP12id("ry2012a");
  TString geomP14ig("ry2013_1c");
  TString geomP14ii("ry2014a");
  TString geomP15ic("ry2014a");
  TString geomP15ie("ry2014a");
  TString geomP16id("ry2014a");
  TString geomP16ia("ry2015b");
  TString geomP16id_run15("ry2015c");
  TString geomP16ig("ry2014a");
  TString geomP16ij("ry2016a");
  TString geomP17id("ry2014a");
  TString geomP17id_run16("ry2016a");

  TString xgeom(" useXgeom");
  TString chain1Opt("in,magF,tpcDb,NoDefault,TpxRaw,-ittf,NoOutput");
  TString chain2Opt("gen_T,geomT,sim_T,TpcRS,-ittf,-tpc_daq,nodefault");
	if(bPythia){
		chain2Opt += ",fzin";
	}
	else {
		chain2Opt += ",NoInput,PrepEmbed";
	}
  chain2Opt += " ";

  TString chain3Opt("");
  if (prodName == "P08icpp")           { chain3Opt = prodP08icpp;       chain2Opt += geomP08ic;}
  else if (prodName == "P08iepp")      { chain3Opt = prodP08iepp;       chain2Opt += geomP08ic;}
  else if (prodName == "P08icAuAu9")   { chain3Opt = prodP08icAuAu9;    chain2Opt += geomP08ic;}
  else if (prodName == "P08icdAu")     { chain3Opt = prodP08icdAu;      chain2Opt += geomP08ic;}
  else if (prodName == "P08iedAu")     { chain3Opt = prodP08iedAu;      chain2Opt += geomP08ic;}
  else if (prodName == "P08icAuAu200") { chain3Opt = prodP08icAuAu200;  chain2Opt += geomP08ic;}
  else if (prodName == "P09igpp500")   { chain3Opt = prodP09igpp500;    chain2Opt += geomP10ic;}
  else if (prodName == "P11ibpp500")   { chain3Opt = prodP11ibpp500;    chain2Opt += geomP10ic;}
  else if (prodName == "P10iapp")      { chain3Opt = prodP10iapp;       chain2Opt += geomP10ih;}
  else if (prodName == "P10icpp200")   { chain3Opt = prodP10icpp200;    chain2Opt += geomP10ic;}
  else if (prodName == "P11idpp200")   { chain3Opt = prodP11idpp200;    chain2Opt += geomP10ic;}
  else if (prodName == "P10ikAuAu62")  { chain3Opt = prodP10ikAuAu62;   chain2Opt += geomP10ik;}
  else if (prodName == "P10ihAuAu39")  { chain3Opt = prodP10ihAuAu39;   chain2Opt += geomP10ih;}
  else if (prodName == "P10ikAuAu39")  { chain3Opt = prodP10ikAuAu39;   chain2Opt += geomP10ik;}
  else if (prodName == "P10ihAuAu11")  { chain3Opt = prodP10ihAuAu11;   chain2Opt += geomP10ih;}
  else if (prodName == "P10ihAuAu7")   { chain3Opt = prodP10ihAuAu7;    chain2Opt += geomP10ih;}
  else if (prodName == "P10ikAuAu200") { chain3Opt = prodP10ikAuAu200;  chain2Opt += geomP10ik;}
  else if (prodName == "P11idAuAu200") { chain3Opt = prodP11idAuAu200;  chain2Opt += geomP11id;}
  else if (prodName == "P11idAuAu27")  { chain3Opt = prodP11idAuAu27;   chain2Opt += geomP11id;}
  else if (prodName == "P11idAuAu19")  { chain3Opt = prodP11idAuAu19;   chain2Opt += geomP11id;}
  else if (prodName == "P11idpp500")   { chain3Opt = prodP11idpp500;    chain2Opt += geomP11id;}
  else if (prodName == "P12idUU193")   { chain3Opt = prodP12idUU193;    chain2Opt += geomP12id;}
  else if (prodName == "P12idpp200")   { chain3Opt = prodP12idpp200;    chain2Opt += geomP12id;}
  else if (prodName == "P14igpp500")   { chain1Opt += xgeom; chain3Opt = prodP14igpp500;    chain3Opt += ",mtdsim";  chain2Opt += geomP14ig;}
  else if (prodName == "P14iiAuAu15")  { chain1Opt += xgeom; chain3Opt = prodP14iiAuAu15;   chain3Opt += ",mtdsim";  chain2Opt += geomP14ii;}
  else if (prodName == "P15icAuAu200") { chain1Opt += xgeom; chain3Opt = prodP15icAuAu200;  chain3Opt += ",mtdsim";  chain2Opt += geomP15ic;}
  else if (prodName == "P15ieAuAu200") { chain1Opt += xgeom; chain3Opt = prodP15ieAuAu200;  chain3Opt += ",mtdsim";  chain2Opt += geomP15ie;}
  else if (prodName == "P15ieCuAu200") { chain3Opt = prodP15ieCuAu200;  chain2Opt += geomP12id;}
  else if (prodName == "P16idAuAu200") { chain1Opt += xgeom; chain3Opt = prodP16idAuAu200;  chain3Opt += ",mtdsim";  chain2Opt += geomP16id;}
  else if (prodName == "P16iaAuAu5")   { chain1Opt += xgeom; chain3Opt = prodP16iaAuAu5;    chain3Opt += ",mtdsim";  chain2Opt += geomP16ia;}
  else if (prodName == "P16idpp200")   { chain1Opt += xgeom; chain3Opt = prodP16idpp200;    chain3Opt += ",mtdsim";  chain2Opt += geomP16id_run15;}
  else if (prodName == "P16idpAu200")  { chain1Opt += xgeom; chain3Opt = prodP16idpAu200;   chain3Opt += ",mtdsim";  chain2Opt += geomP16id_run15;}
  else if (prodName == "P16ibpp500")   { chain3Opt = prodP16ibpp500;    chain2Opt += geomP10ic;}
  else if (prodName == "P16igAuAu200") { chain1Opt += xgeom; chain3Opt = prodP16igAuAu200;  chain3Opt += ",mtdsim";  chain2Opt += geomP16ig;}
  else if (prodName == "P16ijAuAu200") { chain1Opt += xgeom; chain3Opt = prodP16ijAuAu200;  chain3Opt += ",mtdsim";  chain2Opt += geomP16ij;}
  else if (prodName == "P17idAuAu200") { chain1Opt += xgeom; chain3Opt = prodP17idAuAu200;  chain3Opt += ",mtdsim";  chain2Opt += geomP17id;}
  else if (prodName == "P17iddAu200")  { chain1Opt += xgeom; chain3Opt = prodP17iddAu200;   chain3Opt += ",mtdsim";  chain2Opt += geomP17id_run16;}
  else if (prodName == "P17iddAu39")   { chain1Opt += xgeom; chain3Opt = prodP17iddAu39;    chain3Opt += ",mtdsim";  chain2Opt += geomP17id_run16;}
  else if (prodName == "P17iddAu20")   { chain1Opt += xgeom; chain3Opt = prodP17iddAu20;    chain3Opt += ",mtdsim";  chain2Opt += geomP17id_run16;}

  else {
    cout << "Choice prodName " << prodName << " does not correspond to known chain. Processing impossible. " << endl;
    return;
  }
  chain3Opt += ",TpcMixer,GeantOut,MiniMcMk,McAna,-in,NoInput,useInTracker,emcSim,BEmcMixer,EEfs,EEmcMixer"; 

  bool useEndcapSlowSim = true; // turn Endcap slow simu On/Off 
  if (useEndcapSlowSim) chain3Opt += ",EEss";


  // Dynamically link some shared libs
  gROOT->LoadMacro("bfc.C");
  if (gClassTable->GetID("StBFChain") < 0) Load();
  //______________Create the main chain object______________________________________
  Chain = new StChain("Embedding");
  //________________________________________________________________________________
  bfc(-1,chain1Opt,daqfile);
  chain1 = chain;
  chain1->SetName("One"); 
  Chain->cd();
  //________________________________________________________________________________  
  if(bPythia){
    bfc(-1,chain2Opt,fzdfile);
  	chain2 = chain;
  	chain2->SetName("Two"); 
  	Chain->cd();
  	if (chain2->GetOption("TRS")){
    	StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Trs");
    	if (! trsMk) {
      	cout << "Cannot find Trs in chain2" << endl;
      	return;
    	}
    	trsMk->setNormalFactor(1.32);
  	}
	}
	else {
		bfc(-1,chain2Opt);
  	chain2 = chain;
  	chain2->SetName("Two"); 
  	Chain->cd();
	}
#if 0
  if (chain2->GetOption("TRS")){
    StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Trs");
    if (! trsMk) {
      cout << "Cannot find Trs in chain2" << endl;
      return;
    }
    trsMk->setNormalFactor(1.32);
    trsMk->SetMode(0);
  }
#endif
  //________________________________________________________________________________
  //  gSystem->Load("StFtpcMixerMaker");
  //  StFtpcMixerMaker  *ftpcmixer = new StFtpcMixerMaker("FtpcMixer","daq","trs");
  //________________________________________________________________________________
  TString OutputFileName(gSystem->BaseName(daqfile));
  OutputFileName.ReplaceAll("*","");
  OutputFileName.ReplaceAll(".daq","");
  //  OutputFileName.Append("_emb.root");
  OutputFileName.Append(".root");
  bfc(-1,chain3Opt,0,OutputFileName);
  chain3 = chain;
  chain3->SetName("Three"); 
  Chain->cd();
  //________________________________________________________________________________
  StTpcMixerMaker  *mixer = (StTpcMixerMaker *) chain3->Maker("TpcMixer");
  if( prodName == "P08icAuAu200")
	{
	  mixer->SetInput("Input1","MixerEvent");
	}
  else
	{
	  mixer->SetInput("Input1","TpxRaw/.data/Event");
        }

  if (chain2Opt.Contains("TpcRS",TString::kIgnoreCase)) {
    mixer->SetInput("Input2","TpcRS/Event");
  } else {
    mixer->SetInput("Input2","Trs/.const/Event");
  }
  Chain->cd();

  //.............. Ensure EEmc maker order here ....................

  // EEMC fast simulator should be run, even if slow simulator is run (Ting Lin)
  StEEmcFastMaker  *eemcFastSim = (StEEmcFastMaker*) chain3->Maker("eefs");
  if (!eemcFastSim) eemcFastSim = (StEEmcFastMaker*) chain3->Maker("EEmcFastSim");
  if (!eemcFastSim) eemcFastSim = new StEEmcFastMaker();
  eemcFastSim->SetEmbeddingMode();
  //  eemcFastSim->SetDebug();
  
  StEEmcMixerMaker* eemcMixer = (StEEmcMixerMaker *) chain3->Maker("EEmcMixer");
  if (!eemcMixer) cout << "ERROR in finding EEMC Mixer!" << endl;
  // eemcMixer->SetDebug();
  chain3->AddBefore(eemcMixer->GetName(),eemcFastSim);

  if (useEndcapSlowSim) {
    StEEmcSlowMaker  *eemcSlowSim = (StEEmcSlowMaker*) chain3->Maker("eess");
    if (!eemcSlowSim) cout << "ERROR in finding EEMC Slow Simulator!" << endl;
    eemcSlowSim->setEmbeddingMode();
    chain3->AddBefore(eemcMixer->GetName(),eemcSlowSim);
  }

  StMcEventMaker* mcEventMaker = (StMcEventMaker *) chain3->Maker("StMcEventMaker");
  if (!mcEventMaker)  mcEventMaker = new StMcEventMaker();
  chain3->AddAfter(eemcMixer->GetName(),mcEventMaker);


  
  //________________________________________________________________________________
  {
    TDatime t;
    gMessMgr->QAInfo() << Form("Run is started at Date/Time %i/%i",t.GetDate(),t.GetTime()) << endm;
  }
  gMessMgr->QAInfo() << Form("Run on %s in %s",gSystem->HostName(),gSystem->WorkingDirectory()) << endm;
  gMessMgr->QAInfo() << Form("with %s", Chain->GetCVS()) << endm;
  // embedded particle set
	if(!bPythia){
	  StPrepEmbedMaker *embMk = (StPrepEmbedMaker *) Chain->Maker("PrepEmbed");
  	if (! embMk) return;
 		cout << "bfcMixer: Setting PID: "<<pid<<endl;
  	embMk->SetTagFile(tagfile);
  	//            pTlow,ptHigh,etaLow,etaHigh,phiLow,phiHigh
  	embMk->SetOpt(  pt_low,    pt_high,  eta_low,    eta_high,    0.,   6.283185, type); 
  	//                pid, mult
  	embMk->SetPartOpt(  pid,mult);

  	// Default is no event selections
  	embMk->SetSkipMode(kTRUE);

  	embMk->SetTemp(0.35);

	//embMk->SetRapidityMode(kFALSE);  //default is 'kTRUE'
	
	//Switch to prime mode for nucleus (with geantID > 10000) embedding, default is 'kFALSE'
	//embMk->SetPrimeMode(kTRUE);

  	// Make trigger and z-vertex cuts (only if SkipMode is true)
  	// Trigger cut
  	//   Can put multiple trigger id's 
  	if ( !triggers.empty() ){
  	  for(std::vector<Int_t>::iterator iter = triggers.begin(); iter != triggers.end(); iter++){
    	  embMk->SetTrgOpt((*iter)) ;
    	}
  	}
  	// z-vertex cuts
  	embMk->SetZVertexCut(vzlow, vzhigh) ;
  	// vr = sqrt{vx^2 + vy^2} cut
  	embMk->SetVrCut(vr);

	//cut on VpdVz, need moretags.root
	//embMk->SetVpdVzCutMode(kTRUE);
	//embMk->SetVpdVzCut(3);
	
	//cut on PVranking, need moretags.root
	//embMk->SetPVRankCutMode(kTRUE);
	//embMk->SetPVRankCut(0);  // pvrank > 0

	}

  TAttr::SetDebug(0);
  Chain->SetAttr(".Privilege",0,"*"                ); 	//All  makers are NOT priviliged
  Chain->SetAttr(".Privilege",1,"StBFChain::*" ); 	//StBFChain is priviliged
  Chain->SetAttr(".Privilege",1,"StIOInterFace::*" ); 	//All IO makers are priviliged
  Chain->SetAttr(".Privilege",1,"St_geant_Maker::*"); 	//It is also IO maker
  if(!bPythia)Chain->SetAttr(".Privilege",1,"StPrepEmbedMaker::*"); //It is also IO maker
  //  Chain->SetDEBUG(0);
  if (Nevents < 0) return;
  Int_t iInit = Chain->Init();
  if (iInit >=  kStEOF) {Chain->FatalErr(iInit,"on init"); return;}
  StMaker *treeMk = Chain->GetMaker("outputStream");
  Chain->EventLoop(Nevents,treeMk);
  gMessMgr->QAInfo() << "Run completed " << endm;
  gSystem->Exec("date");
}
//________________________________________________________________________________
void bfcMixer_Tpx(Int_t Nevents, const Char_t *daqfile, Char_t *tagfile,
		  Double_t pt_low, Double_t pt_high, Double_t eta_low, Double_t eta_high, 
		  Double_t vzlow, Double_t vzhigh, Double_t vr, Int_t pid, Double_t mult,
		  const Char_t *triggersC, const Char_t *prodName, const Char_t* type) {
  std::vector<Int_t> triggers;
  if (triggersC) {
    TPMERegexp pm(":");
    Int_t N = pm.Split(triggersC);
    for (Int_t i = 0; i < N; i++) {
      TString num(pm[i]);
      triggers.push_back(num.Atoi());
    }
  }
  bfcMixer_Tpx(Nevents, daqfile, tagfile, pt_low, pt_high, eta_low, eta_high, vzlow, vzhigh, vr, pid, mult, triggers, prodName, type);
}
