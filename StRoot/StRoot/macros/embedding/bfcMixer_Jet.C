 //////////////////////////////////////////////////////////////////////////
//
// $Id: bfcMixer_Jet.C,v 1.1 2021/02/18 02:48:31 starembd Exp $
//
// $Log: bfcMixer_Jet.C,v $
// Revision 1.1  2021/02/18 02:48:31  starembd
// bfcMixer for Jet embedding
//
//
// JET EMBEDDING MACRO
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 27 July 2011
//
//////////////////////////////////////////////////////////////////////////

class StChain;
StChain* Chain = 0;

class StBFChain;
StBFChain* chain1 = 0;
StBFChain* chain2 = 0;
StBFChain* chain3 = 0;

//_____________________________________________________________________
void bfcMixer_Jet(const Int_t Nevents = 10,
                  const Char_t* daqfile = "@run16078031.list",
                  const Char_t* fzdfile = "pt15_20_16078031_1.fzd",
                  const Char_t* prodName = "P16idpp200RFF",
                  const Char_t* DbVoption = "DbV20160418"){
  
  // Production chains for P08ic - p+p, Au+Au 9 GeV and d+Au
	TString prodP08iepp("DbV20081117 B2008a ITTF IAna ppOpt l3onl emcDY2 fpd ftpc trgd ZDCvtx NosvtIT NossdIT Corr4 OSpaceZ2 OGridLeak3D VFMCE -hitfilt");
	TString prodP08iedAu("DbV20090213 P2008 ITTF OSpaceZ2 OGridLeak3D beamLine VFMCE TpxClu -VFMinuit -hitfilt");
	TString prodP10iapp("DbV20091001 pp2009c TpcRS ITTF OSpaceZ2 OGridLeak3D beamLine, VFMCE TpcRS -VFMinuit -hitfilt");

	// BES Run10 chains
	TString prodP10ihAuAu39("DbV20100909 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");
	TString prodP10ihAuAu11("DbV20100821 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");
	TString prodP10ihAuAu7("DbV20100821 P2010a,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D,VFMCE TpxClu -VFMinuit -hitfilt");

	// Run 9 500 GeV chain for W from Jan Balewski
	TString prodP09igpp500("DbV20101215 OGGVoltErr pp2009c ITTF VFPPVnoCTB BEmcChkStat beamLine Corr4 OSpaceZ2 OGridLeak3D -dstout -evout");

	// Run 9 200 GeV chain for jet embedding
	TString prodP10icpp200("pp2009c ITTF VFPPVnoCTB beamLine BEmcChkStat btof fmsdat Corr4 OSpaceZ2 OGridLeak3D");

	// Run 9 200 GeV FF chain
	// NOTE: "Sti" needs to be added for this chain to work !!!
	TString prodP11idpp200FF("DbV20120622 pp2009d ITTF Sti VFPPVnoCTB beamLine BEmcChkStat btof fmsdat Corr4 OSpaceZ2 OGridLeak3D -hitfilt");

	// Run 9 pp200 RFF chain
	// Added "Sti" just in case
	TString prodP11idpp200RFF("DbV20120908 pp2009d ITTF Sti VFPPVnoCTB beamLine BEmcChkStat btof fmsdat Corr4 OSpaceZ2 OGridLeak3D -hitfilt");

	//Run12 pp500 RFF chain
	// Will not run without Sti !!!
	TString prodP12iapp500RFF("DbV20121219 pp2012b Sti AgML mtdDat btof fmsDat VFPPVnoCTB beamline BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D -hitfilt");
	// Run12 pp500 P13ib production
	TString prodP13ibpp500RFF("DbV20130212 pp2012b AgML mtdDat btof fmsDat VFPPVnoCTB useBTOF4Vtx beamline BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D -evout -hitfilt");

	// Run13 pp500 P14ia production
	TString prodP14iapp500RFF("DbV20140222 DbV20160325_EMC_Calibrations DbV20170831_TRG_Calibrations pp2013a mtd btof Sti fmsDat fgt fgtPoint VFPPVnoCTB beamline BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D -hitfilt"); //Amilkar: Same as official production but include "Sti" to activate Sti tool kit (if not there is an error check StGenericVertexMaker/StPPVertexFinder.cxx line 124). Also add the PPV boost http://www.star.bnl.gov/HyperNews-star/protected/get/starpwgc/4154.html. 13 Sep 2018 remove DbV20160325_tracker_Calibrations
	//Zilong has in the run 12 request "DbV20160325_EMC_Calibrations DbV20160325_TRG_Calibrations", https://drupal.star.bnl.gov/STAR/system/files/Run12pp500EmbeddingRequest.pdf

	// Run13 pp500 P14ig production   TEST 30 Sep 2018
	TString prodP14igpp500RFF("DbV20140905 DbV20160325_EMC_Calibrations DbV20170831_TRG_Calibrations pp2013b mtd btof Sti fmsDat fgt fgtPoint VFPPVnoCTB beamline StiHftP BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D -hitfilt -evout"); // //Amilkar:Same as official production but include "Sti" to activate Sti tool kit (if not there is an error check StGenericVertexMaker/StPPVertexFinder.cxx line 124). Also add the PPV boost http://www.star.bnl.gov/HyperNews-star/protected/get/starpwgc/4154.html. The production was not doen with PPV boost so the embedding should not also. Contacted Jinlong for the exact date of the run 13 gain calibration was uploaded 20160930, (update 4 Oct 2018) but using DbV20161001_EMC_Calibrations does not work.

	// Run15 pp200 P16id production
	//TString prodP16idpp200RFF("DbV20160418 DbV20181115_EMC_Calibrations DbV20170831_TRG_Calibrations pp2015c btof mtd mtdCalib pp2pp fmsDat fmsPoint fpsDat BEmcChkStat -evout CorrX OSpaceZ2 OGridLeak3D -hitfilt");

	// Run15 pp200 P16id production
//	TString prodP16idpp200RFF("DbV20160418 DbV20190702_EMC_Calibrations DbV20170831_TRG_Calibrations pp2015c btof mtd mtdCalib pp2pp fmsDat fmsPoint fpsDat BEmcChkStat -evout CorrX OSpaceZ2 OGridLeak3D -hitfilt");
//        TString prodP16idpp200RFF("DbV20160418 DbV20190702_EMC_Calibrations DbV20190702_EEMC_Calibrations DbV20190702_TRG_Calibrations pp2015c Sti mtd mtdCalib pp2pp BEmcChkStat -evout CorrX OSpaceZ2 OGridLeak3D -hitfilt");

//        TString prodP16idpp200RFF("DbV20160418 DbV20190702_EMC_Calibrations DbV20190702_EEMC_Calibrations DbV20190702_TRG_Calibrations pp2015c btof mtd mtdCalib pp2pp fmsDat fmsPoint fpsDat BEmcChkStat -evout CorrX OSpaceZ2 OGridLeak3D -hitfilt"); // use in 20190928
        TString prodP16idpp200RFF("DbV20160418 DbV20190702_EMC_Calibrations DbV20190702_EEMC_Calibrations DbV20190702_TRG_Calibrations pp2015c btof Sti mtd mtdCalib pp2pp fmsDat fmsPoint fpsDat BEmcChkStat -evout CorrX OSpaceZ2 OGridLeak3D -hitfilt"); // use in 20191205 add Sti

	//TString prodP16idpp200RFF("DbV20160310 DbV20181115_EMC_Calibrations DbV20170831_TRG_Calibrations pp2015c btof mtd mtdCalib pp2pp fmsDat fmsPoint fpsDat BEmcChkStat -evout CorrX OSpaceZ2 OGridLeak3D -hitfilt");

	// Run15 pAu200 P16id production
	TString prodP16idpAu200RFF("DbV20160710 pp2015c btof mtd mtdCalib pp2pp fmsDat fmsPoint fpsDat BEmcChkStat -evout CorrX OSpaceZ2 OGridLeak3D -hitfilt");
	

	// Additional tags needed for embedding
	prodP09igpp500 += " TpxClu -VFMinuit VFPPVnoCTB beamLine -hitfilt";
	prodP10icpp200 += " TpxClu -VFMinuit VFPPVnoCTB beamLine -hitfilt";
	prodP11idpp200FF  += " TpxClu -VFMinuit VFPPVnoCTB beamLine -hitfilt";
	prodP11idpp200RFF += " TpxClu -VFMinuit VFPPVnoCTB beamLine -hitfilt";
	prodP12iapp500RFF += " TpxClu -VFMinuit VFPPVnoCTB beamLine -hitfilt";
	prodP13ibpp500RFF += " TpxClu -VFMinuit VFPPVnoCTB beamLine -hitfilt";
	prodP14iapp500RFF += " TpxClu ";
	prodP14igpp500RFF += " TpxClu ";
	prodP16idpp200RFF += " TpxClu -VFMinuit VFPPVnoCTB beamLine";
	prodP16idpAu200RFF+= " TpxClu ";
	
	TString geomP08ic("ry2008");
	TString geomP10ih("ry2010");
	TString geomP09ig("ry2009a");
	TString geomP10ic("ry2009a");
	TString geomP11id("ry2009d");  
	TString geomP12ia("ry2012a");
	TString geomP13ib("ry2012a");
	TString geomP14ia("ry2013_2c");
	TString geomP14ig("ry2013_1c");
	TString geomP16id("ry2015c");

	TString chain1Opt("in,magF,tpcDb,NoDefault,TpxRaw,-ittf,NoOutput");
	TString chain2Opt("fzin,gen_T,geomT,sim_T,TpcRS,-ittf,-tpc_daq,nodefault");
	chain2Opt += " ";

	TString chain3Opt;
	if (prodName == "P08icpp")           { chain3Opt = prodP08icpp;       chain2Opt += geomP08ic; }
	else if (prodName == "P08iepp")      { chain3Opt = prodP08iepp;       chain2Opt += geomP08ic; }
	else if (prodName == "P08icAuAu9")   { chain3Opt = prodP08icAuAu9;    chain2Opt += geomP08ic; }
	else if (prodName == "P08icdAu")     { chain3Opt = prodP08icdAu;      chain2Opt += geomP08ic; }
	else if (prodName == "P08iedAu")     { chain3Opt = prodP08iedAu;      chain2Opt += geomP08ic; }
	else if (prodName == "P08icAuAu200") { chain3Opt = prodP08icAuAu200;  chain2Opt += geomP08ic; }
	else if (prodName == "P10iapp")      { chain3Opt = prodP10iapp;       chain2Opt += geomP10ih; }
	else if (prodName == "P10ihAuAu39")  { chain3Opt = prodP10ihAuAu39;   chain2Opt += geomP10ih; }
	else if (prodName == "P10ihAuAu11")  { chain3Opt = prodP10ihAuAu11;   chain2Opt += geomP10ih; }
	else if (prodName == "P10ihAuAu7")   { chain3Opt = prodP10ihAuAu7;    chain2Opt += geomP10ih; }
	else if (prodName == "P09igpp500")   { chain3Opt = prodP09igpp500;    chain2Opt += geomP09ig; }
	else if (prodName == "P10icpp200")   { chain3Opt = prodP10icpp200;    chain2Opt += geomP10ic; }
	else if (prodName == "P11idpp200FF") { chain3Opt = prodP11idpp200FF;  chain2Opt += geomP11id; }
	else if (prodName == "P11idpp200RFF"){ chain3Opt = prodP11idpp200RFF; chain2Opt += geomP11id; }
	else if (prodName == "P12iapp500RFF") { chain3Opt = prodP12iapp500RFF; chain2Opt += geomP12ia; }
	else if (prodName == "P13ibpp500RFF") { chain3Opt = prodP13ibpp500RFF; chain2Opt += geomP13ib; }
	else if (prodName == "P14iapp500RFF") { chain1Opt += " useXgeom"; chain3Opt = prodP14iapp500RFF; chain2Opt += geomP14ia; }
	else if (prodName == "P14igpp500RFF") { chain3Opt = prodP14igpp500RFF; chain2Opt += geomP14ig; }
	else if (prodName == "P16idpp200RFF") { chain1Opt += " useXgeom"; chain3Opt = prodP16idpp200RFF; chain2Opt += geomP16id; }
	else if (prodName == "P16idpAu200RFF"){ chain1Opt += " useXgeom"; chain3Opt = prodP16idpAu200RFF; chain2Opt += geomP16id; }
	else {
		cout << "Choice prodName " << prodName << " does not correspond to known chain. Processing impossible." << endl;
		return;
	}

	//chain3Opt.Prepend(' ');
	//chain3Opt.Prepend(DbVoption);
	chain3Opt += ",TpcMixer,GeantOut,MiniMcMk,McAna,-in,NoInput,useInTracker"; 
	chain3Opt += ",";

	if (prodName == "P08icpp")           { chain3Opt += geomP08ic; }
	else if (prodName == "P08iepp")      { chain3Opt += geomP08ic; }
	else if (prodName == "P08icAuAu9")   { chain3Opt += geomP08ic; }
	else if (prodName == "P08icdAu")     { chain3Opt += geomP08ic; }
	else if (prodName == "P08iedAu")     { chain3Opt += geomP08ic; }
	else if (prodName == "P08icAuAu200") { chain3Opt += geomP08ic; }
	else if (prodName == "P10iapp")      { chain3Opt += geomP10ih; }
	else if (prodName == "P10ihAuAu39")  { chain3Opt += geomP10ih; }
	else if (prodName == "P10ihAuAu11")  { chain3Opt += geomP10ih; }
	else if (prodName == "P10ihAuAu7")   { chain3Opt += geomP10ih; }
	else if (prodName == "P09igpp500")   { chain3Opt += geomP09ig; }
	else if (prodName == "P10icpp200")   { chain3Opt += geomP10ic; }
	else if (prodName == "P11idpp200FF") { chain3Opt += geomP11id; }
	else if (prodName == "P11idpp200RFF"){ chain3Opt += geomP11id; }
	else if (prodName == "P12iapp500RFF"){ chain3Opt += geomP12ia; }
	else if (prodName == "P14iapp500RFF"){ chain3Opt += geomP14ia; }
	else if (prodName == "P14igpp500RFF"){ chain3Opt += geomP14ig; }
	else if (prodName == "P16idpp200RFF"){ chain3Opt += geomP16id; }
	else if (prodName == "P16idpAu200RFF"){ chain3Opt += geomP16id; }
	else {
		cout << "Choice prodName " << prodName << " does not correspond to known chain. Processing impossible. " << endl;
		return;
	}

	// Test BTof Amilkar: from Ting
	//chain3Opt += ",vpdsim,btofSim,btofMixer,btofMatch,vpdCalib,btofCalib";

	// Add BEMC simulators to chain
	chain3Opt += ",emcSim";

	// Add EEMC fast simulator to chain
	chain3Opt += ",EEfs";

	// Dynamically link some shared libs
	gROOT->LoadMacro("bfc.C");
	if (gClassTable->GetID("StBFChain") < 0) Load();
	//______________Create the main chain object______________________________________
	Chain = new StChain("Embedding");
	//________________________________________________________________________________
	bfc(-1,chain1Opt,daqfile);
	chain1 = chain;
	chain1->SetName("One");
	chain1->SetAttr(".call","SetActive(0)","St_db_Maker::"); // Use DB cache to reduce overhead
	Chain->cd();
	//________________________________________________________________________________  
	bfc(-1,chain2Opt,fzdfile);
	chain2 = chain;
	chain2->SetName("Two");
	Chain->cd();
	if (chain2->GetOption("TRS")){
		StTrsMaker *trsMk = (StTrsMaker *) chain2->GetMaker("Trs");
		if (!trsMk) {
			cout << "Cannot find Trs in chain2" << endl;
			return;
		}
		trsMk->setNormalFactor(1.32);
		trsMk->SetMode(0);
	}
	//________________________________________________________________________________
	//  gSystem->Load("StFtpcMixerMaker");
	//  StFtpcMixerMaker  *ftpcmixer = new StFtpcMixerMaker("FtpcMixer","daq","trs");
	//________________________________________________________________________________
	TString OutputFileName(gSystem->BaseName(fzdfile));
	OutputFileName.ReplaceAll("*","");
	OutputFileName.ReplaceAll(".fzd","");
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

        // Set BTof to embedding mode:
        //StBTofSimMaker *btofSim = (StBTofSimMaker*)chain3->GetMaker("TofSim");
        //btofSim->setEmbeddingMode(true);

        //StVpdCalibMaker *vpdCalib = (StVpdCalibMaker*)chain3->GetMaker("vpdCalib");
        //vpdCalib->setUseVpdStart(kFALSE);
	
	//------------------------------------ EMC MIXERS ------------------------------------
	// Add BEMC mixer to chain3
	StEmcRawMaker* emcRaw = (StEmcRawMaker*)chain3->GetMaker("emcRaw");
	emcRaw->getBemcRaw()->saveAllStEvent(true); // use all 4800 BEMC towers
	gSystem->Load("StEmcMixerMaker");
	StEmcMixerMaker* bemcMixer = new StEmcMixerMaker;
	chain3->AddAfter("EmcSimulator",bemcMixer);
	// Set EEMC fast and slow simulator in embedding mode
	StEEmcFastMaker* eefs = (StEEmcFastMaker*)chain3->GetMaker("eefs");
	eefs->SetEmbeddingMode(); // Use local StEmcCollection
	eefs->UseFullTower(true); // Use full ETOW detector
	StEEmcSlowMaker* eess = new StEEmcSlowMaker;
	eess->setEmbeddingMode(true);
	// Add EEMC mixer to chain3
	StEEmcMixerMaker* eemcMixer = new StEEmcMixerMaker;
	//------------------------------------------------------------------------------------

	//----------------------------- TRIGGER FILTER -----------------------------
	// We want to achieve the following ordering for makers:
	// 1. BBC simulator
	// 2. BEMC simulator
	// 3. BEMC mixer
	// 4. EEMC fast simulator
	// 5. EEMC slow simulator
	// 6. EEMC mixer
	// 7. Pythia event maker
	// 8. Trigger simulator
	// 9. Trigger filter
	// 10. TPC maker

	// Place TPC chain after EMC makers
	chain3->AddAfter("eefs",chain3->GetMaker("tpcChain"));
	chain3->AddAfter("eefs",eemcMixer);
	chain3->AddAfter("eefs",eess);

	// Place Pythia maker after GEANT maker
	// and trigger filter after EMC makers
	gSystem->Load("StMCAsymMaker");
	gSystem->Load("StJetSkimEvent");
	gSystem->Load("StBfcTriggerFilterMaker");

	StPythiaEventMaker* pythia = new StPythiaEventMaker;
	TString pyfile = gSystem->BaseName(fzdfile);
	pyfile.ReplaceAll(".fzd",".pythia.root");
	pythia->SetPythiaFile(pyfile);
	chain3->AddAfter("geant",pythia);

	// Place trigger simulator after EMC makers
	gSystem->Load("StTriggerUtilities");
	StTriggerSimuMaker* trgsim = new StTriggerSimuMaker;
	//trgsim->useOnlineDB();   
	trgsim->useOfflineDB();
	trgsim->setMC(1);
	//trgsim->setMC(2); //Amilkar Do not use
	trgsim->useBemc();
	//trgsim->bemc->setConfig(StBemcTriggerSimu::kOffline);
	trgsim->bemc->setConfig(StBemcTriggerSimu::kOnline);  //Amilkar 
	trgsim->useEemc();
	trgsim->eemc->setSource("StEvent");

	/*	trgsim->setOverlapJetPatchTh(0,34);
	trgsim->setOverlapJetPatchTh(1,43);
	trgsim->setOverlapJetPatchTh(2,66);

	trgsim->setBarrelJetPatchTh(0,34);
	trgsim->setBarrelJetPatchTh(1,43);
	trgsim->setBarrelJetPatchTh(2,66);
	trgsim->setBarrelJetPatchTh(3,17);   //Amilkar

	trgsim->setEndcapJetPatchTh(0,34);
	trgsim->setEndcapJetPatchTh(1,43);
	trgsim->setEndcapJetPatchTh(2,66);
	trgsim->setEndcapJetPatchTh(3,17);*/  //Amilkar

	StBfcTriggerFilterMaker* trgfilt = new StBfcTriggerFilterMaker; //needed for trig filt
	trgfilt->SetJP0();
	//trgfilt->SetJP1();    //Amilkar: Use the lowest threshold triggers
	//trgfilt->SetJP2(); //needed for trig filt for JP2
	//if(Chain->GetEventNumber() >= 14082037) trgfilt->SetJP0dijet();
	//if(Chain->GetEventNumber() >= 14082035) trgfilt->SetJP1dijet();
	//trgfilt->SetBAJP();
	trgfilt->SetBHT2();
	trgfilt->SetEHT0();

	chain3->AddBefore("tpcChain",trgsim);  
	chain3->AddBefore("tpcChain",trgfilt); 

	// Move these makers after trigger decision
	// *** VERY IMPORTANT ***
	// The order of TpxRaw and TpcRS *must* be preserved
	// or the embedding will *not* work. [RT# 2299]
	// http://www.star.bnl.gov/rt2/Ticket/Display.html?id=2299
	StTpcRSMaker* TpcRS = (StTpcRSMaker*)chain2->GetMaker("TpcRS");
	StTpcHitMaker* TpxRaw = (StTpcHitMaker*)chain1->GetMaker("TpxRaw");
	chain3->AddBefore("TpcMixer",TpxRaw);
	chain3->AddBefore("TpcMixer",TpcRS);

	//--------------------------------------------------------------------------
	TString trgfile = gSystem->BaseName(fzdfile);
	trgfile.ReplaceAll(".fzd",".trig.root");
	TFile* ofile = TFile::Open(trgfile,"recreate");
	assert(ofile);
	TH2F* hBarrelHighTowerSimu = new TH2F("hBarrelHighTowerSimu","BEMC high tower simu;trigger patch;high tower",300,0,300,64,0,64);
	TH2F* hBarrelPatchSumSimu = new TH2F("hBarrelPatchSumSimu","BEMC patch sum simu;trigger patch;patch sum",300,0,300,64,0,64);
	TH2F* hEndcapHighTowerSimu = new TH2F("hEndcapHighTowerSimu","EEMC high tower simu;trigger patch;high tower",90,0,90,64,0,64);
	TH2F* hEndcapPatchSumSimu = new TH2F("hEndcapPatchSumSimu","EEMC patch sum simu;trigger patch;patch sum",90,0,90,64,0,64);
	TH2F* hBarrelJetPatchSimu = new TH2F("hBarrelJetPatchSimu","BEMC jet patch;jet patch;adc",18,0,18,160,0,160);
	TH2F* hEndcapJetPatchSimu = new TH2F("hEndcapJetPatchSimu","EEMC jet patch;jet patch;adc",6,0,6,160,0,160);
	TH2F* hOverlapJetPatchSimu = new TH2F("hOverlapJetPatchSimu","BEMC-EEMC-overlap;jet patch;adc",6,0,6,160,0,160);
	//--------------------------------------------------------------------------
	// Initialize chain
	Chain->Init();
	PrintTimer(Chain);
	puts("Order of makers in BFCMIXER:");
	StMaker::lsMakers(Chain);
	// Event loop
	int mNTotal = 0;
	int mNFailed = 0;
	TBenchmark evnt;
	StIOMaker* inputStream = (StIOMaker*)chain1->GetMaker("inputStream");
	for (int iEvent = 1; iEvent <= Nevents; ++iEvent) {
		evnt.Reset();
		evnt.Start("QAInfo:");
		Chain->Clear();
		int iMake = Chain->Make(iEvent);
		if (iMake == kStErr) ++mNFailed;
		if (inputStream->GetMakeReturn() % 10 == kStEOF) {
			inputStream->Rewind();
			--iEvent;
			continue;
		}
		if (iMake % 10 == kStEOF || iMake % 10 == kStFatal) break;
		++mNTotal;
		PrintTimer(Chain);
		//--------------------------------------------------------------------------
		// BEMC high towers and trigger patches
		for (int triggerpatch = 0; triggerpatch < 300; ++triggerpatch) {
			hBarrelHighTowerSimu->Fill(triggerpatch,trgsim->bemc->getBEMC_FEE_HT_ADC()[triggerpatch]);
			hBarrelPatchSumSimu->Fill(triggerpatch,trgsim->bemc->getBEMC_FEE_TP_ADC()[triggerpatch]);
		} // for triggerpatch
		// BEMC jet patches
		for (int jetpatch = 0; jetpatch < 18; ++jetpatch) {
			hBarrelJetPatchSimu->Fill(jetpatch,trgsim->bemc->barrelJetPatchAdc(jetpatch));
		} // for jetpatch
		// EEMC high towers and trigger patches
		for (int triggerpatch = 0; triggerpatch < 90; ++triggerpatch) {
			hEndcapHighTowerSimu->Fill(triggerpatch,trgsim->eemc->getOutHT(triggerpatch));
			hEndcapPatchSumSimu->Fill(triggerpatch,trgsim->eemc->getOutTPsum(triggerpatch));
		} // for triggerpatch
		// EEMC jet patches
		for (int jetpatch = 0; jetpatch < 6; ++jetpatch) {
			hEndcapJetPatchSimu->Fill(jetpatch,trgsim->eemc->endcapJetPatchAdc(jetpatch));
		} // for jetpatch
		// BEMC-EEMC-overlap jet patches
		for (int i = 0; i < 2; ++i) {
			int jetpatch, adc;
			trgsim->emc->getOverlapJetPatchAdc(i,jetpatch,adc);
			hOverlapJetPatchSimu->Fill(jetpatch,adc);
		} // for i
		//--------------------------------------------------------------------------
		evnt.Stop("QAInfo:");
		printf("QAInfo: Done with Event [no. %d/run %d/evt. %d/Date.Time %d.%d/sta %d] Real Time = %10.2f seconds Cpu Time = %10.2f seconds\n",iEvent,Chain->GetRunNumber(),Chain->GetEventNumber(),Chain->GetDate(),Chain->GetTime(),chain3->GetMakeReturn(),evnt.GetRealTime("QAInfo:"),evnt.GetCpuTime("QAInfo:"));
	} // End event loop
	printf("QAInfo:EventLoop completed code %d\n",iMake);
	gSystem->Exec("date");
	TDatime t;
	printf("QAInfo:Run is finished at Date/Time %i/%i; Total events processed: %i and not completed: %i\n",t.GetDate(),t.GetTime(),mNTotal,mNFailed);
	//--------------------------------------------------------------------------
	ofile->Write();
	ofile->Close();
	//--------------------------------------------------------------------------
}

// Print timers for all makers in chain
class StMaker;
void PrintTimer(StMaker* chain)
{
	TIter next(chain->GetMakeList());
	StMaker* maker;
	while (maker = (StMaker*)next()) {
		maker->PrintTimer();
		PrintTimer(maker);
		// Hack to reset timer
		maker->StartTimer(true);
		maker->StopTimer();
	}
}
