void runTpcHitMap(const Char_t *file = "/star/data03/daq/2021/072/22072052/st_physics_adc_22072052_raw_3000002.daq", int nEvent = 1){

    gROOT->LoadMacro("Load.C");
    Load();
    gSystem->Load("StTpcHitMap");
    //  bfc(1,"P2007b ittf OSpaceZ2 OGridLeak3D BEmcChkStat KeepTpcHit",file);
    //  bfc(nEvent,"pp2009a ittf OSpaceZ2 Corr3 OGridLeak3D BEmcChkStat",file);
    //  bfc(nEvent,"DbV20080418 B2007g ITTF IAna KeepSvtHit hitfilt VFMinuit3 l3onl emcDY2 fpd ftpc trgd ZDCvtx svtIT ssdIT Corr5 pmdReco -dstout",file);
    //  bfc(nEvent,"P2010a ITTF VFMinuit3 BEmcChkStat btofDat btofMatch Corr4 OSpaceZ2 OGridLeak3D KeepTpcHit", file);
    //  bfc(1, "P2010a ITTF VFMinuit3 BEmcChkStat btofDat btofMatch Corr4 OSpaceZ2 OGridLeak3D KeepTpcHit", file);
    //  bfc(1, "pp2012a mtdDat btof fmsDat VFPPVnoCTB beamline BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D -hitfilt KeepTpcHit", file);
    //  bfc(1, "P2012a mtdDat btof fmsDat VFPPVnoCTB beamline BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D -hitfilt KeepTpcHit", file);
    //  bfc(1, "P2012a AgML mtdDat btof fmsDat BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D -hitfilt KeepTpcHit", file);
    //  bfc(1, "P2014a mtd btof BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D -hitfilt KeepTpcHit", file);

    gROOT->LoadMacro("bfc.C");
    // Run12 pp 510
    // bfc(1, "DbV20130502 pp2012b AgML mtdDat btof fmsDat VFPPVnoCTB beamline BEmcChkStat Corr4 OSpaceZ2 OGridLeak3D -hitfilt KeepTpcHit", file);

    // Run17 pp500 Init
    // bfc(1, "pp2017,StiCA,btof,BEmcChkStat,CorrX,OSpaceZ2,OGridLeak3D,-hitfilt", file);

    // Run10 AuAu62GeV
    // bfc(1,"P2010a,pmdReco,btof,BEmcChkStat,Corr4,OSpaceZ2,OGridLeak3D",file);

    // Run17 AuAu54GeV
    // bfc(1, "P2017,btof,BEmcChkStat,SCScalerCal,CorrX,OSpaceZ2,OGridLeak3D,-hitfilt,KeepTpcHit", file);

    // Run18, iTPC
    // bfc(1, "B2018,BAna,in, tpcDB,TpxRaw,TpxClu,StiCA,KFVertex,CorrX,OGridLeak3D,KFVertex,TpcHitMover,analysis,NoHistos,NoRunco,-HitFilt,-evout,KeepTpcHit",file);
    ////    bfc(1, "P2018a,StiCA,btof,mtd,PicoVtxDefault,BEmcChkStat,QAalltrigs,OSpaceZ2,OGridLeak3D,-hitfilt,KeepTpcHit", file);
    // chain->Finish();

    // Run19, iTPC simulatioin
    // bfc(1, "rung.1 fzin tpcRS y2019 AgML usexgeom FieldOn MakeEvent StiCA NoSsdIt NoSvtIt TpcHitMover TpxClu Idst BAna l0 Tree logger genvtx tpcDB tags EEfs evout -geantout -dstout IdTruth bigbig MiniMcMk clearmem iTPCIT",
    //     "/gpfs01/star/pwg/iraklic/iTPC/AgML/JustAgML/FromJason/y2019/V02/AuAu200y2019_1_10evts.fzd");
    // Run19, AuAu 19.6GeV fast offline
    // bfc(1,"P2019a,StiCA,PicoVtxDefault,BEmcChkStat,OSpaceZ2,OGridLeakFull,-OPr13,OPr40,-hitfilt,QAalltrigs,-beamline3D,EvOut,KeepTpcHit", file);

    // Run20
    // bfc(1,"P2020a,StiCA,btof,mtd,BEmcChkStat,-hitfilt ", file);

    // Run21
    bfc(1,"P2021a,StiCA,BEmcChkStat,-hitfilt", file);

    const char* mapParFileName = "mapParameters.dat";
    StTpcHitMap *hitMap = new StTpcHitMap(StTpcDb::instance());
    
    //    cout << "execution path " << pathSwitch << endl;
    hitMap->calculateMap(chain->GetDataBase("RunLog"), StMagUtilities::Instance());
    hitMap->writeMap();
    
    double mapDriftVelocity = StTpcDb::instance()->DriftVelocity() * 1.0e-6;
    double mapDriftDistance = StTpcDb::instance()->Dimensions()->gatingGridZ();
    double mapTimeBinWidth  = 1.0 / StTpcDb::instance()->Electronics()->samplingFrequency();
    
    ofstream mapParOut(mapParFileName);
    mapParOut << setprecision(10);
    mapParOut << "  mapDriftVelocity      "<<mapDriftVelocity << "\n"
	      << mapDriftDistance << "\n"
	      << mapTimeBinWidth  << "\n"
	      << mapDriftVelocity * mapTimeBinWidth << "\n"
	      << StTpcDb::instance()->Electronics()->samplingFrequency() << endl;
    
    // for ( int sector = 1; sector <= 24; ++sector ) {
    //     mapParOut << StTpcDb::instance()->DriftVelocity(sector)*1e-6 << endl;
    // }
    mapParOut.close();
}        



