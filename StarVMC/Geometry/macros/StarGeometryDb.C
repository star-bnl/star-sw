// Star Geometry Database
Geom_t geom;

void setTitle(const Char_t *x) { geom.SetTitle(Form("STAR Geometry %s: %s",geom.select.Data(),x)); }

void StarGeometryDb()
{

  //std::cout << "Creating STAR Geometry Database" << std::endl;
  // Common to most geometry definitions

  geom.caveFlag = "CAVEon"; geom.caveStat=1; // Cave must be on
  geom.pipeFlag = "PIPEon"; geom.pipeStat=1; // Pipe must be on
  geom.magpFlag = "MAGPon"; geom.magpStat=1; // Magnet must be on
  geom.zcalFlag = "ZCALon"; geom.zcalStat=1; // ZDC must be on
  geom.upstFlag = "UPSTon"; geom.upstStat=1; // Upstream geometry on

  geom.bbcmFlag = "BBCMof"; geom.bbcmStat=0; // BBC defaults to off

  geom.idsmFlag = "IDSMof"; geom.idsmStat=0; // IDSM defaults to off
  geom.fgtdFlag = "FGTDof"; geom.fgtdStat=0; // FGTD defaults to off

  geom.fsceFlag = "FSCEof";  geom.fsceStat = 0; // FWD calo off by default
  geom.eiddFlag = "EIDDof";  geom.eiddStat = 0; // EIDD off by default
  geom.hcalFlag = "HCALof";  geom.hcalStat = 0; // Default HCAL off

  geom.tpcxFlag = "TPCXof";  geom.tpcxStat = 0; // TPC eXtreme off by default
  geom.istdFlag = "ISTDof";  geom.istdStat = 0; // ISTD off by default

  geom.pixlFlag = "PIXLof";  geom.pixlStat = 0; // Default pixel detector is off
  geom.pxstFlag = "PXSTof";  geom.pxstStat = 0; // Default piston support is off
  geom.psupFlag = "PSUPof";  geom.psupStat = 0; // Default supports off

  

  //
  // Setup STAR Geometries y2000 to present
  //
  //std::cout << ">>> Setup";
  y2000();   geom.Last(); setTitle("Year 2000 Production Geometry");  geom.select="y2000pro"; geom.fill();
  y2001();   geom.Last(); setTitle("Year 2001 Production Geometry");  geom.select="y2001pro"; geom.fill();
  y2002();   geom.Last(); setTitle("Year 2002 Production Geometry");  geom.select="y2002pro"; geom.fill();
  y2003();   geom.Last(); setTitle("Year 2003 Production Geometry");  geom.select="y2003pro"; geom.fill();
  y2004();   geom.Last(); setTitle("Year 2004 Production Geometry");  geom.select="y2004pro"; geom.fill();
  y2005();   geom.Last(); setTitle("Year 2005 Production Geometry");  geom.select="y2005pro"; geom.fill();
  y2006();   geom.Last(); setTitle("Year 2006 Production Geometry");  geom.select="y2006pro"; geom.fill();
  y2007();   geom.Last(); setTitle("Year 2007 Production Geometry");  geom.select="y2007pro"; geom.fill();
  y2008();   geom.Last(); setTitle("Year 2008 Production Geometry");  geom.select="y2008pro"; geom.fill();
  y2009();   geom.Last(); setTitle("Year 2009 Production Geometry");  geom.select="y2009pro"; geom.fill();
  y2010();   geom.Last(); setTitle("Year 2010 Production Geometry");  //geom.select="y2010pro"; geom.fill();
  y2011();   geom.Last(); setTitle("Year 2011 Production Geometry");  //geom.select="y2011pro"; geom.fill();
  y2012();   geom.Last(); setTitle("Year 2012 Production Geometry");  //geom.select="y2012pro"; geom.fill();
  y2013();   geom.Last(); setTitle("Year 2013 Production Geometry");  //geom.select="y2013pro"; geom.fill();
  y2014();   geom.Last(); setTitle("Year 2014 Production Geometry");   //geom.select="y2014pro"; geom.fill();
  y2015();   geom.Last(); setTitle("Year 2015 First Cut Geometry");   //geom.select="y2014pro"; geom.fill();
  y2016();   geom.Last(); setTitle("Yeat 2016 Asymptotic Geometry");

  // Setup upgrade geometries
  upgrade();

  // Setup development geometries
  devel();


  //
  // Upgrade studies tagged with a year (2000+)
  //
  //  dev14(); geom.Last(); setTitle("Year 2014 Development Geometry"); geom.select="dev14pro"; geom.fill();

  dev15(); 

  // resets things, so be careful...
  test();

  //
  // Geometries for studying future versions of the detector
  //
  complete(); geom.Last(); setTitle("Extrapolation of STAR to y2013.  Currently just FGT + IDSM."); geom.select="future"; geom.fill();

  //
  // Geometries for eRHIC studies
  //
  estar(); geom.Last(); setTitle("eSTAR simulation geometries."); 


  //std::cout << "<<<" << std::endl;

}

void y2000()
{
  // --------------------------------------------------------------------------------------------------------------------------- y2000
  //
  //replace [exe y2000;] with [;"corrected: MWC readout, RICH reconstructed position, no TOF ";
  //                            "actual 2000:  TPC+CTB+RICH+caloPatch+svtLadder"
  //                             exe TPCE00; exe VPDDof; exe ECALof; exe FTPCof;  exe SVTT00; exe CALB00; exe MFLD23;  ]
  //std::cout << " y2000 " << std::flush;
  geom.select   = "y2000";
  geom.tpceFlag = "TPCE00"; geom.tpceStat = 1;
  geom.svttFlag = "SVTT00"; geom.ftpcStat = 1;
  geom.calbFlag = "CALB00"; geom.calbStat = 1;
  geom.mfldFlag = "MFLD23"; geom.mfldStat = 1;
  geom.ftpcFlag = "FTPCof"; geom.ftpcStat = 0;
  geom.ecalFlag = "ECALof"; geom.ecalStat = 0;
  geom.vpddFlag = "VPDDof"; geom.vpddStat = 0;
  geom.bbcmFlag = "BBCMof"; geom.bbcmStat = 0;
  setTitle("First cut");
  geom.fill();

}

void y2001()
{

  // --------------------------------------------------------------------------------------------------------------------------- y2001
  //
  //replace [exe y2001;] with ["2001 geometry - TPC+CTB+FTPC+RICH+CaloPatch+SVT+FPD";
  //" 02/09/2004  Jerome signed off on changing, retroactively, the"
  //" position of the wafers in year2001, which was incorrectly offset"
  //" by 250 um insterad of 150 um."
  //" -- Obsoleted CorrNum = 1;"
  // exe TPCE00;        exe SVT101;        exe FTPC00;        exe BTOF42;         exe RICH02;        exe ECAL31;         exe CALBa0;         exe MFLD53;
  //std::cout << " y2001 "<< std::flush;;  
  geom.select   = "y2001";
  geom.svttFlag = "SVT101"; geom.svttStat = 1;
  geom.btofFlag = "BTOF42"; geom.btofStat = 1;
  geom.richFlag = "RICH02"; geom.richStat = 1;
  geom.ecalFlag = "ECAL31"; geom.ecalStat = 1;
  geom.calbFlag = "CALBa0"; geom.calbStat = 1;
  geom.mfldFlag = "MFLD53"; geom.mfldStat = 1;
  geom.bbcmFlag = "BBCMof"; geom.bbcmStat = 0;
  setTitle("First cut");
  geom.fill();

}

void y2002() 
{ 
  //replace [exe y2002;] with ["january 2002 geometry - TPC+CTB+FTPC+CaloPatch2+Rich+SVT3+BBC+FPD";                           
  //        exe TPCE00;         exe SVT100;         exe RICH02;         exe BTOF42;         exe CALBa0; 
  //        exe ECALof;        exe BBCMon;         exe FPDM00;         exe VPDD02;         exe MFLD54;
  //std::cout << " y2002 "<< std::flush;;
  geom.select   = "y2002";
  geom.svttFlag = "SVT100"; geom.svttStat = 1;
  geom.ecalFlag = "ECALof"; geom.ecalStat = 0;
  geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;
  geom.fpdmFlag = "FPDM00"; geom.fpdmStat = 1;
  geom.vpddFlag = "VPDD02"; geom.vpddStat = 1;
  geom.mfldFlag = "MFLD54"; geom.mfldStat = 1;
  geom.btofFlag = "BTOF42"; geom.btofStat = 1;
  geom.calbFlag = "CALBa0"; geom.calbStat = 1;
  setTitle("First cut");
  geom.fill();

}
        
void y2003()
{   //std::cout << " y2003 "<< std::flush;;

  //
  // Defaults for y2003
  //   
  {
    geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;
    geom.mutdFlag = "MUTDof"; geom.mutdStat = 0;
    geom.richFlag = "RICHof"; geom.richStat = 0;
    geom.upstFlag = "UPSTof"; geom.upstStat = 0;
    geom.calbFlag = "CALBb0"; geom.calbStat = 1; geom.calbCuts = 0;
    geom.ecalFlag = "ECAL11"; geom.ecalStat = 1; geom.ecalCuts = 0;
    geom.sconFlag = "SCONof"; geom.sconStat = 0;
    geom.phmdFlag = "PHMDof"; geom.phmdStat = 0;
    geom.ftroFlag = "FTROof"; geom.ftroStat = 0;
    geom.sisdFlag = "SISDof"; geom.sisdStat = 0;
  }

//replace [exe y2003;] with ["draft 2003 geometry - TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL";
//         exe TPCE00;          exe SVT100;          exe RICHof;          exe BTOF52; 
//         exe CALBb0;          exe ECAL11;          exe BBCMon;          exe FPDM00; 
//         exe VPDD03;          exe MFLD54;

  geom.select   = "y2003"; {
    geom.tpceFlag = "TPCE00"; geom.tpceStat = 1;
    geom.svttFlag = "SVT100"; geom.svttStat = 1;
    geom.richFlag = "RICHof"; geom.richStat = 0;
    geom.btofFlag = "BTOF52"; geom.btofStat = 1;
    geom.calbFlag = "CALBb0"; geom.calbStat = 1;         
    geom.ecalFlag = "ECAL11"; geom.ecalStat = 1;
    geom.fpdmFlag = "FPDM00"; geom.fpdmStat = 1;
    geom.vpddFlag = "VPDD03"; geom.vpddStat = 1;
    geom.mfldFlag = "MFLD54"; geom.mfldStat = 1;
    setTitle("First cut");
    geom.fill();
  }

  //***********************************************************************
  //* In y2003a:
  //*    removed serious bugs from SUPOGEO (incorrect positioning inside the SVT,
  //*    where is doesn't belong)
  //*    corrected CALB -- the shift variable (was 0,0 so the barrel wasn't tilted right)
  //*    corrected SVT  -- the layer radii (from 250 to 150 microns, see the svt code)
  //****************************************************************************************
  //    exe y2003;         exe CALBd0;         exe FTPC00;         exe SVT101
  geom.select = "y2003a"; {
    geom.calbFlag = "CALBd0"; geom.calbStat = 1;
    geom.ftpcFlag = "FTPC00"; geom.ftpcStat = 1;
    geom.svttFlag = "SVT101"; geom.svttStat = 1;
    geom.SetTitle("STAR Geometry y2003a");
    setTitle("Remove SUPO bugs, corrected CALB and SVT alignment");
    geom.fill();
  }

  //***********************************************************************
  //* y2003b is y2003a, but with the extra material in the SVT
  //* This is actually an important case (i.e. the "most precise" geometry
  //* approximation for the early 2003 run) which we were lacking so far.
  //* This is achieved by setting CorrNum to 2.
  //* The endcap EMC has one third of one wheel, as before
  //* For more info on the extra material in SVT -- see web page
  //***********************************************************************
  //  exe y2003a;         exe SVT102;
  geom.select = "y2003b"; {
    geom.svttFlag = "SVT102"; geom.svttStat = 1;
    setTitle("Extra SVT material");
    geom.fill();
  }
  
  //replace [exe y2003c;] with [ "Better SVT model on top of 2003B: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL";
  //        exe y2003b;         exe SVT106;
  geom.select = "y2003c"; {
    geom.svttFlag = "SVT106"; geom.svttStat = 1;
    setTitle("Improved SVT");
    geom.fill();
  }

  //replace [exe y2003x;] with [ "same as y2003b but with full calorimeters and PHMD";
  //        exe y2003b;         exe CALBc0;         exe ECAL33;          exe PHMD01;
  geom.select = "y2003x"; {
    geom.calbFlag = "CALBc0"; geom.calbStat = 1;
    geom.ecalFlag = "ECAL33"; geom.ecalStat = 1;
    geom.phmdFlag = "PHMD01"; geom.phmdStat = 1;
    setTitle("Development");
    geom.fill();
  }



}


void y2004()
{ //std::cout << " y2004 "<< std::flush;;

  //
  // Defaults for y2004
  //   
  {
    geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;
    geom.mutdFlag = "MUTDof"; geom.mutdStat = 0;
    geom.richFlag = "RICHof"; geom.richStat = 0;
    geom.upstFlag = "UPSTof"; geom.upstStat = 0;
  }

  //* baseline 2004 geometry: TPC+CTB+FTPC+CaloPatch2+SVT3+BBC+FPD+ECAL+PHMD with low cuts GSTPAR in PHMD;
  //replace [exe y2004;] with [
  //        exe TPCE01;        exe SVT103;         exe BTOF72;         exe CALB01;         exe ECAL31;         exe BBCMon; 
  //        exe FPDM01;        exe VPDD04;         exe MFLD54;         exe FTPC00;         exe PHMD01;         exe SISD02;
  geom.select   = "y2004"; {
    geom.tpceFlag = "TPCE01"; geom.tpceStat = 1;
    geom.svttFlag = "SVT103"; geom.svttStat = 1;
    geom.btofFlag = "BTOF72"; geom.btofStat = 1;
    geom.calbFlag = "CALB01"; geom.calbStat = 1; geom.calbCuts = 0;
    geom.ecalFlag = "ECAL31"; geom.ecalStat = 1; geom.ecalCuts = 0;
    geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;
    geom.fpdmFlag = "FPDM01"; geom.fpdmStat = 1;
    geom.vpddFlag = "VPDD04"; geom.vpddStat = 1;
    geom.ftpcFlag = "FTPC00"; geom.ftpcStat = 1;
    geom.phmdFlag = "PHMD01"; geom.phmdStat = 1;
    geom.sisdFlag = "SISD02"; geom.sisdStat = 1;
    geom.SetTitle("STAR Geometry y2004");
    geom.fill();
  }

  //replace [exe y2004a;] with [ exe y2004; exe PHMD02; ]
  geom.select = "y2004a"; {
    geom.phmdFlag = "PHMD02"; geom.phmdStat = 1;
    geom.SetTitle("STAR Geometry y2004a");
    geom.fill();
  }
    
  //replace [exe y2004b;] with [ exe y2004a; exe SISD12; ]
  geom.select = " y2004b"; {
    geom.sisdFlag = "SISD12"; geom.sisdStat = 1;
    geom.SetTitle("STAR Geometry y2004b");
    geom.fill();
  }

  //replace [exe y2004c;] with [ exe y2004b; exe TPCE02; exe SVT204; exe SCON02; ]
  geom.select = "y2004c"; {
    geom.tpceFlag = "TPCE02"; geom.tpceStat = 1;
    geom.svttFlag = "SVT204"; geom.svttStat = 1;
    geom.sconFlag = "SCON02"; geom.sconStat = 1;
    geom.SetTitle("STAR Geometry y2004c");
    geom.fill();
  }

  //replace [exe y2004d;] with [
  //                exe SVT206; exe BTOF72; exe CALB01; exe ECAL31; exe BBCMon; exe FPDM01; exe SCON02;
  //                exe VPDD04; exe MFLD54; exe FTPC01; exe PHMD02; exe SISD22; exe FTRO01; exe TPCE02;
  geom.select = "y2004d"; {
    geom.svttFlag = "SVT206"; geom.svttStat = 1;
    geom.btofFlag = "BTOF72"; geom.btofStat = 1;
    geom.calbFlag = "CALB01"; geom.calbStat = 1; 
    geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;
    geom.fpdmFlag = "FPDM01"; geom.fpdmStat = 1;
    geom.sconFlag = "SCON02"; geom.sconStat = 1;
    geom.vpddFlag = "VPDD04"; geom.vpddStat = 1;
    geom.mfldFlag = "MFLD54"; geom.mfldStat = 1;
    geom.ftpcFlag = "FTPC01"; geom.ftpcStat = 1;
    geom.phmdFlag = "PHMD02"; geom.phmdStat = 1;
    geom.sisdFlag = "SISD22"; geom.sisdStat = 1;
    geom.ftroFlag = "FTRO01"; geom.ftroStat = 1;
    geom.tpceFlag = "TPCE02"; geom.tpceStat = 1;
    geom.SetTitle("STAR Geometry y2004d");
    geom.fill();
  }
  

  // y2004x, y2004y deprecated for STAR VMC

  //*********   y2004x   ***
  //replace [exe y2004x;] with [
  //                exe SVT203; exe BTOF72; exe CALBe0; exe ECAL31; exe BBCMon; exe FPDM01;
  //                exe VPDD04; exe MFLD54; exe FTPC01; exe PHMD02; exe SISD02; exe TPCE01;
  //]

  geom.select = "y2004x"; {
    geom.svttFlag = "SVT203";
    geom.calbFlag = "CALBe0";
    geom.sisdFlag = "SISD02";
    geom.tpceFlag = "TPCE01";
    geom.fill();
  };

  //*********   y2004y   ***
  //replace [exe y2004y;] with [
  //                exe SVT204; exe SCON02; exe BTOF72; exe CALBe0; exe ECAL31; exe BBCMon; exe FPDM01;
  //                exe VPDD04; exe MFLD54; exe FTPC01; exe FTRO01; exe PHMD02; exe SISD22; exe TPCE02;
  //]
  geom.select = "y2004y"; {
    geom.svttFlag = "SVT204";
    geom.ftpcFlag = "FTPC01";
    geom.ftroFlag = "FTRO01";
    geom.sisdFlag = "SISD22";
    geom.tpceFlag = "TPCE02";
    geom.fill();
  };

}

void y2005()
{ 

  //std::cout << " y2005 "<< std::flush;;
  // Switch on some detectors by default
  geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;

  // Switch off some detectors by default in y2005
  geom.mutdFlag = "MUTDof"; geom.mutdStat = 0;
  geom.richFlag = "RICHof"; geom.richStat = 0;



  //replace [exe y2005;] with [
  //         exe SVT203; exe SCON02; exe BTOF72; exe CALB01; exe ECAL31; exe BBCMon; exe FPDM01;
  //         exe VPDD04; exe MFLD54; exe FTPC01; exe PHMD02; exe FTRO01; exe SISD23; exe TPCE01;
  geom.select = "y2005"; {
    geom.svttFlag = "SVT203"; geom.svttStat = 1;
    geom.sconFlag = "SCON02"; geom.sconStat = 1;
    geom.btofFlag = "BTOF72"; geom.btofStat = 1;
    geom.calbFlag = "CALB01"; geom.calbStat = 1;
    geom.ecalFlag = "ECAL31"; geom.ecalStat = 1;
    geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;
    geom.fpdmFlag = "FPDM01"; geom.fpdmStat = 1;
    geom.vpddFlag = "VPDD04"; geom.vpddStat = 1;
    geom.mfldFlag = "MFLD54"; geom.mfldStat = 0;
    geom.ftpcFlag = "FTPC01"; geom.ftpcStat = 1;
    geom.phmdFlag = "PHMD02"; geom.phmdStat = 1;
    geom.ftroFlag = "FTRO01"; geom.ftroStat = 1;
    geom.sisdFlag = "SISD23"; geom.sisdStat = 1;
    geom.tpceFlag = "TPCE01"; geom.tpceStat = 1;
    geom.SetTitle("STAR Geometry y2005");
    geom.fill();
  }


  //replace [exe y2005b;] with [
  //                exe SVT204; exe SCON02; exe BTOF72; exe CALB01; exe ECAL31; exe BBCMon; exe FPDM01;
  //                exe VPDD04; exe MFLD54; exe FTPC01; exe PHMD02; exe FTRO01; exe SISD24; exe TPCE02;
  geom.select = "y2005b";{
    geom.svttFlag = "SVT204"; geom.svttStat = 1;
    geom.sisdFlag = "SISD24"; geom.sisdStat = 1;
    geom.tpceFlag = "TPCE02"; geom.tpceStat = 1;
    geom.SetTitle("STAR Geometry y2005b");    
    geom.fill();
  }

  //replace [exe y2005c;] with [ exe y2005b;  exe BTOF84;]
  geom.select = "y2005c";{
    geom.btofFlag = "BTOF84"; geom.btofStat = 1;
    geom.SetTitle("STAR Geometry y2005c");    
    geom.fill();
  }

  //replace [exe y2005d;] with [ exe y2005c;  exe SVT206;]
  geom.select = "y2005d";
  geom.svttFlag = "SVT206"; geom.svttStat = 1;
  geom.SetTitle("STAR Geometry y2005d");
  geom.fill();

  //replace [exe y2005e;] with [
  //                exe SVT306; exe SCON02; exe BTOF84; exe CALB01; exe ECAL31; exe BBCMon; exe FPDM01;
  //                exe VPDD04; exe MFLD54; exe FTPC01; exe PHMD02; exe FTRO01; exe SISD35; exe TPCE02;
  geom.select = "y2005e";
  geom.svttFlag = "SVT306"; geom.svttStat = 1;
  geom.sconFlag = "SCON02"; geom.sconStat = 1;
  geom.btofFlag = "BTOF84"; geom.btofStat = 1;
  geom.calbFlag = "CALB01"; geom.calbStat = 1;
  geom.ecalFlag = "ECAL31"; geom.ecalStat = 1;
  geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;
  geom.fpdmFlag = "FPDM01"; geom.fpdmStat = 1;
  geom.vpddFlag = "VPDD04"; geom.vpddStat = 1;
  geom.mfldFlag = "MFLD54"; geom.mfldStat = 0;
  geom.ftpcFlag = "FTPC01"; geom.ftpcStat = 1;
  geom.phmdFlag = "PHMD02"; geom.phmdStat = 1;
  geom.ftroFlag = "FTRO01"; geom.ftroStat = 1;
  geom.sisdFlag = "SISD35"; geom.sisdStat = 1;
  geom.tpceFlag = "TPCE02"; geom.tpceStat = 1;
  geom.SetTitle("STAR Geometry y2005e");
  geom.fill();


  //replace [exe y2005f;] with [ 
  //             exe y2005e;  exe CALB02; exe EMCUTS(bemc,0); "disable 10 keV cuts"; exe EMCUTS(eemc,0); "disable 10 keV cuts";
  //             exe SISD55;
  geom.select = "y2005f";
  geom.calbFlag = "CALB02"; geom.calbStat = 1; geom.calbCuts = 0;
  geom.ecalFlag = "ECAL31"; geom.ecalStat = 1; geom.ecalCuts = 0;
  geom.sisdFlag = "SISD55"; geom.sisdStat = 1;
  geom.SetTitle("STAR Geometry y2005f");
  geom.fill();

  //replace [exe y2005g;] with [ 
  //         exe y2005f;          exe svt312;          exe sisd75;]
  geom.select = "y2005g";
  geom.svttFlag = "SVT312"; geom.svttStat = 1;
  geom.sisdFlag = "SISD75"; geom.sisdStat = 1;
  geom.SetTitle("STAR Geometry y2005g");  
  geom.fill();

  //replace [exe y2005h;] with [         exe y2005g;         exe TPCE04;        ]
  geom.select = "y2005h";
  geom.tpceFlag = "TPCE04"; geom.tpceStat = 1;
  geom.SetTitle("STAR Geometry y2005h");  
  geom.fill();

  //replace [exe y2005i;] with [
  //        exe y2005h; 
  //        exe ECALv6;         "Latest version of the EEMC geometry";
  //        exe EMCUTS(bemc,1); "10 keV transport cuts in the BEMC";
  //        exe EMCUTS(eemc,1); "10 keV transport cuts in the EEMC";
  geom.select = "y2005i";
  geom.calbFlag = "CALB02"; geom.calbStat = 1; geom.calbCuts = 1;
  geom.ecalFlag = "ECALv6"; geom.ecalStat = 1; geom.ecalCuts = 1;
  geom.SetTitle("STAR Geometry y2005i");  
  geom.fill();

}

void y2006()
{
  //std::cout << " y2006 "<< std::flush;;
  //replace [exe y2006;] with [ "y2006 baseline which is Y2005D+fixed TPC backplane+New SSD"
  //                exe SVT306; exe SCON02; exe BTOF84; exe CALB01; exe ECAL31;	exe BBCMon; exe FPDM01; exe VPDD04; 
  //                exe MFLD54; exe FTPC01; exe PHMD02; exe FTRO01; exe SISD35; exe TPCE03; exe mutd01; exe cave03;
  geom.select = "y2006";
  geom.svttFlag = "SVT306"; geom.svttStat = 1;
  geom.sconFlag = "SCON02"; geom.sconStat = 1;
  geom.btofFlag = "BTOF84"; geom.btofStat = 1;
  geom.calbFlag = "CALB01"; geom.calbStat = 1;
  geom.ecalFlag = "ECAL31"; geom.ecalStat = 1;
  geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;
  geom.fpdmFlag = "FPDM01"; geom.fpdmStat = 1;
  geom.vpddFlag = "VPDD04"; geom.vpddStat = 1;
  geom.mfldFlag = "MFLD54"; geom.mfldStat = 0;
  geom.ftpcFlag = "FTPC01"; geom.ftpcStat = 1;
  geom.phmdFlag = "PHMD02"; geom.phmdStat = 1;
  geom.ftroFlag = "FTRO01"; geom.ftroStat = 1;
  geom.sisdFlag = "SISD35"; geom.sisdStat = 1;
  geom.tpceFlag = "TPCE03"; geom.tpceStat = 1;
  geom.mutdFlag = "MUTD01"; geom.mutdStat = 1;
  geom.caveFlag = "CAVE03"; geom.caveStat = 1;
  geom.SetTitle("STAR Geometry y2006");
  geom.fill();
                 
  //replace [exe y2006a;] with ["Y2006 baseline which is Y2005D+fixed TPC backplane+New SSD"
  //         exe y2006;         exe FPDM02;
  geom.select = "y2006a";
  geom.fpdmFlag = "FPDM02"; geom.fpdmStat = 1;
  geom.SetTitle("STAR Geometry y2006a");
  geom.fill();

  //replace [exe y2006b;] with ["Y2006A + improved SSD with dead area + improved CALB"
  //         exe y2006;  exe CALB02; exe EMCUTS(bemc,0); "disable 10 keV cuts"; exe EMCUTS(eemc,0); "disable 10 keV cuts";
  //         exe FPDM02; exe SISD55;
  geom.select = "y2006b";
  geom.calbFlag = "CALB02"; geom.calbStat = 1; geom.calbCuts = 0;
  geom.ecalFlag = "ECAL31"; geom.ecalStat = 1; geom.ecalCuts = 0;
  geom.fpdmFlag = "FPDM02"; geom.fpdmStat = 1;
  geom.sisdFlag = "SISD55"; geom.sisdStat = 1;
  geom.SetTitle("STAR Geometry y2006b");
  geom.fill();

  //replace [exe y2006c;] with ["Y2006B without the PHMD"
  //         exe y2006b; exe PHMDof;
  geom.select = "y2006c";
  geom.phmdFlag = "PHMDof"; geom.phmdStat = 0;
  geom.SetTitle("STAR Geometry y2006c");
  geom.fill();

  //replace [exe y2006g;] with ["Y2006C new SVT dead material"
  //         exe y2006c;  exe SVT312; exe SISD75;
  geom.select = "y2006g";
  geom.svttFlag = "SVT312"; geom.svttStat = 1;
  geom.sisdFlag = "SISD75"; geom.sisdStat = 1;
  geom.SetTitle("STAR Geometry y2006g");
  geom.fill();

  //replace [exe y2006h;] with ["y2006g + new BEMC, new EEMC";
  //        exe y2006g;    "Y2006h modifies Y2006g geometry"; 
  //        exe TPCe04;    "Latest model of the TPC, with additional mass";  
  //exe CALB02;    "Latest model of the BEMC, with additional volumes";
  //    exe ECALv6;    "Latest model of the EEMC, with additional volumes and bug fixes";
  //    exe EMCUTS(eemc,1); "10 keV cuts in b/emc calorimeter volumes";
  //    exe EMCUTS(bemc,1); "10 keV cuts in b/emc calorimeter volumes";
  //    ]
  geom.select = "y2006h";
  geom.tpceFlag = "TPCE04"; geom.tpceStat = 1;
  geom.calbFlag = "CALB02"; geom.calbStat = 1; geom.calbCuts = 1;
  geom.ecalFlag = "ECALv6"; geom.ecalStat = 1; geom.ecalCuts = 1;
  geom.SetTitle("STAR Geometry y2006h");
  geom.fill();

}

void y2007()
{
  //std::cout << " y2007 "<< std::flush;;
  //  replace [exe y2007;] with ["y2006 baseline which is Y2006+FMS"
  //           exe SVT306x; exe SCON02; exe BTOFa5; exe CALB02; exe ECAL31;
  //           exe EMCUTS(eemc,0);   "disable 10 keV calorimeter cuts";
  //           exe EMCUTS(bemc,0);   "disable 10 keV calorimeter cuts";
  //           exe BBCMon;  exe FPDM03; exe VPDD07; exe MFLD54; exe FTPC01; exe PHMD02; exe FTRO01; exe SISD55; exe TPCE03; exe mutd01; exe cave04;
  geom.select   = "y2007"; {
    geom.svttFlag = "SVT306x"; geom.svttStat = 1;
    geom.sconFlag = "SCON02";  geom.sconStat = 1;
    geom.btofFlag = "BTOFa5";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1;  geom.calbCuts = 0;
    geom.ecalFlag = "ECAL31";  geom.ecalStat = 1;  geom.ecalCuts = 0;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.vpddFlag = "VPDD07";  geom.vpddStat = 1;
    geom.mfldFlag = "MFLD54";  geom.mfldStat = 0;
    geom.ftpcFlag = "FTPC01";  geom.ftpcStat = 1;
    geom.phmdFlag = "PHMD02";  geom.phmdStat = 1;
    geom.ftroFlag = "FTRO01";  geom.ftroStat = 1;
    geom.sisdFlag = "SISD55";  geom.sisdStat = 1;
    geom.tpceFlag = "TPCE03";  geom.tpceStat = 1;
    geom.mutdFlag = "MUTD01";  geom.mutdStat = 1;
    geom.caveFlag = "CAVE04";  geom.caveStat = 1;    
    geom.fpdmFlag = "FPDM03";  geom.fpdmStat = 1; /* FMS geometry */
    geom.SetTitle("STAR Geometry y2007");
  }
  geom.fill();

  //replace [exe y2007a;] with ["y2007 but corrected SVT,carbon instead of Be water channels";
  // exe y2007; exe SVT310x;
  geom.Use("select", "y2007");
  geom.select = "y2007a"; {
    geom.svttFlag = "SVT310x"; geom.svttStat = 1;
    geom.SetTitle("STAR Geometry y2007a");
  }
  geom.fill();


  //replace [exe y2007g;] with ["y2007A + dead material from Rene"
  //         exe y2007a;          exe SVT312x;         exe SISD75;
  geom.Use("select", "y2007a");
  geom.select = "y2007g"; {
    geom.svttFlag = "SVT312x"; geom.svttStat = 1;
    geom.sisdFlag = "SISD75";  geom.sisdStat = 1;
    geom.SetTitle("STAR Geometry y2007g");
  }
  geom.fill();


  //replace [exe y2007h;] with ["y2007g + TPC y2009"
  //         exe y2007g;          exe TPCE04;         ]
  geom.Use("select","y2007g"); 
  geom.select = "y2007h"; {
    geom.tpceFlag = "TPCE04"; geom.tpceStat = 1;
    geom.vpddFlag = "VPDD07"; geom.vpddStat = 1;
    geom.SetTitle("STAR Geometry y2007h: y2007h with TPCe04 model");
  }
  geom.fill();

}


void y2008() 
{

  geom.mfldFlag = "MFLDof";

  //std::cout << " y2008 "<< std::flush;;
  //replace [exe y2008;] with [;
  //{ "y2008 baseline: no SVT,  cones,beam support,FTPC in CAVE now"
  //    exe SCON02;    exe TPCE03;    exe BTOFb6;    exe CALB02;    exe ECAL31;
  //    exe EMCUTS(eemc,0);   "disable 10 keV calorimeter cuts";
  //    exe EMCUTS(bemc,0);   "disable 10 keV calorimeter cuts";
  //    exe BBCMon;    exe FPDM03;    exe VPDD07;    exe FTPC01;    exe SVTTof;    exe PHMDof;    exe SISDof;    exe FTRO01;    exe MUTD03;    exe CAVE04;    exe PIPE12;
  geom.select = "y2008";
  geom.sconFlag = "SCON02"; geom.sconStat = 1;
  geom.tpceFlag = "TPCE03"; geom.tpceStat = 1;
  geom.btofFlag = "BTOFb6"; geom.btofStat = 1;
  geom.calbFlag = "CALB02"; geom.calbStat = 1; geom.calbCuts = 0;
  geom.ecalFlag = "ECAL31"; geom.ecalStat = 1; geom.ecalCuts = 0;
  geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;
  geom.fpdmFlag = "FPDM03"; geom.fpdmStat = 1;
  geom.ftpcFlag = "FTPC01"; geom.ftpcStat = 1;
  geom.svttFlag = "SVTTof"; geom.svttStat = 0; // No SVT from 2008
  geom.phmdFlag = "PHMDof"; geom.phmdStat = 0; // PHMD out in 2008
  geom.sisdFlag = "SISDof"; geom.sisdStat = 0;
  geom.ftroFlag = "FTRO01"; geom.ftroStat = 1;
  geom.mutdFlag = "MUTD03"; geom.mutdStat = 1;
  geom.caveFlag = "CAVE04"; geom.caveStat = 1;
  geom.pipeFlag = "PIPE12"; geom.pipeStat = 1;
  geom.vpddFlag = "VPDD07"; geom.vpddStat = 1;
  geom.SetTitle("STAR Geometry y2008");
  geom.fill();


  //replace [exe y2008a;] with [;exe y2008; exe SCON13;]
  geom.Use("select","y2008");
  geom.select = "y2008a"; {
    geom.sconFlag = "SCON13"; geom.sconStat = 1;
    geom.SetTitle("STAR Geometry y2008a");
    geom.fill();
  }

  //replace [exe y2008b;] with [;exe y2008a; exe TPCE04; exe CALB02; exe ECALv6;
  geom.Use("select","y2008a");
  geom.select = "y2008b"; {
    geom.tpceFlag = "TPCE04"; geom.tpceStat = 1;
    geom.calbFlag = "CALB02"; geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6"; geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.SetTitle("STAR Geometry y2008b");
    geom.fill();
  }


  //replace [exe y2008c;] with ["Y2008 production tag C: Fixes TOF response " ; 
  //        exe y2008b ; "Inherit everything from y2008b";
  //        exe TPCE04r; "Reduce the TPC envelope raidus";
  //        exe BTOFb7;           "Fixed TOF sensitve volumes";
  geom.Use("select","y2008b");
  geom.select = "y2008c"; {
    geom.tpceFlag = "TPCE04r"; geom.tpceStat = 1;
    geom.btofFlag = "BTOFb7";  geom.btofStat = 1;
    geom.SetTitle("STAR Geometry y2008c");
    geom.fill();
  }

  geom.Use("select","y2008c");
  geom.select="y2008d"; {
    geom.sconFlag = "SCON14"; geom.sconStat = 1;
    geom.fill();
  }

  geom.Use("select","y2008d");
  geom.select="y2008e"; {
    geom.calbFlag = "CALB02"; geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6"; geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.fill();
  }


}

void y2009()
{
  //std::cout << " y2009 "<< std::flush;;
  geom.upstFlag = "UPSTon"; geom.upstStat=1;


  //replace [exe y2009;] with [;{   "y2009 baseline: much more detailed TPC (thnx YF)"
  //    exe SCON13;    exe TPCE04;    exe BTOFc6;    exe CALB02;    exe ECAL31;   
  //    exe EMCUTS(eemc,0); "disable 10 keV cuts";
  //    exe EMCUTS(bemc,0); "disable 10 keV cuts";
  //    exe BBCMon;    exe FPDM03;    exe VPDD07;    exe FTPC01;    exe SVTTof;    exe PHMDof;    exe SISDof;    exe FTRO01;    exe MUTD03;    exe CAVE04;    exe PIPE12; 
  geom.select = "y2009"; {
    geom.sconFlag = "SCON13"; geom.sconStat = 1;
    geom.tpceFlag = "TPCE04"; geom.tpceStat = 1;
    geom.btofFlag = "BTOFc6"; geom.btofStat = 1;
    geom.calbFlag = "CALB02"; geom.calbStat = 1;
    geom.ecalFlag = "ECAL31"; geom.ecalStat = 1;
    geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;
    geom.fpdmFlag = "FPDM03"; geom.fpdmStat = 1;
    geom.vpddFlag = "VPDD07"; geom.vpddStat = 1;
    geom.ftpcFlag = "FTPC01"; geom.ftpcStat = 1;
    geom.svttFlag = "SVTTof"; geom.svttStat = 0;
    geom.phmdFlag = "PHMDof"; geom.phmdStat = 0;
    geom.sisdFlag = "SISDof"; geom.sisdStat = 0;
    geom.ftroFlag = "FTRO01"; geom.ftroStat = 1;
    geom.mutdFlag = "MUTD03"; geom.mutdStat = 1;
    geom.caveFlag = "CAVE04"; geom.caveStat = 1;
    geom.pipeFlag = "PIPE12"; geom.pipeStat = 1;
    geom.SetTitle("STAR Geometry y2009");
    geom.fill();
  }
  

  //replace [exe y2009a;] with [;{   "y2009a baseline: much more detailed TPC (thnx YF), version 6.1 of the endcap geometry"
  //    exe SCON13;      "support cone without SVT and new cable weight estimates";
  //    exe TPCE04;      "agstar version of yf model";
  //    exe BTOFc6;      "time of flight";
  //    exe CALB02;      "updated bemc model";
  //    exe ECALv6;      "several bugfixes in eemc geometry"; ===
  //    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
  //    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
  //    exe BBCMon;      "beam beam counters";
  //    exe FPDM03;      "";
  //    exe VPDD07;      "";
  //    exe FTPC01;      "";
  //    exe SVTTof;      "";
  //    exe PHMDof;      "Photon mult detector out of 2009a";
  //    exe SISDof;
  //    exe FTRO01;
  //    exe MUTD03;
  //    exe CAVE04;
  //    exe PIPE12;
  //};]
  geom.Use("select","y2009");
  geom.select = "y2009a"; {
    geom.calbFlag = "CALB02"; geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6"; geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.SetTitle(Form("STAR Geometry %s",geom.select.Data()));
    geom.fill();
  }


  //replace [exe y2009b;] with [;
  //{   "y2009b production tag B: Y2009A tag with the old tracking cuts in the EEMC.";
  //"This tag is not appropriate for EEMC simulations.";
  //  exe Y2009A;           "Y2009A configugration";
  //  exe EMCUTS(eemc,0);   "10 keV EM thresholds in barrel and endcap calorimeters";
  //  exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
  geom.Use("select","y2009a");
  geom.select="y2009b"; {
    geom.ecalFlag = "ECALv6"; geom.ecalStat = 1; geom.ecalCuts = 0;
    geom.SetTitle("STAR Geometry y2009b: y2009a with high EM cuts in EEMC.");
    geom.fill();
  }
  
  //replace [exe y2009c;] with [;
  //   "y2009b production tag C: Y2009A tag with fixed TOF resonse";
  //   exe Y2009A;           "Y2009A configugration";
  //   exe TPCE04r;          "Reduced TPC envelope radius";
  //   exe BTOFc7;           "Fixed TOF sensitve volumes";
  geom.Use("select","y2009a");
  geom.select = "y2009c"; {
    geom.tpceFlag = "TPCE04r"; geom.tpceStat = 1;
    geom.btofFlag = "BTOFc7";  geom.btofStat = 1;
    geom.SetTitle("Star Geometry y2009c");
    geom.fill();
  }

  geom.Use("select","y2009c"); 
  geom.select="y2009d";
  {
    geom.sconFlag = "SCON14"; geom.sconStat = 1;
    geom.fill();
  }

}

void y2010()
{
  //std::cout << " y2010 "<< std::flush;;
  //replace [exe y2010;] with [;
  //{ "y2010 baseline: y2009a+full tof+phmd, blessed 04/13 jcw"
  //  exe y2009a; 
  //  exe BTOF66;
  //  exe PHMD02;
  //};]
  geom.Use("select","y2009a");
  geom.select = "y2010"; 
  geom.btofFlag = "BTOF66"; geom.btofStat = 1;
  geom.phmdFlag = "PHMD02"; geom.phmdStat = 1;
  geom.SetTitle("STAR Geometry y2010");
  geom.fill();
  
  

  //replace [exe y2010a;] with [;
  // "y2010a: production tag A"
  //  exe SCON13;      "support cone without SVT and new cable weight estimates";
  //  exe TPCE04;      "agstar version of yf model";
  //  exe BTOF66;      "time of flight";
  //  exe CALB02;      "updated bemc model";
  //  exe ECALv6;      "several bugfixes in eemc geometry";
  //  exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
  //  exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
  //  exe BBCMon;      "beam beam counters";
  //  exe FPDM03;      "";
  //  exe VPDD07;      "";
  //  exe FTPC01;      "";
  //  exe SVTTof;      "";
  //  exe PHMD02;      "Photon mult detector";
  //  exe SISDof;
  //  exe FTRO01;
  //  exe MUTD03;
  //  exe CAVE04;
  //  exe PIPE12;
  //;]
  geom.Use("select","y2010");
  geom.select="y2010a"; {
    geom.btofFlag = "BTOF66"; geom.btofStat = 1;
    geom.phmdFlag = "PHMD02"; geom.phmdStat = 1;
    geom.ecalFlag = "ECALv6"; geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.calbFlag = "CALB02"; geom.calbStat = 1; geom.calbCuts = 1;
    geom.SetTitle("STAR Geometry y2010a");
    geom.fill();
  }


  //place [exe y2010b;] with ["Y2010 production tag B: Based on A, with TOF fixes";
  // exe y2010a;           "Inherit from y2010a";
  // exe TPCE04r;          "reduced TPC envelope raidus";
  // exe BTOF67;           "fixes to TOF sensitive volume dimensions";
  geom.Use("select","y2010a");
  geom.select = "y2010b"; {
    geom.tpceFlag = "TPCE04r"; geom.tpceStat = 1;
    geom.btofFlag = "BTOF67";  geom.btofStat = 1;
    setTitle("y2010a with TPC radius/TOF fix");
    geom.fill();
  }

  geom.Use("select","y2010b");
  geom.select="y2010c";
  {
    geom.sconFlag="SCON14"; geom.sconStat=1;
    geom.fill();
  }

  geom.Use("select","y2010c");
  geom.select="y2010x";
  {
    geom.tpceFlag="TPCE40"; geom.tpceStat=1;
    geom.fill();
  }

}

void y2011()
{
  //std::cout << " y2011 "<< std::flush;;
  /*
REPLACE [exe y2011;] with ["y2011 baseline: Essentially Y2010a with fixes to TPC envelope radius and TOF";
    exe SCON13;      "support cone without SVT and new cable weight estimates";
    exe TPCE04r;     "agstar version of yf model with reduced Rmax";
    exe BTOF67;      "time of flight";
    exe CALB02;      "updated bemc model";
    exe ECALv6;      "several bugfixes in eemc geometry";
    exe EMCUTS(eemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe EMCUTS(bemc,1);   "10 keV EM thresholds in barrel and endcap calorimeters";
    exe BBCMon;      "beam beam counters";
    exe FPDM03;      "Latest version of FPD";
    exe VPDD07;      "Latest version of VPD";
    exe FTPC01;      "FTPC";
    exe SVTTof;      "No SVT";
    exe PHMD02;      "Photon mult detector on";
    exe SISDof;      "No sisd";
    exe FTRO01;      "FTPC readout";
    exe MUTD04;      "Muon telescope detector";
    exe CAVE04;      "Cave and tunnel";
    exe PIPE12;      "The beam pipe";
  */
  geom.select = "y2011"; {
    geom.sconFlag = "SCON14"; geom.sconStat = 1;
    geom.tpceFlag = "TPCE04r"; geom.tpceStat = 1;
    geom.btofFlag = "BTOF67"; geom.btofStat = 1;
    geom.calbFlag = "CALB02"; geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6"; geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;
    geom.fpdmFlag = "FPDM03"; geom.fpdmStat = 1;
    geom.vpddFlag = "VPDD07"; geom.vpddStat = 1;
    geom.ftpcFlag = "FTPC01"; geom.ftpcStat = 1;
    geom.svttFlag = "SVTTof"; geom.svttStat = 0;
    geom.phmdFlag = "PHMD02"; geom.phmdStat = 1;
    geom.sisdFlag = "SISDof"; geom.sisdStat = 0;
    geom.ftroFlag = "FTRO01"; geom.ftroStat = 1;
    geom.mutdFlag = "MUTD04"; geom.mutdStat = 1;
    geom.caveFlag = "CAVE04"; geom.caveStat = 1;
    geom.pipeFlag = "PIPE12"; geom.pipeStat = 1;
    setTitle("First cut");
    geom.fill();
  }

  geom.select = "y2011a"; {
    geom.sconFlag = "SCON14"; geom.sconStat = 1;
    geom.tpceFlag = "TPCE04r"; geom.tpceStat = 1;
    geom.btofFlag = "BTOF67"; geom.btofStat = 1;
    geom.calbFlag = "CALB02"; geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6"; geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon"; geom.bbcmStat = 1;
    geom.fpdmFlag = "FPDM03"; geom.fpdmStat = 1;
    geom.vpddFlag = "VPDD07"; geom.vpddStat = 1;
    geom.ftpcFlag = "FTPC01"; geom.ftpcStat = 1;
    geom.svttFlag = "SVTTof"; geom.svttStat = 0;
    geom.phmdFlag = "PHMD02"; geom.phmdStat = 1;
    geom.sisdFlag = "SISDof"; geom.sisdStat = 0;
    geom.ftroFlag = "FTRO01"; geom.ftroStat = 1;
    geom.mutdFlag = "MUTD04"; geom.mutdStat = 1;
    geom.caveFlag = "CAVE04"; geom.caveStat = 1;
    geom.pipeFlag = "PIPE12"; geom.pipeStat = 1;
    setTitle("Production geometry y2011a");
    geom.fill();
  }

  geom.select="y2011x";
  {
    geom.tpceFlag="TPCE40"; geom.tpceStat=1;
    geom.fill();
  }

}


void y2012()
{
  //std::cout << " y2012 "<< std::flush;;
  geom.Use("select","y2011");

  geom.select = "y2012"; {
    // ================================================ 
    geom.caveFlag = "CAVE04";  geom.caveStat = 1;
    // ================================================ 
    geom.sconFlag = "SCONof"; geom.sconStat = 0;
    geom.ftroFlag = "FTROof"; geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof"; geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof"; geom.svttStat = 0;
    geom.phmdFlag = "PHMDof"; geom.phmdStat = 0;
    // ================================================ 
    geom.tpceFlag = "TPCE04r"; geom.tpceStat = 1;
    geom.btofFlag = "BTOF67";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.fpdmFlag = "FPDM03";  geom.fpdmStat = 1;
    geom.vpddFlag = "VPDD07";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD12";  geom.mutdStat = 1;
    geom.pipeFlag = "PIPE12";  geom.pipeStat = 1;
    setTitle("First cut");
    // ================================================ 
    geom.idsmFlag = "IDSM01";   geom.idsmStat = 1;
    geom.fgtdFlag = "FGTD31";   geom.fgtdStat=1;
    // ================================================ 
    setTitle("Upgrade studies with 6 complete FGT disks");
    geom.fill();
  };

  geom.select = "y2012a"; {
    // ================================================ 
    geom.caveFlag = "CAVE04";  geom.caveStat = 1;
    // ================================================ 
    geom.sconFlag = "SCONof"; geom.sconStat = 0;
    geom.ftroFlag = "FTROof"; geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof"; geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof"; geom.svttStat = 0;
    geom.phmdFlag = "PHMDof"; geom.phmdStat = 0;
    // ================================================ 
    geom.tpceFlag = "TPCE04r"; geom.tpceStat = 1;
    geom.btofFlag = "BTOF67";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.fpdmFlag = "FPDM03";  geom.fpdmStat = 1;
    geom.vpddFlag = "VPDD07";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD12";  geom.mutdStat = 1;
    geom.pipeFlag = "PIPE12";  geom.pipeStat = 1;
    setTitle("First cut");
    // ================================================ 
    geom.idsmFlag = "IDSM01";   geom.idsmStat = 1;
    geom.fgtdFlag = "FGTD31";   geom.fgtdStat=1;
    // ================================================ 
    setTitle("y2012a production geometry tag");
    geom.fill();
  };

  // Y2012b is identical to y2012a.  New tag defined because starsim/geometry.g
  // did not create the MTD in either y2012 or y2012a tags.
  geom.select = "y2012b"; {
    setTitle("Y2012b production geometry tag");
    geom.fill();
  }

  geom.select="y2012x";
  {
    geom.tpceFlag="TPCE40"; geom.tpceStat=1;
    geom.fill();
  }

}


void complete()
{
  //std::cout << " y2012 "<< std::flush;;
  geom.Use("select","y2011");

  geom.select = "complete"; {
    // ================================================ 
    geom.caveFlag = "CAVE04";  geom.caveStat = 1;
    // ================================================ 
    geom.sconFlag = "SCONof";  geom.sconStat = 0;
    geom.ftroFlag = "FTROof";  geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof";  geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof";  geom.svttStat = 0;
    // ================================================ 
    geom.tpceFlag = "TPCE04r"; geom.tpceStat = 1;
    geom.btofFlag = "BTOF67";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.fpdmFlag = "FPDM03";  geom.fpdmStat = 1;
    geom.vpddFlag = "VPDD07";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD05";  geom.mutdStat = 1;
    geom.pipeFlag = "PIPE12";  geom.pipeStat = 1;
    setTitle("First cut");
    // ================================================ 
    geom.idsmFlag = "IDSM01";  geom.idsmStat = 1;
    geom.fgtdFlag = "FGTD32";  geom.fgtdStat=1;
    // ================================================ 
    // ================================================
    setTitle("Upgrade studies with 6 complete FGT disks");
    geom.fill();
  };

}



void estar()
{
  //std::cout << " estar "<< std::flush;;

  geom.select = "devE"; {
    // ================================================ The 4th version of the CAVE
    geom.caveFlag = "CAVE04";  geom.caveStat = 1;
    // ================================================ Disable old STAR inner detectors
    geom.sconFlag = "SCONof";  geom.sconStat = 0;
    geom.ftroFlag = "FTROof";  geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof";  geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof";  geom.svttStat = 0;
    geom.phmdFlag = "PHMDof";  geom.phmdStat = 0;
    // ================================================ Tracking, calorimetry, etc..
    geom.tpceFlag = "TPCE04r"; geom.tpceStat = 1;
    geom.btofFlag = "BTOF67";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.vpddFlag = "VPDD07";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD04";  geom.mutdStat = 1;
    geom.pipeFlag = "PIPE12";  geom.pipeStat = 1;
    setTitle("First cut");
    // ================================================ The new Inner Detectors
    geom.idsmFlag = "IDSM01";  geom.idsmStat = 1;
    geom.fgtdFlag = "FGTDvf";  geom.fgtdStat = 1;
    // ================================================ 
    geom.fpdmFlag = "FPDM13";  geom.fpdmStat = 1;        // open FPDM
    geom.fsceFlag = "FSCEv0";  geom.fsceStat = 1;
    geom.eiddFlag = "EIDDv0";  geom.eiddStat = 1;

    setTitle("eSTAR upgrades development geometry");
    geom.fill();
  };


  geom.select = "devT"; {
    // ================================================ The 4th version of the CAVE
    geom.caveFlag = "CAVE04";  geom.caveStat = 1;
    // ================================================ Disable old STAR inner detectors
    geom.sconFlag = "SCONof";  geom.sconStat = 0;
    geom.ftroFlag = "FTROof";  geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof";  geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof";  geom.svttStat = 0;
    geom.phmdFlag = "PHMDof";  geom.phmdStat = 0;
    // ================================================ Tracking, calorimetry, etc..
    geom.tpcxFlag = "TPCX10";  geom.tpceStat = 0; geom.tpcxStat = 1;
    geom.btofFlag = "BTOF67";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.vpddFlag = "VPDD07";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD04";  geom.mutdStat = 1;
    geom.pipeFlag = "PIPE12";  geom.pipeStat = 1;
    setTitle("First cut");
    // ================================================ The new Inner Detectors
    geom.idsmFlag = "IDSM01";  geom.idsmStat = 1;
    geom.fgtdFlag = "FGTD31";  geom.fgtdStat = 1;
    // ================================================ 
    geom.fpdmFlag = "FPDM13";  geom.fpdmStat = 1;        // open FPDM
    geom.fsceFlag = "FSCEv0";  geom.fsceStat = 1;
    geom.eiddFlag = "EIDDv0";  geom.eiddStat = 1;

    setTitle("eSTAR upgrades development geometry");
    geom.fill();
  };


  geom.select="eStar2"; {
    // ================================================ The 4th version of the CAVE
    geom.caveFlag = "CAVE04";  geom.caveStat = 1;
    // ================================================ Disable old STAR inner detectors
    geom.sconFlag = "SCONof";  geom.sconStat = 0;
    geom.ftroFlag = "FTROof";  geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof";  geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof";  geom.svttStat = 0;
    geom.phmdFlag = "PHMDof";  geom.phmdStat = 0;
    // ================================================ Tracking, calorimetry, etc..
    geom.tpcxFlag = "TPCX16";  geom.tpceStat = 0; geom.tpcxStat = 1;
    geom.btofFlag = "BTOF67";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.vpddFlag = "VPDD07";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD13";  geom.mutdStat = 1;
    geom.pipeFlag = "PIPE12";  geom.pipeStat = 1;

    // ================================================ The new Inner Detectors
    geom.idsmFlag = "IDSM01";   geom.idsmStat = 1;
    geom.fgtdFlag = "FGTDv55";  geom.fgtdStat = 1;
    // ================================================ 
    geom.fpdmFlag = "FPDM13";  geom.fpdmStat = 1;        // open FPDM
    geom.fsceFlag = "FSCEv0";  geom.fsceStat = 1;
    geom.eiddFlag = "EIDDv0";  geom.eiddStat = 1;
    // ================================================ 

    setTitle("eSTAR upgrades development geometry");
    geom.fill();

  };


}

//////////////////////////////////////////////////////////////////////////////////////////////
void test()
{
  geom.select="test"; { /* just the cave */

    geom.tpcRefSys = false; // Create TPC reference system

    geom.caveFlag = "CAVE05";  geom.caveStat = 1;

    geom.sconFlag = "SCONof";  geom.sconStat = 0;
    geom.ftroFlag = "FTROof";  geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof";  geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof";  geom.svttStat = 0;
    geom.magpFlag = "MAGPof";  geom.magpStat = 0;
    geom.bbcmFlag = "BBCMof";  geom.bbcmStat = 0;
    // ================================================ Tracking, calorimetry, etc..
    geom.tpceFlag = "TPCEof";  geom.tpceStat = 0;
    geom.btofFlag = "BTOFof";  geom.btofStat = 0;
    geom.calbFlag = "CALBof";  geom.calbStat = 0; geom.calbCuts = 1;
    geom.ecalFlag = "ECALof";  geom.ecalStat = 0; geom.ecalCuts = 1;
    geom.fpdmFlag = "FPDMof";  geom.fpdmStat = 0;
    geom.vpddFlag = "VPDDof";  geom.vpddStat = 0;
    geom.mutdFlag = "MUTDof";  geom.mutdStat = 0;
    // ================================================
    geom.idsmFlag = "IDSMof";  geom.idsmStat = 0;
    geom.fgtdFlag = "FGTDof";  geom.fgtdStat = 0;
    geom.pipeFlag = "PIPEof";  geom.pipeStat = 0;
    geom.upstFlag = "UPSTof";  geom.upstStat = 0;
    geom.zcalFlag = "ZCALof";  geom.zcalStat = 0;
    geom.phmdFlag = "PHMDof";  geom.phmdStat = 0;
    // ================================================
    geom.pixlFlag = "PIXLof";  geom.pixlStat = 0;
    geom.pxstFlag = "PXSTof";  geom.pxstStat = 0;
    geom.psupFlag = "PSUPof";  geom.psupStat = 0; // pixel support off

    geom.dtubFlag = "DTUBof";  geom.dtubStat = 0;
    // ================================================
    geom.sisdFlag = "SISDof"; geom.sisdStat = 0;
    geom.svttFlag = "SISDof"; geom.svttStat = 0;
    geom.istdFlag = "ISTDof"; geom.istdStat = 0; // ISTD off by default


    setTitle("== test geometry ==");
    // ================================================
    geom.fill();
  }



  geom.select="hctest"; { // HCAL test setup
    geom.hcalFlag = "HCALvF"; geom.hcalStat = 1;
    setTitle("HCAL test setup");
    geom.fill();
  };

  // switch back off
  geom.hcalFlag = "HCALof"; geom.hcalStat = 0;

}




















void y2013()
{

  //std::cout << " y2013 "<< std::flush;;

  geom.select = "dev13"; {
    // ================================================ 
    geom.caveFlag = "CAVE04";  geom.caveStat = 1;
    // ================================================ 
    geom.sconFlag = "SCONof";  geom.sconStat = 0;
    geom.ftroFlag = "FTROof";  geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof";  geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof";  geom.svttStat = 0;
    geom.phmdFlag = "PHMDof"; geom.phmdStat = 0;
    // ================================================ 
    geom.tpceFlag = "TPCE04r"; geom.tpceStat = 1;
    geom.btofFlag = "BTOF67";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.fpdmFlag = "FPDM03";  geom.fpdmStat = 1;
    geom.vpddFlag = "VPDD07";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD13";  geom.mutdStat = 1; 
    geom.pipeFlag = "PIPEv1";  geom.pipeStat = 1;
    // ================================================ 
    geom.idsmFlag = "IDSM02";  geom.idsmStat = 1;
    geom.fgtdFlag = "FGTD32";  geom.fgtdStat = 1;
    geom.pxstFlag = "PXST01";  geom.pxstStat = 1;
    geom.psupFlag = "PSUP01";  geom.psupStat = 1;
    geom.pixlFlag = "PIXL02";  geom.pixlStat = 1; 
    
    // ================================================ 
    //
    setTitle("Upgrade studies with 6 complete FGT disks");
    //
    // ================================================ 
    geom.fill();
  };




  geom.select = "y2013"; {
    // ================================================ 
    geom.caveFlag = "CAVE04";  geom.caveStat = 1;
    geom.magpFlag = "MAGPv1"; geom.magpStat = 1;
    // ================================================ 
    geom.sconFlag = "SCONof";  geom.sconStat = 0;
    geom.ftroFlag = "FTROof";  geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof";  geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof";  geom.svttStat = 0;
    geom.phmdFlag = "PHMDof"; geom.phmdStat = 0;
    // ================================================ 
    geom.tpceFlag = "TPCE04r";  geom.tpceStat = 1;    
    geom.btofFlag = "BTOFv8";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.fpdmFlag = "FPDM03";  geom.fpdmStat = 1;
    geom.vpddFlag = "VPDD07";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD13";  geom.mutdStat = 1;
    geom.pipeFlag = "PIPEv2";  geom.pipeStat = 1;
    // ================================================ 
    geom.idsmFlag = "IDSM02";  geom.idsmStat = 1;
    geom.fgtdFlag = "FGTD32";  geom.fgtdStat = 1;
    geom.pxstFlag = "PXST01";  geom.pxstStat = 1;
    geom.pixlFlag = "PIXL05";  geom.pixlStat = 1; 
    geom.dtubFlag = "DTUB01";  geom.dtubStat = 1;
    geom.psupFlag = "PSUPof";  geom.psupStat = 0;
    // ================================================ 
    //
    setTitle("Y2013 baseline");
    //
    // ================================================ 
    geom.fill();
  };

  /// Y2013 Baseline Configuration ///////////////////////////////
  geom.select = "y2013_1"; {
    setTitle("Y2013 baseline");
    geom.tpcRefSys = true; // Create TPC reference system
    geom.fill();
  }
  geom.select = "y2013_2"; {
    setTitle("Y2013 baseline sans PIXL");
    // geom.pxstStat = 1; // support tube always on
    geom.tpcRefSys = true; // Create TPC reference system
    geom.pixlStat = 0;
    geom.dtubStat = 0;
    geom.fill();
  }
  geom.Use("select","y2013"); // Back to baseline


  // Setup y2013a
  geom.select = "y2013a"; {
    setTitle("Y2013a first production geometry PIXL in");
    geom.tpcRefSys = true; // set reference system for TPC
    geom.caveFlag = "CAVE05";
    geom.pipeFlag = "PIPEv3";
    geom.fill();
  };
  geom.select = "y2013_1a"; {
    setTitle("Y2013a first production geometry PIXL in");
    geom.tpcRefSys = true; // set reference system for TPC
    geom.fill();
  };
  geom.select = "y2013_2a"; {
    setTitle("Y2013a first production geometry PIXL out");
    geom.tpcRefSys = true; // set reference system for TPC
    geom.pixlStat = 0;
    geom.dtubStat = 0;
    geom.fill();
  };

  // Setup y2013b
  geom.Use( "select","y2013a");
  geom.select = "y2013b"; {
    setTitle("Y2013a first production geometry PIXL in");
    geom.tpcRefSys = true; // set reference system for TPC
    geom.caveFlag = "CAVE05";
    geom.pipeFlag = "PIPEv3";
    geom.psupFlag = "PSUP01"; geom.psupStat = 1;
    geom.pixlFlag = "PIXL05"; geom.pixlStat = 1; 
    geom.pxstFlag = "PXST01"; geom.pxstStat = 1; 
    geom.dtubFlag = "DTUB01"; geom.dtubStat = 1;
    geom.fill();
  };
  geom.select = "y2013_1b"; {
    setTitle("Y2013a first production geometry PIXL in");
    geom.tpcRefSys = true; // set reference system for TPC
    geom.fill();
  };
  geom.select = "y2013_2b"; {
    setTitle("Y2013a first production geometry PIXL out");
    geom.tpcRefSys = true; // set reference system for TPC
    geom.pixlStat = 0;
    geom.dtubStat = 0;
    geom.psupStat = 0;     // switch psup off
    geom.fill();
  };
  

  //
  // y2013_1c and 2c are defined to match the configuration used in
  // the y2013 production.  The production was mistakenly launched 
  // using the asymptotic geometry as defined in the SL14a library.
  // That geometry uses the new model of the cave and the version3.1
  // of the TPC.  Version3.1 of the TPC changes the 
  //
  geom.Use("select","y2013_1b"); // Inherit from 1b
  geom.select = "y2013_1c"; {
    geom.caveFlag = "CAVE05";
    geom.tpceFlag = "TPCE31";
    geom.tpcRefSys = true;
    setTitle("Y2013_1c production geometry = y2013_1x in SL14a");
    geom.fill();
    geom.select("y2013c"); 
    geom.fill();
  };

  geom.select = "y2013_2c"; {
    geom.caveFlag = "CAVE05";
    geom.tpceFlag = "TPCE31";
    geom.tpcRefSys = true;
    setTitle("Y2013_2c production geometry = y2013_2x in SL14a");
    geom.pixlStat = 0;
    geom.dtubStat = 0;
    geom.psupStat = 0;     // switch psup off
    geom.fill();
  }
  geom.Use("select","y2013_1c"); // restore pixel detector
    
    



  /// Y2013 Asymptotic Configuration /////////////////////////////
  geom.select = "y2013_1x"; {
    // Inherits y2013 and
    geom.caveFlag = "CAVE05";
    geom.tpceFlag = "TPCE40"; geom.tpceStat = 1;
    geom.tpcRefSys = true; // Create TPC reference system
    setTitle("Asymptotic y2013");
    geom.fill();
  }
  geom.select="y2013x"; geom.fill();

  geom.select = "y2013_2x"; {
    setTitle("Asymptotic y2013 sans PIXL");
    geom.caveFlag = "CAVE05";
    geom.tpcRefSys = true; // Create TPC reference system
    // geom.pxstStat = 1; // support tube always on
    geom.pixlStat = 0;
    geom.dtubStat = 0;
    geom.fill();
  }
  geom.Use("select","y2013_1x");  // Restore asymptotic values  


}

void y2014()
{

  geom.select = "y2014"; {
    geom.tpcRefSys = true; // set reference system for TPC
    // ================================================
    geom.caveFlag = "CAVE05";  geom.caveStat = 1;
    // ================================================
    geom.sconFlag = "SCONof";  geom.sconStat = 0;
    geom.ftroFlag = "FTROof";  geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof";  geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof";  geom.svttStat = 0;
    geom.phmdFlag = "PHMDof";  geom.phmdStat = 0;
    geom.fpdmFlag = "FPDMof";  geom.fpdmStat = 0;
    geom.fgtdFlag = "FGTDof";  geom.fgtdStat = 0;
    // ================================================
    geom.tpceFlag = "TPCE04r"; geom.tpceStat = 1;
    geom.btofFlag = "BTOFv8";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.vpddFlag = "VPDD07";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD14";  geom.mutdStat = 1;
    geom.pipeFlag = "PIPEv3";  geom.pipeStat = 1;
    geom.sisdFlag = "SISD85";  geom.sisdStat = 1;
    // ================================================
    geom.pixlFlag = "PIXL06";  geom.pixlStat = 1;
    geom.istdFlag = "ISTD02";  geom.istdStat = 1;
    // ================================================
    geom.pxstFlag = "PXST01";  geom.pxstStat = 1;
    geom.dtubFlag = "DTUB01";  geom.dtubStat = 1;
    geom.psupFlag = "PSUP01";  geom.psupStat = 1;
    geom.dtubFlag = "DTUB01";  geom.dtubStat = 1;
    geom.idsmFlag = "IDSM14";  geom.idsmStat = 1;
    // ================================================
    //
    setTitle("Y2014 first cut geometry");
    //
    // ================================================
    geom.fill();
  };


  geom.select="y2014a";
  {

    geom.tpcRefSys = true; // set reference system for TPC
    // ================================================
    geom.caveFlag = "CAVE05";  geom.caveStat = 1;
    geom.magpFlag = "MAGPv1"; geom.magpStat = 1; // set this explicitly
    // ================================================
    geom.sconFlag = "SCONof";  geom.sconStat = 0;
    geom.ftroFlag = "FTROof";  geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof";  geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof";  geom.svttStat = 0;
    geom.phmdFlag = "PHMDof";  geom.phmdStat = 0;
    geom.fpdmFlag = "FPDMof";  geom.fpdmStat = 0;
    geom.fgtdFlag = "FGTDof";  geom.fgtdStat = 0;
    // ================================================
    geom.tpceFlag = "TPCE31";  geom.tpceStat=1;
    geom.btofFlag = "BTOFv8";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.vpddFlag = "VPDD07";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD14";  geom.mutdStat = 1;
    geom.pipeFlag = "PIPEv3";  geom.pipeStat = 1;
    geom.sisdFlag = "SISD85";  geom.sisdStat = 1;
    // ================================================
    geom.pixlFlag = "PIXL06";  geom.pixlStat = 1;
    geom.istdFlag = "ISTD02";  geom.istdStat = 1;
    // ================================================
    geom.pxstFlag = "PXST01";  geom.pxstStat = 1;
    geom.dtubFlag = "DTUB01";  geom.dtubStat = 1;
    geom.psupFlag = "PSUP01";  geom.psupStat = 1;
    geom.dtubFlag = "DTUB01";  geom.dtubStat = 1;
    geom.idsmFlag = "IDSM14";  geom.idsmStat = 1;
    // ================================================

    geom.fill();
  }

  
  geom.Use("select","y2014a");  
  geom.select="y2014b"; 
  {
    geom.hcalFlag = "HCALv0"; geom.hcalStat = 1; // HCAL prototype
    geom.fill();
  }


}





void y2015()
{
  geom.select = "y2015"; /* y2015 first cut */ {
    //
    geom.tpcRefSys = true; // set reference system for TPC
    //
    // ================================================
    geom.caveFlag = "CAVE06";  geom.caveStat = 1;
    geom.magpFlag = "MAGPv1"; geom.magpStat = 1;
    // ================================================
    geom.sconFlag = "SCONof";  geom.sconStat = 0;
    geom.ftroFlag = "FTROof";  geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof";  geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof";  geom.svttStat = 0;
    geom.phmdFlag = "PHMDof";  geom.phmdStat = 0;
    geom.fpdmFlag = "FPDMof";  geom.fpdmStat = 0;
    geom.fgtdFlag = "FGTDof";  geom.fgtdStat = 0;
    geom.hcalFlag = "HCALof";  geom.hcalStat = 0;    
    // ================================================
    geom.magpFlag = "MAGPv1";  geom.magpStat = 1;
    geom.tpceFlag = "TPCE31";  geom.tpceStat = 1;
    geom.btofFlag = "BTOFv8";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.vpddFlag = "VPDD08";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD14";  geom.mutdStat = 1;
    geom.pipeFlag = "PIPEv3";  geom.pipeStat = 1;
    geom.sisdFlag = "SISD85";  geom.sisdStat = 1;
    // ================================================
    geom.pixlFlag = "PIXL06";  geom.pixlStat = 1;
    geom.istdFlag = "ISTD02";  geom.istdStat = 1;
    // ================================================
    geom.pxstFlag = "PXST01";  geom.pxstStat = 1;
    geom.dtubFlag = "DTUB01";  geom.dtubStat = 1;
    geom.psupFlag = "PSUP01";  geom.psupStat = 1;
    geom.dtubFlag = "DTUB01";  geom.dtubStat = 1;
    geom.idsmFlag = "IDSM14";  geom.idsmStat = 1;
    // ================================================
    geom.fpdmFlag = "FPDM04";  geom.fpdmStat = 1;  
    // ================================================
    geom.fill();
  }    

  geom.select = "y2015a"; /* y2015a production reference */ {
    //
    geom.tpcRefSys = true; // set reference system for TPC
    geom.pixlFlag = "PIXL62";  geom.pixlStat = 1;
    //
    geom.fill();
  }    

 
}
void y2016()
{ 
  geom.select = "dev2016"; /* y2016 development */ 
  {
    // ================================================
    geom.caveFlag = "CAVE06";  geom.caveStat = 1;
    geom.magpFlag = "MAGPv1"; geom.magpStat = 1;
    // ================================================
    geom.sconFlag = "SCONof";  geom.sconStat = 0;
    geom.ftroFlag = "FTROof";  geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof";  geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof";  geom.svttStat = 0;
    geom.phmdFlag = "PHMDof";  geom.phmdStat = 0;
    geom.fpdmFlag = "FPDMof";  geom.fpdmStat = 0;
    geom.fgtdFlag = "FGTDv56";  geom.fgtdStat = 1; // Forward FGT
    geom.hcalFlag = "HCALv1";  geom.hcalStat = 1; // HCAL
    // ================================================
    geom.magpFlag = "MAGPv1";  geom.magpStat = 1;
    geom.tpceFlag = "TPCE31";  geom.tpceStat = 1;
    geom.btofFlag = "BTOFv8";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.vpddFlag = "VPDD08";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD14";  geom.mutdStat = 1;
    geom.pipeFlag = "PIPEv3";  geom.pipeStat = 1;
    geom.sisdFlag = "SISD85";  geom.sisdStat = 1;
    // ================================================
    geom.pixlFlag = "PIXL06";  geom.pixlStat = 1;
    geom.istdFlag = "ISTD02";  geom.istdStat = 1;
    // ================================================
    geom.pxstFlag = "PXST01";  geom.pxstStat = 1;
    geom.dtubFlag = "DTUB01";  geom.dtubStat = 1;
    geom.psupFlag = "PSUP01";  geom.psupStat = 1;
    geom.dtubFlag = "DTUB01";  geom.dtubStat = 1;
    geom.idsmFlag = "IDSM14";  geom.idsmStat = 1;
    // ================================================   
    geom.fill();
  }

  // switch off experimental 
  geom.hcalFlag = "HCALof";  geom.hcalStat = 0;    

}





// -----------------------------------------------------------------------------------------------------------------

void dev15()
{

  // DEV 15 baseline is y2014a
  geom.select = "dev15"; {

    geom.tpcRefSys = true; // set reference system for TPC
    // ================================================
    geom.caveFlag = "CAVE05";  geom.caveStat = 1;
    // ================================================
    geom.sconFlag = "SCONof";  geom.sconStat = 0;
    geom.ftroFlag = "FTROof";  geom.ftroStat = 0;
    geom.ftpcFlag = "FTPCof";  geom.ftpcStat = 0;
    geom.svttFlag = "SVTTof";  geom.svttStat = 0;
    geom.phmdFlag = "PHMDof";  geom.phmdStat = 0;
    geom.fpdmFlag = "FPDMof";  geom.fpdmStat = 0;
    geom.fgtdFlag = "FGTDof";  geom.fgtdStat = 0;
    geom.hcalFlag = "HCALof";  geom.hcalStat = 0;    
    // ================================================
    geom.magpFlag = "MAGPv1";  geom.magpStat = 1;
    geom.tpceFlag = "TPCE31";  geom.tpceStat = 1;
    geom.btofFlag = "BTOFv8";  geom.btofStat = 1;
    geom.calbFlag = "CALB02";  geom.calbStat = 1; geom.calbCuts = 1;
    geom.ecalFlag = "ECALv6";  geom.ecalStat = 1; geom.ecalCuts = 1;
    geom.bbcmFlag = "BBCMon";  geom.bbcmStat = 1;
    geom.vpddFlag = "VPDD07";  geom.vpddStat = 1;
    geom.mutdFlag = "MUTD14";  geom.mutdStat = 1;
    geom.pipeFlag = "PIPEv3";  geom.pipeStat = 1;
    geom.sisdFlag = "SISD85";  geom.sisdStat = 1;
    // ================================================
    geom.pixlFlag = "PIXL06";  geom.pixlStat = 1;
    geom.istdFlag = "ISTD02";  geom.istdStat = 1;
    // ================================================
    geom.pxstFlag = "PXST01";  geom.pxstStat = 1;
    geom.dtubFlag = "DTUB01";  geom.dtubStat = 1;
    geom.psupFlag = "PSUP01";  geom.psupStat = 1;
    geom.dtubFlag = "DTUB01";  geom.dtubStat = 1;
    geom.idsmFlag = "IDSM14";  geom.idsmStat = 1;
    // ================================================

    geom.fill();
  }

  // Define dev15a: baseline plus FMS++ (fms w/ preshower)
  geom.Use("select","dev15");
  geom.select="dev15a"; {    
    geom.fpdmFlag = "FPDM04";   geom.fpdmStat = 1;  
    geom.fill();
  };

  // Define dev15b: baseline plus HCAL
  geom.Use("select","dev15");
  geom.select="dev15b"; {
    geom.hcalFlag="HCALv1"; geom.hcalStat = 1;
    geom.fill();
  };

  // switch off experimental 
  geom.hcalFlag = "HCALof";  geom.hcalStat = 0;    

}

void devel()
{

  // DEV 15 baseline is y2015
  geom.Use("select","y2015a");
  geom.select = "dev2020"; {
    // ================================================
    geom.ftsdFlag = "FTSDv0"; geom.ftsdStat = 1;
    geom.ecalStat = 0;
    // ================================================
    geom.fill();
  }


}



void upgrade()
{
  // We won't actually support these with AgML, but we will define them so that cons will be able to export
  const Char_t *upgrs[]={
    "upgr01", "upgr02", "upgr03", "upgr04", "upgr05", "upgr06", "upgr07", "upgr08", "upgr09", "upgr10",
    "upgr11", "upgr12", "upgr13", "upgr14", "upgr15", "upgr16", "upgr16a", "upgr17", "upgr21", "upgr22", 
    "upgr23" };

  for ( UInt_t ii=0;ii<sizeof(upgrs)/sizeof(Char_t *);ii++ )
    {
      geom.select = upgrs[ii];
      geom.pixlStat = 0;
      geom.dtubStat = 0;
      geom.magpStat = 0;
      geom.tpceStat = 0;
      geom.tpcxStat = 0;
      geom.ecalStat = 0;
      geom.pipeStat = 0;
      geom.calbStat = 0;
      geom.btofStat = 0;
      geom.vpddStat = 0;
      geom.bbcmStat = 0;
      geom.idsmStat = 0;
      geom.fgtdStat = 0;
      geom.fill();
    }
  
}
