// $Id: FtpcDriftMapMaker.C,v 1.5 2009/11/10 11:07:09 jcs Exp $
// $Log: FtpcDriftMapMaker.C,v $
// Revision 1.5  2009/11/10 11:07:09  jcs
// Now necessary to also load StDetectorDbMaker library
//
// Revision 1.4  2009/11/10 10:59:26  jcs
// change map to Map to avoid conflict with cint
//
// Revision 1.3  2006/08/02 13:59:16  jcs
// add deltaAr argument to allow user to change gas compostion (default: deltaAr=0)
//
// Revision 1.2  2005/12/12 09:30:52  jcs
// load StarMagField library
//
// Revision 1.1  2005/12/12 09:09:11  jcs
// Macro used to run StFtpcDriftMapMakerZ
//
//
//======================================================================
// owner:  Janet Seyboth  (jcs@mppmu.mpg.de)
// what it does: compute drift map for FTPCs
//
//    const Int_t      Map     = 2           use mapped field values
//                             = 1           use constant field values
//    const Float_t   |factor| > 0.8         scale from full field
//                    factor   > 0           normal field
//                    factor   < 0           reversed field
//                                           factor is the field scaling factor
//    const Float_t   deltaAr                change gas composition: 
//                                                   % Ar + deltaAr
//                                                   % CO2 - deltaAr
//
//    Default:  const Int_t   Map    = 2
//              const Float_t factor = 1.0
//              const Float_t deltaAr = 0.0
//   
//======================================================================

void FtpcDriftMapMaker(const Int_t Map=2, const Float_t factor=1.0, const Float_t deltaAr = 0.0)
{
    cout<<"FtpcDriftMapMaker.C called with:"<<endl;
    cout<<"                                  Map     = "<<Map<<endl;
    cout<<"                                  factor  = "<<factor<<endl;
    cout<<"                                  deltaAr = "<<deltaAr<<endl;
    if (gClassTable->GetID("TTable") < 0) gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StDetectorDbMaker");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StarMagField");
    gSystem->Load("StMagF");
    gSystem->Load("libftpc_Tables");
    gSystem->Load("StFtpcDriftMapMaker");
    gSystem->Load("StFtpcClusterMaker");

    //  Create the makers to be called by the current chain
    const char *mysqlDB =  "MySQL:StarDb";
    const char *paramsDB = "$STAR/StarDb";
    //const char *paramsDB = "$PWD/StarDb";
    StChain *chain =  new StChain();
    
    St_db_Maker *dbMk = new St_db_Maker("db",mysqlDB,paramsDB);
    dbMk->SetDateTime(20010501,00000);

  // Full Field Positive ?
  if ( factor > 0.8 ) {
     dbMk->SetFlavor("ffp10kv","ftpcVDrift");
     dbMk->SetFlavor("ffp10kv","ftpcdVDriftdP");
     dbMk->SetFlavor("ffp10kv","ftpcDeflection");
     dbMk->SetFlavor("ffp10kv","ftpcdDeflectiondP");
     gMessMgr->Info() << "StFtpcDriftMapMaker::InitRun: flavor set to ffp10kv"<<endm;
  }
  else if ( factor > 0.2 ) {
     dbMk->SetFlavor("hfp10kv","ftpcVDrift");
     dbMk->SetFlavor("hfp10kv","ftpcdVDriftdP");
     dbMk->SetFlavor("hfp10kv","ftpcDeflection");
     dbMk->SetFlavor("hfp10kv","ftpcdDeflectiondP");
     gMessMgr->Info() << "StFtpcDriftMapMaker::InitRun: flavor set to hfp10kv"<<endm;
  }
  else if ( factor > -0.2 ) {
     dbMk->SetFlavor("zf10kv","ftpcVDrift");
     dbMk->SetFlavor("zf10kv","ftpcdVDriftdP");
     dbMk->SetFlavor("zf10kv","ftpcDeflection");
     dbMk->SetFlavor("zf10kv","ftpcdDeflectiondP");
     gMessMgr->Info() << "StFtpcDriftMapMaker::InitRun: flavor set to zf10kv"<<endm;
  }
  else if ( factor > -0.8 ) {
     dbMk->SetFlavor("hfn10kv","ftpcVDrift");
     dbMk->SetFlavor("hfn10kv","ftpcdVDriftdP");
     dbMk->SetFlavor("hfn10kv","ftpcDeflection");
     dbMk->SetFlavor("hfn10kv","ftpcdDeflectiondP");
     gMessMgr->Info() << "StFtpcDriftMapMaker::InitRun: flavor set to hfn10kv"<<endm;
  }
  else {
     dbMk->SetFlavor("ffn10kv","ftpcVDrift");
     dbMk->SetFlavor("ffn10kv","ftpcdVDriftdP");
     dbMk->SetFlavor("ffn10kv","ftpcDeflection");
     dbMk->SetFlavor("ffn10kv","ftpcdDeflectiondP");
     gMessMgr->Info() << "StFtpcDriftMapMaker::InitRun: flavor set to ffn10kv"<<endm;
  }
    dbMk->Init();
    dbMk->Make();
    
    //b=new TBrowser();
    
    StFtpcDriftMapMaker *ftpcDriftMapMk = new StFtpcDriftMapMaker(Map,factor,deltaAr);
}
