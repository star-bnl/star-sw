// $Id: FtpcDriftMapMaker.C,v 1.9 2003/09/30 08:13:14 jcs Exp $
// $Log: FtpcDriftMapMaker.C,v $
// Revision 1.9  2003/09/30 08:13:14  jcs
// StarClassLibrary must be loaded before StDbUtilities (needs StThreeVectorF)
//
// Revision 1.8  2003/04/30 20:40:11  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.7  2001/10/29 13:02:19  jcs
// select FTPC drift maps according to flavor of magnetic field
//
// Revision 1.4  2001/05/16 18:34:18  jcs
// select timestamp
//
// Revision 1.3  2001/03/07 15:16:49  jcs
// use MySQL database
//
// Revision 1.2  2001/01/19 15:00:07  jcs
// replace the default option ("Normal") with "FullField"
//
// Revision 1.1  2001/01/02 13:02:06  jcs
// macro to run standalone maker StFtpcDriftMapMaker
//
//======================================================================
// owner:  Janet Seyboth  (jcs@mppmu.mpg.de)
// what it does: compute drift map for FTPCs
//
//    const Int_t      map     = 2           use mapped field values
//                             = 1           use constant field values
//    const Float_t   |factor| > 0.8         scale from full field
//                    factor   > 0           normal field
//                    factor   < 0           reversed field
//                                           factor is the field scaling factor
//
//    Default:  const Int_t   map    = 2
//              const Float_t factor = 1.0
//   
//======================================================================

void FtpcDriftMapMaker(const Int_t map=2, const Float_t factor=1.0)
{
    if (gClassTable->GetID("TTable") < 0) gSystem->Load("libTable");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
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
    
    StFtpcDriftMapMaker *ftpcDriftMapMk = new StFtpcDriftMapMaker(map,factor);
}
