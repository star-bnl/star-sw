// $Id: FtpcDriftMapMaker.C,v 1.6 2001/07/20 09:29:18 jcs Exp $
// $Log: FtpcDriftMapMaker.C,v $
// Revision 1.6  2001/07/20 09:29:18  jcs
// change timestamp for Y2001 real data
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
    if (gClassTable->GetID("TTable") < 0) gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StMagF");
    gSystem->Load("libftpc_Tables");
    gSystem->Load("StFtpcDriftMapMaker");
    gSystem->Load("StFtpcClusterMaker");

  //  Create the makers to be called by the current chain
  const char *mysqlDB =  "MySQL:StarDb";
  const char *paramsDB = "$STAR/StarDb";
  StChain *chain =  new StChain();

  St_db_Maker *dbMk = new St_db_Maker("db",mysqlDB,paramsDB);
  dbMk->SetDateTime(20010701,10000);
//  dbMk->SetDateTime("year_2b");
  dbMk->Init();
  dbMk->Make();

  StFtpcDriftMapMaker *ftpcDriftMapMk = new StFtpcDriftMapMaker(map,factor);
}
