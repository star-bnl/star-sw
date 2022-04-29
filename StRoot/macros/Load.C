// $Id: Load.C,v 1.23 2016/12/01 15:10:44 jeromel Exp $
//=======================================================================
// owner: Yuri Fisyak
// what it does: 
//=======================================================================

void Load()
{
const char * const loadList[]={
 "libCore"
,"libPhysics"
,"libTree"
,"libGeom"
,"St_base"
,"St_Tables"
,"StarRoot"
,"StChain"
,"StDetectorDbMaker"
,"StBichsel"
,"StarClassLibrary"
,"StEvent"
,"StEventUtilities"
,"StUtilities"
,"StTpcDb"
,"StDbLib"
,"StDbBroker"
,"St_db_Maker"
,"StTriggerDataMaker"
,"StStrangeMuDstMaker"
,"StEmcUtil"
,"StMuDSTMaker"
,"StBFChain"
,0};

  for (int i=0;loadList[i];i++) {
    TString ts(loadList[i]);
    printf("  Loading %s\n",ts.Data());
    int ians = gSystem->Load(ts.Data());
    if (ians<0) printf("**Failed %s = %d\n",ts.Data(),ians);
    ians = gSystem->Load(ts.Data());
    if (ians<0) printf("**Failed %s = %d\n",ts.Data(),ians);
    ians = gSystem->Load(ts.Data());
    if (ians<0) printf("**Failed %s = %d\n",ts.Data(),ians);
  }
}
