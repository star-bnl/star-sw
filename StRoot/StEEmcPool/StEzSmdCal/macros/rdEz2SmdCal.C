//#include "StRoot/StEEmcUtil/EEfeeRaw/EEname2Index.h"
//.L StRoot/StEEmcUtil/EEfeeRaw/EEname2Index.cxx+
class EEmcDbItem;

rdEz2SmdCal(int mxEve=1000000,
       char *runL="R5107005_1 R5107005 ",
       TString iPath="/star/data04/sim/balewski/daq/ezTree/pp200/pp2/",
       int firstSec=5,
       int lastSec=5,
       int oflTrigId=0
      ) {
  //  iPath="/tmp/balewski/";  
  // runL="R5107005";

  char *libL[]={
    "StRoot/StDbLib/StDbLib.so",  
    "StRoot/StEEmcDbMaker/libEEmcDbMaker.so", 
    "StRoot/StEEmcUtil/EEfeeRaw/libEEfeeRaw.so",
    "StRoot/StEEmcUtil/EEmcGeom/libEEmcGeom.so",
    "StRoot/StEEmcUtil/StEEmcSmd/libEEmcSmdGeom.so",
    "StRoot/StEEmcPool/StEzSmdCal/libEzSmdCal.so",
    "EEmcDb/libEEmcDb.so",
    "libPhysics"
 };
  
  int i;
  for(i=0;i<sizeof(libL)/sizeof(char*);i++) {
    printf("   load '%s' ...\n",libL[i]);
    assert( !gSystem->Load(libL[i])); 
  }
  printf("loaded %d libraries\n",i);

  // .........   input  event file   .........

  TChain *chain = new TChain("ezstar");

  char *run=strtok(runL," "); // init 'strtok'
  int i=0;
  do {
    printf("add run %d '%s' \n",i++,run);
    TString fullName=iPath+run+".ez.root";  
    chain->Add(fullName);
  } while(run=strtok(0," "));  // advance by one nam
  
  int nEntries = (Int_t)chain->GetEntries();
  printf("Sort %d  of total Events %d\n",mxEve, nEntries);
  int nEve=0;


  Int_t nEntries = (Int_t)chain->GetEntries();
  if(nEntries<=0) {
    printf("\n\nSth is wrong, chain is empty nEntries=%d\n\n",nEntries);
    assert(nEntries>0);
  }

  EEfeeRawEvent  *eFee=0;
  EEmcEventHeader *eHead =0;
  EEstarTrig    *eTrig=0;
  chain->SetBranchAddress("eemc",&eFee);
  chain->SetBranchAddress("head",&eHead);
  chain->SetBranchAddress("trig",&eTrig);
 
  chain->GetEntry(0);// read first event

  long timeStamp=eHead->getTimeStamp();

  printf("run   timeStamp=%d=%s\n" ,timeStamp,ctime((const time_t *)&timeStamp));

 //............  DB-reader .................
  EEmcDb *db=new EEmcDb() ;
  db->setThreshold(3.0);
  db->setPreferredFlavor("expoSlope1","eemcPIXcal");

  // set all DB flags before DB request
  db->requestDataBase(timeStamp,firstSec,lastSec); // range of sectors
  
  // dump current DB content
  // db->exportAscii("dbDump1.dat"); return;
  
  TObjArray  HList;
  //........... sorters ..........
  EzEEsmdCal *sorter=new  EzEEsmdCal(5); // sectID
  sorter->set(&HList,db,eFee,eHead,eTrig);
  sorter->init();
   
  printf("Sort %d  of total Events %d, use trigId=%d\n",mxEve, nEntries,oflTrigId);

  int nEve=0,nAcc=0,nOK=0;
  int t1=time(0);  
  for(nEve=0; nEve<nEntries && nEve<mxEve; nEve++) {
    chain->GetEntry(nEve);
   if(nEve%2000==0)printf("in %d, acc %d  ok=%d\n",nEve,nAcc,nOK);
   if(oflTrigId>0 && ! eTrig->isTrigID(oflTrigId) ){ continue;}
    nAcc++;
    // verify data block consistency
    int nCr=eFee->maskWrongCrates(timeStamp,eHead->getToken(),EEfeeRawEvent::headVer1);
    if(nCr==22) nOK++;
    sorter->make();
  }
  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*nEve/(t2-t1);
  printf("sorting done, nEve=%d, CPU event rate=%.1f Hz, total time %.1f minute(s) \n",nEve,rate,tMnt);

  sorter->finish();

  // save output histograms
  sorter->saveHisto("aaa7P+11+5d");
  // HList.ls(); 

  h=(TH1F *) HList.FindObject("myStat");   assert(h);
  float *my=h->GetArray();
  printf("%d -->UxV multi=%.1f  one=%.1f any=%.1f non1x1=%.1f\n",nEve,my[2],my[3],my[4],my[5]);

  printf(" -->MIP tw=%.1f cntr=%.1f \n",my[6],my[7]);

   return;
  gStyle->SetPalette(1,0);
  printf("drawing ...\n");
  c=new TCanvas();  c->Divide(1,2);
  c->cd(1);h=(TH1F*)HList.FindObject("case"); h->Draw("");
  c->cd(2);h2=(TH2F*)HList.FindObject("xy05c"); h2->Draw("colz");

 //gPad->SetLogy();
 } 

