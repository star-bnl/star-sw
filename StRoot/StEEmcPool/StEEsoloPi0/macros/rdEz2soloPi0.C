rdEz2soloPi0(  char *run="R5112017",  int mxEve=200000 , int typeG=0) {
  //    runL=" R5107005 R5107005_1  R5107008  R5109025  R5109026 R5109027   R5109028a  R5109028b  R5109028c  R5109029a  R5109030   R5109031  R5109034  R5109035";
 TString out="/auto/pdsfdv34/starspin/balewski/calib2004/outPi0B/";
 if(typeG==1)  out="/auto/pdsfdv34/starspin/balewski/calib2004/outPi0C/";

  int firstSec=5;
  int lastSec=8;
  char *libL[]={
    "StRoot/StDbLib/StDbLib.so",  
    "StRoot/StEEmcDbMaker/libEEmcDbMaker.so", 
    "StRoot/StEEmcUtil/EEfeeRaw/libEEfeeRaw.so",
    "StRoot/StEEmcUtil/EEmcGeom/libEEmcGeom.so",
    // "StRoot/StEEmcUtil/StEEmcSmd/libEEmcSmdGeom.so",
    // "StRoot/StEEmcUtil/EEmcSmdMap/libEEmcSmdMap.so",
    "StRoot/StEEmcPool/StEEsoloPi0/libEEsoloPi0.so",
    "EEmcDb/libEEmcDb.so",
    "libPhysics"
  };
  
  gStyle->SetPalette(1,0);
  int i;
  for(i=0;i<sizeof(libL)/sizeof(char*);i++) {
    printf("   load '%s' ...\n",libL[i]);
    assert( !gSystem->Load(libL[i])); 
  }
  printf("loaded %d libraries\n",i);

  // .........   input  event file   .........

  TChain *chain = new TChain("ezstar");


#if 0
  //............................
  // use simple list of runs from fixed input directory
  TString iPath="/auto/pdsfdv34/starspin/relyea/butterfly/"; //LBL
  //  iPath="/star/data04/sim/balewski/daq/ezTree/pp200/pp2/";// BNL
  char *run=strtok(runL," "); // init 'strtok'
  int i=0;
  do {
    printf("add run %d '%s' \n",i++,run);
    TString fullName=iPath+run+".ez.root";  
    chain->Add(fullName);
  } while(run=strtok(0," "));  // advance by one nam
#endif  


 #if 1
  //............................
  // read exact full path from the list
  TString runList="ezListLBL/"; runList+=run; 
  out+=run;
  printf("aa=%s=\n",runList.Data());

  FILE *fd=fopen(runList.Data(),"r"); assert(fd);
  int i=0;
  while(1) { 
    char text[500];
    int ret= fscanf(fd,"%s",text);
    if(ret<=0) break;
    i++;
    chain->Add(text);    
    printf("%d =%s=\n",i,text);
  }
#endif

  int nEntries = (Int_t)chain->GetEntries();
  int mSect=lastSec-firstSec+1;
  printf("Sort %d  of total Events %d, mSect=%d\n",mxEve, nEntries,mSect);
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
  db->setThreshold(5.0); 

  //  db->setPreferredFlavor("expoSlope1","eemcPIXcal");

  // set all DB flags before DB request
  db->requestDataBase(timeStamp,firstSec,lastSec); // range of sectors
  // db->changeGains("setG1.dat");
  //  db->changeMask("setM1.dat");

  // overwrit tower gains
  TString calDir3="../WWW-E/calibration/run4/smd+PQRT-calib-w-MIP/iter3/";
  int secID=0;
  for(secID=5;secID<=8*typeG;secID++){
    char tt1[100];
    sprintf(tt1,"%02d",secID);
    TString tt;
    tt=calDir3+"gains"+tt1+"tower.dat";   db->changeGains(tt.Data());
  }
  
  TObjArray  HList;
  //........... sorter(s) ..........

  EzEEsoloPi0 *task = new EzEEsoloPi0;
  task->set(&HList,db,eFee,eHead,eTrig);
  task->init();
  // dump current DB content
  //db->exportAscii("dbDump2.dat"); return;
     
  printf("Sort %d  of total Events %d\n",mxEve, nEntries);
  
  int nEve=0,nAcc=0,nOK=0;
  int t1=time(0);  
  for(nEve=0; nEve<nEntries && nEve<mxEve; nEve++) {
    chain->GetEntry(nEve);
    // printf("\n================= %d ===\n",nEve);
    if(nEve%3000==0)printf("in %d, acc %d  ok=%d\n",nEve,nAcc,nOK);
   //   printf("%d %d %d\n",  eTrig->isTrigID(10), eTrig->isTrigID(45010), eTrig->isTrigID(45020));

   if(! eTrig->isTrigID(10) &&
      ! eTrig->isTrigID(45010) &&
      ! eTrig->isTrigID(45020) ) continue;
   // this is for sure minB events
    nAcc++;
    // verify data block consistency
    eFee->maskWrongCrates(timeStamp,eHead->getToken(),EEfeeRawEvent::headVer1);
    eFee-> maskBEMC();
    int nCr=eFee->getNGoodBlock();
    // printf("%d\n",nCr);
    if(nCr!=22) continue;
    // use only 100% healthy events
    nOK++;
    task->make();
  }
  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*nEve/(t2-t1);
  printf("sorting done, nEve=%d, acc=%d CPU event rate=%.1f Hz, total time %.1f minutes \n",nEve,nAcc,rate,tMnt);


  task->finish();
  
   // save output histograms
  out+=".hist.root";
  TFile f( out,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList.GetEntries(),out.Data());
  HList.Write();
  f.Close();

  return;
  // HList.ls(); 

  h=(TH1F *) HList.FindObject("myStat05");   assert(h);
  float *my=h->GetArray();
  printf("%d -->UxV multi=%.1f  one=%.1f any=%.1f tw=%.1f\n",nEve,my[2],my[3],my[4],my[5]);

  printf(" -->MIP cntr=%.1f w/tag=%.1f \n",my[6],my[7]);

   return;
 } 

