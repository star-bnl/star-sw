rdEz2SmdCal(  char *run="R5107008",  int mxEve=1000 ) {
  TString calDir="./calD1/";
  TString out="/star/data05/scratch/balewski/outD2/";

  int firstSec=5;
  int lastSec=8;
  char *libL[]={
    "StRoot/StDbLib/StDbLib.so",  
    "StRoot/StEEmcDbMaker/libEEmcDbMaker.so", 
    "StRoot/StEEmcUtil/EEfeeRaw/libEEfeeRaw.so",
    "StRoot/StEEmcUtil/EEmcGeom/libEEmcGeom.so",
    "StRoot/StEEmcUtil/StEEmcSmd/libEEmcSmdGeom.so",
    "StRoot/StEEmcUtil/EEmcSmdMap/libEEmcSmdMap.so",
    "StRoot/StEEmcPool/StEzSmdCal/libEzSmdCal.so",
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
  char *run=strtok(runL," "); // init 'strtok'
  int i=0;
  do {
    printf("add run %d '%s' \n",i++,run);
    TString fullName=iPath+run+".ez.root";  
    chain->Add(fullName);
  } while(run=strtok(0," "));  // advance by one nam
#endif

  TString runList="ezList/"; runList+=run; 
  FILE *fd=fopen(runList.Data(),"r"); assert(fd);
  printf("aa=%s=\n",runList.Data());
  int i=0;
  while(1) {
    char text[500];
    int ret= fscanf(fd,"%s",text);
    if(ret<=0) break;
    i++;
    chain->Add(text);    
    printf("%d =%s=\n",i,text);
  }

  int mSect=lastSec-firstSec+1;
  int nEntries = (Int_t)chain->GetEntries();
  printf("Sort %d  of total Events %d, mSect=%d\n",mxEve, nEntries,mSect);
  int nEve=0;
  //  return;  

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

  int j;  
  for(j=0;j<mSect;j++) {
    int secID=firstSec+j;
    char tt1[100];
    sprintf(tt1,"%02d",secID);
    TString tt=calDir+"gains"+tt1+"tower.dat";    
    db->changeGains(tt.Data());
    tt=calDir+"gains"+tt1+"PQR.dat";    
    db->changeGains(tt.Data());
    tt=calDir+"gains"+tt1+"Usmd.dat";    
    db->changeGains(tt.Data());
    tt=calDir+"gains"+tt1+"Vsmd.dat";    
    db->changeGains(tt.Data());

    tt=calDir+"mask"+tt1+".dat";    
    db->changeMask(tt.Data());
  }
  
  TObjArray  HList;
  //........... sorters ..........
  float thrMipSmdE=0.3/1000.;
  int emptyStripCount=6;
  float offCenter=0.8; // fiducial area of tower =offCenter^2
  float twMipRelEneLow=0.3, twMipRelEneHigh=2.;

  EzEEsmdCal **sorterA=new  EzEEsmdCal *[mSect];

  for(j=0;j<mSect;j++) {
    sorterA[j]=new  EzEEsmdCal(firstSec+j); // sectID
    sorterA[j]->set(&HList,db,eFee,eHead);
    sorterA[j]->setTwCuts(twMipRelEneLow, twMipRelEneHigh,offCenter);
    sorterA[j]->setSmdCuts(thrMipSmdE,emptyStripCount);
    sorterA[j]->init();
    sorterA[j]->initRun(eHead->getRunNumber());
  }

  // dump current DB content
  db->exportAscii("dbDumpD1.dat"); return;
   
  printf("Sort %d  of total Events %d\n",mxEve, nEntries);

  int nEve=0,nAcc=0,nOK=0;
  int t1=time(0);  
  for(nEve=0; nEve<nEntries && nEve<mxEve; nEve++) {
    chain->GetEntry(nEve);
   if(nEve%2000==0)printf("in %d, acc %d  ok=%d\n",nEve,nAcc,nOK);
   // printf("%d %d %d\n",  eTrig->isTrigID(10), eTrig->isTrigID(45010), eTrig->isTrigID(45020));

   if(! eTrig->isTrigID(10) &&
      ! eTrig->isTrigID(45010) &&
      ! eTrig->isTrigID(45020) ) continue;
   // this is for sure minB events
    nAcc++;
    // verify data block consistency
    eFee->maskWrongCrates(timeStamp,eHead->getToken(),EEfeeRawEvent::headVer1);
    eFee-> maskBEMC(); 
    int nCr=eFee->getNGoodBlock();
    //printf("nCr=%d\n",nCr);

    //eFee->print(0); break;
    if(nCr!=22 ) continue; // zero tolarance for corruption
    // use only 100% healthy events
    nOK++;
    for(int j=0;j<mSect;j++) {
      sorterA[j]->make();
    }
  }
  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*nEve/(t2-t1);
  printf("sorting done, nEve=%d, acc=%d CPU event rate=%.1f Hz, total time %.1f minute(s) \n",nEve,nAcc,rate,tMnt);

  //  sorterA[0]->finish(1);
  for(int j=0;j<mSect;j++) {
    sorterA[j]->finish();
  }

  // save output histograms
  out+=run;
  out+=".hist.root";
  TFile f( out,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList.GetEntries(),out.Data());
  HList.Write();
  f.Close();
  
  // HList.ls(); 

  for(j=0;j<mSect;j++) {
    int secID=firstSec+j;
    char tt1[100];
    sprintf(tt1,"myStat%02d",secID);
    h=(TH1F *) HList.FindObject(tt1);   assert(h);
    float *my=h->GetArray();
    printf("sec=%d nInp=%d  -->UxV multi=%.1f  one=%.1f any=%.1f tw=%.1f\n",secID,nEve,my[2],my[3],my[4],my[5]);
    printf(" -->MIP cntr=%.1f w/tag=%.1f \n",my[6],my[7]);
  }
   return;
 } 

