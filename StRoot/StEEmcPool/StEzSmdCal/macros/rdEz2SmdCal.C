rdEz2SmdCal(
	    char *runL=" R5107005 R5107005_1  R5107008 R5109025 R5109026 R5109027 R5109028a R5109028c R5109029a R5109030 R5109031  R5109034  R5112004",
	    char *outname="allAE"
	    ) {

  // char *runL=" R5109034 R5109035 R5112004 R5132005 R5132007 R5132008 R5132009 R5132010 R5132011 R5132012 R5132013 R5132020 R5132021 ";
  // char *runL=" R5107005 ";
  
  TString iPath="/star/data04/sim/balewski/daq/ezTree/pp200/pp2/";
  int mxEve=5000000;
  int nDot=8;
  int firstSec=5;
  int lastSec=5;
  int oflTrigId=0;;
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
  
  TObjArray  HList;
  //........... sorters ..........
  float thrMipSmdE=0.1;
  int emptyStripCount=nDot;
  float twMipEdev=0.5;
  float presMipElow=0.3, presMipEhigh=3.;
 
  EzEEsmdCal *sorter=new  EzEEsmdCal(5); // sectID
  sorter->set(&HList,db,eFee,eHead,eTrig);
  sorter->setMipCuts(thrMipSmdE,emptyStripCount, twMipEdev,presMipElow,presMipEhigh);
  sorter->init();
  sorter->initRun(eHead->getRunNumber());

  // dump current DB content
  // db->exportAscii("dbDump2.dat"); return;
  
   
  printf("Sort %d  of total Events %d, use trigId=%d\n",mxEve, nEntries,oflTrigId);

  int nEve=0,nAcc=0,nOK=0;
  int t1=time(0);  
  for(nEve=0; nEve<nEntries && nEve<mxEve; nEve++) {
    chain->GetEntry(nEve);
   if(nEve%2000==0)printf("in %d, acc %d  ok=%d\n",nEve,nAcc,nOK);
   //   printf("%d %d %d\n",  eTrig->isTrigID(10), eTrig->isTrigID(45010), eTrig->isTrigID(45020));

   if(! eTrig->isTrigID(10) &&
      ! eTrig->isTrigID(45010) &&
      ! eTrig->isTrigID(45020) ) continue;
   // this is for sure minB events
    nAcc++;
    // verify data block consistency
    int nCr=eFee->maskWrongCrates(timeStamp,eHead->getToken(),EEfeeRawEvent::headVer1);
    if(nCr!=22) continue;
    // use only 100% healthy events
    nOK++;
    sorter->make();
  }
  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*nEve/(t2-t1);
  printf("sorting done, nEve=%d, acc=%d CPU event rate=%.1f Hz, total time %.1f minute(s) \n",nEve,nAcc,rate,tMnt);

  sorter->finish();

  // save output histograms

  TString out="out/";
  out+=outname;
  //out+="_";
  //out+=nDot;
  sorter->saveHisto(out);
  // HList.ls(); 

  h=(TH1F *) HList.FindObject("myStat");   assert(h);
  float *my=h->GetArray();
  printf("%d -->UxV multi=%.1f  one=%.1f any=%.1f tw=%.1f\n",nEve,my[2],my[3],my[4],my[5]);

  printf(" -->MIP cntr=%.1f w/tag=%.1f \n",my[6],my[7]);

   return;
  gStyle->SetPalette(1,0);
  printf("drawing ...\n");  TString out=outname;
  out+="_";
  out+=nDot;
  sorter->saveHisto(out);
  // HList.ls(); 

  h=(TH1F *) HList.FindObject("myStat");   assert(h);
  float *my=h->GetArray();
  printf("%d -->UxV multi=%.1f  one=%.1f any=%.1f tw=%.1f\n",nEve,my[2],my[3],my[4],my[5]);

  printf(" -->MIP cntr=%.1f w/tag=%.1f \n",my[6],my[7]);

   return;
  gStyle->SetPalette(1,0);
  printf("drawing ...\n");
  c=new TCanvas();  c->Divide(1,2);
  c->cd(1);h=(TH1F*)HList.FindObject("case"); h->Draw("");
  c->cd(2);h2=(TH2F*)HList.FindObject("xy05c"); h2->Draw("colz");

 //gPad->SetLogy();
 } 

