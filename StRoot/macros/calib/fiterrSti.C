int runsti(const char *daqFile,int nEvents,const char *flg="")
{
int ians=0;
gROOT->ProcessLine(".L bfc.C");

//if(!daqFile) daqFile = "/star/data03/daq/2005/073/st_physics_6073023_raw_1040005.daq";
TString myFlg(flg);
TString opt("P2005");
for (int god=2001;god <=2009;god++) {
  TString ts("/"); ts+=god; ts +="/";
  if (strstr(daqFile,ts.Data())) {opt ="P";opt+=god;break;}
}
opt +=",MakeEvent,ITTF,OShortR,OSpaceZ2,-dstout";
int tpcOnly = myFlg.Contains("tpcOnly",TString::kIgnoreCase);
if (!tpcOnly)  opt += ",ssddat,SvtIt,SsdIt";

ians = bfc(-1,opt, daqFile,0,"pulls.root");
if (ians) {printf("ERROR: bfc(-1,...) == %d\n",ians); return iAns;}

chain->SetAttr("dbSnapshot","dbSnapshot.root","db");		
chain->SetAttr(".call","SetActive(0)","MuDst");		//NO MuDst
chain->SetAttr(".call","SetActive(0)","outputStream");	//NO Out
chain->SetAttr(".call","SetActive(0)","kink2");
chain->SetAttr(".call","SetActive(0)","StTagsMaker::");
chain->SetAttr("makePulls",1,"Sti");
if (tpcOnly) chain->SetAttr("useSvt",0,"Sti" );
if (tpcOnly) chain->SetAttr("useSsd",0,"Sti" );

chain->SetAttr(".Privilege",1,"Sti" );
ians = chain->Init(); 
if (ians) {printf("ERROR: Init() == %d\n",ians); return iAns;}

//if (tpcOnly) nEvents=10;
ians = chain->EventLoop(1,nEvents); /*>> 0.log*/
if ((ians%10)== 2) ians=0;
if (ians) {printf("ERROR: EventLoop() == %d\n",ians); return iAns;}
return 99;
}
