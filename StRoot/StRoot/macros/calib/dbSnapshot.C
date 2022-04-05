int dbSnapshot(const char *daqFile,int nEvents,const char *flg="")
{
int ians=0;
gROOT->ProcessLine(".L bfc.C");

TString opt(flg);
printf("\n============= runJan: file=%s\n",daqFile);
printf("============= runJan: Opt =%s\n",opt.Data());

ians = bfc(-1,opt, daqFile,0,"pulls.root");
if (ians) {printf("ERROR: bfc(-1,...) == %d\n",ians); return iAns;}

chain->SetAttr("dbSnapshot","dbSnapshot.root","db");		
ians = chain->Init(); 
if (ians) {printf("ERROR: Init() == %d\n",ians); return iAns;}

ians = chain->EventLoop(nEvents);
return ians;
}
