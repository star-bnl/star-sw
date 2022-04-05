int prepass(const char *daqFile,const char *flg)
{
gROOT->ProcessLine(".L bfc.C");
TString opt(flg);
opt += ",SpcChgCalG,MakeEvent,ITTF,OShortR,OSpaceZ2,MuDst,-dstout";
bfc(0,opt,daqFile);
chain->SetAttr(".call","SetActive(0)","MuDst");		//NO MuDst
chain->SetAttr(".call","SetActive(0)","outputStream");	//NO Out
chain->SetAttr(".call","SetActive(0)","kink2");
chain->SetAttr(".call","SetActive(0)","StTagsMaker::");
chain->SetAttr(".call","SetActive(0)","StStrangeMuDstMaker::");
chain->EventLoop(1,10000);
return 99;
}
