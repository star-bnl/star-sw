void getPrescales(const char *input)
{
  gSystem->Load("MyEvent/MyEvent");

  TFile *mFile=new TFile(input,"OPEN");
  TTree *myEventTree=(TTree*)mFile->Get("mEventTree");
  MyEvent *ev=new MyEvent();
  myEventTree->SetBranchAddress("branch",&ev);

  int i=0;
  int runprev=0;
  int NmbInRun=0;
  int Nht1InRun=0;
  int Nht2InRun=0;
  float ps_mb=0.;
  float ps_ht1=0.;
  float ps_ht2=0;
  while(myEventTree->GetEntry(i)){
    int runn=ev->runId();
    if(runn!=runprev){
      if(i==0) cout<<"run    nmb    nht1    nht2    psmb    psht1    psht2"<<endl;
      else cout<<runprev<<" "<<NmbInRun<<" "<<Nht1InRun<<" "<<Nht2InRun<<" "<<ps_mb<<" "<<ps_ht1<<" "<<ps_ht2<<endl;

      runprev=runn;
      ps_mb=ev->prescale(0);
      ps_ht1=ev->prescale(2);
      ps_ht2=ev->prescale(3);
      NmbInRun=0;
      Nht1InRun=0;
      Nht2InRun=0;

    }
	
    if(ev->trigger()&1) NmbInRun++;
    if(ev->trigger()&2) Nht1InRun++;
    if(ev->trigger()&4) Nht2InRun++;

    i++;
  }

}
