MuMomShift(const Int_t   mode=0,
           const Int_t   nevents=10,
           const Char_t  *path="/star/data13/reco/dev/2001/10/",
           const Char_t  *file="st_physics_2304060_raw_0303.event.root",
           const Char_t* outDir="./"){
  gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  StChain *chain=new StChain();  // Need chain for communication between makers
  StMuDstMaker *mudst_mk=new StMuDstMaker(mode,0,path,file);

  StMuMomentumShiftMaker *mom_mk=new StMuMomentumShiftMaker(outDir);
  
  Int_t i_event=0;
  while (mudst_mk->Make()==kStOk && i_event < nevents) {
    StMuEvent *event=mudst_mk->muDst()->event();
    Float_t mag_field=event->runInfo().magneticField();
    if (mag_field < 2.46) 
      mom_mk->setScaleFactor(0.499/0.49);
    else 
      mom_mk->setScaleFactor(1);
    mom_mk->Make();
    i_event++;
  }  
  mom_mk->Finish();
}
