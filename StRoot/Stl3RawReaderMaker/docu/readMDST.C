{
gROOT->Reset() ;
gROOT->LoadMacro("/afs/rhic/star/packages/DEV/StRoot/macros/Load.C") ;
Load() ;

// load shared lib to get definition of l3 mini Event
gSystem->Load("Stl3RawReaderMaker") ;

// load mini event
TFile f("/afs/rhic/star/users/flierl/public/test/run007.root","Read") ;
f->ls();
myTree = (TTree*) f->Get("L3GTracks;1") ;
myTree->Print() ;

// display temperature
myTree->Draw("mTracks.mPt","mNTracks>1000 && mTracks.mNHits>15 && mTracks.mPt<2") ;


}
