{
// make sure that switch WriteMiniEvent = 1 in Stl3RawReaderMaker::Init()
// .x bfc.C(5,"off tdaq tpcDB","/star/rcf/data08/daq/2000/08/st_physics_1243037_raw_0001.daq")
// tree = ((Stl3RawReaderMaker*) (chain->GetMaker("l3RawReader")))->GetGlobalTrackTree()
// tree->Print()
// tree->Draw("mTracks.mOuterMostRow")

// if you want to write the Tree out
// TFile f("run007.root","new")
// tree->Write()
// tree->Close()
}
