/* Print MuTrack paramters
   
 */

void MuTrack(Int_t i = 0) {
  StMuTrack *muTrack = StMuDst::instance()->globalTracks(i);
  if (! muTrack) return;
  muTrack->Print();
  Float_t p[6], C[21];
  muTrack.kfpTrackAtFirstHit().GetXYZPxPyPz(p);
  muTrack.kfpTrackAtFirstHit().GetCovarianceXYZPxPyPz(C);
  cout << "Float_t kfp[6] = {"; for (Int_t i = 0; i < 6; i++) {cout << Form("%10.5g",p[i]); if (i !=5) cout << ","; else cout << "};" << endl;}
  cout << "Float_t kfC[21] = {"; for (Int_t i = 0; i < 21; i++) {cout << Form("%10.3g",C[i]); if (i !=20) cout << ","; else cout << "};" << endl;}
};
