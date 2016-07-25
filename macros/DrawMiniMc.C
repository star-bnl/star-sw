{
  StMiniMcTree->Draw("mMcTracks.mEtaMc","mMcTracks.mPtMc>0.2&&mMcTracks.mIsValid");
  StMiniMcTree->Draw("mMatchedPairs.mEtaMc","mMatchedPairs.mPtMc>0.2&&mMatchedPairs.mIsValid");

}
