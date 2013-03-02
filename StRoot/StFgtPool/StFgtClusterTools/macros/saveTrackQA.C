
void saveTrackQA(Char_t* signalFile="signalShapes.root")
{
  TFile f(signalFile);
  Char_t buffer[100];
  Char_t quadName[10];
  Char_t layerName[10];


  Char_t* trkChi2="";
  Char_t* numTrkPerEv="numTracksPerEvent";
  Char_t* numPointsPerTrack="numPointsPerTrack";
  Char_t* vtxDist="z_Vtx_From_trk_fit";
  TCanvas c;
  TH1D* h=(TH1D*)f.Get(numTrkPerEv);
  if(h!=0)
    {
      h->SetTitle("Number of Tracks per Event");
      h->SetName(numTrkPerEv);
      h->Draw();
      c.SaveAs("numTrkPerEv.png");
    }
  
  h=(TH1D*)f.Get(numPointsPerTrack);
  if(h!=0)
    {
      h->SetTitle("Number of Points per Track");
      h->SetName(numPointsPerTrack);
      h->Draw();
      c.SaveAs("numPointsPerTrack.png");
    }

	h=(TH1D*)f.Get(vtxDist);
      if(h!=0)
    {
      h->SetTitle("Vertex Distribution");
      h->SetName(vtxDist);
      h->Draw();
      c.SaveAs("zVtxDist.png");
    }

}
