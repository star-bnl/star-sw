// Create the Sti "drawable" geometry view from the current STAR "chain"
TVolume *MakeStiGeometry()
{
   StMaker *starChain = StMaker::GetChain();
   StiDetectorVolume *v =0;
   if (starChain) {
      // Find the StiMaker
     StiMaker *stiMk = starChain->Maker("Sti");
     if (stiMk) {
        StiToolkit *t = stiMk->getToolkit();
        if (t) {
           StiMasterDetectorBuilder *b = t->getDetectorBuilder();
           if (b) v = new StiDetectorVolume(*b);
        }
     }
  }
  return v;
}
