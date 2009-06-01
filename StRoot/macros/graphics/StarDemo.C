void StarDemo(const char *detector="StarDetectorUnfolding")
{
  gROOT->Macro("Load.C");
  gEventDisplay->ShowDetectorTest(detector);
}
