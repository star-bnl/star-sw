void RunGVC()
{
  gSystem->Load("VoltageCalib");
  GVC g;
  g.setInputFile("test.txt");
  g.setOutputFile("testOut.txt");
  g.process();
}

