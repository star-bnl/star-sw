void GenerateTemplate()
{
  gSystem->Load("VoltageCalib");
  VoltCalibrator vc;
  vc.setRefFile("test.txt");
  vc.createTemplates();
}

