void lsFile(const char *fileName 
= "/star/rcf/data03/reco/auau200/hijing/b0_3_jetq_on/jet05/year_1h/hadronic_on/tfs_6/*.root"
           ,const char *opt=0)
{

  gSystem->Load("St_base.so");

  StFile stf;
  stf.SetDebug(0);
  stf.AddFile(fileName);
  stf.ls(opt);

}
