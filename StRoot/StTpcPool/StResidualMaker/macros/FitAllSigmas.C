
// run this after you have a fully configured ResidualMaker
// (e.g. you have just read it in, or so)
// it fits all the sigma distributions, and makes .gif files
// for them


void FitAllSigmas(StResidualMaker* rm){
  rm->fitRMShisto(rm->mPrimaryHistos[10])->Print("SigXinnPrim.gif","gif");
  rm->fitRMShisto(rm->mPrimaryHistos[11])->Print("SigXoutPrim.gif","gif");
  rm->fitRMShisto(rm->mPrimaryHistos[12])->Print("SigZinnPrim.gif","gif");
  rm->fitRMShisto(rm->mPrimaryHistos[13])->Print("SigZoutPrim.gif","gif");

  rm->fitRMShisto(rm->mGlobalHistos[10])->Print("SigXinnGlob.gif","gif");
  rm->fitRMShisto(rm->mGlobalHistos[11])->Print("SigXoutGlob.gif","gif");
  rm->fitRMShisto(rm->mGlobalHistos[12])->Print("SigZinnGlob.gif","gif");
  rm->fitRMShisto(rm->mGlobalHistos[13])->Print("SigZoutGlob.gif","gif");
}
