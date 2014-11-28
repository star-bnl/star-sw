void KFParticleTest() {
  if (! TClass::GetClass("KFParticle")) gSystem->Load("KFParticle");
  if (! TClass::GetClass("KFParticleTest")) gSystem->Load("KFParticleTest");
  KFParticleTest kfptest; 
  kfptest.RunTest();
}
