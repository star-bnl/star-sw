{
if (gClassTable->GetID("StBFChain") >=0) {
  delete StMaker::GetChain();
}
printf("\nThis is the end of ROOT -- Goodbye\n\n");
}
