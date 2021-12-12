{
  if (TClassTable::GetDict("StMaker"))
  {
    StMaker* mk = StMaker::GetChain();
    if (mk) {
      mk->Finish();
    }
  }

  std::cout << "\nThis is the end of STAR ROOT -- Goodbye\n" << std::endl;
}
