// $Id: rootlogoff.C,v 1.2 1999/11/02 22:55:43 kathy Exp $
// $Log: rootlogoff.C,v $
// Revision 1.2  1999/11/02 22:55:43  kathy
// fixing documentation in macro
//
///////////////////////////////////////////////////////////////
// owner:
// description:
///////////////////////////////////////////////////////////////
{
if (gClassTable->GetID("StBFChain") >=0) {
  delete StMaker::GetChain();
}
printf("\nThis is the end of ROOT -- Goodbye\n\n");
}
