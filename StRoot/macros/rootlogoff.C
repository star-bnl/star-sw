// $Id: rootlogoff.C,v 1.3 2000/08/27 17:46:08 fisyak Exp $
// $Log: rootlogoff.C,v $
// Revision 1.3  2000/08/27 17:46:08  fisyak
// Call Finish for a Chain
//
// Revision 1.2  1999/11/02 22:55:43  kathy
// fixing documentation in macro
//
///////////////////////////////////////////////////////////////
// owner:
// description:
///////////////////////////////////////////////////////////////
{
if (gClassTable->GetID("StChain") >=0) {
  delete StMaker::GetChain();
}
printf("\nThis is the end of ROOT -- Goodbye\n\n");
}
