// $Id: rootlogoff.C,v 1.4 2000/08/31 13:15:45 fisyak Exp $
// $Log: rootlogoff.C,v $
// Revision 1.4  2000/08/31 13:15:45  fisyak
// Force call to Finish before quit (Victor)
//
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
  StMaker *mk = StMaker::GetChain();
  if (mk) {mk->Finish(); delete mk;}
}
printf("\nThis is the end of ROOT -- Goodbye\n\n");
}
