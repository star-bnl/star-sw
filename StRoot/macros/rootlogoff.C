// $Id: rootlogoff.C,v 1.6 2003/10/02 17:58:24 perev Exp $
// $Log: rootlogoff.C,v $
// Revision 1.6  2003/10/02 17:58:24  perev
// little improvement in rootlogoff
//
// Revision 1.5  2001/09/01 20:04:45  perev
// namespace introduced to avoi clashes
//
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
class StMaker;
namespace rootlogoff {
TClass *tclassMk=0;
::StMaker *mk=0;
}

rootlogoff::tclassMk = gROOT->GetClass("StMaker",0);
if (rootlogoff::tclassMk && rootlogoff::tclassMk->GetClassInfo()) 
{
  rootlogoff::mk = StMaker::GetChain();
  if (rootlogoff::mk) {rootlogoff::mk->Finish(); delete rootlogoff::mk;}
}
printf("\nThis is the end of ROOT -- Goodbye\n\n");
}
