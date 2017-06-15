// $Id: rootlogoff.C,v 1.1 2006/11/03 19:02:42 jeromel Exp $
// $Log: rootlogoff.C,v $
// Revision 1.1  2006/11/03 19:02:42  jeromel
// Forgot to commit those for the root5/ branch
//
// Revision 1.1  2006/10/12 20:53:43  jeromel
// Moving rootlogoff.C to ROOTSYS
//
// Revision 1.12  2005/02/05 01:21:37  perev
// STARNODELETE env added
//
// Revision 1.11  2004/11/14 17:03:56  fisyak
// delete chain in .DEV version only
//
// Revision 1.10  2004/02/11 23:23:36  perev
// avoid ROOT warning
//
// Revision 1.9  2004/01/23 21:55:07  perev
// delete chain only in .DEV
//
// Revision 1.8  2004/01/15 00:53:55  perev
// Delete chain only for DEBUG lib
//
// Revision 1.7  2004/01/14 20:13:13  perev
// flag -nodelete added
//
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
// namespace rootlogoff {
// ::StMaker *mk=0;
// }
 StMaker *rootlogoff_mk = 0;
if (TClassTable::GetDict("StMaker")) 
{
  rootlogoff_mk = StMaker::GetChain();
  if (rootlogoff_mk) {
    

    rootlogoff_mk->Finish();
#if 0
    if (TString(gSystem->Getenv("STAR_VERSION")).BeginsWith(".DEV")
	&& !gSystem->Getenv("STARNODELETE")) delete rootlogoff_mk;
    else  
#endif
      printf ("*** Chain not deleted***\n");
  }
}
printf("\nThis is the end of ROOT -- Goodbye\n\n");
}
