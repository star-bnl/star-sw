// $Id: rootlogon.C,v 1.5 1999/06/11 23:01:56 perev Exp $
// $Log: rootlogon.C,v $
// Revision 1.5  1999/06/11 23:01:56  perev
// cleanup
//
// Revision 1.4  1999/05/21 15:34:00  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner:  Valery Fine
// what it does: 
//=======================================================================

{
#include <iostream.h>
   printf("\nWelcome to the ROOT tutorials\n\n");
   printf("\nType \".x STAR_Demos.C\" to get a toolbar from which to execute the STAR demos\n");
   printf("\nType \".x demos.C\" to get a toolbar from which to execute the demos\n");
   printf("\nType \".x demoshelp.C\" to see the help window\n\n");
#pragma includepath "./StRoot"
#pragma includepath "/afs/rhic/star/packages/dev/StRoot"

// 	Assign bif size of hashtable for STAR I/O
TBuffer::SetGlobalWriteParam(2003);
printf("fgMapSize=%d\n",TBuffer::GetGlobalWriteParam());
gROOT->SetStyle("Plain");

}
 
