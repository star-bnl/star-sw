//*-- Author :    Valery Fine   10/05/99  (E-mail: fine@bnl.gov)
// $Id: St_tableDescriptor.h,v 1.1 2000/02/25 00:48:09 fine Exp $
#ifndef STAR_St_tableDescriptor
#define STAR_St_tableDescriptor

#include "StTypeDefs.h"
#include "TTableDescriptor.h"

//______________________________________________________________________________
// $Log: St_tableDescriptor.h,v $
// Revision 1.1  2000/02/25 00:48:09  fine
// temporary interface for ROOT 2.23/12 with STAR classes inside
//
// Revision 1.7  2000/01/24 03:55:48  fine
// new nethod CreateLeafList() to create text descriptor compatible with TBranch ctor
//
// Revision 1.6  1999/10/28 00:32:55  fine
// method At() has been removed
//
// Revision 1.5  1999/09/07 19:30:29  fine
// table descriptor access has been changed. All tables are affected and must be re-compiled
//
// Revision 1.4  1999/08/12 18:53:49  fine
// clash between St_tableDescriptor::GetSize and St_Table::GetSize resolved
//
// Revision 1.3  1999/08/12 02:23:30  fine
//  GetRowDescriptor must be const
//
// Revision 1.2  1999/08/11 14:44:39  fine
// name clash with ROOT over enum resolved
//
// Revision 1.1  1999/08/11 00:40:12  fine
// new class St_tableDescriptor
//______________________________________________________________________________


#endif
