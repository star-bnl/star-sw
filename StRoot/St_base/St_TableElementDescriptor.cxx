//*-- Author :    Valery Fine   10/05/99  (E-mail: fine@bnl.gov)
// $Id: St_TableElementDescriptor.cxx,v 1.7 1999/12/07 22:26:27 fine Exp $
// $Log: St_TableElementDescriptor.cxx,v $
// Revision 1.7  1999/12/07 22:26:27  fine
// Clean up to remove the compilation warnings
//
// Revision 1.6  1999/08/06 15:25:03  fine
// St_TableElementDescriptor.cxx has been restored
//
// Revision 1.5  1999/08/02 00:44:12  fine
//  Wrong element size was returned - fixed
//
// Revision 1.4  1999/07/01 23:12:37  fine
// Bug fixed, when an array is supplied with no index. Afftect I/O
//
// Revision 1.3  1999/06/25 17:29:31  fine
// Some bugs with a new Streamer fixed
//
// Revision 1.2  1999/05/18 20:08:46  fine
// St_TAbleElementDescriptor class have been introduced
//
// Revision 1.1  1999/05/18 18:00:43  fine
// The first implementation of the table column descriptor
//  

#include <stdlib.h> 
#include "St_TableElementDescriptor.h"
#include "St_Table.h"
#include "TClass.h"
#include "TDataMember.h"
#include "TDataType.h"

ClassImp(St_TableElementDescriptor)

//____________________________________________________________________________
void St_TableElementDescriptor::LearnTable(St_Table *parentTable, const Char_t *columnName)
{
//
//  LearnTable() creates a descriptor of one element of the row by CINT dictionary
//

  if (!parentTable || !columnName || !columnName[0]) {
    MakeZombie();
    return;
  }
  // define dimensions if any;
  // count the open "["
  Char_t *name = StrDup(columnName);
  m_Dimensions = 0;
  Char_t *br = name - 1;
  while((br = strchr(br+1,'['))) {
    if (!m_Dimensions) *br = 0; 
    m_Dimensions++;
  }

  Int_t *indexArray = 0;
  if (m_Dimensions) {
     indexArray = new Int_t[m_Dimensions];
     memset(indexArray,0,m_Dimensions*sizeof(Int_t));
     // Define the index
     const char *openBracket  = columnName-1;
     const char *closeBracket = columnName-1;
     for (Int_t i=0; i< Int_t(m_Dimensions); i++) 
     {
          openBracket  = strchr(openBracket+1, '[');
          closeBracket = strchr(closeBracket+1,']');
          if (closeBracket > openBracket) 
             indexArray[i] = atoi(openBracket+1);
          else {
            Error("St_Table ctor", "Wrong parethethis <%s>",columnName);
            MakeZombie();
            return;
          }
     }      
  }
 
  SetName(columnName);
  SetTitle(parentTable->GetTitle());

  TClass *classPtr = parentTable->GetRowClass();
  if (!classPtr) return;

  if (!classPtr->GetListOfRealData()) classPtr->BuildRealData();
  if (!(classPtr->GetNdata())) return;

  const Char_t *types;
  Char_t *varname;

  TIter next(classPtr->GetListOfDataMembers());
  TDataMember *member = 0;
  while ( (member = (TDataMember *) next()) ) {
    varname = (Char_t *) member->GetName();   
    // define index
    if (strcmp(varname,name)) continue;
    TDataType *memberType = member->GetDataType();
    types = memberType->GetTypeName();
    m_Type = kNAN;
    if (!strcmp("float", types)) 
      m_Type = kFloat ;  
    else if (!strcmp("int", types)) 
      m_Type = kInt   ;  
    else if (!strcmp("long", types)) 
      m_Type = kLong  ;  
    else if (!strcmp("short", types)) 
      m_Type = kShort ;  
    else if (!strcmp("double", types)) 
      m_Type = kDouble;  
    else if (!strcmp("unsigned int", types)) 
      m_Type = kUInt  ;  
    else if (!strcmp("unsigned long", types)) 
      m_Type = kULong ; 
    else if (!strcmp("unsigned short", types)) 
      m_Type = kUShort; 
    else if (!strcmp("unsigned char", types))
      m_Type = kUChar;
    else if (!strcmp("char", types))
      m_Type= kChar;

    if (m_Type != kNAN) {
      Int_t dim = 0;
      Int_t globalIndex = 0;
      if ( (dim = member->GetArrayDim()) ) {
      // Check dimensions
        if (m_Dimensions && dim != Int_t(m_Dimensions)) {
           Error("LearnTable","Wrong dimension");
           parentTable->Print();
           return;
        }
        // Calculate the global index
        if (!m_Dimensions) {
            m_Dimensions = dim;
            globalIndex = 1;
        }
        for( Int_t indx=0; indx < Int_t(m_Dimensions); indx++ ){
           globalIndex *= member->GetMaxIndex(indx);
           if (indexArray) globalIndex += indexArray[indx];
        } 
      }
//      m_Size   = memberType->Size() * globalIndex; // memberType->Size();
//      m_Offset = member->GetOffset() + m_Size;     // memberType->Size() * globalIndex;
      //  for VP ***       m_Size   = memberType->Size() * (!indexArray?1:globalIndex);
      m_Size   = memberType->Size() * (indexArray?1:globalIndex);
      m_Offset = member->GetOffset() + (indexArray?m_Size * globalIndex : 0);
    }
    break;
  }
  delete [] name;
}

