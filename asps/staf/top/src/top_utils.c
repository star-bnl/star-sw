/* top_utils.c */
#include <string.h>
#include "top_utils.h"

top_bool isValidSelectSpec(const char * spec)
{
   switch (spec[0]) {
   case '{':
      switch (spec[strlen(spec) -1]) {
      case '}':
	 break;
      default:
	 return FALSE;
	 break;
      }
      break;
   case '-':
      return FALSE;
      break;
   default:
      return FALSE;
      break;
   }
   return TRUE;		/* Obvious HACK */
}

top_bool isValidWhereClause(const char * clause)
{
   switch (clause[0]) {
   case '{':
      switch (clause[strlen(clause) -1]) {
      case '}':
	 break;
      default:
	 return FALSE;
	 break;
      }
      break;
   case '-':
      return FALSE;
      break;
   default:
      return FALSE;
      break;
   }
   return TRUE;		/* Obvious HACK */
}

