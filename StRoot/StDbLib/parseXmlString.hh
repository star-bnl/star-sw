/***************************************************************************
 *
 * $Id: parseXmlString.hh,v 1.3 1999/12/28 21:31:43 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  parses Xml file & returns contents between request TAGs
 *
 ***************************************************************************
 *
 * $Log: parseXmlString.hh,v $
 * Revision 1.3  1999/12/28 21:31:43  porter
 * added 'using std::vector' and 'using std::list' for Solaris CC5 compilation.
 * Also fixed some warnings arising from the CC5 compiles
 *
 * Revision 1.2  1999/09/30 02:06:15  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef PARSEXMLSTRING_HH
#define PARSEXMLSTRING_HH


class parseXmlString {

public:

  parseXmlString(){};
  ~parseXmlString(){};

  char * getString(char* line, char* key1, char* key2);
  char * getStringAfter(char* line, char* key);
  char * getStringBefore(char* line, char* key);
  int getIndexAfter(char* line, char* key);
  int getIndexBefore(char* line, char* key);
  char* removeBlankEnds(char* line);
};


#endif










