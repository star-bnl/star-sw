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





