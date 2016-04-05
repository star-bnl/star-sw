#ifndef _DISPLAY_DEFS_H_
#define _DISPLAY_DEFS_H_

#include <TObject.h>
#include <stdlib.h>
#include <rtsLog.h>

//#include <libxml/xmlreader.h>
struct _xmlTextReader;
typedef _xmlTextReader xmlTextReader;
typedef xmlTextReader *xmlTextReaderPtr;
typedef unsigned int u_int;


class DisplayProperty {
 public:
  char *name;
  char *value;
  DisplayProperty *next;

  DisplayProperty();
  ~DisplayProperty();
  
  void dump(int indent=0);
  void setName(const char *s);
  void setValue(const char *s);
};

class DisplayNode {
 public:
  DisplayNode *child;
  DisplayNode *prev;
  DisplayNode *next;
  DisplayNode *parent;

  char *name;
  DisplayProperty *properties;  // array of properties
  int leaf;                     // 1 if this is a histogram

  DisplayNode();
  ~DisplayNode();

  void dump(int indent=0);
  void setName(const char *s);
  void addProperty(DisplayProperty *p);
  const char *_getProperty(const char *s);
  const char *getProperty(const char *s);
  int matchTags(char *tags);
  int _matchTags(char *tags);

  int nSiblings() { 
    LOG(DBG, "nsibs:   %s  next=0x%x",name,(unsigned long) next);
    if(!next) return 0; 
    return next->nSiblings() + 1;
  }

  int getIntParentProperty(const char *str)
  {
    const char *ret;

    if(!parent) {
      LOG(ERR, "No parent for node %s\n",name);
      return -1;
    }

    ret = parent->getProperty((char *)str);
    if(!ret) return -1;
    
    return atoi(ret);
  }

  void freeChildren();
  DisplayNode *findChild(char *name);
  
  void writeXML(FILE *out, int depth);
  void spaces(FILE *out, int depth);
  void printProperties(FILE *out, int depth);

  void insertChildAlpha(DisplayNode *node);
};


// The DisplayFile class has a split personality
//
//  * "root" contains the parsed tree
//  * "textBuff" contains the raw xml
//
//  The raw xml is sent to the presenter from the server
//  truely, this is a presenter oriented structure,
//  however the server needs it because the server creates the
//  pdf file for the database.
//  
//  It also is the interface for obtaining display information.
//  As such, one needs to set which display is to be used...
//  The current display tree is stored in the "displayRoot"
//
class DisplayFile {
 public:
  int ignoreServerTags;
  char *serverTags;    // The detectors in the run

  DisplayNode *root;  // The parsed display
  DisplayNode *displayRoot;   // The root node of the current display

  char *textBuff;     // The text buffer & its length
  int textBuffLen;    // The text buffer is passed around and reparsed
  
  DisplayFile();
  ~DisplayFile();
  void chomp(char *to, char *from, int max);
  
  int Read(char *fn);
  int ReadBuff(char *buff, int len);
  int Write(char *fn);

  int isHistoString(const char *s);
  int isDirString(const char *s);
  int isNodeString(const char *s);
  
  DisplayNode *readNewNode(xmlTextReaderPtr reader);
  DisplayProperty *readNewProperty(xmlTextReaderPtr reader);

  // Manipulate displays
  void setServerTags(const char *tags);

  char *getDisplayName();
  int getDisplayIdx();
  char *getDisplay(int idx);
  int setDisplay(char *display_name);
  int setDisplay(int display);
  int nDisplays() {
    if(!root) return 0;
    if(!root->child) return 0;
    return root->child->nSiblings();   // Don't include pallete, but do include me...
  }

  // Interface for usage...
  static unsigned int getTabBase();
  static unsigned int getTabDepthMult(u_int idx);
  static unsigned int getTabNextIdx(u_int idx);
  static unsigned int getTabChildIdx(u_int idx);
  static unsigned int getTabIdxAtDepth(u_int idx, u_int depth);
  static unsigned int getFinalTabIdx(u_int idx);
  static unsigned int getPenultimateTabIdx(u_int idx);

  //DisplayNode *getTab(u_int combo_index, int *isCanvas);
  //
  // isCanvas is equilvalent to node->leaf
  // (get properties from node->parent...)
  //
  DisplayNode *getTab(u_int combo_index);
  //TabDescriptor *GetTab(int i, int j=-1);
  //HistogramDescriptor *GetHist(int i, int j, int k);

  static void test(char *fn);
  
  void dump() {
    if(!root) {
      printf("No root!\n");
    }
    else {
      root->dump();
    }
  }
};

#endif
