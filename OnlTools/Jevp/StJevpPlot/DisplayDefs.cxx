#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <math.h>

#include "DisplayDefs.h"
#include <libxml2/libxml/xmlreader.h>
#include "RTS/include/rtsLog.h"

DisplayProperty::DisplayProperty()
{
  name = NULL;
  value = NULL;
  next = NULL;
}

DisplayProperty::~DisplayProperty()
{
  if(name) delete name;
  if(value) delete value;
}

void DisplayProperty::dump(int indent)
{  
  for(int i=0;i<indent;i++) printf(" ");
  printf("(%s = %s)\n",name,value);
}

void DisplayProperty::setName(const char *s)
{
  if(name) delete name;
  name = new char[strlen(s)+1];
  strcpy(name, s);
}

void DisplayProperty::setValue(const char *s)
{
  if(value) delete value;
  value = new char[strlen(s)+1];
  strcpy(value, s);
}

DisplayNode::DisplayNode()
{
  child = NULL;
  prev = NULL;
  next = NULL;
  parent = NULL;
  name = NULL;
  properties = NULL;
  leaf = 0;
}

DisplayNode::~DisplayNode()
{
  if(name) delete name;
  
  DisplayProperty *p = properties;
  while(p) {
    DisplayProperty *n = p->next;
    delete(p);
    p = n;
  }
}

void DisplayNode::dump(int indent)
{
  for(int i=0;i<indent;i++) printf(" ");
  
  printf("%s-%s\n", leaf ? "histo" : "tab", name);

  DisplayProperty *p = properties;

  while(p) {
    p->dump(indent+2);
    p = p->next;
  }

  if(child) child->dump(indent+3);
  if(next) next->dump(indent);
}

void DisplayNode::setName(const char *s)
{
  if(name) delete name;
  name = new char[strlen(s)+1];
  strcpy(name, s);
}

void DisplayNode::addProperty(DisplayProperty *p)
{
  if(!properties) {
    properties = p;
  }
  else {
    DisplayProperty *child = properties;
    while(child->next) child = child->next;
    child->next = p;
  }
}

const char *DisplayNode::_getProperty(const char *s)
{  
  DisplayProperty *p = properties;
  while(p) {
    if(strcmp(p->name,s) == 0) {
      return p->value;
    }
    p = p->next;
  }
  return NULL;
}

const char *DisplayNode::getProperty(const char *s)
{
  DisplayProperty *p = properties;
  while(p) {
    if(strcmp(p->name,s) == 0) {
      break;
    }
    p = p->next;
  }

  
  if(p) return p->value;

  // There are certain defaults...
  static char val[20];
  int wide = 0;
  int deep = 0;
  
  if(child) {
    if(child->leaf) {
      // Count children:
   
      int nchildren = child->nSiblings() + 1;
 
      // Must define wide / deep
      const char *v = _getProperty("wide");
      if(v) wide = atoi(v);
      v = _getProperty("deep");
      if(v) deep = atoi(v);

      if((wide == 0) && (deep == 0)) {
	double x = sqrt(nchildren);
	wide = (int)(x+.9999);
	deep = (int)(x+.9999);
	while(wide*(deep-1) >= nchildren)  deep--;
      }
      else if(wide == 0) {
	wide = (int)((nchildren / deep) + .9999);
      }
      else if(deep == 0) {
	deep = (int)((nchildren / wide) + .9999);
      }
    }
  }
           
  if(strcmp(s, "wide") == 0) {
    sprintf(val, "%d", wide);
    return val;
  }

  if(strcmp(s, "deep") == 0) {
    sprintf(val, "%d", deep);
    return val;
  }
    
  return NULL;
}

void DisplayNode::spaces(FILE *out, int depth)
{
  for(int i=0;i<depth;i++) {
    fprintf(out, " ");
  }
}

void DisplayNode::printProperties(FILE *out, int depth)
{
  DisplayProperty *p = properties;

  while(p) {
    spaces(out, depth*3+2);
    fprintf(out, "<%s>%s</%s>\n", p->name, p->value, p->name);
    p = p->next;
  }
}

void DisplayNode::writeXML(FILE *out, int depth)
{
  int pallete = 0;
  char tag[30];

  if(depth == 0) strcpy(tag, "doc");
  else if(depth == 1) {
    if(name && (strcmp(name, "pallete") == 0)) {
      strcpy(tag, "pallete");
      pallete = 1;
    }
    else {
      strcpy(tag, "display_def");
    }
  }
    
  else if (leaf) strcpy(tag, "histogram");
  else strcpy(tag, "tab");
  
  
  spaces(out, depth*3);
  if((depth == 0) || pallete) {
    fprintf(out, "<%s>\n",tag);
  }
  else {
    fprintf(out, "<%s>%s\n", tag, name);
  }

  printProperties(out, depth);

  if(child) child->writeXML(out, depth+1);

  spaces(out, depth*3);
  fprintf(out, "</%s>\n",tag);

  if(next) next->writeXML(out, depth);
}

void DisplayNode::insertChildAlpha(DisplayNode *node)
{
  node->parent = this;

  DisplayNode *curr = child;
  if(!child) {
    child = node;
    LOG(DBG, "no other children...");
    return;
  }
  
  DisplayNode *p = curr;

  while(curr) {
    if(strcmp(node->name, curr->name) < 0) {  // Insert before curr!

      LOG(DBG, "insert before %s", curr->name);

      if(curr->prev == NULL) this->child = node;
      else curr->prev->next = node;
      node->prev = curr->prev;
      node->next = curr;
      curr->prev = node;
      return;
    }
    p = curr;
    curr = curr->next;
  }

  // Insert after p
  LOG(DBG, "insert after %s", p->name);
  if(p->next) p->next->prev = node;
  node->next = p->next;
  p->next = node;
  node->prev = p;
}


DisplayFile::DisplayFile() {
  root = NULL;
  displayRoot = NULL;
  textBuffLen = 0;
  textBuff = NULL;
  ignoreServerTags = 0;
  serverTags = NULL;
}

DisplayFile::~DisplayFile() {
  if(textBuff) free(textBuff);
  if(root) delete root;
  if(serverTags) free(serverTags);
}

// Free's all nodes below..
void DisplayNode::freeChildren()
{
  if(child) {
    delete child;
    child = NULL;
  }
}

DisplayNode *DisplayNode::findChild(char *name)
{
  DisplayNode *curr = child;

  while(curr) {
    if(curr->name == NULL) {
      LOG(ERR, "name is null");
    }
    if(strcmp(name, curr->name) == 0) {
      return curr;
    }

    curr = curr->next;
  }
  return NULL;
}


void DisplayFile::chomp(char *to, char *str, int max)
{

  if(str == NULL) {
    //printf("str == null --->");
    to[0] = '\0';
    return;
  }

  //printf("str=%s\n",str);

  while(isspace(*str)) {
    if(*str == '\0') {
      to[0] = '\0';
      return;
    }
    str++;
  }

  strncpy(to,str,max);
  to[max-1] = '\0';

  int n=strlen(to);
  for(int i=n-1;i>=0;i--) {
    if(isspace(to[i])) to[i] = '\0';
    else return;
  }
}

int DisplayFile::Read(char *fn)
{
  LIBXML_TEST_VERSION
    
  xmlTextReaderPtr reader;
  //  int err;
  // TabDescriptor *curr=NULL;
  int ret;
  //char tmp[100];

  LOG(DBG, "IN read...%s",fn);

  reader = xmlReaderForFile(fn, NULL, XML_PARSE_NOBLANKS);
  if(!reader) {
    LOG(ERR, "Error creating reader for file %s", fn);
    return -1;
  }
 
  
  ret = xmlTextReaderRead(reader);
  if(ret != 1) {
    LOG(ERR, "Error reading\n");
    return -1;
  }

  LOG(DBG, "Read new node");
  root = readNewNode(reader);

  xmlCleanupParser();

  if(root) ret = 0;

  // Now cache the old xml file text...
  int fd = open(fn, O_RDONLY);
  if(fd < 0) {
    //    char tmp[100];
    //printf("pwd = %s\n",getwd(tmp));
    LOG(ERR,"can't open %s\n",fn);
    return -1;
  }

  struct stat sbuff;
  ret = fstat(fd, &sbuff);
  int len = sbuff.st_size;

  if(textBuff) free(textBuff);
  textBuff = (char *)malloc(len);
  textBuffLen = len;

  char *buff = textBuff;
  while(len) {
    ret = read(fd, buff, len);
    len -= ret;
    buff += ret;
  }
  
  close(fd);

  displayRoot = root->child;

  return ret;
}

void DisplayFile::setServerTags(const char *builders)
{
  if(serverTags) free(serverTags);
  int len = strlen(builders);
  serverTags = (char *)malloc(len+1);
  if(!serverTags) {
    LOG(ERR, "Error allocating serverTags");
  }
  else {
    strcpy(serverTags, builders);
  }
}

int DisplayFile::Write(char *fn)
{
  FILE *f = fopen(fn, "w");
  if(!f) {
    LOG(ERR, "Error opening file %s",fn);
    return -1;
  }
  
  root->writeXML(f, 0);

  fclose(f);
  return 1;
}

int DisplayFile::isDirString(const char *s)
{
  if(strcmp(s, "doc") == 0) return true;
  if(strcmp(s, "display_def") == 0) return true;
  if(strcmp(s, "tab") == 0) return true;
  if(strcmp(s, "pallete") == 0) return true;
  
  return false;
}

int DisplayFile::isHistoString(const char *s)
{
  if(strcmp(s, "histogram") == 0) return true;
  else return false;
}

int DisplayFile::isNodeString(const char *s)
{
  return (isHistoString(s) || isDirString(s));
}

DisplayProperty *DisplayFile::readNewProperty(xmlTextReaderPtr reader)
{ 
  // Read before calling read new node, should be pointing to the node element!
  //   int ret = xmlTextReaderRead(reader);
  //   if(ret != 1) {
  //     return NULL;
  //   }

  char *element = (char *)xmlTextReaderConstName(reader);
  int type = xmlTextReaderNodeType(reader);  
  
  if(type != XML_READER_TYPE_ELEMENT) {
    LOG(CRIT,"Not an element!");
    return NULL;
  }

  DisplayProperty *p = new DisplayProperty();
  p->setName(element);

  // Parse the subxml to this node...
  for(;;) {
    LOG(DBG,"reading. type=%d   %d %d %d\n",type,XML_READER_TYPE_TEXT,
	   XML_READER_TYPE_END_ELEMENT,
	   XML_READER_TYPE_ELEMENT);

    int ret = xmlTextReaderRead(reader);
    if(ret != 1) {
      LOG(ERR, "Unexpected end of file ret = %d\n");
      return NULL;
    }

    type = xmlTextReaderNodeType(reader); 

    
    if(type == XML_READER_TYPE_TEXT) {
      char tmp[256];
      chomp(tmp, (char *)xmlTextReaderConstValue(reader), 256);
      if(strlen(tmp) > 0) {
	p->setValue(tmp);
      }
    }
    
    if(type == XML_READER_TYPE_END_ELEMENT) {
      char *curr_el = (char *)xmlTextReaderConstName(reader);
      if(strcmp(element, curr_el) != 0) {
	LOG(ERR, "Got end element for element %s but working on %s?  What's up?",
	    element, curr_el);
	return NULL;
      }
      
      return p;
    }
  }

  LOG(CRIT, "Should never happen");
  return NULL;
}

DisplayNode *DisplayFile::readNewNode(xmlTextReaderPtr reader)
{
  // Read before calling read new node, should be pointing to the node element!
  //   int ret = xmlTextReaderRead(reader);
  //   if(ret != 1) {
  //     return NULL;
  //   }

  LOG(DBG, "Yup... here");

  char *element = (char *)xmlTextReaderConstName(reader);
  int type = xmlTextReaderNodeType(reader);  
  
  //printf("element = %s\n",element);

  if((type == XML_READER_TYPE_ELEMENT) && isNodeString(element)) {
    char curr_el[256];

    strcpy(curr_el, element);    
    DisplayNode *node = new DisplayNode();
    if(isHistoString(element)) {
      node->leaf = true;
    }

    //printf("Have new neode\n");

    // Parse the subxml to this node...
    for(;;) {
      int ret = xmlTextReaderRead(reader);
      if(ret != 1) {
	LOG(ERR, "Unexpected end of file");
	return NULL;
      }

      element = (char *)xmlTextReaderConstName(reader);
      type = xmlTextReaderNodeType(reader);

      
      //printf("Next element: %s\n",element);

      // If its a new node, add the new node...
      if((type == XML_READER_TYPE_ELEMENT) && isNodeString(element)) {   
	if(!node->child) {
	  node->child = readNewNode(reader);
	  node->child->parent = node;

	  if(strcmp(element, "pallete") == 0) {
	    node->child->setName("pallete");
	  }
	}
	else {
	  DisplayNode *child = node->child;
	  while(child->next) child = child->next;
	  
	  child->next = readNewNode(reader);

	  if(strcmp(element, "pallete") == 0) {
	    child->next->setName("pallete");
	  }

	  child->next->parent = node;
	  child->next->prev = child;
	}
      }

      if((type == XML_READER_TYPE_ELEMENT) && !isNodeString(element)) { 
	//printf("Its a property\n");

	node->addProperty(readNewProperty(reader));
      }


      if(type == XML_READER_TYPE_TEXT) {
	//printf("here\n");
	char tmp[256];
	
	chomp(tmp, (char *)xmlTextReaderConstValue(reader), 256);

	//printf("everywhere\n");

	if(strlen(tmp) > 0) {
	  // printf("setname %s\n",tmp);
	  node->setName(tmp);
	}
	//printf("There\n");
      }

      if(type == XML_READER_TYPE_END_ELEMENT) {

	//printf("End element %s %s\n",element, curr_el);

	if(strcmp(element, curr_el) != 0) {
	  printf("Got end element for element %s but working on %s?  What's up?\n",
		 element, curr_el);
	  return NULL;
	}

	return node;
      }
    }
  }

  LOG(CRIT, "Should never happen!");
  return NULL;    // shouldn't happen...
}

int DisplayFile::ReadBuff(char *buff, int len)
{
  char templat[100];
  strcpy(templat, "/tmp/evpreader.XXXXXX");

  int fd = mkstemp(templat);
  int ret;

  // printf("there 0x%x %d\n",buff,len);


  //printf("buff\n%s\nendbuff\n",buff);

  while(len > 0) {
    ret = write(fd, buff, len);
    if(ret <= 0) { return -1; };
    buff += ret;
    len -= ret;
  }

  close(fd);
  
  ret = Read(templat);
  
  unlink(templat);
  
  return ret;
}

char *DisplayFile::getDisplayName()
{
  return displayRoot->name;
}

int DisplayFile::getDisplayIdx()
{
  DisplayNode *disp = root->child;

  int i=0;
  while(disp) {
    if(disp == displayRoot)  return i;
    disp = disp->next;
    i++;
  }
  return -1;
}

char *DisplayFile::getDisplay(int idx)
{
  DisplayNode *disp = root->child;
  int i=0;
  while(disp) {
    if(i == idx) return disp->name;
    i++;
    disp = disp->next;
  }

  return NULL;
}

int DisplayFile::setDisplay(char *display_name)
{
  DisplayNode *disp = root->child;

  int i=0;
  while(disp) {
    LOG(DBG, "0x%x",disp->name);
    LOG(DBG, "disp->name = %s",disp->name);

    if(strcmp(disp->name, display_name) == 0) {
      displayRoot = disp;
      return i;
     }
    
    disp = disp->next;
    i++;
  }

  return -1;
}

// Forget the index, go by the order...
int DisplayFile::setDisplay(int idx)
{
  DisplayNode *disp = root->child;
  int i=0;
  while(disp) {
    if(i == idx) {
      displayRoot = disp;
      printf("Set display  %s : %d\n",disp->name, idx);
      return idx;
    }
    
    disp = disp->next;
    i++;
  }
  
  return -1;
}

#define TAB_BASE 100

u_int DisplayFile::getTabBase()
{
  return TAB_BASE;
}

// Gets the multiplier to access the final
u_int DisplayFile::getTabDepthMult(u_int idx)
{
  u_int depth = 1;
  idx /= TAB_BASE;
  while(idx) {
    idx = idx / TAB_BASE;
    depth = depth * TAB_BASE;
  }
  return depth;
}

u_int DisplayFile::getTabNextIdx(u_int idx)
{
  u_int m = getTabDepthMult(idx);
  idx += m;
  return idx;
}

u_int DisplayFile::getTabChildIdx(u_int idx)
{
  u_int m = getTabDepthMult(idx);
  m *= TAB_BASE;
  idx += m;
  return idx;
}

u_int DisplayFile::getTabIdxAtDepth(u_int idx, u_int depth)
{
  while(depth) {
    idx /= TAB_BASE;
    depth--;
  }

  return idx % TAB_BASE;
}

// Now returns a displayNode
// isCanvas means it has histograms rather than tabs...
//
// Used to returns either a tab or a canvas descriptor....
//
// TabDescriptor *tab;     // if *isCanvas=0
// CanvasDescriptor *can;  // if *isCanvas=1
//



DisplayNode *DisplayFile::getTab(u_int combo_index)
{
  //LOG("JEFF", "get: %d", combo_index);

  // "1" == first child...
  DisplayNode *node = displayRoot->child;

  for(int depth=0;;depth++) {
    int idx = getTabIdxAtDepth(combo_index, depth);
    int next_idx = getTabIdxAtDepth(combo_index, depth+1);

    //LOG("JEFF","A:idx(%d@%d)=%d next=%d (%s) ignore=%d match=%d\n",combo_index,depth,idx,next_idx,node ? node->name : "null", ignoreServerTags, node->matchTags(serverTags));
    
    while(node && !(ignoreServerTags || node->matchTags(serverTags))) {
      node = node->next;   
      if(node) {
	//LOG("JEFF","B:idx(%d@%d)=%d next=%d (%s) ignore=%d match=%d\n",combo_index,depth,idx,next_idx,node ? node->name : "null", ignoreServerTags, node->matchTags(serverTags));
      }
      else {
	//LOG("JEFF", "no siblings");
	continue;
      }
    }
    
    if(!node) return NULL;

    int x;
    for(x=1;x<idx;x++) {
      LOG(DBG,"---->horizontal search[%d / %d] looking at: %s\n",x,idx,node ? node->name : "null");

      node = node->next;

      while(node && !(ignoreServerTags || node->matchTags(serverTags))) {
	node = node->next;
      }

      if(!node) return NULL;
    }
    
    if(!node) return NULL;
 
    LOG(DBG, "---->horizontal search[%d / %d] found %s\n",x,idx,node->name);
    
    if(next_idx == 0) {
      //printf("Got a tab...%s\n",node->name);
      return node;
    }

    if(!node->child) {
      LOG(ERR, "No child node for %s?",node->name);
      return NULL;
    }

    node = node->child;
  }
}

int DisplayNode::matchTags(char *tags) {
  if(!_matchTags(tags)) return 0;

  // Ok if a leaf
  if(leaf) return 1;

  // If a tab, some subtab must be filled...
  DisplayNode *node = child;

  while(node) {
    if(node->matchTags(tags)) return 1;
    node = node->next;
  }

  return 0;
}

int DisplayNode::_matchTags(char *tags)
{
  if(!tags) return 0;

  const char *val = _getProperty("requireTag");
  if(!val) return 1;

  static char requiredTags[256];
  static char req[32];
  
  strcpy(requiredTags, val);
  char *x = strtok(requiredTags, ",");

  while(x) {
    sprintf(req, "|%s|", x);
    if(!strstr(tags, req)) {
      LOG(DBG, "Match builders return false: didn't find %s in %s",req,tags);
      return 0;
    }

    x = strtok(NULL, ",");
  }
  
  LOG(DBG, "Match builders return true: found %s in %s",val, tags);
  return 1;
}


// Surely these are obsolete?

// TabDescriptor *DisplayDefinition::GetTab(int i, int j)
// {
//   TabDescriptor *tab = tabs;
  
//   for(int x=0;x<i;x++) {
//     if(tab == NULL) return NULL;
//     tab = tab->next;
//   }

//   if(j == -1) return tab;

//   tab = tab->child;
//   for(int x=0;x<j;x++) {
//     if(tab == NULL) return NULL;
//     tab = tab->next;
//   }
  
//   return tab;
// }

// HistogramDescriptor *DisplayDefinition::GetHist(int i, int j, int k)
// {
//   TabDescriptor *tab = GetTab(i,j);
//   if(!tab) return NULL;
  
//   if(!tab->canvas) return NULL;
  
//   HistogramDescriptor *hd = tab->canvas->first_histo;
  
//   for(int x=0;x<k;x++) {
//     if(hd == NULL) return NULL;
//     hd = hd->next;
//   }

//   return hd;
// }
    
    
void DisplayFile::test(char *fn)
{
  rtsLogOutput(RTS_LOG_NET);
  rtsLogAddDest((char *)"172.16.0.1",8004);
  rtsLogLevel((char *)DBG);
  
  LOG(DBG, "HERE...");
  DisplayFile *dd = new DisplayFile();
  LOG(DBG, "Read...");
  dd->Read(fn);
  LOG(DBG, "Done...");
  dd->root->dump();
  delete dd;
}
