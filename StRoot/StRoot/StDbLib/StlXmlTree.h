#ifndef NoXmlTreeReader
#ifndef __CINT__
#include <libxml/xmlreader.h>
#endif
/*! \class StlXmlTree
* 
* Name: StlXmlTree
* 
* Author: Mikhail Kopytin
* 
* Date: 08/29/2006
* 
* 1. Abstract: a data structure relying on STL containers to represent a
* generic XML tree. The constructor can optionally use another object of
* this class as a filter to optimize tree browsing.
* 
* 2. The basic  idea: do build a  data struture, but  embed the decision
* making  into   the  building  process.   Do   not  include  irrelevant
* information in the  data structure.  Veto tree branches  which are not
* part of the context once the context is resolved.
* 
* 3. The basic data structure to  associate element names with values is
* a multimap, called XmlTree. The underlying structure is a tree, giving
* logarithmic access  time when  accessing by a  key.  Maps  implement a
* tree internally. The hierarchy of  the XML is transformed into the key
* string sequence,  and the internal tree (based  on string comparisons)
* will have a corresponding topology.
* 
* Example:
* <a> A <b> B <c> C </c> </b> </a>
* will correspond to an StlXmlTree ("--" separates the associated pairs):
* -----------------------------------------
* XmlTree contains:
* >a() -- #text=A;
* >a()(#text=A;)>b() -- #text=B;
* >a()(#text=A;)>b()(#text=B;)>c() -- #text=C;
* -----------------------------------------
* 
* As one can see, the information in the tree provides a self-guided way
* to retrieve information associated with  a node based on the operation
* opposite to  parsing, namely string  concatenation, followed by  a map
* look-up.
* 
* Attributes are treated much like regular content: there is no separate
* data structure  to hold them.   libxml2 checks to ensure  a one-to-one
* correspondence between a set of attribute names and a set of attribute
* values.
* 
* Example: 
* <a>
* <b c="C" cc="CC1" /> 
* <b c="XXX" ccc="CCC"> this is B1 </b>
* <b c="C2"> this is B2 </b>
* </a>
* will correspond to an StlXmlTree:
* -----------------------------------------
* XmlTree contains:
* >a()>b -- c=C;cc=CC1;
* >a()>b -- c=XXX;ccc=CCC;
* >a()>b -- c=C2;
* >a()>b(c=C2;) -- #text=thisisB2;
* >a()>b(c=XXX;ccc=CCC;) -- #text=thisisB1;
* -----------------------------------------
* 
* 
* 
* 4. XML Filtering. 
* 
* 4.1 Data structure. 
* 
* Initially  the parser  receives  information from  the  user, such  as
* server       scope,       site       name,      and       so       on.
* (http://www.star.bnl.gov/STAR/comp/sofi/FileCatalog/schema/SCATALOG.html)
* That  information  is  used  to  build  parts  of  the  tree.   Later,
* information with  contradicting values will be  ignored.  For example,
* if user's credentials say his site is BNL, then information related to
* LBL  will  be  ignored  ("vetoed").  The  information  describing  the
* vetoing is stored in another StlXmlTree object. I use two instances of
* a single  XML-parsing class, one  to hold user's info  without vetoing
* anything, the  other using the  first as a  veto holder (filter)  in a
* conditional  tree-building  process.    The  filter  is  therefore  an
* optional property of  an StlXmlTree object.  If the  filter is absent,
* parse unconditionally.
* 
* 4.2 Interaction with the filter.
* 
* In  the  subsequent  discussion  we  distinguish a  filter  file  (XML
* instance) from a 'base' file  (XML instance).  The purpose of a filter
* file is to  control selection of a subset of a  base file.  The filter
* file is an XML file which may or may not follow the same schema as the
* base file.   Information in the  filter file has  a veto right  on the
* base XML file content, rather  than being a prerequisite for accepting
* the content.  In other words, the system defaults to accepting maximum
* content  unless explicit  contradiction between  filter and  base file
* data is found (is "greedy").
* 
* 
* 
* Example:
* The base file contains:
* <a>A<...></a>
* If the filter file contains
* <t>test</t>
* or
* <a>A</a>
* or
* <a></a>
* then the base file will be fully parsed.
* 
* If the filter file contains
* <a>Another a</a>
* then the base file will be fully skipped.
* 
* Selective parsing examples:
* The base file contains:
* 
* <a a1="A1">
* <b b1="B1" b2="B2">
* <c c1="B1C1"></c>
* </b> 
* <b b1="111" b2="222">
* <c c1="C1">
* this is c
* </c>
* </b>
* <o> more stuff </o>
* </a>
* 
* 
* 
* When parsed with no filter, a data structure looks like:
* -----------------------------------------
* XmlTree contains:
* >a -- a1=A1;
* >a(a1=A1;)>b -- b1=B1;b2=B2;
* >a(a1=A1;)>b -- b1=111;b2=222;
* >a(a1=A1;)>b(b1=111;b2=222;)>c -- c1=C1;
* >a(a1=A1;)>b(b1=111;b2=222;)>c(c1=C1;) -- #text=thisisc;
* >a(a1=A1;)>b(b1=B1;b2=B2;)>c -- c1=B1C1;
* >a(a1=A1;)>o() -- #text=morestuff;
* -----------------------------------------
* 
* 
* If the filter  file contains <a a1="A2"> </a> then  the content of the
* base file is fully skipped.
* 
* If a filter  node contains attributes not found in  the base file, the
* content  of the  node is  accepted unconditionally.   Example:  if the
* filter file contains <a a2="A2"> </a>  the content of the base file is
* fully accepted.
* 
* Example: select  stuff belonging  to b-node with  attributes b1="111",
* b2="222".  Filter file:
* 
* <a a1="A1"> <b b1="111" b2="222"/> </a>
* -----------------------------------------
* XmlTree contains:
* >a -- a1=A1;
* >a(a1=A1;)>b -- b1=111;b2=222;
* >a(a1=A1;)>b(b1=111;b2=222;)>c -- c1=C1;
* >a(a1=A1;)>b(b1=111;b2=222;)>c(c1=C1;) -- #text=thisisc;
* >a(a1=A1;)>o() -- #text=morestuff;
* -----------------------------------------
* 
* 
* Content of the  <b b1="B1" b2="B2"> (and its  very existence) has been
* ignored. This had no implication on the <o>...</o> node.
* 
* 
* If attributes  of a node are  not fully described in  the filter file,
* the content of the node is accepted unconditionally.
* 
* Example:
* Base file:
* <a a1="A1">
* <b b1="B1" b2="B2">
* <c c1="B1C1"></c>
* </b> 
* <b b1="111" b2="222">
* <c c1="C1">
* this is c
* </c>
* <c c1="another c">
* this is another c
* </c>
* </b>
* </a>
* 
* Filter file:
* <a a1="A1">
* <b b2="222">
* <c c1="another c">
* </c>
* </b>
* </a>
* -----------------------------------------
* XmlTree contains:
* >a -- a1=A1;
* >a(a1=A1;)>b -- b1=111;b2=222;
* >a(a1=A1;)>b(b1=111;b2=222;)>c -- c1=C1;
* >a(a1=A1;)>b(b1=111;b2=222;)>c -- c1=anotherc;
* >a(a1=A1;)>b(b1=111;b2=222;)>c(c1=C1;) -- #text=thisisc;
* >a(a1=A1;)>b(b1=111;b2=222;)>c(c1=anotherc;) -- #text=thisisanotherc;
* -----------------------------------------
* 
* 
* To select "another c" only, use
* <a a1="A1">
* <b b1="111" b2="222">
* <c c1="another c">
* </c>
* </b>
* </a>
* -----------------------------------------
* XmlTree contains:
* >a -- a1=A1;
* >a(a1=A1;)>b -- b1=111;b2=222;
* >a(a1=A1;)>b(b1=111;b2=222;)>c -- c1=anotherc;
* >a(a1=A1;)>b(b1=111;b2=222;)>c(c1=anotherc;) -- #text=thisisanotherc;
* -----------------------------------------
* 
* 5. String Filtering
* 
* Currently white spaces, tabulation symbols, new lines are removed.
* 
*/

#ifndef StlXmlTree_h
#define StlXmlTree_h
#ifdef __ROOT__
#include "Rtypes.h"
#endif
#include <vector>
#include <map>
#include <string>

namespace stl_xml_tree {
  const std::string sep = ">"; // will be parsed out of XML or cause an error -- safe to use here
  enum 
    {
      NO_ERROR,
      NO_XML_BASE, // no file to be parsed
      BAD_XML      // parsing error, the structure may be incomplete
    };
}

class StlXmlTree
{
 private:
  StlXmlTree* Filter = 0;
#ifndef __CINT__
  xmlTextReaderPtr reader;
#endif
  std::multimap<std::string,std::string> XmlTree;
  char* xmlfilename = 0;
  std::multimap<std::string,std::string>::const_iterator OnTheTree;
  int Depth = 0;
  int PreviousDepth = 0;
  int MaxDepthWanted = 0;
  std::string XmlTreeNodeName;
  std::string NodeName;
  std::string IdString;
  short MyStatus = 0;

  void ProcessNode();
#ifdef DEBUG
  void ProcessNodeDebug();
#endif
  void ProcessAttribute();
  bool SkipBasedOnValue(const std::string key, const std::string value);
  void UpdateDepth();


public:
  StlXmlTree();
  StlXmlTree(const std::string xmlfilename, StlXmlTree* filter=0);
  virtual ~StlXmlTree(){;}
  void ShowTree();
  static std::map<std::string,std::string> ParseAttributeString(const std::string);
  static std::string MakeKey(const std::string FullyQualifiedParent, const std::string Child);
  static std::string QualifyParent(const std::string Parent, const std::string Attributes);
  std::vector<std::string> LookUpValueByKey(const std::string key);
  std::vector<std::string> LookUpValueByKey
    (std::string& Parent, const std::string ParentAttributes, const std::string Child);
//--------------^ enables recursion, returns the look-up key good for the next step
  void InsertKeyValuePair(std::string key, std::string value);
  static bool AttributesContain(std::string attributeString, std::string attributeName, std::string attributeValue);
  inline short GetStatus() {return MyStatus;};
#ifdef __ROOT__
  ClassDef(StlXmlTree,0);
#endif
};
#endif
#endif
