#ifndef NoXmlTreeReader
#include <iostream>
#include <limits.h>
#include "StlXmlTree.h"
#include "ChapiStringUtilities.h"

using std::string;
using std::vector;
using std::map;
using std::multimap;
using namespace chapi_string_utilities;
using stl_xml_tree::sep; // separation symbol
using stl_xml_tree::NO_ERROR;
using stl_xml_tree::NO_XML_BASE;
using stl_xml_tree::BAD_XML;
using std::cout;
using std::cerr;
#ifndef __STDB_STANDALONE__
#include "StMessMgr.h"
#else
#define LOG_DEBUG cout
#define LOG_INFO cout
#define LOG_WARN cout
#define LOG_ERROR cerr
#define LOG_FATAL cerr
#define LOG_QA cout
#define endm "\n"
#endif



typedef vector<string>::const_iterator VCI;
typedef map<string,string>::const_iterator MCI;
typedef multimap<string,string>::const_iterator MMCI;



//////////////////////////////////////////////////////////////
StlXmlTree::StlXmlTree():
  Filter(0),
  XmlTree(multimap<string,string>()),
  Depth(0),
  PreviousDepth(0),
  MaxDepthWanted(INT_MAX),
  XmlTreeNodeName(""),   
  MyStatus(NO_XML_BASE)
{
}
//////////////////////////////////////////////////////////////
StlXmlTree::StlXmlTree(const string xmlfilename, StlXmlTree* filter) : 
  Filter(filter),
  XmlTree(multimap<string,string>()),
  Depth(0),
  PreviousDepth(0),
  MaxDepthWanted(INT_MAX),
  XmlTreeNodeName(""),   
  MyStatus(NO_ERROR)
{
  int ret;
  reader = xmlNewTextReaderFilename(xmlfilename.c_str());
  if (reader != NULL) 
    {
      ret = xmlTextReaderRead(reader);
      while (ret == 1) 
	{
	  	  ProcessNode();
		  //   ProcessNodeDebug();
	  ret = xmlTextReaderRead(reader);
	}
      xmlFreeTextReader(reader);
      if (ret != 0) 
	{
	  LOG_ERROR<<"StlXmlTree::StlXmlTree: failed to parse "<< xmlfilename<<endm;
	  MyStatus = BAD_XML;
	}
    } 
  else 
    {
    LOG_ERROR<<"StlXmlTree::StlXmlTree: unable to open "<< xmlfilename<< endm;
    MyStatus = NO_XML_BASE;
    }
}
////////////////////////////////////////////////////////
void StlXmlTree::ShowTree()
{
  LOG_INFO << "-----------------------------------------"<<endm;
  LOG_INFO << "XmlTree contains"<<endm;
  for (MMCI I=XmlTree.begin(); I!=XmlTree.end(); ++I)
    {
      LOG_INFO << (*I).first <<" -- "<<(*I).second <<endm;
    }
  LOG_INFO << "-----------------------------------------"<<endm;
}
////////////////////////////////////////////////////////
string StlXmlTree::MakeKey(string FullyQualifiedParent, string Child)
{
  return FullyQualifiedParent + sep + Child;
}
////////////////////////////////////////////////////////
string StlXmlTree::QualifyParent(string Parent, string Attributes)
{
  string rtrn = Parent + "(" + Attributes + ")";
  return rtrn;
}
////////////////////////////////////////////////////////
vector<string> StlXmlTree::LookUpValueByKey(string key)
{
  vector<string> rtrn;
  MMCI f = XmlTree.find(key);
  if (f!=XmlTree.end())
    {
      MMCI b = XmlTree.lower_bound(key);
      MMCI e = XmlTree.upper_bound(key);
      
      for (MMCI i = b; i!=e; ++i)
	{
	  rtrn.push_back((*i).second);
	}
    }
  return rtrn;
}
////////////////////////////////////////////////////////
vector<string> StlXmlTree::LookUpValueByKey
(string& Parent, string ParentAttributes, string Child)
{
  string qp = QualifyParent(Parent,ParentAttributes);
  string key = MakeKey(qp,Child);
#ifdef DEBUG
  LOG_INFO << "StlXmlTree::LookUpValueByKey key is " << key <<endm;
#endif
  vector<string> rtrn = LookUpValueByKey(key);
  Parent = key; // return key to the calling module -- enable recursion
  return rtrn;
}
////////////////////////////////////////////////////////
void StlXmlTree::InsertKeyValuePair(string Key, string Value)
{
 OnTheTree = XmlTree.insert(make_pair(Key,Value));
}
////////////////////////////////////////////////////////
bool StlXmlTree::AttributesContain(string A, string N, string V)
{
  // as input, expect any of the strings returned by LookUpValueByKey
  if ((N=="" && V=="") || N=="") 
    {
      return false;
    }
  map<string,string> nv = ParseAttributeString(A);
  map<string,string>::iterator I = nv.find(N);
  if (I==nv.end())
    {
      return false;
    }
  else
    {
      if ((*I).second==V)
	{
	  return true;
	}
      else
	{
	  return false;
	}
    }
}
////////////////////////////////////////////////////////
#ifdef DEBUG
void StlXmlTree::ProcessNodeDebug()
{
  /*
From http://dotgnu.org/pnetlib-doc/System/Xml/XmlNodeType.html

xmlTextReaderNodeType returns the following values:
None = 0
Element = 1
Attribute = 2
Text = 3
EntityReference = 5
Entity = 6
SignificantWhitespace = 14
EndElement = 15
EndEntity = 16
  */
    const xmlChar *name, *value;

    //    name = xmlTextReaderConstName(reader);
    name = xmlTextReaderName(reader);
    if (name == NULL)
	name = BAD_CAST "--";

    //    value = xmlTextReaderConstValue(reader);
    value = xmlTextReaderValue(reader);

    LOG_INFO << xmlTextReaderDepth(reader) <<" "<<xmlTextReaderNodeType(reader)<<" "<<name<<" "<<xmlTextReaderIsEmptyElement(reader)<<" "<<  xmlTextReaderHasValue(reader)<<endm;
	if (value == NULL)
	{
		LOG_INFO <<endm;
	}
	else
	{
		if (xmlStrlen(value) > 40)
		{
		LOG_INFO<<value<<".40s..."<endm;
		}
		else
		{
		LOG_INFO<<" "<<value<<endm;
		}
	}

  short HasAttributes = xmlTextReaderHasAttributes(reader);
      if (HasAttributes)
	{
	  while (xmlTextReaderMoveToNextAttribute(reader))
	    {
	      ProcessNodeDebug();
	    }
	}
}
#endif
////////////////////////////////////////////////////////
void StlXmlTree::UpdateDepth()
{
  PreviousDepth = Depth;
  Depth = xmlTextReaderDepth(reader);
}
////////////////////////////////////////////////////////
void StlXmlTree::ProcessAttribute()  
{ 

/*  attributes are on  the same
depth as value text of the node, one level below the name of the node.
The difference between  attribute value and regular value  is that for
the attribute, the name and value are read out in one step.*/

  int NodeType = xmlTextReaderNodeType(reader);

  UpdateDepth();

  if (NodeType!=XML_READER_TYPE_ATTRIBUTE)
    {
      return;
    }

 xmlChar* atname = xmlTextReaderName(reader);
 string AttributeName;
 if ( atname != NULL ) {
 	AttributeName = (char*)atname;
	xmlFree(atname);
	atname = NULL;
 }

 xmlChar* tmp = xmlTextReaderValue(reader);
 string NodeValue;
 if ( tmp != NULL ) {
	NodeValue = filter_string((char*)tmp);
	xmlFree(tmp);
	tmp = NULL;
 }

 IdString += AttributeName + "=" + NodeValue + ";";
}
////////////////////////////////////////////////////////
void StlXmlTree::ProcessNode()
{
  int NodeType = xmlTextReaderNodeType(reader);
  short HasValue = xmlTextReaderHasValue(reader);
  short HasAttributes = xmlTextReaderHasAttributes(reader);
  xmlChar* name = xmlTextReaderName(reader);
  if ( name != NULL ) {
  	NodeName = (char*)name;
  	xmlFree(name);
  	name = NULL;
  }
  
  string NodeValue = "";

  IdString = "";
  if (HasValue)
    {
	  xmlChar* tmp = xmlTextReaderValue(reader);
	  if ( tmp != NULL ) {
      	NodeValue = filter_string((char*)tmp);
  	  	xmlFree(tmp);
		tmp = NULL;
	  }

      IdString = NodeName + "=" + NodeValue + ";";
    }

  UpdateDepth();

  if (Depth>MaxDepthWanted)
    {
      return;
    }
  if (Depth<MaxDepthWanted)
    {
      MaxDepthWanted = INT_MAX;
    }

  if (Depth<PreviousDepth || NodeType==XML_READER_TYPE_END_ELEMENT)
    {
      cut_string_after_sub(XmlTreeNodeName,sep);
    }

  if ((NodeType==XML_READER_TYPE_ELEMENT || NodeValue !="") && NodeType != XML_READER_TYPE_COMMENT )
    {
      if (NodeType==XML_READER_TYPE_ELEMENT)
	{
	  XmlTreeNodeName  = XmlTreeNodeName  + sep + NodeName;

	  if (HasAttributes)
	    {

	      while (xmlTextReaderMoveToNextAttribute(reader))
		{
		  ProcessAttribute();
		}
	    }
	}
      
      if (!SkipBasedOnValue(XmlTreeNodeName,IdString))
	{
	  string key = XmlTreeNodeName;
	  string value = IdString;
	  
	  //	  XmlTreeNodeName = XmlTreeNodeName + "("+IdString+")";
	  XmlTreeNodeName = QualifyParent(XmlTreeNodeName,IdString);

	  //	  if (value!="") 
	    {
	      OnTheTree = XmlTree.insert(make_pair(key,value));
	      MaxDepthWanted = INT_MAX;
	    }
	}
      else
	{
	  MaxDepthWanted = Depth-1;
	}
    }
	

}
////////////////////////////////////////////////////////
map<string,string>  StlXmlTree::ParseAttributeString(string in)
{
  vector<string> v = slice(in,";");
  map<string,string> m = associate_pieces(v,"=");
  return m;
}
////////////////////////////////////////////////////////
bool StlXmlTree::SkipBasedOnValue(const string key, const string value)
{
  if (!Filter) 
    {
      return false;
    }

  map<string,string> m = ParseAttributeString(value);

#ifdef DEBUG
  LOG_INFO << "StlXmlTree::SkipBasedOnValue key = "<<key<<" value =  "<<value<<endm;
#endif
  MMCI b = Filter->XmlTree.lower_bound(key);
  MMCI e = Filter->XmlTree.upper_bound(key);

  bool rtrn = false;

  /*  skip if all  values mismatch  the key  <==> if  there is  even a
single match, take it.  If there is no key, take it !*/



  if (b!=e)
    {
      for (MMCI i=b; i!=e; ++i)
	{
	  map<string,string> f = ParseAttributeString((*i).second);

	  /* use info in the filter: a single mismatching value vetoes */

	  for (MCI ii = f.begin(); ii != f.end(); ++ii)
	    {
	      string search_key = (*ii).first;

	      MCI mykey = m.find(search_key);
	      
	      if (mykey!=m.end())
		{
		      /* generalizing for the attribute which is in itself a comma-separated sequence. 
			 In that case, split into comma-separated elements and match element-by-element.
			 Return true if not a single match is found. 
		      */

		      vector<string> mykey_v = slice((*mykey).second,",");
		      vector<string> f_v = slice((*ii).second,",");

		      rtrn = true;
			  VCI m_v_i = mykey_v.begin();
			  while (m_v_i != mykey_v.end() && rtrn)
			    {
			      VCI f_v_i = f_v.begin();
			      
			      while (f_v_i != f_v.end() && rtrn)
				{
#ifdef DEBUG
				  LOG_INFO <<" compare " <<*m_v_i <<" and "<<*f_v_i <<endm;
#endif
				  if (*m_v_i == *f_v_i) 
				    {
				      rtrn = false;
				    }
				
				  ++f_v_i;
				}
			      ++m_v_i;
			    }

			  if (rtrn) 
			    {
#ifdef DEBUG
			      LOG_INFO << "StlXmlTree::SkipBasedOnValue "<< rtrn <<endm;
#endif
			      return rtrn;
			    }
		}
	    }
	}      
    }
  else
    {
      rtrn = false; /* otherwise will need to have a prototype structure in 
		       a filter file to enable parsing*/
    }

#ifdef DEBUG
			      LOG_INFO << "StlXmlTree::SkipBasedOnValue "<< rtrn <<endm;
#endif
  return rtrn;
}

#endif

