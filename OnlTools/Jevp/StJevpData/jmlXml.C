#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <libxml2/libxml/xmlreader.h>
#include <unistd.h>
#include <stdlib.h>

void chomp(char *to, char *str, int max)
{
  strncpy(to,str,max);
  to[max-1] = '\0';

  int n=strlen(to);
  for(int i=n-1;i>=0;i--) {
    if(isspace(to[i])) to[i] = '\0';
    else return;
  }
}

void dump_tree(xmlNode *node)
{
    while(node) {
	if(node->type == XML_ELEMENT_NODE) {
	    printf("node type: Element, name: %s\n",node->name);
	}
	else {
	    printf("Node type %d\n",node->type);
	}
    
	if(node->children) {
	    printf("start child\n");
	    dump_tree(node->children);
 	    printf("end child\n");
	}
	node = node->next;
    }
}


int main(int argc, char *argv[])
{

    LIBXML_TEST_VERSION

	xmlTextReaderPtr reader;
    int ret;
  
    for(;;) {
	reader = xmlReaderForFile(argv[1], NULL, XML_PARSE_NOBLANKS);
	if(reader != NULL) {
	    do {
		ret = xmlTextReaderRead(reader);
      
		// Process...
		const xmlChar *name, *value;

		name = xmlTextReaderConstName(reader);
		value = xmlTextReaderConstValue(reader);
      
		printf("depth:%d type:%d name:%s empty:%d value%d", 
		       xmlTextReaderDepth(reader),
		       xmlTextReaderNodeType(reader),
		       name,
		       xmlTextReaderIsEmptyElement(reader),
		       xmlTextReaderHasValue(reader));
		if (value == NULL)
		    printf("\n");
		else {
		    char val[100];
		    chomp(val, (char *)value, 100);

		    if (strlen(val) > 40)
			printf(" val = %.40s\n", val);
		    else
			printf(" str = %s\n", val);
		}

	    } while(ret == 1);
	}
  
	xmlFreeTextReader(reader);

	
	xmlCleanupParser();  

	sleep(10);
    }
    //   xmlDoc *doc = NULL;
    //   xmlNode *root = NULL;
    //   doc = xmlReadFile(argv[1], NULL, XML_PARSE_NOBLANKS);
    //   if(!doc) {
    //     printf("Couldn't parse xml file for tree...\n");
    //     return 0;
    //   }

    //   root = xmlDocGetRootElement(doc);
    //   dump_tree(root);

  
    return 0;
}

