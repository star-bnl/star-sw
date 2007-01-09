#ifndef NoXmlTreeReader
/*! \class StDbServiceBroker
* 
* Name: StDbServiceBroker
* 
* Author: Mikhail Kopytin
* 
* Date: 08/29/2006
* 
* Abstract: parse XML configuration file, obtain user's credentials,
* direct the user to the server. Implement load balancing and class of service
* policies as described in the XML file. This class is dependent on a particular
* schema, found in 
* http://www.star.bnl.gov/STAR/comp/sofi/FileCatalog/schema/SCATALOG.html
*  
*/
#ifndef StDbServiceBroker_h
#define StDbServiceBroker_h
#include <vector>
#include <string>

#include "StlXmlTree.h"
#include "ChapiDbHost.h"

namespace st_db_service_broker
{
  const std::string MyScatalogVersion = "version=1.0.1;"; 
  const short NightEnds = 8;
  const short NightBegins = 22;
  const short DayBegins = NightEnds;
  const short DayEnds = NightBegins;
  
  enum 
    {
      NO_ERROR,
      NO_XML_BASE,
      NO_USER,
      NO_DOMAIN,
      NO_HOSTS
    };
}

class StDbServiceBroker
{
 private:

  StlXmlTree ParsedXml;
  std::vector<ChapiDbHost> MyHostList;
  std::vector<ChapiDbHost>::const_iterator MyBestHost;
  void FormHostList();
  void PrintHostList();
  void RecommendHost();
  short MyStatus;  // see namespace st_db_service_broker

 public:

  StDbServiceBroker(): MyStatus(st_db_service_broker::NO_XML_BASE){};
  StDbServiceBroker(const std::string xmlbase);
  StDbServiceBroker(const std::string xmlbase, const std::string xmlfilter);
  void DoLoadBalancing();

  std::string GiveHostName();
  short GiveHostPort();
  std::string GiveUserName();
  std::string GiveUserPassword();
  inline short GetStatus(){return MyStatus;};
};
#endif
#endif
