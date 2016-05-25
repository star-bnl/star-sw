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
#ifdef __ROOT__
#include "Rtypes.h"
#endif
namespace st_db_service_broker {
  const std::string MyScatalogVersion = "version=1.0.1;"; 
  const short NightEnds = 8;
  const short NightBegins = 22;
  const short DayBegins = NightEnds;
  const short DayEnds = NightBegins;
  
  enum 
    {
      NO_ERROR,   // 0
      NO_XML_BASE,// 1
      NO_USER,    // 2
      NO_DOMAIN,  // 3
      NO_HOSTS,   // 4
      BAD_XML     // 5, introduced by Dmitry, to propagate errors correctly
    };
}

namespace lb_error {
  enum
    {
      NO_ERROR,           // 0
      NO_LPD_ENV_VAR,     // 1
      NO_GPD_ENV_VAR,     // 2
      NO_WRITE_PERMISSION,// 3
      NO_LPD_DIR,         // 4 
      AFS_ERROR,          // 5
      WWW_ERROR           // 6
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
  int RecommendHost();
  void SendEmail(time_t timediff);
  short MyStatus;  // see namespace st_db_service_broker
  time_t last_succeeded_connect_time = 0;
  time_t seconds_to_reach_for_connect = 1800;

 public:

  StDbServiceBroker(): MyStatus(st_db_service_broker::NO_XML_BASE){};
  StDbServiceBroker(const std::string xmlbase);
  StDbServiceBroker(const std::string xmlbase, const std::string xmlfilter);
  virtual ~StDbServiceBroker(){};
  void DoLoadBalancing();

  std::string GiveHostName();
  short GiveHostPort();
//  std::string GiveUserName();
//  std::string GiveUserPassword();
  inline short GetStatus(){return MyStatus;};
  static int updateLocalLbPolicy(); // returns an lb_error:: error
#ifdef __ROOT__
  ClassDef(StDbServiceBroker,0)
#endif
};
#endif
#endif
