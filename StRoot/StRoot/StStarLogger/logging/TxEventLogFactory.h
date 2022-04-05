/*
 * @file TxEventLogFactory.h
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogFactory.h,v 1.3 2010/09/24 17:17:35 fine Exp $
 *
 * TxEventLogFactory instantiuate the concrete implementation for 
 * * TxEventLOf interface
 */
 
#ifndef TX_EVENT_LOG_FACTORY_H
#define TX_EVENT_LOG_FACTORY_H
namespace TxLogging {
class TxEventLog;
class TxEventLogFactory {
   public:
      static TxEventLog* create(const char *technology="file");
      static int main(int argc, const char *argv[]);
};
}
#endif
