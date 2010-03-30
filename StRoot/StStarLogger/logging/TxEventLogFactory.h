/*
 * @file TxEventLogFactory.h
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogFactory.h,v 1.2 2010/03/30 20:05:37 fine Exp $
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
};
}
#endif
