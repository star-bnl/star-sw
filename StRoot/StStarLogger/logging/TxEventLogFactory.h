/*
 * @file TxEventLogFactory.h
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogFactory.h,v 1.1 2009/06/17 22:12:00 fine Exp $
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
