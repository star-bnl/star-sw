
#ifndef FTFLOG_H
#define FTFLOG_H

extern int ftfLogTarget;

#define FTF_LOG_PRINTF 1
#define FTF_LOG_REMOTE 2

void ftfLog(const char *fmt, ...) ;

#endif

