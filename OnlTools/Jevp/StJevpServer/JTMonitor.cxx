#include <stdio.h>
#include <unistd.h>
#include "JTMonitor.h"
#include <errno.h>
#include <string.h>
#include <rtsLog.h>
#include <poll.h>

JTMonitor::JTMonitor()
{
    sList = new TList();
}

JTMonitor::~JTMonitor()
{
    delete sList;
}

void JTMonitor::Add(TSocket *s)
{
    sList->Add(s);
}

void JTMonitor::Remove(TSocket *s)
{
    sList->Remove(s);
}

TSocket *JTMonitor::Select(int delay)
{
    struct pollfd pollfds[_JT_NSOCK_];
    TSocket *s;

    TListIter next(sList); 
    int n = 0;
    while((s = (TSocket *)next())) {
	pollfds[n].fd = s->GetDescriptor();
	pollfds[n].events = POLLIN | POLLPRI;
	pollfds[n].revents = 0;
    
	if(n >= _JT_NSOCK_) {
	    LOG(CRIT, "N(%d) > _JT_NSOCK_(%d)",n,_JT_NSOCK_);
	}
    
	n++;
    }

    int result = poll(pollfds, n, delay);
  
    if(result < 0) {
	LOG(ERR, "An error calling poll: %s",strerror(errno));
	return NULL;
    }

    if(result == 0) {
	LOG(NOTE, "A timeout calling poll");
	return NULL;
    }

    // Have data...
    for(int i=0;i<n;i++) {
	if(pollfds[i].revents) {
	    TListIter next2(sList);
	    while((s = (TSocket *)next2())) {
		if(s->GetDescriptor() == pollfds[i].fd) {
		    return s;
		}
	    }
	    LOG(ERR, "Poll returned activity on socket %d but couldn't find it...",
		pollfds[i].fd);
	}
    }
  
    LOG(ERR, "Poll returned activity but not on any socket?");
    return NULL;
  
}
