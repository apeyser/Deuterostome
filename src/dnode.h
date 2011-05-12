#ifndef DNODE_H
#define DNODE_H

#include <signal.h>

#define DM_RESTART (SIGUSR1)
static const union sigval DM_RESTART_VAL = {.sival_int = 1};

#endif
