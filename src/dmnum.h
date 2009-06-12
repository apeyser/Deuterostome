#ifndef DMNUM_H
#define DMNUM_H

#include "dm.h"

__attribute__ ((unused)) 
static W TYPEBYTES[] = {
  sizeof(B),
  sizeof(W),
  sizeof(L32),
  sizeof(L64),
  sizeof(S),
  sizeof(D)
};
#define nTYPES (sizeof(TYPEBYTES)/sizeof(TYPEBYTES[0]))

#endif
