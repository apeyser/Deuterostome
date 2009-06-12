#ifndef DM_CONVERT_H
#define DM_CONVERT_H

#include "dm.h"

#if DM_HOST_IS_32_BIT
// converts from a dm-3.2 file object to a dm-4.0 object in memory
// dir filename | root
P op_readf32(void);
#endif //DM_HOST_IS_32_BIT

#endif //DM_CONVERT_H
