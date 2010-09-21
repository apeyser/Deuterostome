#ifndef DM_SEM_H
#define DM_SEM_H

#include "dm.h"

#ifdef DM_ENABLE_SEM //intentional ifdef for undef
#if DM_DISABLE_SEM
#undef DM_ENABLE_SEM
#ifdef DM_HAVE_CONFIG_H
#undef ENABLE_SEM
#endif //DM_HAVE_CONFIG_H
#endif //DM_DISABLE_SEM
#endif //DM_ENABLE_SEM


P inter_lock_init(void);
P reset_inter_lock(void);
P do_inter_lock(void);
P do_inter_unlock(void);
P do_inter_lock_reset(void);

P op_inter_lock(void);
P op_inter_unlock(void);
P op_inter_lock_implicit(void);
P op_inter_lock_set(void);

#endif //DM_SEM_H
