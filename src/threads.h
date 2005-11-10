#ifndef THREADS_H
#define THREADS_H

#if THREADS_ENABLED

L op_makethreads(void);
L op_threads(void);

typedef L (*thread_func)(UL id, B* data);

L threads_do(UL nways, thread_func func, B* data);
L thread_share_lock(UL id, thread_func func, B* data);

extern UL thread_num;
extern UL thread_max;

#endif //THREADS_ENABLED

#endif //THREADS_H
