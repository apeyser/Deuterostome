#ifndef THREADS_H
#define THREADS_H

#ifdef ENABLE_THREADS
#if DISABLE_THREAD
#undef ENABLE_THREADS
#endif //DISABLE_THREADS
#endif //ENABLE_THREADS

#if ENABLE_THREADS

L op_makethreads(void);
L op_threads(void);

typedef L (*thread_func)(UL id, B* data);

L threads_do(UL nways, thread_func func, B* data);

void thread_share_lock_f(void);
void thread_share_unlock_f(void);
#define thread_share_lock() thread_share_lock_f(); {
#define thread_share_unlock() }; thread_share_unlock_f();
    
extern UL thread_num;
extern UL thread_max;

#endif //ENABLE_THREADS && ! DISABLE_THREADS

#endif //THREADS_H
